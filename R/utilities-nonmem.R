#' @title simulate_pk
#' @description
#' Simulate PK from PK data.
#' Currently, only 1-, 2- and 3-compartment infusion models are supported.
#' @param data A data.frame of PK data.
#' @return A data.frame with simulated PK data.
#' @export
#' @importFrom mrgsolve mread_cache data_set mrgsim_df tgrid
#' @examples
#'
#' head(simulate_pk(data_501 |> dplyr::filter(ID == 1)))
simulate_pk <- function(data){
  data_names <- names(data)
  # Load appropriate model from available variable names
  model_file <- ifelse(
    "V3" %in% data_names,
    "3cmt.cpp",
    ifelse("V2" %in% data_names, "2cmt.cpp", "1cmt.cpp")
    )
  pk_model <- mrgsolve::mread(system.file("models", model_file, package = "nonmem.utils"), quiet = TRUE)
  #mrgsolve does not support EVID=3 or 4
  pk_data <- fill_nonmem_vars(data) |>
    mutate(
      oldID = ID,
      ID = cumsum(c(1, diff(ID))!=0)*100 + cumsum(EVID %in% c(3, 4)),
      EVID = ifelse(EVID %in% 4, 1, EVID),
      CMT = 1
    ) |>
    filter(EVID != 3)

  sim_data <- lapply(
    unique(pk_data$ID),
    function(id){
      ind_data <- pk_data |> filter(ID %in% id)
      pk_model |>
        mrgsolve::data_set(data = ind_data) |>
        mrgsolve::mrgsim_df(
          tgrid = mrgsolve::tgrid(
            start = min(ind_data$TIME, na.rm = TRUE),
            end = max(ind_data$TIME+1, na.rm = TRUE),
            delta = 0.1
          ),
          tad = TRUE,
          obsaug = TRUE,
          obsonly = TRUE,
          carry_out = "oldID"
        )
    }
  )

  sim_data <- do.call("rbind", sim_data) |>
    mutate(ID = oldID) |>
    select(-oldID)

  return(sim_data)
}

#' @title calc_tad
#' @description
#' Calculate time after dose (TAD).
#' Records before the first dose are output < 0.
#' For infusions (`dur>0` or `rate>0`), the time after the end of infusion is calculated.
#' @param id Subject identifiers values
#' @param evid Event identifiers values
#' @param time Time values
#' @param amt Amount values
#' @param rate Rate values
#' @param dur Duration values
#' @export
#' @examples
#'
#' data_501 |>
#' dplyr::mutate(TAD2 = calc_tad(id = ID, time = TIME, amt = AMT))
#'
#' data_501 |>
#' dplyr::mutate(TAD2 = calc_tad(id = ID, time = TIME, evid = EVID))
#'
#' data_501 |>
#' dplyr::mutate(TAD2 = calc_tad(
#' id = ID,
#' time = TIME,
#' evid = EVID,
#' amt = AMT,
#' rate = RATE
#' ))
#'
calc_tad <- function(id, time, amt = NULL, evid = NULL, rate = NULL, dur = NULL){
  # No amt nor evid, first value per id is 0
  only_time <- all(is.null(amt), is.null(evid))
  if(only_time){
    tad <- stats::ave(time, id, FUN = function(x){x-x[1]})
    return(tad)
  }
  # Rely only on evid
  if(is.null(amt)){
    occ <- stats::ave(evid, id, FUN = function(x){cumsum(x>3)})
    dose_id <- evid %in% c(1, 4)
    id_occ <- id*10+occ
    # Get time of first dose
    tfirst <- stats::ave(time*ifelse(dose_id, 1, NA), id_occ, FUN = function(x){min(x, na.rm = TRUE)})
    # Get time of last dose
    tlast <- stats::ave(time*dose_id, id_occ, FUN = function(x){cummax(x)})
    # For predose data, tlast needs to be time of first dose
    tlast[tlast==0] <- tfirst[tlast==0]
    tad <- time-tlast
    return(tad)
  }
  # Include duration in calculation
  # If rate or dur provided, delay end of dose
  dur <- dur %||% rep(0, length(time))
  if(!is.null(rate)){
    dur <- amt/ifelse(rate>0, rate, 0)
  }
  dur <- ifelse(is.na(dur), 0, dur)
  # Rely only on amt
  if(is.null(evid)){
    dose_id <- ifelse(is.na(amt), FALSE, amt>0)
    # Get time of first dose
    tfirst <- stats::ave(dur+time*ifelse(dose_id, 1, NA), id, FUN = function(x){min(x, na.rm = TRUE)})
    # Get time of last dose
    tlast <- stats::ave(dur+time*dose_id, id, FUN = function(x){cummax(x)})
    # For predose data, tlast needs to be time of first dose
    tad <- time-tlast
    return(tad)
  }
  occ <- stats::ave(evid, id, FUN = function(x){cumsum(x>3)})
  dose_id <- evid %in% c(1, 4)
  id_occ <- id*10+occ
  # Get time of first dose
  tfirst <- stats::ave(dur+time*ifelse(dose_id, 1, NA), id_occ, FUN = function(x){min(x, na.rm = TRUE)})
  # Get time of last dose
  tlast <- stats::ave(dur+time*dose_id, id_occ, FUN = function(x){cummax(x)})
  # For predose data, tlast needs to be time of first dose
  tlast[tlast==0] <- tfirst[tlast==0]
  tad <- time-tlast
  return(tad)
}
