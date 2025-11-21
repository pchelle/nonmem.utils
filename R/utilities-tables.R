#' @title cor_report
#' @description
#' Report of Spearman correlation with p-value
#' @param data A data.frame
#' @param x name of `x` variable
#' @param y name of `y` variable
#' @return A character of correlation and p-value
#' @export
#' @importFrom stats cor.test
#' @examples
#'
#' # Summarize data by ID
#' sum_data <- data_501 |>
#'   dplyr::group_by(ID) |>
#'   dplyr::summarise_all(dplyr::first)
#'
#' # Spearman correlation with p-value
#' cor_report(sum_data, x = "WT", y = "AGE")
#'
cor_report <- function(data, x, y) {
  cor_result <- tryCatch(
    {
      cor.test(x = data[[x]], y = data[[y]], method = "spearman", exact = FALSE)
    },
    error = function(e) {
      list(estimate = NA, p.value = 1)
    }
  )

  cor_value <- paste0(
    sprintf("%.3f", cor_result$estimate),
    " (p",
    ifelse(cor_result$p.value < 1e-3, "< ", ": "),
    ifelse(cor_result$p.value < 1e-3, "0.001", sprintf("%.3f", cor_result$p.value)),
    ")"
  )
  return(cor_value)
}

#' @title lm_report
#' @description
#' Report of lm coefficients with p-value
#' If a categorical variable is used, should be as `x` argument
#' @param data A data.frame
#' @param x name of `x` variable (categorical variable)
#' @param y name of `y` variable (continuous variable)
#' @return A character of lm coefficients and p-value
#' @export
#' @importFrom stats lm as.formula
#' @examples
#'
#' # Summarize data by ID
#' sum_data <- data_501 |>
#'   dplyr::group_by(ID) |>
#'   dplyr::summarise_all(dplyr::first)
#'
#' # Map categorical data
#' sum_data <- map_cat_data(sum_data, meta_data_501)
#'
#' # ANOVA correlation with p-value
#' # Categorical variable, SEX, as x argument
#' lm_report(sum_data, x = "SEX", y = "WT")
#'
lm_report <- function(data, x, y) {
  lm_result <- summary(lm(formula = as.formula(paste(y, "~", x)), data = data))
  lm_names <- gsub(pattern = x, replacement = "", row.names(lm_result$coefficients))
  lm_names <- gsub(
    pattern = "\\(Intercept\\)",
    replacement = head(levels(data[[x]]), 1),
    lm_names
  )
  lm_value <- paste0(
    lm_names, ": ",
    sprintf("%.2f", lm_result$coefficients[, 1]),
    " (p",
    ifelse(lm_result$coefficients[, 4] < 1e-3, "< ", ": "),
    ifelse(lm_result$coefficients[, 4] < 1e-3, "0.001", sprintf("%.3f", lm_result$coefficients[, 4])),
    ")",
    collapse = "<br>"
  )
  return(lm_value)
}

#' @title check_ranges
#' @description
#' Get a data.frame summarizing data
#' @param data A data.frame of the pk data
#' @param meta_data A data.frame of meta data
#' @return A data.frame summarizing the data
#' @export
check_ranges <- function(data, meta_data) {
  variable_names <- sapply(
    c("amt", "dv", "mdv", "evid", "blq"),
    function(x) pull_name(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  meta_data$`Data Min` <- sapply(
    meta_data$Name,
    function(variable_name) {
      if (variable_name %in% variable_names$amt) {
        selected_rows <- data[[variable_names$evid]] %in% c(1, 4)
        return(min(data[selected_rows, variable_name], na.rm = TRUE))
      }
      if (variable_name %in% variable_names$dv) {
        selected_rows <- data[[variable_names$mdv]] == 0 & data[[variable_names$blq]] <= 0
        return(min(data[selected_rows, variable_name], na.rm = TRUE))
      }
      min(data[[variable_name]], na.rm = TRUE)
    },
    USE.NAMES = FALSE, simplify = FALSE
  )
  meta_data$`Data Max` <- sapply(
    meta_data$Name,
    function(variable_name) {
      if (variable_name %in% variable_names$amt) {
        selected_rows <- data[[variable_names$evid]] %in% c(1, 4)
        return(max(data[selected_rows, variable_name], na.rm = TRUE))
      }
      if (variable_name %in% variable_names$dv) {
        selected_rows <- data[[variable_names$mdv]] == 0 & data[[variable_names$blq]] <= 0
        return(max(data[selected_rows, variable_name], na.rm = TRUE))
      }
      max(data[[variable_name]], na.rm = TRUE)
    },
    USE.NAMES = FALSE, simplify = FALSE
  )
  meta_data$`Out of Range` <- sapply(
    seq_along(meta_data$Name),
    function(variable_index) {
      variable_name <- meta_data$Name[variable_index]
      meta_min <- meta_data$Min[variable_index]
      if (is.na(meta_min)) {
        meta_min <- -Inf
      }
      meta_max <- meta_data$Max[variable_index]
      if (is.na(meta_max)) {
        meta_max <- Inf
      }
      if (variable_name %in% variable_names$amt) {
        selected_rows <- data[[variable_names$evid]] %in% c(1, 4)
        out_of_range <- data[selected_rows, variable_name] < meta_min | data[selected_rows, variable_name] > meta_max
        return(sum(out_of_range, na.rm = TRUE))
      }
      if (variable_name %in% variable_names$dv) {
        selected_rows <- data[[variable_names$mdv]] == 0 & data[[variable_names$blq]] <= 0
        out_of_range <- data[selected_rows, variable_name] < meta_min | data[selected_rows, variable_name] > meta_max
        return(sum(out_of_range, na.rm = TRUE))
      }
      out_of_range <- data[[variable_name]] < meta_min | data[[variable_name]] > meta_max
      return(sum(out_of_range, na.rm = TRUE))
    },
    USE.NAMES = FALSE, simplify = FALSE
  )
  return(meta_data)
}

#' @title bin_values
#' @description
#' Bin values into specified number of bins based on quantiles.
#' @param values A vector of values to be binned
#' @param bins Number of bins to create
#' @return A factor with the binned values
#' @export
#' @examples
#'
#' data_501 |> dplyr::mutate(Bins = bin_values(values = TIME, bins = 7))
#'
bin_values <- function(values, bins = 7) {
  # Makes a more robust binning based on the number of values
  unique_values <- unique(values)
  if (length(unique_values) <= bins) {
    return(factor(values, levels = unique_values))
  }
  time_bins <- unique(quantile(x = values, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE))
  time_bins[1] <- floor(time_bins[1] * 100) / 100
  time_bins[length(time_bins)] <- ceiling(time_bins[length(time_bins)] * 100) / 100

  return(cut(values, breaks = time_bins, include.lowest = TRUE))
}


#' @title run_vpc
#' @description
#' Run a VPC analysis on the provided data.
#' @param data A data.frame of the pk data
#' @param x Name of `x` variable, usually `"TIME"`
#' @param group Name of `group` variable
#' @param bins Number of bins for the VPC
#' @param ci Confidence interval for the VPC, usually `0.8` (10th-90th percentiles)
#' @param lloq Lower limit of quantification
#' @return A data.frame
#' @export
#' @examples
#'
#' run_vpc(data_501, x = "TIME")
#'
run_vpc <- function(data, x = "TIME", group = "All", bins = 7, ci = 0.8, lloq = 10) {
  # First step prepare data bins
  vpc_data <- fill_nonmem_vars(data) |>
    dplyr::filter(MDV == 0) |>
    dplyr::mutate(
      # observed BLQ data use IPRED
      OBS = ifelse(BLQ > 0, IPRED, OBS),
      Bins = bin_values(.data[[x]], bins = bins)
    )
  # Second step prepare correction factors for pc and pvc VPCs
  # medPRED = median value of typical predictions within time bin (used for pcVPC)
  # pcDV = Prediction-corrected simulated data
  # pcOBS = Prediction-corrected observed data
  vpc_data <- vpc_data |>
    dplyr::group_by(REP, Bins) |>
    dplyr::mutate(
      medPRED = median(PRED, na.rm = TRUE),
      pcDV = DV * median(PRED, na.rm = TRUE) / PRED,
      pcOBS = OBS * median(PRED, na.rm = TRUE) / PRED
    )

  # Third step summarize median, min and max for all the variables for each repetition, bins and group
  vpc_data_rep <- vpc_data |>
    group_by(REP, Bins, .data[[group]]) |>
    summarize(
      # Median values
      time_med = median(.data[[x]], na.rm = TRUE),
      obs_med = median(OBS, na.rm = TRUE),
      dv_med = median(DV, na.rm = TRUE),
      pc_obs_med = median(pcOBS, na.rm = TRUE),
      pc_dv_med = median(pcDV, na.rm = TRUE),
      # pvc_obs_med = median(pvcOBS, na.rm = TRUE),
      # pvc_dv_med = median(pvcDV, na.rm = TRUE),
      npde_obs_med = 0,
      npde_dv_med = median(NPDE, na.rm = TRUE),
      blq_obs_med = 100 * sum(as.numeric(BLQ) > 0, na.rm = TRUE) / n(),
      blq_dv_med = 100 * sum(DV < LLOQ, na.rm = TRUE) / n(),
      # Min values
      time_min = as.numeric(quantile(.data[[x]], probs = (1 - ci) / 2, na.rm = TRUE)),
      obs_min = as.numeric(quantile(OBS, probs = (1 - ci) / 2, na.rm = TRUE)),
      dv_min = as.numeric(quantile(DV, probs = (1 - ci) / 2, na.rm = TRUE)),
      pc_obs_min = as.numeric(quantile(pcOBS, probs = (1 - ci) / 2, na.rm = TRUE)),
      pc_dv_min = as.numeric(quantile(pcDV, probs = (1 - ci) / 2, na.rm = TRUE)),
      # pvc_obs_min = as.numeric(quantile(pvcOBS, probs = (1-ci)/2, na.rm = TRUE)),
      # pvc_dv_min = as.numeric(quantile(pvcDV, probs = (1-ci)/2, na.rm = TRUE)),
      npde_obs_min = stats::qnorm((1 - ci) / 2),
      npde_dv_min = as.numeric(quantile(NPDE, probs = (1 - ci) / 2, na.rm = TRUE)),
      blq_obs_min = 100 * sum(as.numeric(BLQ) > 0, na.rm = TRUE) / n(),
      blq_dv_min = 100 * sum(DV < LLOQ, na.rm = TRUE) / n(),
      # Max values
      time_max = as.numeric(quantile(.data[[x]], probs = (1 + ci) / 2, na.rm = TRUE)),
      obs_max = as.numeric(quantile(OBS, probs = (1 + ci) / 2, na.rm = TRUE)),
      dv_max = as.numeric(quantile(DV, probs = (1 + ci) / 2, na.rm = TRUE)),
      pc_obs_max = as.numeric(quantile(pcOBS, probs = (1 + ci) / 2, na.rm = TRUE)),
      pc_dv_max = as.numeric(quantile(pcDV, probs = (1 + ci) / 2, na.rm = TRUE)),
      # pvc_obs_max = as.numeric(quantile(pvcOBS, probs = (1+ci)/2, na.rm = TRUE)),
      # pvc_dv_max = as.numeric(quantile(pvcDV, probs = (1+ci)/2, na.rm = TRUE)),
      npde_obs_max = stats::qnorm((1 + ci) / 2),
      npde_dv_max = as.numeric(quantile(NPDE, probs = (1 + ci) / 2, na.rm = TRUE)),
      blq_obs_max = 100 * sum(as.numeric(BLQ) > 0, na.rm = TRUE) / n(),
      blq_dv_max = 100 * sum(DV < LLOQ, na.rm = TRUE) / n()
    )
  # Fourth step summarize across repetitions
  vpc_data_med <- vpc_data_rep |>
    ungroup() |>
    group_by(Bins, .data[[group]]) |>
    summarize_all(~ median(.x, na.rm = TRUE)) |>
    mutate(statistics = "med")
  vpc_data_min <- vpc_data_rep |>
    ungroup() |>
    group_by(Bins, .data[[group]]) |>
    summarize_all(~ as.numeric(quantile(.x, probs = (1 - ci) / 2, na.rm = TRUE))) |>
    mutate(statistics = "min")
  vpc_data_max <- vpc_data_rep |>
    ungroup() |>
    group_by(Bins, .data[[group]]) |>
    summarize_all(~ as.numeric(quantile(.x, probs = (1 + ci) / 2, na.rm = TRUE))) |>
    mutate(statistics = "max")

  vpc_data <- bind_rows(vpc_data_med, vpc_data_min, vpc_data_max) |>
    arrange(.data[[group]], Bins)

  vpc_data <- vpc_data |>
    dplyr::select(c(Bins, statistics, ends_with("min"), ends_with("max"), ends_with("med"))) |>
    tidyr::pivot_wider(
      values_from = c(ends_with("min"), ends_with("max"), ends_with("med")),
      names_from = c(statistics)
    )

  return(vpc_data)
}
