#' @title fill_nonmem_vars
#' @description
#' Fill in the nonmem variables that are not present in the data
#' @param data A data.frame of the pk data
#' @param lloq Lower limit of quantification (default is 10)
#' @param group Name of the grouping variable
#' @return A data.frame with the nonmem variables filled in
#' @export
fill_nonmem_vars <- function(data, lloq = 10, group = "All") {
  data_names <- names(data)
  if (isFALSE("ID" %in% data_names)) {
    data$ID <- 1
  }
  if (isFALSE("CID" %in% data_names)) {
    data$CID <- 1
  }
  if (isFALSE("OCC" %in% data_names)) {
    data$OCC <- 1
  }
  if (isFALSE("MDV" %in% data_names)) {
    data$MDV <- 0
  }
  if (isFALSE("EVID" %in% data_names)) {
    data$EVID <- 0
  }
  if (isFALSE("OBS" %in% data_names)) {
    data$OBS <- data$DV
  }
  if (isFALSE("IPRED" %in% data_names)) {
    data$IPRED <- data$DV
  }
  if (isFALSE("PRED" %in% data_names)) {
    data$PRED <- data$DV
  }
  if (isFALSE("BLQ" %in% data_names)) {
    data$BLQ <- 0
  }
  if (isFALSE("LLOQ" %in% data_names)) {
    data$LLOQ <- lloq
  }
  if (isFALSE("NPDE" %in% data_names)) {
    data$NPDE <- 0
  }
  if (isFALSE("REP" %in% data_names)) {
    data$REP <- 1
  }
  if (isFALSE(group %in% data_names)) {
    data[[group]] <- ""
  }
  return(data)
}


#' @title pull_cat
#' @description
#' Parse categorical variable names from meta_data
#' @param name Name of variable to retrieve
#' @param meta_data A data.frame of meta data
#' @export
pull_cat <- function(name, meta_data = NULL) {
  meta_data <- meta_data %||% default_meta_data
  unit <- meta_data |>
    dplyr::filter(Name %in% name) |>
    dplyr::pull(Unit)

  if (is.na(unit)) {
    return()
  }
  all_units <- unlist(strsplit(unit, "\\|"))
  cat_data <- lapply(
    all_units,
    function(unit_mapping) {
      map_list <- jsonlite::fromJSON(paste0("{", trimws(unit_mapping), "}"))
      map_data <- data.frame(value = names(map_list), label = as.character(map_list))
      return(map_data)
    }
  )
  cat_data <- do.call(rbind, cat_data)
  return(cat_data)
}

#' @title pull_name
#' @description
#' Pull Name from data `Type` field
#' @param type Type of variable to retrieve
#' @param meta_data A data.frame of meta data
#' @export
#' @examples
#' # Dictionary
#' meta_data <- readr::read_csv(
#' system.file("template-dictionary.csv", package = "nonmem.utils"),
#' na = c("NA", "N/A", "", ".")
#' )
#' meta_data
#'
#' pull_name("time", meta_data)
#'
pull_name <- function(type, meta_data = NULL) {
  meta_data <- meta_data %||% default_meta_data
  map_name <- meta_data |>
    dplyr::filter(Type %in% type) |>
    dplyr::pull(Name)
  return(map_name)
}

#' @title pull_label
#' @description
#' Pull Label potentially with Unit from `Name` field
#' @param name Name of variable to retrieve
#' @param meta_data A data.frame of meta data
#' @export
#' @examples
#' # Dictionary
#' meta_data <- readr::read_csv(
#' system.file("template-dictionary.csv", package = "nonmem.utils"),
#' na = c("NA", "N/A", "", ".")
#' )
#' meta_data
#'
#' pull_label("TIME", meta_data)
#'
pull_label <- function(name, meta_data = NULL) {
  meta_data <- meta_data %||% default_meta_data
  map_label <- meta_data |>
    dplyr::filter(Name %in% name) |>
    dplyr::arrange(match(Name, name)) |>
    dplyr::mutate(
      Unit = ifelse(Type %in% "cat", NA, Unit),
      with_Unit = ifelse(is.na(Unit), "", paste0(" [", Unit, "]")),
      Label_with_Unit = paste0(Label, with_Unit)
    ) |>
    dplyr::pull(Label_with_Unit)
  return(map_label)
}

#' @title pull_limits
#' @description
#' Pull limits `Name` field
#' @param name Name of variable to retrieve
#' @param meta_data A data.frame of meta data
#' @export
pull_limits <- function(name, meta_data = NULL) {
  meta_data <- meta_data %||% default_meta_data
  map_limits <- meta_data |>
    dplyr::filter(Name %in% name) |>
    dplyr::mutate(
      Min = ifelse(is.na(Min), -Inf, Min),
      Max = ifelse(is.na(Max), Inf, Max)
    )

  return(c(map_limits$Min, map_limits$Max))
}

#' @title map_cat_data
#' @description
#' Map categorical factor levels to a data.frame
#' @param data A data.frame of the pk data
#' @param meta_data A data.frame of meta data
#' @export
map_cat_data <- function(data, meta_data) {
  cat_variables <- pull_name("cat", meta_data)
  if (length(cat_variables) == 0) {
    return(data)
  }
  map_data <- data
  for (cat_variable in cat_variables) {
    cat_data <- pull_cat(cat_variable, meta_data)
    map_data <- map_data |>
      dplyr::mutate(across(
        all_of(cat_variable),
        ~ factor(.x, levels = cat_data$value, labels = cat_data$label)
      ))
  }
  return(map_data)
}

# TODO: create help to compute time after dose, predose, etc.
# get_tad <- function(){}
# get_predose <- function(){}
