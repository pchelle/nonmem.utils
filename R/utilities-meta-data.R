#' @title dictionary_check
#' @description
#' Check if consistency between `data` and `meta_data`
#' @param data A data.frame of the pk data
#' @param meta_data A data.frame of meta data
#' @return A data.frame with the meta data variables filled in
#' @export
dictionary_check <- function(data, meta_data){
  data_names <- names(data)
  #meta_data$Name
  # Check if all meta data variables are present in the data
  meta_not_in_data <- setdiff(meta_data$Name, data_names)
  if(length(meta_not_in_data)>0){
    cli::cli_alert_warning("{.emph meta_data} variable Names not found in {.emph data}: {.val {meta_not_in_data}}")
  }
  # Check if all data variables are present in the meta_data
  data_not_in_meta <- setdiff(data_names, meta_data$Name)
  if(length(data_not_in_meta)>0){
    cli::cli_alert_warning("{.emph data} variables not found in {.emph meta_data} Names: {.val {data_not_in_meta}}")
  }
  return(invisible())
}

#' @title fill_meta_vars
#' @description
#' Fill required meta data types and variables
#' @param meta_data A data.frame of meta data
#' @return A data.frame with the meta data variables filled in
#' @export
fill_meta_vars <- function(meta_data = NULL){
  if(is.null(meta_data)){
    return(NULL)
  }
  meta_data_types <- meta_data$Type
  # Filling ID variable
  if (isFALSE("id" %in% meta_data_types)) {
    meta_data <- bind_rows(
      meta_data,
      data.frame(Name = "ID", Type = "id", Label = "Subject ID")
    )
  }
  # Filling OCC variable
  if (isFALSE("occ" %in% meta_data_types)) {
    meta_data <- bind_rows(
      meta_data,
      data.frame(Name = "ID", Type = "occ", Label = "Occasion")
    )
  }
  # Filling TIME variable
  if (isFALSE("time" %in% meta_data_types)) {
    meta_data <- bind_rows(
      meta_data,
      data.frame(Name = "TIME", Type = "time", Label = "Time", Unit = "h")
    )
  }
  # Filling TAD variable
  if (isFALSE("tad" %in% meta_data_types)) {
    meta_data <- bind_rows(
      meta_data,
      data.frame(Name = "TIME", Type = "tad", Label = "Time after dose", Unit = "h")
    )
  }
  # Filling EVID variable
  if (isFALSE("evid" %in% meta_data_types)) {
    meta_data <- bind_rows(
      meta_data,
      data.frame(Name = "EVID", Type = "evid", Label = "Event ID")
    )
  }
  # Filling MDV variable
  if (isFALSE("mdv" %in% meta_data_types)) {
    meta_data <- bind_rows(
      meta_data,
      data.frame(Name = "MDV", Type = "mdv", Label = "Missing Dependent Variable")
    )
  }
  # Filling BLQ variable
  if (isFALSE("blq" %in% meta_data_types)) {
    meta_data <- bind_rows(
      meta_data,
      data.frame(Name = "BLQ", Type = "blq", Label = "BLQ")
    )
  }
  # Filling BLQ variable
  if (isFALSE("lloq" %in% meta_data_types)) {
    meta_data <- bind_rows(
      meta_data,
      data.frame(Name = "LLOQ", Type = "lloq", Label = "LLOQ")
    )
  }
  return(meta_data)
}

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
  # Filling ID variable
  no_id <- all(isFALSE("ID" %in% data_names), isFALSE("CID" %in% data_names))
  if (no_id) {
    data$ID <- 1
  }
  if (isFALSE("ID" %in% data_names)) {
    data$ID <- data$CID
  }
  # AMT
  if (isFALSE("AMT" %in% data_names)) {
    data$AMT <- 0
  }
  # EVID
  if (isFALSE("EVID" %in% data_names)) {
    data$EVID <- ifelse(is.na(data$AMT), 0, as.numeric(data$AMT>0))
  }
  # OCC
  if (isFALSE("OCC" %in% data_names)) {
    data$OCC <- stats::ave(
      data$EVID,
      data$ID,
      FUN = function(x) {cumsum(x >= 3)+1}
      )
  }
  # MDV
  if (isFALSE("MDV" %in% data_names)) {
    data$MDV <- as.numeric(data$EVID>0)
  }
  # BLQ handling
  if (isFALSE("LLOQ" %in% data_names)) {
    data$LLOQ <- lloq
  }
  # Diagnostic plots
  for(var_name in c("OBS", "PRED", "IPRED")){
    if (isFALSE(var_name %in% data_names)) {
      data[[var_name]] <- data$DV
      }
  }
  for(var_name in c("BLQ", "CWRES", "NPDE")){
    if (isFALSE(var_name %in% data_names)) {
      data[[var_name]] <- 0
    }
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
#' @examples
#' # Dictionary
#' meta_data_501
#'
#' pull_cat("SEX", meta_data_501)
#'
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
#' meta_data_501
#'
#' pull_name("time", meta_data_501)
#'
#' pull_name("cov", meta_data_501)
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
#' meta_data_501
#'
#' pull_label("TIME", meta_data_501)
#'
#' pull_label(c("WT", "AGE", "SEX"), meta_data_501)
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
#' @examples
#' # Dictionary
#' meta_data_501
#'
#' pull_limits("TIME", meta_data_501)
#'
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
#' @examples
#' # Dictionary
#' meta_data_501
#'
#' # Data
#' head(data_501)
#'
#' # Map data
#' head(map_cat_data(data_501, meta_data_501))
#'
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
# get_predose <- function(){}
