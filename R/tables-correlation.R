#' @title cov_cor
#' @description
#' Get a data.frame of Spearman correlation between covariates
#' @param data A data.frame of the pk data
#' @param meta_data A data.frame of meta data
#' @return A data.frame summarizing the data
#' @export
#' @examples
#'
#' # Summarize data by ID
#' sum_data <- data_501 |>
#'   dplyr::group_by(ID) |>
#'   dplyr::summarise_all(dplyr::first)
#'
#' cov_cor(sum_data, meta_data_501)
#'
cov_cor <- function(data, meta_data) {
  id_variable <- pull_name("id", meta_data)
  if (length(id_variable) == 0) {
    cli::cli_alert_danger("No {.strong id} variable found in {.emph meta_data}")
    return()
  }
  covariates <- meta_data |> filter(Name %in% pull_name("cov", meta_data))
  categoricals <- meta_data |> filter(Name %in% pull_name("cat", meta_data))
  if (nrow(covariates) + nrow(categoricals) == 0) {
    cli::cli_alert_danger("No {.strong cov} nor {.strong cat} variable found in {.emph meta_data}")
    return()
  }
  sum_data <- data |>
    group_by(.data[[id_variable]]) |>
    summarise_all(first)
  if (nrow(categoricals) > 0) {
    sum_data <- map_cat_data(sum_data, meta_data)
  }

  cov_names <- covariates$Name
  cat_names <- categoricals$Name
  cov_labels <- stringi::stri_unescape_unicode(covariates$Label)
  cat_labels <- stringi::stri_unescape_unicode(categoricals$Label)

  cor_data <- data.frame(Covariates = c(cov_labels, cat_labels))

  for (cov_index_y in seq_along(cov_names)) {
    cov_name_y <- cov_names[cov_index_y]
    cor_data[[cov_name_y]] <- ""
    # Continuous covariates: spearman correlation
    for (cov_index_x in seq_along(cov_names)) {
      if (cov_index_x <= cov_index_y) {
        next
      }
      cov_name_x <- cov_names[cov_index_x]
      cor_data[cov_index_x, cov_name_y] <- cor_report(sum_data, cov_name_x, cov_name_y)
    }
    # Categorical covariates: lm regression
    for (cat_index_x in seq_along(cat_names)) {
      cat_name_x <- cat_names[cat_index_x]
      cor_data[length(cov_names) + cat_index_x, cov_name_y] <- lm_report(sum_data, cat_name_x, cov_name_y)
    }
  }
  names(cor_data) <- c("Covariates", cov_labels)
  return(cor_data)
}

#' @title eta_cor
#' @description
#' Get a data.frame of
#' - Spearman correlation between covariates and etas
#' - Wilcoxon test between categorical covariates and etas
#' @param data A data.frame of the pk data
#' @param meta_data A data.frame of meta data
#' @return A data.frame summarizing the data
#' @export
#' @examples
#'
#' # Summarize data by ID
#' sum_data <- data_501 |>
#'   dplyr::group_by(ID) |>
#'   dplyr::summarise_all(dplyr::first)
#'
#' eta_cor(sum_data, meta_data_501)
#'
eta_cor <- function(data, meta_data) {
  id_variable <- pull_name("id", meta_data)
  if (length(id_variable) == 0) {
    cli::cli_alert_danger("No {.strong id} variable found in {.emph meta_data}")
    return()
  }
  etas <- meta_data |> filter(Name %in% pull_name("eta", meta_data))
  covariates <- meta_data |> filter(Name %in% pull_name("cov", meta_data))
  categoricals <- meta_data |> filter(Name %in% pull_name("cat", meta_data))
  if (nrow(etas) == 0) {
    cli::cli_alert_danger("No {.strong eta} variable found in {.emph meta_data}")
    return()
  }
  sum_data <- data |>
    group_by(.data[[id_variable]]) |>
    summarise_all(first)
  if (nrow(categoricals) > 0) {
    sum_data <- map_cat_data(sum_data, meta_data)
  }

  cor_data <- data.frame(
    Covariates = stringi::stri_unescape_unicode(c(
      covariates$Label,
      categoricals$Label
    ))
  )

  for (eta_name in etas$Name) {
    cor_data[[eta_name]] <- NA
    for (cov_index_x in seq_along(covariates$Name)) {
      cov_name <- covariates$Name[cov_index_x]
      cor_data[cov_index_x, eta_name] <- cor_report(sum_data, cov_name, eta_name)
    }
    for (cat_index_x in seq_along(categoricals$Name)) {
      cat_name <- categoricals$Name[cat_index_x]
      cor_data[length(covariates$Name) + cat_index_x, eta_name] <- lm_report(sum_data, cat_name, eta_name)
    }
  }
  names(cor_data) <- c("Covariates", stringi::stri_unescape_unicode(etas$Label))
  return(cor_data)
}
