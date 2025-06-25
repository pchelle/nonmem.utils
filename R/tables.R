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
  covariates <- meta_data |>
    filter(Name %in% pull_name("cov", meta_data))
  categoricals <- meta_data |>
    filter(Name %in% pull_name("cat", meta_data))
  if (nrow(covariates) + nrow(categoricals) == 0) {
    return(data.frame())
  }
  sum_data <- data |>
    group_by(.data[[id_variable]]) |>
    summarise_all(first)
  if (nrow(categoricals) > 0) {
    sum_data <- map_cat_data(sum_data, meta_data)
  }

  cov_names <- covariates$Name
  cat_names <- categoricals$Name
  cov_labels <- covariates$Label
  cat_labels <- categoricals$Label

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
#' Spearman correlation between covariates and etas
#' Wilcoxon test between categorical covariates and etas
#' @param data A data.frame of the pk data
#' @param meta_data A data.frame of meta data
#' @return A data.frame summarizing the data
#' @export
eta_cor <- function(data, meta_data) {
  id_variable <- pull_name("id", meta_data)
  etas <- meta_data |>
    filter(Name %in% pull_name("eta", meta_data))
  covariates <- meta_data |>
    filter(Name %in% pull_name("cov", meta_data))
  categoricals <- meta_data |>
    filter(Name %in% pull_name("cat", meta_data))
  if (nrow(etas) == 0) {
    return(data.frame())
  }
  sum_data <- data |>
    group_by(.data[[id_variable]]) |>
    summarise_all(first)
  if (nrow(categoricals) > 0) {
    sum_data <- map_cat_data(sum_data, meta_data)
  }

  cor_data <- data.frame(Covariates = c(covariates$Label, categoricals$Label))

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
  names(cor_data) <- c("Covariates", etas$Label)
  return(cor_data)
}

#' @title data_inventory
#' @description
#' Get a list of data.frames summarizing data
#' @param data A data.frame of the pk data
#' @param meta_data A data.frame of meta data
#' @return A list of data.frame summarizing the data
#' @export
#' @import dplyr
#' @examples
#'
#' # Currently requires BLQ data
#' inventory_meta <- dplyr::bind_rows(
#'   meta_data_501,
#'   data.frame(Name = "BLQ", Type = "blq", Label = "Below LLOQ")
#' )
#' inventory_data <- data_501 |> dplyr::mutate(BLQ = 0)
#'
#' all_inventories <- data_inventory(inventory_data, inventory_meta)
#'
#' all_inventories$All
#'
#' all_inventories$Sex
#'
data_inventory <- function(data, meta_data) {
  variable_names <- sapply(
    c("id", "occ", "mdv", "amt", "dv", "blq", "cat"),
    function(x) pull_name(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  variable_labels <- sapply(
    variable_names,
    function(x) pull_label(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  # Summarize total number of subjects, pk studies, doses, observations and blq ratio
  # Summarize per number of subjects: pk studies, doses and observations
  # Summarize per number of pk study: doses and observations
  data_inventory_total <- fill_nonmem_vars(data) |>
    summarise(
      Subjects = n_distinct(.data[[variable_names$id]]),
      Studies = n_distinct(paste(.data[[variable_names$id]], .data[[variable_names$occ]])),
      Doses = sum(.data[[variable_names$amt]] > 0, na.rm = TRUE),
      Observations = sum(.data[[variable_names$mdv]] > 0, na.rm = TRUE),
      `Percent BLQ` = round(100 * sum(.data[[variable_names$blq]] > 0, na.rm = TRUE) / sum(.data[[variable_names$mdv]] > 0, na.rm = TRUE), 2),
    ) |>
    mutate(
      `Studies per Subject` = round(Studies / Subjects, 2),
      `Doses per Subject` = round(Doses / Subjects, 2),
      `Doses per Study` = round(Doses / Studies, 2),
      `Observations per Subject` = round(Observations / Subjects, 2),
      `Observations per Study` = round(Observations / Studies, 2)
    )

  # if meta data includes source type data
  # Perform same inventory grouped by source
  if (length(variable_names$cat) == 0) {
    return(list(All = data_inventory_total))
  }
  data_inventory_by_source <- sapply(
    variable_names$cat,
    function(cat_name) {
      map_cat_data(data, meta_data) |>
        group_by(.data[[cat_name]]) |>
        summarise(
          Subjects = n_distinct(.data[[variable_names$id]]),
          Studies = n_distinct(paste(.data[[variable_names$id]], .data[[variable_names$occ]])),
          Doses = sum(.data[[variable_names$amt]] > 0, na.rm = TRUE),
          Observations = sum(.data[[variable_names$mdv]] > 0, na.rm = TRUE),
          `Percent BLQ` = round(100 * sum(.data[[variable_names$blq]] > 0, na.rm = TRUE) / sum(.data[[variable_names$mdv]] > 0, na.rm = TRUE), 2),
        ) |>
        mutate(
          `Studies per Subject` = round(Studies / Subjects, 2),
          `Doses per Subject` = round(Doses / Subjects, 2),
          `Doses per Study` = round(Doses / Studies, 2),
          `Observations per Subject` = round(Observations / Subjects, 2),
          `Observations per Study` = round(Observations / Studies, 2)
        )
    },
    USE.NAMES = TRUE, simplify = FALSE
  )
  names(data_inventory_by_source) <- variable_labels$cat
  data_inventory <- c(data_inventory_by_source, list(All = data_inventory_total))
  return(data_inventory)
}

#' @title cov_inventory
#' @description
#' Get a list of data.frames summarizing covariate data
#' @param data A data.frame of the pk data
#' @param meta_data A data.frame of meta data
#' @return A list of data.frames summarizing the covariate data
#' @export
#' @import tidyr
#' @importFrom stats median quantile sd
#' @examples
#'
#' cov_inventories <- cov_inventory(data_501, meta_data_501)
#'
#' cov_inventories$All
#'
#' cov_inventories[["SEX : Female"]]
#'
#' cov_inventories[["SEX : Male"]]
#'
cov_inventory <- function(data, meta_data) {
  id_variable <- pull_name("id", meta_data)
  covariates <- meta_data |>
    filter(Name %in% pull_name("cov", meta_data))
  categoricals <- meta_data |>
    filter(Name %in% pull_name("cat", meta_data))
  if (nrow(covariates) == 0) {
    return()
  }
  sum_data <- fill_nonmem_vars(data) |>
    group_by(.data[[id_variable]]) |>
    summarise_all(first)

  covariate_summary <- sum_data |>
    select(all_of(covariates$Name)) |>
    pivot_longer(
      cols = everything(),
      names_to = "Covariate",
      values_to = "Values"
    ) |>
    group_by(Covariate) |>
    summarise(
      N = sum(!is.na(Values)),
      Mean = mean(Values, na.rm = TRUE),
      Median = median(Values, na.rm = TRUE),
      SD = sd(Values, na.rm = TRUE),
      Min = min(Values, na.rm = TRUE),
      Max = max(Values, na.rm = TRUE)
    ) |>
    # Round values to 2 decimal
    mutate_if(is.numeric, round, 2)

  covariate_summary <- covariate_summary |>
    pivot_longer(cols = c(-Covariate), names_to = "Statistics") |>
    pivot_wider(names_from = c(Covariate)) |>
    select(matches(c("Statistics", covariates$Name)))
  names(covariate_summary) <- c("Statistics", pull_label(covariates$Name, meta_data))

  if (nrow(categoricals) == 0) {
    return(list(All = covariate_summary))
  }
  sum_data <- map_cat_data(sum_data, meta_data)
  covariate_summaries <- list()
  for (cat_name in categoricals$Name) {
    for (cat_level in levels(sum_data[[cat_name]])) {
      covariate_summary_by_cat <- sum_data |>
        filter(.data[[cat_name]] %in% cat_level) |>
        select(all_of(covariates$Name)) |>
        pivot_longer(
          cols = everything(),
          names_to = "Covariate",
          values_to = "Values"
        ) |>
        group_by(Covariate) |>
        summarise(
          N = sum(!is.na(Values)),
          Mean = mean(Values, na.rm = TRUE),
          Median = median(Values, na.rm = TRUE),
          SD = sd(Values, na.rm = TRUE),
          Min = min(Values, na.rm = TRUE),
          Max = max(Values, na.rm = TRUE)
        ) |>
        # Round values to 2 decimal
        mutate_if(is.numeric, round, 2)

      covariate_summary_by_cat <- covariate_summary_by_cat |>
        pivot_longer(cols = c(-Covariate), names_to = "Statistics") |>
        pivot_wider(names_from = c(Covariate)) |>
        select(matches(c("Statistics", covariates$Name)))
      names(covariate_summary_by_cat) <- c("Statistics", pull_label(covariates$Name, meta_data))

      covariate_summaries[[paste(cat_name, ":", cat_level)]] <- covariate_summary_by_cat
    }
  }
  covariate_summaries[["All"]] <- covariate_summary
  return(covariate_summaries)
}

#' @title cat_inventory
#' @description
#' Get a data.frame summarizing categorical data
#' @param data A data.frame of the pk data
#' @param meta_data A data.frame of meta data
#' @return A data.frame summarizing the data
#' @export
#' @examples
#'
#' cat_inventories <- cat_inventory(data_501, meta_data_501)
#'
#' cat_inventories
#'
cat_inventory <- function(data, meta_data) {
  id_variable <- pull_name("id", meta_data)
  categoricals <- meta_data |>
    filter(Name %in% pull_name("cat", meta_data))
  if (nrow(categoricals) == 0) {
    return()
  }
  sum_data <- fill_nonmem_vars(data) |>
    group_by(.data[[id_variable]]) |>
    summarise_all(first)
  sum_data <- map_cat_data(sum_data, meta_data)
  # For each categorical, creates a table with categories, count and percent
  cat_summaries <- list()
  for (cat_index in seq_along(categoricals$Name)) {
    cat_name <- categoricals$Name[cat_index]
    cat_label <- categoricals$Label[cat_index]
    cat_summary <- sum_data |>
      select(all_of(cat_name)) |>
      group_by(.data[[cat_name]]) |>
      summarise(
        Count = n(),
        Percent = 100 * n() / nrow(sum_data)
      )
    names(cat_summary) <- c(cat_label, "Count", "Percent")
    cat_summaries[[cat_name]] <- cat_summary
  }
  max_rows <- max(sapply(cat_summaries, nrow))
  cat_summaries <- lapply(
    cat_summaries,
    function(cat_summary) {
      na_rows <- max_rows - nrow(cat_summary)
      if (na_rows == 0) {
        return(cat_summary)
      }
      cat_summary[seq(1 + max_rows - na_rows, max_rows), ] <- NA
      return(cat_summary)
    }
  )
  return(bind_cols(cat_summaries, .name_repair = "minimal"))
}

#' @title vpc_summary
#' @description
#' Get a data.frame summarizing data as a vpc
#' @param data A data.frame of the pk data
#' @param x Name of `x` variable
#' @param y Name of `y` variable
#' @param group Name of `group` variable
#' @param bins Number of bins if one value is provided.
#' Or edges of bins if an array is provided.
#' Default binning uses number of rows/10
#' @param stairstep Logical defining if VPC data.frame correspond to stairstep plot
#' @param ci Confidence interval (value between 0 and 1)
#' @return A data.frame summarizing the data distribution
#' with variables: `bins`, `x`, `n`, `y`, `ymin`, `ymax`, and `blq`
#' @export
#' @examples
#'
#' vpc_summary(
#'   data = data_501 |> dplyr::filter(MDV == 0),
#'   x = "TIME",
#'   y = "DV",
#'   bins = 7
#' )
#'
vpc_summary <- function(data, x, y, group = NULL, bins = 5, stairstep = FALSE, ci = 0.9) {
  if (is.null(group)) {
    group <- "group"
    data[[group]] <- ""
  }
  time_bins <- unique(quantile(x = data[[x]], probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE))
  time_bins[1] <- floor(time_bins[1] * 100) / 100
  time_bins[length(time_bins)] <- ceiling(time_bins[length(time_bins)] * 100) / 100

  vpc_data <- data |>
    dplyr::mutate(bins = bin_values(.data[[x]])) |>
    tidyr::complete(bins, .data[[group]]) |>
    dplyr::group_by(bins, .data[[group]]) |>
    dplyr::summarise(
      n = sum(!is.na(.data[[y]])),
      x = median(as.numeric(.data[[x]]), na.rm = TRUE),
      y = median(as.numeric(.data[[y]]), na.rm = TRUE),
      ymin = as.numeric(quantile(as.numeric(.data[[y]]), probs = (1 - ci) / 2, na.rm = TRUE)),
      ymax = as.numeric(quantile(as.numeric(.data[[y]]), probs = (1 + ci) / 2, na.rm = TRUE)),
      blq = 100 * sum(as.numeric(.data[[y]]) > 0, na.rm = TRUE) / n(),
      .groups = "drop_last"
    )
  vpc_data <- dplyr::as_tibble(vpc_data) |> dplyr::arrange(.data[[group]], bins)

  if (!stairstep) {
    return(vpc_data)
  }

  vpc_data_min <- vpc_data |> dplyr::mutate(x = time_bins[as.numeric(bins)])
  vpc_data_max <- vpc_data |> dplyr::mutate(x = time_bins[as.numeric(bins) + 1])
  vpc_data <- dplyr::bind_rows(vpc_data_min, vpc_data_max) |>
    dplyr::arrange(.data[[group]], bins)

  return(vpc_data)
}
