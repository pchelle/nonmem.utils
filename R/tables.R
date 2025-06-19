#' @title cor_report
#' @description
#' Report of Spearman correlation with p-value
#' @param data A data.frame
#' @param x name of `x` variable
#' @param y name of `y` variable
#' @return A character of correlation and p-value
#' @export
cor_report <- function(data, x, y){
  cor_result <- tryCatch({
    cor.test(x = data[[x]], y = data[[y]], method = "spearman", exact = FALSE)
    },
    error = function(e) {list(estimate = NA, p.value = 1)}
  )

  cor_value <- paste0(
    sprintf("%.3f", cor_result$estimate),
    " (p",
    ifelse(cor_result$p.value<1e-3, "<", ":"),
    ifelse(cor_result$p.value<1e-3, "0.001", sprintf("%.3f", cor_result$p.value)),
    ")"
  )
  return(cor_value)
}

#' @title lm_report
#' @description
#' Report of lm coefficients with p-value
#' @param data A data.frame
#' @param x name of `x` variable
#' @param y name of `y` variable
#' @return A character of lm coefficients and p-value
#' @export
lm_report <- function(data, x, y){
  lm_result <- summary(lm(formula = as.formula(paste(y, "~", x)), data = data))
  lm_value <- paste0(
    gsub(pattern = x, replacement = "", row.names(lm_result$coefficients)),
    ":",
    sprintf("%.3f", lm_result$coefficients[,1]),
    " (p",
    ifelse(lm_result$coefficients[,4]<1e-3, "<", ":"),
    ifelse(lm_result$coefficients[,4]<1e-3, "0.001", sprintf("%.3f", lm_result$coefficients[,4])),
    ")",
    collapse = "/"
  )
  return(lm_value)
}

#' @title cov_cor
#' @description
#' Get a data.frame of Spearman correlation between covariates
#' @param data A data.frame of the pk data
#' @param meta_data A data.frame of meta data
#' @return A data.frame summarizing the data
#' @export
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

  for(cov_index_y in seq_along(cov_names)){
    cov_name_y <- cov_names[cov_index_y]
    cor_data[[cov_name_y]] <- ""
    # Continuous covariates: spearman correlation
    for(cov_index_x in seq_along(cov_names)){
      if(cov_index_x<=cov_index_y){
        next
      }
      cov_name_x <- cov_names[cov_index_x]
      cor_data[cov_index_x, cov_name_y] <- cor_report(sum_data, cov_name_x, cov_name_y)
    }
    # Categorical covariates: lm regression
    for(cat_index_x in seq_along(cat_names)){
      cat_name_x <- cat_names[cat_index_x]
      cor_data[length(cov_names)+cat_index_x, cov_name_y] <- lm_report(sum_data, cat_name_x, cov_name_y)
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

  for(eta_name in etas$Name){
    cor_data[[eta_name]] <- NA
    for(cov_index_x in seq_along(covariates$Name)){
      cov_name <- covariates$Name[cov_index_x]
      cor_data[cov_index_x, eta_name] <- cor_report(sum_data, cov_name, eta_name)
    }
    for(cat_index_x in seq_along(categoricals$Name)){
      cat_name <- categoricals$Name[cat_index_x]
      cor_data[length(covariates$Name)+cat_index_x, eta_name] <- lm_report(sum_data, cat_name, eta_name)
    }
  }
  names(cor_data) <- c("Covariates", etas$Label)
  return(cor_data)
}

#' @title data_inventory
#' @description
#' Get a data.frame summarizing data
#' @param data A data.frame of the pk data
#' @param meta_data A data.frame of meta data
#' @return A list of data.frame summarizing the data
#' @export
#' @import dplyr
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
#' Get a data.frame summarizing covariate data
#' @param data A data.frame of the pk data
#' @param meta_data A data.frame of meta data
#' @return A data.frame summarizing the data
#' @export
#' @import tidyr
#' @importFrom stats median quantile sd
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

#' @title bin_values
#' @description
#' Bin values into specified number of bins based on quantiles.
#' @param values A vector of values to be binned
#' @param bins Number of bins to create
#' @return A factor with the binned values
#' @export
bin_values <- function(values, bins = 7) {
  time_bins <- unique(quantile(x = values, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE))
  time_bins[1] <- floor(time_bins[1] * 100) / 100
  time_bins[length(time_bins)] <- ceiling(time_bins[length(time_bins)] * 100) / 100

  return(cut(values, breaks = time_bins, include.lowest = TRUE))
}


#' @title run_vpc
#' @description
#' Run a VPC analysis on the provided data.
#' #' @param data A data.frame of the pk data
#' @param x Name of `x` variable, usually `"TIME"`
#' @param group Name of `group` variable
#' @param bins Number of bins for the VPC
#' @param ci Confidence interval for the VPC, usually `0.8` (10th-90th percentiles)
#' @param lloq Lower limit of quantification
#' @return A data.frame
#' @export
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
