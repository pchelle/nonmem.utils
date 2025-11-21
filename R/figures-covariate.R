#' @title eta_plot
#' @description Pairs plot of etas
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @export
#' @import GGally
#' @examples
#'
#' eta_plot(data_501, meta_data_501)
#'
eta_plot <- function(data, meta_data = NULL) {
  id_variable <- pull_name("id", meta_data)
  if (length(id_variable) == 0) {
    cli::cli_alert_danger("No {.strong id} variable found in {.emph meta_data}")
    return()
  }
  etas <- meta_data |> filter(Name %in% pull_name("eta", meta_data))
  categoricals <- meta_data |> filter(Name %in% pull_name("cat", meta_data))
  if (nrow(etas) == 0) {
    cli::cli_alert_danger("No {.strong eta} variable found in {.emph meta_data}")
    return(ggplot())
  }
  sum_data <- data |>
    group_by(.data[[id_variable]]) |>
    summarise_all(first)

  p <- ggpairs(
    sum_data,
    columns = etas$Name,
    columnLabels = pull_label(etas$Name, meta_data),
    upper = list(continuous = robust_cor, na = "na"),
    lower = list(continuous = wrap("smooth_loess", se = FALSE, color = "royalblue"), na = "na"),
    diag = list(continuous = eta_bar, na = "naDiag")
  ) + theme_bw()

  all_eta_plots <- list(All = p)

  if (nrow(categoricals) == 0) {
    return(all_eta_plots)
  }
  sum_data <- map_cat_data(sum_data, meta_data)

  for (cat_name in categoricals$Name) {
    p <- ggpairs(
      sum_data,
      mapping = aes(color = .data[[cat_name]]),
      columns = etas$Name,
      columnLabels = pull_label(etas$Name, meta_data),
      upper = list(continuous = robust_cor, na = "na"),
      lower = list(continuous = "points", na = "na"),
      diag = list(continuous = wrap("barDiag", bins = 11), na = "naDiag")
    ) + theme_bw() +
      scale_colour_viridis_d()

    all_eta_plots[[cat_name]] <- p
  }
  return(all_eta_plots)
}

#' @title eta_cov_plot
#' @description Plot of etas vs covariates
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @export
#' @import GGally
#' @examples
#'
#' eta_cov_plot(data_501, meta_data_501)
#'
eta_cov_plot <- function(data, meta_data = NULL) {
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
    return(ggplot())
  }
  sum_data <- data |>
    group_by(.data[[id_variable]]) |>
    summarise_all(first)
  if (nrow(categoricals) > 0) {
    sum_data <- map_cat_data(sum_data, meta_data)
  }

  p <- ggduo(
    sum_data,
    columnsX = c(covariates$Name, categoricals$Name),
    columnsY = etas$Name,
    types = list(
      continuous = wrap("smooth_lm", se = FALSE, color = "royalblue"),
      comboVertical = "box_no_facet",
      comboHorizontal = "box_no_facet",
      discrete = "colbar",
      na = "na"
    ),
    columnLabelsX = pull_label(c(covariates$Name, categoricals$Name), meta_data),
    columnLabelsY = pull_label(etas$Name, meta_data),
    progress = FALSE,
    xlab = "Covariates",
    ylab = "\u03b7 variables"
  ) + theme_bw() +
    geom_hline(yintercept = 0, linetype = "dashed")

  all_eta_plots <- list(All = p)

  if (nrow(categoricals) == 0) {
    return(all_eta_plots)
  }

  for (cat_name in categoricals$Name) {
    p <- ggduo(
      sum_data,
      mapping = aes(color = .data[[cat_name]]),
      columnsX = covariates$Name,
      columnsY = etas$Name,
      types = list(
        continuous = "points", # wrap("smooth_loess", se = FALSE),
        comboVertical = "box_no_facet",
        comboHorizontal = "box_no_facet",
        discrete = "colbar",
        na = "na"
      ),
      columnLabelsX = pull_label(covariates$Name, meta_data),
      columnLabelsY = pull_label(etas$Name, meta_data),
      progress = FALSE,
      xlab = "Covariates",
      ylab = "\u03b7 variables",
      legend = 2,
    ) + theme_bw() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_colour_viridis_d() +
      labs(color = pull_label(cat_name, meta_data))

    all_eta_plots[[cat_name]] <- p
  }
  return(all_eta_plots)
}

#' @title cov_plot
#' @description Pairs plot of covariates
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @export
#' @import GGally
#' @examples
#'
#' cov_plot(data_501, meta_data_501)
#'
cov_plot <- function(data, meta_data = NULL) {
  id_variable <- pull_name("id", meta_data)
  if (length(id_variable) == 0) {
    cli::cli_alert_danger("No {.strong id} variable found in {.emph meta_data}")
    return()
  }
  covariates <- meta_data |> filter(Name %in% pull_name("cov", meta_data))
  categoricals <- meta_data |> filter(Name %in% pull_name("cat", meta_data))
  if (nrow(covariates) + nrow(categoricals) == 0) {
    cli::cli_alert_danger("No {.strong cov} nor {.strong cat} variable found in {.emph meta_data}")
    return(ggplot())
  }
  sum_data <- data |>
    group_by(.data[[id_variable]]) |>
    summarise_all(first)
  if (nrow(categoricals) > 0) {
    sum_data <- map_cat_data(sum_data, meta_data)
  }
  p <- ggpairs(
    sum_data,
    columns = c(covariates$Name, categoricals$Name),
    columnLabels = pull_label(c(covariates$Name, categoricals$Name), meta_data),
    upper = list(
      continuous = "cor",
      combo = "box_no_facet", # summarise_by
      discrete = "colbar",
      na = "na"
    ),
    lower = list(
      continuous = wrap("smooth_loess", se = FALSE, color = "royalblue"),
      combo = "box_no_facet",
      discrete = "colbar",
      na = "na"
    ),
    diag = list(
      continuous = wrap("barDiag", bins = 11, color = "black", fill = "grey80"),
      discrete = "barDiag",
      na = "naDiag"
    )
  ) + theme_bw()

  return(p)
}


#' @title boot_hist
#' @description Histogram of bootstrap estimated (`y`)
#' @param y_type Type of y variable
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @param bins Number of bins
#' @param ci Confidence interval
#' @export
#' @examples
#'
#' # Mimicking bootstrap results obtained through nonmem
#' boot_data <- data.frame(
#'   theta1 = stats::rlnorm(1e3, meanlog = log(2), sdlog = 1),
#'   theta2 = stats::rlnorm(1e3, meanlog = log(10), sdlog = 0.5),
#'   omega11 = stats::runif(1e3),
#'   sigma11 = stats::rlnorm(1e3, meanlog = 0, sdlog = 2)
#' )
#'
#' boot_meta <- data.frame(
#'   Name = c("theta1", "theta2", "omega11", "sigma11"),
#'   # Estimated values assessed by bootstrap
#'   Value = c(2, 10, 0.5, 1),
#'   Label = c("Clearance", "Volume", "BSV", "RUV: SD"),
#'   Unit = c("L/h", "L", "%", "mg/L")
#' )
#'
#' # Fixed effects
#' p_theta <- boot_hist(y_type = "theta", data = boot_data, meta_data = boot_meta)
#' patchwork::wrap_plots(p_theta)
#'
#' # Random effects
#' p_omega <- boot_hist(y_type = "omega", data = boot_data, meta_data = boot_meta)
#' p_sigma <- boot_hist(y_type = "sigma", data = boot_data, meta_data = boot_meta)
#' patchwork::wrap_plots(c(p_omega, p_sigma))
#'
boot_hist <- function(y_type = "theta", data, meta_data = NULL, bins = 11, ci = 0.95) {
  # By default assumes usual Nonmem names and labels
  type_meta_data <- meta_data |> filter(grepl(pattern = y_type, x = Name))
  if (y_type %in% "sigma") {
    type_meta_data$Value <- sqrt(type_meta_data$Value) *
      ifelse(grepl("CV", type_meta_data$Unit), 100, 1)
    for (row_index in 1:nrow(type_meta_data)) {
      data[[type_meta_data$Name[row_index]]] <- sqrt(data[[type_meta_data$Name[row_index]]]) *
        ifelse(grepl(pattern = "CV", type_meta_data$Unit[row_index]), 100, 1)
    }
  }
  if (y_type %in% "omega") {
    mat_indices <- gsub(pattern = "omega", replacement = "", type_meta_data$Name) |>
      as.integer()
    # Correlation
    for (row_index in 1:nrow(type_meta_data)) {
      if (mat_indices[row_index] %% 11 == 0) {
        next
      }
      index_1 <- mat_indices %% 10
      index_2 <- (mat_indices - index_1) / 10
      omega_names <- paste0("omega", rep(c(index_1, index_2), c(index_1, index_2)))
      omegas <- type_meta_data |>
        filter(Name %in% omega_names) |>
        pull(Value)
      type_meta_data$Value[row_index] <- type_meta_data$Value[row_index] /
        sqrt(omegas[1] * omegas[2])
      data[[type_meta_data$Name[row_index]]] <- data[[type_meta_data$Name[row_index]]] /
        sqrt(data[[omega_names[1]]] * data[[omega_names[2]]])
    }
    # CV
    for (row_index in 1:nrow(type_meta_data)) {
      if (mat_indices[row_index] %% 11 != 0) {
        next
      }
      type_meta_data$Value[row_index] <- 100 * sqrt(type_meta_data$Value[row_index])
      data[[type_meta_data$Name[row_index]]] <- 100 * sqrt(data[[type_meta_data$Name[row_index]]])
    }
  }
  type_meta_data <- split(type_meta_data, 1:nrow(type_meta_data))

  boot_plots <- sapply(
    type_meta_data,
    function(meta_data_index) {
      bootstrap_values <- quantile(
        data[[meta_data_index$Name]],
        probs = c(0.5, (1 - ci) / 2, (1 + ci) / 2)
      )
      variable_label <- meta_data_index$Label
      if (!is.na(meta_data_index$Unit)) {
        variable_label <- paste0(variable_label, " [", meta_data_index$Unit, "]")
      }

      ggplot(data = data) +
        theme_bw() +
        geom_histogram(
          mapping = aes(
            x = .data[[meta_data_index$Name]],
            y = after_stat(100 * count / sum(count))
          ),
          bins = bins,
          color = "black", fill = "grey80"
        ) +
        geom_vline(xintercept = bootstrap_values[1], color = "dodgerblue") +
        geom_vline(xintercept = bootstrap_values[2:3], color = "dodgerblue", linetype = "longdash") +
        geom_vline(xintercept = meta_data_index$Value, color = "firebrick") +
        labs(x = stringi::stri_unescape_unicode(variable_label), y = "Frequency [%]")
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )
  return(boot_plots)
}
