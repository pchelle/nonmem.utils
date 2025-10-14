#' @title eta_plot
#' @description Pairs plot of etas
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @export
#' @import GGally
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

#' @title dv_preds
#' @description Plot DV vs PREDs
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @export
dv_preds <- function(data, meta_data = NULL) {
  # By default assumes usual Nonmem names and labels
  # TODO
  meta_data <- fill_meta_vars(meta_data) %||% default_meta_data
  variable_names <- sapply(
    c("mdv", "dv", "blq", "lloq"),
    function(x) pull_name(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  variable_labels <- sapply(
    variable_names,
    function(x) pull_label(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )

  p <- ggplot(
    data = fill_nonmem_vars(data) |>
      dplyr::filter(
        .data[[variable_names$mdv]] == 0,
        .data[[variable_names$blq]] <= 0
      ),
    mapping = aes(x = .data[[variable_names$dv]])
  ) +
    theme_bw() +
    theme(legend.position = "top", legend.direction = "vertical") +
    geom_rug(
      data = fill_nonmem_vars(data) |>
        dplyr::filter(
          .data[[variable_names$mdv]] == 0,
          .data[[variable_names$blq]] > 0
        ),
      mapping = aes(x = .data[[variable_names$lloq]], IPRED, text = tooltip_text(.data, names(data)))
    ) +
    geom_point(mapping = aes(y = PRED, text = tooltip_text(.data, names(data)), color = "Population")) +
    geom_point(mapping = aes(y = IPRED, text = tooltip_text(.data, names(data)), color = "Individual")) +
    geom_smooth(
      mapping = aes(x = .data[[variable_names$dv]], y = PRED, color = "Population (smoother)"),
      formula = y ~ x, method = "loess", se = FALSE
    ) +
    geom_smooth(
      mapping = aes(x = .data[[variable_names$dv]], y = IPRED, color = "Individual (smoother)"),
      formula = y ~ x, method = "loess", se = FALSE
    ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      x = paste("Observed", variable_labels$dv),
      y = paste("Predicted", variable_labels$dv),
      color = NULL
    ) +
    scale_color_manual(
      values = c(
        "Population" = "royalblue", "Individual" = "firebrick",
        "Population (smoother)" = "navy", "Individual (smoother)" = "tomato"
      )
    )

  return(p)
}

#' @title residual_plot
#' @description Plot residual (`y`) vs `x`
#' @param x_type Type of x variable
#' @param y_type Type of y variable
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @export
residual_plot <- function(x_type = "time", y_type = "cwres", data, meta_data = NULL) {
  # By default assumes usual Nonmem names and labels
  meta_data <- fill_meta_vars(meta_data) %||% default_meta_data
  variable_names <- sapply(
    c(x_type, "mdv", "dv", "blq"),
    function(x) pull_name(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  variable_names$pred <- "PRED"
  variable_names$ipred <- "IPRED"
  variable_labels <- sapply(
    variable_names,
    function(x) pull_label(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  variable_labels$pred <- paste("Population Predictions of", variable_labels$dv)
  variable_labels$ipred <- paste("Individual Predictions of", variable_labels$dv)

  res_data <- fill_nonmem_vars(data) |>
    dplyr::filter(
      .data[[variable_names$mdv]] == 0,
      .data[[variable_names$blq]] <= 0
    )

  p <- ggplot(
    data = res_data,
    mapping = aes(
      x = .data[[variable_names[[x_type]]]],
      y = .data[[toupper(y_type)]],
      text = tooltip_text(.data, names(res_data)),
      group = 1
    )
  ) +
    theme_bw() +
    geom_point(color = "grey30") +
    geom_smooth(formula = y ~ x, method = "loess", se = FALSE, color = "royalblue") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = variable_labels[[x_type]],
      y = toupper(y_type)
    )

  return(p)
}

#' @title residual_qq
#' @description Plot qq-plot of residuals (`y`)
#' @param y_type Type of y variable
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @export
residual_qq <- function(y_type = "cwres", data, meta_data = NULL) {
  # By default assumes usual Nonmem names and labels
  meta_data <- fill_meta_vars(meta_data) %||% default_meta_data
  variable_names <- sapply(
    c("mdv", "dv", "blq"),
    function(x) pull_name(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  variable_labels <- sapply(
    variable_names,
    function(x) pull_label(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  res_data <- fill_nonmem_vars(data) |>
    dplyr::filter(
      .data[[variable_names$mdv]] == 0,
      .data[[variable_names$blq]] <= 0
    )

  p <- ggplot(
    data = res_data,
    mapping = aes(sample = .data[[toupper(y_type)]])
  ) +
    theme_bw() +
    geom_qq(color = "grey30") +
    geom_qq_line(color = "royalblue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Normal distribution quantiles", y = paste(toupper(y_type), "quantiles"))

  return(p)
}

#' @title residual_plot
#' @description Plot histogram of residual (`y`)
#' @param y_type Type of y variable
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @param bins Number of bins
#' @export
residual_hist <- function(y_type = "cwres", data, meta_data = NULL, bins = 21) {
  # By default assumes usual Nonmem names and labels
  meta_data <- fill_meta_vars(meta_data) %||% default_meta_data
  variable_names <- sapply(
    c("mdv", "dv", "blq"),
    function(x) pull_name(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  variable_labels <- sapply(
    variable_names,
    function(x) pull_label(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )

  res_data <- fill_nonmem_vars(data) |>
    dplyr::filter(
      .data[[variable_names$mdv]] == 0,
      .data[[variable_names$blq]] <= 0
    )

  mean_res <- mean(res_data[[toupper(y_type)]])
  sd_res <- sd(res_data[[toupper(y_type)]])
  binwidth <- diff(range(res_data[[toupper(y_type)]])) / bins
  n_res <- length(res_data[[toupper(y_type)]])

  p <- ggplot(
    data = res_data,
    mapping = aes(x = .data[[toupper(y_type)]])
  ) +
    theme_bw() +
    geom_histogram(fill = "grey80", color = "grey30", bins = bins) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_vline(
      mapping = aes(xintercept = mean(.data[[toupper(y_type)]])),
      color = "royalblue"
    ) +
    stat_function(
      fun = function(x) {
        stats::dnorm(x, mean = mean_res, sd = sd_res) * n_res * binwidth
      },
      color = "royalblue"
    ) +
    labs(y = "Count")

  return(p)
}

#' @title time_profile
#' @description Plot TIME vs DV
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @param bins Number of bins to use for VPC, default is 7
#' @export
#' @examples
#'
#' # Requires BLQ and LLOQ
#' tp_meta <- dplyr::bind_rows(
#'   meta_data_501,
#'   data.frame(Name = c("BLQ", "LLOQ"), Type = c("blq", "lloq"), Label = c("BLQ", "LLOQ"))
#' )
#' tp_data <- data_501 |>
#'   dplyr::mutate(BLQ = 0, LLOQ = 1) |>
#'   dplyr::filter(MDV == 0)
#'
#' # Get the time profile plots
#' tp_plots <- time_profile(tp_data, tp_meta)
#'
#' # Plots are split by categorical variables, all use them all
#' names(tp_plots)
#'
#' # DV in linear scale
#' tp_plots$All$Linear
#'
#' # DV in log scale
#' tp_plots$All$Log
#'
#' # Data split by categorical covariate
#' tp_plots$SEX$Linear
#'
time_profile <- function(data, meta_data = NULL, bins = 7) {
  # By default assumes usual Nonmem names and labels
  meta_data <- fill_meta_vars(meta_data) %||% default_meta_data
  variable_names <- sapply(
    c("time", "mdv", "dv", "blq", "lloq"),
    function(x) pull_name(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  variable_labels <- sapply(
    variable_names,
    function(x) pull_label(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  categoricals <- meta_data |>
    filter(Name %in% pull_name("cat", meta_data))

  tp_data <- map_cat_data(fill_nonmem_vars(data), meta_data) |>
    dplyr::filter(.data[[variable_names$mdv]] == 0)

  vpc_data_all <- vpc_summary(
    tp_data,
    x = variable_names$time,
    y = variable_names$dv,
    bins = bins
  )
  blq_data_all <- vpc_summary(
    tp_data,
    x = variable_names$time,
    y = variable_names$blq,
    bins = bins
  )

  tp_plot <- base_tp_plot(
    data = tp_data,
    vpc_data = vpc_data_all,
    variable_names = variable_names,
    variable_labels = variable_labels
  )

  blq_plot <- ggplot(data = blq_data_all, mapping = aes(x = x, y = blq)) +
    theme_bw() +
    geom_line(color = "royalblue") +
    labs(x = variable_labels$time, y = "Percent BLQ [%]")

  if (nrow(categoricals) == 0) {
    return(list(All = list(
      "Linear" = tp_plot,
      "Log" = gg_log(tp_plot, x = FALSE),
      "Percent BLQ" = blq_plot
    )))
  }

  tp_plots <- list()
  for (cat_name in categoricals$Name) {
    vpc_data_cat <- vpc_summary(
      tp_data,
      x = variable_names$time,
      y = variable_names$dv,
      group = cat_name,
      bins = bins
    )
    blq_data_cat <- vpc_summary(
      tp_data,
      x = variable_names$time,
      y = variable_names$blq,
      group = cat_name,
      bins = bins
    )
    tp_plot_cat <- base_tp_plot(
      data = tp_data,
      vpc_data = vpc_data_cat,
      variable_names = variable_names,
      variable_labels = variable_labels
    ) + facet_wrap(as.formula(paste0("~", cat_name)))

    blq_plot_cat <- ggplot(data = blq_data_cat, mapping = aes(x = x, y = blq)) +
      theme_bw() +
      geom_line(color = "royalblue") +
      labs(x = variable_labels$time, y = "Percent BLQ [%]") +
      facet_wrap(as.formula(paste0("~", cat_name)))

    tp_plots[[cat_name]] <- list(
      "Linear" = tp_plot_cat,
      "Log" = gg_log(tp_plot_cat, x = FALSE),
      "Percent BLQ" = blq_plot_cat
    )
  }
  tp_plots[["All"]] <- list(
    "Linear" = tp_plot,
    "Log" = gg_log(tp_plot, x = FALSE),
    "Percent BLQ" = blq_plot
  )
  return(tp_plots)
}

#' @title tad_profile
#' @description Plot Time After Dose (TAD) vs DV
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @param bins Number of bins to use for VPC, default is 7
#' @export
#' @examples
#'
#' # Requires BLQ and LLOQ
#' tp_meta <- dplyr::bind_rows(
#'   meta_data_501,
#'   data.frame(Name = c("BLQ", "LLOQ"), Type = c("blq", "lloq"), Label = c("BLQ", "LLOQ"))
#' )
#' tp_data <- data_501 |> dplyr::mutate(BLQ = 0, LLOQ = 1)
#'
#' # Get the time profile plots
#' tp_plots <- tad_profile(tp_data, tp_meta)
#'
#' # Plots are split by categorical variables, all use them all
#' names(tp_plots)
#'
#' # DV in linear scale
#' tp_plots$All$Linear
#'
#' # DV in log scale
#' tp_plots$All$Log
#'
#' # Data split by categorical covariate
#' tp_plots$SEX$Linear
#'
tad_profile <- function(data, meta_data = NULL, bins = 7) {
  # By default assumes usual Nonmem names and labels
  meta_data <- fill_meta_vars(meta_data) %||% default_meta_data
  variable_names <- sapply(
    c("tad", "mdv", "dv", "blq", "lloq"),
    function(x) pull_name(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  variable_labels <- sapply(
    variable_names,
    function(x) pull_label(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  categoricals <- meta_data |>
    filter(Name %in% pull_name("cat", meta_data))

  tp_data <- map_cat_data(fill_nonmem_vars(data), meta_data) |>
    dplyr::filter(.data[[variable_names$mdv]] == 0)

  vpc_data_all <- vpc_summary(
    tp_data,
    x = variable_names$tad,
    y = variable_names$dv,
    bins = bins
  )
  blq_data_all <- vpc_summary(
    tp_data,
    x = variable_names$tad,
    y = variable_names$blq,
    bins = bins
  )

  tp_plot <- base_tp_plot(
    data = tp_data,
    vpc_data = vpc_data_all,
    variable_names = variable_names,
    variable_labels = variable_labels,
    use_tad = TRUE
  )

  blq_plot <- ggplot(data = blq_data_all, mapping = aes(x = x, y = blq)) +
    theme_bw() +
    geom_line(color = "royalblue") +
    labs(x = variable_labels$time, y = "Percent BLQ [%]")

  if (nrow(categoricals) == 0) {
    return(list(All = list(
      "Linear" = tp_plot,
      "Log" = gg_log(tp_plot, x = FALSE),
      "Percent BLQ" = blq_plot
    )))
  }

  tp_plots <- list()
  for (cat_name in categoricals$Name) {
    vpc_data_cat <- vpc_summary(
      tp_data,
      x = variable_names$tad,
      y = variable_names$dv,
      group = cat_name,
      bins = bins
    )
    blq_data_cat <- vpc_summary(
      tp_data,
      x = variable_names$tad,
      y = variable_names$blq,
      group = cat_name,
      bins = bins
    )
    tp_plot_cat <- base_tp_plot(
      data = tp_data,
      vpc_data = vpc_data_cat,
      variable_names = variable_names,
      variable_labels = variable_labels,
      use_tad = TRUE
    ) + facet_wrap(as.formula(paste0("~", cat_name)))

    blq_plot_cat <- ggplot(data = blq_data_cat, mapping = aes(x = x, y = blq)) +
      theme_bw() +
      geom_line(color = "royalblue") +
      labs(x = variable_labels$time, y = "Percent BLQ [%]") +
      facet_wrap(as.formula(paste0("~", cat_name)))

    tp_plots[[cat_name]] <- list(
      "Linear" = tp_plot_cat,
      "Log" = gg_log(tp_plot_cat, x = FALSE),
      "Percent BLQ" = blq_plot_cat
    )
  }
  tp_plots[["All"]] <- list(
    "Linear" = tp_plot,
    "Log" = gg_log(tp_plot, x = FALSE),
    "Percent BLQ" = blq_plot
  )
  return(tp_plots)
}

#' @title ind_time_profiles
#' @description Plot DV, PRED and IPRED vs TIME
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @param n_rows Number of rows in the plot grid, default is 2
#' @param n_cols Number of columns in the plot grid, default is 3
#' @export
#' @examples
#'
#' # Simulate 1-compartment model
#' pk_data <- data_501 |>
#'   dplyr::mutate(CL = 2, V = 40)
#' ind_time_profiles(pk_data, meta_data_501)
#'
ind_time_profiles <- function(data, meta_data = NULL, n_rows = 2, n_cols = 3) {
  # By default assumes usual Nonmem names and labels
  meta_data <- fill_meta_vars(meta_data) %||% default_meta_data
  variable_names <- sapply(
    c("id", "time", "mdv", "dv", "blq", "lloq"),
    function(x) pull_name(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  variable_labels <- sapply(
    variable_names,
    function(x) pull_label(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  sim_data <- simulate_pk(data)
  tp_data <- fill_nonmem_vars(data) |>
    dplyr::filter(.data[[variable_names$mdv]] == 0)

  unique_ids <- unique(tp_data[[variable_names$id]])
  n_plots <- ceiling(length(unique_ids) / (n_rows * n_cols))
  all_plots <- list()
  for (plot_index in seq_len(n_plots)) {
    selected_indices <- seq(
      (plot_index - 1) * n_rows * n_cols + 1,
      min(plot_index * n_rows * n_cols, length(unique_ids))
    )
    selected_ids <- unique_ids[selected_indices]

    all_plots[[plot_index]] <- ggplot(
      data = tp_data |>
        dplyr::filter(
          .data[[variable_names$blq]] <= 0,
          .data[[variable_names$id]] %in% selected_ids
        ),
      mapping = aes(x = .data[[variable_names$time]])
    ) +
      theme_bw() +
      theme(legend.position = "top", legend.direction = "horizontal") +
      geom_rug(
        data = tp_data |>
          dplyr::filter(
            .data[[variable_names$blq]] > 0,
            .data[[variable_names$id]] %in% selected_ids
          ),
        mapping = aes(x = .data[[variable_names$lloq]], IPRED, text = tooltip_text(.data, names(data)))
      ) +
      geom_line(
        data = sim_data |> filter(ID %in% selected_ids),
        mapping = aes(x = TIME, y = DV, text = tooltip_text(.data, names(sim_data)), group = ID, color = "Individual")
      ) +
      geom_point(mapping = aes(y = PRED, text = tooltip_text(.data, names(tp_data)), color = "Population")) +
      geom_point(mapping = aes(y = IPRED, text = tooltip_text(.data, names(tp_data)), color = "Individual")) +
      geom_point(mapping = aes(y = .data[[variable_names$dv]], text = tooltip_text(.data, names(tp_data)), color = "Observed")) +
      labs(x = variable_labels$time, y = variable_labels$dv, color = NULL) +
      scale_color_manual(
        values = c("Observed" = "black", "Population" = "royalblue", "Individual" = "firebrick")
      ) +
      facet_wrap(
        as.formula(paste0("~", variable_names$id)),
        nrow = n_rows, ncol = n_cols, scales = "free"
      )
  }

  return(all_plots)
}


#' @title ind_tad_profiles
#' @description Plot DV, PRED and IPRED vs TAD
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @param n_rows Number of rows in the plot grid, default is 2
#' @param n_cols Number of columns in the plot grid, default is 3
#' @export
#' @examples
#'
#' # Simulate 1-compartment model
#' pk_data <- data_501 |>
#'   dplyr::mutate(CL = 2, V = 40)
#' ind_tad_profiles(pk_data, meta_data_501)
#'
ind_tad_profiles <- function(data, meta_data = NULL, n_rows = 2, n_cols = 3) {
  # By default assumes usual Nonmem names and labels
  meta_data <- fill_meta_vars(meta_data) %||% default_meta_data
  variable_names <- sapply(
    c("id", "tad", "mdv", "dv", "blq", "lloq"),
    function(x) pull_name(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  variable_labels <- sapply(
    variable_names,
    function(x) pull_label(x, meta_data),
    USE.NAMES = TRUE, simplify = FALSE
  )
  sim_data <- simulate_pk(data)
  tp_data <- fill_nonmem_vars(data) |>
    dplyr::filter(.data[[variable_names$mdv]] == 0)

  unique_ids <- unique(tp_data[[variable_names$id]])
  n_plots <- ceiling(length(unique_ids) / (n_rows * n_cols))
  all_plots <- list()
  for (plot_index in seq_len(n_plots)) {
    selected_indices <- seq(
      (plot_index - 1) * n_rows * n_cols + 1,
      min(plot_index * n_rows * n_cols, length(unique_ids))
    )
    selected_ids <- unique_ids[selected_indices]

    all_plots[[plot_index]] <- ggplot(
      data = tp_data |>
        dplyr::filter(
          .data[[variable_names$blq]] <= 0,
          .data[[variable_names$id]] %in% selected_ids
        ),
      mapping = aes(x = .data[[variable_names$tad]])
    ) +
      theme_bw() +
      theme(legend.position = "top", legend.direction = "horizontal") +
      geom_rug(
        data = tp_data |>
          dplyr::filter(
            .data[[variable_names$blq]] > 0,
            .data[[variable_names$id]] %in% selected_ids
          ),
        mapping = aes(x = .data[[variable_names$lloq]], IPRED, text = tooltip_text(.data, names(data)))
      ) +
      geom_line(
        data = sim_data |> filter(ID %in% selected_ids),
        mapping = aes(x = tad, y = DV, text = tooltip_text(.data, names(sim_data)), group = ID, color = "Individual")
      ) +
      geom_point(mapping = aes(y = PRED, text = tooltip_text(.data, names(tp_data)), color = "Population")) +
      geom_point(mapping = aes(y = IPRED, text = tooltip_text(.data, names(tp_data)), color = "Individual")) +
      geom_point(mapping = aes(y = .data[[variable_names$dv]], text = tooltip_text(.data, names(tp_data)), color = "Observed")) +
      labs(x = variable_labels$tad, y = variable_labels$dv, color = NULL) +
      scale_color_manual(
        values = c("Observed" = "black", "Population" = "royalblue", "Individual" = "firebrick")
      ) +
      facet_wrap(
        as.formula(paste0("~", variable_names$id)),
        nrow = n_rows, ncol = n_cols, scales = "free"
      )
  }

  return(all_plots)
}

#' @title boot_hist
#' @description Histogram of bootstrap estimated (`y`)
#' @param y_type Type of y variable
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @param bins Number of bins
#' @param ci Confidence interval
#' @export
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
