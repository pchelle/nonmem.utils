eta_bar <- function(data, mapping, ...) {
  GGally::ggally_barDiag(data, mapping, ..., bins = 11, color = "black", fill = "grey80") +
    geom_vline(
      mapping = aes(xintercept = mean(.data[[mapping$x]])),
      color = "royalblue"
      ) +
    geom_vline(xintercept = 0, color = "firebrick", linetype = "dashed")
}

robust_cor <- function(data, mapping, ...) {
  tryCatch(
    {GGally::ggally_cor(data, mapping, ...)},
    error = function(e) {
      GGally::ggally_blank(...)
    }
  )
}

#' @title eta_plot
#' @description Pairs plot of etas
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @export
#' @import GGally
eta_plot <- function(data, meta_data = NULL) {
  id_variable <- pull_name("id", meta_data)
  etas <- meta_data |>
    filter(Name %in% pull_name("eta", meta_data))
  categoricals <- meta_data |>
    filter(Name %in% pull_name("cat", meta_data))
  if (nrow(etas) == 0) {
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
  etas <- meta_data |>
    filter(Name %in% pull_name("eta", meta_data))
  covariates <- meta_data |>
    filter(Name %in% pull_name("cov", meta_data))
  categoricals <- meta_data |>
    filter(Name %in% pull_name("cat", meta_data))
  if (nrow(etas) == 0) {
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
        continuous = "points", #wrap("smooth_loess", se = FALSE),
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
cov_plot <- function(data, meta_data = NULL) {
  id_variable <- pull_name("id", meta_data)
  covariates <- meta_data |>
    filter(Name %in% pull_name("cov", meta_data))
  categoricals <- meta_data |>
    filter(Name %in% pull_name("cat", meta_data))
  if (nrow(covariates) + nrow(categoricals) == 0) {
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
      continuous = wrap("barDiag", bins = 11),
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
  meta_data <- meta_data %||% read.csv(
    system.file("template-dictionary.csv", package = "nonmem.utils"),
    na = c("NA", "N/A", "", ".")
  )
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
    data = data |>
      dplyr::filter(
        .data[[variable_names$mdv]] == 0,
        .data[[variable_names$blq]] <= 0
      ),
    mapping = aes(x = .data[[variable_names$dv]])
  ) +
    theme_bw() +
    theme(legend.position = "top", legend.direction = "vertical") +
    geom_rug(
      data = data |>
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
  meta_data <- meta_data %||% read.csv(
    system.file("template-dictionary.csv", package = "nonmem.utils"),
    na = c("NA", "N/A", "", ".")
  )
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

  res_data <- data |>
    dplyr::filter(
      .data[[variable_names$mdv]] == 0,
      .data[[variable_names$blq]] <= 0
    )

  p <- ggplot(
    data = res_data,
    mapping = aes(
      x = .data[[variable_names[[x_type]]]],
      y = .data[[toupper(y_type)]]
    )
  ) +
    theme_bw() +
    geom_point(
      mapping = aes(text = tooltip_text(.data, names(res_data))),
      color = "grey30"
    ) +
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
  meta_data <- meta_data %||% read.csv(
    system.file("template-dictionary.csv", package = "nonmem.utils"),
    na = c("NA", "N/A", "", ".")
  )
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
  res_data <- data |>
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
#' @export
residual_hist <- function(y_type = "cwres", data, meta_data = NULL) {
  # By default assumes usual Nonmem names and labels
  meta_data <- meta_data %||% read.csv(
    system.file("template-dictionary.csv", package = "nonmem.utils"),
    na = c("NA", "N/A", "", ".")
  )
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

  res_data <- data |>
    dplyr::filter(
      .data[[variable_names$mdv]] == 0,
      .data[[variable_names$blq]] <= 0
    )

  p <- ggplot(
    data = res_data,
    mapping = aes(x = .data[[toupper(y_type)]])
  ) +
    theme_bw() +
    geom_histogram(fill = "grey80", color = "grey30") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_vline(
      mapping = aes(xintercept = mean(.data[[toupper(y_type)]])),
      color = "royalblue"
    ) +
    labs(y = "Count")

  return(p)
}

#' @title tooltip_text
#' @description
#' When interactive plots are generated using `plotly`,
#' `tootltip` will display text aesthetic allowing easy tracking of data.
#' @param data A data.frame of data
#' @param var_names A vector of variable names to display
#' @export
tooltip_text <- function(data, var_names) {
  text_display <- lapply(
    var_names,
    function(var_name) {
      paste("<b>", var_name, "</b>:", data[[var_name]])
    }
  )
  text_display <- do.call(paste, c(text_display, sep = "<br>"))
  return(text_display)
}

#' @title gg_log
#' @description Use pretty log scale plot
#' @param plot_object A ggplot object
#' @param x Logical, if `TRUE`, x-axis will be log scale
#' @param y Logical, if `TRUE`, y-axis will be log scale
#' @export
#' @import ggplot2
gg_log <- function(plot_object, x = TRUE, y = TRUE) {
  if (!any(x, y)) {
    return(plot_object)
  }
  log_plot_object <- plot_object
  if (x) {
    log_plot_object <- log_plot_object +
      scale_x_log10(
        breaks = scales::log_breaks(),
        labels = scales::label_log(),
        guide = "axis_logticks"
      )
  }
  if (y) {
    log_plot_object <- log_plot_object +
      scale_y_log10(
        breaks = scales::log_breaks(),
        labels = scales::label_log(),
        guide = "axis_logticks"
      )
  }
  return(log_plot_object)
}

#' @title plotly_log
#' @description
#' Use log scale when plotting with `plotly`
#' @param plotly_object A plotly object
#' @param x Logical, if `TRUE`, x-axis will be log scale
#' @param y Logical, if `TRUE`, y-axis will be log scale
#' @export
plotly_log <- function(plotly_object, x = TRUE, y = TRUE) {
  if (!any(x, y)) {
    return(plotly_object)
  }
  log_plotly_object <- plotly_object |>
    plotly::layout(
      xaxis = list(type = ifelse(x, "log", "lin")),
      yaxis = list(type = ifelse(y, "log", "lin"))
    )
  return(log_plotly_object)
}

#' @title gg_lim
#' @description
#' Apply `Min` and `Max` from `meta_data` as limits of the ggplot
#' @param plot_object A ggplot object
#' @param meta_data A meta_data data.frame
#' @export
gg_lim <- function(plot_object, meta_data, x = NULL, y = NULL) {
  x_lim <- NULL
  y_lim <- NULL
  if (!is.null(x)) {
    x_lim <- pull_limits(x, meta_data)
  }
  if (!is.null(y)) {
    y_lim <- pull_limits(y, meta_data)
  }
  plot_object + coord_cartesian(xlim = x_lim, ylim = y_lim)
}

if (FALSE) {
  getShrinkageResults <- function(data,
                                  etaName,
                                  etaBSV,
                                  etaDisplayName = etaName,
                                  binwidth = stats::sd(data[, etaName], na.rm = TRUE) / 2) {
    x <- data[, etaName]
    shrinkage <- 100 * (1 - stats::sd(x, na.rm = TRUE) / etaBSV)

    xFit <- seq(-max(abs(x)) * 1.1, max(abs(x)) * 1.1, length.out = 1e3)
    dataFit <- data.frame(
      x = xFit,
      yObserved = length(x) * binwidth * dnorm(xFit, mean = mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE)),
      yEstimated = length(x) * binwidth * dnorm(xFit, mean = 0, sd = etaBSV)
    )
    names(dataFit) <- c(etaName, "yObserved", "yEstimated")

    etaPlot <- ggplot2::ggplot(
      dataFit,
      mapping = ggplot2::aes_string(x = paste0("`", etaName, "`"))
    ) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_histogram(
        data = data, binwidth = binwidth,
        color = "black", fill = "grey80"
      ) +
      ggplot2::geom_line(
        mapping = ggplot2::aes_string(y = "yObserved"),
        color = "dodgerblue", size = 1
      ) +
      ggplot2::geom_line(
        mapping = ggplot2::aes_string(y = "yEstimated"),
        color = "firebrick", size = 1
      ) +
      ggplot2::geom_vline(xintercept = mean(x, na.rm = TRUE), color = "dodgerblue", size = 1) +
      ggplot2::geom_vline(xintercept = 0, color = "firebrick", size = 1) +
      labs(x = etaDisplayName, y = "Count", caption = paste0("shrinkage: ", round(shrinkage, 1), "%"))

    return(list(
      plot = etaPlot,
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      shrinkage = shrinkage
    ))
  }
}

#' @title time_profile
#' @description Plot TIME vs DV
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @export
time_profile <- function(data, meta_data = NULL, bins = 7) {
  # By default assumes usual Nonmem names and labels
  meta_data <- meta_data %||% read.csv(
    system.file("template-dictionary.csv", package = "nonmem.utils"),
    na = c("NA", "N/A", "", ".")
  )
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

  tp_data <- map_cat_data(data, meta_data) |>
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
#' @export
tad_profile <- function(data, meta_data = NULL, bins = 7) {
  # By default assumes usual Nonmem names and labels
  meta_data <- meta_data %||% read.csv(
    system.file("template-dictionary.csv", package = "nonmem.utils"),
    na = c("NA", "N/A", "", ".")
  )
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

  tp_data <- map_cat_data(data, meta_data) |>
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

#' @title base_tp_plot
#' @description Base time profile plot
#' @param data A data.frame of data
#' @param vpc_data A data.frame of VPC data
#' @param variable_names A list of variable names
#' @param variable_labels A list of variable labels
#' @param use_tad Logical, if `TRUE`, use TAD instead of time
base_tp_plot <- function(data,
                         vpc_data,
                         variable_names,
                         variable_labels,
                         use_tad = FALSE) {
  tp_plot <- ggplot(
    data = data |> filter(.data[[variable_names$blq]] <= 0),
    mapping = aes(x = .data[[variable_names[[ifelse(use_tad, "tad", "time")]]]])
  ) +
    theme_bw() +
    theme(legend.position = "top", legend.direction = "vertical") +
    geom_rug(
      data = data |> filter(.data[[variable_names$blq]] > 0),
      mapping = aes(y = .data[[variable_names$lloq]], text = tooltip_text(.data, names(data)))
    ) +
    geom_point(mapping = aes(y = .data[[variable_names$dv]], text = tooltip_text(.data, names(data)), color = "Observed")) +
    geom_line(
      data = vpc_data,
      mapping = aes(
        x = x, y = ymin,
        text = tooltip_text(.data, names(vpc_data)),
        group = "Median [5th-95th] Percentile",
        color = "Median [5th-95th] Percentile"
      ),
      linewidth = 0.75
    ) +
    geom_line(
      data = vpc_data,
      mapping = aes(
        x = x, y = ymax,
        text = tooltip_text(.data, names(vpc_data)),
        group = "Median [5th-95th] Percentile",
        color = "Median [5th-95th] Percentile"
      ),
      linewidth = 0.75
    ) +
    geom_line(
      data = vpc_data,
      mapping = aes(
        x = x, y = y,
        text = tooltip_text(.data, names(vpc_data)),
        group = "Median [5th-95th] Percentile",
        color = "Median [5th-95th] Percentile"
      ),
      linewidth = 0.75
    ) +
    labs(
      x = variable_labels[[ifelse(use_tad, "tad", "time")]],
      y = variable_labels$dv,
      color = NULL
    ) +
    scale_color_manual(
      values = c(
        "Observed" = "grey30",
        "Median [5th-95th] Percentile" = "royalblue"
      )
    )
  return(tp_plot)
}


#' @title vpc_plots
#' @description
#' Generate VPC plots for a given dataset.
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @export
vpc_plots <- function(data, meta_data, x = "TAD", bins = 7, ci = 0.8, lloq = 10) {
  vpc_data_all <- run_vpc(data, x = x, group = "All", bins = bins, ci = ci, lloq = lloq)
  obs_data <- fill_nonmem_vars(data) |>
    dplyr::filter(REP == REP[1], MDV == 0) |>
    dplyr::mutate(Bins = bin_values(.data[[x]], bins = bins)) |>
    dplyr::group_by(Bins) |>
    dplyr::mutate(
      time_med_med = .data[[x]],
      obs_med_med = ifelse(BLQ > 0, IPRED, OBS),
      med_pred = median(PRED, na.rm = TRUE),
      pc_obs_med_med = OBS * med_pred / PRED,
      blq_obs_med_med = as.numeric(BLQ > 0),
      npde_obs_med_med = NPDE
    )

  variable_labels <- list(
    x = pull_label(x, meta_data),
    y = pull_label("DV", meta_data)
  )
  vpc_plots_all <- sapply(
    c("vpc", "pc_vpc", "blq", "npde"),
    function(vpc_type) {
      base_vpc_plot(
        data = obs_data,
        vpc_data = vpc_data_all,
        variable_labels = variable_labels,
        ci = ci,
        type = vpc_type
      ) |>
        gg_lim(x = x, meta_data = meta_data)
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )
  return(vpc_plots_all)
}


#' @title base_vpc_plot
#' @description Base vpc plot
#' @param data A data.frame of data
#' @param vpc_data A data.frame of VPC data
#' @param variable_labels A list of variable labels
#' @param ci Confidence interval for the VPC plot
#' @param type Type of VPC plot, one of `"vpc"`, `"pc_vpc"`, `"pvc_vpc"`, `"blq"`, `"npde"`
#' @return A ggplot object
#' @export
base_vpc_plot <- function(data,
                          vpc_data,
                          variable_labels,
                          ci = 0.8,
                          type = "vpc") {
  obs_var <- switch(type,
    "vpc" = "obs_",
    "pc_vpc" = "pc_obs_",
    "pvc_vpc" = "pvc_obs_",
    "blq" = "blq_obs_",
    "npde" = "npde_obs_"
  )
  sim_var <- switch(type,
    "vpc" = "dv_",
    "pc_vpc" = "pc_dv_",
    "pvc_vpc" = "pvc_dv_",
    "blq" = "blq_dv_",
    "npde" = "npde_dv_"
  )
  obs_legend <- paste0("Observed Median, ", 50 * (1 - ci), "th and ", 50 * (1 + ci), "th Percentiles")
  sim_legend <- paste0("Simulated Median, ", 50 * (1 - ci), "th and ", 50 * (1 + ci), "th Percentiles")
  y_lab <- switch(type,
    "vpc" = variable_labels$y,
    "pc_vpc" = paste0("Prediction-corrected\n", variable_labels$y),
    "pvc_vpc" = paste0("Prediction-variability-corrected\n", variable_labels$y),
    "blq" = "Percent BLQ [%]",
    "npde" = "NPDE"
  )

  if (type %in% "blq") {
    tp_plot <- ggplot(data = vpc_data, mapping = aes(x = time_med_med)) +
      theme_bw() +
      theme(legend.position = "top", legend.direction = "vertical") +
      geom_ribbon(
        aes(
          ymin = .data[[paste0(sim_var, "min_min")]],
          ymax = .data[[paste0(sim_var, "min_max")]],
          fill = sim_legend
        ),
        alpha = 0.6
      ) +
      geom_ribbon(
        aes(
          ymin = .data[[paste0(sim_var, "max_min")]],
          ymax = .data[[paste0(sim_var, "max_max")]],
          fill = sim_legend
        ),
        alpha = 0.6
      ) +
      geom_ribbon(
        aes(
          ymin = .data[[paste0(sim_var, "med_min")]],
          ymax = .data[[paste0(sim_var, "med_max")]],
          fill = sim_legend
        ),
        alpha = 0.6
      ) +
      geom_line(aes(y = .data[[paste0(sim_var, "min_med")]], color = sim_legend)) +
      geom_line(aes(y = .data[[paste0(sim_var, "max_med")]], color = sim_legend)) +
      geom_line(aes(y = .data[[paste0(sim_var, "med_med")]], color = sim_legend)) +
      geom_line(aes(y = .data[[paste0(obs_var, "min_med")]], color = obs_legend)) +
      geom_line(aes(y = .data[[paste0(obs_var, "max_med")]], color = obs_legend)) +
      geom_line(aes(y = .data[[paste0(obs_var, "med_med")]], color = obs_legend)) +
      scale_color_manual(breaks = c(obs_legend, sim_legend), values = c("royalblue", "firebrick")) +
      scale_fill_manual(breaks = sim_legend, values = "tomato") +
      guides(fill = "none", alpha = "none") +
      labs(x = variable_labels$x, y = y_lab, color = NULL)

    return(tp_plot)
  }

  tp_plot <- ggplot(data = vpc_data, mapping = aes(x = time_med_med)) +
    theme_bw() +
    theme(legend.position = "top", legend.direction = "vertical") +
    geom_point(
      data = data,
      mapping = aes(
        y = .data[[paste0(obs_var, "med_med")]],
        alpha = ifelse(blq_obs_med_med > 0, 0.6, 1)
      )
    ) +
    geom_ribbon(
      aes(
        ymin = .data[[paste0(sim_var, "min_min")]],
        ymax = .data[[paste0(sim_var, "min_max")]],
        fill = sim_legend
      ),
      alpha = 0.6
    ) +
    geom_ribbon(
      aes(
        ymin = .data[[paste0(sim_var, "max_min")]],
        ymax = .data[[paste0(sim_var, "max_max")]],
        fill = sim_legend
      ),
      alpha = 0.6
    ) +
    geom_ribbon(
      aes(
        ymin = .data[[paste0(sim_var, "med_min")]],
        ymax = .data[[paste0(sim_var, "med_max")]],
        fill = sim_legend
      ),
      alpha = 0.6
    ) +
    geom_line(aes(y = .data[[paste0(sim_var, "min_med")]], color = sim_legend)) +
    geom_line(aes(y = .data[[paste0(sim_var, "max_med")]], color = sim_legend)) +
    geom_line(aes(y = .data[[paste0(sim_var, "med_med")]], color = sim_legend)) +
    geom_line(aes(y = .data[[paste0(obs_var, "min_med")]], color = obs_legend)) +
    geom_line(aes(y = .data[[paste0(obs_var, "max_med")]], color = obs_legend)) +
    geom_line(aes(y = .data[[paste0(obs_var, "med_med")]], color = obs_legend)) +
    scale_color_manual(breaks = c(obs_legend, sim_legend), values = c("royalblue", "firebrick")) +
    scale_fill_manual(breaks = sim_legend, values = "tomato") +
    guides(fill = "none", alpha = "none") +
    labs(x = variable_labels$x, y = y_lab, color = NULL)
}
