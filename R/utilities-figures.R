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
    {
      GGally::ggally_cor(data, mapping, ...)
    },
    error = function(e) {
      GGally::ggally_blank(...)
    }
  )
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
#' @param x A variable name for x-axis
#' @param y A variable name for y-axis
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

#' @title base_tp_plot
#' @description Base time profile plot
#' @param data A data.frame of data
#' @param vpc_data A data.frame of VPC data
#' @param variable_names A list of variable names
#' @param variable_labels A list of variable labels
#' @param use_tad Logical, if `TRUE`, use TAD instead of time
#' @keywords internal
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
        x = .data[["x"]], y = .data[["ymin"]],
        text = tooltip_text(.data, names(vpc_data)),
        group = "Median [5th-95th] Percentile",
        color = "Median [5th-95th] Percentile"
      ),
      linewidth = 0.75
    ) +
    geom_line(
      data = vpc_data,
      mapping = aes(
        x = .data[["x"]], y = .data[["ymax"]],
        text = tooltip_text(.data, names(vpc_data)),
        group = "Median [5th-95th] Percentile",
        color = "Median [5th-95th] Percentile"
      ),
      linewidth = 0.75
    ) +
    geom_line(
      data = vpc_data,
      mapping = aes(
        x = .data[["x"]], y = .data[["y"]],
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
