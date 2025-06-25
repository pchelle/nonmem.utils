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

#' @title base_vpc_plot
#' @description Base vpc plot
#' @param data A data.frame of data
#' @param vpc_data A data.frame of VPC data
#' @param variable_labels A list of variable labels
#' @param ci Confidence interval for the VPC plot
#' @param type Type of VPC plot, one of `"vpc"`, `"pc_vpc"`, `"pvc_vpc"`, `"blq"`, `"npde"`
#' @return A ggplot object
#' @keywords internal
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
