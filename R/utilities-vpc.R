#' @title bin_values
#' @description
#' Bin values into specified number of bins based on quantiles.
#' @param values A vector of values to be binned
#' @param bins Number of bins to create
#' @return A factor with the binned values
#' @export
#' @examples
#'
#' data_501 |> dplyr::mutate(Bins = bin_values(values = TIME, bins = 7))
#'
bin_values <- function(values, bins = 7) {
  # Makes a more robust binning based on the number of values
  unique_values <- unique(values)
  if(length(unique_values) <= bins) {
    return(factor(values, levels = unique_values))
  }
  time_bins <- unique(quantile(x = values, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE))
  time_bins[1] <- floor(time_bins[1] * 100) / 100
  time_bins[length(time_bins)] <- ceiling(time_bins[length(time_bins)] * 100) / 100

  return(cut(values, breaks = time_bins, include.lowest = TRUE))
}


#' @title run_vpc
#' @description
#' Run a VPC analysis on the provided data.
#' @param data A data.frame of the pk data
#' @param x Name of `x` variable, usually `"TIME"`
#' @param group Name of `group` variable
#' @param bins Number of bins for the VPC
#' @param ci Confidence interval for the VPC, usually `0.8` (10th-90th percentiles)
#' @param lloq Lower limit of quantification
#' @return A data.frame
#' @export
#' @examples
#'
#' run_vpc(data_501, x = "TIME")
#'
run_vpc <- function(data, x = "TIME", group = "All", bins = 7, ci = 0.8, lloq = 10) {
  cli::cli_h2("VPC Analysis")
  cli::cli_progress_step("Binning and preparing prediction-correction")
  # First step prepare data bins
  vpc_data <- fill_nonmem_vars(data) |>
    dplyr::filter(MDV == 0) |>
    dplyr::mutate(
      # observed BLQ data use IPRED
      OBS = ifelse(BLQ > 0, IPRED, OBS),
      Bins = bin_values(.data[[x]], bins = bins),
      sdPRED = stats::sd(PRED[REP==1], na.rm = TRUE)
    )
  cli::cli_alert_info("Calculated Bins: {.val {levels(vpc_data$Bins)}}")
  # Second step prepare correction factors for pc and pvc VPCs
  # medPRED = median value of typical predictions within time bin (used for pcVPC)
  # pcDV = Prediction-corrected simulated data
  # pcOBS = Prediction-corrected observed data
  vpc_data <- vpc_data |>
    dplyr::mutate(
      medPRED = median(PRED, na.rm = TRUE),
      sdBinsPRED = stats::sd(PRED, na.rm = TRUE),
      pcDV = DV * medPRED / PRED,
      pcOBS = OBS * medPRED / PRED,
      pvcDV = pcDV*sdBinsPRED/sdPRED,
      pvcOBS = pcOBS*sdBinsPRED/sdPRED,
      .by = c("REP", "Bins")
    )

  cli::cli_progress_step("Summarizing per {.field Bins} and Repetition {.field REP}")
  # Third step summarize median, min and max for all the variables for each repetition, bins and group
  vpc_data_rep <- vpc_data |>
    summarize(
      # Median values
      n = sum(!is.na(OBS)),
      time_med = median(.data[[x]], na.rm = TRUE),
      obs_med = median(OBS, na.rm = TRUE),
      dv_med = median(DV, na.rm = TRUE),
      pc_obs_med = median(pcOBS, na.rm = TRUE),
      pc_dv_med = median(pcDV, na.rm = TRUE),
      pvc_obs_med = median(pvcOBS, na.rm = TRUE),
      pvc_dv_med = median(pvcDV, na.rm = TRUE),
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
      pvc_obs_min = as.numeric(quantile(pvcOBS, probs = (1-ci)/2, na.rm = TRUE)),
      pvc_dv_min = as.numeric(quantile(pvcDV, probs = (1-ci)/2, na.rm = TRUE)),
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
      pvc_obs_max = as.numeric(quantile(pvcOBS, probs = (1+ci)/2, na.rm = TRUE)),
      pvc_dv_max = as.numeric(quantile(pvcDV, probs = (1+ci)/2, na.rm = TRUE)),
      npde_obs_max = stats::qnorm((1 + ci) / 2),
      npde_dv_max = as.numeric(quantile(NPDE, probs = (1 + ci) / 2, na.rm = TRUE)),
      blq_obs_max = 100 * sum(as.numeric(BLQ) > 0, na.rm = TRUE) / n(),
      blq_dv_max = 100 * sum(DV < LLOQ, na.rm = TRUE) / n(),
      .by = c("REP", "Bins", group)
    )
  cli::cli_progress_step("Summarizing across Repetitions {.field REP}")
  # Fourth step summarize across repetitions
  vpc_results <- sapply(
    vpc_types,
    FUN = function(vpc_type){
      obs_name <- vpc_variable_name(vpc_type, simulated = FALSE)
      sim_name <- vpc_variable_name(vpc_type, simulated = TRUE)
      vpc_data_rep |>
        ungroup() |>
        summarize(
          n = median(n, na.rm = TRUE),
          time = median(time_med, na.rm = TRUE),
          obs_med = median(.data[[paste0(obs_name, "med")]], na.rm = TRUE),
          obs_min = median(.data[[paste0(obs_name, "min")]], na.rm = TRUE),
          obs_max = median(.data[[paste0(obs_name, "max")]], na.rm = TRUE),
          sim_med_med = median(.data[[paste0(sim_name, "med")]], na.rm = TRUE),
          sim_med_min = as.numeric(quantile(
            .data[[paste0(sim_name, "med")]],
            probs = (1 - ci) / 2,
            na.rm = TRUE
            )),
          sim_med_max = as.numeric(quantile(
            .data[[paste0(sim_name, "med")]],
            probs = (1 + ci) / 2,
            na.rm = TRUE
          )),
          sim_min_med = median(.data[[paste0(sim_name, "min")]], na.rm = TRUE),
          sim_min_min = as.numeric(quantile(
            .data[[paste0(sim_name, "min")]],
            probs = (1 - ci) / 2,
            na.rm = TRUE
          )),
          sim_min_max = as.numeric(quantile(
            .data[[paste0(sim_name, "min")]],
            probs = (1 + ci) / 2,
            na.rm = TRUE
          )),
          sim_max_med = median(.data[[paste0(sim_name, "max")]], na.rm = TRUE),
          sim_max_min = as.numeric(quantile(
            .data[[paste0(sim_name, "max")]],
            probs = (1 - ci) / 2,
            na.rm = TRUE
          )),
          sim_max_max = as.numeric(quantile(
            .data[[paste0(sim_name, "max")]],
            probs = (1 + ci) / 2,
            na.rm = TRUE
          )),
          .by = c("Bins", group)
        )
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )
  return(c(vpc_results, list(data = vpc_data |> dplyr::filter(REP==1))))
}

vpc_variable_name <- function(type, simulated = TRUE){
  if(simulated){
    sim_var <- switch(type,
                      "vpc" = "dv_",
                      "pc_vpc" = "pc_dv_",
                      "pvc_vpc" = "pvc_dv_",
                      "blq" = "blq_dv_",
                      "npde" = "npde_dv_"
    )
    return(sim_var)
  }
  obs_var <- switch(type,
                    "vpc" = "obs_",
                    "pc_vpc" = "pc_obs_",
                    "pvc_vpc" = "pvc_obs_",
                    "blq" = "blq_obs_",
                    "npde" = "npde_obs_"
  )
  return(obs_var)
}

#' @title vpc_plots
#' @description
#' Generate VPC plots for a given dataset.
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @param x The x variable to use for VPC, default is "TAD"
#' @param bins Number of bins to use for VPC, default is 7
#' @param ci Confidence interval for the VPC plot, default is 0.8
#' @param lloq Lower limit of quantification, default is 10
#' @export
vpc_plots <- function(data, meta_data, x = "TAD", bins = 7, ci = 0.8, lloq = 10) {
  cli::cli_h1("VPC Plots: All")
  vpc_results <- run_vpc(data, x = x, group = "All", bins = bins, ci = ci, lloq = lloq)
  variable_labels <- list(
    x = pull_label(x, meta_data),
    y = pull_label("DV", meta_data)
  )
  vpc_plots_all <- sapply(
    vpc_types,
    function(vpc_type) {
      base_vpc_plot(
        data = vpc_results$data,
        vpc_data = vpc_results[[vpc_type]],
        variable_labels = variable_labels,
        x = x,
        ci = ci,
        type = vpc_type
      ) |>
        gg_lim(x = x, meta_data = meta_data)
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )
  # Group by cat
  # for(){
  # cli::cli_h1("VPC Plots: {All}")
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
#' @keywords internal
base_vpc_plot <- function(data,
                          vpc_data,
                          variable_labels,
                          x = "TAD",
                          ci = 0.8,
                          type = "vpc") {
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
    tp_plot <- ggplot(data = vpc_data, mapping = aes(x = .data[["time"]])) +
      theme_bw() +
      theme(legend.position = "top", legend.direction = "vertical") +
      geom_ribbon(
        aes(
          ymin = .data[["sim_min_min"]],
          ymax = .data[["sim_min_max"]],
          fill = .data[["sim_legend"]]
          ),
        alpha = 0.6
      ) +
      geom_ribbon(
        aes(
          ymin = .data[["sim_max_min"]],
          ymax = .data[["sim_max_max"]],
          fill = .data[["sim_legend"]]
        ),
        alpha = 0.6
      ) +
      geom_ribbon(
        aes(
          ymin = .data[["sim_med_min"]],
          ymax = .data[["sim_med_max"]],
          fill = .data[["sim_legend"]]
        ),
        alpha = 0.6
      ) +
      geom_line(aes(y = .data[["sim_min_med"]], color = .data[["sim_legend"]])) +
      geom_line(aes(y = .data[["sim_max_med"]], color = .data[["sim_legend"]])) +
      geom_line(aes(y = .data[["sim_med_med"]], color = .data[["sim_legend"]])) +
      geom_line(aes(y = .data[["obs_min"]], color = .data[["obs_legend"]])) +
      geom_line(aes(y = .data[["obs_max"]], color = .data[["obs_legend"]])) +
      geom_line(aes(y = .data[["obs_med"]], color = .data[["obs_legend"]])) +
      scale_color_manual(breaks = c(obs_legend, sim_legend), values = c("royalblue", "firebrick")) +
      scale_fill_manual(breaks = sim_legend, values = "tomato") +
      guides(fill = "none", alpha = "none") +
      labs(x = variable_labels$x, y = y_lab, color = NULL)

    return(tp_plot)
  }

  dv_var <- switch(type, "vpc" = "OBS", "pc_vpc" = "pcOBS", "pvc_vpc" = "pvcOBS", "npde" = "NPDE")

  tp_plot <- ggplot(data = vpc_data, mapping = aes(x = .data[["time"]])) +
    theme_bw() +
    theme(legend.position = "top", legend.direction = "vertical") +
    geom_point(
      data = data,
      mapping = aes(x = .data[[x]], y = .data[[dv_var]], alpha = ifelse(BLQ > 0, 0.6, 0.8))
    ) +
    geom_ribbon(
      aes(ymin = .data[["sim_min_min"]], ymax = .data[["sim_min_max"]], fill = .data[["sim_legend"]]),
      alpha = 0.6
    ) +
    geom_ribbon(
      aes(ymin = .data[["sim_max_min"]], ymax = .data[["sim_max_max"]], fill = .data[["sim_legend"]]),
      alpha = 0.6
    ) +
    geom_ribbon(
      aes(ymin = .data[["sim_med_min"]], ymax = .data[["sim_med_max"]], fill = .data[["sim_legend"]]),
      alpha = 0.6
    ) +
    geom_line(aes(y = .data[["sim_min_med"]], color = .data[["sim_legend"]])) +
    geom_line(aes(y = .data[["sim_max_med"]], color = .data[["sim_legend"]])) +
    geom_line(aes(y = .data[["sim_med_med"]], color = .data[["sim_legend"]])) +
    geom_line(aes(y = .data[["obs_min"]], color = .data[["obs_legend"]])) +
    geom_line(aes(y = .data[["obs_max"]], color = .data[["obs_legend"]])) +
    geom_line(aes(y = .data[["obs_med"]], color = .data[["obs_legend"]])) +
    scale_color_manual(breaks = c(obs_legend, sim_legend), values = c("royalblue", "firebrick")) +
    scale_fill_manual(breaks = sim_legend, values = "tomato") +
    scale_alpha_identity() +
    guides(fill = "none", alpha = "none") +
    labs(x = variable_labels$x, y = y_lab, color = NULL)
  return(tp_plot)
}
