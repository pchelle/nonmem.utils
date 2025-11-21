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
