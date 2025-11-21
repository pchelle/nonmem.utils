#' @title dv_preds
#' @description Plot DV vs PREDs
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @export
#' @examples
#'
#' dv_preds(data_501, meta_data_501)
#'
dv_preds <- function(data, meta_data = NULL) {
  # By default assumes usual Nonmem names and labels
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
      mapping = aes(x = .data[[variable_names$lloq]], y = IPRED, text = tooltip_text(.data, names(data)))
    ) +
    geom_point(mapping = aes(y = PRED, text = tooltip_text(.data, names(data)), color = "Population")) +
    geom_point(mapping = aes(y = IPRED, text = tooltip_text(.data, names(data)), color = "Individual")) +
    geom_smooth(
      mapping = aes(x = .data[[variable_names$dv]], y = PRED), color = "navy",
      formula = y ~ x, method = "loess", se = FALSE
    ) +
    geom_smooth(
      mapping = aes(x = .data[[variable_names$dv]], y = IPRED), color = "tomato",
      formula = y ~ x, method = "loess", se = FALSE
    ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      x = paste("Observed", variable_labels$dv),
      y = paste("Predicted", variable_labels$dv),
      color = NULL
    ) +
    scale_color_manual(
      values = c("Population" = "royalblue", "Individual" = "firebrick")
    )

  return(p)
}

#' @title dv_pred
#' @description Plot DV vs PRED
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @export
#' @examples
#'
#' dv_pred(data_501, meta_data_501)
#'
dv_pred <- function(data, meta_data = NULL) {
  # By default assumes usual Nonmem names and labels
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
    mapping = aes(x = .data[[variable_names$dv]], y = .data[["PRED"]])
  ) +
    theme_bw() +
    theme(legend.position = "top", legend.direction = "vertical") +
    geom_rug(
      data = fill_nonmem_vars(data) |>
        dplyr::filter(
          .data[[variable_names$mdv]] == 0,
          .data[[variable_names$blq]] > 0
        ),
      mapping = aes(x = .data[[variable_names$lloq]], y = PRED, text = tooltip_text(.data, names(data)))
    ) +
    geom_point(color = "grey30") +
    geom_smooth(formula = y ~ x, method = "loess", se = FALSE, color = "royalblue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      x = paste("Observed", variable_labels$dv),
      y = paste("Population Predicted", variable_labels$dv),
      color = NULL
    )

  return(p)
}

#' @title dv_ipred
#' @description Plot DV vs IPRED
#' @param data A data.frame of data
#' @param meta_data A data.frame of meta data
#' @export
#' @examples
#'
#' dv_ipred(data_501, meta_data_501)
#'
dv_ipred <- function(data, meta_data = NULL) {
  # By default assumes usual Nonmem names and labels
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
    mapping = aes(x = .data[[variable_names$dv]], y = .data[["IPRED"]])
  ) +
    theme_bw() +
    theme(legend.position = "top", legend.direction = "vertical") +
    geom_rug(
      data = fill_nonmem_vars(data) |>
        dplyr::filter(
          .data[[variable_names$mdv]] == 0,
          .data[[variable_names$blq]] > 0
        ),
      mapping = aes(x = .data[[variable_names$lloq]], text = tooltip_text(.data, names(data)))
    ) +
    geom_point(color = "grey30") +
    geom_smooth(formula = y ~ x, method = "loess", se = FALSE, color = "royalblue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      x = paste("Observed", variable_labels$dv),
      y = paste("Fitted", variable_labels$dv),
      color = NULL
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
#' @examples
#'
#' residual_plot(
#' x_type = "time",
#' y_type = "cwres",
#' data = data_501,
#' meta_data = meta_data_501
#' )
#'
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
#' @examples
#'
#' residual_qq(
#' y_type = "cwres",
#' data = data_501,
#' meta_data = meta_data_501
#' )
#'
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
#' @examples
#'
#' residual_hist(
#' y_type = "cwres",
#' data = data_501,
#' meta_data = meta_data_501,
#' bins = 11
#' )
#'
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
