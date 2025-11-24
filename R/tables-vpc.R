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
  vpc_data <- data |>
    dplyr::mutate(bins = bin_values(.data[[x]], bins = bins)) |>
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
