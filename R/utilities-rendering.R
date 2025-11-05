#' @title report_dataset_analysis
#' @description
#' Render a docx document that analyses a dataset
#' @param data_path Path to the data file
#' @param meta_data_path Path to the metadata file
#' @param report_path Path to the output report file
#' @param bins Number of bins for time profile plots
#' @export
#' @family report
report_dataset_analysis <- function(data_path,
                                    meta_data_path,
                                    report_path = "dataset-analysis.docx",
                                    bins = 7) {
  temp_dir <- "tempdir101"
  dir.create(temp_dir, showWarnings = FALSE)
  file.copy(system.file("quarto", "dataset-analysis.qmd", package = "nonmem.utils"), temp_dir, overwrite = TRUE)
  rmarkdown::render(
    file.path(temp_dir, "dataset-analysis.qmd"),
    params = list(
      data_path = data_path,
      meta_data_path = meta_data_path,
      bins = bins
    )
  )
  file.copy(file.path(temp_dir, "dataset-analysis.docx"), report_path, overwrite = TRUE)
  unlink(temp_dir, recursive = TRUE)
  return(file.exists(report_path))
}

#' @title report_gof_analysis
#' @description
#' Render a docx document that analyses goodness of fit results
#' @param data_path Path to the data file
#' @param meta_data_path Path to the metadata file
#' @param report_path Path to the output report file
#' @export
#' @family report
report_gof_analysis <- function(data_path,
                                meta_data_path,
                                report_path = "gof-analysis.docx") {
  temp_dir <- "tempdir101"
  dir.create(temp_dir, showWarnings = FALSE)
  file.copy(system.file("quarto", "gof-analysis.qmd", package = "nonmem.utils"), temp_dir, overwrite = TRUE)
  rmarkdown::render(
    file.path(temp_dir, "gof-analysis.qmd"),
    params = list(
      data_path = data_path,
      meta_data_path = meta_data_path
    )
  )
  file.copy(file.path(temp_dir, "gof-analysis.docx"), report_path, overwrite = TRUE)
  unlink(temp_dir, recursive = TRUE)
  return(file.exists(report_path))
}

#' @title report_workflow_summary
#' @description
#' Render a docx document that analyses the nonmem results
#' @param dir_path Path to the nonmem results directory
#' @param report_path Path to the output report file
#' @export
#' @family report
report_workflow_summary <- function(dir_path,
                                    report_path = "workflow-summary.docx") {
  temp_dir <- "tempdir101"
  dir.create(temp_dir, showWarnings = FALSE)
  file.copy(system.file("quarto", "workflow-summary.qmd", package = "nonmem.utils"), temp_dir, overwrite = TRUE)
  rmarkdown::render(
    file.path(temp_dir, "workflow-summary.qmd"),
    params = list(dir_path = dir_path)
  )
  file.copy(file.path(temp_dir, "workflow-summary.docx"), report_path, overwrite = TRUE)
  unlink(temp_dir, recursive = TRUE)
  return(file.exists(report_path))
}

#' @title report_limited_sampling_analysis
#' @description
#' Render a docx document that report limited sampling analyses results
#' @param data_path Path to the data file
#' @param model_path Path to the `.cpp` model file
#' @param sampling_path Path to the sampling strategies `.xslx` file.
#' Template file available at `system.file("Sampling-Strategies.xlsx", package = "nonmem.utils")`
#' @param run_folder Path to the folder where the runs will be stored
#' @param pop_size Population size for the analysis
#' @param pop_cv Coefficient of variation for the population
#' @param covariates Covariate names to be used in the analysis
#' @param categoricals Categorical covariate names to be used in the analysis
#' @param lloq Lower limit of quantification for the analysis
#' @param baseline Baseline value for the analysis
#' @param y_lim Y-axis limits for the plots
#' @param report_path Path to the output report file
#' @export
#' @family report
report_limited_sampling_analysis <- function(model_path,
                                             data_path,
                                             sampling_path = "Sampling-Strategies.xlsx",
                                             run_folder = "LSA_Runs",
                                             pop_size = 1000,
                                             pop_cv = 0.025,
                                             covariates = c("BW", "AGE", "FFM"),
                                             categoricals = NULL,
                                             lloq = 0.01,
                                             baseline = 0.005,
                                             y_lim = 50,
                                             report_path = "limited-sampling-analysis.docx") {
  temp_dir <- "tempdir101"

  dir.create(temp_dir, showWarnings = FALSE)
  file.copy(system.file("quarto", "limited-sampling-analysis.qmd", package = "nonmem.utils"), temp_dir, overwrite = TRUE)
  rmarkdown::render(
    file.path(temp_dir, "limited-sampling-analysis.qmd"),
    params = list(
      model_path = model_path,
      data_path = data_path,
      sampling_path = sampling_path,
      run_folder = run_folder,
      pop_size = pop_size,
      pop_cv = pop_cv,
      covariates = covariates,
      categoricals = categoricals,
      lloq = lloq,
      baseline = baseline,
      y_lim = y_lim
    )
  )
  file.copy(file.path(temp_dir, "limited-sampling-analysis.docx"), report_path, overwrite = TRUE)
  unlink(temp_dir, recursive = TRUE)
  return(file.exists(report_path))
}


#' @title report_dataset_analysis
#' @description
#' Render a docx document that analyses a dataset
#' @param data_path Path to the data file
#' @param meta_data_path Path to the metadata file
#' @param report_path Path to the output report file
#' @param bins Number of bins for time profile plots
#' @param ci Confidence interval for the analysis
#' @export
#' @family report
report_vpc_analysis <- function(data_path,
                                meta_data_path,
                                report_path = "vpc-analysis.docx",
                                bins = 7,
                                ci = 0.8) {
  temp_dir <- "tempdir101"
  dir.create(temp_dir, showWarnings = FALSE)
  file.copy(system.file("quarto", "vpc-analysis.qmd", package = "nonmem.utils"), temp_dir, overwrite = TRUE)
  rmarkdown::render(
    file.path(temp_dir, "vpc-analysis.qmd"),
    params = list(
      data_path = data_path,
      meta_data_path = meta_data_path,
      bins = bins,
      ci = 100 * ci
    )
  )
  file.copy(file.path(temp_dir, "vpc-analysis.docx"), report_path, overwrite = TRUE)
  unlink(temp_dir, recursive = TRUE)
  return(file.exists(report_path))
}

#' @title typst_table
#' @description
#' Print a table in raw typst for pdf document rendering
#' @param data A data.frame to render
#' @param linebreak Line break pattern to be subbed by typst line break
#' @export
#' @family report
#' @examples
#'
#' data_with_linebreak <- data.frame(Example = c("$10^2$<br>$-->$", "#sym.checkmark"))
#' typst_table(data_with_linebreak)
#'
typst_table <- function(data, linebreak = "<br>") {
  table_header <- paste0("[*", names(data), "*]", collapse = "")
  table_content <- c(
    "```{=typst}",
    "#table(",
    "  stroke: 1pt,",
    "  align: horizon,",
    paste0("  columns: ", ncol(data), ","),
    paste0("  table.header", table_header, ",")
  )
  for (row_index in seq_len(nrow(data))) {
    row_values <- as.character(as.data.frame(data[row_index, ]))
    row_values <- ifelse(grepl("text\\(", row_values), row_values, paste0("[", row_values, "]"))
    table_row <- paste0(row_values, ",", collapse = "")
    table_row <- gsub(pattern = linebreak, replacement = " \\\\ ", table_row)
    table_content <- c(table_content, paste0("  ", table_row))
  }
  table_content <- c(table_content, ")", "```")
  return(table_content)
}

#' @title typst_scientific
#' @description
#' Add characters to render number into scientific writing
#' @param values Values to render into scientific format
#' @param digit Number of digits
#' @export
#' @family report
#' @examples
#'
#' typst_scientific(stats::rlnorm(5))
#'
typst_scientific <- function(values, digit = 2) {
  typst_values <- sprintf(paste0("%.", digit, "e"), values)
  typst_values <- gsub(pattern = "e\\+", replacement = " dot 10^(", typst_values)
  typst_values <- gsub(pattern = "e-", replacement = " dot 10^(-", typst_values)
  return(paste0("$", typst_values, ")$"))
}

#' @title highlight_significant
#' @description
#' Add color for significant cell
#' @param data A data.frame
#' @param pval Significance threshold for p-value
#' @param color Color when significant
#' @param format `"typst"`, `"html"`
#' @export
#' @family report
#' @examples
#'
#' cor_data <- cov_cor(data_501, meta_data_501)
#' cor_data
#'
#' cor_data |> highlight_significant(pval = 0.1, format = "html")
#'
#' cor_data |> highlight_significant(pval = 0.1, format = "typst")
#'
#' cor_data |> highlight_significant(pval = 0.1, format = "docx")
#'
highlight_significant <- function(data, pval = 0.05, color = "green", format = "html") {
  for (col_index in seq_len(ncol(data))) {
    pvalues <- parse_pvalue(data[[col_index]])
    data[[col_index]] <- dplyr::if_else(
      pvalues <= pval,
      true = switch(format,
        "html" = paste0("<font color=\"", color, "\"><strong>", data[[col_index]], "</strong></font>"),
        "docx" = paste0("**", data[[col_index]], "**"),
        "typst" = paste0("#text(", color, ")[*", data[[col_index]], "*]")
      ),
      false = data[[col_index]],
      missing = data[[col_index]]
    )
  }
  return(data)
}

#' @title parse_pvalue
#' @description
#' Parse pvalue as `"(p: 0.xyz)"` or `"(p< 0.001)"` from text
#' @param text character vector
#' @keywords internal
parse_pvalue <- function(text) {
  pvalue <- gsub(pattern = ".*(\\((p:|p<))|(\\))", replacement = "", text)
  return(suppressWarnings({
    as.numeric(pvalue)
  }))
}
