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
