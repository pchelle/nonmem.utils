#' @title dplyrVariables
#' @description
#' Remove check warning for the variables listed below
#' Variables called withing dplyr and tidyr functions
#' @keywords internal
dplyrVariables <- c(
  # Meta data variables
  "Name", "Type", "Label", "Unit", "with_Unit", "Label_with_Unit",
  # Inventory variables
  "Subjects", "Studies", "Doses", "Observations", "Covariate", "Values",
  # Nonmem variables
  "ID", "EVID", "TIME", "PRED", "IPRED", 'MDV', 'BLQ', 'OBS', 'REP', 'DV','NPDE', "LLOQ",
  # VPC variables
  'blq', 'blq_obs_med_med', 'med_pred', 'time_med_med',
  'Bins', 'pcOBS', 'pcDV', 'statistics', 'Max', 'Min',
  'CV', 'Estimates', 'RSE', 'Shrinkage'
)

utils::globalVariables(c(dplyrVariables))

default_meta_data <- read.csv(
  system.file("template-dictionary.csv", package = "nonmem.utils"),
  na = c("NA", "N/A", "", ".")
)


#' @title data_501
#' @description
#' Dataset from Nonmem 501 example
#' @export
data_501 <- read.csv(
  system.file("test", "501-data.csv", package = "nonmem.utils"),
  na = c("NA", "N/A", "", ".")
)

#' @title data_501
#' @description
#' Dictionary example for Nonmem 501 example dataset
#' @export
meta_data_501 <- read.csv(
  system.file("test", "501-dictionary.csv", package = "nonmem.utils"),
  na = c("NA", "N/A", "", ".")
)
