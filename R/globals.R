#' @title dplyrVariables
#' @description
#' Remove check warning for the variables listed below
#' Variables called withing dplyr and tidyr functions
#' @keywords internal
dplyrVariables <- c(
  # Meta data variables
  "Name", "Type", "Label", "Unit", "with_Unit", "Label_with_Unit",
  # Inventory variables
  "Subjects", "Studies", "Doses", "Observations", "Covariate", "Value", "Values", "Percent BLQ",
  # Nonmem variables
  "ID", "EVID", "TIME", "TAD", "PRED", "IPRED", "MDV", "BLQ", "OBS", "REP", "DV", "NPDE", "CWRES", "LLOQ",
  # VPC variables
  "pvcDV", "pvcOBS", "sdBinsPRED", "sdPRED", "time_bins", "time_med",
  "tad", "blq", "blq_obs_med_med", "med_pred", "time_med_med",
  "Bins", "pcOBS", "pcDV", "medPRED", "statistics", "Max", "Min",
  "CV", "Estimates", "RSE", "Shrinkage",
  "x", "y", "x_row", "y_row"
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

#' @title vpc_types
#' @description
#' All VPC types
#' @export
vpc_types <- c("vpc", "pc_vpc", "pvc_vpc", "blq", "npde")
