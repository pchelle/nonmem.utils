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
  "PRED", "IPRED"
)

utils::globalVariables(c(dplyrVariables))

default_meta_data <- read.csv(
  system.file("template-dictionary.csv", package = "nonmem.utils"),
  na = c("NA", "N/A", "", ".")
)
