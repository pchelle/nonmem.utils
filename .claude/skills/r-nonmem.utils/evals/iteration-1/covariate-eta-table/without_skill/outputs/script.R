## =============================================================================
## Eta-based covariate analysis with nonmem.utils
##
## Inputs:
##   - tab_path  : a NONMEM .tab result file containing (at least) ETA1, ETA2
##                 and the covariate columns declared in the dictionary.
##   - dict_path : a column-dictionary CSV that does NOT yet declare the etas.
##
## Outputs:
##   (a) an ETA pairs plot (eta-vs-eta scatter matrix)
##   (b) an ETA-by-covariate correlation table with significant cells
##       highlighted, rendered for an HTML Quarto document.
##
## The script is deliberately defensive: several nonmem.utils helpers return
## wrapper lists (e.g. list(plot = ..., data = ...)) rather than bare objects,
## so a small extractor is used to fish out the relevant component.
## =============================================================================

library(nonmem.utils)
library(dplyr)
library(ggplot2)

## ---- Inputs (placeholders -- replace with real paths) -----------------------
tab_path  <- "path/to/run001.tab"
dict_path <- "path/to/template-dictionary.csv"

## ---- Helpers ----------------------------------------------------------------

## NULL-coalescing operator (rlang-style) so the script does not hard-depend
## on rlang being attached.
`%||%` <- function(a, b) if (is.null(a)) b else a

## Pull a bare object out of a (possibly) list-valued return value.
## If `x` is already a data.frame / ggplot / atomic, it is returned unchanged.
extract_component <- function(x, candidates, predicate = NULL) {
  if (is.null(x)) return(NULL)
  if (!is.list(x) || is.data.frame(x)) return(x)        # already a bare object
  for (nm in candidates) {                              # named slots first
    elt <- x[[nm]]
    if (!is.null(elt) && (is.null(predicate) || predicate(elt))) return(elt)
  }
  for (elt in x) {                                      # then any matching elt
    if (!is.null(elt) && (is.null(predicate) || predicate(elt))) return(elt)
  }
  x[[1]]                                                # last resort
}

is_ggplot     <- function(x) inherits(x, "ggplot")
is_data_frame <- function(x) is.data.frame(x)

## Append ETA declarations to a dictionary, regardless of the exact column
## naming convention used (names / name / column / variable ...).
declare_etas <- function(meta_data, eta_cols, labels) {
  name_col  <- (intersect(c("names", "name", "column", "colname",
                            "variable", "col"), names(meta_data))[1])
  type_col  <- intersect(c("type", "role", "kind"),           names(meta_data))[1]
  label_col <- intersect(c("label", "description", "descr"),  names(meta_data))[1]
  if (is.na(name_col)) name_col <- names(meta_data)[1]

  out <- meta_data
  for (i in seq_along(eta_cols)) {
    row <- meta_data[1, , drop = FALSE]      # template row with correct col types
    row[] <- NA
    row[[name_col]] <- eta_cols[i]
    if (!is.na(type_col))  row[[type_col]]  <- "eta"
    if (!is.na(label_col)) row[[label_col]] <- labels[i]
    out <- bind_rows(out, row)
  }
  out
}

## ---- Read & extend the data dictionary --------------------------------------
meta_data <- read.csv(dict_path, check.names = FALSE, stringsAsFactors = FALSE)

eta_cols <- c("ETA1", "ETA2")
eta_labs <- c("ETA1 (CL/F)", "ETA2 (Vc/F)")
meta_data <- declare_etas(meta_data, eta_cols, eta_labs)

## ---- Read the NONMEM table --------------------------------------------------
nm <- nonmem_res(tab_path)
nm_data <- extract_component(nm, c("data", "tab", "table", "results"),
                             predicate = is_data_frame)
stopifnot(is.data.frame(nm_data))

## Keep only subjects that have an eta record (drop records where etas are NA).
nm_data <- nm_data %>% filter(if_all(all_of(eta_cols), ~ !is.na(.x)))

## ---- (a) ETA pairs plot -----------------------------------------------------
eta_pairs <- tryCatch(
  eta_plot(nm_data, meta_data),
  error = function(e) {
    message("eta_plot() failed (", conditionMessage(e),
            ") - falling back to GGally::ggpairs()")
    if (requireNamespace("GGally", quietly = TRUE)) {
      GGally::ggpairs(nm_data, columns = eta_cols,
                      title = "ETA pairs")
    } else NULL
  }
)
eta_pairs <- extract_component(eta_pairs, c("plot", "p", "ggplot"),
                               predicate = is_ggplot)
print(eta_pairs)

## ---- (b) ETA-by-covariate correlation table ---------------------------------
eta_cov_cor <- tryCatch(
  eta_cor(nm_data, meta_data),
  error = function(e) {
    message("eta_cor() failed: ", conditionMessage(e)); NULL
  }
)
eta_cov_df <- extract_component(eta_cov_cor,
                                c("table", "data", "cor", "result"),
                                predicate = is_data_frame)

if (!is.null(eta_cov_df)) {
  ## Highlight significant cells for the HTML Quarto document.
  eta_cov_highlighted <- highlight_significant(eta_cov_df)
  print(eta_cov_highlighted)

  ## Optional: a self-contained HTML correlation report block.
  cor_report(eta_cov_df, format = "html")
} else {
  message("No ETA-by-covariate correlation table produced.")
}
