#| label: eta-covariate-analysis
#| echo: false
#
# Eta-based covariate analysis using the `nonmem.utils` R package.
#
# Inputs (expected to be defined upstream, e.g. in a Quarto setup chunk or
# passed in via `params`):
#   tab_path  - NONMEM output table (.tab, 2-line header) with ETA1 and ETA2
#   dict_path - dictionary CSV (does NOT yet declare the etas)
#
# Produces, for an HTML Quarto document:
#   (a) a pairs plot of the etas (GGally ggmatrix), and
#   (b) an eta-by-covariate correlation table with significant cells highlighted.

library(nonmem.utils)
library(readr)
library(dplyr)
library(knitr)

# ---- 1. Read inputs --------------------------------------------------------
# .tab files have a 2-line header, so skip the first line (table title).
data <- read_table(tab_path, skip = 1, show_col_types = FALSE)
meta_data <- read_csv(dict_path, show_col_types = FALSE)

# ---- 2. Declare ETA1 / ETA2 as Type "eta" if not already present -----------
# The dictionary is assumed NOT to declare the etas. Add them robustly, only
# for etas that actually exist in `data` and are not already declared.
eta_names <- c("ETA1", "ETA2")
eta_in_data   <- intersect(eta_names, names(data))           # only what we have
already_etas  <- meta_data$Name[meta_data$Type == "eta"]     # already declared
missing_etas  <- setdiff(eta_in_data, already_etas)

if (length(missing_etas) > 0) {
  eta_rows <- data.frame(
    Name  = missing_etas,
    Type  = "eta",
    Label = missing_etas,
    stringsAsFactors = FALSE
  )
  # Pad with NA for every other dictionary column (Unit, Min, Max, ...) so the
  # row aligns with `meta_data` regardless of its exact column set.
  for (col in setdiff(names(meta_data), names(eta_rows))) {
    eta_rows[[col]] <- NA
  }
  eta_rows  <- eta_rows[, names(meta_data)]   # match column order
  meta_data <- bind_rows(meta_data, eta_rows)
}

# Defensively fill any missing NONMEM default Types (id, time, mdv, ...).
meta_data <- fill_meta_vars(meta_data)

# ---- 3. (a) Eta pairs plot -------------------------------------------------
# `eta_plot()` returns a *named list* of GGally `ggmatrix` objects
# (entry "All" plus one per categorical covariate). ggmatrix objects must be
# displayed with `print()`; they are NOT plain ggplot objects.
eta_pairs <- eta_plot(data, meta_data)

print_eta_pairs <- function(x) {
  if (is.null(x)) {
    return(invisible(NULL))
  }
  if (inherits(x, "list")) {
    for (nm in names(x)) {
      cat(sprintf("\n\n### Eta pairs — %s\n", nm))
      print(x[[nm]])                       # ggmatrix -> needs print()
    }
  } else {
    # Defensive: a bare ggmatrix / ggplot slipped through.
    print(x)
  }
}
print_eta_pairs(eta_pairs)

# ---- 4. (b) Eta-by-covariate correlation table (HTML-highlighted) ----------
# `eta_cor()` returns a single data.frame: rows = covariates (cov + cat),
# columns = etas. `highlight_significant()` parses p-values out of the cell
# text and styles cells with p <= pval; format = "html" emits colored bold
# markup suitable for a Quarto HTML document.
eta_corr <- eta_cor(data, meta_data)

if (!is.null(eta_corr) && is.data.frame(eta_corr)) {
  eta_corr_html <- eta_corr |>
    highlight_significant(pval = 0.05, format = "html")

  # Render without escaping so the HTML markup in the cells is preserved.
  kable(eta_corr_html, format = "html", escape = FALSE,
        caption = "Eta-by-covariate correlations (highlighted: p <= 0.05)")
}
