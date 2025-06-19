#' @title nonmem_res
#' @description
#' Parse a Nonmem result file
#' @param res_file File path to parse
#' @export
nonmem_res <- function(res_file) {
  res_content <- trimws(readLines(res_file))
  to_remove <- res_content %in% ""
  res_content <- res_content[!to_remove]

  res_list <- list()
  res_list$Name <- gsub(".res", "", basename(res_file))
  res_list$Date <- parse_in_text("Current Date", res_content, "date")
  # About dataset
  res_list$Subjects <- parse_in_text("INDIVIDUALS", res_content)
  res_list$Observations <- parse_in_text("OBS RECS", res_content)
  # About model
  res_list$n_theta <- parse_in_text("LENGTH OF THETA", res_content)
  res_list$n_omega <- parse_in_text(c("OMEGA", "DIMENSION"), res_content)
  if (is.na(res_list$n_omega)) {
    res_list$n_omega <- parse_omega_size(res_content)
  }
  res_list$n_sigma <- parse_in_text(c("SIGMA", "DIMENSION"), res_content)
  # About method
  res_list$Method <- parse_in_text("#METH", res_content, "character")
  # Results
  res_list$Min <- sum(grepl("MINIMIZATION SUCCESSFUL", res_content)) > 0
  res_list$Cov <- !parse_in_text("COVARIANCE STEP OMITTED", res_content, "logical")
  res_list$Evaluations <- parse_in_text("FUNCTION EVALUATIONS USED", res_content)
  res_list$`Significant Digits` <- parse_in_text("NO\\. OF SIG\\. DIGITS IN FINAL EST\\.", res_content)
  n_par <- res_list$n_theta + res_list$n_omega + res_list$n_sigma + res_list$n_omega * res_list$Subjects
  res_list$OFV <- parse_in_text("OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT", res_content)
  res_list$AIC <- res_list$OFV + 2 * n_par
  res_list$BIC <- res_list$OFV + res_list$Observations * n_par
  # res_list$OFV2 <- parse_in_text("OBJECTIVE FUNCTION VALUE WITH CONSTANT", res_content)
  res_list$`Estimation Time [s]` <- parse_in_text("Elapsed estimation  time", res_content)
  res_list$`Covariance Time [s]` <- parse_in_text("Elapsed covariance  time", res_content)
  # Results
  res_list$Theta <- parse_theta(res_list$n_theta, res_content)
  res_list$Omega <- parse_omega(res_list$n_omega, res_content)
  res_list$Sigma <- parse_sigma(res_list$n_sigma, res_content)
  return(res_list) # |> data.frame(check.names = FALSE)
}

#' @title find_row
#' @description
#' Find row that matches patterns
#' @param patterns character vector of patterns to match
#' @param text character vector of text to search
#' @keywords internal
find_row <- function(patterns, text) {
  if (length(patterns) == 1) {
    return(grepl(pattern = patterns, x = text))
  }
  selected_row <- lapply(
    patterns,
    function(pattern) {
      grepl(pattern = pattern, x = text)
    }
  )
  selected_row <- do.call("&", selected_row)
  return(selected_row)
}

#' @title matrix_indices
#' @description
#' Get x and y indices of a `n_dim`x`n_dim` matrix
#' @param n_dim Matrix dimension
#' @export
matrix_indices <- function(n_dim) {
  indices <- expand.grid(
    x = seq_len(n_dim),
    y = seq_len(n_dim)
  ) |>
    filter(x >= y) |>
    arrange(x)
  return(indices)
}

#' @title parse_in_text
#' @description
#' Parse a variable at the end of a line matching target patterns
#' @param patterns character vector of patterns to match
#' @param text character vector of text to search
#' @param type type of variable to parse
#' @keywords internal
parse_in_text <- function(patterns, text, type = "numeric") {
  selected_row <- find_row(patterns, text)
  if (sum(selected_row) != 1) {
    return(NA)
  }
  elements_to_keep <- switch(type,
    "numeric" = 1,
    "logical" = 1,
    "character" = -1,
    "date" = 3
  )
  value_in_text <- strsplit(text[selected_row], "[[:space:]]") |>
    unlist() |>
    tail(elements_to_keep)
  value_in_text <- switch(type,
    "numeric" = value_in_text |> as.numeric(),
    "logical" = value_in_text %in% "YES",
    "character" = paste(value_in_text, collapse = " "),
    "date" = paste(value_in_text, collapse = " ") |> as.Date(format = "%d %B %Y")
  )
  return(value_in_text)
}

#' @title parse_theta
#' @description
#' Parse THETA (fixed effects) characteristics in text
#' @param n_theta Number of THETA parameters
#' @param text character vector of text to search
#' @keywords internal
parse_theta <- function(n_theta, text) {
  theta_names <- paste0("THETA", seq_len(n_theta))
  initial_theta_row <- grepl("INITIAL ESTIMATE OF THETA", text) |>
    which() + 1
  theta_pattern <- seq(0, 3 * (n_theta - 1), 3)
  if (length(initial_theta_row) == 0) {
    initial_theta <- rep(NA, 3 * n_theta)
  } else {
    initial_theta <- strsplit(text[initial_theta_row + seq_len(n_theta)], "[[:space:]]") |>
      unlist() |>
      as.numeric()
    initial_theta <- initial_theta[!is.na(initial_theta)]
  }
  theta_row_indices <- grepl("THETA - VECTOR OF FIXED EFFECTS PARAMETERS", text) |>
    which() + 2
  if (length(theta_row_indices) == 0) {
    return(NA)
  }
  theta <- strsplit(trimws(text[theta_row_indices]), "[[:space:]]") |>
    unlist() |>
    as.numeric()
  theta <- theta[!is.na(theta)]
  if (length(theta_row_indices) == 1) {
    return(list(
      Name = theta_names,
      Initial = initial_theta[2 + theta_pattern],
      Lower = initial_theta[1 + theta_pattern],
      Upper = initial_theta[3 + theta_pattern],
      Estimates = theta |> head(n_theta),
      RSE = rep(NA, n_theta)
    ))
  }
  return(list(
    Name = theta_names,
    Initial = initial_theta[2 + theta_pattern],
    Lower = initial_theta[1 + theta_pattern],
    Upper = initial_theta[3 + theta_pattern],
    Estimates = theta |> head(n_theta),
    RSE = 100 * tail(theta, n_theta) / head(theta, n_theta)
  ))
}

#' @title parse_sigma
#' @description
#' Parse SIGMA (random effects - eps) characteristics in text
#' @param n_sigma Number of SIGMA parameters
#' @param text character vector of text to search
#' @keywords internal
parse_sigma <- function(n_sigma, text) {
  sigma_indices <- matrix_indices(n_sigma)
  rse_indices <- which(sigma_indices$x == sigma_indices$y)
  sigma_names <- paste0("SIGMA(", sigma_indices$x, ",", sigma_indices$y, ")")
  initial_sigma_row <- grep("INITIAL ESTIMATE OF SIGMA", text)

  if (length(initial_sigma_row) == 0) {
    initial_sigma <- rep(NA, nrow(sigma_indices))
  } else {
    initial_sigma <- strsplit(text[initial_sigma_row + seq_len(n_sigma)], "[[:space:]]") |>
      unlist() |>
      as.numeric()
    initial_sigma <- initial_sigma[!is.na(initial_sigma)]
  }
  sigma_row_indices <- grep("SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS", text) + 1
  if (length(sigma_row_indices) == 0) {
    return(NA)
  }
  sigma_row_indices <- c(sapply(
    sigma_row_indices,
    function(x) {
      x + seq_len(2 * n_sigma)
    }
  ))
  suppressWarnings({
    sigma <- strsplit(text[sigma_row_indices], "[[:space:]]") |>
      unlist() |>
      as.numeric()
  })
  sigma <- sigma[!is.na(sigma)]

  # Shrinkage
  shrinkage_row_indice <- grep("EPSSHRINKSD", text)
  sigma_shrinkage <- rep(NA, nrow(sigma_indices))
  if (length(shrinkage_row_indice) > 0) {
    suppressWarnings({
      shrinkage_values <- strsplit(text[shrinkage_row_indice], "[[:space:]]") |>
        unlist() |>
        as.numeric()
    })
    shrinkage_values <- shrinkage_values[!is.na(shrinkage_values)]
    sigma_shrinkage[rse_indices] <- shrinkage_values
  }
  # RSE
  sigma_rse <- rep(NA, nrow(sigma_indices))
  if (length(sigma_row_indices) == 1) {
    return(list(
      Name = sigma_names,
      Initial = initial_sigma,
      Estimates = sigma |> head(nrow(sigma_indices)),
      RSE = sigma_rse,
      Shrinkage = sigma_shrinkage
    ))
  }
  sigma_rse[rse_indices] <- 100 * (tail(sigma, n_sigma) / sigma[rse_indices])
  return(list(
    Name = sigma_names,
    Initial = initial_sigma,
    Estimates = sigma |> head(nrow(sigma_indices)),
    RSE = sigma_rse,
    Shrinkage = sigma_shrinkage
  ))
}

#' @title parse_omega
#' @description
#' Parse OMEGA size when BOV is used and the matrix is using blocks
#' @param text character vector of text to search
#' @keywords internal
parse_omega_size <- function(text) {
  omega_first_row <- grep("OMEGA HAS BLOCK FORM", text)
  omega_last_row <- grep("DEFAULT OMEGA BOUNDARY TEST OMITTED", text)
  n_omega <- omega_last_row - omega_first_row - 1
  return(n_omega)
}

#' @title parse_omega
#' @description
#' Parse OMEGA (random effects - etas) characteristics in text
#' @param n_sigma Number of SIGMA parameters
#' @param text character vector of text to search
#' @keywords internal
parse_omega <- function(n_omega, text) {
  omega_indices <- matrix_indices(n_omega)
  rse_indices <- which(omega_indices$x == omega_indices$y)
  omega_names <- paste0("OMEGA(", omega_indices$x, ",", omega_indices$y, ")")
  initial_omega_row <- grep("INITIAL ESTIMATE OF OMEGA", text)

  if (any(length(initial_omega_row) == 0, grepl("BLOCK SET NO", text))) {
    initial_omega <- rep(NA, nrow(omega_indices))
  } else {
    initial_omega <- strsplit(text[initial_omega_row + seq_len(n_omega)], "[[:space:]]") |>
      unlist() |>
      as.numeric()
    initial_omega <- initial_omega[!is.na(initial_omega)]
  }
  omega_row_indices <- grep("OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS", text) + 1
  if (length(omega_row_indices) == 0) {
    return(NA)
  }
  omega_row_indices <- c(sapply(
    omega_row_indices,
    function(x) {
      x + seq_len(2 * n_omega)
    }
  ))
  suppressWarnings({
    omega <- strsplit(text[omega_row_indices], "[[:space:]]") |>
      unlist() |>
      as.numeric()
  })
  omega <- omega[!is.na(omega)]

  # Shrinkage
  shrinkage_row_indice <- grep("ETASHRINKSD", text)
  omega_shrinkage <- rep(NA, nrow(omega_indices))
  if (length(shrinkage_row_indice) > 0) {
    suppressWarnings({
      shrinkage_values <- strsplit(text[shrinkage_row_indice], "[[:space:]]") |>
        unlist() |>
        as.numeric()
    })
    shrinkage_values <- shrinkage_values[!is.na(shrinkage_values)]
    omega_shrinkage[rse_indices] <- shrinkage_values
  }
  # RSE
  omega_rse <- rep(NA, nrow(omega_indices))
  if (length(omega_row_indices) == 1) {
    return(list(
      Name = omega_names,
      Initial = initial_omega,
      Estimates = omega |> head(nrow(omega_indices)),
      RSE = omega_rse,
      Shrinkage = omega_shrinkage
    ))
  }
  omega_rse[rse_indices] <- 100 * (tail(omega, n_omega) / omega[rse_indices])
  return(list(
    Name = omega_names,
    Initial = initial_omega,
    Estimates = omega |> head(nrow(omega_indices)),
    RSE = omega_rse,
    Shrinkage = omega_shrinkage
  ))
}


#' @title cov_to_cor
#' @description
#' Translate covariance to correlation values
#' @param data A data.frame
#' @export
cov_to_cor <- function(data, cov_name = "Estimates") {
  n_dim <- round((sqrt(1 + 8 * nrow(data)) - 1) / 2)
  cov_indices <- matrix_indices(n_dim) |>
    mutate(x_row = row_number(), y_row = row_number())
  diag_indices <- cov_indices$x == cov_indices$y
  cov_indices$x_row[!diag_indices] <- sapply(
    cov_indices$x[!diag_indices],
    function(x) {
      cov_indices$x_row[diag_indices & cov_indices$x == x]
    }
  )
  cov_indices$y_row[!diag_indices] <- sapply(
    cov_indices$y[!diag_indices],
    function(y) {
      cov_indices$y_row[diag_indices & cov_indices$y == y]
    }
  )
  new_data <- bind_cols(data, cov_indices) |>
    mutate(
      CV = ifelse(x == y, sqrt(Estimates), Estimates / (Estimates[x_row] * Estimates[y_row]))
    ) |>
    select(-c(x, y, x_row, y_row))
  return(new_data)
}

#' @title format_result
#' @description
#' Format results as a data.frame
#' @param nonmem_result A list of Nonmem results
#' @export
format_result <- function(nonmem_result) {
  # Theta, Omega, and Sigma are matrices to be converted as character
  formatted_theta <- nonmem_result$Theta |>
    as.data.frame() |>
    mutate(Label = paste0(Name, ": ", round(Estimates, 3), " [RSE: ", round(RSE, 1), "%]")) |>
    pull(Label)
  formatted_theta <- paste(formatted_theta, collapse = "\\\n")
  formatted_omega <- nonmem_result$Omega |>
    as.data.frame() |>
    cov_to_cor() |>
    filter(Estimates > 0) |>
    mutate(Label = paste0(
      Name, ": ", round(100 * CV, 1),
      "% [Shk:", round(Shrinkage, 1), "%-RSE:", round(RSE, 1), "%]"
    )) |>
    pull(Label)
  formatted_omega <- paste(formatted_omega, collapse = "\\\n")
  formatted_sigma <- nonmem_result$Sigma |>
    as.data.frame() |>
    cov_to_cor() |>
    filter(Estimates > 0) |>
    mutate(Label = paste0(
      Name, ": ", round(100 * CV, 1),
      "% [Shk:", round(Shrinkage, 1), "%-RSE:", round(RSE, 1), "%]"
    )) |>
    pull(Label)
  formatted_sigma <- paste(formatted_sigma, collapse = "\\\n")
  nonmem_result$Theta <- formatted_theta
  nonmem_result$Omega <- formatted_omega
  nonmem_result$Sigma <- formatted_sigma
  return(data.frame(nonmem_result, check.names = FALSE))
}
