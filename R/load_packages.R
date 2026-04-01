pkgs <- c(
  "dplyr", "tidyr", "tibble", "purrr", "rlang",
  "data.table",
  "ggplot2",
  "haven", "readxl", "lmerTest", "lmtest", "emmeans",
  "DescTools", "irr", "pROC",
  "broom", "knitr", "rmarkdown", "brms"
)

missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    stop("Package 'renv' is required to install missing packages.")
  }
  
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  renv::install(missing_pkgs)
}

invisible(lapply(pkgs, library, character.only = TRUE))