pkgs <- c(
  "dplyr", "tidyr", "tibble", "purrr", "rlang",
  "data.table",
  "ggplot2",
  "haven", "readxl", "lmerTest", "lmtest", "emmeans",
  "DescTools", "irr", "pROC",
  "broom", "knitr", "rmarkdown"
)

invisible(lapply(pkgs, library, character.only = TRUE))