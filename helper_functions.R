# =========================
# Helper functions
# =========================

# Test normality column-wise using Shapiro-Wilk p-values
# Returns one row with one p-value per column
test_normality <- function(data) {
  data %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        ~ {
          x <- stats::na.omit(.x)
          
          # Shapiro test requires numeric data and 3-5000 observations
          if (!is.numeric(x) || length(x) < 3 || length(x) > 5000) {
            NA_real_
          } else {
            stats::shapiro.test(x)$p.value
          }
        }
      )
    )
}

# Extract partial eta-squared from a model and round to 3 decimals
getPartialEtaSq <- function(m_fixed) {
  as.data.frame(DescTools::EtaSq(m_fixed))[2] |>
    round(3)
}

# Z-transform each column of a numeric data frame / matrix
# Returns a data frame with the same columns
z_transform <- function(data) {
  as.data.frame(scale(data, center = TRUE, scale = TRUE))
}

# Extract coefficient estimates and p-values from a linear model
# Keeps the same intended output format as your original function:
# one row, columns = sex / age / cog_score / interaction
extract_coefficients_pvalues <- function(m, cog_score) {
  coefs <- summary(m)$coefficients[-1, , drop = FALSE]
  
  estimates <- round(coefs[, "Estimate"], 2)
  p_values  <- coefs[, "Pr(>|t|)"]
  
  p_values_formatted <- dplyr::case_when(
    p_values < 0.001 ~ "< 0.001",
    p_values < 0.01  ~ "< 0.01",
    p_values < 0.05  ~ "< 0.05",
    TRUE             ~ as.character(round(p_values, 2))
  )
  
  coef_pval <- paste0(estimates, " (", p_values_formatted, ")")
  
  # Preserve your original intended labels / structure
  names(coef_pval) <- c("sex", "age", "cog_score", "interaction")
  
  as.data.frame(as.list(coef_pval), check.names = FALSE)
}

# Remove rows containing outliers in any column
# Remove outliers beyond +/- n_sd SD
remove_sd_outliers <- function(data, exclude_vars, n_sd = 2) {
  data %>%
    dplyr::select(-any_of(exclude_vars)) %>%
    mutate(
      across(
        where(is.numeric),
        ~ {
          z <- abs(.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
          ifelse(z <= n_sd, .x, NA)
        }
      )
    ) %>%
    dplyr::select(where(~ !all(is.na(.)))) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}


get_cog_score_means <- function(test_scores){
  summary_stats <- test_scores %>%
    dplyr::select(-stdyptid, -gender) %>%
    group_by(dxgroupFK) %>%
    summarise(
      across(
        everything(),
        list(
          Mean = ~ mean(.x, na.rm = TRUE),
          SD   = ~ sd(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    ) %>% mutate(round(., 2))
  
  return (summary_stats)
}
 
