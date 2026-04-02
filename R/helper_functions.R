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
}# Returns one row with one p-value per column
test_normality <- function(data) {
  data %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        ~ {
          x <- stats::na.omit(.x)
          
          # Shapiro test requires:
          # - numeric input
          # - between 3 and 5000 observations
          # - at least 2 unique values
          if (
            !is.numeric(x) ||
            length(x) < 3 ||
            length(x) > 5000 ||
            length(unique(x)) < 2
          ) {
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
# Remove extreme outliers (beyond +/- n_sd SD) and round remaining values
remove_sd_outliers <- function(data, exclude_vars = character(0), n_sd = 2) {
  data %>%
    dplyr::mutate(
      dplyr::across(
        where(is.numeric) & !dplyr::any_of(exclude_vars),
        ~ {
          m <- mean(.x, na.rm = TRUE)
          s <- sd(.x, na.rm = TRUE)
          z <- abs(.x - m) / s
          ifelse(z <= n_sd, .x, NA_real_)
        }
      )
    ) %>%
    dplyr::select(where(~ !all(is.na(.)))) %>%   # drop all-NA columns
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ round(.x, 3))
    )
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
 


plot_age_density <- function(data, age_col = "agevisit", group_col = "dxgroupFK") {
  ggplot(data, aes_string(x = age_col, fill = paste0("factor(", group_col, ")"))) +
    geom_density(alpha = 0.5) +
    labs(
      title = "Distribution of Ages Across Groups",
      x = "Chronological Age",
      y = "Density",
      fill = "Group"
    ) +
    theme_classic()
}

# 2. Wilcoxon test for age by group
run_age_wilcox <- function(data, age_col = "agevisit", group_col = "dxgroupFK") {
  formula <- as.formula(paste(age_col, "~ as.factor(", group_col, ")", sep = ""))
  wilcox.test(formula, data = data)
}


# 5. Chi-square test for gender by group
run_gender_chisq <- function(data, group_col = "dxgroupFK", gender_col = "gender") {
  gender_table <- table(data[[group_col]], data[[gender_col]])
  chisq.test(gender_table)
}

plot_loadings <- function(loadings_df, pc = "PC2", title = NULL) {
  
  if (is.null(title)) {
    title <- paste0(pc, " loadings")
  }
  
  ggplot(loadings_df, aes(x = reorder(variable, .data[[pc]]), y = .data[[pc]])) +
    geom_col(width = 0.7) +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_text(
      aes(label = round(.data[[pc]], 2)),
      hjust = ifelse(loadings_df[[pc]] > 0, -0.15, 1.15),
      size = 4
    ) +
    expand_limits(
      y = c(min(loadings_df[[pc]]) - 0.15,
            max(loadings_df[[pc]]) + 0.15)
    ) +
    labs(
      title = title,
      x = NULL,
      y = "Loading"
    ) +
    theme_minimal(base_size = 13)
}

  
run_lm_brainage_model <- function(
    data,
    outcome,
    predictor,
    group = "dxgroupFK",
    covariates = c("gender", "agevisit"),
    interaction = TRUE
) {
  
  vars_needed <- c(outcome, predictor, group, covariates)
  
  missing_vars <- setdiff(vars_needed, names(data))
  if (length(missing_vars) > 0) {
    stop("These variables are missing from `data`: ",
         paste(missing_vars, collapse = ", "))
  }
  
  df <- data %>%
    dplyr::select(dplyr::all_of(vars_needed)) %>%
    tidyr::drop_na()
  
  if (nrow(df) == 0) {
    stop("No complete cases remain after dropping missing values.")
  }
  
  main_term <- if (interaction) {
    paste0(predictor, " * ", group)
  } else {
    paste0(predictor, " + ", group)
  }
  
  formula_str <- paste0(
    outcome, " ~ ", main_term,
    if (length(covariates) > 0) {
      paste0(" + ", paste(covariates, collapse = " + "))
    } else {
      ""
    }
  )
  
  model_formula <- stats::as.formula(formula_str)
  model <- stats::lm(model_formula, data = df)
  
  list(
    model = model,
    summary = summary(model),
    data_used = df,
    formula = model_formula
  )
}

run_pca <- function(df, n_components) {
  df %>%
    select(where(is.numeric)) %>%   
    select(-c("agevisit", 'dxgroupFK', 'gender')) %>%  
    select(where(~ !all(is.na(.)))) %>%               # drop all-NA columns
    mutate(
      across(everything(), ~ log10(. + 1))            
    ) %>%
    mutate(
      #flip direction so that higher = worse performance
      cwicnrw    = -cwicnrw,
      cwicnrw    = -cwicnrw,
      cwiirw     = -cwiirw    ,
      cwiisrw    = -cwiisrw,
      stroop1    = -stroop1,
      stroop2    = -stroop2,
     ) %>% mutate(across(everything(), scale)) %>%  psych::principal(nfactors = n_components)
}


get_loadings <- function(pca_result, digits = 2, flip_sign = TRUE) {
  
  loadings_mat <- pca_result$loadings
  
  if (flip_sign) {
    loadings_mat <- loadings_mat * -1
  }
  
  loadings_df <- as.data.frame(unclass(loadings_mat)) %>%
    tibble::rownames_to_column("variable")
  
  # Identify PC columns dynamically
  pc_cols <- setdiff(names(loadings_df), "variable")
  
  # Apply rounding across all PCs
  loadings_df <- loadings_df %>%
    dplyr::mutate(across(all_of(pc_cols), ~ round(.x, digits)))
  
  return(loadings_df)
}
