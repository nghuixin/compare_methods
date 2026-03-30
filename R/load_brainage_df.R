# =========================
# Load and preprocess brain-age dataframe
# =========================
library(dplyr)
load_brainage_df <- function(
    path = "../data_processed/df_all_brainage_2025.csv"
) {
  
  # -------------------------
  # Read data
  # -------------------------
  df <- read.csv(path)
  
  # -------------------------
  # Select relevant columns
  # -------------------------
  df <- df %>%
    dplyr::select(
      stdyptid,
      gender,
      dxgroupFK,
      visit,
      ridge_BrainAge,
      age_prediction,
      ageprediction_BrainAgeR,
      cnn_BrainAge,
      agevisit
    )
  
  # -------------------------
  # Feature engineering
  # -------------------------
  df <- df %>%
    dplyr::mutate(
      bpad_ridge     = ridge_BrainAge - agevisit,
      bpad_photon    = age_prediction - agevisit,
      bpad_BrainAgeR = ageprediction_BrainAgeR - agevisit,
      bpad_cnn       = cnn_BrainAge - agevisit,
      age2           = agevisit * 2
    )
  
  # -------------------------
  # Remove missing values within each participant
  # -------------------------
  df <- data.table::setDT(df)[, lapply(.SD, na.omit), by = stdyptid]
  
  # -------------------------
  # Harmonize diagnosis coding
  # -------------------------
  df <- df %>%
    dplyr::mutate(
      dxgroupFK = as.numeric(haven::as_factor(dxgroupFK))
    )
  
  return(df)
}
