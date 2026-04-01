# =========================================================
# File: R/load_analysis_df.R
# Purpose: Load and merge brain-age, original clinical/cog,
#          and derived cognitive variables into one analysis
#          dataframe, while resolving duplicate IDs by keeping
#          the row with the fewest missing values.
# =========================================================

library(dplyr)
library(haven)
library(readr)

# ---------------------------------------------------------
# Helper: keep one row per ID with the fewest missing values
# ---------------------------------------------------------
keep_least_na_row <- function(df, id_col = "stdyptid", tie_break_col = "visit") {
  df %>%
    dplyr::mutate(
      na_count = rowSums(is.na(.))
    ) %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::arrange(
      na_count,
      .data[[tie_break_col]],
      .by_group = TRUE
    ) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-na_count)
}

# ---------------------------------------------------------
# Load and preprocess brain-age dataframe
# ---------------------------------------------------------
load_brainage_df <- function(
    path = "../data_processed/df_all_brainage_2025.csv"
) {
  
  df <- read.csv(path)
  
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
    ) %>%
    dplyr::mutate(
      stdyptid       = as.character(stdyptid),
      bpad_ridge     = ridge_BrainAge - agevisit,
      bpad_photon    = age_prediction - agevisit,
      bpad_BrainAgeR = ageprediction_BrainAgeR - agevisit,
      bpad_cnn       = cnn_BrainAge - agevisit,
      age2           = agevisit * 2
    ) %>%
    dplyr::filter(!is.na(stdyptid))
  
  return(df)
}

# ---------------------------------------------------------
# Load original dataframe and keep needed variables
# ---------------------------------------------------------
load_original_df <- function(
    path = file.path(
      "..", "data_original",
      "Master_BAI_080720_with_051517completers_060117imagingmaster_genotypes_brainages_ENIGMA_FS_MERGEFK_30122020.sav"
    )
) {
  
  df <- haven::read_sav(path)
  
  df <- df %>%
    dplyr::select(
      stdyptid,
      BDI,
      agevisit,
      dxgroupFK,
      gender,
      visit,
      
      # DKEFS Trails
      trvsoerw,
      trvscerw,
      trnsctrw,
      trlnctrw,
      trmsctrw,
      stroop,
      
      # DKEFS Color-Word Interference
      cwicnrw,
      cwiwrrw,
      cwiirw,
      cwiisrw,
      
      # Clinical measures
      scid_a1,
      panspos,
      pansneg,
      pansgen,
      ymrstot,
      hamtot17,
      hamtot28,
      HistoryPsychosis
    ) %>%
    dplyr::mutate(
      stdyptid = as.character(stdyptid)
    ) %>%
    dplyr::filter(!is.na(stdyptid))
  
  return(df)
}

# ---------------------------------------------------------
# Load derived cognitive variables
# ---------------------------------------------------------
load_derived_cog_df <- function(
    path = "../data_processed/outliers_removed_with_stroop_tmtb_a.csv"
) {
  
  df <- read.csv(path)
  
  df <- df %>%
    dplyr::select(
      stdyptid,
      stroop1,
      stroop2,
      tmtb_a
    ) %>%
    dplyr::mutate(
      stdyptid = as.character(stdyptid)
    ) %>%
    dplyr::filter(!is.na(stdyptid))
  
  return(df)
}

# ---------------------------------------------------------
# Merge everything into one analysis dataframe
# ---------------------------------------------------------
load_merged_analysis_df <- function(
    brainage_path = "../data_processed/df_all_brainage_2025.csv",
    original_path = file.path(
      "..", "data_original",
      "Master_BAI_080720_with_051517completers_060117imagingmaster_genotypes_brainages_ENIGMA_FS_MERGEFK_30122020.sav"
    ),
    derived_cog_path = "../data_processed/outliers_removed_with_stroop_tmtb_a.csv"
) {
  
  brainage_df <- load_brainage_df(brainage_path)
  original_df <- load_original_df(original_path)
  derived_cog_df <- load_derived_cog_df(derived_cog_path)
  
  merged_df <- brainage_df %>%
    dplyr::left_join(
      original_df %>%
        dplyr::select(-agevisit, -gender, -dxgroupFK, -visit),
      by = "stdyptid"
    ) %>%
    dplyr::left_join(derived_cog_df, by = "stdyptid") %>%
    keep_least_na_row(id_col = "stdyptid", tie_break_col = "visit")
  
  return(merged_df)
}