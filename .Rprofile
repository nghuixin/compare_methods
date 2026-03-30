# ---- CRAN mirror + renv settings ----
options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  renv.config.auto.snapshot = FALSE
)

# ---- Activate renv ----
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

# ---- R version check ----
required_r_version <- "4.3.2"

current_version <- as.character(getRversion())

if (current_version != required_r_version) {
  stop(
    sprintf(
      "\nThis project requires R %s, but you are using R %s.\n",
      required_r_version,
      current_version
    ),
    call. = FALSE
  )
}
