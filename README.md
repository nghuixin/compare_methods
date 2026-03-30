# compare_methods


## Requirements

- **R 4.3.2**
- **RStudio** recommended
- Internet access during first setup so `renv` can install packages

## Project structure

Key files:

- `renv.lock` — records the package environment for this project
- `renv/activate.R` — activates the project-local `renv` environment
- `.Rprofile` — project startup configuration
- analysis scripts / `.Rmd` files — main code for reproducing results

## First-time setup

### 1. Install the correct R version

This project was developed with **R 4.3.2**.

Please make sure you are running:

```r
R.version.string
