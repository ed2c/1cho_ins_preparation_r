## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Inladen Packages.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2021 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: In dit script worden alle benodigde packages geinstalleerd als ze nog
## niet geinstalleerd zijn, vervolgens worden deze ingeladen.
##
## Afhankelijkheden: Geen
##
## Datasets: Geen
##
## Opmerkingen:
## 1) Geen
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# In order for load
packages_base <- c(
  "base",
  "methods",
  "utils",
  "stats",
  "graphics",
  "grDevices",
  "datasets")


# TODO When updating this list, also run outcommented code
packages_cran <- c(
  "dataReporter",       # Create a data audit report
  "rlang",          # Enable complex operations
  "config",         # Set up configuration files and functions
  "janitor",        # Clean up names from special characters
  "lubridate",      # Work with dates and times
  "purrr",          # Work with functions and vectors
  "readxl",         # Read xlsx
  "readr",          # Read data (csv, tsv, and fwf)
  "slackr",         # Send messages in Slack
  "stringi",        # Work with other strings
  "stringr",        # Work with strings
  "tibble",         # Edit and create tibbles
  "tidyr",          # Tidy data in the tidyverse environment
  "utils",          # Provide utility functions
  "fst",            # Perform operations with large data files
  "dplyr"          # Utilise the dplyr environment
)

packages_github <- c(
  "vusa"            # Utilise packages from the VU team
)

# packages_vusa <- c("vvcommander",
#                    "vvauditor",
#                    "vvmover",
#                    "vvconverter",
#                    "vvsculptor",
#                    "vusa")

# Combine packages
packages <- c(packages_base, packages_cran, packages_github)
packages <- packages[packages != "config"]
packages_renv <- c(packages_cran, packages_github)


# Configure renv
options(renv.snapshot.filter = function(project) {
  return(packages_renv)
})

renv::snapshot(type = "custom")

renv::restore()


# Load packages
# TODO Set to TRUE when adding packages to check if there are problematic conflicts
warn_conflicts <- FALSE
suppressMessages(purrr::walk(packages, ~library(.x,
                                                character.only = TRUE,
                                                warn.conflicts = warn_conflicts)))


clear_script_objects()
