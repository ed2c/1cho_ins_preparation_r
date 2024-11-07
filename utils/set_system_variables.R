## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Npuls CEDA (Centre for Educational Data Analytics)
## Web Page: https://edu.nl/twt84
## Contact: corneel.denhartogh@surf.nl
##
##' *INFO*:
## 1) This file runs all scripts to build all datasets.
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Set the argument (name for Sys.setenv dynamically and execute it)
set_all_env_variables <- function(var_name, var_value) {
  args = list(var_value)
  names(args) = var_name
  do.call(Sys.setenv, args)
}

##' *INFO*: Dit is momenteel enkel beschikbaar op "main", vandaar volledig bestandspad
dfSystem_variables <- read_csv2("utils/proj_settings/renviron.csv", show_col_types = FALSE)

## zet variabelen in R system variables
walk2(dfSystem_variables$variable, dfSystem_variables$value, set_all_env_variables)

clear_script_objects()
