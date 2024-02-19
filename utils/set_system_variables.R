

## Set the argument (name for Sys.setenv dynamically and execute it)
set_all_env_variables <- function(var_name, var_value) {
  args = list(var_value)
  names(args) = var_name
  do.call(Sys.setenv, args)
}

##' *INFO*: Dit is momenteel enkel beschikbaar op "main", vandaar volledig bestandspad
dfSystem_variables <- read_csv2("utils/proj_settings/renviron.csv", show_col_types = FALSE)

## zet variabelen in R system variables
pwalk(list(dfSystem_variables$variable, dfSystem_variables$value), set_all_env_variables)


walk2(dfSystem_variables$variable, dfSystem_variables$value, set_all_env_variables)

clear_script_objects()
