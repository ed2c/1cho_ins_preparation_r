## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Copyright
## Web Page:
## Contact:
##
##' *INFO*:
## 1) This file runs all scripts to build all datasets.

source("utils/00_set_up_environment.R")

sFolders <- write_config_proj() %>%
  pull(script_dir)

sScripts_to_run <- sFolders %>%
   map(., ~paste0(., "/", list.files(.))) %>%
   unlist()

# Create a data frame for script validation
dfScriptvalidation <- tribble(
  ~script,
  sScripts_to_run
) %>%
  unnest(c(script))

## Read the Script validation data in
##' *Note*: All scripts are run here, it takes a long time
dfScriptvalidation <- dfScriptvalidation %>%
  mutate(validation_df = map_dfr(script, safely(~ vusa::validate_script_proj(.x, TRUE)))) %>%
  unnest_wider(validation_df) %>%
  unnest_wider(result)
