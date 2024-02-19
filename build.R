## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Copyright
## Web Page:
## Contact:
##
##' *INFO*:
## 1) This file runs all scripts to build all datasets.

sFolders <- write_config_proj() %>%
  pull(script_dir)

 sScripts_to_run <- sFolders %>%
   map(., list.files) %>%
   unlist()


walk(sScipts_to_run, source)
