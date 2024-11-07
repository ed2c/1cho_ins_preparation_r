## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Npuls CEDA (Centre for Educational Data Analytics)
## Web Page: https://edu.nl/twt84
## Contact: corneel.denhartogh@surf.nl
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. READ ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

croho_naming <- read_documentation("Documentatie_CROHO.csv")

## The 00_download_croho should deliver this
file_path <- "data/00_raw/croho_actueel.txt"

croho_import_definitions <- read_import_definitions("CROHO.csv")

## Open croho
croho <- LaF::laf_open_fwf(file_path,
                           column_widths = croho_import_definitions$widths,
                           column_names = croho_import_definitions$names_croho,
                           column_types = croho_import_definitions$types
                          )[,]


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. ASSERT ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Fix before assert
colnames(croho) <- croho_import_definitions$names_croho

croho <- croho %>%
  mutate_all(~replace(., . == "", NA))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. MODIFY ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

croho <- croho %>%
  wrapper_translate_colnames_documentation(croho_naming) %>%
  distinct()


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(croho, "croho")

clear_script_objects()

