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

## TODO Integrate documentatie into import definitions
CROHO_naming <- read_documentation("Documentatie_CROHO.csv")

## The 00_download_croho should deliver this
file_path <- "data/00_raw/CrohoAct.txt"

CROHO_import_definitions <- read_import_definitions("CROHO.csv")

## Open croho
CROHO <- LaF::laf_open_fwf(file_path,
                           column_widths = CROHO_import_definitions$widths,
                           column_names = CROHO_import_definitions$names_croho,
                           column_types = CROHO_import_definitions$types
                          )[,]


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. ASSERT ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Fix before assert
colnames(CROHO) <- CROHO_import_definitions$names_croho

CROHO <- CROHO %>%
  mutate_all(~replace(., . == "", NA))

assert_naming(CROHO, CROHO_naming, "CROHO")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. MODIFY ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

CROHO <- CROHO %>%
  wrapper_translate_colnames_documentation(CROHO_naming) %>%
  distinct()


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(CROHO, "CROHO")

clear_script_objects()

