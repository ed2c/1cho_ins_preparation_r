## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Inlezen CROHO.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2021 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: Inlezen van het CROHO bestand van DUO
##
## Afhankelijkheden: Geen
##
## Datasets: CROHO register van DUO
## https://www.duo.nl/zakelijk/images/crohoact.zip
##
## Opmerkingen:
## 1) Leest crohoact in zodat er een actuele versie beschikbaar is van dit bestand
##

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 0. VOORBEREIDEN
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Inlezen documentatie bestand
CROHO_naming <- read_documentation("Documentatie_CROHO.csv")

## To be downloaded from DUO
file_path <- "data/00_raw/CrohoAct.txt"

dfMeta <- read.csv("metadata/data_dictionary_start/Metadata_CROHO.csv")

## Lees Het crohobestand in
CROHO <- LaF::laf_open_fwf(file_path,
                           column_widths = dfMeta$widths,
                           column_names = dfMeta$names_croho,
                           column_types = dfMeta$types
                          )[,]


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. ASSERTIONS ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
assert_naming(CROHO, CROHO_naming, "CROHO")
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

CROHO <- CROHO %>%
  ## Vertaal kolomnamen volgens de documentatie
  wrapper_translate_colnames_documentation(CROHO_naming) %>%
  ## Filter dubbele waarden uit het bestand
  distinct()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(CROHO, "CROHO")

clear_script_objects()

