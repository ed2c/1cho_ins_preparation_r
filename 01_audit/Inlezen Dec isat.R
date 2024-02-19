## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Inlezen Dec isat.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2021 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: In dit script wordt het Dec_isat.asc omgezet naar RDS en worden
## de kolomnamen omgezet
##
## Afhankelijkheden: Index.R
##
## Datasets: /1cHO/2020/LEESMIJ en reerentietabellen/Dec_isat.asc
##
## Opmerkingen:
## 1) Geen
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lees alle benodigde bestanden in:
Bestandspad <- paste0(
  config::get("metadata_1cho_decoding_files_dir"), "Dec_isat.asc"
)

Dec_isat <- read_fwf(
  Bestandspad,
  fwf_widths(c(5, 195)),
  locale = locale(encoding = "windows-1252")
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Bestandsbeschrijving_Dec-bestanden.txt bevat uitleg voor de inhoud van de kolommen

## Verander kolomnamen voor gebruik als mapping table
Dec_isat <- Dec_isat %>%
  ## Verwijderen van accenten
  mutate(across(
    everything(),
    ~stri_trans_general(str = ., id = "Latin-ASCII")
  )) %>%
  ## Splits kolom X1 in 2 kolommen
  rename(
    from = X1,
    to = X2
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Bewaar bestand in de versies die als mapping-tables gebruikt worden

write_file_proj(Dec_isat,
                name = "Mapping_OPL_Code_in_jaar_OPL_Naam_in_jaar",
                full_dir = Sys.getenv("MAP_TABLE_DIR"),
                extensions = "csv")

clear_script_objects()
