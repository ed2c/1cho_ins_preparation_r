## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Inlezen Dec nationaliteitscode.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2021 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: In dit script wordt het Dec_nationaliteitscode.csv omgezet naar RDS en worden
## de kolomnamen omgezet
##
## Afhankelijkheden: Index.R
##
## Datasets: /1cHO/2020/LEESMIJ en reerentietabellen/Dec_nationaliteitscode.asc
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
  config::get("metadata_1cho_decoding_files_dir"), "Dec_nationaliteitscode.asc"
)

Dec_nationaliteitscode <- read_fwf(
  Bestandspad,
  fwf_widths(c(4, 50, 1, 12, 2, 31)),
  locale = locale(encoding = "windows-1252")
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Bestandsbeschrijving_Dec-bestanden.txt bevat uitleg voor de inhoud van de kolommen

## Verwijder accenten en splits kolom X1 in 2 kolommen
Dec_nationaliteitscode <- Dec_nationaliteitscode %>%
  ## Verwijderen van accenten
  mutate(across(
    everything(),
    ~ stringi:::stri_trans_general(str = ., id = "Latin-ASCII")
  )) %>%
  ## Splits kolom X1 in 2 kolommen
  rename(
    from = X1,
    to = X2
  ) %>%
  select(from, to)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(Dec_nationaliteitscode,
                name = "Mapping_DEM_Nationaliteit_code_DEM_Nationaliteit_naam",
                full_dir = Sys.getenv("MAP_TABLE_DIR"),
                extensions = "csv")

clear_script_objects()
