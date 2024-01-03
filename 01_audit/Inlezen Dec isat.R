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
  fwf_widths(c(5, 195)))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Splits kolom X1 in 2 kolommen en geef de juiste kolomnamen
Dec_isat <- Dec_isat %>%
  rename(
    INS_Opleidingscode = X1,
    INS_Opleidingsnaam = X2
  )

## Verander kolomnamen voor gebruik als mapping table
Dec_isat <- Dec_isat %>%
  rename(from = INS_Opleidingscode,
         to = INS_Opleidingsnaam)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Bewaar bestand in de versies die als mapping-tables gebruikt worden

write_file_proj(Dec_isat,
                name = "Mapping_INS_Opleidingscode_actueel_INS_Opleidingsnaam.csv",
                full_dir = Sys.getenv("MAP_TABLE_DIR"),
                extensions = "csv")

write_file_proj(Dec_isat,
                name = "Mapping_INS_Examen_bachelor_opleidingscode_actueel_INS_Examen_bachelor_opleidingsnaam.csv",
                full_dir = Sys.getenv("MAP_TABLE_DIR"),
                extensions = "csv")


clear_script_objects()
