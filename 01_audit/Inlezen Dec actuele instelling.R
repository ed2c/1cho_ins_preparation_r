## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Inlezen Dec actuele instelling.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2021 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: In dit script wordt het Dec_actuele_instelling.asc omgezet naar RDS en worden
## de kolomnamen omgezet
##
## Afhankelijkheden:
##
## Datasets: /1cHO/2020/LEESMIJ en reerentietabellen/Dec_actuele_instelling.asc
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
  Sys.getenv("NETWORK_DIR"),
  "Datasets/1cHO/2022/referentietabellen en documentatie/Dec_actuele_instelling.asc"
)

Dec_actuele_instelling <- read_delim(Bestandspad,
  col_types = cols(X1 = col_character()),
  delim = ";",
  col_names = FALSE
)
## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Up to date check
## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

up_to_date(
  bestandspad = Bestandspad,
  frequentie = 365,
  contact = "Helmut Matheis",
  inleesscript = "Inlezen Dec actuele instelling.R"
)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Splits kolom X1 in 2 kolommen en geef de juiste kolomnamen
Dec_actuele_instelling <- Dec_actuele_instelling %>%
  mutate(
    INS_Instellingscode = substr(X1, 1, 4),
    INS_Instelling = trimws(substr(X1, 5, 200))
  ) %>%
  select(-X1)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Bewaar bestand in de versies die als mapping-tables gebruikt worden
vvmover::write_file_proj(Dec_actuele_instelling, "Mapping_INS_Instellingscode_INS_Instellingsnaam")
vvmover::write_file_proj(Dec_actuele_instelling, "Mapping_INS_Examen_bachelor_instellingscode_INS_Examen_bachelor_instellingsnaam")

vvmover::write_file_proj(Dec_actuele_instelling, "INS_Dec_actuele_instelling")

clear_script_objects()
