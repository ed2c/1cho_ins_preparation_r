## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Inlezen Dec vopl.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2021 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: In dit script wordt het Dec_vopl.asc omgezet naar RDS en worden
## de kolomnamen omgezet
##
## Afhankelijkheden: Index.R
##
## Datasets: /1cHO/2020/LEESMIJ en reerentietabellen/Dec_vopl.asc
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
  "Datasets/1cHO/2022/referentietabellen en documentatie/Dec_vopl.asc"
)

Dec_vopl <- read_delim(Bestandspad,
  col_types = cols(X1 = col_character()),
  delim = ";",
  col_names = FALSE,
  locale = locale(encoding = "windows-1252")
)
## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Up to date check
## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

up_to_date(
  bestandspad = Bestandspad,
  frequentie = 365,
  contact = "Helmut Matheis",
  inleesscript = "Dec vooropl.R"
)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Splits kolom X1 in 2 kolommen en geef de juiste kolomnamen
Dec_vopl <- Dec_vopl %>%
  ## Verwijderen van accenten
  mutate_at(
    c("X1"),
    ~ stringi:::stri_trans_general(str = ., id = "Latin-ASCII")
  ) %>%
  ## Splits kolom X1 in 2 kolommen
  mutate(
    INS_Hoogste_vooropleiding_binnen_HO_code = substr(X1, 1, 5),
    INS_Hoogste_vooropleiding_binnen_HO_naam = trimws(substr(X1, 6, 200))
  ) %>%
  ## Verwijder kolom X1
  select(-X1)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Bewaar bestand in de versies die als mapping-tables gebruikt worden
# saverds_csv(Dec_vooropl, "Mapping_INS_Hoogste_vooropleiding_binnen_HO_code_INS_Hoogste_vooropleiding_binnen_HO_naam", save_csv = T)

vvmover::write_file_proj(Dec_vopl, "INS_Dec_vopl")

clear_script_objects()