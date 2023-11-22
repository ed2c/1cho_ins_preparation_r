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
  Sys.getenv("NETWORK_DIR"),
  "Datasets/1cHO/2022/referentietabellen en documentatie/Dec_nationaliteitscode.asc"
)

Dec_nationaliteitscode <- read_delim(Bestandspad,
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
  inleesscript = "Dec nationaliteitscode.R"
)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## Verwijder accenten en splits kolom X1 in 2 kolommen
Dec_nationaliteitscode <- Dec_nationaliteitscode %>%
  ## Verwijderen van accenten
  mutate_at(
    c("X1"),
    ~ stringi:::stri_trans_general(str = ., id = "Latin-ASCII")
  ) %>%
  ## Splits kolom X1 in 6 kolommen
  ## TO DO: Debug kolom 3 t/m 6, nu leeg
  mutate(
    DEM_Nationaliteit_code = str_sub(X1, 1, 4),
    DEM_Nationaliteit_naam = trimws(str_sub(X1, 5, 50)),
    DEM_Etniciteit_kort = str_sub(X1, 55, 1),
    DEM_Etniciteit_kort_naam = trimws(str_sub(X1, 56, 12)),
    DEM_Etniciteit_lang = substr(X1, 68, 2),
    DEM_Etniciteit_lang_naam = trimws(substr(X1, 70, 32))
  ) %>%
  ## Verwijder kolom X1
  select(-X1)



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

saverds_csv(Dec_nationaliteitscode, "DEM_Dec_nationaliteitscode", save_csv = TRUE)

clear_script_objects()
