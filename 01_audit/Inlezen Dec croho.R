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
  config::get("metadata_1cho_decoding_files_dir"), "Croho.asc"
)

Dec_Croho <- read_fwf(Bestandspad,
                      fwf_widths(c(4, 5, 4, 4, 5, 5, 1, 2, 3, 2, 120, 2, 5)),
                     locale = locale(encoding = "windows-1252"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Bestandsbeschrijving_Croho.txt bevat uitleg voor de inhoud van de kolommen

Dec_Croho <- Dec_Croho %>%
  ## Filter op instelling omdat historische codes anders over instellingen heen kunnen verschillen
  filter(X3 == config::get("metadata_institution_BRIN")) %>%
  ## Verwijderen van accenten
  mutate(across(
    everything(),
    ~ stringi:::stri_trans_general(str = ., id = "Latin-ASCII")
  )) %>%
  ## Splits kolom X1 in 2 kolommen
  rename(
    from = X5,
    to = X6
  ) %>%
  select(from, to) %>%
  distinct()


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##'*INFO* Voor universiteiten is het de croho code in jaar, voor HBO's de actuele code
write_file_proj(Dec_Croho,
                name = "Mapping_OPL_Code_in_jaar_OPL_Code_historisch",
                full_dir = Sys.getenv("MAP_TABLE_DIR"),
                extensions = "csv")

clear_script_objects()
