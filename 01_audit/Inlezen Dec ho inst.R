## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Inlezen Dec ho inst.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2021 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: In dit script wordt het Dec_ho-inst.asc omgezet naar RDS en worden
## de kolomnamen omgezet
##
## Afhankelijkheden:
##
## Datasets: /1cHO/2020/LEESMIJ en reerentietabellen/Dec_ho-inst.asc
##
## Opmerkingen:
## 1) Geen
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Lees Dec_ho-inst bestand in
Bestandspad <- paste0(
  Sys.getenv("NETWORK_DIR"),
  "Datasets/1cHO/2022/referentietabellen en documentatie/Dec_ho-inst.asc"
)

Dec_ho_instelling <- read_delim(Bestandspad,
  col_types = cols(X1 = col_character()),
  delim = ";",
  col_names = FALSE
)

## Splits kolom X1 in 2 kolommen en geef de juiste kolomnamen
Dec_ho_instelling <- Dec_ho_instelling %>%
  mutate(
    INS_Instellingscode = substr(X1, 1, 4),
    INS_Instelling = trimws(substr(X1, 5, 200))
  ) %>%
  select(-X1)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Bewaar gewijzigd bestand
vvmover::write_file_proj(Dec_ho_instelling, "Dec_ho_instelling")

# Bewaar bestand in de versies die als mapping-tables gebruikt worden
vvmover::write_file_proj(Dec_ho_instelling, "Mapping_INS_Hoogste_vooropleiding_instellingscode_INS_Hoogste_vooropleiding_instellingsnaam")

## Opruimen
clear_script_objects()
