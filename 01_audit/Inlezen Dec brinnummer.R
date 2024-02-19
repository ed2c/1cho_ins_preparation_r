## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2023 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
##' *INFO*:
## 1) ___
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Bestandspad <- paste0(
  config::get("metadata_1cho_decoding_files_dir"), "Dec_brinnummer.asc"
)

dfDec_BRIN_4_nummer <- read_fwf(
  Bestandspad,
  fwf_widths(c(4, 30, 4, 24, 8, 8, 3, 31)),
  locale = locale(encoding = "windows-1252")
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Bestandsbeschrijving_Dec-bestanden.txt bevat uitleg voor de inhoud van de kolommen

Dec_BRIN_4_nummer <- dfDec_BRIN_4_nummer  %>%
  ## Verwijderen van accenten
  mutate(across(
    everything(),
    ~ stri_trans_general(str = ., id = "Latin-ASCII")
  ))

## Dit decoderingsbestand leidt tot meerdere mapping tables
Dec_BRIN_4_naam <- Dec_BRIN_4_nummer %>%
  rename(from = X1,
         to = X2) %>%
  select(from, to)

Dec_BRIN_4_postcode <- Dec_BRIN_4_nummer %>%
  rename(from = X1,
         to = X3) %>%
  select(from, to)

Dec_BRIN_4_plaats <- Dec_BRIN_4_nummer %>%
  rename(from = X1,
         to = X4) %>%
  select(from, to)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(Dec_BRIN_4_naam,
                name = "Mapping_BRIN_4_nummer_INS_Instellingsnaam",
                full_dir = Sys.getenv("MAP_TABLE_DIR"),
                extensions = "csv")

write_file_proj(Dec_BRIN_4_postcode,
                name = "Mapping_BRIN_4_nummer_INS_Postcode",
                full_dir = Sys.getenv("MAP_TABLE_DIR"),
                extensions = "csv")

write_file_proj(Dec_BRIN_4_plaats,
                name = "Mapping_BRIN_4_nummer_INS_Plaats",
                full_dir = Sys.getenv("MAP_TABLE_DIR"),
                extensions = "csv")

vvcommander::clear_script_objects()
