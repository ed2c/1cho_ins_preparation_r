## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Inlezen Inschrijvingen 1CHO VUdata.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2021 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: In dit script wordt het inschrjivingen-bestand 1CHO (OWD205) ingelezen
## uit VUdata
##
## Afhankelijkheden: Index.R
##
## Datasets: date_time_ODW205 - VUanalytics 1CHO Inschrijving
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lees alle benodigde bestanden in:
bestandspad <- get_recent_file(
  paste0(
    Sys.getenv("NETWORK_DIR"),
    "Datasets/MIVU/sftp_download/"
  ),
  "ODW205 - VUanalytics 1CHO Inschrijving",
  date_type = "filename_ymd"
)

## Lees het bestand Inschrijvingen in uit het zip-bestand
Inschrijvingen_1cho <- unzip_read_delim(
  bestandspad,
  delim = ",",
  na = c("", "NA", "#", "Niet toegewezen"),
  col_types = cols(
    .default = col_guess(),
    INS_Studentnummer = col_double(),
    INS_Vooropleiding_voor_HO_gem_cijfer = col_double(),
    DEM_Nationaliteit_3 = col_integer(),
    INS_Vooropleiding_binnen_HO_soort = col_character(),
    INS_Vooropleiding_voor_HO_postcode_student = col_integer(),
    INS_Vooropleiding_voor_HO_postcode_woonadres = col_integer(),
    INS_Vooropleiding_voor_HO_Soort = col_character(),
    INS_Hoogste_vooropleiding_soort_1CHO = col_character(),
    INS_Hoogste_vooropleiding_binnen_HO_code = col_integer()
  )
)

## Lees het namingbestand in
Inschrijvingen_1cho_naming <- read_documentation("Documentatie_Inschrijvingen_1CHO_VUdata.csv")

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. ASSERTIONS ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
assert_naming(Inschrijvingen_1cho, Inschrijvingen_1cho_naming, "Inschrijvingen_1cho")
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Pas kolomnamen aan met behulp van de documentatie
Inschrijvingen_1cho <- wrapper_translate_colnames_documentation(
  Inschrijvingen_1cho,
  Inschrijvingen_1cho_naming
)

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  distinct() %>%
  filter(!is.na(INS_Studentnummer)) %>%
  mutate(
    INS_Faculteit = stringi::stri_trans_general(INS_Faculteit, "Latin-ASCII"),
    INS_Opleidingsnaam_2002 = stringi::stri_trans_general(INS_Opleidingsnaam_2002, "Latin-ASCII")
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
vvmover::write_file_proj(Inschrijvingen_1cho, "INS_Inschrijvingen_1CHO_VUdata")

clear_script_objects()
