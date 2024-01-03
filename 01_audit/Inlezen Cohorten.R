## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Inlezen Cohorten.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2021 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: In dit script wordt het bestand INS_Cohorten_VUDATA ingelezen
##
## Afhankelijkheden: Index.R
##
## Datasets: MIVU/test/2020-02-20-09-23-11_ODW204 - VUanalytics EOI Cohorten.zip
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lees alle benodigde bestanden in:

## TODO Evaluate the usage of zip files later on
sFile_path_Cohorten_VUDATA <- config::get("data_1cho_starting_cohorts_file_path")

dfCohorten_VUDATA <- unzip_read_delim(sFile_path_Cohorten_VUDATA, NULL,
  ",",
  quote = "\"",
  col_types = cols(
    STUDENT_CD = col_character(),
    HER1 = col_double(),
    HER2 = col_double(),
    HER3 = col_double(),
    HER4 = col_double(),
    HER5 = col_double(),
    HER6 = col_double(),
    HER7 = col_integer(),
    HER8 = col_integer(),
    JREXOF = col_double(),
    JREXGF = col_double()
  ),
  na = c("", "NA", "#"),
  trim_ws = TRUE,
  guess_max = 100000
) %>%
  mutate(STUDENT_CD = suppressWarnings(as.integer(STUDENT_CD)))

## Zorg dat de examenwaardes lower-case zijn
dfCohorten_VUDATA <- dfCohorten_VUDATA %>% mutate_at(vars(starts_with("EX")), tolower)

## TODO Dit moet naar manipuleren
## Filter dubbele waarden uit het bestand en selecteer alleen VU-studenten
dfCohorten_VUDATA <- dfCohorten_VUDATA %>%
  filter(
    INSTCODEACT == "VU",
    !is.na(STUDENT_CD)
  ) %>%
  distinct()

## voorkom dubbelingen in groepeer variabelen
group_vars <- c(
  "OPLCODEACT",
  "EOIJAAR",
  "STUDENT_CD"
)

dfCohorten_VUDATA <- dfCohorten_VUDATA %>%
  distinct(across(all_of(group_vars)), .keep_all = TRUE)

## Lees het namingbestand in
dfCohorten_VUDATA_naming <- read_documentation(
  "Documentatie_cohorten_VUDATA_bestand.csv"
)


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. ASSERTIONS ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
assert_naming(dfCohorten_VUDATA, dfCohorten_VUDATA_naming, "Cohorten")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Pas kolomnamen aan met behulp van de documentatie
## Pas kolomnamen aan met documentatie naar conventie, en maak uniek
dfCohorten_VUDATA <- dfCohorten_VUDATA %>%
  wrapper_translate_colnames_documentation(dfCohorten_VUDATA_naming) %>%
  distinct()


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
write_file_proj(dfCohorten_VUDATA, "INS_Cohorten")

clear_script_objects()
