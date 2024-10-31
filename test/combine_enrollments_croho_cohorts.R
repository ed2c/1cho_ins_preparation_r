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

Inschrijvingen_1cho_basis <- read_file_proj("Inschrijvingen_1cho")

CROHO_per_jaar <- read_file_proj("CROHO_per_jaar")

if (config::get("metadata_institution_BRIN") == "21PL") {
  Cohorten <- read_file_proj("Cohorten")
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.1 CROHO info koppelen ####

## TODO CROHO goed toevoegen
CROHO_per_jaar_1cho <- CROHO_per_jaar %>%
  select(
    OPL_Opleidingsnaam_CROHO_actueel,
    OPL_Code_in_jaar,
    OPL_Nominale_studieduur,
    OPL_Academisch_jaar,
    OPL_Bekostiging,
    OPL_Bekostigingsduur,
    Relatie_soort
  ) %>%
  filter(OPL_Academisch_jaar >= 2002) %>%
  select(-OPL_Academisch_jaar) %>%
  group_by(OPL_Code_in_jaar) %>%
  mutate(OPL_Nominale_studieduur = min(OPL_Nominale_studieduur),
         OPL_Bekostigingsduur = min(OPL_Bekostigingsduur)) %>%
  distinct()

Inschrijvingen_1cho2 <- Inschrijvingen_1cho_basis %>%
  left_join(CROHO_per_jaar_1cho,
            by = c("INS_Inschrijvingsjaar" =
                     "OPL_Academisch_jaar",
                   "OPL_Code_in_jaar")
  )


Inschrijvingen_1cho <- Inschrijvingen_1cho_basis %>%
  left_join(CROHO_per_jaar_1cho2,
            by = c("OPL_Code_in_jaar")
  )

test <- Inschrijvingen_1cho2 %>% filter(is.na(OPL_Nominale_studieduur)) %>%
  count(OPL_Code_in_jaar, OPL_code_historisch, INS_Inschrijvingsjaar)


if (config::get("metadata_institution_BRIN") == "21PL") {

  ## Selecteer per studiejaar, per opleiding de nominale studieduur
  CROHO_per_jaar_cohorten <- CROHO_per_jaar %>%
    select(
      OPL_Code_in_jaar,
      OPL_Code_actueel,
      OPL_Academisch_jaar
    ) %>%
    ## Verwijder dubbele waarden (die onstaan door opleidingsvorm (voltijd en deeltijd))
    distinct()

  ## Koppel de nominale studeduur aan Cohorten
  Cohorten <- Cohorten %>%
    mapping_translate(current = "OPL_Code_in_jaar", new = "OPL_Naam_in_jaar")

  Cohorten <- Cohorten %>%
    left_join(CROHO_per_jaar_cohorten,
              by = c(
                "INS_Eerste_jaar_opleiding_en_instelling" =
                  "OPL_Academisch_jaar",
                "OPL_Code_in_jaar"
              )
    )

  Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
    left_join(Cohorten,
              by = c("INS_Studentnummer",
                     "OPL_Code_actueel",
                     "INS_Eerste_jaar_opleiding_en_instelling"))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Voor eerste keer runnen, maak een documentatie bestand aan
## vvmover::create_documentatie(Dataset, "Dataset")

write_file_proj(Inschrijvingen_1cho, "Inschrijvingen_1cho_1")

clear_script_objects()
