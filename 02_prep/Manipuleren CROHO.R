## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Manipuleren CROHO.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2021 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: In dit script worden de actuele opleidingen uit CROHO gehaald.
##
## Afhankelijkheden:
##
## Datasets:
## Output/1. Ingelezen data/CROHO.rds
##
## Opmerkingen:
## 1)
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lees alle benodigde bestanden in:

CROHO <- read_file_proj("CROHO")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## BEWERKEN: Wijzig type van velden (oa ivm koppeling opleidingstabel) ####

CROHO <- CROHO %>%
  mutate(
    INS_Opleidingscode_actueel = as.integer(
      INS_Opleidingscode_actueel
    ),
    OPL_Studielast_nominaal = as.integer(OPL_Studielast_nominaal),
    OPL_Bekostigingsduur = as.integer(OPL_Bekostigingsduur),
    OPL_Studielast_nominaal_in_maanden = OPL_Studielast_nominaal,
    OPL_Studielast_nominaal = OPL_Studielast_nominaal / 12
  )

nMax_jaar <- max(CROHO$Datum_begin_opleiding) %>% academic_year()

CROHO_per_jaar <- CROHO %>%
  filter(
    INS_Opleidingscode_actueel == 50952,
    OPL_Onderwijsinstelling == "Erasmus Universiteit Rotterdam"
  ) %>%
  mutate(
    OPL_Academisch_jaar = academic_year(Datum_begin_opleiding)
  ) %>%
  group_by(INS_Opleidingscode_actueel, OPL_Instellingscode, INS_Opleidingsvorm, OPL_Academisch_jaar) %>%
  arrange(desc(Datum_begin_opleiding)) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(INS_Opleidingscode_actueel, OPL_Instellingscode, INS_Opleidingsvorm) %>%
  mutate(volgend_jaar = lead(OPL_Academisch_jaar - 1, default = nMax_jaar)) %>%
  rowwise() %>%
  mutate(Opvolgende_jaren = list(OPL_Academisch_jaar: volgend_jaar)) %>%
  unnest(Opvolgende_jaren) %>%
  select(-c(volgend_jaar, OPL_Academisch_jaar)) %>%
  rename(OPL_Academisch_jaar = Opvolgende_jaren) %>%
  ungroup()


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(
  CROHO, "CROHO"
)

write_file_proj(
  CROHO_per_jaar, "CROHO_per_jaar"
)

clear_script_objects()

