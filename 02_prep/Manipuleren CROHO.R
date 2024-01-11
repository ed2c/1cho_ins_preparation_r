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

CROHO <- CROHO %>%
  ## Pak opleidingen van instelling
  filter(OPL_Instellingscode == config::get("metadata_institution_BRIN")) %>%
  mutate(
    ## Wijzig type van velden (oa ivm koppeling opleidingstabel)
    INS_Opleidingscode_actueel = as.integer(INS_Opleidingscode_actueel),
    OPL_Nominale_studielast_EC_aantal = as.integer(OPL_Nominale_studielast_EC_aantal),
    ## Creëer variabele voor studielast per jaar
    OPL_Nominale_studieduur = as.integer(OPL_Nominale_studielast_EC_aantal / 60)
  )

## Helper variabele voor gebruik in volgende stap, zie stijlgids principe F Self-documenting code
nMax_jaar <- max(CROHO$Datum_begin_opleiding) %>% academic_year()
nMax_jaar <- config::get("year")


## Het CROHO-bestand bevat alleen rijen per wijziging. Om het paar je maken selecteren we de laatste
## wijziging per jaar en vervolgens vullen we ontbrekende jaren om met de data van het laatst
## ingevulde jaar
CROHO_per_jaar <- CROHO %>%
  mutate(
    OPL_Academisch_jaar = academic_year(Datum_begin_opleiding),
    Datum_einde_opleiding = as.Date("1899-12-31") + suppressWarnings(days(Datum_einde_opleiding)),
    OPL_Academisch_jaar_einde_opleiding = academic_year(Datum_einde_opleiding),
    OPL_Academisch_jaar_einde_opleiding = pmin(OPL_Academisch_jaar_einde_opleiding, nMax_jaar)
  ) %>%
  group_by(INS_Opleidingscode_actueel, OPL_Instellingscode, INS_Opleidingsvorm, OPL_Academisch_jaar) %>%
  arrange(desc(Datum_begin_opleiding)) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(INS_Opleidingscode_actueel, OPL_Instellingscode, INS_Opleidingsvorm) %>%
  mutate(
    ## Voorkom dat er een volgend jaar wordt toegevoegd als de opleiding historisch is
    temp_max_jaar = if_else(all(Code_stand_record == "HISTORISCH"),
                            max(OPL_Academisch_jaar_einde_opleiding),
                            nMax_jaar),
    volgend_jaar = lead(OPL_Academisch_jaar - 1, default = max(temp_max_jaar))) %>%
  mutate(Opvolgende_jaren = map2(OPL_Academisch_jaar, volgend_jaar, ~seq(.x, .y))) %>%
  unnest(Opvolgende_jaren) %>%
  ## Verwijder tijdelijke variabelen
  select(-c(volgend_jaar, OPL_Academisch_jaar, temp_max_jaar)) %>%
  rename(OPL_Academisch_jaar = Opvolgende_jaren) %>%
  ungroup()


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(CROHO)

write_file_proj(CROHO_per_jaar)

clear_script_objects()

