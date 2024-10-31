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
  mutate(Datum_einde_opleiding = as.Date("1899-12-31") + suppressWarnings(days(Datum_einde_opleiding)),
         Datum_einde_instroom = as.Date("1899-12-31") + suppressWarnings(days(Datum_einde_instroom)))

# Create synthetic rows based on rows VU
if (Sys.getenv("R_CONFIG_ACTIVE") %in% c("synthetic", "default", "")) {

  synthetic_rows <- CROHO %>%
    filter(OPL_Instellingscode == "21PL") %>%
    mutate(OPL_Instellingscode = "21XX")

  CROHO <- CROHO %>%
    bind_rows(synthetic_rows)

}

CROHO <- CROHO %>%
  ## Pak opleidingen van instelling
  filter(OPL_Instellingscode == config::get("metadata_institution_BRIN")) %>%
  mutate(
    ## Wijzig type van velden (oa ivm koppeling opleidingstabel)
    OPL_Code_in_jaar = as.integer(OPL_Code_in_jaar),
    OPL_Nominale_studielast_EC_aantal = as.integer(OPL_Nominale_studielast_EC_aantal),
    ## Creëer variabele voor studielast per jaar
    OPL_Nominale_studieduur = as.integer(OPL_Nominale_studielast_EC_aantal / 60)
  ) %>%
  mapping_translate("OPL_Code_in_jaar", "OPL_Code_historisch")


nMax_jaar <- config::get("year")

## Het CROHO-bestand bevat alleen rijen per wijziging. Om het per jaar je maken selecteren we de laatste
## wijziging per jaar en vervolgens vullen we ontbrekende jaren om met de data van het laatst
## ingevulde jaar
CROHO_per_jaar <- CROHO %>%
  mutate(
    OPL_Academisch_jaar = academic_year(Datum_begin_opleiding),
    OPL_Academisch_jaar_einde_opleiding = academic_year(Datum_einde_opleiding),
    OPL_Academisch_jaar_einde_opleiding = pmin(OPL_Academisch_jaar_einde_opleiding, nMax_jaar)
  ) %>%
  group_by(OPL_Code_in_jaar, OPL_Instellingscode, INS_Opleidingsvorm, OPL_Academisch_jaar) %>%
  arrange(desc(Datum_begin_opleiding)) %>%
  slice(1) %>%
  ungroup() %>%
  ## Groepeer dit eerst om ongeacht vorm de juiste actuele codes en namen te krijgen
  ## Instellingscode is van belang omdat historische opleidingen anders kunnen zijn voor
  ## verschillende instellingen
  group_by(OPL_Code_historisch, OPL_Instellingscode) %>%
  arrange(Datum_begin_opleiding) %>%
  ## Bepaal meest recente code en naam
  mutate(OPL_Code_actueel = last(OPL_Code_in_jaar),
         OPL_Opleidingsnaam_CROHO_actueel = last(OPL_Opleidingsnaam_CROHO)) %>%
  ungroup() %>%
  ##'*INFO* Dit is nu inclusief vorm, dus niet alleen CROHO per jaar, maar CROHO_vorm_per_jaar
  ## TODO Controleren welke verschillen rijen met verschillende vormen nog meer hebben, misschien
  ## vormen ook samenvoegen
  group_by(OPL_Code_in_jaar, OPL_Instellingscode, INS_Opleidingsvorm) %>%
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
  ungroup() %>%
  rename(OPL_Opleidingsnaam_CROHO_in_jaar = OPL_Opleidingsnaam_CROHO)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(CROHO)

write_file_proj(CROHO_per_jaar)

clear_script_objects()

