## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2023 VU
## Web Page: http://www.vu.nl
##
##'*INFO*
## 1) ___
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Inschrijvingen_1cho_basis <- read_file_proj("INS_eencijfer_enrollments")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Inschrijvingen_1cho <- Inschrijvingen_1cho_basis %>%
  distinct() %>%
  filter(!is.na(INS_Studentnummer))

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.2 Recoding ####

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  ## Omzetten naar date zodat we de min en max datum kunnen vinden
  mutate(
    INS_Datum_inschrijving = as.Date(INS_Datum_inschrijving, format = "%d/%m/%Y", tryFormats = c("%d/%m/%Y", "%d-%m-%Y")),
    INS_Datum_uitschrijving = as.Date(INS_Datum_uitschrijving, format = "%d/%m/%Y", tryFormats = c("%d/%m/%Y", "%d-%m-%Y")),
  )

## Premaster goed zetten
## TODO
# Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
#   mutate(INS_Premaster = recode(INS_Premaster,
#                                 "ja" = "P",
#                                 "nee" = NA_character_
#   ))

## Maak Indicatie variabelen Boolean
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  ## Maak de Indicatie variabelen met "J" en  "N" tot boolean variabelen
  mutate_at(vars(DEM_Indicatie_internationale_student, DEM_Indicatie_nationaliteit_EER_actueel,
                 DEM_Indicatie_nationaliteit_EER_peildatum), ~if_else(. == "J", TRUE, FALSE))


## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.3 Mapping tables ####

## Invullen ontbrekende gegevens soort vooropleiding
## Vervang bij de vooropleiding codes de NA's door 0
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(
    INS_Vooropleiding_binnen_HO_code = replace_na(INS_Vooropleiding_binnen_HO_code, 0),
    INS_Vooropleiding_voor_HO_code = replace_na(INS_Vooropleiding_voor_HO_code, 0),
    INS_Hoogste_vooropleiding_code_1CHO = replace_na(INS_Hoogste_vooropleiding_code_1CHO, 0)
  )

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
    "INS_Vooropleiding_binnen_HO_code",
    "INS_Vooropleiding_binnen_HO_sector",
    mapping_table_name = "Mapping_INS_Vooropleiding_code_INS_Vooropleiding_naam"
  )

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(INS_Hoogste_vooropleiding_code_1CHO = as.numeric(INS_Hoogste_vooropleiding_code_1CHO)) %>%
  mapping_translate(
    "INS_Hoogste_vooropleiding_code_1CHO",
    "INS_Hoogste_vooropleiding_soort_1CHO",
    mapping_table_name = "Mapping_INS_Vooropleiding_code_INS_Vooropleiding_cat"
  )

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
    "INS_Vooropleiding_voor_HO_code",
    "INS_Vooropleiding_voor_HO_profiel",
    mapping_table_name = "Mapping_INS_Vooropleiding_code_INS_Vooropleiding_naam"
  )

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
    "INS_Vooropleiding_voor_HO_code",
    "INS_Vooropleiding_voor_HO_omschrijving",
    mapping_table_name = "Mapping_INS_Vooropleiding_code_INS_Vooropleiding_cat"
  )


Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
    "INS_Vooropleiding_binnen_HO_code",
    "INS_Vooropleiding_binnen_HO_soort",
    mapping_table_name = "Mapping_INS_Vooropleiding_code_INS_Vooropleiding_cat"
  )

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "INS_Hoogste_vooropleiding_code_1CHO",
  "INS_Hoogste_vooropleiding_nieuw_cat",
  mapping_table_name = "Mapping_INS_Vooropleiding_code_INS_Vooropleiding_cat"
)

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "INS_Vooropleiding_voor_HO_BRIN",
  "INS_Vooropleiding_voor_HO_naam",
  mapping_table_name = "Mapping_BRIN_4_nummer_INS_Instellingsnaam"
)

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "INS_Vooropleiding_binnen_HO_BRIN",
  "INS_Vooropleiding_binnen_HO_naam",
  mapping_table_name = "Mapping_BRIN_4_nummer_INS_Instellingsnaam"
)

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "INS_Hoogste_vooropleiding_BRIN_1CHO",
  "INS_Hoogste_vooropleiding_instellingsnaam",
  mapping_table_name = "Mapping_BRIN_4_nummer_INS_Instellingsnaam"
)

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "INS_Vooropleiding_voor_HO_BRIN",
  "INS_Vooropleiding_voor_HO_postcode",
  mapping_table_name = "Mapping_BRIN_4_nummer_INS_Postcode"
) %>% mutate(INS_Vooropleiding_voor_HO_postcode = as.double(INS_Vooropleiding_voor_HO_postcode))

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "INS_Vooropleiding_binnen_HO_BRIN",
  "INS_Vooropleiding_binnen_HO_postcode",
  mapping_table_name = "Mapping_BRIN_4_nummer_INS_Postcode"
) %>% mutate(INS_Vooropleiding_binnen_HO_postcode = as.double(INS_Vooropleiding_binnen_HO_postcode))

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "INS_Hoogste_vooropleiding_BRIN_1CHO",
  "INS_Hoogste_vooropleiding_postcode",
  mapping_table_name = "Mapping_BRIN_4_nummer_INS_Postcode"
) %>% mutate(INS_Hoogste_vooropleiding_postcode = as.double(INS_Hoogste_vooropleiding_postcode))

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "INS_Vooropleiding_voor_HO_BRIN",
  "INS_Vooropleiding_voor_HO_plaats",
  mapping_table_name = "Mapping_BRIN_4_nummer_INS_Plaats"
)

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "INS_Vooropleiding_binnen_HO_BRIN",
  "INS_Vooropleiding_binnen_HO_plaats",
  mapping_table_name = "Mapping_BRIN_4_nummer_INS_Plaats"
)

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "INS_Hoogste_vooropleiding_BRIN_1CHO",
  "INS_Hoogste_vooropleiding_plaats",
  mapping_table_name = "Mapping_BRIN_4_nummer_INS_Plaats"
)

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "INS_Examenresultaat_code",
  "INS_Examenresultaat_omschrijving"
)

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "INS_Indicatie_eerste_jaars_instelling",
  "INS_Indicatie_eerste_jaars_instelling_cat"
)

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "INS_Indicatie_eerste_jaars_instelling",
  "INS_Indicatie_eerste_jaars_instelling_naam"
)

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "INS_Indicatie_eerste_jaars_opleiding_en_instelling",
  "INS_Indicatie_eerste_jaars_opleiding_en_instelling_naam"
)

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "DEM_Nationaliteit_1",
  "DEM_Nationaliteit_1_naam",
  mapping_table_name = "Mapping_DEM_Nationaliteit_code_DEM_Nationaliteit_naam"
)

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "DEM_Nationaliteit_2",
  "DEM_Nationaliteit_2_naam",
  mapping_table_name = "Mapping_DEM_Nationaliteit_code_DEM_Nationaliteit_naam"
)

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
  "DEM_Nationaliteit_3",
  "DEM_Nationaliteit_3_naam",
  mapping_table_name = "Mapping_DEM_Nationaliteit_code_DEM_Nationaliteit_naam"
)

## Omdat de univariate plots per categorie werken, worden continue variabelen
## zoals leeftijd ook omgevormd tot categorische variabelen.
## Ook deze operatie is gedocumenteerd in de documentatie en
## gebruikt een mapping tabel.

Inschrijvingen_1cho <- mapping_category(
  Inschrijvingen_1cho,
  "DEM_Leeftijd_peildatum_1_jan",
  "DEM_Leeftijd_peildatum_1_jan_cat",
  mapping_table_name = "Mapping_DEM_Leeftijd_cat"
)

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
    "INS_Soort_inschrijving_1CHO_code",
    "INS_Soort_inschrijving_1CHO_cat"
  )

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
    "INS_Vooropleiding_binnen_HO_BRIN",
    "INS_Vooropleiding_binnen_HO_Instellingsnaam",
    mapping_table_name = "Mapping_BRIN_4_nummer_INS_Instellingsnaam"
  )

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
    "INS_Vooropleiding_voor_HO_BRIN",
    "INS_Vooropleiding_voor_HO_Instellingsnaam",
    mapping_table_name = "Mapping_BRIN_4_nummer_INS_Instellingsnaam"
  )

Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Indicatie_actief_op_peildatum_code",
  "INS_Indicatie_actief_op_peildatum_omschrijving"
)

Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Opleidingsfase_actueel_code",
  "INS_Opleidingsfase_actueel_naam"
)

Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Opleidingsvorm_naam",
  "INS_Opleidingsvorm_code"
)

Inschrijvingen_1cho <- mapping_category(Inschrijvingen_1cho,
                                   "DEM_Leeftijd_peildatum_1_oktober",
                                   "DEM_Leeftijd_peildatum_1_oktober_cat",
                                   mapping_table_name = "Mapping_DEM_Leeftijd_cat"
)

Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "DEM_Geslacht_code",
  "DEM_Geslacht_naam"
)

## Deze variabele leidt tot heel veel categorieen omdat dit een aantal is.
## Voor de univariate plots maken we daarom een categorische variabele hiervan.
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_category(
    "INS_Verblijfsjaren_wetenschappelijk_onderwijs",
    "INS_Verblijfsjaren_wetenschappelijk_onderwijs_vanaf_0_cat",
    mapping_table_name = "Mapping_INS_Verblijfsjaren_wetenschappelijk_onderwijs_cat"
  )
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_category(
    "INS_Verblijfsjaren_hoger_onderwijs",
    "INS_Verblijfsjaren_hoger_onderwijs_vanaf_0_cat",
    mapping_table_name = "Mapping_INS_Verblijfsjaren_wetenschappelijk_onderwijs_cat"
  )



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(Inschrijvingen_1cho, "INS_Inschrijvingen_1CHO_part_1")

clear_script_objects()
