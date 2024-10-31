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

Inschrijvingen_1cho_basis <- read_file_proj("INS_Inschrijvingen_1CHO_part_1",
                                      dir = "02_prepared"
                                      )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.1 Direct af te leiden ####

## Bepaal ahv INS_Postcode_student_1okt_peildatum en INS_Postcode_student_voor_HO of een student
## uitwonend is: als de velden gelijk zijn voor een student is deze thuiswonend, als ze verschillend
## zijn uitwonend
Inschrijvingen_1cho <- Inschrijvingen_1cho_basis %>%
  mutate(INS_Uitwonend = if_else(INS_Postcode_student_1okt_peildatum == INS_Postcode_student_voor_HO,
                                 FALSE,
                                 TRUE
  ))

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(INS_Indicatie_voltijd = if_else(INS_Opleidingsvorm_code == 1,
                                         TRUE, FALSE
  ))

# Bepaal dubbele studie instelling. Gebruik datums om doorstromers en switchers te filteren
# (Bijv: afronding B & begin M in één jaar of uitschrijvingen voor 1 feb en inschrijving 1 feb)
# Gebruik OPL_Code Actueel uniek om joint degrees te filteren
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  group_by(INS_Studentnummer, INS_Inschrijvingsjaar, INS_Datum_inschrijving, INS_Datum_uitschrijving) %>%
  mutate(
    INS_Aantal_inschrijvingen_jaar_instelling = length(unique(OPL_code_historisch)),
    INS_Aantal_EOI_inschrijvingen_jaar_instelling = sum(
      INS_Indicatie_eerste_jaars_opleiding_en_instelling == 1
    )
  ) %>%
  ungroup() %>%
  mutate(INS_Dubbele_studie_instelling = INS_Aantal_inschrijvingen_jaar_instelling > 1)

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.2. Middelbaar onderwijs profielen ####

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(
    ## Eerst verwijderen we van deze variabele enkele voorvoegsels:
    INS_Vooropleiding_voor_HO_profiel_standaard =
      str_replace_all(
        INS_Vooropleiding_voor_HO_profiel,
        c("vwo profiel |havo |algemeen|havo profiel |profiel "),
        ""
      ),
    ## Daarna kijken we of de bekende profielen voorkomen, en anders voegen
    ## we op dit moment een missende waarde toe.
    INS_Vooropleiding_voor_HO_profiel_standaard = if_else(str_detect(
      INS_Vooropleiding_voor_HO_profiel_standaard,
      "cultuur|economie|natuur"
    ),
    INS_Vooropleiding_voor_HO_profiel_standaard,
    NA_character_
    ),
    ## Omdat er nog enkele verkeerde records overblijven, worden deze apart
    ## nog verwijderd in een derde stap:
    INS_Vooropleiding_voor_HO_profiel_standaard = if_else(str_detect(
      INS_Vooropleiding_voor_HO_profiel_standaard, "mbo|vmbo|vbo"
    ),
    NA_character_,
    INS_Vooropleiding_voor_HO_profiel_standaard
    ),
    ## Een extra variabele wordt aangemaakt, waarbij alleen voor
    ## VWO profielen deze ingevuld wordt. In de standaardvariabele staan ook
    ## de HAVO profielen, waar beschikbaar.
    INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO =
      if_else(str_detect(INS_Vooropleiding_voor_HO_profiel, "vwo"),
              INS_Vooropleiding_voor_HO_profiel_standaard, NA_character_
      ),
    ## Per profiel los (voor modelleren vooral relevant, deze features
    ## kunnen beter begrepen worden)
    INS_Vooropleiding_voor_HO_profiel_standaard_NT =
      str_detect(INS_Vooropleiding_voor_HO_profiel, "techniek"),
    INS_Vooropleiding_voor_HO_profiel_standaard_NG =
      str_detect(INS_Vooropleiding_voor_HO_profiel, "gezondheid"),
    INS_Vooropleiding_voor_HO_profiel_standaard_EM =
      str_detect(INS_Vooropleiding_voor_HO_profiel, "maatschappij"),
    INS_Vooropleiding_voor_HO_profiel_standaard_CM =
      str_detect(INS_Vooropleiding_voor_HO_profiel, "cultuur"),
    ## Combinatieprofiel los definieeren
    ## (of een student twee profielen heeft gedaan)
    INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_combinatieprofiel =
      str_detect(INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO, "/")
  )

## Profielen omschrijven naar afkortingen
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
    "INS_Vooropleiding_voor_HO_profiel_standaard",
    "INS_Vooropleiding_voor_HO_profiel_standaard_afk",
    mapping_table_name = "Mapping_INS_Profiel_omschrijving_Profiel_afkorting"
  )
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mapping_translate(
    "INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO",
    "INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_afk",
    mapping_table_name = "Mapping_INS_Profiel_omschrijving_Profiel_afkorting"
  )

## Maak variabele INS_Vooropleiding_voor_HO_profiel_standaard_zonder_combinatie,
## deze variabele bevat de veel voorkomende variaties en dus niet: combis'van natuur en maatschappij

## Maak helper object voor profielen, zie Stijlgids Principe F, self-documenting code
vProfielen_levels <- c("NT",
                       "NG",
                       "NT & NG",
                       "EM", "CM",
                       "EM & CM")

vProfielen_genegeerd <- c("NG & CM",
                          "NG & EM",
                          "NT & EM",
                          "NT & CM")

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(
    ## Variabele voor VWO en HAVO
    INS_Vooropleiding_voor_HO_profiel_standaard_zonder_combinatie = if_else(
      INS_Vooropleiding_voor_HO_profiel_standaard %in% vProfielen_genegeerd,
      NA_character_,
      INS_Vooropleiding_voor_HO_profiel_standaard
    ),
    ## Maak factor
    INS_Vooropleiding_voor_HO_profiel_standaard_zonder_combinatie = factor(
      INS_Vooropleiding_voor_HO_profiel_standaard_zonder_combinatie,
      levels = vProfielen_levels
    ),
    ## Variabele alleen VWO
    INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_zonder_combinatie = if_else(
      INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO %in% vProfielen_genegeerd,
      NA_character_,
      INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO
    ),
    ## Maak factor
    INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_zonder_combinatie = factor(
      INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_zonder_combinatie,
      levels = vProfielen_levels
    )
  )


## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.3 Studiejaar & Uitschrijving 1 Feb ####

## TODO logic aanpassen
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  ## Studiejaar wordt per student-opleiding combinatie berekend
  group_by(
    INS_Studentnummer,
    OPL_code_historisch
  ) %>%
  # Sorteer het bestand op inschrijvingsjaar zodat het studiejaar bepaald
  # kan worden
  arrange(
    INS_Studentnummer,
    OPL_code_historisch,
    INS_Inschrijvingsjaar
  ) %>%
  mutate(
    # Bereken INS_Studiejaar op basis van plek in lijst unieke Inschrijvingsjaar (dit kan omdat
    # het hierboven arranged is)
    INS_Studiejaar = match(INS_Inschrijvingsjaar, unique(INS_Inschrijvingsjaar)),
    INS_Tussenjaren_binnen_opleiding = sum(!((min(INS_Inschrijvingsjaar):max(INS_Inschrijvingsjaar)) %in% INS_Inschrijvingsjaar)),
    ## Maak een boolean variabele aan om aan te geven of het
    ## inschrijvingsjaar het EOI-jaar is
    INS_Inschrijvingsjaar_is_EOI =
      INS_Inschrijvingsjaar == INS_Inschrijvingsjaar_EOI
  ) %>%
  ungroup()

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(
    ## Bepaal of de uitschrijving in hetzelfde studiejaar
    ## voor 1 februari was
    INS_Uitschrijving_voor_1_feb = INS_Datum_uitschrijving <= as_date(
      paste0(INS_Inschrijvingsjaar + 1, "-01-31")
    ),
    ## Bepaal of deze uitschrijving een EOI was. Alleen in dit geval wordt
    ## de BSA ontlopen
    INS_Uitschrijving_voor_1_feb_EOI =
      INS_Inschrijvingsjaar_EOI ==
      INS_Inschrijvingsjaar &
      INS_Uitschrijving_voor_1_feb
  ) %>%
  ## Groepeer per student/opleiding
  group_by(INS_Studentnummer, OPL_code_historisch) %>%
  mutate(
    INS_Herinschrijving_jaar_2_na_uitschrijving_voor_1_feb =
      case_when(
        ## Als de student in geen enkel jaar een TRUE heeft op de
        ## variabele INS_EOI_uitschrijving_voor_1_feb, wordt het
        ## een FALSE
        sum(INS_Uitschrijving_voor_1_feb_EOI) == 0 ~ FALSE,
        ## Bij alle overgebleven inschrijvingen is er dus wel sprake van
        ## het ontlopen van het bsa. Als het maximale studiejaar groter
        ## is dan 1, betekent dit dat er een herinschrijving is geweest
        max(INS_Studiejaar) > 1 ~ TRUE,
        ## in alle overige gevallen is hier geen sprake van.
        .default = FALSE
      )
  ) %>%
  ungroup() %>%
  mutate(INS_Studiejaar_gecorrigeerd_uitschrijving_1_feb_EOI = case_when(
    INS_Herinschrijving_jaar_2_na_uitschrijving_voor_1_feb == TRUE & INS_Studiejaar > 1 ~ INS_Studiejaar - 1,
    .default = INS_Studiejaar
  ))


## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.4 Tussenjaren ####

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  ## Maak variabele INS_Tussenjaren_voor_M voor studenten met één of meer
  ## tussenjaren tussen hun bachelor en master
  mutate(
    ## Maak variabele om te zien of inschrijving direct is
    INS_Direct = (
      INS_Hoogste_vooropleiding_jaar_1CHO + 1) == INS_Inschrijvingsjaar_EOI,
    ## Maak variabele INS_Indicatie_Tussenjaar_voor_B voor studenten met een
    ## tussenjaar voor hun eerste jaar in het hoger onderwijs
    INS_Indicatie_Tussenjaar_voor_B = (
      INS_Hoogste_vooropleiding_jaar_1CHO + 1 < INS_Inschrijvingsjaar_EOI &
        OPL_Fase == "B" &
        INS_Verblijfsjaren_wetenschappelijk_onderwijs == 1 &
        INS_Verblijfsjaren_hoger_onderwijs == 1),
    ## Maak variabele INS_Indicatie_Tussenjaar_voor_P voor studenten met één of meer
    ## tussenjaren tussen hun bachelor en hun premaster
    INS_Indicatie_Tussenjaar_voor_P = (
      INS_Hoogste_vooropleiding_jaar_1CHO + 1 < INS_Inschrijvingsjaar_EOI &
        OPL_Fase == "S" &
        # De student is nog niet begonnen met de premasterfase in dit jaar
        INS_Verblijfsjaar_type_onderwijs_binnen_HO <= 1),
    INS_Indicatie_Tussenjaar_voor_M = (
      INS_Hoogste_vooropleiding_jaar_1CHO + 1 < INS_Inschrijvingsjaar_EOI &
        OPL_Fase == "M" &
        # De student heeft niet eerder een M gedaan
        INS_Verblijfsjaar_type_onderwijs_binnen_HO <= 1
    )
  )

## We generaliseren de variabelen voor alle inschrijvingen van de student in de opleiding
Inschrijvingen_1cho <- Inschrijvingen_1cho  %>%
  group_by(INS_Studentnummer, OPL_code_historisch) %>%
  mutate(
    INS_Indicatie_Tussenjaar_voor_B = INS_Indicatie_Tussenjaar_voor_B[first(which(INS_Studiejaar == 1))],
    INS_Indicatie_Tussenjaar_voor_P = INS_Indicatie_Tussenjaar_voor_P[first(which(INS_Studiejaar == 1))],
    INS_Indicatie_Tussenjaar_voor_M = INS_Indicatie_Tussenjaar_voor_M[first(which(INS_Studiejaar == 1))],
    INS_Direct = INS_Direct[first(which(INS_Studiejaar == 1))]
  ) %>%
  ungroup() %>%
  mutate(INS_Indicatie_Tussenjaar = coalesce(INS_Indicatie_Tussenjaar_voor_B,
                                   INS_Indicatie_Tussenjaar_voor_P,
                                   INS_Indicatie_Tussenjaar_voor_M))


## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.5 Aansluiting ####
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(
    ## Aansluiting bepalen obv direct en tussenjaar. Verschil tussen
    ## switch intern en extern wordt later bepaald
    INS_Aansluiting =
      case_when(
        INS_Direct &
          INS_Hoogste_vooropleiding_BRIN_1CHO == config::get("metadata_institution_BRIN") ~
          "Direct na diploma instelling",
        INS_Direct ~ "Direct na diploma extern",
        INS_Indicatie_Tussenjaar == TRUE ~ "Tussenjaar",
        #SUC_Instroom_switch_VU == TRUE ~ "Switch binnen VU",
        INS_Verblijfsjaar_type_onderwijs_binnen_HO > INS_Studiejaar ~ "Switch / Tweede studie",
        .default = "Onbekend"
      )
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(Inschrijvingen_1cho)

clear_script_objects()
