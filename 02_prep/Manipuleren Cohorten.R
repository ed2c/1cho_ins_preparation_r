## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Manipuleren Cohorten.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2021 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##

##'*INFO* See metadata/data_dictionary/Bestandsbeschrijving EOIcohort_VSNU _1cH2021.docx
## Doel: In dit script worden verscheidene succesvariabelen gedefinieerd op
## basis van herinschrijvings -en examengegevens uit het Cohortenbestand
## (waaronder doorstroom en succes binnen en buiten de VU). Bovendien worden
## enkele Inschrijvings variabelen bepaald: Onderwijsherkomst Soort_eerstejaars.
##
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lees alle benodigde bestanden in:

Cohorten <- read_file_proj("INS_Cohorten")

## Lees CROHO in voor de nominale studieduur
CROHO_per_jaar <- read_file_proj("CROHO_per_jaar",
                        dir = "02_prepared")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. SUCCESVARIABELEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Cohorten <- Cohorten %>%
  filter(INS_Eerste_jaar_opleiding_en_instelling >= config::get("first_year"))

## Zet per student-cohortjaar-opleidingscode de variabelen
## INS_Examen_na_XX_jaar en INS_Herinschrijving_na_XX_jaar in rijen onder elkaar
## (klap brede tabel om naar lange tabel).
## Bepaal successvariabelen en klap de tabel terug.
Cohorten_succes <- Cohorten %>%
  ## Selecteer beperkt aantal rijen tbv ontwikkeling script
  ## filter(row_number() < 1000) %>%
  ## Selecteer per rij de sleutelvelden en de velden die omgeklapt moeten worden.
  select(
    INS_Studentnummer,
    OPL_Code_in_jaar,
    INS_Eerste_jaar_opleiding_en_instelling,
    starts_with("INS_Examen_na"),
    starts_with("INS_Herinschrijving_na")
  ) %>%
  ## Zet INS_Herinschrijving_na_1_jaar vast, omdat deze in alle rijen beschikbaar
  ## moet zijn (ivm bepaling Diplomarendement van herinschrijvers na jaar 1).
  mutate(INS_Herinschrijving_na_1_jaar_vastgezet = INS_Herinschrijving_na_1_jaar) %>%
  ## Klap de tabel in twee stappen om. In twee stappen omdat de
  ## INS_Herinschrijvings -en INS_Examen-velden naast elkaar moeten blijven
  ## (en niet onder elkaar; dit zou gebeuren als je tabel in 1 keer omklapt).
  gather(
    INS_Herinschrijving_key, INS_Herinschrijving,
    -INS_Studentnummer,
    -OPL_Code_in_jaar,
    -INS_Eerste_jaar_opleiding_en_instelling,
    -INS_Herinschrijving_na_1_jaar_vastgezet,
    -starts_with("INS_Examen_na")
  ) %>%
  gather(
    INS_Examen_key, INS_Examen,
    -INS_Herinschrijving_key,
    -INS_Herinschrijving,
    -INS_Studentnummer,
    -OPL_Code_in_jaar,
    -INS_Eerste_jaar_opleiding_en_instelling,
    -INS_Herinschrijving_na_1_jaar_vastgezet
  ) %>%
  ## Alleen van hetzelfde jaar zijn combinaties van Examen en Herinschrijving
  ## relevant, terwijl nu van iedere combinatie een rij is gemaakt.
  ## Daarom alleen rijen van hetzelfde jaar filteren.
  filter(
    str_extract(
      INS_Herinschrijving_key,
      "[1-9]"
    ) == str_extract(INS_Examen_key, "[1-9]")
  ) %>%
  ## Selecteer rijen waarvoor INS_Examen en INS_Herinschrijving geen NA's zijn.
  filter(!is.na(INS_Examen) & !is.na(INS_Herinschrijving)) %>%
  ## Bepaal de variabele Jaar Deze is een deel van de key voor alle value's.
  ## Extraheer het getal uit de key-variabele
  mutate(Jaar = str_extract(INS_Examen_key, "[1-9]")) %>%
  ## Verwijder de key-velden INS_Herinschrijving_key en INS_Examen_key
  select(
    -INS_Herinschrijving_key,
    -INS_Examen_key
  ) %>%
  ## Bepaal de verschillende successvariabelen
  ## Hierbij is de betekenis van waarden voor INS_Examen (zie documentatie
  ## 1cHO-Cohortbestand, met beslisboom):
  ## Uitval bij opleiding
  mutate(
    SUC_Type_uitstroom_cohorten =
      case_when(
        # First check if we can determine the status
        is.na(INS_Examen) ~ NA_character_,
        # Diploma gets highest priority - successful completion
        INS_Examen %in% c("a", "b") ~ "Diploma",
        # Then check various types of switches and dropouts
        # Order matters: from most specific to most general
        # Switches within same institution
        !(INS_Examen %in% c("a", "b")) &
          INS_Herinschrijving %in% c("2", "3", "4", "5") ~ "Switch binnen instelling",
        #'*INFO* Hoe gaat dit bij HBO instelling (nu hebben die dit bestand nog niet)
        # Switch to other university
        !(INS_Examen %in% c("a", "b")) &
          INS_Herinschrijving %in% c("6", "7", "9", "10") ~ "Switch naar WO",
        # Switch to HBO
        !(INS_Examen %in% c("a", "b")) &
          INS_Herinschrijving %in% c("8", "11") ~ "Switch naar HBO",
        # Complete dropout from higher education
        !(INS_Examen %in% c("a", "b")) &
          INS_Herinschrijving %in% c("12", "13") ~ "Uitval HO",
        .default = "Onbekend"
      ),
    SUC_Uitval =
      case_when(
        ## Als de Examen-variabele leeg is, is de student nog aan het
        ## studeren. De variabele kan dus ook niet bepaald worden.
        is.na(INS_Examen) ~ NA,
        ## Er is sprake van uitval bij opleiding als student diploma
        ## voor EOI-opleiding niet heeft gehaald
        !(INS_Examen %in% c("a", "b")) &
          ## en zich daarna niet herinschrijft voor dezelfde opleiding
          INS_Herinschrijving != 1 ~
          TRUE,
        ## In alle andere gevallen is er geen sprake van uitval
        .default = FALSE
      )
  ) %>%
  ## Uitval instelling
  mutate(
    SUC_Uitval_instelling =
      case_when(
        is.na(INS_Examen) ~ NA,
        !(INS_Examen %in% c("a", "b")) &
        ## Als student zich daarna niet herinschrijft bij instelling
          INS_Herinschrijving %in% c("6", "7", "8", "9", "10", "11", "12", "13") ~
          TRUE,
        ## In alle andere gevallen is er geen sprake van uitval
        .default = FALSE
      )
  ) %>%
  ## Uitval uit HO
  mutate(
    SUC_Uitval_HO =
      case_when(
        is.na(INS_Examen) ~ NA,
        !(INS_Examen %in% c("a", "b")) &
          ## en zich daarna niet herinschrijft in HO
          INS_Herinschrijving %in% c("12", "13") ~
          TRUE,
        ## In alle andere gevallen is er geen sprake van uitval
        .default = FALSE
      )
  ) %>%
  ## Switch binnen instelling
  mutate(
    SUC_Switch_binnen_instelling =
      case_when(
        is.na(INS_Examen) ~ NA,
        !(INS_Examen %in% c("a", "b")) &
          ## zich ## daarna herinschrijft voor andere opleiding binnen vu
          ## (opleidng binnen gelijke, lagere of hogere fase)
          INS_Herinschrijving %in% c("2", "3", "4", "5") ~
          TRUE,
        ## In alle andere gevallen is er geen sprake van switch
        .default = FALSE
      )
  ) %>%
  ## Switch binnen WO
  mutate(
    SUC_Switch_naar_WO =
      case_when(
        is.na(INS_Examen) ~ NA,
        !(INS_Examen %in% c("a", "b")) &
        ## als student daarna herinschrijft voor opleiding bij andere universiteit
        ## (opleidng binnen gelijke, lagere of hogere fase)
        ## (INS_Herinschrijving).
          INS_Herinschrijving %in% c("6", "7", "9", "10") ~
          TRUE,
        ## In alle andere gevallen is er geen sprake van switch
        .default = FALSE
      )
  ) %>%
  ## Switch naar HBO
  mutate(
    SUC_Switch_naar_HBO =
      case_when(
        is.na(INS_Examen) ~ NA,
        ## Er is sprake van switch naar hbo als student diploma
        ## voor EOI-opleiding niet heeft gehaald (INS_Examen) en zich
        ## daarna herinschrijft voor opleiding binnen hbo
        ## (opleidng binnen gelijke of hogere fase)
        ## (INS_Herinschrijving).
        !(INS_Examen %in% c("a", "b")) &
          INS_Herinschrijving %in% c("8", "11") ~
          TRUE,
        ## In alle andere gevallen is er geen sprake van switch
        TRUE ~
          FALSE
      )
  ) %>%
  ## Diplomarendement van herinschrijvers na jaar 1
  ## Gebruik: INS_Herinschrijving_na_1_jaar_vastgezet
  mutate(
    SUC_Diploma_herinschrijvers =
      case_when(
        is.na(INS_Examen) ~ NA,
        ## Er is sprake van diploma herinschrijving als student zich
        ## na 1 jaar herinschrijft voor dezelfde opleiding
        ## (INS_Herinschrijving_na_1_jaar_vastgezet) en het diploma
        ## voor EOI-opleiding haalt (INS_Examen).
        INS_Examen %in% c("a", "b") &
          INS_Herinschrijving_na_1_jaar_vastgezet == 1 ~
          TRUE,
        ## In alle andere gevallen is er geen sprake van diploma herinschrijvers
        .default = FALSE
      )
  ) %>%
  ## Diplomarendement oorspronkelijk cohort
  mutate(
    SUC_Diploma =
      case_when(
        is.na(INS_Examen) ~ NA,
        ## Er is sprake van diploma als student diploma
        ## voor EOI-opleiding heeft gehaald (INS_Examen).
        INS_Examen %in% c("a", "b") ~
          TRUE,
        ## In alle andere gevallen is er geen sprake van diploma
        .default = FALSE
      )
  ) %>%
  ## Verwijder de velden die niet meer nodig zijn
  select(
    -INS_Herinschrijving_na_1_jaar_vastgezet,
    -INS_Examen,
    -INS_Herinschrijving
  ) %>%
  ## Tabel in een keer omklappen naar lange tabel
  gather(
    key,
    value,
    -INS_Studentnummer,
    -OPL_Code_in_jaar,
    -INS_Eerste_jaar_opleiding_en_instelling,
    -Jaar
  )

## Bepaal nieuwe succesvariabelen die diploma, switch -en uitvalvariabelen uit
## Cohorten_succes samenvatten
Cohorten_succes_samenvatting <- Cohorten_succes %>%
  ## Selecteer de rijen waarvoor de verschillende succesvariabelen
  ## (veld "key") de waarde TRUE hebben: veld "value" = TRUE
  filter(value == TRUE) %>%
  ## Groepeer velden
  group_by(
    INS_Studentnummer,
    OPL_Code_in_jaar,
    INS_Eerste_jaar_opleiding_en_instelling,
    key
  ) %>%
  ## Bepaal per key/succesvariabele het eerste jaar waarin de waarde = TRUE
  summarise(Jaar_min = as.numeric(min(Jaar)), .groups = "keep") %>%
  ## Bepaal per succesvariabele een variabele SUC_x_aantal_jaar_cohorten
  ## (geeft aan na hoeveel jaar de situatie uit succes variabele plaatsvindt):
  ## Maak een nieuwe key ter voorbereiding op volgende spread-stap.
  mutate(key_nw = paste(key, "aantal_jaar_cohorten", sep = "_")) %>%
  ## Tabel omklappen naar brede tabel met de waarden voor key_nw als kolommen.
  ## Resultaat is een brede tabel met per student/jaar/opleiding zoveel
  ## rijen als er gevulde succesvariabelen zijn.
  spread(
    key = key_nw,
    value = Jaar_min
  ) %>%
  ## Verwijder oude key
  select(-key) %>%
  ## Bewerkingen om per student/jaar/opleiding 1 rij te krijgen:
  ## Stap 1: Tabel in 1 keer omklappen naar lange tabel
  gather(
    key,
    value,
    SUC_Diploma_aantal_jaar_cohorten,
    SUC_Diploma_herinschrijvers_aantal_jaar_cohorten,
    SUC_Switch_binnen_instelling_aantal_jaar_cohorten,
    SUC_Switch_naar_HBO_aantal_jaar_cohorten,
    SUC_Switch_naar_WO_aantal_jaar_cohorten,
    SUC_Uitval_aantal_jaar_cohorten,
    SUC_Uitval_HO_aantal_jaar_cohorten,
    SUC_Uitval_instelling_aantal_jaar_cohorten,
    -INS_Studentnummer,
    -OPL_Code_in_jaar,
    -INS_Eerste_jaar_opleiding_en_instelling
  ) %>%
  ## Stap 2: Selecteer rijen waarvoor SUC_x_aantal_jaar_cohorten een waarde heeft
  filter(!is.na(value)) %>%
  ## Stap 3: Tabel omklappen naar brede tabel
  spread(
    key = key,
    value = value
  ) %>%
  ## Voer voor onderstaande variabelen dezelfde functie uit
  mutate_at(
    vars(
      SUC_Diploma_herinschrijvers_aantal_jaar_cohorten,
      SUC_Diploma_aantal_jaar_cohorten,
      SUC_Uitval_aantal_jaar_cohorten,
      SUC_Switch_binnen_instelling_aantal_jaar_cohorten,
      SUC_Uitval_instelling_aantal_jaar_cohorten,
      SUC_Uitval_HO_aantal_jaar_cohorten,
      SUC_Switch_naar_WO_aantal_jaar_cohorten,
      SUC_Switch_naar_HBO_aantal_jaar_cohorten
    ),
    # De functie is as.numeric.
    as.numeric
  )

test <- Cohorten_succes %>%

## Ga verder met tabel Cohorten_succes: zodanig bewerken naar brede tabel zodat
## tabel aan Cohorten gekoppeld kan worden.
Cohorten_succes <- Cohorten_succes %>%
  ## Maak nieuw key-veld welke bestaat uit samenstelling van oude key en Jaar.
  ## Gebruik "na" of "binnen" in key afhankelijk of het een variabele mbt
  ## diplomarendement betreft.
  mutate(key = case_when(
    str_detect(key, coll("Diploma", ignore_case = TRUE)) ~
      paste(key, "binnen", Jaar, "jaar_cohorten", sep = "_"),
    TRUE ~
      paste(key, "na_jaar", Jaar, "cohorten", sep = "_")
  )) %>%
  ## Verwijder de kolom Jaar
  select(-Jaar) %>%
  ## Klap de tabel in een keer terug naar een brede tabel met 1 rij per
  ## combinatie van student-cohortjaar-opleidingscode.
  spread(
    key = key,
    value = value
  )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## RUIM TUSSENTIJDS OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Verwijder niet langer benodigde objecten om geheugen vrij te maken
rm(Cohorten_succes)
gc()
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## Controleer of er duplicaten zijn ontstaan
assert_no_duplicates_in_group(Cohorten, c(
  "INS_Studentnummer",
  "INS_Eerste_jaar_opleiding_en_instelling",
  "OPL_Code_in_jaar"
))

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

## Koppel de succes-samenvatting-tabel aan Cohorten
Cohorten <- Cohorten %>%
  left_join(Cohorten_succes_samenvatting, by = c(
      "INS_Studentnummer",
      "OPL_Code_in_jaar",
      "INS_Eerste_jaar_opleiding_en_instelling"
    ), suffix = c("", ".y")) %>%
  select(-ends_with(".y")) %>%
  ## Wijzig SUC_Diploma_aantal_jaar_cohorten naar een integer
  mutate(SUC_Diploma_aantal_jaar_cohorten = as.integer(SUC_Diploma_aantal_jaar_cohorten))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## RUIM TUSSENTIJDS OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Verwijder niet langer benodigde objecten om geheugen vrij te maken
rm(Cohorten_succes_samenvatting)
gc()
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## Controleer of er duplicaten zijn ontstaan
assert_no_duplicates_in_group(Cohorten, c(
  "INS_Studentnummer",
  "INS_Eerste_jaar_opleiding_en_instelling",
  "OPL_Code_in_jaar"
))

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^



write_file_proj(Cohorten)

clear_script_objects()
