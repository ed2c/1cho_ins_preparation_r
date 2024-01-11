## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Manipuleren Cohorten.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2021 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: In dit script worden verscheidene succesvariabelen gedefinieerd op
## basis van herinschrijvings -en examengegevens uit het Cohortenbestand
## (waaronder doorstroom en succes binnen en buiten de VU). Bovendien worden
## enkele Inschrijvings variabelen bepaald: Onderwijsherkomst Soort_eerstejaars.
## Het cohortenbestand kan daarna direct aan de analyseset gekoppeld worden.
##
##
## Opmerkingen:
## 1) Gebruik de Confluence documentatie van het 1cHO-Cohortenbestand om dit script te
## begrijpen.
## 2) Twee succesvariabelen (uitval opleiding,
## diplomarendement herinschrijvers) worden op 2 plaatsen bepaald, nl.
## Manipulatie Inschrijvingen.R en Manipulatie Cohorten.R.
## Vergelijk de 2 sets succesvariabelen met script
## 20.Test/Test_succesvariabelen_inschrijvingen_cohorten.R.
## 3) In voorjaar: gebruik oorspronkelijk 1cHO-Cohortenbestand.
## In najaar: gebruik schattingen-cohortenbestand.
## 4) In voorjaar: gebruik INS_Inschrijvingen_voorjaar.rds.
## In najaar: gebruik INS_Inschrijvingen_najaar.rds
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

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. ASSERTIONS ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
assert_no_duplicates_in_group(
  Cohorten,
  c(
    "INS_Studentnummer",
    "INS_Eerste_jaar_opleiding_en_instelling",
    "INS_Opleidingscode_actueel"
  )
)
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2A. BEWERKEN: SUCCESVARIABELEN MBV INS_HERINSCHRIJVING, INS_EXAMEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Bepaal voor 8 jaar de volgende nieuwe succesvariabelen, gebruik makend van
## INS_Herinschrijving -en INS_Examen-velden:
## - Uitval bij opleiding
## - Uitval uit instelling
## - Uitval uit HO - NIEUW
## - Switch binnen instelling
## - Switch binnen WO - NIEUW
## - Switch binnen HBO - NIEUW
## - Diplomarendement van herinschrijvers na jaar 1 (voor 9 jaar)
## - Diplomarendement oorspronkelijk cohort (voor 9 jaar)
## Bovendien samenvatting van bovenstaande succesvariabelen, uitgedrukt in:
## - SUC_Diploma_herinschrijvers_aantal_jaar_cohorten
## - SUC_Diploma_aantal_jaar_cohorten
## - SUC_Uitval_aantal_jaar_cohorten
## - SUC_Switch_binnen_vu_aantal_jaar_cohorten
## - SUC_Uitval_vu_aantal_jaar_cohorten
## - SUC_Uitval_HO_aantal_jaar_cohorten
## - SUC_Switch_WO_aantal_jaar_cohorten
## - SUC_Switch_HBO_aantal_jaar_cohorten

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
    INS_Opleidingscode_actueel,
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
    -INS_Opleidingscode_actueel,
    -INS_Eerste_jaar_opleiding_en_instelling,
    -INS_Herinschrijving_na_1_jaar_vastgezet,
    -starts_with("INS_Examen_na")
  ) %>%
  gather(
    INS_Examen_key, INS_Examen,
    -INS_Herinschrijving_key,
    -INS_Herinschrijving,
    -INS_Studentnummer,
    -INS_Opleidingscode_actueel,
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
  ##      Ba (I) cohort	                    Ma (V) cohort
  ## a	Equi-opl nieuwe stelsel (Ba)	    equi-opl nieuwe stelsel (Ma)
  ## b	Equi-opl oude stelsel (Doc)	        equi-opl oude stelsel (Pdoc)
  ## c	hoop/instelling (Ba)	            hoop/instelling (Ma)
  ## d	hoop/instelling (Doc)	            hoop/instelling (PDoc)
  ## e	instelling (Ba)	                    instelling (Ma)
  ## f	instelling (Doc)	                instelling (PDoc)
  ## g	hoop/srt_HO (Ba)	                hoop/srt_HO (Ma)
  ## h	hoop/srt_HO (Doc)	                hoop/srt_HO (Pdoc)
  ## i	Srt_HO (Ba)	                        srt_HO (Ma)
  ## j	Srt_HO (Doc)	                    srt_HO (Pdoc)
  ## k	niet srt_HO	                        niet srt_HO
  ## l	master behaald zelfde instelling
  ## p	master behaald andere instelling
  ## q (alleen bij schattingen)               bachelor behaald zelfde instelling
  ## Uitval bij opleiding
  mutate(
    SUC_Uitval =
      case_when(
        ## Als de Examen-variabele leeg is, is de student nog aan het
        ## studeren. De variabele kan dus ook niet bepaald worden.
        is.na(INS_Examen) ~
          NA,
        ## Er is sprake van uitval bij opleiding als student diploma
        ## voor EOI-opleiding niet heeft gehaald (INS_Examen) en zich
        ## daarna niet herinschrijft voor dezelfde opleiding
        ## (INS_Herinschrijving).
        !(INS_Examen %in% c("a", "b")) &
          INS_Herinschrijving != 1 ~
          TRUE,
        ## In alle andere gevallen is er geen sprake van uitval
        TRUE ~
          FALSE
      )
  ) %>%
  ## Uitval VU
  mutate(
    SUC_Uitval_vu =
      case_when(
        ## Als de Examen-variabele leeg is, is de student nog aan het
        ## studeren. De variabele kan dus ook niet bepaald worden.
        is.na(INS_Examen) ~
          NA,
        ## Er is sprake van uitval vu als student diploma
        ## voor EOI-opleiding niet heeft gehaald (INS_Examen) en zich
        ## daarna niet herinschrijft bij vu (INS_Herinschrijving).
        !(INS_Examen %in% c("a", "b")) &
          INS_Herinschrijving %in% c("6", "7", "8", "9", "10", "11", "12", "13") ~
          TRUE,
        ## In alle andere gevallen is er geen sprake van uitval
        TRUE ~
          FALSE
      )
  ) %>%
  ## Uitval uit HO
  mutate(
    SUC_Uitval_HO =
      case_when(
        ## Als de Examen-variabele leeg is, is de student nog aan het
        ## studeren. De variabele kan dus ook niet bepaald worden.
        is.na(INS_Examen) ~
          NA,
        ## Er is sprake van uitval uit HO als student diploma
        ## voor EOI-opleiding niet heeft gehaald (INS_Examen) en zich
        ## daarna niet herinschrijft in HO (INS_Herinschrijving)
        !(INS_Examen %in% c("a", "b")) &
          INS_Herinschrijving %in% c("12", "13") ~
          TRUE,
        ## In alle andere gevallen is er geen sprake van uitval
        TRUE ~
          FALSE
      )
  ) %>%
  ## Switch binnen instelling
  mutate(
    SUC_Switch_binnen_vu =
      case_when(
        ## Als de Examen-variabele leeg is, is de student nog aan het
        ## studeren. De variabele kan dus ook niet bepaald worden.
        is.na(INS_Examen) ~ NA,
        ## Er is sprake van switch binnen vu als student diploma
        ## voor EOI-opleiding niet heeft gehaald (INS_Examen) en zich
        ## daarna herinschrijft voor andere opleiding binnen vu
        ## (opleidng binnen gelijke, lagere of hogere fase)
        ## (INS_Herinschrijving).
        !(INS_Examen %in% c("a", "b")) &
          INS_Herinschrijving %in% c("2", "3", "4", "5") ~
          TRUE,
        ## In alle andere gevallen is er geen sprake van switch
        TRUE ~
          FALSE
      )
  ) %>%
  ## Switch binnen WO
  mutate(
    SUC_Switch_WO =
      case_when(
        ## Als de Examen-variabele leeg is, is de student nog aan het
        ## studeren. De variabele kan dus ook niet bepaald worden.
        is.na(INS_Examen) ~ NA,
        ## Er is sprake van switch binnen wo als student diploma
        ## voor EOI-opleiding niet heeft gehaald (INS_Examen) en zich
        ## daarna herinschrijft voor opleiding bij andere universiteit
        ## (opleidng binnen gelijke, lagere of hogere fase)
        ## (INS_Herinschrijving).
        !(INS_Examen %in% c("a", "b")) &
          INS_Herinschrijving %in% c("6", "7", "9", "10") ~
          TRUE,
        ## In alle andere gevallen is er geen sprake van switch
        TRUE ~
          FALSE
      )
  ) %>%
  ## Switch binnen HBO
  mutate(
    SUC_Switch_HBO =
      case_when(
        ## Als de Examen-variabele leeg is, is de student nog aan het
        ## studeren. De variabele kan dus ook niet bepaald worden.
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
        ## Als de Examen-variabele leeg is, is de student nog aan het
        ## studeren. De variabele kan dus ook niet bepaald worden.
        is.na(INS_Examen) ~ NA,
        ## Er is sprake van diploma herinschrijving als student zich
        ## na 1 jaar herinschrijft voor dezelfde opleiding
        ## (INS_Herinschrijving_na_1_jaar_vastgezet) en het diploma
        ## voor EOI-opleiding haalt (INS_Examen).
        INS_Examen %in% c("a", "b") &
          INS_Herinschrijving_na_1_jaar_vastgezet == 1 ~
          TRUE,
        ## In alle andere gevallen is er geen sprake van diploma herinschrijvers
        TRUE ~
          FALSE
      )
  ) %>%
  ## Diplomarendement oorspronkelijk cohort
  mutate(
    SUC_Diploma =
      case_when(
        ## Als de Examen-variabele leeg is, is de student nog aan het
        ## studeren. De variabele kan dus ook niet bepaald worden.
        is.na(INS_Examen) ~ NA,
        ## Er is sprake van diploma als student diploma
        ## voor EOI-opleiding heeft gehaald (INS_Examen).
        INS_Examen %in% c("a", "b") ~
          TRUE,
        ## In alle andere gevallen is er geen sprake van diploma
        TRUE ~
          FALSE
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
    -INS_Opleidingscode_actueel,
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
    INS_Opleidingscode_actueel,
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
    SUC_Switch_binnen_vu_aantal_jaar_cohorten,
    SUC_Switch_HBO_aantal_jaar_cohorten,
    SUC_Switch_WO_aantal_jaar_cohorten,
    SUC_Uitval_aantal_jaar_cohorten,
    SUC_Uitval_HO_aantal_jaar_cohorten,
    SUC_Uitval_vu_aantal_jaar_cohorten,
    -INS_Studentnummer,
    -INS_Opleidingscode_actueel,
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
      SUC_Switch_binnen_vu_aantal_jaar_cohorten,
      SUC_Uitval_vu_aantal_jaar_cohorten,
      SUC_Uitval_HO_aantal_jaar_cohorten,
      SUC_Switch_WO_aantal_jaar_cohorten,
      SUC_Switch_HBO_aantal_jaar_cohorten
    ),
    # De functie is as.numeric.
    as.numeric
  )

## Ga verder met tabel Cohorten_succes: zodanig bewerken naar brede tabel zodat
## tabel aan Cohorten gekoppeld kan worden.
Cohorten_succes <- Cohorten_succes %>%
  ## Maak nieuw key-veld welke bestaat uit samenstelling van oude key en Jaar.
  ## Gebruik "na" of "binnen" in key afhankelijk of het een variabele mbt
  ## diplomarendement betreft.
  mutate(key = case_when(
    stringr::str_detect(key, coll("Diploma", ignore_case = TRUE)) ~
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

## Koppel de succes-tabel aan Cohorten
Cohorten <- Cohorten %>%
  left_join(Cohorten_succes, by = c(
           "INS_Studentnummer" =
           "INS_Studentnummer",
           "INS_Opleidingscode_actueel" =
           "INS_Opleidingscode_actueel",
           "INS_Eerste_jaar_opleiding_en_instelling" =
           "INS_Eerste_jaar_opleiding_en_instelling"),
            suffix = c("", ".y")) %>%
  select(-ends_with(".y")) %>%
  ## Bepaal de variabele: SUC_Diploma_herinschrijvers_binnen_9_jaar_cohorten.
  ## Variabele kon niet in voorgaande stap bepaald worden (omgeklapte tabel)
  ## omdat er geen veld bestaat voor INS_Herinschrijving_na_9_jaar.
  ## Daarom hier apart bepalen.
  mutate(
    SUC_Diploma_herinschrijvers_binnen_9_jaar_cohorten =
      case_when(
        is.na(INS_Examen_na_9_jaar) ~
          NA,
        INS_Examen_na_9_jaar %in% c("a", "b") &
          INS_Herinschrijving_na_1_jaar == 1 ~
          TRUE,
        TRUE ~
          FALSE
      )
  ) %>%
  ## Bepaal de variabele: SUC_Diploma_binnen_9_jaar_cohorten.
  ## Variabele kon niet in voorgaande stap bepaald worden (omgeklapte tabel)
  ## omdat er geen veld bestaat voor INS_Herinschrijving_na_9_jaar.
  ## Daarom hier apart bepalen.
  mutate(
    SUC_Diploma_binnen_9_jaar_cohorten =
      case_when(
        is.na(INS_Examen_na_9_jaar) ~
          NA,
        INS_Examen_na_9_jaar %in% c("a", "b") ~
          TRUE,
        TRUE ~
          FALSE
      )
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
  "INS_Opleidingscode_actueel"
))
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

## Koppel de succes-samenvatting-tabel aan Cohorten
Cohorten <- Cohorten %>%
  left_join(Cohorten_succes_samenvatting, by = c(
      "INS_Studentnummer" =
        "INS_Studentnummer",
      "INS_Opleidingscode_actueel" =
        "INS_Opleidingscode_actueel",
      "INS_Eerste_jaar_opleiding_en_instelling" =
        "INS_Eerste_jaar_opleiding_en_instelling"
    ), suffix = c("", ".y")) %>%
  select(-ends_with(".y")) %>%
  ## Correctie variabele omdat voor jaar 9 de variabele is bepaald nadat
  ## Cohorten_samenvatting is samengesteld.
  ## SUC_Diploma_aantal_jaar_cohorten
  mutate(
    SUC_Diploma_aantal_jaar_cohorten =
      if_else(is.na(SUC_Diploma_aantal_jaar_cohorten) &
        SUC_Diploma_binnen_9_jaar_cohorten == TRUE,
      9,
        SUC_Diploma_aantal_jaar_cohorten
      )
  ) %>%
  ## Correctie variabele omdat voor jaar 9 de variabele is bepaald nadat
  ## Cohorten_samenvatting is samengesteld.
  mutate(
    SUC_Diploma_herinschrijvers_aantal_jaar_cohorten =
      if_else(is.na(SUC_Diploma_herinschrijvers_aantal_jaar_cohorten) &
        SUC_Diploma_herinschrijvers_binnen_9_jaar_cohorten == TRUE,
      9,
        SUC_Diploma_herinschrijvers_aantal_jaar_cohorten
      )
  ) %>%
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
  "INS_Opleidingscode_actueel"
))
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2B. BEWERKEN: SUCCESVARIABELEN OA TBV TABLEAU, NOMINALE STUDIEDUUR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Bepaal nieuwe succesvariabelen tbv tableau, mbv nominale studieduur:
## - SUC_Diploma_cohorten (mogelijke waarden: TRUE, FALSE of NA)
## - SUC_Diploma_herinschrijvers_binnen_1_tm_9_jaar_cohorten (tbv Tableau)
## - SUC_Uitval_na_jaar_1_tm_8_cohorten (tbv Tableau)
## - SUC_Diploma_nominaal_cohorten
## - SUC_Diploma_nominaal_plus1_cohorten
## - SUC_Diploma_nominaal_plus2_cohorten
## - SUC_Diploma_nominaal_plus3_cohorten
## - SUC_Diploma_nominaal_plus4_cohorten
## - SUC_Diploma_nominaal_plus5_cohorten
## - SUC_Diploma_nominaal_plus1_tm_9_cohorten (tbv Tableau)
## - SUC_Doorstroom_van_bachelor_naar_master_vu_elders_cohorten

## Selecteer per studiejaar, per opleiding de nominale studieduur
CROHO_per_jaar <- CROHO_per_jaar %>%
  select(
    OPL_Opleidingsnaam_CROHO,
    INS_Opleidingscode_actueel,
    OPL_Nominale_studieduur,
    OPL_Academisch_jaar
  ) %>%
  ## Verwijder dubbele waarden (die onstaan door opleidingsvorm (voltijd en deeltijd))
  distinct()

## Koppel de nominale studeduur aan Cohorten
Cohorten <- Cohorten %>%
  mapping_translate(current = "INS_Opleidingscode_actueel", new = "INS_Opleidingsnaam_2002")

Cohorten <- Cohorten %>%
  left_join(CROHO_per_jaar,
            by = c(
              "INS_Eerste_jaar_opleiding_en_instelling" =
                "OPL_Academisch_jaar",
              "INS_Opleidingscode_actueel"
            )
  )


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## Controleer of er duplicaten zijn ontstaan
assert_no_duplicates_in_group(Cohorten, c(
  "INS_Studentnummer",
  "INS_Eerste_jaar_opleiding_en_instelling",
  "INS_Opleidingscode_actueel"
))
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

## Bepaal nieuwe succesvariabelen
Cohorten <- Cohorten %>%
  ## Bepaal nieuwe variabele SUC_Diploma_cohorten: geeft aan of diploma voor
  ## EOI-opleiding wordt behaald (mogelijke waarden: TRUE, FALSE of NA).
  mutate(
    SUC_Diploma_cohorten =
      case_when(!is.na(SUC_Diploma_aantal_jaar_cohorten) ~ TRUE)
  ) %>%
  ## Bepaal variabele SUC_Diploma_herinschrijvers_binnen_1_tm_9_jaar_cohorten:
  ## Variabele geeft, als herinschrijver diploma voor eoi-opleiding heeft
  ## behaald, in woorden aan na hoeveel jaar diploma is behaald. Tbv Tableau.
  mutate(
    SUC_Diploma_herinschrijvers_binnen_1_tm_9_jaar_cohorten =
      case_when(
        !is.na(SUC_Diploma_herinschrijvers_aantal_jaar_cohorten) ~
          paste("Diploma jaar",
            SUC_Diploma_herinschrijvers_aantal_jaar_cohorten,
            "herinschrijvers",
            sep = " "
          ),
        TRUE ~
          "(Nog) geen diploma herinschrijvers"
      )
  ) %>%
  ## Bepaling nieuwe variabele: SUC_Uitval_na_jaar_1_tm_8_cohorten. Variabele
  ## geeft, als er uitval bij opleiding is, in woorden aan na hoeveel jaar
  ## deze uitval plaatsvindt. Tbv Tableau.
  mutate(
    SUC_Uitval_na_jaar_1_tm_8_cohorten =
      case_when(
        !is.na(SUC_Uitval_aantal_jaar_cohorten) ~
          paste("Uitval jaar",
            SUC_Uitval_aantal_jaar_cohorten,
            sep = " "
          ),
        TRUE ~
          "Geen uitval"
      )
  ) %>%
  ## Bepaling nieuwe variabele: SUC_Diploma_nominaal_plus1_tm_9_cohorten.
  ## Tbv Tableau. Ten eerste bepaal verschil in jaren tussen aantal jaren nodig
  ## voor diploma en nominale studieduur.
  mutate(
    SUC_Diploma_verschil_nominaal_aantal_jaar =
      SUC_Diploma_aantal_jaar_cohorten - OPL_Nominale_studieduur
  ) %>%
  mutate(
    SUC_Diploma_nominaal_plus1_tm_9_cohorten =
      case_when(
        SUC_Diploma_verschil_nominaal_aantal_jaar == 0 ~ "Nominaal",
        SUC_Diploma_verschil_nominaal_aantal_jaar > 0 ~
          paste0("Nominaal plus ", SUC_Diploma_verschil_nominaal_aantal_jaar),
        is.na(SUC_Diploma_aantal_jaar_cohorten) ~
          "(Nog) geen diploma",
        # Alle andere gevallen, o.a. als OPL_Nominale_studieduur onbekend is.
        TRUE ~
          as.character(SUC_Diploma_aantal_jaar_cohorten)
      )
  ) %>%
  select(-SUC_Diploma_verschil_nominaal_aantal_jaar)

## Definieer de functie Bepaal_Nominaalplus waarmee 6 nieuwe variabelen voor
## SUC_Diploma in combinatie met nominale studieduur worden bepaald.
## Deze variabelen worden in oude rapporten gebruikt.
Bepaal_Nominaalplus <- function(df, plus) {
  ## Plus geeft aantal jaar boven nominaal weer (nominaal_plus).
  ## Plus heeft waarde tussen 0 en 5.
  df <- df %>%
    ## Bepaal som van Nominale studieduur en plus
    mutate(som = OPL_Nominale_studieduur + plus) %>%
    ## Bepaal nieuwe variabele: SUC_Diploma_nominaal_plus
    mutate(
      SUC_Diploma_nominaal_plus =
        case_when(
          som == 1 ~
            SUC_Diploma_binnen_1_jaar_cohorten,
          som == 2 ~
            SUC_Diploma_binnen_2_jaar_cohorten,
          som == 3 ~
            SUC_Diploma_binnen_3_jaar_cohorten,
          som == 4 ~
            SUC_Diploma_binnen_4_jaar_cohorten,
          som == 5 ~
            SUC_Diploma_binnen_5_jaar_cohorten,
          som == 6 ~
            SUC_Diploma_binnen_6_jaar_cohorten,
          som == 7 ~
            SUC_Diploma_binnen_7_jaar_cohorten,
          TRUE ~
            NA
        )
    ) %>%
    ## Hernoem SUC_Diploma_nominaal_plus: Voeg "plus" (of aantal jaar boven nominaal) toe
    rename_at(vars(SUC_Diploma_nominaal_plus), ~ paste0(., plus, "_cohorten")) %>%
    ## Verwijder som
    select(-som)
  ## Hernoemen werkt alleen indien plus=0
  # rename_at(vars(starts_with("SUC_Diploma_nominaal_plus0")),
  # ~ paste0("SUC_Diploma_nominaal_cohorten"))
}

## Gebruik de functie Bepaal_Nominaalplus voor de bepaling van 6 nieuwe variabelen
## SUC_Diploma_nominaal_cohorten, SUC_Diploma_nominaal_plus1 t/m 5 _cohorten.
## SUC_Diploma_nominaal_cohorten
Cohorten <- Bepaal_Nominaalplus(df = Cohorten, plus = 0) %>%
  dplyr::rename(
    SUC_Diploma_nominaal_cohorten =
      SUC_Diploma_nominaal_plus0_cohorten
  ) %>%
  ## SUC_Diploma_nominaal_plus1_cohorten
  Bepaal_Nominaalplus(plus = 1) %>%
  ## SUC_Diploma_nominaal_plus2_cohorten
  Bepaal_Nominaalplus(plus = 2) %>%
  ## SUC_Diploma_nominaal_plus3_cohorten
  Bepaal_Nominaalplus(plus = 3) %>%
  ## SUC_Diploma_nominaal_plus4_cohorten
  Bepaal_Nominaalplus(plus = 4) %>%
  ## SUC_Diploma_nominaal_plus5_cohorten
  Bepaal_Nominaalplus(plus = 5)

## Bepaal nieuwe succesvariabelen
Cohorten <- Cohorten %>%
  ## Bepaal waarde voor herinschrijving in jaar na behalen van EOI-diploma
  mutate(
    INS_Herinschrijving_na_eoi_diploma =
      case_when(
        SUC_Diploma_aantal_jaar_cohorten == 1 ~
          as.integer(INS_Herinschrijving_na_1_jaar),
        SUC_Diploma_aantal_jaar_cohorten == 2 ~
          as.integer(INS_Herinschrijving_na_2_jaar),
        SUC_Diploma_aantal_jaar_cohorten == 3 ~
          as.integer(INS_Herinschrijving_na_3_jaar),
        SUC_Diploma_aantal_jaar_cohorten == 4 ~
          as.integer(INS_Herinschrijving_na_4_jaar),
        SUC_Diploma_aantal_jaar_cohorten == 5 ~
          as.integer(INS_Herinschrijving_na_5_jaar),
        SUC_Diploma_aantal_jaar_cohorten == 6 ~
          as.integer(INS_Herinschrijving_na_6_jaar),
        SUC_Diploma_aantal_jaar_cohorten == 7 ~
          as.integer(INS_Herinschrijving_na_7_jaar),
        SUC_Diploma_aantal_jaar_cohorten == 8 ~
          as.integer(INS_Herinschrijving_na_8_jaar),
        TRUE ~
          NA_integer_
      )
  ) %>%
  ## Bepaal of EOI-opleiding een bachelor -of masteropleiding is
  mutate(
    INS_BaMa =
      if_else(
        INS_Opleidingscode_actueel < 59999,
        "B",
        "M"
      )
  ) %>%
  ## Als diploma voor Ba-EOI-opleiding behaald: bepaal of student doorstroomt
  ## naar master. Zo ja, aangegeven: binnen VU, naar andere universiteit.
  ## Cohorten-schattingen: waarden voor INS_Herinschrijving_na_eoi_diploma
  ## zijn alleen geupdate voor binnen VU (her: 2, 3)
  ## N.B. Let op dat alleen doorstroom in jaar volgend op behalen eoi-diploma
  ## wordt meegenomen.
  ## Hierbij is de betekenis van waarden voor INS_Herinschrijving (zie documentatie
  ## 1cHO-Cohortbestand, met beslisboom):
  ##      Ba (I) cohort	                     Ma (V) cohort
  ## 1	equi-opleiding	                     equi-opleiding
  ## 2	hogere fase binnen hoop/instelling	 gelijk fase binnen hoop/instelling
  ## 3	hogere fase binnen instelling	     gelijk fase binnen instelling
  ## 4	gelijk fase binnen hoop/instelling	 lagere fase binnen hoop/instelling
  ## 5	gelijk fase binnen instelling	     lagere fase binnen instelling
  ## 6	hogere fase binnen hoop/srtHO	     gelijk fase binnen hoop/srtHO
  ## 7	hogere fase binnen srtHO	         gelijk fase binnen srtHO
  ## 8	hogere fase HO (alleen HBO cohorten) komt niet voor
  ## 9	gelijk fase binnen hoop/srtHO	     lagere fase binnen hoop/srtHO
  ## 10	gelijk fase binnen srtHO	         lagere fase binnen srtHO
  ## 11	gelijk fase HO	                     lagere fase HO
  ## 12	geen inschrijvingen	                 geen inschrijvingen
  ## 13   geen gegevens voor schatting         geen gegevens voor schatting
  mutate(
    SUC_Doorstroom_van_bachelor_naar_master_vu_elders_cohorten =
      case_when(
        SUC_Diploma_cohorten &
          INS_BaMa == "B" &
          INS_Herinschrijving_na_eoi_diploma %in% c("2", "3") ~
          "Master VU",
        SUC_Diploma_cohorten &
          INS_BaMa == "B" &
          INS_Herinschrijving_na_eoi_diploma %in% c("6", "7") ~
          "Master Universiteit NL",
        SUC_Diploma_cohorten &
          INS_BaMa == "B" &
          INS_Herinschrijving_na_eoi_diploma %in% c("8") ~
          "Master HBO",
        SUC_Diploma_cohorten &
          INS_BaMa == "B" &
          INS_Herinschrijving_na_eoi_diploma %in% c(
            "1", "4", "5", "8", "9", "10", "11", "12", "13"
          ) ~
          "Overig"
      )
  ) %>%
  ## Verwijder variabelen die niet meer nodig zijn
  select(
    -OPL_Nominale_studieduur,
    -INS_Herinschrijving_na_eoi_diploma
  )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2C. BEWERKEN: SUCCESVARIABELEN MBV DIPLOMA (ON)GELIJKE FASE ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## Bepaal succesvariabelen mbt diploma in gelijke en ongelijke fase:
## - SUC_Diploma_ho_gelijke_fase_cohorten
## - SUC_Diploma_ho_ongelijke_fase_cohorten
## - SUC_Diploma_typeho_gelijke_fase_cohorten
## - SUC_Diploma_typeho_ongelijke_fase_cohorten
## - SUC_Diploma_ho_gelijke_fase_aantal_jaar_cohorten
## - SUC_Diploma_ho_ongelijke_fase_aantal_jaar_cohorten
## - SUC_Diplomajaar_gelijke_fase_cohorten
## - SUC_Diplomajaar_ongelijke_fase_cohorten

## Uitleg:
## Gelijke fase, voorbeeld: inschrijving bachelor, diploma bachelor
## Ongelijke fase, voorbeeld: inschrijving bachelor, diploma master

## Bepaal nieuwe succesvariabelen: Diploma(jaar) gelijke en ongelijke fase in HO.
## Hierbij is de betekenis van waarden voor INS_Examen (zie documentatie
## 1cHO-Cohortbestand, met beslisboom):
##      Ba (I) cohort	                    Ma (V) cohort
## a	Equi-opl nieuwe stelsel (Ba)	    equi-opl nieuwe stelsel (Ma)
## b	Equi-opl oude stelsel (Doc)	        equi-opl oude stelsel (Pdoc)
## c	hoop/instelling (Ba)	            hoop/instelling (Ma)
## d	hoop/instelling (Doc)	            hoop/instelling (PDoc)
## e	instelling (Ba)	                    instelling (Ma)
## f	instelling (Doc)	                instelling (PDoc)
## g	hoop/srt_HO (Ba)	                hoop/srt_HO (Ma)
## h	hoop/srt_HO (Doc)	                hoop/srt_HO (Pdoc)
## i	Srt_HO (Ba)	                        srt_HO (Ma)
## j	Srt_HO (Doc)	                    srt_HO (Pdoc)
## k	niet srt_HO	                        niet srt_HO
## l	master behaald zelfde instelling
## p	master behaald andere instelling
Cohorten <- Cohorten %>%
  ## Maak nieuwe variabele: SUC_Diploma_ho_gelijke_fase_cohorten
  mutate(
    SUC_Diploma_ho_gelijke_fase_cohorten =
      case_when(
        INS_BaMa == "B" &
          INS_Examen_gelijke_fase %in% c("a", "b") ~
          "VU - Bachelordiploma - EOI-opleiding",
        INS_BaMa == "B" &
          INS_Examen_gelijke_fase %in% c("c", "d") ~
          "VU - Bachelordiploma - Andere opleiding - Zelfde HOOP-gebied",
        INS_BaMa == "B" &
          INS_Examen_gelijke_fase %in% c("e", "f") ~
          "VU - Bachelordiploma - Andere opleiding - Ander HOOP-gebied",
        INS_BaMa == "B" &
          INS_Examen_gelijke_fase %in% c("g", "h") ~
          "Universiteit NL - Bachelordiploma - Zelfde HOOP-gebied",
        INS_BaMa == "B" &
          INS_Examen_gelijke_fase %in% c("i", "j") ~
          "Universiteit NL - Bachelordiploma - Ander HOOP-gebied",
        INS_BaMa == "B" &
          INS_Examen_gelijke_fase %in% c("l") ~
          "VU - Masterdiploma - Geen bachelordiploma",
        INS_BaMa == "B" &
          INS_Examen_gelijke_fase %in% c("p") ~
          "Universiteit NL - Masterdiploma - Geen bachelordiploma",
        INS_BaMa == "M" &
          INS_Examen_gelijke_fase %in% c("a", "b") ~
          "VU - Masterdiploma - EOI-opleiding",
        INS_BaMa == "M" &
          INS_Examen_gelijke_fase %in% c("c", "d") ~
          "VU - Masterdiploma - Andere opleiding - Zelfde HOOP-gebied",
        INS_BaMa == "M" &
          INS_Examen_gelijke_fase %in% c("e", "f") ~
          "VU - Masterdiploma - Andere opleiding - Ander HOOP-gebied",
        INS_BaMa == "M" &
          INS_Examen_gelijke_fase %in% c("g", "h") ~
          "Universiteit NL - Masterdiploma - Zelfde HOOP-gebied",
        INS_BaMa == "M" &
          INS_Examen_gelijke_fase %in% c("i", "j") ~
          "Universiteit NL - Masterdiploma - Ander HOOP-gebied",
        INS_Examen_gelijke_fase %in% c("k") ~
          "HBO - Diploma",
        INS_Examen_gelijke_fase %in% c("x") ~
          "Geen diploma"
      )
  ) %>%
  ## Maak nieuwe variabele: SUC_Diploma_ho_ongelijke_fase_cohorten
  mutate(
    SUC_Diploma_ho_ongelijke_fase_cohorten =
      case_when(
        INS_BaMa == "B" &
          INS_Examen_ongelijke_fase %in% c("a", "b") ~
          "VU - Masterdiploma - EOI-opleiding",
        INS_BaMa == "B" &
          INS_Examen_ongelijke_fase %in% c("c", "d") ~
          "VU - Masterdiploma - Andere opleiding - Zelfde HOOP-gebied",
        INS_BaMa == "B" &
          INS_Examen_ongelijke_fase %in% c("e", "f") ~
          "VU - Masterdiploma - Andere opleiding - Ander HOOP-gebied",
        INS_BaMa == "B" &
          INS_Examen_ongelijke_fase %in% c("g", "h") ~
          "Universiteit NL - Masterdiploma - Zelfde HOOP-gebied",
        INS_Examen_ongelijke_fase %in% c("i", "j") ~
          "Universiteit NL - Masterdiploma - Ander HOOP-gebied",
        INS_BaMa == "B" &
          INS_Examen_gelijke_fase %in% c("l") ~
          "VU - Masterdiploma - Geen bachelordiploma",
        INS_BaMa == "B" &
          INS_Examen_gelijke_fase %in% c("p") ~
          "Universiteit NL - Masterdiploma - Geen bachelordiploma",
        INS_BaMa == "M" &
          INS_Examen_ongelijke_fase %in% c("a", "b") ~
          "VU - Bachelordiploma - EOI-opleiding",
        INS_BaMa == "M" &
          INS_Examen_ongelijke_fase %in% c("c", "d") ~
          "VU - Bachelordiploma - Andere opleiding - Zelfde HOOP-gebied",
        INS_BaMa == "M" &
          INS_Examen_ongelijke_fase %in% c("e", "f") ~
          "VU - Bachelordiploma - Andere opleiding - Ander HOOP-gebied",
        INS_BaMa == "M" &
          INS_Examen_ongelijke_fase %in% c("g", "h") ~
          "Universiteit NL - Bachelordiploma - Zelfde HOOP-gebied",
        INS_BaMa == "M" &
          INS_Examen_ongelijke_fase %in% c("i", "j") ~
          "Universiteit NL - Bachelordiploma - Ander HOOP-gebied",
        INS_Examen_ongelijke_fase %in% c("k") ~
          "HBO - Diploma",
        INS_Examen_ongelijke_fase %in% c("x") ~
          "Geen diploma"
      )
  ) %>%
  ## Maak nieuwe variabele: SUC_Diploma_typeho_gelijke_fase_cohorten
  mutate(
    SUC_Diploma_typeho_gelijke_fase_cohorten =
      case_when(
        INS_Examen_gelijke_fase %in% c("a", "b", "c", "d", "e", "f", "l") ~
          "Diploma VU",
        INS_Examen_gelijke_fase %in% c("g", "h", "i", "j", "p") ~
          "Diploma Universiteit NL",
        INS_Examen_gelijke_fase %in% c("k") ~
          "Diploma HBO",
        INS_Examen_gelijke_fase %in% c("x") ~
          "Geen diploma"
      )
  ) %>%
  ## Maak nieuwe variabele: SUC_Diploma_typeho_ongelijke_fase_cohorten
  mutate(
    SUC_Diploma_typeho_ongelijke_fase_cohorten =
      case_when(
        INS_Examen_ongelijke_fase %in% c("a", "b", "c", "d", "e", "f", "l") ~
          "Diploma VU",
        INS_Examen_ongelijke_fase %in% c("g", "h", "i", "j", "p") ~
          "Diploma Universiteit NL",
        INS_Examen_ongelijke_fase %in% c("k") ~
          "Diploma HBO",
        INS_Examen_ongelijke_fase %in% c("x") ~
          "Geen diploma"
      )
  ) %>%
  ## Maak nieuwe variabele: SUC_Diploma_ho_gelijke_fase_aantal_jaar_cohorten
  mutate(
    SUC_Diploma_ho_gelijke_fase_aantal_jaar_cohorten =
      INS_Jaar_examen_gelijke_fase
  ) %>%
  ## Maak nieuwe variabele: SUC_Diploma_ho_ongelijke_fase_aantal_jaar_cohorten
  mutate(
    SUC_Diploma_ho_ongelijke_fase_aantal_jaar_cohorten =
      INS_Jaar_examen_ongelijke_fase
  ) %>%
  ## Maak nieuwe variabele: SUC_Diplomajaar_gelijke_fase_cohorten
  mutate(
    SUC_Diplomajaar_gelijke_fase_cohorten =
      INS_Eerste_jaar_opleiding_en_instelling +
      SUC_Diploma_ho_gelijke_fase_aantal_jaar_cohorten
  ) %>%
  ## Maak nieuwe variabele: SUC_Diplomajaar_ongelijke_fase_cohorten
  mutate(
    SUC_Diplomajaar_ongelijke_fase_cohorten =
      INS_Eerste_jaar_opleiding_en_instelling +
      SUC_Diploma_ho_ongelijke_fase_aantal_jaar_cohorten
  ) %>%
  ## Verwijder INS_BaMa
  select(-INS_BaMa)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2D. BEWERKEN: INSCHRIJVINGSVARIABELEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Cohorten <- Cohorten %>%
  mapping_translate("INS_Soort_eerstejaars_code", "INS_Onderwijs_herkomst_cat")

Cohorten <- Cohorten %>%
  mapping_translate("INS_Soort_eerstejaars_code", "INS_Soort_eerstejaars_cat")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Controleer of er duplicaten zijn ontstaan
assert_no_duplicates_in_group(Cohorten, c(
  "INS_Studentnummer",
  "INS_Eerste_jaar_opleiding_en_instelling",
  "INS_Opleidingscode_actueel"
))

write_file_proj(Cohorten, "INS_Cohorten")

clear_script_objects()
