
## TODO lastig dit, dit kan pas na combinen

## TODO versimpelen
## TODO
## Tijd tot diploma obv datum aantekening diploma ####

## Bepaal ook het eerste jaar
min_jaar <- min(Inschrijvingen$INS_Inschrijvingsjaar)

## Eerst wordt een tijdelijke dataset met de diplomadatum per
## student / opleiding gemaakt, omdat in de inschrijvingen de diplomadatum staat
## bij de inschrijving van het jaar waarin het diploma behaald wordt.
Diplomagegevens <- Inschrijvingen %>%
  ## Filter alle waarden waarbij de diplomadatum bekend is
  filter(!is.na(INS_Datum_diploma)) %>%
  select(
    INS_Studentnummer,
    INS_Opleidingsnaam_Z08,
    INS_Datum_diploma
  ) %>%
  distinct() %>%
  ## Ontdubbel de diplomadatum, en kies de hoogste waarde
  ## Groepeer op studentnummer en opleidingsnaam
  group_by(
    INS_Studentnummer,
    INS_Opleidingsnaam_Z08
  ) %>%
  ## Sorteer de datum van eerder tot meest recent
  arrange(desc(INS_Datum_diploma)) %>%
  ## Kies de hoogste waarde
  slice(1)

## Deze datum wordt weer terug geschreven naar de inschrijvingen set.
## "INS_Datum_diploma" krijgt hierdoor een ruimere, logischere betekenis waarbij
## de datum diploma niet alleen bij de laatste inschrijving staat.
## INS_Datum_origineel doet dit wel
Inschrijvingen <- Inschrijvingen %>%
  ## Hernoem de originele INS_Datum_diploma
  ## naar INS_Datum_diploma_origineel
  rename(INS_Datum_diploma_origineel = INS_Datum_diploma) %>%
  mutate(
    INS_Datum_diploma_origineel = ifelse(
      is.na(INS_Datum_diploma_origineel),
      NA,
      ifelse(
        academic_year(INS_Datum_diploma_origineel) == INS_Inschrijvingsjaar,
        INS_Datum_diploma_origineel,
        NA
      )
    ),
    INS_Datum_diploma_origineel = as.Date(INS_Datum_diploma_origineel, origin = "1970-01-01")
  ) %>%
  strict_left_join(Diplomagegevens,
                   by = c(
                     "INS_Studentnummer",
                     "INS_Opleidingsnaam_Z08"
                   )
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Omdat bij diploma"s het relevant is na hoeveel tijd een student dit
## behaalt, wordt een variabele "tijd tot diploma" uitgerekend.
## Als de datum diploma voor de datum inschrijving ligt, geef dan
## INS_Tijd_tot_diploma_vanaf_huidige_inschrijving de waarde 0; bereken
## anders de tijd tot diploma in jaren (dagen / 356)

Inschrijvingen <- Inschrijvingen %>%
  mutate(
    INS_Tijd_tot_diploma_vanaf_huidige_inschrijving =
      if_else(INS_Datum_diploma <= INS_Datum_inschrijving, 0,
              as.numeric(
                (INS_Datum_diploma - INS_Datum_inschrijving) / 365
              )
      )
  )


## TODO Versimpelen

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### INS_Eerste_datum_inschrijving & INS_Tijd_tot_diploma
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Omdat diplomarendement vaak, ook voor ouderejaars, beredeneerd wordt vanaf
## de eerste inschrijving aan de opleiding wordt deze datum aan elke
## inschrijving toegevoegd.
Inschrijvingen <- Inschrijvingen %>%
  group_by(
    INS_Studentnummer,
    INS_Opleidingsnaam_Z08
  ) %>%
  mutate(INS_Eerste_datum_inschrijving = min(
    INS_Datum_inschrijving,
    na.rm = TRUE
  )) %>%
  ungroup() %>%
  ## Bepaal de maand van de eerste inschrijving. Deze variabele kan gebruikt
  ## worden om onderscheid te maken tussen februari en september instroom.
  mutate(
    INS_Maand_eerste_inschrijving = month(INS_Eerste_datum_inschrijving),
    ## Februari en september-instroom worden bepaald op basis van de eerste
    ## van de maand
    INS_September_februari_instroom = case_when(
      INS_Maand_eerste_inschrijving %in% c(1, 9:12) ~ "September",
      INS_Maand_eerste_inschrijving %in% c(2:8) ~ "Februari"
    )
  ) %>%
  ## Vanuit de INS_Eerste_datum_inschrijving wordt een meer algemene variabele
  ## "Tijd tot diploma" uitgerekend.
  mutate(
    INS_Tijd_tot_diploma =
      if_else(INS_Datum_diploma <= INS_Datum_inschrijving, 0,
              as.numeric(
                (INS_Datum_diploma - INS_Eerste_datum_inschrijving) /
                  365
              )
      )
  ) %>%
  ## De tijd tot het laatst beschikbare diploma wordt ook uitgerekend.
  ## Dit gebeurt op een variabele manier zodat dit eenvoudig te updaten is
  ## in de toekomst, en wordt later gebruikt om te bepalen of het al mogelijk
  ## was om in "x jaar" een diploma te halen.
  mutate(
    INS_Tijd_tot_laatste_diploma =
      if_else(
        INS_Datum_diploma <= INS_Datum_inschrijving, 0,
        as.numeric(
          (max(
            INS_Datum_diploma,
            na.rm = TRUE
          ) - INS_Eerste_datum_inschrijving) /
            365
        )
      )
  )


## Uitstroom ####
#'*INFO*
SUC_Diploma_nominaal_plus1_tm_9_cohorten
SUC_Uitval_na_jaar_1_tm_8_cohorten
## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Bepaal type uitstroom, door de uitval en diploma variabelen samen te voegen.
Inschrijvingen <- Inschrijvingen %>%
  mutate(
    SUC_Type_uitstroom =
      case_when(
        !is.na(
          SUC_Diploma_nominaal_plus_aantal_jaar_omschrijving
        ) ~
          SUC_Diploma_nominaal_plus_aantal_jaar_omschrijving,
        !is.na(
          SUC_Uitval_aantal_jaar_omschrijving
        ) ~
          SUC_Uitval_aantal_jaar_omschrijving
      )
  )



## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Bepaal variabele: INS_Onechte_herinschrijving - iemand die het eerste jaar
## overdoet
## Definitie: EOI, die zich voor 1 feb uitschrijft en het jaar erop weer
## inschrijft voor dezelfde studie. Dit wordt gedaan om de BSA te vermijden.
##
## Voor het aanmaken van deze variabelen zijn ook INS_Uitschrijving_voor_1_feb
## en INS_EOI_uitschrijving_voor_1_feb benodigd.


## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## SUC_Type_uistroom_studiejaar wordt aangemaakt na INS_Herinschrijving_jaar_2_na_uitschrijving_voor_1_feb_in_jaar_1


## Uitstroom per studiejaar ####

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Maak variabel SUC_Type_uitstroom_studiejaar aan
Inschrijvingen <- Inschrijvingen %>%
  Wrapper_bepaal_uitstroom_studiejaar() %>%
  select(-OPL_Studielast_nominaal)

#'*INFO* wrapper_determine_outflow_study_year()

#'*INFO* kan niet in cohorten natuurlijk
mutate(SUC_Type_uitstroom_studiejaar = purrr::pmap_chr(
  list(
    SUC_Type_uitstroom,
    INS_Studiejaar,
    OPL_Studielast_nominaal
  ),
  determine_outflow_study_year
))



## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Switch binnen VU: SUC_Switch_binnen_vu_aantal_jaar
# Per student en opleidingsfase checken hoeveel EOI inschrijvingen icm Type_uitstroom
Switch_binnen_vu <- Inschrijvingen %>%
  select(
    INS_Studentnummer,
    INS_Opleidingscode_Z08,
    INS_Opleidingsnaam_Z08,
    INS_Inschrijvingsjaar,
    INS_Inschrijvingsjaar_EOI,
    INS_Opleidingsfase_actueel,
    INS_Studiejaar,
    INS_Diplomajaar,
    SUC_Diploma_aantal_jaar,
    SUC_Uitval_aantal_jaar,
    SUC_Type_uitstroom,
    SUC_Type_uitstroom_studiejaar,
    SUC_Diploma_behaald,
    INS_Max_Inschrijvingsjaar
  ) %>%
  ## Soms komen er zeer oude inschrijvingsjaren EOI omhoog. Over deze cohorten rapporteren we niet
  filter(INS_Inschrijvingsjaar_EOI > 2005) %>%
  ## Switch houdt in dat er uitval is bij een van de studies, dus selecteer
  ## alleen de studenten die met uitval te maken hebben
  filter(INS_Studentnummer %in% (Inschrijvingen %>% filter(str_detect(SUC_Type_uitstroom, "Uitval")) %>%
                                   pull(INS_Studentnummer))) %>%
  ## Kijk per student naar de rijen binnen de zelfde opleidingsfase (zoals B, of M)
  ## want een switch vind zich plaats binnen een fase
  mutate(INS_Jaar_na_uitval = if_else(str_detect(SUC_Type_uitstroom, "Uitval"),
                                      INS_Max_Inschrijvingsjaar + 1,
                                      NA_integer_)) %>%
  group_by(INS_Studentnummer, INS_Opleidingsfase_actueel) %>%
  ## Bereken hoeveel EOI inschrijvingen een student heeft binnen de fase
  mutate(Unieke_EOI_jaren = n_distinct(INS_Inschrijvingsjaar_EOI)) %>%
  ## We zijn geinteresseerd in studenten met meerdere EOIs per fase
  filter(Unieke_EOI_jaren > 1) %>%
  mutate(list_EOI = list(unique(INS_Inschrijvingsjaar_EOI)),
         list_jaar_na_uitval = list(unique(INS_Jaar_na_uitval))) %>%
  arrange(INS_Inschrijvingsjaar_EOI, INS_Inschrijvingsjaar) %>%
  ## EOI jaar nieuwe inschrijving-EOI eerste inschrijving
  mutate(Rank_EOI = min_rank(INS_Inschrijvingsjaar_EOI)) %>%
  ungroup() %>%
  ##' *INFO* Bepaling student switch is als volgt:
  ## Als student uitvalt en gedurende een opleiding en direct het jaar erna bij een andere opleiding
  ## in dezelfde fase start.
  mutate(
    SUC_Uitval_switch_studie = map2_lgl(INS_Jaar_na_uitval, list_EOI, ~.x %in% .y),
    SUC_Uitval_switch_studiejaar = pmap_lgl(list(INS_Jaar_na_uitval, list_EOI, SUC_Type_uitstroom_studiejaar), ~str_detect(..3, "Uitval") & ..1 %in% ..2),
    SUC_Instroom_switch_VU = map2_lgl(INS_Inschrijvingsjaar_EOI, list_jaar_na_uitval, ~.x %in% .y)
  ) %>%
  ## Bereken aantal jaren tussen de start met eerste studie (eerste EOI) en de start
  ## met de tweede studie (tweede EOI)
  mutate(
    SUC_Uitval_switch_binnen_VU_aantal_jaar = if_else(
      SUC_Uitval_switch_studie == TRUE,
      INS_Jaar_na_uitval - INS_Inschrijvingsjaar_EOI,
      #(INS_Inschrijvingsjaar_EOI[first(which(Rank_EOI > 1))] - INS_Inschrijvingsjaar_EOI[first(which(Rank_EOI == 1))]),
      NA_integer_)
  ) %>%
  select(INS_Studentnummer,
         INS_Opleidingscode_Z08,
         INS_Inschrijvingsjaar,
         SUC_Uitval_switch_studie,
         SUC_Uitval_switch_studiejaar,
         SUC_Uitval_switch_binnen_VU_aantal_jaar,
         SUC_Instroom_switch_VU
  )


Inschrijvingen <- Inschrijvingen %>%
  left_join(Switch_binnen_vu, by = c(
    "INS_Studentnummer",
    "INS_Opleidingscode_Z08",
    "INS_Inschrijvingsjaar"
  ))

## Maak type uitstroom variabele aan die ook switch heeft
Inschrijvingen <- Inschrijvingen %>% mutate(
  SUC_Type_uitstroom_incl_switch = case_when(
    str_detect(SUC_Type_uitstroom, "Uitval") & SUC_Uitval_switch_studie == TRUE ~ str_replace(SUC_Type_uitstroom, "Uitval", "Switch binnen VU na"),
    str_detect(SUC_Type_uitstroom, "Uitval") & SUC_Uitval_switch_studie == FALSE ~ str_replace(SUC_Type_uitstroom, "Uitval", "Uitval van VU"),
    .default = SUC_Type_uitstroom)
)

Inschrijvingen <- Inschrijvingen %>% mutate(
  SUC_Type_uitstroom_studiejaar_incl_switch = case_when(
    str_detect(SUC_Type_uitstroom_studiejaar, "Uitval") & SUC_Uitval_switch_studiejaar == TRUE ~ str_replace(SUC_Type_uitstroom_studiejaar, "Uitval", "Switch binnen VU"),
    str_detect(SUC_Type_uitstroom_studiejaar, "Uitval") & SUC_Uitval_switch_studiejaar == FALSE ~ str_replace(SUC_Type_uitstroom_studiejaar, "Uitval", "Uitval op VU"),
    .default = SUC_Type_uitstroom_studiejaar)
)


## _________________________________________________________________________________________________
## Aansluiting ####

## Aansluiting is een gecombineerde variabele waarin de voorgeschiedenis
## van de student wordt bepaald.
Inschrijvingen <- Inschrijvingen %>%
  mutate(
    ## Aansluiting bepalen obv direct en tussenjaar. Verschil tussen
    ## switch intern en extern wordt later bepaald
    INS_Aansluiting =
      case_when(
        INS_Direct &
          INS_Hoogste_vooropleiding_BRIN_1CHO == "21PL" ~
          "Direct na diploma VU",
        INS_Direct ~ "Direct na diploma extern",
        INS_Tussenjaar_voor_B == TRUE &
          INS_Opleidingsfase_actueel == "B" ~ "Tussenjaar",
        INS_Tussenjaar_voor_P == TRUE &
          INS_Opleidingsfase_actueel == "P" ~ "Tussenjaar",
        INS_Tussenjaar_voor_M == TRUE &
          INS_Opleidingsfase_actueel == "M" ~ "Tussenjaar",
        SUC_Instroom_switch_VU == TRUE ~ "Switch binnen VU",
        INS_Verblijfsjaar_type_onderwijs_binnen_HO > INS_Studiejaar ~ "Switch extern / Tweede studie",
        TRUE ~ "Onbekend"
      )
  )

