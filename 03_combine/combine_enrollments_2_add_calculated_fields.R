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

Inschrijvingen <- read_file_proj("INS_Inschrijvingen_1",
                                 dir = "03_combined")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.1 SUC_TYpe_uitstroom per studiejaar ####

determine_outflow_study_year <- function(SUC_Type_uitstroom, INS_Studiejaar, OPL_Nominale_studieduur) {

  ## Maak onderscheid tussen diploma en uitval
  SUC_Afgestudeerd <- case_when(
    startsWith(SUC_Type_uitstroom, "Nominaal") ~ TRUE,
    startsWith(SUC_Type_uitstroom, "Uitval") ~ FALSE,
    .default = NA_integer_
  )

  ## Haal het cijfer uit de SUC_Type_Uitstroom variabele
  ## VB: "Nominaal +1" wordt 1, "Uitval jaar 3" wordt 3
  SUC_Aantal_jaar <- dplyr::case_when(
    (is.na(SUC_Type_uitstroom) | SUC_Type_uitstroom == "Nog studerend") ~ NA_character_,
    SUC_Type_uitstroom == "Nominaal" ~ "0",
    .default = utils::tail(stringr::str_split(SUC_Type_uitstroom, " ")[[1]], n = 1)
    )

  ## Doe de variabele met nominaal + nominale studiejaar om zo het studiejaar te krijgen
  SUC_Uitstroom_jaar <-
    dplyr::case_when(
      SUC_Afgestudeerd == TRUE ~ as.double(SUC_Aantal_jaar) + OPL_Nominale_studieduur,
      SUC_Afgestudeerd == FALSE ~ as.double(SUC_Aantal_jaar),
      .default = NA_real_
    )
  ## Bepaal per studiejaar of dit ook uitstroomjaar was, indien dat niet
  ## het geval is, wordt dit één groep (Nog studerend)
  SUC_Type_uitstroom_studiejaar <-
    dplyr::case_when(
      is.na(SUC_Type_uitstroom) ~ NA_character_,
      is.na(SUC_Aantal_jaar) ~ "Nog studerend",
      SUC_Uitstroom_jaar != INS_Studiejaar ~ "Nog studerend",
      SUC_Afgestudeerd == TRUE ~ "Diploma",
      SUC_Afgestudeerd == FALSE ~ "Uitval"
    )

  return(SUC_Type_uitstroom_studiejaar)
}


#'*INFO* Code block duurt lang
Inschrijvingen <- Inschrijvingen %>%
  mutate(SUC_Type_uitstroom_studiejaar = purrr::pmap_chr(
    list(
      SUC_Type_uitstroom,
      INS_Studiejaar,
      OPL_Nominale_studieduur
    ),
    determine_outflow_study_year
))


## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.2 Switch binnen instelling ####
#: SUC_Switch_binnen_instelling_aantal_jaar
# Per student en opleidingsfase checken hoeveel EOI inschrijvingen icm Type_uitstroom
Switch_binnen_instelling <- Inschrijvingen %>%
  group_by(INS_Studentnummer, OPL_Code_actueel) %>%
  mutate(INS_Max_Inschrijvingsjaar = max(INS_Inschrijvingsjaar)) %>%
  ungroup() %>%
  select(
    INS_Studentnummer,
    OPL_Code_actueel,
    INS_Inschrijvingsjaar,
    INS_Inschrijvingsjaar_EOI,
    INS_Opleidingsfase_actueel_naam,
    INS_Studiejaar,
    SUC_Type_uitstroom,
    SUC_Type_uitstroom_studiejaar,
    INS_Max_Inschrijvingsjaar
  ) %>%
  ## Switch houdt in dat er uitval is bij een van de studies, dus selecteer
  ## alleen de studenten die met uitval te maken hebben
  filter(INS_Studentnummer %in% (Inschrijvingen %>% filter(str_detect(SUC_Type_uitstroom, "Uitval")) %>%
                                   pull(INS_Studentnummer))) %>%
  ## Kijk per student naar de rijen binnen de zelfde opleidingsfase (zoals B, of M)
  ## want een switch vind zich plaats binnen een fase
  mutate(INS_Jaar_na_uitval = if_else(str_detect(SUC_Type_uitstroom, "Uitval"),
                                      INS_Max_Inschrijvingsjaar + 1,
                                      NA_integer_)) %>%
  group_by(INS_Studentnummer, INS_Opleidingsfase_actueel_naam) %>%
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
    SUC_Instroom_switch_instelling = map2_lgl(INS_Inschrijvingsjaar_EOI, list_jaar_na_uitval, ~.x %in% .y)
  ) %>%
  ## Bereken aantal jaren tussen de start met eerste studie (eerste EOI) en de start
  ## met de tweede studie (tweede EOI)
  mutate(
    SUC_Uitval_switch_binnen_instelling_aantal_jaar = if_else(
      SUC_Uitval_switch_studie == TRUE,
      INS_Jaar_na_uitval - INS_Inschrijvingsjaar_EOI,
      #(INS_Inschrijvingsjaar_EOI[first(which(Rank_EOI > 1))] - INS_Inschrijvingsjaar_EOI[first(which(Rank_EOI == 1))]),
      NA_integer_)
  ) %>%
  select(INS_Studentnummer,
         OPL_Code_actueel,
         SUC_Type_uitstroom,
         INS_Inschrijvingsjaar,
         SUC_Uitval_switch_studie,
         SUC_Uitval_switch_studiejaar,
         SUC_Uitval_switch_binnen_instelling_aantal_jaar,
         SUC_Instroom_switch_instelling
  ) %>%
  distinct()


Inschrijvingen <- Inschrijvingen %>%
  left_join(Switch_binnen_instelling, by = c(
    "INS_Studentnummer",
    "OPL_Code_actueel",
    "SUC_Type_uitstroom",
    "INS_Inschrijvingsjaar"
  ))

## Maak type uitstroom variabele aan die ook switch heeft
Inschrijvingen <- Inschrijvingen %>%
  mutate(
    SUC_Type_uitstroom_incl_switch = case_when(
      str_detect(SUC_Type_uitstroom, "Uitval") & SUC_Uitval_switch_studie == TRUE ~ str_replace(SUC_Type_uitstroom, "Uitval", "Switch binnen instelling na"),
      str_detect(SUC_Type_uitstroom, "Uitval") & SUC_Uitval_switch_studie == FALSE ~ str_replace(SUC_Type_uitstroom, "Uitval", "Uitval bij instelling"),
      .default = SUC_Type_uitstroom)
  )

Inschrijvingen <- Inschrijvingen %>%
  mutate(
    SUC_Type_uitstroom_studiejaar_incl_switch = case_when(
      str_detect(SUC_Type_uitstroom_studiejaar, "Uitval") & SUC_Uitval_switch_studiejaar == TRUE ~ str_replace(SUC_Type_uitstroom_studiejaar, "Uitval", "Switch binnen instelling"),
      str_detect(SUC_Type_uitstroom_studiejaar, "Uitval") & SUC_Uitval_switch_studiejaar == FALSE ~ str_replace(SUC_Type_uitstroom_studiejaar, "Uitval", "Uitval bij instelling"),
      .default = SUC_Type_uitstroom_studiejaar)
  )


## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.3 Aansluiting Update met Switch binnen VU ####
Inschrijvingen <- Inschrijvingen %>%
  mutate(
    ## Aansluiting bepalen obv direct en tussenjaar. Verschil tussen
    ## switch intern en extern wordt later bepaald
    INS_Aansluiting =
      case_when(
        INS_Direct &
          INS_Hoogste_vooropleiding_BRIN_1CHO == config::get("metadata_institution_BRIN") ~
          "Direct na diploma VU",
        INS_Direct ~ "Direct na diploma extern",
        INS_Indicatie_Tussenjaar == TRUE ~ "Tussenjaar",
        #SUC_Instroom_switch_VU == TRUE ~ "Switch binnen VU",
        INS_Verblijfsjaar_type_onderwijs_binnen_HO > INS_Studiejaar ~ "Switch extern / Tweede studie",
        .default = "Onbekend"
      )
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Voor eerste keer runnen, maak een documentatie bestand aan
## vvmover::create_documentatie(Dataset, "Dataset")

write_file_proj(Inschrijvingen)

clear_script_objects()
