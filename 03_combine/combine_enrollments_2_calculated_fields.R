## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Npuls CEDA (Centre for Educational Data Analytics)
## Web Page: https://edu.nl/twt84
## Contact: corneel.denhartogh@surf.nl
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. READ ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

enrollments_start <- read_file_proj("enrollments_1",
                                    dir = "03_combined")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.MODIFY ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### 2.1 SUC_Type_uitstroom ####

enrollments <- enrollments_start %>%
  group_by(INS_Studentnummer,
           OPL_code_historisch) %>%
  mutate(
    ## Met CROHO is studieduur toegevoegd, obv hiervan kunnen we meer variabelen maken
    SUC_Type_uitstroom = case_when(
      INS_Uitval == TRUE ~ paste0("Uitval jaar ", INS_Aantal_inschrijvingen), !is.na(INS_Datum_tekening_diploma) &
        INS_Aantal_inschrijvingen_tot_diploma == OPL_Nominale_studieduur ~
        "Nominaal",
      !is.na(INS_Datum_tekening_diploma) &
        INS_Aantal_inschrijvingen_tot_diploma < OPL_Nominale_studieduur ~
        paste0(
          "Nominaal - ",
          OPL_Nominale_studieduur - INS_Aantal_inschrijvingen_tot_diploma
        ),
      !is.na(INS_Datum_tekening_diploma) &
        INS_Aantal_inschrijvingen_tot_diploma > OPL_Nominale_studieduur ~
        paste0(
          "Nominaal + ",
          INS_Aantal_inschrijvingen_tot_diploma - OPL_Nominale_studieduur
        ),
      .default = NA_character_),
    SUC_Type_uitstroom_studiejaar = case_when(
      is.na(SUC_Type_uitstroom) ~ "Nog studerend",
      INS_Aantal_inschrijvingen != INS_Studiejaar ~ "Nog studerend",
      is.na(INS_Datum_tekening_diploma) ~ "Uitval",
      .default = "Diploma"
    )) %>%
  ungroup()


## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.2 Switch binnen instelling ####
# TODO Improve code
#: SUC_Switch_binnen_instelling_aantal_jaar
# Per student en opleidingsfase checken hoeveel EOI inschrijvingen icm Type_uitstroom
Switch_binnen_instelling <- enrollments %>%
  group_by(INS_Studentnummer, OPL_Code_in_jaar) %>%
  mutate(INS_Max_Inschrijvingsjaar = max(INS_Inschrijvingsjaar)) %>%
  ungroup() %>%
  select(
    INS_Studentnummer,
    OPL_Code_in_jaar,
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
  filter(INS_Studentnummer %in% (enrollments %>% filter(str_detect(SUC_Type_uitstroom, "Uitval")) %>%
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
         OPL_Code_in_jaar,
         INS_Inschrijvingsjaar,
         SUC_Uitval_switch_studie,
         SUC_Uitval_switch_studiejaar,
         SUC_Uitval_switch_binnen_instelling_aantal_jaar,
         SUC_Instroom_switch_instelling
  ) %>%
  distinct()


enrollments <- enrollments %>%
  left_join(Switch_binnen_instelling, by = c(
    "INS_Studentnummer",
    "OPL_Code_in_jaar",
    "INS_Inschrijvingsjaar"
  ))

## Maak type uitstroom variabele aan die ook switch heeft
enrollments <- enrollments %>%
  mutate(
    SUC_Type_uitstroom_incl_switch = case_when(
      str_detect(SUC_Type_uitstroom, "Uitval") & SUC_Uitval_switch_studie == TRUE ~ str_replace(SUC_Type_uitstroom, "Uitval", "Switch binnen instelling na"),
      str_detect(SUC_Type_uitstroom, "Uitval") & SUC_Uitval_switch_studie == FALSE ~ str_replace(SUC_Type_uitstroom, "Uitval", "Uitval bij instelling"),
      .default = SUC_Type_uitstroom)
  )

enrollments <- enrollments %>%
  mutate(
    SUC_Type_uitstroom_studiejaar_incl_switch = case_when(
      str_detect(SUC_Type_uitstroom_studiejaar, "Uitval") & SUC_Uitval_switch_studiejaar == TRUE ~ str_replace(SUC_Type_uitstroom_studiejaar, "Uitval", "Switch binnen instelling"),
      str_detect(SUC_Type_uitstroom_studiejaar, "Uitval") & SUC_Uitval_switch_studiejaar == FALSE ~ str_replace(SUC_Type_uitstroom_studiejaar, "Uitval", "Uitval bij instelling"),
      .default = SUC_Type_uitstroom_studiejaar)
  )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(enrollments)

clear_script_objects()
