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
    # With CROHO study duration has been added, based on this we can create more variables
    SUC_Type_uitstroom = case_when(
      INS_Uitval == TRUE ~ paste0("Dropout year ", INS_Aantal_inschrijvingen), !is.na(INS_Datum_tekening_diploma) &
        INS_Aantal_inschrijvingen_tot_diploma == OPL_Nominale_studieduur ~
        "Nominal",
      !is.na(INS_Datum_tekening_diploma) &
        INS_Aantal_inschrijvingen_tot_diploma < OPL_Nominale_studieduur ~
        paste0(
          "Nominal - ",
          OPL_Nominale_studieduur - INS_Aantal_inschrijvingen_tot_diploma
        ),
      !is.na(INS_Datum_tekening_diploma) &
        INS_Aantal_inschrijvingen_tot_diploma > OPL_Nominale_studieduur ~
        paste0(
          "Nominal + ",
          INS_Aantal_inschrijvingen_tot_diploma - OPL_Nominale_studieduur
        ),
      .default = NA_character_),
    SUC_Type_uitstroom_studiejaar = case_when(
      is.na(SUC_Type_uitstroom) ~ "Still studying",
      INS_Aantal_inschrijvingen != INS_Studiejaar ~ "Still studying",
      is.na(INS_Datum_tekening_diploma) ~ "Dropout",
      .default = "Diploma"
    )) %>%
  ungroup()


## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.2 Switch within institution ####
# TODO Improve code
#: SUC_Switch_binnen_instelling_aantal_jaar
# Check per student and study phase how many EOI enrollments combined with Type_uitstroom
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
  # Switch means there is dropout in one of the studies, so select
  # only the students who have experienced dropout
  filter(INS_Studentnummer %in% (enrollments %>% filter(str_detect(SUC_Type_uitstroom, "Dropout")) %>%
                                   pull(INS_Studentnummer))) %>%
  # Look at the rows within the same study phase per student (like B, or M)
  # because a switch takes place within a phase
  mutate(INS_Jaar_na_uitval = if_else(str_detect(SUC_Type_uitstroom, "Dropout"),
                                      INS_Max_Inschrijvingsjaar + 1,
                                      NA_integer_)) %>%
  group_by(INS_Studentnummer, INS_Opleidingsfase_actueel_naam) %>%
  # Calculate how many EOI enrollments a student has within the phase
  mutate(Unieke_EOI_jaren = n_distinct(INS_Inschrijvingsjaar_EOI)) %>%
  # We are interested in students with multiple EOIs per phase
  filter(Unieke_EOI_jaren > 1) %>%
  mutate(list_EOI = list(unique(INS_Inschrijvingsjaar_EOI)),
         list_jaar_na_uitval = list(unique(INS_Jaar_na_uitval))) %>%
  arrange(INS_Inschrijvingsjaar_EOI, INS_Inschrijvingsjaar) %>%
  # EOI year new enrollment - EOI first enrollment
  mutate(Rank_EOI = min_rank(INS_Inschrijvingsjaar_EOI)) %>%
  ungroup() %>%
  #' *INFO* Student switch determination is as follows:
  # If student drops out during a program and starts directly the next year at another program
  # in the same phase.
  mutate(
    SUC_Uitval_switch_studie = map2_lgl(INS_Jaar_na_uitval, list_EOI, ~.x %in% .y),
    SUC_Uitval_switch_studiejaar = pmap_lgl(list(INS_Jaar_na_uitval, list_EOI, SUC_Type_uitstroom_studiejaar), ~str_detect(..3, "Dropout") & ..1 %in% ..2),
    SUC_Instroom_switch_instelling = map2_lgl(INS_Inschrijvingsjaar_EOI, list_jaar_na_uitval, ~.x %in% .y)
  ) %>%
  # Calculate number of years between the start of first study (first EOI) and the start
  # of the second study (second EOI)
  mutate(
    SUC_Uitval_switch_binnen_instelling_aantal_jaar = if_else(
      SUC_Uitval_switch_studie == TRUE,
      INS_Jaar_na_uitval - INS_Inschrijvingsjaar_EOI,
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

# Create type outflow variable that also includes switch
enrollments <- enrollments %>%
  mutate(
    SUC_Type_uitstroom_incl_switch = case_when(
      str_detect(SUC_Type_uitstroom, "Dropout") & SUC_Uitval_switch_studie == TRUE ~ str_replace(SUC_Type_uitstroom, "Dropout", "Switch within institution after"),
      str_detect(SUC_Type_uitstroom, "Dropout") & SUC_Uitval_switch_studie == FALSE ~ str_replace(SUC_Type_uitstroom, "Dropout", "Dropout from institution"),
      .default = SUC_Type_uitstroom)
  )

enrollments <- enrollments %>%
  mutate(
    SUC_Type_uitstroom_studiejaar_incl_switch = case_when(
      str_detect(SUC_Type_uitstroom_studiejaar, "Dropout") & SUC_Uitval_switch_studiejaar == TRUE ~ str_replace(SUC_Type_uitstroom_studiejaar, "Dropout", "Switch within institution"),
      str_detect(SUC_Type_uitstroom_studiejaar, "Dropout") & SUC_Uitval_switch_studiejaar == FALSE ~ str_replace(SUC_Type_uitstroom_studiejaar, "Dropout", "Dropout from institution"),
      .default = SUC_Type_uitstroom_studiejaar)
  )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(enrollments)

clear_script_objects()
