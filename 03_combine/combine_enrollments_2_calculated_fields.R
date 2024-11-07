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
#' *INFO* Make separate dataframe to improve speed

drop_out_student_numbers <- enrollments %>%
  filter(str_detect(SUC_Type_uitstroom, "Dropout")) %>%
  pull(INS_Studentnummer)

# Check per student and study phase how many EOI enrollments combined with Type_uitstroom
switch_within_institution <- enrollments %>%
  select(
    INS_Studentnummer,
    OPL_Code_in_jaar,
    INS_Inschrijvingsjaar,
    INS_Inschrijvingsjaar_EOI,
    INS_Opleidingsfase_actueel_naam,
    INS_Studiejaar,
    SUC_Type_uitstroom,
    SUC_Type_uitstroom_studiejaar,
    INS_Inschrijvingsjaar_max,
  ) %>%
  # Switch means there is dropout in one of the studies, so select
  # only the students who have experienced dropout
  filter(INS_Studentnummer %in% drop_out_student_numbers) %>%
  # Look at the rows within the same study phase per student (like B, or M)
  # because a switch takes place within a phase
  mutate(INS_Jaar_na_uitval = if_else(str_detect(SUC_Type_uitstroom, "Dropout"),
                                      INS_Inschrijvingsjaar_max + 1,
                                      NA_integer_)) %>%
  group_by(INS_Studentnummer, INS_Opleidingsfase_actueel_naam) %>%
  # Filter for multiple EOIs
  filter(n_distinct(INS_Inschrijvingsjaar_EOI) > 1) %>%
  # Calculate how many EOI enrollments a student has within the phase
  # Calculate switches
  mutate(
    # Basic switch: dropout year + 1 matches any EOI year
    SUC_Uitval_switch_studie = INS_Jaar_na_uitval %in% INS_Inschrijvingsjaar_EOI,
    # Switch in study year: same as above but must have dropout
    SUC_Uitval_switch_studiejaar = SUC_Uitval_switch_studie &
      str_detect(SUC_Type_uitstroom_studiejaar, "Dropout"),
    # Institution switch: EOI year matches any post-dropout year
    SUC_Instroom_switch_instelling = INS_Inschrijvingsjaar_EOI %in%
      (INS_Inschrijvingsjaar_max + 1),
    # Years between switches
    SUC_Uitval_switch_binnen_instelling_aantal_jaar = if_else(
      SUC_Uitval_switch_studie,
      INS_Jaar_na_uitval - INS_Inschrijvingsjaar_EOI,
      NA_integer_
    )
  ) %>%
  ungroup() %>%
  # Select final columns
  select(INS_Studentnummer,
         OPL_Code_in_jaar,
         INS_Inschrijvingsjaar,
         SUC_Uitval_switch_studie,
         SUC_Uitval_switch_studiejaar,
         SUC_Uitval_switch_binnen_instelling_aantal_jaar,
         SUC_Instroom_switch_instelling) %>%
  distinct()

enrollments <- enrollments %>%
  left_join(switch_within_institution,
            by = c(
              "INS_Studentnummer",
              "OPL_Code_in_jaar",
              "INS_Inschrijvingsjaar"
              )
            )

# Create type outflow variable that also includes switch
enrollments <- enrollments %>%
  mutate(
    SUC_Type_uitstroom_incl_switch = case_when(
      str_detect(SUC_Type_uitstroom, "Dropout") &
        SUC_Uitval_switch_studie == TRUE ~ str_replace(
          SUC_Type_uitstroom,
          "Dropout",
          "Switch within institution after"
        ),
      str_detect(SUC_Type_uitstroom, "Dropout") &
        SUC_Uitval_switch_studie == FALSE ~ str_replace(
          SUC_Type_uitstroom,
          "Dropout",
          "Dropout from institution"
        ),
      .default = SUC_Type_uitstroom
    )
  )

enrollments <- enrollments %>%
  mutate(
    SUC_Type_uitstroom_studiejaar_incl_switch = case_when(
      str_detect(SUC_Type_uitstroom_studiejaar, "Dropout") &
        SUC_Uitval_switch_studiejaar == TRUE ~ str_replace(
          SUC_Type_uitstroom_studiejaar,
          "Dropout",
          "Switch within institution"
        ),
      str_detect(SUC_Type_uitstroom_studiejaar, "Dropout") &
        SUC_Uitval_switch_studiejaar == FALSE ~ str_replace(
          SUC_Type_uitstroom_studiejaar,
          "Dropout",
          "Dropout from institution"
        ),
      .default = SUC_Type_uitstroom_studiejaar
    )
  )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE-AND-CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(enrollments)

clear_script_objects()
