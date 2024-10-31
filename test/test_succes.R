
test <- Inschrijvingen_1CHO_basis %>%
  #filter(INS_Studentnummer < 1000) %>%
  group_by(INS_Studentnummer,
           OPL_code_historisch) %>%
  mutate(
    INS_Eerste_datum_inschrijving = min(INS_Datum_inschrijving, na.rm = TRUE),
    INS_Tijd_tot_diploma =
      case_when(
        INS_Datum_tekening_diploma <= INS_Eerste_datum_inschrijving ~ 0,

        !is.na(INS_Datum_tekening_diploma) ~
          round(time_length(interval(INS_Eerste_datum_inschrijving, INS_Datum_tekening_diploma),
                      "months"), 0),
        .default = NA_real_
        )
  ) %>%
  ungroup() %>%
  select(INS_Eerste_datum_inschrijving, INS_Datum_tekening_diploma, INS_Tijd_tot_diploma, INS_Studentnummer, OPL_code_historisch)

test2 <- Inschrijvingen_1cho %>%
    #filter(INS_Studentnummer < 1000) %>%
    group_by(INS_Studentnummer,
             OPL_code_historisch) %>%
    mutate(INS_Eerste_datum_inschrijving = min(INS_Datum_inschrijving, na.rm = TRUE),
           INS_Laatste_datum_uitschrijving = max(INS_Datum_uitschrijving, na.rm = TRUE),
           INS_Duur_inschrijving_in_maanden = round(time_length(interval(INS_Eerste_datum_inschrijving, INS_Datum_tekening_diploma),
                                                                "months"), 0),
           INS_Duur_inschrijving_in_jaar = ceiling(time_length(interval(INS_Eerste_datum_inschrijving, INS_Laatste_datum_uitschrijving),
                                                             "years")),
           INS_Tijd_tot_diploma_in_maanden =
             case_when(
               INS_Datum_tekening_diploma <= INS_Eerste_datum_inschrijving ~ 0,
               !is.na(INS_Datum_tekening_diploma) ~
                 round(time_length(interval(INS_Eerste_datum_inschrijving, INS_Datum_tekening_diploma),
                                   "months"), 0),
               .default = NA_real_
             ),
           INS_Tijd_tot_diploma_in_jaar = ceiling(time_length(interval(INS_Eerste_datum_inschrijving, INS_Datum_tekening_diploma),
                                                             "years")),
           INS_Inschrijvingsjaar_max = max(INS_Inschrijvingsjaar),
           INS_Actief_in_max_jaar = INS_Inschrijvingsjaar_max == config::get("year"),
           INS_Uitval = if_else(
             is.na(INS_Datum_tekening_diploma) & !INS_Actief_in_max_jaar,
             TRUE,
             FALSE
           ),
           SUC_Type_Uitstroom = case_when(
            INS_Uitval == TRUE ~ paste0("Uitval jaar ", INS_Duur_inschrijving_in_jaar),
            !is.na(INS_Datum_tekening_diploma) & INS_Tijd_tot_diploma_in_jaar == OPL_Nominale_studieduur ~
            "Diploma nominaal",
            !is.na(INS_Datum_tekening_diploma) & INS_Tijd_tot_diploma_in_jaar < OPL_Nominale_studieduur ~
              paste0("Diploma nominaal - ", (OPL_Nominale_studieduur - INS_Tijd_tot_diploma_in_jaar)),
            !is.na(INS_Datum_tekening_diploma) & INS_Tijd_tot_diploma_in_jaar > OPL_Nominale_studieduur ~
              paste0("Diploma nominaal + ", (INS_Tijd_tot_diploma_in_jaar - OPL_Nominale_studieduur)),
            .default = NA_character_
           )
    ) %>%
  ungroup()

## OPL_Nominale studieduur soms leeg

test3 <- tabyl(Inschrijvingen_1cho$OPL_Nominale_studieduur)
