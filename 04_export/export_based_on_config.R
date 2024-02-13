
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

INS_Inschrijvingen <- read_file_proj("INS_Inschrijvingen_combined")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##'*INFO* Adapt based on config
if (config::get("filter_only_own_students") == TRUE) {
  INS_Inschrijvingen <- INS_Inschrijvingen %>%
    filter(INS_Instelling == config::get("metadata_institution_name"))
}

if (config::get("filter_only_gov_funded_programmes") == TRUE) {
  INS_Inschrijvingen <- INS_Inschrijvingen %>%
    filter(OPL_Code_in_jaar < 70000)
}


if (config::get("fix_duplicated_enrollments") == TRUE) {

  INS_Inschrijvingen_duplicated <-
    INS_Inschrijvingen[duplicated(INS_Inschrijvingen[, c(
      "INS_Studentnummer",
      "INS_Inschrijvingsjaar",
      "OPL_Code_in_jaar"
    )]), ]

  INS_Inschrijvingen_duplicated <- INS_Inschrijvingen_duplicated %>%
    arrange(INS_Studentnummer, INS_Inschrijvingsjaar)

  INS_Inschrijvingen_duplicated_reverse <-
    INS_Inschrijvingen[duplicated(INS_Inschrijvingen[, c(
      "INS_Studentnummer",
      "INS_Inschrijvingsjaar",
      "OPL_Code_in_jaar",
      "INS_Instelling"
    )], fromLast = TRUE), ]

  INS_Inschrijvingen_duplicated_reverse <- INS_Inschrijvingen_duplicated_reverse %>%
    arrange(INS_Studentnummer, INS_Inschrijvingsjaar)

  df_Inschrijvingen_duplicated <- bind_rows(INS_Inschrijvingen_duplicated, INS_Inschrijvingen_duplicated_reverse) %>%
    arrange(INS_Inschrijvingsjaar, OPL_Code_in_jaar, INS_Studentnummer)

  ## De duplicates hieronder hebben verschillen in oa de volgende variabelen:
  ## - Datum van in- en uitschrijving
  ##    - We zijn geinteresseerd in Indicatie actief op 1 okt = 1 (inschrijving)
  ##      Indien in de duplicates en 2 (uitgeschreven voor peildatum) en 3 (ingeschreven na peildatum)
  ##      voorkomen, willen we de rij met waarde 2 behouden en deze waarde vervangen in een 1 (inschrijving).
  ## - Soort inschrijving Ho: keuze voor de minimale code
  ## - Actief of peildatum : keuze voor de minimale code
  ## - Code beeindiging : keuze voor de minimale code

  df_Inschrijvingen_duplicated <- df_Inschrijvingen_duplicated %>%
    group_by(INS_Studentnummer, OPL_Code_in_jaar, INS_Inschrijvingsjaar) %>%
    arrange(INS_Indicatie_actief_op_peildatum) %>%
    ## Neem voor de duplicate rijen de eerste inschrijvings- en de laatste uitschrijvingsdatum
    mutate(
      INS_Datum_inschrijving = min(INS_Datum_inschrijving),
      INS_Datum_uitschrijving = max(INS_Datum_uitschrijving)
    ) %>%
    ## Baseer maand inschrijving van en tot op datum inscchrijving en uitschrijving
    mutate(
      INS_Maand_inschrijving_van = month(INS_Datum_inschrijving),
      ## Als datum uitschrijving gelijk is aan augustus, dan hier NA (niet actief uitgeschreven)
      ## Anders de datum van de uitschrijving nemen + 1 (want uitschrijving is laatste dag vd maand)
      INS_Maand_inschrijving_tot = if_else(month(INS_Datum_uitschrijving) != 08, month(INS_Datum_uitschrijving) + 1, NA_real_)
    )%>%
    ## Neem laagste code van beeindiging mee
    mutate(
      INS_Code_beeindiging_inschrijving = min(INS_Code_beeindiging_inschrijving),
      INS_Soort_inschrijving_1CHO_code = min(INS_Soort_inschrijving_1CHO_code)
    ) %>%
    ## Alleen de bovenste waarde meenemen
    slice(1) %>%
    ungroup()

  ## Verwijder alle waarden die duplicates hebben uit inschrijvigen 1cho en voeg bovenstaande niet duplicate set toe
  INS_Inschrijvingen <- INS_Inschrijvingen %>%
    anti_join(df_Inschrijvingen_duplicated, by = c(
      "INS_Studentnummer",
      "INS_Inschrijvingsjaar",
      "OPL_Code_in_jaar"
    )) %>%
    bind_rows(df_Inschrijvingen_duplicated)

}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Set filename based on config
file_name <- as_label(expr(INS_Inschrijvingen))
file_name_suffix <- maptbl_config2suffix(config::get("data_manipulation_config"))
file_name <- paste0(file_name, file_name_suffix, "_", config::get("metadata_institution_name"))

write_file_proj_out(INS_Inschrijvingen, file_name)

clear_script_objects()
