
Inschrijvingen$INS_Indicatie_actief_op_peildatum_status <- NULL
Inschrijvingen <- mapping_translate(
  Inschrijvingen,
  "INS_Indicatie_actief_op_peildatum",
  "INS_Indicatie_actief_op_peildatum_status",
  mapping_table_name = "Mapping_INS_Indicatie_INS_Inschrijvingen.csv"
)

Inschrijvingen <- mapping_translate(
  Inschrijvingen,
  "INS_Opleidingsfase_actueel",
  "INS_Opleidingsfase_actueel_naam"
)

Inschrijvingen <- mapping_translate(
  Inschrijvingen,
  "INS_Opleidingsvorm",
  "INS_Opleidingsvorm_naam"
)


## Missing values van de INS_Code_examenresultaat worden herschreven naar
## "Niet van toepassing"
Inschrijvingen <- Inschrijvingen %>%
  mutate(
    INS_Examenresultaat =
      if_else(is.na(INS_Code_examenresultaat),
              "Niet van toepassing",
              INS_Examenresultaat
      )
  )


Inschrijvingen <- mapping_category(Inschrijvingen,
                                   "DEM_Leeftijd_peildatum_1_oktober",
                                   "DEM_Leeftijd_peildatum_1_oktober_cat",
                                   mapping_table_name = "Mapping_DEM_Leeftijd_cat.csv"
)

Inschrijvingen <- mapping_translate(
  Inschrijvingen,
  "DEM_Geslacht",
  "DEM_Geslacht",
  mapping_table_name = "Mapping_Geslacht.csv",
  KeepOriginal = FALSE
)




Inschrijvingen <- Inschrijvingen %>%
  ## Maak variabele INS_Tussenjaren_voor_M voor studenten met één of meer
  ## tussenjaren tussen hun bachelor en master
  mutate(
    ## Maak variabele om te zien of inschrijving direct is
    INS_Direct = (
      INS_Hoogste_vooropleiding_jaar_1CHO + 1) == INS_Inschrijvingsjaar_EOI,
    ## Maak variabele INS_Tussenjaar_voor_B voor studenten met een
    ## tussenjaar voor hun eerste jaar in het hoger onderwijs
    INS_Tussenjaar_voor_B = (
      INS_Hoogste_vooropleiding_jaar_1CHO + 2 <= INS_Inschrijvingsjaar_EOI &
        INS_Opleidingsfase_actueel == "B" &
        INS_Verblijfsjaren_wetenschappelijk_onderwijs == 0 &
        INS_Verblijfsjaren_hoger_onderwijs == 0),
    ## Maak variabele INS_Tussenjaar_voor_P voor studenten met één of meer
    ## tussenjaren tussen hun bachelor en hun premaster
    INS_Tussenjaar_voor_P = (
      INS_Hoogste_vooropleiding_jaar_1CHO + 2 <= INS_Inschrijvingsjaar_EOI &
        INS_Opleidingsfase_actueel == "P" &
        # De student is nog niet begonnen met de premasterfase in dit jaar
        INS_Verblijfsjaar_type_onderwijs_binnen_HO <= 1),
    INS_Tussenjaar_voor_M = (
      INS_Hoogste_vooropleiding_jaar_1CHO + 2 <= INS_Inschrijvingsjaar_EOI &
        INS_Opleidingsfase_actueel == "M" &
        (is.na(INS_Status_Zachte_knip) | INS_Status_Zachte_knip != "Zachte knip") &
        # De student is nog niet begonnen met de masterfase in dit jaar
        INS_Verblijfsjaar_type_onderwijs_binnen_HO <= 1
    )
  )

## We generaliseren de variabelen
## INS_Tussenjaar_voor_B en INS_Tussenjaren_voor_M voor de student per opleiding
Inschrijvingen <- Inschrijvingen %>%
  group_by(INS_Studentnummer, INS_Opleidingsnaam_Z08) %>%
  mutate(
    INS_Tussenjaar_voor_B = INS_Tussenjaar_voor_B[first(which(INS_Studiejaar == 1))],
    INS_Tussenjaar_voor_P = INS_Tussenjaar_voor_P[first(which(INS_Studiejaar == 1))],
    INS_Tussenjaar_voor_M = INS_Tussenjaar_voor_M[first(which(INS_Studiejaar == 1))],
    INS_Direct = INS_Direct[first(which(INS_Studiejaar == 1))]
  ) %>%
  ungroup()


Inschrijvingen <- Inschrijvingen %>%
  mutate(INS_Indicatie_voltijd = if_else(INS_Opleidingsvorm == 1,
                                         TRUE, FALSE
  ))
