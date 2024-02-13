


## Alleen de Bachelor en Master studenten worden geselecteerd.
## Premasterstudenten zijn hier gekenmerkt als "B", "P"
Inschrijvingen <- Inschrijvingen %>% filter(
  INS_Opleidingsfase_actueel %in% c("B", "M", "P")
)


## TODO: Dit lijkt erg VU specifiek, IF-je of var bestaat
## Variabelen transformeren van ja/nee naar TRUE/FALSE
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(
    INS_Inclusief_UvA = transform_no_yes_to_ft(INS_Inclusief_UvA)
  )

### Als student 2 diploma's heeft (voor dezelfde opleidingscode z08) ####
## 1. Pak diplomajaar dat ook een datum heeft
## 2. Als allebei (g)een datum", pak eerste diplomajaar
Inschrijvingen <- Inschrijvingen %>%
  # mutate(extra_kolom = academic_year(INS_Datum_diploma))
  group_by(INS_Studentnummer, INS_Opleidingscode_Z08) %>%
  arrange(INS_Datum_diploma) %>%
  mutate(INS_Datum_diploma = first(na.omit(INS_Datum_diploma))) %>%
  ungroup() %>%
  mutate(INS_Diplomajaar2 = academic_year(INS_Datum_diploma)) %>%
  mutate(INS_Diplomajaar = coalesce(INS_Diplomajaar2, INS_Diplomajaar)) %>%
  select(-INS_Diplomajaar2)


## Meerdere studenten switchen tussen praktisch dezelfde opleidingen met andere namen,
## bijvoorbeeld Bedrijfskunde en IBA waardoor het studiejaar niet meer doortelt.
## Deze studenten zijn soms in hetzelfde inschrijvingsjaar met hetzelfde inschrijvingsjaar EOI
## voor beide studies ingeschreven, en gaan daarna door met de andere variant
## Studiejaren hierboven werden bepaald obv opleidingsnaam, waardoor die bij een
## afsplitsing niet doortelt
## TODO: Als er meer opleidingen worden toegevoegd, niet meer met de case_when werken
## voor switch tussen deze master opleidingen die niet met elkaar te maken hebben.
## Dan misschien een code gebruiken die hetzelfde is voor afgesplitste opleidingen en daarop groupen
Inschrijvingen_afsplitsers <- Inschrijvingen %>%
  filter(
    INS_Opleidingsnaam_2002 %in%
      c(
        "B Bedrijfskunde",
        "B International Business Administration",
        "M Business Administration",
        "M Digital Business and Innovation",
        "M Transport and Supply Chain Management",
        "M Filosofie (2 jaar)",
        "M Filosofie van Cultuur en Bestuur"
      )
  ) %>%
  group_by(INS_Studentnummer, INS_Inschrijvingsjaar, INS_Inschrijvingsjaar_EOI) %>%
  arrange(INS_Studiejaar) %>%
  slice(1) %>%
  ungroup() %>%
  ## Group ook bij Opleidingsfase anders tel je studiejaar door als je van een
  ## van de bovengenoemde B opleidingen doorstroomt naar een van de masters.
  group_by(INS_Studentnummer, INS_Opleidingsfase_BPM) %>%
  arrange(INS_Inschrijvingsjaar) %>%
  filter(any(!is.na(INS_Studiejaar))) %>%
  mutate(
    ## Maak een logische variabele aan om aan te geven of het
    ## inschrijvingsjaar het EOI-jaar is
    INS_Inschrijvingsjaar_is_EOI =
      INS_Inschrijvingsjaar == INS_Eerste_jaar_opleiding_en_instelling,
    # Studiejaar kan alleen berekend worden als EOI bekend is in onze data,
    # maak hiervoor een variabele aan.
    INS_EOI_in_data = case_when(any(INS_Inschrijvingsjaar_is_EOI) ~ TRUE)
  ) %>%
  mutate(INS_Studiejaar2 = if_else(INS_EOI_in_data,
                                   as.double(row_number()), INS_Studiejaar
  )) %>%
  ## Als INS_Inschrijvingsjaar van de vorige rij hetzelfde is, pak dan de
  ## vorige INS_Studiejaar waarde. Anders wordt een dubbele inschrijving in
  ## hetzelfde Inschrijvingsjaar als extra studiejaar geteld.
  mutate(INS_Studiejaar2 = if_else(
    (INS_Inschrijvingsjaar == lag(INS_Inschrijvingsjaar)),
    lag(INS_Studiejaar), INS_Studiejaar2
  )) %>%
  select(-INS_Inschrijvingsjaar_is_EOI, -INS_EOI_in_data) %>%
  ungroup()

## Invullen juiste Studiejaar voor afsplitsers (dus doortellen na afsplitsing)
Inschrijvingen <- Inschrijvingen %>%
  left_join(
    select(
      Inschrijvingen_afsplitsers,
      INS_Studentnummer,
      INS_Opleidingsnaam_2002,
      INS_Inschrijvingsjaar,
      INS_Studiejaar2
    ),
    by = c("INS_Studentnummer", "INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar")
  ) %>%
  mutate(INS_Studiejaar = ifelse(!is.na(INS_Studiejaar2), INS_Studiejaar2, INS_Studiejaar)) %>%
  select(-INS_Studiejaar2)
