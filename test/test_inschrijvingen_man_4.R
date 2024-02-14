
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
