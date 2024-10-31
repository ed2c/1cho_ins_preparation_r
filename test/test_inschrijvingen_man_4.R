
## TODO lastig dit, dit kan pas na combinen

## TODO versimpelen
## TODO
## Tijd tot diploma obv datum aantekening diploma ####

## Bepaal ook het eerste jaar
min_jaar <- min(Inschrijvingen$INS_Inschrijvingsjaar)



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
  )
