

Inschrijvingen <- Inschrijvingen %>%
  group_by(INS_Studentnummer, INS_Inschrijvingsjaar) %>%
  mutate(
    INS_Aantal_inschrijvingen_jaar = n(),
    INS_Aantal_EOI_inschrijvingen_jaar = sum(
      INS_Indicatie_eerste_jaars_opleiding_en_instelling == 1
    )
  ) %>%
  ungroup()


## TODO versimpelen
## We berekenen ook het aantal unieke studies uit de vorige inschrijvingen aan
## de VU, per aantallen jaren terug, om te kunnen bepalen of studenten hiervoor
## andere studies bij de VU deden.
## Voor het berekenen van het aantal unieke studies gebruiken we onderstaande
## functies.
## Deze functie wordt gebruikt om het aantal inschrijvingen in jaar i te bepalen
Tel_opleidingen_huidig <- function(jaar_i, jaar, opleidingsnaam) {
  ## het jaar is gelijk aan het huidige jaar
  jaar_telt_mee <- jaar == jaar_i
  ## Hierna bepalen we de unieke opleidingsnamen in het huidige jaar
  unieke_opleidingen <- unique(opleidingsnaam[jaar_telt_mee])
  ## Het aantal unieke opleidingsnamen in het huidige jaar
  length(unieke_opleidingen)
}

Tel_opleidingen_min_1 <- function(jaar_i, jaar, opleidingsnaam) {
  ## we kijken naar het huidige jaar en het jaar daarvoor
  jaar_telt_mee <- jaar == jaar_i | jaar == jaar_i - 1
  ## Hierna bepalen we de unieke opleidingsnamen in deze tijdsperiode
  unieke_opleidingen <- unique(opleidingsnaam[jaar_telt_mee])
  ## Het aantal unieke opleidingsnamen in deze tijdsperiode
  length(unieke_opleidingen)
}

Tel_opleidingen_min_2 <- function(jaar_i, jaar, opleidingsnaam) {
  ## we kijken naar het huidige jaar en de twee jaren daarvoor
  jaar_telt_mee <- jaar == jaar_i | jaar == jaar_i - 1 | jaar == jaar_i - 2
  ## Hierna bepalen we de unieke opleidingsnamen in deze tijdsperiode
  unieke_opleidingen <- unique(opleidingsnaam[jaar_telt_mee])
  ## Het aantal unieke opleidingsnamen in deze tijdsperiode
  length(unieke_opleidingen)
}

Aantal_unieke_opleidingen <- function(opleidingsnaam, jaar, Jaren_terug = 1) {
  ## Het aantal unieke jaren wordt bepaald
  unieke_jaren <- unique(jaar)

  ## Afhankelijk van het aantal jaren terug dat wordt opgegeven, wordt de
  ## bijbehorende functie gekozen, als het geen van deze functie is wordt NA
  ## teruggegeven.
  if (Jaren_terug == 0) {
    Resultaat <- purrr::map_int(unieke_jaren,
                                .f = Tel_opleidingen_huidig,
                                jaar = jaar,
                                opleidingsnaam = opleidingsnaam
    )
  } else if (Jaren_terug == 1) {
    Resultaat <- purrr::map_int(unieke_jaren,
                                .f = Tel_opleidingen_min_1,
                                jaar = jaar,
                                opleidingsnaam = opleidingsnaam
    )
  } else if (Jaren_terug == 2) {
    Resultaat <- purrr::map_int(unieke_jaren,
                                .f = Tel_opleidingen_min_2,
                                jaar = jaar,
                                opleidingsnaam = opleidingsnaam
    )
  } else {
    return(NA)
  }

  ## Het jaartal en het aantal unieke opleidingsnamen worden gematcht waardoor
  ## het resultaat aan de data gekoppeld kan worden.
  Resultaat[match(jaar, unieke_jaren)]
}

## Bovenstaande functies worden gebruikt om het aantal unieke studies van
## een student te berekenen.
Inschrijvingen <- Inschrijvingen %>%
  group_by(INS_Studentnummer) %>%
  mutate(
    INS_Aantal_unieke_opleidingen_huidig_jaar =
      Aantal_unieke_opleidingen(INS_Opleidingsnaam_Z08,
                                INS_Inschrijvingsjaar,
                                Jaren_terug = 0
      ),
    INS_Aantal_unieke_opleidingen_laatste_2_jaar =
      Aantal_unieke_opleidingen(INS_Opleidingsnaam_Z08,
                                INS_Inschrijvingsjaar,
                                Jaren_terug = 1
      ),
    INS_Aantal_unieke_opleidingen_laatste_3_jaar =
      Aantal_unieke_opleidingen(INS_Opleidingsnaam_Z08,
                                INS_Inschrijvingsjaar,
                                Jaren_terug = 2
      )
  ) %>%
  ungroup() %>%
  ## Definieren van dubbele studie als losse variabele
  mutate(
    INS_Dubbele_studie_VU = INS_Aantal_unieke_opleidingen_huidig_jaar > 1
  )

