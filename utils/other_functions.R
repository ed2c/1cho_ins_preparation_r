#' Create documentatie tests
#'
#' Functie die test documentatie aanmaakt, op basis van het meegegeven dataframe.
#' Deze documentatie wordt weggeschreven naar een csv bestand met de write_documentatie_tests
#' functie.
#' @param df Het meegegeven dataframe
#' @param Naam Naam van het bestand
#' @param Limietwaarden_bestandspad Bestandpad binnen XX. Documentatie waarin de
#' limietwaarden van numerieke waarden zijn opgeslagen als csv bestand.Dit wordt
#' gebruikt voor de testdocumentatie
#' @family assertions
#' @family tests
#' @export
create_documentatie_tests <- function(df, Naam, Limietwaarden_bestandspad = NA) {
  Veldnaam <- Veldtype <- lower_new <- upper_new <- lower <- upper <- NULL
  ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  ## SAMENVATTING FUNCTIE:
  ## 1. Deze functie maakt eerst vectoren van Veldnamen, Veldtypes, NAS, Lower,
  ## Upper, Pattern en Uniek
  ## 2. Vervolgens checkt de functie of de character types een subset zouden moeten zijn,
  ## dit betekent dat de waarden in het te testen dataframe alleen de waarden
  ## uit de list mogen hebben, die in deze functie zijn gedefinieerd.
  ## 3. Vervolgens maakt de functie een tabel, die wordt weggeschreven naar een csv bestand

  ## _____________________________________________________________________________
  ## 1. A. MAAK VECTOREN VOOR DE KOLOMMEN VAN DE TESTDOCUMENTATIE

  ## Sla de kolomnamen op
  Veldnamen <- names(df)
  ## Sla de veldtypes op
  Veldtypes <- purrr::map_chr(df, first_class)
  ## Controleer of de veldtypes die character zijn een subset zouden moeten zijn;
  ## De huidige regel is dat het veld minder dan 20 unieke waardes mag bevatten
  Veldtypes <- purrr::map_chr(Veldnamen, veld_is_subset, df = df, Veldnamen = Veldnamen, Veldtypes = Veldtypes)
  ## Check voor het percentage missende waarden in de kolommen
  p_NAs <- purrr::map_dbl(df, bereken_na_waarden)

  ## Vind de max en min waarden voor numerieke kolommen.
  Lower <- purrr::map_dbl(df, min_num, na.rm = TRUE)


  Upper <- purrr::map_dbl(df, max_num, na.rm = TRUE)

  ## Vind patronen die kolommen met type character kunnen hebben mbv de match_kolom_met_patroon
  Pattern <- purrr::map_chr(df, match_kolom_met_patroon)

  ## Voor pattern: gebruik XX. Documentatie/Testpatterns/Patterns om voor patronen te zoeken
  ## Ga voor elke variabele in de df na of deze matcht met Ã©Ã©n van deze patronen
  ## Als dit zo is, voeg patroon toe aan pattern kolom

  Uniek <- purrr::map_lgl(Veldnamen, is_unieke_identifier, df = df)

  ## Zet alle kolommen op In_gebruik is TRUE
  In_gebruik <- rep(TRUE, length(Veldnamen))

  ## _____________________________________________________________________________
  ## 1. B. MAAK DATAFRAME OM LATER WEG TE SCHRIJVEN
  ## Vectoren samen vormen een table
  Table <- tibble::tibble(Veldnaam = Veldnamen,
                          In_gebruik = In_gebruik,
                          Veldtype = Veldtypes,
                          p_na = p_NAs,
                          lower = Lower,
                          upper = Upper,
                          pattern = Pattern,
                          Uniek = Uniek)

  ## _____________________________________________________________________________
  ## 2. CHECK OF KOLOM TYPE SUBSET HEEFT
  ## Maak subdataframe met veldnamen en veldtypen
  df_veldnaam_en_type <- Table %>%
    dplyr::select(Veldnaam, Veldtype)
  ## Zoek voor elk veld van type subset (of list) naar de kolomwaarden
  Set_kolomwaarden <- purrr::pmap(df_veldnaam_en_type, vind_kolomwaarden, df = df) %>%
    unlist()

  ## Voeg de kolomwaarden toe aan de tabel
  Table <- Table %>%
    dplyr::mutate(Kolomwaarden = Set_kolomwaarden)

  ## 08-02-2019: JvZ: Dit bestand is een afhankelijkheid, er staat een bestandspad
  ## hard geprogrammeerd maar de gebruiker weet niet waar deze staat. In een
  ## ander project staat dit bestand niet, en krijg je een error. Kan je het bestandspad instelbaar
  ## maken, of eerst controleren of het bestand bestaat?
  ##
  ## Van te voren ingestelde limietwaarden voor numerieke kolommen vinden en
  ## toevoegen aan het test-df
  if (file.exists(paste0(Sys.getenv("DOCUMENTATION_DIR"), Limietwaarden_bestandspad))) {
    ## Lees een tabel in met de vaste limiet-waarden voor sommige numerieke variabelen (bijv studentnummer)
    Tabel_limietwaarden <- read_documentation(Limietwaarden_bestandspad, readr = TRUE)
    #Tabel_limietwaarden <- read_documentatie("Testdocumentatie/Numerieke_limietwaarden.csv", readr = TRUE)
    ## Verander kolomtype naar double, zodat deze beter vergeleken kan worden met andere tabellen
    Tabel_limietwaarden <- Tabel_limietwaarden %>%
      dplyr::mutate(lower_new = as.double(lower_new), upper_new = as.double(upper_new))
    ## Verander de lower en upper waarden van variabelen die in de Tabel_limietwaarden
    ## voorkomen naar de waarden uit de tabel
    Table <- Table %>%
      dplyr::left_join(Tabel_limietwaarden, by = "Veldnaam") %>%
      dplyr::mutate(lower = dplyr::coalesce(lower_new, lower),
                    upper = dplyr::coalesce(upper_new, upper)) %>%
      dplyr::select(-lower_new, -upper_new)
  }
  ## _____________________________________________________________________________
  ## 3. SCHRIJF DATAFRAME WEG NAAR DE JUISTE LOCATIE
  ## Schrijf de tabel weg naar een csv bestand in de volgende map: XX. Documentatie/Testdocumentatie/
  write_documentatie_tests(Table, Naam)
  return(Table)
}



#' Create documentation
#'
#' @param df The dataframe
#' @param Naam Name of the documentation file to create
#' @param Limietwaarden_bestandspad NA
#'
#' @return Table
#' @export
#'
create_documentatie <- function(df, Naam, Limietwaarden_bestandspad = NA) {
  Veldnaam <- Veldtype <- lower_new <- upper_new <- lower <- upper <- NULL
  ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  ## SAMENVATTING FUNCTIE:
  ## 1. Deze functie maakt eerst vectoren van Veldnamen, Veldtypes, NAS, Lower,
  ## Upper, Pattern en Uniek
  ## 2. Vervolgens checkt de functie of de character types een subset zouden moeten zijn,
  ## dit betekent dat de waarden in het te testen dataframe alleen de waarden
  ## uit de list mogen hebben, die in deze functie zijn gedefinieerd.
  ## 3. Vervolgens maakt de functie een tabel, die wordt weggeschreven naar een csv bestand

  ## _____________________________________________________________________________
  ## 1. A. MAAK VECTOREN VOOR DE KOLOMMEN VAN DE TESTDOCUMENTATIE

  ## Sla de kolomnamen op
  Veldnamen <- names(df)
  ## Sla de veldtypes op
  Veldtypes <- purrr::map_chr(df, first_class)
  ## Controleer of de veldtypes die character zijn een subset zouden moeten zijn;
  ## De huidige regel is dat het veld minder dan 20 unieke waardes mag bevatten
  Veldtypes <- purrr::map_chr(Veldnamen, veld_is_subset, df = df, Veldnamen = Veldnamen, Veldtypes = Veldtypes)
  ## Check voor het percentage missende waarden in de kolommen
  p_NAs <- colMeans(is.na(df)) * 100

  ## Vind de max en min waarden voor numerieke kolommen.
  Lower <- purrr::map_dbl(df, ~ifelse(all(is.na(.)), NA, minnum(.)))
  Upper <- purrr::map_dbl(df, ~ifelse(all(is.na(.)), NA, maxnum(.)))


  ## Vind patronen die kolommen met type character kunnen hebben mbv de match_kolom_met_patroon
  Pattern <- purrr::map_chr(df, match_kolom_met_patroon)

  ## Voor pattern: gebruik XX. Documentatie/Testpatterns/Patterns om voor patronen te zoeken
  ## Ga voor elke variabele in de df na of deze matcht met Ã©Ã©n van deze patronen
  ## Als dit zo is, voeg patroon toe aan pattern kolom

  Uniek <- purrr::map_lgl(Veldnamen, is_unieke_identifier, df = df)

  ## Zet alle kolommen op In_gebruik is TRUE
  In_gebruik <- rep(TRUE, length(Veldnamen))

  ##
  Percentages <- purrr::map_chr(df, get_ratio)

  ##
  Distribution <- purrr::map_chr(df, get_dist)

  ## Maak een lege kolom voor opmerkingen
  Opmerkingen <- ""

  ## _____________________________________________________________________________
  ## 1. B. MAAK DATAFRAME OM LATER WEG TE SCHRIJVEN
  ## Vectoren samen vormen een table
  Table <- tibble::tibble(Veldnaam = Veldnamen,
                          Veldnaam_export = Veldnamen,
                          In_gebruik = In_gebruik,
                          Veldtype = Veldtypes,
                          p_na = p_NAs,
                          lower = Lower,
                          upper = Upper,
                          pattern = Pattern,
                          Uniek = Uniek,
                          Percentages = Percentages,
                          Distribution = Distribution,
                          Opmerkingen = Opmerkingen)

  ## _____________________________________________________________________________
  ## 2. CHECK OF KOLOM TYPE SUBSET HEEFT
  ## Maak subdataframe met veldnamen en veldtypen
  df_veldnaam_en_type <- Table %>%
    dplyr::select(Veldnaam, Veldtype)
  ## Zoek voor elk veld van type subset (of list) naar de kolomwaarden
  Set_kolomwaarden <- purrr::pmap(df_veldnaam_en_type, vind_kolomwaarden, df = df) %>%
    unlist()

  ## 08-02-2019: JvZ: Dit bestand is een afhankelijkheid, er staat een bestandspad
  ## hard geprogrammeerd maar de gebruiker weet niet waar deze staat. In een
  ## ander project staat dit bestand niet, en krijg je een error. Kan je het bestandspad instelbaar
  ## maken, of eerst controleren of het bestand bestaat?
  ##
  if (file.exists(paste0(Sys.getenv("DOCUMENTATION_DIR"), Limietwaarden_bestandspad))) {
    ## Lees een tabel in met de vaste limiet-waarden voor sommige numerieke variabelen (bijv studentnummer)
    Tabel_limietwaarden <- read_documentation(Limietwaarden_bestandspad, readr = TRUE)
    #Tabel_limietwaarden <- read_documentatie("Testdocumentatie/Numerieke_limietwaarden.csv", readr = TRUE)
    ## Verander kolomtype naar double, zodat deze beter vergeleken kan worden met andere tabellen
    Tabel_limietwaarden <- Tabel_limietwaarden %>%
      dplyr::mutate(lower_new = as.double(lower_new), upper_new = as.double(upper_new))
    ## Verander de lower en upper waarden van variabelen die in de Tabel_limietwaarden
    ## voorkomen naar de waarden uit de tabel
    Table <- Table %>%
      dplyr::left_join(Tabel_limietwaarden, by = "Veldnaam") %>%
      dplyr::mutate(lower = dplyr::coalesce(lower_new, lower),
                    upper = dplyr::coalesce(upper_new, upper)) %>%
      dplyr::select(-lower_new, -upper_new)
  }

  ## _____________________________________________________________________________
  ## 3. SCHRIJF DATAFRAME WEG NAAR DE JUISTE LOCATIE
  ## Schrijf de tabel weg naar een csv bestand in de volgende map: XX. Documentatie/Testdocumentatie/
  write_documentatie(Table, Naam)
  return(Table)
}

#' First class
#'
#' Functie die de class van een element of vector zoekt en hier, indien er meerdere
#' benamingen van het class zijn, de eerste class van neemt.
#' @param x Object waar de class van wordt gezocht
#' @export
first_class <- function(x) {
  return(class(x)[1])
}

#' Bereken NA waarden
#'
#' Functie om per kolom te berekenen wat het percentage NAs is. Inclusief marge
#' van 10 procent.
#' @param x vector
#' @param ... Extra argumenten
#' @export
bereken_na_waarden <- function(x, ...) {
  ## ipv onderstaand moet de uitgecommente code worden gebruikt.
  ## return(sum(is.na(x)) / length(x) * 110)
  ## Er wordt een marge genomen van 10%, hierbij wordt een correctie gemaakt obv de grootte
  ## van de data. A = [1 - 1/log(x)] is deze correctie, die aan de marge van 10% wordt
  ## bijgevoegd: A*marge. Hierbij wordt ook nog een marge toegevoegd op basis van het huidige
  ## percentage missende waarden: 10% van huidige missings. Dit wordt aan het huidige percentage
  ## NAs toegevoegd, om bij een kleine stijging in missende waarden de assertion niet meteen af te
  ## laten gaan.
  return((sum((is.na(x))/length(x)) + (sum(is.na(x))/length(x))*0.10 + (1 - 1/log(length(x)))*0.10)*100)
}

#' Veld is subset
#'
#' Functie die checkt of een veld bestaat uit subset-waarden. We defineren een subset
#' als het een characterveld is dat minder dan 20 unieke waarden bevat.
#'
#' @param Veld Naam van het te controleren veld
#' @param df Dataframe dat gecontroleerd word
#' @param Veldnamen Set waar het Veld een onderdeel van is
#' @param Veldtypes Types die bij de Veldnamen horen
#' @family tests
#' @family assertions
veld_is_subset <- function(Veld, df, Veldnamen, Veldtypes) {
  ## Vind de index positie van de veldnaam
  Index <- which(Veldnamen == Veld)
  ## Check of Kolom een character is
  if (Veldtypes[Index] == "character") {
    ## Check of deze minder dan 20 unieke waardes bevat
    if (length(unique(df[[Veld]])) < 20) {
      ## Return type is subset
      return("subset")
    } else {
      ## Anders blijft het type hetzelfde
      return(Veldtypes[[Index]])
    }
  } else {
    ## Anders blijft het type hetzelfde
    return(Veldtypes[[Index]])
  }
}

#' Minimum numeric
#'
#' Function finds minimum of numeric vector. Else it returns NA
#' @param x Vector of which minimum is wanted
#' @param ... extra arguments
#' @export
min_num <- function(x, ...) {
  if (!is.numeric(x)) {
    return(NA_real_)
  } else {
    ## vind  minimum in vector x
    min_x <- min(x, ...)
    ## Behoud een range van 10% voor het minimum
    if (min_x < 0 ) {
      min_x <- min_x*1.1
    } else {
      min_x <- min_x*0.9
    }
    return(min_x)
  }
}


#' Round context
#' negative values rounded down, positive up
#'
#' @param x numeric
#'
#' @return rounded value
#' @export
#'
round_values <- function(x) {
  ifelse(x >= 0, ceiling(x), floor(x))
}

#' Minimum numeric
#'
#' Function finds minimum of numeric vector. Else it returns NA
#' @param x Vector of which minimum is wanted
#' @export
minnum <- function(x) {
  if (is.numeric(x)) {
    non_missing_values <- x[!is.na(x)]
    if (length(non_missing_values) > 0) {
      return(round(round_values(min(non_missing_values)), digits = 0))
    } else {
      return(NA_real_)
    }
  } else {
    return(NA_real_)
  }
}

#' Maximum numeric
#'
#' Function finds maximum of numeric vector.
#' Else it returns NA
#' @param x Vector of which minimum is wanted
#' @export
maxnum <- function(x) {
  if (is.numeric(x)) {
    non_missing_values <- x[!is.na(x)]
    if (length(non_missing_values) > 0) {
      return(round(round_values(max(non_missing_values)), digits = 0))
    } else {
      return(NA_real_)
    }
  } else {
    return(NA_real_)
  }
}

#' Maximum numeric
#'
#' Function finds maximum of numeric vector.
#' Else it returns NA
#' @param x Vector of which minimum is wanted
#' @param ... extra arguments
#' @export
max_num <- function(x, ...) {
  if (!is.numeric(x)) {
    return(NA_real_)
  } else {
    max_x <- max(x, ...)
    if (max_x < 0) {
      max_x <- max_x*0.9
    } else {
      max_x <- max_x*1.1
    }
    return(max_x)
  }
}

#' Is unieke identifier
#'
#' Functie die voor ieder veld kijkt of dit een unieke identifier is.
#' @param Veldnaam De naam die getest wordt op uniek zijn
#' @param df dataframe waar unieke identifier voor wordt gezocht
#' @export
is_unieke_identifier <- function(Veldnaam, df) {
  Kolom <- df %>% dplyr::select(Veldnaam)
  ## Zoek voor unique identifier
  ## length(df$Veldnaam) == length(unique(df$Veldnaam))
  if (nrow(df) == dplyr::n_distinct(Kolom)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  ## zoek naar combinaties van unique identifier
  # nrow(df) ==  nrow(distinct(df, Diploma, Studentnummer))
  #
  # Meest voorkomende combinaties:
  # INS_Studentnummer
  # INS_Opleidingsnaam_2002
  # INS_Opleidingscode_actueel
  # INS_Inschrijvingsjaar
  # INS_Inschrijvingsjaar_EOI
}

#' Match kolom met patroon
#'
#' Functie die voor veld in dataframe checkt of deze matcht met patroon uit patronen-bestand
#' @param x vector waarop check moet worden uitgevoerd
#' @family assertions
#' @family tests
match_kolom_met_patroon <- function(x) {
  if (is.numeric(x)) {
    if (all(x > 2000, na.rm = T)) {
      x <- as.character(x)
    }
  }
  if (lubridate::is.Date(x)) {
    x <- as.character(x)
  }
  if (lubridate::is.POSIXct(x) | lubridate::is.POSIXt(x) | lubridate::is.POSIXlt(x)) {
    x <- as.character(x)
  }
  ## Als de kolom geen character is, wordt NA geretourneerd
  if (!is.character(x)) {
    return(NA_character_)
  }
  ## Gebruik Veldnaam om met de bijbehorende data te zoeken naar een patroon
  Bestaande_patronen <- read_documentation("/Testpatterns/Patterns.csv")
  for (p in Bestaande_patronen$Pattern) {
    if (all(grepl(p, unique(x[!is.na(x)])))) {
      return(p)
    }
  }
  return(NA_character_)
}


#' get_dist
#'
#' Get distribution for numeric columns
#' @param data_vector column to check
#' @family assertions
#' @family tests
get_dist <- function(data_vector) {
  if (is.numeric(data_vector) ) {
    quantiles <- stats::quantile(data_vector, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    summary_stats <- c(Mean = mean(data_vector, na.rm = TRUE),
                       Std_Dev = stats::sd(data_vector, na.rm = TRUE))
    quantiles <- paste("Q1 =", quantiles[1], " | median =", quantiles[2], " | Q3 = ", quantiles[3],
                       " | mean =", format(summary_stats[1], digits = 3), " | stdev =", format(summary_stats[2], digits = 3))
    return(quantiles)

  }
  else if (is.character(data_vector)) {
    if (any(grepl("^\\d*\\.?\\d*$", sample(data_vector, 100)))) {

      data_vector <- data_vector[grepl("^\\d*\\.?\\d*$",  data_vector)]
      data_vector <- data_vector[!is.na(data_vector)]
      data_vector <- as.double(data_vector)
      quantiles <- stats::quantile(data_vector, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      summary_stats <- c(Mean = mean(data_vector, na.rm = TRUE),
                         Std_Dev = stats::sd(data_vector, na.rm = TRUE))
      quantiles <- paste("Q1 =", quantiles[1], " | median =", quantiles[2], " | Q3 = ", quantiles[3],
                         " | mean =", format(summary_stats[1], digits = 3), " | stdev =", format(summary_stats[2], digits = 3))
      return(quantiles)
    } else {
      default_na <- NA_character_
      return(default_na)
    }
  } else{
    default_na <- NA_character_
    return(default_na)
  }
}


#' get_ratio
#'
#' Get percentages for factor columns
#' @param data_vector column to check
#' @family assertions
#' @family tests
get_ratio <-  function(data_vector) {

  # Initialize percent variable
  percent <- NULL

  # Check if the data_vector meets certain conditions
  if (!lubridate::is.POSIXct(data_vector)) {

    # Create frequency table of the data_vector and sort it by descending order
    frequency_table <- janitor::tabyl(data_vector) %>%
      dplyr::arrange(dplyr::desc(percent)) %>%
      utils::head(10) %>%
      dplyr::filter(!is.na(data_vector))

    # Assign the categories to a variable
    categories <- as.character(frequency_table$data_vector)

    # Assign the percentages to a variable without rounding
    percentages <- frequency_table$percent * 100

    # Only add "Overig" category if there are more than 10 unique elements
    if (sum(percentages) < 100 && length(unique(data_vector)) > 10) {
      percentages <- append(percentages, 100 - sum(percentages))
      categories <- append(categories, "Other")
    } else {
      percentages[length(percentages)] <- 100 - sum(percentages[-length(percentages)])
    }

    # Round percentages to two decimal places for display
    percentages <- format(percentages, digits = 2)

    # Combines the categories and percentages into a single string
    result <- paste0(categories, ": (", percentages, "%)", collapse = " | ")
    return(result)
  }
  else if (lubridate::is.POSIXct(data_vector)) {

    # Create frequency table of the data_vector and sort it by descending order and keep top 10 values
    frequency_table <- janitor::tabyl(data_vector) %>%
      dplyr::arrange(dplyr::desc(percent)) %>%
      dplyr::top_n(10, "percent") %>%
      utils::head(10) %>%
      dplyr::filter(!is.na(data_vector))

    # Assign the categories to a variable
    categories <- as.character(frequency_table$data_vector)

    # Assign the percentages to a variable without rounding
    percentages <- frequency_table$percent * 100

    # Only add "Overig" category if there are more than 10 unique elements
    if (sum(percentages) < 100 && length(unique(data_vector)) > 10) {
      percentages <- append(percentages, 100 - sum(percentages))
      categories <- append(categories, "Other")
    } else {
      percentages[length(percentages)] <- 100 - sum(percentages[-length(percentages)])
    }

    # Round percentages to two decimal places for display
    percentages <- format(percentages, digits = 2)

    # Check if first percentage is less than 1
    if (as.numeric(percentages[1]) < 0.01) {
      return("Not enough occurences")
    } else {
      # Combines the categories and percentages into a single string
      result <- paste0(categories, ": (", percentages, "%)", collapse = " | ")
      return(result)
    }
  }
  else{
    return("Too many categories")
  }
}



#' Vind kolomwaarden
#'
#' Functie om kolomwaarden voor een veld te bepalen. Deze waarden zijn unieke
#' voorkomende waarden, muv NA.
#' @param Veldnaam Veld/kolom waarvoor de kolomwaarden worden gezocht
#' @param Veldtype Type van het veld
#' @param df dataframe waarin de waarden worden gezocht
#' @export
vind_kolomwaarden <- function(Veldnaam, Veldtype, df) {
  Waarde <- NULL
  if (!(Veldtype %in% c("subset", "list"))) {
    return(NA)
  } else {
    df_uniek <- df %>%
      ## Selecteer de kolommen uit de df
      dplyr::select(Veldnaam) %>%
      ## Omklappen van de kolommen
      tidyr::gather(key = "Kolomnaam", value = "Waarde") %>%
      ## Neem alleen de unieke waarden mee, geen NA
      dplyr::distinct() %>%
      dplyr::filter(!is.na(Waarde))
    Kolomwaarden <- paste0("'", df_uniek$Waarde, "'", collapse = ", ")
    return(Kolomwaarden)
  }
}

#' Write documentatie tests
#'
#' Functie om een tabel weg te schrijven in excel als csv file
#'
#' @param Table Tabel die moet worden weggeschreven
#' @param Naam Naam die meegegeven wordt aan het document
#' @family assertions
#' @family tests
write_documentatie_tests <- function(Table, Naam) {
  ## Path waar de tabel moet worden opgeslagen
  path <- paste0(Sys.getenv("DOCUMENTATION_DIR"), "/Testdocumentatie/", Naam, ".csv")
  ## creeÃ«r map om bestanden in op te slaan
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  return(readr::write_csv2(Table, path))
}

write_documentatie <- function(Table, Naam) {
  ## Path waar de tabel moet worden opgeslagen
  path <- paste0(Sys.getenv("DOCUMENTATION_DIR"), Naam, ".csv")
  ## creeÃ«r map om bestanden in op te slaan
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  return(readr::write_csv2(Table, path))
}
