## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Inlezen Decoderingsbestanden.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2021 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: In dit script worden de ascii bestanden Dec_brinnummer, Dec_vooropl, Dec_vopl
## ingelezen en omgezet naar een rds/csv bestand
##
## Afhankelijkheden: geen
##
## Datasets: /1cHO/2020/LEESMIJ en reerentietabellen/Dec_brinnummer.asc
## /1cHO/2020/LEESMIJ en reerentietabellen/Dec_vopl.asc
## /1cHO/2020/LEESMIJ en reerentietabellen/Dec_vooropl.asc
##
## Opmerkingen:
## 1) Voor Dec_vooropl en Dec_vopl inlezen zijn aparte bestanden
## 2) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lees alle benodigde bestanden in:

file_Dec_brinnummer <- paste0(
  Sys.getenv("NETWORK_DIR"),
  "Datasets/1cHO/2022/referentietabellen en documentatie/Dec_brinnummer.asc"
)
Dec_vooropl <- paste0(
  Sys.getenv("NETWORK_DIR"),
  "Datasets/1cHO/2022/referentietabellen en documentatie/Dec_vooropl.asc"
)
Dec_vopl <- paste0(
  Sys.getenv("NETWORK_DIR"),
  "Datasets/1cHO/2022/referentietabellen en documentatie/Dec_vopl.asc"
)

Dec_brinnummer <- read_delim(file_Dec_brinnummer,
  col_types = cols(X1 = col_character()),
  delim = ";",
  col_names = FALSE
)

## locale call gets rid of <?> block
Dec_vooropl <- read_delim(Dec_vooropl,
  col_types = cols(X1 = col_character()),
  delim = ";",
  col_names = FALSE,
  locale = locale(encoding = "windows-1252")
)

Dec_vopl <- read_delim(Dec_vopl,
  col_types = cols(X1 = col_character()),
  delim = ";",
  col_names = FALSE
)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Bestandsbeschrijving_Dec-bestanden.txt bevat uitleg voor selecteren kolommen

## Dec_brinnummer bestaat uit Brinnummer, Naam, Postcodecijfers, Plaats, Datum oprichting,
## Datum opheffing, Code denominatie, Naam Denominatie
Dec_brinnummer <- Dec_brinnummer %>%
  mutate(
    Brinnummer = str_sub(X1, 1, 4),
    Naam = str_sub(X1, 5, 34),
    Postcodecijfers = str_sub(X1, 35, 38),
    Plaats = trimws(str_sub(X1, 39, 62)),
    Datum_oprichting = str_sub(X1, 63, 70),
    Datum_opheffing = trimws(str_sub(X1, 71, 78)),
    Code_denominatie = trimws(str_sub(X1, 79, 81)),
    Naam_denominatie = str_sub(X1, 82, 112)
  ) %>%
  select(-X1) %>%
  mutate(Naam = trimws(utf8::utf8_encode(Naam), which = c("both")))

## Dec_Vooropl bestaat uit code, onderwijssector en omschrijving: split in kolommen
Dec_vooropl <- Dec_vooropl %>%
  ## Verwijderen van accenten
  mutate_at(
    c("X1"),
    ~ stringi:::stri_trans_general(str = ., id = "Latin-ASCII")
  ) %>%
  mutate(
    Code_vooropleiding_oorspr = as.numeric(str_sub(X1, 1, 5)),
    Onderwijssector = str_sub(X1, 6, 8),
    Omschrijving_vooropleiding_oorspr = (trimws(str_sub(X1, 9, 180)))
  ) %>%
  select(-X1)

## Dec_vopl bestaat uit Code vooropleiding, omschrijving vooropleiding
Dec_vopl <- Dec_vopl %>%
  mutate(
    Code_vooropleiding = as.numeric(str_sub(X1, 1, 5)),
    Omschrijving_vooropleiding = trimws(str_sub(X1, 6, 78))
  ) %>%
  select(-X1)


## Voeg extra kolom toe met het soort vooropleiding, dit is een afgekapte Omschrijving vooropleiding
Dec_vopl <- Dec_vopl %>%
  mutate(Omschrijving_vooropleiding_soort = case_when(
    grepl("vwo profiel", Omschrijving_vooropleiding) ~ "vwo profiel",
    grepl("havo profiel", Omschrijving_vooropleiding) ~ "havo profiel",
    grepl("vwo algemeen", Omschrijving_vooropleiding) ~ "vwo oude stijl",
    grepl("havo algemeen", Omschrijving_vooropleiding) ~ "havo oude stijl",
    grepl("mbo", Omschrijving_vooropleiding) ~ "mbo",
    grepl("vmbo", Omschrijving_vooropleiding) ~ "vmbo",
    grepl("vbo", Omschrijving_vooropleiding) ~ "vbo",
    grepl(
      "overig buitenlands diploma / Europees baccalaureaat",
      Omschrijving_vooropleiding
    ) ~ "buitenlands diploma",
    grepl("overig beschikking College van Bestuur", Omschrijving_vooropleiding) ~ "beschikking CvB",
    grepl("overig ministriele beschikking", Omschrijving_vooropleiding) ~ "beschikking ministerie",
    grepl("overig getuigschriften", Omschrijving_vooropleiding) ~ "getuigschriften",
    grepl("overig toelatingsexamen", Omschrijving_vooropleiding) ~ "toelatingsexamen",
    grepl(
      "overig coll.doc./ vooropl.onderzoek / besch.CvB / verkl.toets.cie ma-opl",
      Omschrijving_vooropleiding
    ) ~ "overig / colloquium. Doctum",
    grepl("vooropleiding onbekend", Omschrijving_vooropleiding) ~ "onbekend",
    grepl("wo-ba", Omschrijving_vooropleiding) ~ "wo-ba",
    grepl("wo-on/ma", Omschrijving_vooropleiding) ~ "wo-on/ma",
    grepl("wo-p", Omschrijving_vooropleiding) ~ "wo-p",
    grepl("hbo-ba", Omschrijving_vooropleiding) ~ "hbo-ba",
    grepl("wo-vo/ma/bf", Omschrijving_vooropleiding) ~ "wo-vo/ma/bf",
    grepl("hbo-vo/ma", Omschrijving_vooropleiding) ~ "hbo-vo/ma",
    grepl("hbo-p", Omschrijving_vooropleiding) ~ "hbo-p",
    grepl("hbo-ad", Omschrijving_vooropleiding) ~ "hbo-ad",
    grepl("wo-pim", Omschrijving_vooropleiding) ~ "wo-pim",
    grepl("hbo-pim", Omschrijving_vooropleiding) ~ "hbo-pim",
    grepl("onbekend", Omschrijving_vooropleiding) ~ "onbekend",
    TRUE ~ NA_character_
  ))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
saverds_csv(Dec_brinnummer, "Dec_brinnummer", dataloc = "Datasets/1cHO/2022/", save_csv = TRUE)
saverds_csv(Dec_vooropl, "Dec_vooropl", dataloc = "Datasets/1cHO/2022/", save_csv = TRUE)
saverds_csv(Dec_vopl, "Dec_vopl", dataloc = "Datasets/1cHO/2022/", save_csv = TRUE)

clear_script_objects()
