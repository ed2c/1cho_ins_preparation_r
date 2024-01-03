## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Inladen Packages.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2021 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: In dit script worden alle benodigde packages geinstalleerd als ze nog
## niet geinstalleerd zijn, vervolgens worden deze ingeladen.
##
## Afhankelijkheden: Geen
##
## Datasets: Geen
##
## Opmerkingen:
## 1) Geen
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Basis_packages <- c(
  "dataMaid",       ## Gebruikt voor de export analysesets.
  "rvest",           ## Gebruikt voor html
  "MASS",           ## a lot of basis statistics functions
  "ggplot2",        ## basic plots
  "ggfortify",      ## ggplot extension
  "readxl",         ## Gebruikt om excel (.xls and .xlsx) bestanden in te lezen.
  "officer",        ## tabellen in word en ppt zetten
  "flextable",      ## tabellen omzetten naar tekstformaat
  "stargazer",      ## clean export of model objects
  "broom",          ## function for clean output of model objects
  "openxlsx",       ## open xlsx
  "caret",          ## Gebruikt voor prognose instroom komend jaar
  "BurStMisc",      ## Gebruikt om statistische testen uit te voeren in vusa
  "checkmate",      ## Gebruikt voor de assertions tests.
  "cli",            ## Gebruikt om kleur mee te geven aan berichten in de console
  "digest",         ## Gebruikt voor het hashen van variabelen
  "devtools",       ## Gebruikt om te installeren via Github.
  "gridExtra",      ## Gebruikt om meerdere graphical objects in een tabel te plaatsen.
  "haven",          ## Gebruikt voor importeren van SPSS, STATA en SAS bestanden.
  "httr",           ## Gebruikt om met HTTP te werken.
  "janitor",        ## Gebruikt om namen op te schonen van speciale tekens.
  "lubridate",      ## Gebruikt om te werken met data en tijden.
  "purrr",          ## Gebruikt om met functies and vectoren te werken.
  "furrr",          ## Gebruikt om functies van purrr met meerdere processoren uit te voeren.
  "readr",          ## Gebruikt om data (csv, tsv, and fwf) in te lezen.
  "vroom",          ## Gebruikt om data (csv) sneller in te lezen.
  "slackr",         ## Gebruikt voor het sturen van berichten in Slack.
  "sp",             ## Gebruikt voor het bewerken van dataframes.
  "stats",          ## Gebruikt voor statisctische functies en berekeningen.
  "stringr",        ## Gebruikt voor functies om met strings te werken.
  "testthat",       ## Gebruikt voor het testen in de assertions.
  "tibble",         ## Gebruikt voor bewerken en aanmaken van tibbles.
  "tidyr",          ## Gebruikt om data op te schonen in de tidverse omgeving.
  "utils",          ## Gebruikt voor utility functies
  "fst",            ## Gebruikt om bewerkingen te doen met grote data bestanden.
  "dplyr"          ## Gebruikt voor de dplyr omgeving.
)

## Laad de packages in de library
suppressMessages(purrr::walk(Basis_packages, ~library(.x, character.only = TRUE, warn.conflicts = FALSE)))


sa_packages <- c("vvcommander", "vvauditor", "vvmover", "vvconverter", "vvsculptor", "vusa")
walk(sa_packages, ~library(., character.only = TRUE, warn.conflicts = FALSE))

## +

## Ruim op
rm(Basis_packages, sa_packages)
