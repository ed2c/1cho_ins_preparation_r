## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Manipuleren Inschrijvingen 1cho.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2023 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
## Doel: Doel
##
## Opmerkingen:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Inschrijvingen_1cho <- vvmover::read_file_proj("INS_Inschrijvingen_1CHO_VUdata")

dfTkoppel_Z08 <- readrds_csv(output = "2. Geprepareerde data/INS_Tkoppel_Z08.rds")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  ## Omzetten naar date zodat we de min en max datum kunnen vinden
  mutate(
    INS_Datum_inschrijving = as.Date(INS_Datum_inschrijving, format = "%d/%m/%Y", tryFormats = c("%d/%m/%Y", "%d-%m-%Y")),
    INS_Datum_uitschrijving = as.Date(INS_Datum_uitschrijving, format = "%d/%m/%Y", tryFormats = c("%d/%m/%Y", "%d-%m-%Y")),
  )

## TODO nu is instelling leeg, wellicht dat dit in toekomst anders is
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(INS_Bron = "1cho") %>%
  filter(
    INS_Instelling != "UvA" & INS_Instelling != "PThU",
    INS_Opleidingscode_actueel < 70000
  )

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(
    INS_Opleidingsvorm =
      recode(INS_Opleidingsvorm,
             "Voltijd" = 1,
             "Deeltijd" = 2,
             "Duaal" = 3
      )
  )

## Voeg Z08 opleidingscode en naam toe aan 1cho
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  select(
    -INS_Opleidingsnaam_2002,
    -INS_Faculteit
  ) %>%
  left_join(dfTkoppel_Z08, by = c("INS_Opleidingscode_actueel")) %>%
  mutate(
    INS_Opleidingsnaam_2002_oud = INS_Opleidingsnaam_2002,
    INS_Opleidingsnaam_2002 = INS_Opleidingsnaam_Z08
  )

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Verwijder duplicates ####

Inschrijvingen_1cho_duplicated <-
  Inschrijvingen_1cho[duplicated(Inschrijvingen_1cho[, c(
    "INS_Studentnummer",
    "INS_Inschrijvingsjaar",
    "INS_Opleidingscode_Z08"
  )]), ]

Inschrijvingen_1cho_duplicated <- Inschrijvingen_1cho_duplicated %>%
  arrange(INS_Studentnummer, INS_Inschrijvingsjaar)

Inschrijvingen_1cho_duplicated_reverse <-
  Inschrijvingen_1cho[duplicated(Inschrijvingen_1cho[, c(
    "INS_Studentnummer",
    "INS_Inschrijvingsjaar",
    "INS_Opleidingscode_Z08",
    "INS_Instelling"
  )], fromLast = TRUE), ]

Inschrijvingen_1cho_duplicated_reverse <- Inschrijvingen_1cho_duplicated_reverse %>%
  arrange(INS_Studentnummer, INS_Inschrijvingsjaar)

df_Inschrijvingen_duplicated <- bind_rows(Inschrijvingen_1cho_duplicated, Inschrijvingen_1cho_duplicated_reverse) %>%
  arrange(INS_Inschrijvingsjaar, INS_Opleidingscode_Z08, INS_Studentnummer)

## De duplicates hieronder hebben verschillen in oa de volgende variabelen:
## - Datum van in- en uitschrijving
##    - We zijn geinteresseerd in Indicatie actief op 1 okt = 1 (inschrijving)
##      Indien in de duplicates en 2 (uitgeschreven voor peildatum) en 3 (ingeschreven na peildatum)
##      voorkomen, willen we de rij met waarde 2 behouden en deze waarde vervangen in een 1 (inschrijving).
## - Soort inschrijving Ho: keuze voor de minimale code
## - Actief of peildatum : keuze voor de minimale code
## - Code beeindiging : keuze voor de minimale code

df_Inschrijvingen_duplicated <- df_Inschrijvingen_duplicated %>%
  group_by(INS_Studentnummer, INS_Opleidingscode_Z08, INS_Inschrijvingsjaar) %>%
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
    INS_Soort_inschrijving_1CHO = min(INS_Soort_inschrijving_1CHO)
  ) %>%
  ## Alleen de bovenste waarde meenemen
  slice(1) %>%
  ungroup()

## Verwijder alle waarden die duplicates hebben uit inschrijvigen 1cho en voeg bovenstaande niet duplicate set toe
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  anti_join(df_Inschrijvingen_duplicated, by = c(
    "INS_Studentnummer",
    "INS_Inschrijvingsjaar",
    "INS_Opleidingscode_Z08"
  )) %>%
  bind_rows(df_Inschrijvingen_duplicated)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## ORIGINEEL: Manipuleren Inschrijvingen Deel 2 - Mapping Tables.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Bepaal de scope van de dataset
## Bepaal hiervoor het maximale jaar dat in de data mag zitten, afhankelijk van
## de dag dat het script wordt uitgevoerd. Vanaf 1 oktober wordt het nieuwe
## inschrijvingsjaar toegevoegd
if (month(today()) < 10) {
  max_jaar <- year(today()) - 1
} else {
  max_jaar <- year(today())
}

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Om dubbele records alvast te verwijderen, wordt de functie "distinct"
## aangeroepen over de dataset.
Inschrijvingen_1cho <- distinct(Inschrijvingen_1cho)

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Verwijder alleen rijen uit het huidige en voorgaande inschrijvingsjaar.
## Als er alvast inschrijvingen voor een toekomstig inschrijvingjaar geleverd
## zijn worden succesvariabelen verstoord.

## Filter de rijen uit een toekomstig inschrijvingsjaar uit de data
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  filter(INS_Inschrijvingsjaar <= max_jaar)

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### Premaster goed zetten ####
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(INS_Premaster = recode(INS_Premaster,
                                "ja" = "P",
                                "nee" = NA_character_
  ))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Maak Indicatie variabelen Boolean ####
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  ## Maak de Indicatie variabelen met "J" en  "N" tot boolean variabelen
  mutate_at(vars(DEM_Indicatie_internationale_student, DEM_Indicatie_nationaliteit_EER_actueel,
                 DEM_Indicatie_nationaliteit_EER_peildatum), ~if_else(. == "J", TRUE, FALSE))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## sla variabele INS_Verblijfsjaren_hoger_onderwijs_origineel op ####

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(INS_Verblijfsjaren_hoger_onderwijs_origineel = INS_Verblijfsjaren_hoger_onderwijs)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## ORIGINEEL: Manipuleren Inschrijvingen Deel 2 - Mapping Tables.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Variabelen transformeren van ja/nee naar TRUE/FALSE
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(
    INS_Inclusief_UvA = vvconverter::transform_no_yes_to_ft(INS_Inclusief_UvA)
  )

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#### Mapping tables ####

## Invullen ontbrekende gegevens soort vooropleiding
## Vervang bij de vooropleiding codes de NA's door 0
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(
    INS_Vooropleiding_binnen_HO_code = replace_na(INS_Vooropleiding_binnen_HO_code, 0),
    INS_Vooropleiding_voor_HO_code = replace_na(INS_Vooropleiding_voor_HO_code, 0),
    INS_Hoogste_vooropleiding_code_1CHO = replace_na(INS_Hoogste_vooropleiding_code_1CHO, 0)
  )

Inschrijvingen_1cho$INS_Vooropleiding_binnen_HO_sector <- NULL
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Vooropleiding_binnen_HO_code",
  "INS_Vooropleiding_binnen_HO_sector",
  mapping_table_name = "Mapping_VOPL_code.csv"
)

Inschrijvingen_1cho$INS_Vooropleiding_voor_HO_profiel <- NULL
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Vooropleiding_voor_HO_code",
  "INS_Vooropleiding_voor_HO_profiel",
  mapping_table_name = "Mapping_VOPL_code.csv"
)

# The column already exists, so we replace
Inschrijvingen_1cho$INS_Vooropleiding_binnen_HO_soort <- NULL
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Vooropleiding_binnen_HO_code",
  "INS_Vooropleiding_binnen_HO_soort",
  mapping_table_name = "Mapping_VOPL_soort.csv"
)

Inschrijvingen_1cho$INS_Vooropleiding_voor_HO_soort <- NULL
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Vooropleiding_voor_HO_code",
  "INS_Vooropleiding_voor_HO_soort",
  mapping_table_name = "Mapping_VOPL_soort.csv"
)

Inschrijvingen_1cho$INS_Hoogste_vooropleiding_soort <- NULL
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Hoogste_vooropleiding_code_1CHO",
  "INS_Hoogste_vooropleiding_soort",
  mapping_table_name = "Mapping_VOPL_soort.csv"
)

Inschrijvingen_1cho$INS_Hoogste_vooropleiding_cat <- NULL
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Hoogste_vooropleiding_code_1CHO",
  "INS_Hoogste_vooropleiding_cat",
  mapping_table_name = "Mapping_VOPL_soort_cat.csv"
)

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#### BRIN data koppelen ####
# Naam van de Vooropleiding invullen gebaseerd op de BRIN nummer

Inschrijvingen_1cho$INS_Vooropleiding_voor_HO_BRIN_naam <- NULL
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Vooropleiding_voor_HO_BRIN",
  "INS_Vooropleiding_voor_HO_naam",
  mapping_table_name = "Mapping_VOPL_naam.csv"
)

Inschrijvingen_1cho$INS_Vooropleiding_binnen_HO_BRIN_naam <- NULL
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Vooropleiding_binnen_HO_BRIN",
  "INS_Vooropleiding_binnen_HO_naam",
  mapping_table_name = "Mapping_VOPL_naam.csv"
)

Inschrijvingen_1cho$INS_Hoogste_vooropleiding_naam <- NULL
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Hoogste_vooropleiding_BRIN_1CHO",
  "INS_Hoogste_vooropleiding_naam",
  mapping_table_name = "Mapping_VOPL_naam.csv"
)

Inschrijvingen_1cho$INS_Vooropleiding_voor_HO_postcode <- NULL
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Vooropleiding_voor_HO_BRIN",
  "INS_Vooropleiding_voor_HO_postcode",
  mapping_table_name = "Mapping_VOPL_Postcode.csv"
) %>% mutate(INS_Vooropleiding_voor_HO_postcode = as.double(INS_Vooropleiding_voor_HO_postcode))

Inschrijvingen_1cho$INS_Vooropleiding_binnen_HO_postcode <- NULL
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Vooropleiding_binnen_HO_BRIN",
  "INS_Vooropleiding_binnen_HO_postcode",
  mapping_table_name = "Mapping_VOPL_Postcode.csv"
) %>% mutate(INS_Vooropleiding_binnen_HO_postcode = as.double(INS_Vooropleiding_binnen_HO_postcode))

Inschrijvingen_1cho$INS_Hoogste_vooropleiding_postcode <- NULL
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Hoogste_vooropleiding_BRIN_1CHO",
  "INS_Hoogste_vooropleiding_postcode",
  mapping_table_name = "Mapping_VOPL_Postcode.csv"
) %>% mutate(INS_Hoogste_vooropleiding_postcode = as.double(INS_Hoogste_vooropleiding_postcode))

Inschrijvingen_1cho$INS_Vooropleiding_voor_HO_plaats <- NULL
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Vooropleiding_voor_HO_BRIN",
  "INS_Vooropleiding_voor_HO_plaats",
  mapping_table_name = "Mapping_VOPL_Plaats.csv"
)

Inschrijvingen_1cho$INS_Vooropleiding_binnen_HO_plaats <- NULL
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Vooropleiding_binnen_HO_BRIN",
  "INS_Vooropleiding_binnen_HO_plaats",
  mapping_table_name = "Mapping_VOPL_Plaats.csv"
)

Inschrijvingen_1cho$INS_Hoogste_vooropleiding_plaats <- NULL
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Hoogste_vooropleiding_BRIN_1CHO",
  "INS_Hoogste_vooropleiding_plaats",
  mapping_table_name = "Mapping_VOPL_Plaats.csv"
)

Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Code_examenresultaat",
  "INS_Examenresultaat"
)

Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Hoogste_vooropleiding_soort",
  "INS_Hoogste_vooropleiding_soort_cat"
)

Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Indicatie_eerste_jaars_instelling",
  "INS_Indicatie_eerste_jaars_instelling_cat"
)
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Indicatie_eerste_jaars_instelling",
  "INS_Indicatie_eerste_jaars_instelling_naam"
)
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Indicatie_eerste_jaars_opleiding_en_instelling",
  "INS_Indicatie_eerste_jaars_opleiding_en_instelling_naam"
)

Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "DEM_Nationaliteit_2",
  "DEM_Nationaliteit_2_naam",
  mapping_table_name = "Mapping_DEM_Nationaliteit.csv"
)
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "DEM_Nationaliteit_3",
  "DEM_Nationaliteit_3_naam",
  mapping_table_name = "Mapping_DEM_Nationaliteit.csv"
)

## Omdat de univariate plots per categorie werken, worden continue variabelen
## zoals leeftijd ook omgevormd tot categorische variabelen.
## Ook deze operatie is gedocumenteerd in de documentatie en
## gebruikt een mapping tabel.

Inschrijvingen_1cho <- mapping_category(
  Inschrijvingen_1cho,
  "DEM_Leeftijd_peildatum_1_jan",
  "DEM_Leeftijd_peildatum_1_jan_cat",
  mapping_table_name = "Mapping_DEM_Leeftijd_cat.csv"
)

Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Soort_inschrijving_1CHO",
  "INS_Soort_inschrijving_1CHO",
  mapping_table_name = "Mapping_Soort_inschrijving_1CHO.csv",
  KeepOriginal = FALSE
)

Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Vooropleiding_binnen_HO_BRIN",
  "INS_Vooropleiding_binnen_HO_Instellingsnaam",
  mapping_table_name = "Mapping_BRIN_Instellingsnaam.csv"
)

Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Vooropleiding_voor_HO_BRIN",
  "INS_Vooropleiding_voor_HO_Instellingsnaam",
  mapping_table_name = "Mapping_BRIN_Instellingsnaam.csv"
)

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#### INS_Verblijfsjaren, INS_TUssenjaar variabelen ####

## Maak variabelen DEM_Leeftijd_peildatum_1_oktober_Cat_new,
## INS_Verblijfsjaren_wetenschappelijk_onderwijs en
## INS_Verblijfsjaren_hoger_onderwijs
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(
    ## Waneer er geen verblijfsjaren zijn, betekent dit dat
    ## de inschrijving niet-actief is/opleiding telt niet mee
    INS_Verblijfsjaren_wetenschappelijk_onderwijs =
      if_else(INS_Verblijfsjaren_wetenschappelijk_onderwijs == 0,
              NA_integer_,
              as.integer(
                INS_Verblijfsjaren_wetenschappelijk_onderwijs
              )
      ),
    INS_Verblijfsjaren_hoger_onderwijs =
      if_else(INS_Verblijfsjaren_hoger_onderwijs == 0,
              NA_integer_,
              as.integer(INS_Verblijfsjaren_hoger_onderwijs)
      ),
    ## De definitie voor verblijfsjaren wordt aangepast,
    ## omdat het logischer is om vanaf 0 te tellen
    INS_Verblijfsjaren_wetenschappelijk_onderwijs =
      INS_Verblijfsjaren_wetenschappelijk_onderwijs - 1,
    INS_Verblijfsjaren_hoger_onderwijs =
      INS_Verblijfsjaren_hoger_onderwijs - 1
  )

## Deze variabele leidt tot heel veel categorieen omdat dit een aantal is.
## Voor de univariate plots maken we daarom een categorische variabele hiervan.
Inschrijvingen_1cho <- mapping_category(
  Inschrijvingen_1cho,
  "INS_Verblijfsjaren_wetenschappelijk_onderwijs",
  "INS_Verblijfsjaren_wetenschappelijk_onderwijs_cat",
  mapping_table_name = "Mapping_INS_Verblijfsjaren_wetenschappelijk_onderwijs_cat.csv"
)
Inschrijvingen_1cho <- mapping_category(
  Inschrijvingen_1cho,
  "INS_Verblijfsjaren_hoger_onderwijs",
  "INS_Verblijfsjaren_hoger_onderwijs_cat",
  mapping_table_name = "Mapping_INS_Verblijfsjaren_wetenschappelijk_onderwijs_cat.csv"
)

## Bepaal ahv INS_Postcode_student_1okt_peildatum en INS_Postcode_student_voor_HO of een student
## uitwonend is: als de velden gelijk zijn voor een student is deze thuiswonend, als ze verschillend
## zijn uitwonend
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(INS_Uitwonend = if_else(INS_Postcode_student_1okt_peildatum == INS_Postcode_student_voor_HO,
                                 FALSE,
                                 TRUE
  ))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## ORIGINEEL: Manipuleren Inschrijvingen Deel 3 - Overig.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(
    ## Eerst verwijderen we van deze variabele enkele voorvoegsels:
    INS_Vooropleiding_voor_HO_profiel_standaard =
      str_replace_all(
        INS_Vooropleiding_voor_HO_profiel,
        c("vwo profiel |havo |algemeen|havo profiel |profiel "),
        ""
      ),
    ## Daarna kijken we of de bekende profielen voorkomen, en anders voegen
    ## we op dit moment een missende waarde toe.
    INS_Vooropleiding_voor_HO_profiel_standaard = if_else(str_detect(
      INS_Vooropleiding_voor_HO_profiel_standaard,
      "cultuur|economie|natuur"
    ),
    INS_Vooropleiding_voor_HO_profiel_standaard,
    NA_character_
    ),
    ## Omdat er nog enkele verkeerde records overblijven, worden deze apart
    ## nog verwijderd in een derde stap:
    INS_Vooropleiding_voor_HO_profiel_standaard = if_else(str_detect(
      INS_Vooropleiding_voor_HO_profiel_standaard, "mbo|vmbo|vbo"
    ),
    NA_character_,
    INS_Vooropleiding_voor_HO_profiel_standaard
    ),
    ## Een extra variabele wordt aangemaakt, waarbij alleen voor
    ## VWO profielen deze ingevuld wordt. In de standaardvariabele staan ook
    ## de HAVO profielen, waar beschikbaar.
    INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO =
      if_else(str_detect(INS_Vooropleiding_voor_HO_profiel, "vwo"),
              INS_Vooropleiding_voor_HO_profiel_standaard, NA_character_
      ),
    ## Per profiel los (voor modelleren vooral relevant, deze features
    ## kunnen beter begrepen worden)
    INS_Vooropleiding_voor_HO_profiel_standaard_NT =
      str_detect(INS_Vooropleiding_voor_HO_profiel, "techniek"),
    INS_Vooropleiding_voor_HO_profiel_standaard_NG =
      str_detect(INS_Vooropleiding_voor_HO_profiel, "gezondheid"),
    INS_Vooropleiding_voor_HO_profiel_standaard_EM =
      str_detect(INS_Vooropleiding_voor_HO_profiel, "maatschappij"),
    INS_Vooropleiding_voor_HO_profiel_standaard_CM =
      str_detect(INS_Vooropleiding_voor_HO_profiel, "cultuur"),
    ## Combinatieprofiel los definieeren
    ## (of een student twee profielen heeft gedaan)
    INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_combinatieprofiel =
      str_detect(INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO, "/")
  )

## Profielen omschrijven naar afkortingen
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Vooropleiding_voor_HO_profiel_standaard",
  "INS_Vooropleiding_voor_HO_profiel_standaard",
  mapping_table_name = "Mapping_INS_Vooropleiding_voor_HO_profiel_standaard.csv",
  KeepOriginal = FALSE
)
Inschrijvingen_1cho <- mapping_translate(
  Inschrijvingen_1cho,
  "INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO",
  "INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO",
  mapping_table_name = "Mapping_INS_Vooropleiding_voor_HO_profiel_standaard.csv",
  KeepOriginal = FALSE
)

## Maak variabele INS_Vooropleiding_voor_HO_profiel_standaard_zonder_combinatie,
## deze variabele bevat de veel voorkomende variaties
## (dus niet: natuur en maatschappij)
Inschrijvingen_1cho <- Inschrijvingen_1cho %>%
  mutate(
    ## Variabele voor VWO en HAVO
    INS_Vooropleiding_voor_HO_profiel_standaard_zonder_combinatie =
      factor(
        if_else(INS_Vooropleiding_voor_HO_profiel_standaard %in%
                  c(
                    "NG & CM",
                    "NG & EM",
                    "NT & EM",
                    "NT & CM"
                  ),
                NA_character_,
                as.character(
                  INS_Vooropleiding_voor_HO_profiel_standaard
                )
        ),
        levels = c(
          "NT",
          "NG",
          "NT & NG",
          "EM", "CM",
          "EM & CM"
        )
      ),
    ## Variabele alleen vWO
    INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_zonder_combinatie =
      factor(
        if_else(
          INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO %in%
            c(
              "NG & CM",
              "NG & EM",
              "NT & EM",
              "NT & CM"
            ),
          NA_character_,
          as.character(
            INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO
          )
        ),
        levels = c(
          "NT",
          "NG",
          "NT & NG",
          "EM",
          "CM",
          "EM & CM"
        )
      )
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vvmover::write_file_proj(Inschrijvingen_1cho, "INS_Inschrijvingen_1CHO_VUdata")

vvcommander::clear_script_objects()
