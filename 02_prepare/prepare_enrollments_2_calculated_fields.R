## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Npuls CEDA (Centre for Educational Data Analytics)
## Web Page: https://edu.nl/twt84
## Contact: corneel.denhartogh@surf.nl
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. READ ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

enrollments_start <- read_file_proj("enrollments_1",
                                            dir = "02_prepared"
)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. MODIFY ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.1 Directly derived ####

# Determine based on INS_Postcode_student_1okt_peildatum and INS_Postcode_student_voor_HO whether a student
# is living away from home: if the fields are equal for a student they are living at home, if they are different
# they are living away from home
enrollments <- enrollments_start %>%
  mutate(INS_Uitwonend = if_else(INS_Postcode_student_1okt_peildatum == INS_Postcode_student_voor_HO,
                                 FALSE,
                                 TRUE
  ))

enrollments <- enrollments %>%
  mutate(INS_Indicatie_voltijd = if_else(INS_Opleidingsvorm_code == 1,
                                         TRUE, FALSE
  ))

# Determine double study institution. Use dates to filter transitions and switchers
# (E.g.: completion B & start M in one year or deregistration before Feb 1 and registration Feb 1)
# Use OPL_Code Actueel unique to filter joint degrees
enrollments <- enrollments %>%
  group_by(INS_Studentnummer, INS_Inschrijvingsjaar, INS_Datum_inschrijving, INS_Datum_uitschrijving) %>%
  mutate(
    INS_Aantal_inschrijvingen_jaar_instelling = length(unique(OPL_code_historisch)),
    INS_Aantal_EOI_inschrijvingen_jaar_instelling = sum(
      INS_Indicatie_eerste_jaars_opleiding_en_instelling == 1
    )
  ) %>%
  ungroup() %>%
  mutate(INS_Dubbele_studie_instelling = INS_Aantal_inschrijvingen_jaar_instelling > 1)

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.2. Secondary education profiles ####

enrollments <- enrollments %>%
  mutate(
    # First we remove some prefixes from this variable:
    INS_Vooropleiding_voor_HO_profiel_standaard =
      str_replace_all(
        INS_Vooropleiding_voor_HO_profiel,
        c("vwo profiel |havo |algemeen|havo profiel |profiel "),
        ""
      ),
    # Then we check if the known profiles appear, and otherwise we add
    # a missing value at this point.
    INS_Vooropleiding_voor_HO_profiel_standaard = if_else(str_detect(
      INS_Vooropleiding_voor_HO_profiel_standaard,
      "cultuur|economie|natuur"
    ),
    INS_Vooropleiding_voor_HO_profiel_standaard,
    NA_character_
    ),
    # Because some incorrect records remain, these are separately
    # removed in a third step:
    INS_Vooropleiding_voor_HO_profiel_standaard = if_else(str_detect(
      INS_Vooropleiding_voor_HO_profiel_standaard, "mbo|vmbo|vbo"
    ),
    NA_character_,
    INS_Vooropleiding_voor_HO_profiel_standaard
    ),
    # An extra variable is created, where only for
    # VWO profiles this is filled in. The standard variable also contains
    # the HAVO profiles, where available.
    INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO =
      if_else(str_detect(INS_Vooropleiding_voor_HO_profiel, "vwo"),
              INS_Vooropleiding_voor_HO_profiel_standaard, NA_character_
      ),
    # Per profile separately (mainly relevant for modeling, these features
    # can be better understood)
    INS_Vooropleiding_voor_HO_profiel_standaard_NT =
      str_detect(INS_Vooropleiding_voor_HO_profiel, "techniek"),
    INS_Vooropleiding_voor_HO_profiel_standaard_NG =
      str_detect(INS_Vooropleiding_voor_HO_profiel, "gezondheid"),
    INS_Vooropleiding_voor_HO_profiel_standaard_EM =
      str_detect(INS_Vooropleiding_voor_HO_profiel, "maatschappij"),
    INS_Vooropleiding_voor_HO_profiel_standaard_CM =
      str_detect(INS_Vooropleiding_voor_HO_profiel, "cultuur"),
    # Define combination profile separately
    # (whether a student has done two profiles)
    INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_combinatieprofiel =
      str_detect(INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO, "/")
  )

# Describe profiles in abbreviations
enrollments <- enrollments %>%
  mapping_translate(
    "INS_Vooropleiding_voor_HO_profiel_standaard",
    "INS_Vooropleiding_voor_HO_profiel_standaard_afk",
    mapping_table_name = "Mapping_INS_Profiel_omschrijving_Profiel_afkorting"
  )
enrollments <- enrollments %>%
  mapping_translate(
    "INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO",
    "INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_afk",
    mapping_table_name = "Mapping_INS_Profiel_omschrijving_Profiel_afkorting"
  )

# Create variable INS_Vooropleiding_voor_HO_profiel_standaard_zonder_combinatie,
# this variable contains the common variations and thus not: combinations of nature and society

# Create helper object for profiles, see Style Guide Principle F, self-documenting code
vProfielen_levels <- c("NT",
                       "NG",
                       "NT & NG",
                       "EM", "CM",
                       "EM & CM")

vProfielen_genegeerd <- c("NG & CM",
                          "NG & EM",
                          "NT & EM",
                          "NT & CM")

enrollments <- enrollments %>%
  mutate(
    # Variable for VWO and HAVO
    INS_Vooropleiding_voor_HO_profiel_standaard_zonder_combinatie = if_else(
      INS_Vooropleiding_voor_HO_profiel_standaard %in% vProfielen_genegeerd,
      NA_character_,
      INS_Vooropleiding_voor_HO_profiel_standaard
    ),
    # Make factor
    INS_Vooropleiding_voor_HO_profiel_standaard_zonder_combinatie = factor(
      INS_Vooropleiding_voor_HO_profiel_standaard_zonder_combinatie,
      levels = vProfielen_levels
    ),
    # Variable only VWO
    INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_zonder_combinatie = if_else(
      INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO %in% vProfielen_genegeerd,
      NA_character_,
      INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO
    ),
    # Make factor
    INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_zonder_combinatie = factor(
      INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_zonder_combinatie,
      levels = vProfielen_levels
    )
  )


## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.3 Study year & Deregistration Feb 1 ####

# TODO adjust logic
enrollments <- enrollments %>%
  # Study year is calculated per student-programme combination
  group_by(
    INS_Studentnummer,
    OPL_code_historisch
  ) %>%
  # Sort the file by enrollment year so the study year can be determined
  arrange(
    INS_Studentnummer,
    OPL_code_historisch,
    INS_Inschrijvingsjaar
  ) %>%
  mutate(
    # Calculate INS_Studiejaar based on position in list of unique Enrollment years (this works because
    # it's arranged above)
    INS_Studiejaar = match(INS_Inschrijvingsjaar, unique(INS_Inschrijvingsjaar)),
    INS_Tussenjaren_binnen_opleiding = sum(!((min(INS_Inschrijvingsjaar):max(INS_Inschrijvingsjaar)) %in% INS_Inschrijvingsjaar)),
    # Create a boolean variable to indicate whether the
    # enrollment year is the EOI year
    INS_Inschrijvingsjaar_is_EOI =
      INS_Inschrijvingsjaar == INS_Inschrijvingsjaar_EOI
  ) %>%
  ungroup()

enrollments <- enrollments %>%
  mutate(
    # Determine if the deregistration in the same academic year
    # was before February 1
    INS_Uitschrijving_voor_1_feb = INS_Datum_uitschrijving <= as_date(
      paste0(INS_Inschrijvingsjaar + 1, "-01-31")
    ),
    # Determine if this deregistration was an EOI. Only in this case is
    # the BSA avoided
    INS_Uitschrijving_voor_1_feb_EOI =
      INS_Inschrijvingsjaar_EOI ==
      INS_Inschrijvingsjaar &
      INS_Uitschrijving_voor_1_feb
  ) %>%
  # Group by student/programme
  group_by(INS_Studentnummer, OPL_code_historisch) %>%
  mutate(
    INS_Herinschrijving_jaar_2_na_uitschrijving_voor_1_feb =
      case_when(
        # If the student has no TRUE in any year on the
        # variable INS_EOI_uitschrijving_voor_1_feb, it becomes
        # FALSE
        sum(INS_Uitschrijving_voor_1_feb_EOI) == 0 ~ FALSE,
        # For all remaining enrollments there is a case of
        # avoiding the BSA. If the maximum study year is greater
        # than 1, this means there has been a re-enrollment
        max(INS_Studiejaar) > 1 ~ TRUE,
        # in all other cases this is not the case.
        .default = FALSE
      )
  ) %>%
  ungroup() %>%
  mutate(INS_Studiejaar_gecorrigeerd_uitschrijving_1_feb_EOI = case_when(
    INS_Herinschrijving_jaar_2_na_uitschrijving_voor_1_feb == TRUE & INS_Studiejaar > 1 ~ INS_Studiejaar - 1,
    .default = INS_Studiejaar
  ))


## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.4 Gap years ####

enrollments <- enrollments %>%
  # Create variable INS_Tussenjaren_voor_M for students with one or more
  # gap years between their bachelor and master
  mutate(
    # Create variable to see if enrollment is direct
    INS_Direct = (
      INS_Hoogste_vooropleiding_jaar_1CHO + 1) == INS_Inschrijvingsjaar_EOI,
    # Create variable INS_Indicatie_Tussenjaar_voor_B for students with a
    # gap year before their first year in higher education
    INS_Indicatie_Tussenjaar_voor_B = (
      INS_Hoogste_vooropleiding_jaar_1CHO + 1 < INS_Inschrijvingsjaar_EOI &
        OPL_Fase == "B" &
        INS_Verblijfsjaren_wetenschappelijk_onderwijs == 1 &
        INS_Verblijfsjaren_hoger_onderwijs == 1),
    # Create variable INS_Indicatie_Tussenjaar_voor_P for students with one or more
    # gap years between their bachelor and premaster
    INS_Indicatie_Tussenjaar_voor_P = (
      INS_Hoogste_vooropleiding_jaar_1CHO + 1 < INS_Inschrijvingsjaar_EOI &
        OPL_Fase == "S" &
        # Student hasn't started premaster phase in this year
        INS_Verblijfsjaar_type_onderwijs_binnen_HO <= 1),
    INS_Indicatie_Tussenjaar_voor_M = (
      INS_Hoogste_vooropleiding_jaar_1CHO + 1 < INS_Inschrijvingsjaar_EOI &
        OPL_Fase == "M" &
        # Student hasn't done an M before
        INS_Verblijfsjaar_type_onderwijs_binnen_HO <= 1
    )
  )

# Generalize the variables for all enrollments of the student in the programme
enrollments <- enrollments  %>%
  group_by(INS_Studentnummer, OPL_code_historisch) %>%
  mutate(
    INS_Indicatie_Tussenjaar_voor_B = INS_Indicatie_Tussenjaar_voor_B[first(which(INS_Studiejaar == 1))],
    INS_Indicatie_Tussenjaar_voor_P = INS_Indicatie_Tussenjaar_voor_P[first(which(INS_Studiejaar == 1))],
    INS_Indicatie_Tussenjaar_voor_M = INS_Indicatie_Tussenjaar_voor_M[first(which(INS_Studiejaar == 1))],
    INS_Direct = INS_Direct[first(which(INS_Studiejaar == 1))]
  ) %>%
  ungroup() %>%
  mutate(INS_Indicatie_Tussenjaar = coalesce(INS_Indicatie_Tussenjaar_voor_B,
                                             INS_Indicatie_Tussenjaar_voor_P,
                                             INS_Indicatie_Tussenjaar_voor_M))


## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.5 Connection ####
enrollments <- enrollments %>%
  mutate(
    # Determine connection based on direct and gap year. Difference between
    # internal and external switch is determined later
    INS_Aansluiting =
      case_when(
        INS_Direct &
          INS_Hoogste_vooropleiding_BRIN_1CHO == config::get("metadata_institution_BRIN") ~
          "Direct after diploma institution",
        INS_Direct ~ "Direct after external diploma",
        INS_Indicatie_Tussenjaar == TRUE ~ "Gap year",
        #SUC_Instroom_switch_VU == TRUE ~ "Switch within VU",
        INS_Verblijfsjaar_type_onderwijs_binnen_HO > INS_Studiejaar ~ "Switch / Second study",
        .default = "Unknown"
      )
  )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(enrollments)

clear_script_objects()
