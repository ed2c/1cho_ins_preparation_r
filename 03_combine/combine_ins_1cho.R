
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

INS_Inschrijvingen_1CHO_VUdata <- read_file_proj("INS_Inschrijvingen_1CHO_VUdata")

INS_Cohorten <- read_file_proj("INS_Cohorten")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. SAMENVOEGEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

INS_Cohorten <- INS_Cohorten %>%
  filter(INS_Eerste_jaar_opleiding_en_instelling >= config::get("first_year")) %>%
  select(-OPL_Code_in_jaar)

INS_Inschrijvingen_1CHO_VUdata <- INS_Inschrijvingen_1CHO_VUdata %>%
  filter(INS_Eerste_jaar_opleiding_en_instelling >= config::get("first_year"))

INS_Inschrijvingen_combined <- INS_Inschrijvingen_1CHO_VUdata %>%
  left_join(INS_Cohorten,
            by = c("INS_Studentnummer",
                   "OPL_Code_actueel",
                   "INS_Eerste_jaar_opleiding_en_instelling"))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(INS_Inschrijvingen_combined)











## TODO move these checks into validation scripts
only_INS <- anti_join(INS_Inschrijvingen_1CHO_VUdata,
                  INS_Cohorten,
                  by = c("INS_Studentnummer",
                        "OPL_Code_actueel",
                        "INS_Eerste_jaar_opleiding_en_instelling"
                        ))

only_COH <- anti_join(INS_Cohorten,
                      INS_Inschrijvingen_1CHO_VUdata,
                      by = c("INS_Studentnummer",
                             "OPL_Code_actueel",
                             "INS_Eerste_jaar_opleiding_en_instelling"))

##'*INFO* Cohorten filtert altijd hierop. Daarbij worden aanvullende bewerkingen gedaan, zie
##' metadata/data_dictionary_start/Bestandbeschrijving EOIcohort_VSNU_1chHO2021.docx
only_INS_filtered <- only_INS %>%
  filter(INS_Eerste_jaar_opleiding_en_instelling == INS_Inschrijvingsjaar,
         INS_Indicatie_actief_op_peildatum %in% c(1, 3),
         INS_Soort_inschrijving_1CHO_code %in% c(1, 2, 3, 4),
         INS_Instelling == config::get("metadata_institution_name"))

## 491 Eerstejaars inschrijvingen blijven over (0,3%), met name bij opleidingen die (later)
## joint degree zijn (geworden).
# > clipr::write_clip(only_INS_filtered_count %>% arrange(desc(n))  %>% slice(1:10))
# INS_Opleidingsnaam_2002	OPL_Code_actueel	OPL_Code_in_jaar	n
# B Natuur- en Sterrenkunde	55013	56984	395
# M Physics	65016	60202	324
# M Chemistry	65012	66857	273
# B Scheikunde	55012	56857	170
# B Liberal Arts and Sciences (joint degree)	55002	55002	120
# M Computer Science (joint degree)	65014	65014	48
# M Chemistry (joint degree)	65012	65012	44
# M Bioinformatics and Systems Biology (joint degree)	65020	65020	39
# B Scheikunde (joint degree)	55012	55012	36
# B Geneeskunde	56551	56551	30


clear_script_objects()

