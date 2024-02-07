


INS_Inschrijvingen_1CHO_VUdata <- read_file_proj("INS_Inschrijvingen_1CHO_VUdata")
#CROHO_per_jaar <- read_file_proj("CROHO_per_jaar")
INS_Cohorten <- read_file_proj("INS_Cohorten")
#
#
#
# tabyl(INS_Inschrijvingen_1CHO_VUdata$INS_Faculteit)
#
# test <- INS_Inschrijvingen_1CHO_VUdata %>% filter(INS_Opleidingscode_actueel %in% c(56945, 56983))
#
# ## Moet eigenlijk gefixed worden bij fix voor corrigeren 1cho
# mutate(INS_Opleidingscode_actueel = recode(INS_Opleidingscode_actueel, "56945" = 56983))


INS_Cohorten <- INS_Cohorten %>%
  filter(INS_Eerste_jaar_opleiding_en_instelling >= 2010)

INS_Inschrijvingen_1CHO_VUdata <- INS_Inschrijvingen_1CHO_VUdata %>%
  filter(INS_Eerste_jaar_opleiding_en_instelling >= 2010)

INS_Inschrijvingen_1CHO_VUdata <- INS_Inschrijvingen_1CHO_VUdata %>%
  filter(INS_Indicatie_actief_op_peildatum %in% c(1, 3))


only_INS <- anti_join(INS_Inschrijvingen_1CHO_VUdata,
                  INS_Cohorten,
                  by = c("INS_Studentnummer"#,
                        #"OPL_Code_actueel",
                         #"INS_Eerste_jaar_opleiding_en_instelling"
                        ))

only_COH <- anti_join(INS_Cohorten,
                      INS_Inschrijvingen_1CHO_VUdata,
                      by = c(#"INS_Studentnummer",
                             "OPL_Code_actueel",
                             "INS_Eerste_jaar_opleiding_en_instelling"))

## TODO joint degrees en namen
only_INS_joint <- only_INS %>%
  count(
    INS_Opleidingsnaam_2002,
    OPL_Code_actueel,
    OPL_Code_in_jaar,
    INS_Eerste_jaar_opleiding_en_instelling#,
    #INS_Inschrijvingsjaar
  )

only_COH_joint <- only_COH %>%
  count(
    OPL_Naam_in_jaar,
    OPL_Code_actueel,
    OPL_Code_in_jaar,
    INS_Eerste_jaar_opleiding_en_instelling#,
    #INS_Inschrijvingsjaar
  )


## Bij INS_Inschrijvingen wordt de code door de jaren heen geupdate. Daardoor werkt de join niet goed
Biomed_INS_Cohorten <- INS_Cohorten %>%
  filter(OPL_Naam_in_jaar == "B Biomedical Sciences") %>%
  count(INS_Eerste_jaar_opleiding_en_instelling, OPL_Code_actueel)

Biomed2_INS_Cohorten <- INS_Cohorten %>%
  filter(str_detect(OPL_Naam_in_jaar, "B Biomedisch")) %>%
  count(INS_Eerste_jaar_opleiding_en_instelling, OPL_Code_actueel)

Biomed_INS_Inschrijvingen_1CHO_VUdata <- INS_Inschrijvingen_1CHO_VUdata %>%
  filter(INS_Opleidingsnaam_2002 == "B Biomedical Sciences") %>%
  #filter(INS_Eerste_jaar_opleiding_en_instelling == INS_Inschrijvingsjaar) %>%
  count(INS_Eerste_jaar_opleiding_en_instelling, INS_Opleidingscode_actueel, INS_Opleidingscode_actueel_eerste_jaar)

Biomed2_INS_Inschrijvingen_1CHO_VUdata <- INS_Inschrijvingen_1CHO_VUdata %>%
  filter(str_detect(INS_Opleidingsnaam_2002, "B Biomedisch")) %>%
  filter(INS_Eerste_jaar_opleiding_en_instelling == INS_Inschrijvingsjaar) %>%
  count(INS_Eerste_jaar_opleiding_en_instelling, INS_Opleidingscode_actueel, INS_Opleidingscode_actueel_eerste_jaar)

## B Artificial Intelligence 2018?

only_INS_count <- only_INS %>%
  count(INS_Opleidingsnaam_2002, INS_Eerste_jaar_opleiding_en_instelling) %>%
  arrange(desc(n))

only_COH_count <- only_COH %>%
  count(OPL_Code_in_jaar, OPL_Naam_in_jaar, INS_Eerste_jaar_opleiding_en_instelling) %>%
  arrange(desc(n))

only_COH_count_no_joint <- only_COH_count %>%
  filter(!str_detect(OPL_Naam_in_jaar, "joint"))

test$INS_Opleidingsnaam_2002) %>% arrange(desc(n))
