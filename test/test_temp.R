


test2 <- Inschrijvingen_1cho %>% tabyl(
   OPL_Fase, INS_Diploma_opleidingsfase)

test3 <- Inschrijvingen_1cho %>% tabyl(
  OPL_Fase, INS_Hoogste_vooropleiding_soort_1CHO)

test3_2020 <- Inschrijvingen_1cho %>%
  filter(INS_Inschrijvingsjaar >= 2020) %>%
  tabyl(OPL_Fase, INS_Hoogste_vooropleiding_soort_1CHO)

test3_2020_2 <- Inschrijvingen_1cho %>%
  group_by(INS_Studentnummer) %>%



  filter(INS_Eerste_jaar_opleiding_en_instelling != INS_Inschrijvingsjaar,
         INS_Eerste_jaar_opleiding_en_instelling >= 2020) %>%
  tabyl(OPL_Fase)

tabyl(Inschrijvingen_1cho$INS_Hoogste_vooropleiding_soort_1CHO)

test4 <- Inschrijvingen_1cho %>% tabyl(
  OPL_Fase, INS_Hoogste_vooropleiding_nieuw_cat)

tabyl(Inschrijvingen_1cho$INS_Hoogste_vooropleiding_BRIN_1CHO)
tabyl(Inschrijvingen_1cho$INS_Hoogste_vooropleiding_code_1CHO)
