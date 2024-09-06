Inschrijvingen_eencijfer <- read_file_proj("INS_eencijfer_enrollments")

# Uit 1cHO2021 zijn de volgende records en velden geselecteerd.
# Records:
#   - Inschrijvingsjaar >= 2002
# - Inschrijvingsjaar = Eerste jaar aan deze opleiding-instelling
# - Inschrijvingsvorm = S
# - Actuele instelling = 21PB, 21PC, 21PD, 21PE, 21PF, 21PG, 21PH, 21PI, 21PJ, 21PK, 21PL, 21PM, 21PN
# - Indicatie actief op peildatum = 1, 3
# - Soort inschrijving soort ho = 1, 2, 3, 4


Cohorten_eencijfer <- Inschrijvingen_eencijfer %>%
  filter(INS_Inschrijvingsjaar >= 2002,
         INS_Inschrijvingsjaar == INS_Inschrijvingsjaar_EOI,
         INS_Vorm == "S",
         INS_Instelling == "21PL",
         INS_Indicatie_actief_op_peildatum_code %in% c(1, 3),
         INS_Soort_inschrijving_soort_HO %in% c(1, 2, 3, 4))

# Records met gelijke waarden op de velden Persoonsgebonden nummer, Inschrijvingsjaar, Actuele
# instelling en Opleiding historisch equivalent zijn ontdubbeld met behulp van de velden Datum
# inschrijving, Opleidingsfase actueel, Opleidingsvorm en Opleiding actueel equivalent:
# 1.	Records met een vroegere Datum inschrijving gaan voor records met een latere Datum inschrijving,
# 2.	Bij gelijke Datum inschrijving is ontdubbeld op basis van Opleidingsfase actueel, waarbij de volgende
# prioritering is gehanteerd: S> D > P > B > K > I > M > Z > V 3.
# Bij gelijke Datum inschrijving en Opleidingsfase actueel is ontdubbeld op Opleidingsvorm, waarbij de volgende prioritering is
# gehanteerd: 1 > 2 > 3 4.
# Bij gelijke Datum inschrijving, Opleidingsfase actueel en Opleidingsvorm is ontdubbeld op
# Opleiding actueel equivalent, waarbij hogere opleidingsnummers voor lagere gaan.
# Als op basis van bovenstaande informatie niet ontdubbeld kon worden, is een willekeurig record
# gekozen.


Cohorten_eencijfer_gefilterd <- Cohorten_eencijfer %>%
  mutate(INS_Opleidingsfase_factor = factor(INS_Opleidingsfase_actueel_code,
                                    labels = c("S", "D", "P", "B", "K", "I", "M", "Z", "V"),
                                    levels = c("S", "D", "P", "B", "K", "I", "M", "Z", "V")),
         INS_Opleidingsvorm_factor = factor(INS_Opleidingsvorm_naam,
                                            labels = c("voltijd", "deeltijd", "duaal"),
                                            levels = c("voltijd", "deeltijd", "duaal"))) %>%
  group_by(INS_Studentnummer, INS_Inschrijvingsjaar, INS_Instelling, OPL_code_historisch) %>%
  arrange(INS_Studentnummer,
          INS_Datum_inschrijving,
          INS_Opleidingsfase_factor,
          INS_Opleidingsvorm_factor,
          desc(OPL_Code_in_jaar)) %>%
  slice_head() %>%
  ungroup()
