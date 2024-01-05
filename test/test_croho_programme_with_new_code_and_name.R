
## Returns ACTUEEL only the last instance of a programme even when it has changed names?
test2 <- CROHO_per_jaar2 %>%
  filter(OPL_Instellingscode == "21PL",
         INS_Opleidingscode_actueel %in% c(56945, 56983))

test <- CROHO_per_jaar %>%
  filter(OPL_Instellingscode == "21PL",
         INS_Opleidingscode_actueel %in% c(56945, 56983)
  )


test <- CROHO_per_jaar2 %>%
  filter(OPL_Instellingscode == "21PL",
         INS_Opleidingscode_actueel %in% c(56945, 56983),
         Code_stand_record == "ACTUEEL")


test_oud <- CROHO_per_jaar %>%
  filter(OPL_Instellingscode == "21PL",
         INS_Opleidingscode_actueel %in% c(56945, 56983))
