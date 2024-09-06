## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Npuls CEDA (Centre for Educational Data Analytics)
## Web Page: https://edu.nl/twt84
## Contact: corneel@surf.nl
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. READ ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

eencijfer_enrollments <- read_csv("data/00_raw/2023/EV299XX24.csv",
                                     col_types = cols(
                                       .default = col_guess(),
                                       `Nationaliteit3` = col_double(),
                                       `DatumInschrijving` = col_date(format = "%Y-%m-%d")))

eencijfer_naming <- read_documentation("Documentatie_eencijfer_enrollments_package.csv")

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. ASSERTIONS ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# TODO Not yet able to run
# assert_naming(eencijfer_enrollments, eencijfer_naming, "eencijfer_enrollments")

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# TODO test code
#
# test <- create_documentatie(eencijfer_enrollments, "Documentatie_eencijfer_enrollments.csv")
#
# write_csv2(test, "metadata/assertions/Documentatie_eencijfer_enrollments.csv")

eencijfer_enrollments <- wrapper_translate_colnames_documentation(
  eencijfer_enrollments,
  eencijfer_naming
)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(eencijfer_enrollments, "INS_eencijfer_enrollments")

clear_script_objects()

