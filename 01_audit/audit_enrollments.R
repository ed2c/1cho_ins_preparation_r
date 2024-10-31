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

enrollments <- read_csv(config::get("data_1cho_enrollments_file_path"),
                                     col_types = cols(
                                       .default = col_guess(),
                                       `Nationaliteit3` = col_double(),
                                       `DatumInschrijving` = col_date(format = "%Y-%m-%d")))

enrollments_naming <- read_documentation("Documentatie_enrollments.csv")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. ASSEERT ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# TODO Not yet able to run
# assert_naming(enrollments, enrollments_naming, "enrollments")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. MODIFY ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

enrollments <- wrapper_translate_colnames_documentation(
  enrollments,
  enrollments_naming
)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(enrollments)

clear_script_objects()

