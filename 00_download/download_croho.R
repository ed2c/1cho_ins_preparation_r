## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Npuls CEDA (Centre for Educational Data Analytics)
## Web Page: https://edu.nl/twt84
## Contact: corneel.denhartogh@surf.nl
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Get the croho ascii file from DUO
link_croho <- rvest::read_html("https://duo.nl/zakelijk/hoger-onderwijs/studentenadministratie/croho.jsp") %>%
  rvest::html_nodes(css = "a:contains(ASCI)") %>%
  rvest::html_attr("href") %>%
  paste0("https://duo.nl", .)

# Determine the Croho network directory
raw_data_dir <- "data/00_raw"
# Determine the location where the zip file will be stored, with today's date in the filename.
file_path_zip <- paste0(raw_data_dir, "/", lubridate::ymd(lubridate::today()), "_croho-ascii.zip")

# Download the file to the network drive
invisible(httr::GET(link_croho, httr::write_disk(file_path_zip, overwrite = TRUE)))

# Unzip the file, it is cumulative so we overwrite is fine
unzip(file_path_zip, files = "CrohoAct.txt", exdir = raw_data_dir, overwrite = TRUE)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## CLEAR #######
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

clear_script_objects()
