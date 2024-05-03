## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Student Analytics Vrije Universiteit Amsterdam
## Copyright 2023 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## View the Duo website where the link to the most recent Croho file can be found.
link_croho <- rvest::read_html("https://duo.nl/zakelijk/hoger-onderwijs/studentenadministratie/croho.jsp") %>%
  ## Select all <a> tags containing "ASCI"
  rvest::html_nodes(css = "a:contains(ASCI)") %>%
  ## Extract the "href" attribute from these <a> tags to get the link
  rvest::html_attr("href") %>%
  ## Prepend the protocol and domain to the link
  paste0("https://duo.nl", .)

## Determine the Croho network directory
Croho_network_dir <- "data/00_raw"
## Determine the location where the zip file will be stored, with today's date in the filename.
Croho_network_file_path <- paste0(Croho_network_dir, "/", lubridate::ymd(lubridate::today()), "_croho-ascii.zip")

## Download the file to the network drive
invisible(httr::GET(link_croho, httr::write_disk(Croho_network_file_path, overwrite = TRUE)))

## Unzip the file
unzip(Croho_network_file_path, files = "CrohoAct.txt", exdir = Croho_network_dir, overwrite = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## CLEAR #######
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vusa::clear_script_objects()
