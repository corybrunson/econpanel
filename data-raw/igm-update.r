library(here)
library(rvest)
source(here("data-raw", "igm-utils.r"))

# load existing IGM data
igm_file <- here("data", "igm.rda")
if (file.exists(igm_file)) {
  load(igm_file)
} else {
  stop("IGM data, expected at '", igm_file, "', was not found.")
}

# read new surveys
surveys <- read_surveys(panel = "USA")

# append new surveys
igm <- update_data(igm, surveys)

# sort by date > question > panelist
igm <- dplyr::arrange(igm, date, question, panelist)

devtools::use_data(igm, overwrite = TRUE)
