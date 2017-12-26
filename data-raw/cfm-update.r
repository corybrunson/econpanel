library(here)
library(rvest)
source(here("data-raw", "cfm-utils.r"))

# load existing CFM data
cfm_file <- here("data", "cfm.rda")
if (file.exists(cfm_file)) {
  load(cfm_file)
} else {
  stop("CFM data, expected at '", cfm_file, "', was not found.")
}

# get surveys
surveys <- get_surveys()

if (!exists("cfm")) {
  # scrape fresh
  cfm <- get_data(surveys)
} else {
  # scrape for updates
  cfm <- update_data(cfm, surveys)
}

# sort by date > question > panelist
cfm <- dplyr::arrange(cfm, date, question, participant)

devtools::use_data(cfm, overwrite = TRUE)
