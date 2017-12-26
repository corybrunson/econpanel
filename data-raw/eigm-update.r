library(here)
library(rvest)
source(here("data-raw", "igm-utils.r"))

# load existing EIGM data
eigm_file <- here("data", "eigm.rda")
if (file.exists(eigm_file)) {
  load(eigm_file)
} else {
  stop("EIGM data, expected at '", eigm_file, "', was not found.")
}

# read new surveys
surveys <- read_surveys(panel = "Europe")

# append new surveys
eigm <- update_data(eigm, surveys)

# sort by date > question > panelist
eigm <- dplyr::arrange(eigm, date, question, panelist)

devtools::use_data(eigm, overwrite = TRUE)
