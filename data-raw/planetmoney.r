# Data

# read CSV file
planetmoney <- read.csv("data-raw/planetmoney.csv", as.is = TRUE,
                        encoding = "UTF-8")

#saveRDS(planetmoney, "data/planetmoney.rds")
devtools::use_data(planetmoney, overwrite = TRUE)

rm(list = ls())
