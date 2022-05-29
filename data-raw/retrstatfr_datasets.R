library(tidyverse)
# library(openxlsx)

# ===================================================================================
# sources open data
# ===================================================================================

sources_opendata <- read.csv2("data-raw/sources_open_data.csv", encoding="UTF-8")

usethis::use_data(sources_opendata, overwrite=TRUE)

