library(tidyverse)
# library(openxlsx)

# ===================================================================================
# sources open data
# ===================================================================================

sources_opendata <- read.csv2("data-raw/sources_open_data.csv", encoding="UTF-8")

usethis::use_data(sources_opendata, overwrite=TRUE)

# ===================================================================================
# taux de retraités


txretr <- extrait_opendata("txretr")
txretr <- txretr %>% rename(txretr=valeurs) %>% mutate(txretr = txretr/100)

txnvretr <- extrait_opendata("taux de nouveaux retraités par âge")
txnvretr <- txnvretr %>% rename(txnouvretr=valeurs,age=x1) %>% mutate(txnouvretr = txnouvretr/100)

txretr <- full_join(txretr,txnvretr, by=c("age","annee","sexe","geo"))

usethis::use_data(txretr, overwrite=TRUE)
