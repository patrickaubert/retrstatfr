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

# ===================================================================================
# données des projections du COR

indiccor <- (sources_opendata %>% filter(producteur=="COR",reference=="rapport annuel"))$intitulecourt

tabindics <- lapply(indiccor,
                    function(x){
                      tab <- extrait_opendata(intitulecourt=x) %>% select(-sexe,-geo) %>% rename(scenario=x1)
                      names(tab) <- recode(names(tab),"valeurs"=x)
                      return(tab)
                      })
names(tabindics) <- indiccor

truc <-tabindics$pensmoynetterel


pensmoyrel <- extrait_opendata("pension moyenne relative") %>%
  select(-sexe,-geo) %>% rename(pensmoyrel=valeurs, scenario=x1)

pensrel <- extrait_opendata("pension moyenne relative") %>%
  select(-sexe,-geo) %>% rename(pensrel=valeurs, scenario=x1)


ratiocotretr <- extrait_opendata("rapport nombre cotisants sur retraités") %>%
  select(-sexe,-geo) %>% rename(ratiocotretr=valeurs, scenario=x1)

partretrpib <- extrait_opendata(intitulecourt="partretrpib") %>% #extrait_opendata("dépenses de retraite en pct du pib") %>%
  select(-sexe,-geo) %>% rename(partretrpib=valeurs, scenario=x1)

partressourcespib <- extrait_opendata(intitulecourt="partressourcespib") %>% #extrait_opendata("ressources de retraite en pct du pib") %>%
  select(-sexe,-geo) %>% rename(partressourcespib=valeurs, scenario=x1)

ageconjretr <- extrait_opendata(intitulecourt="ageconjretr") %>%
  select(-sexe,-geo,-x1) %>% rename(ageconjretr=valeurs) %>%
  filter(!is.na(ageconjretr)) %>% distinct()



