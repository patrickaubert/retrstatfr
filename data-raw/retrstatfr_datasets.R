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

  # extraction de tous les noms des séries de données tirées du rapport du COR
indiccor <- (sources_opendata %>% filter(producteur=="COR",reference=="rapport annuel"))$intitulecourt

  # extraction de toutes les séries
tabindics <- lapply(indiccor,
                    function(x){
                      tab <- extrait_opendata(intitulecourt=x) %>%
                        select(-sexe,-geo) %>%
                        filter(!is.na(valeurs)) %>%
                        rename(scenario=x1) %>%
                        mutate(scenario = recode(
                          scenario %>% tolower() %>% trimws(),
                          "observé" = "obs",
                          "observée" = "obs",
                          "1.7999999999999999e-2"="+1,8%/an",
                          "1.4999999999999999e-2"="+1,5%/an",
                          "1.2999999999999999e-2"="+1,3%/an",
                          "0.01"="+1%/an"
                        ))
                      names(tab) <- recode(names(tab),"valeurs"=x)
                      return(tab)
                      })
names(tabindics) <- indiccor

  # identification des séries pour lesquelles les valeurs en projection sont identiques dans tous les scénarios
nbseriesscunique <- (do.call("bind_rows",tabindics) %>%
  pivot_longer(cols=-c("scenario","annee"),names_to="serie",values_to="val") %>%
  filter(!is.na(val)) %>%
  select(scenario,serie) %>%
  distinct() %>%
  group_by(serie) %>% summarize(nbsc=n()) %>% ungroup() %>%
  filter(nbsc<4))$serie

  # corrections de certaines séries (pb dans le fichier Excel initial)
derpartsalva <- tabindics[["partsalva"]] %>% filter(annee==max(annee))
projparsalva <- do.call("bind_rows",
                        replicate(2070-derpartsalva$annee,derpartsalva,simplify=FALSE)) %>%
  mutate(annee=derpartsalva$annee+c(1:n()))
tabindics[["partsalva"]] <- bind_rows(tabindics[["partsalva"]] ,projparsalva)

  # création de la table complète
for (i in c(1:NROW(indiccor))) {
  if (i==1) {projcor  <- tabindics[[i]]}
  else if ((i>1) & !(indiccor[i] %in% c(nbseriesscunique))) {projcor <- projcor %>% left_join(tabindics[[i]],by=c("scenario","annee"))}
  else if ((i>1) & (indiccor[i] %in% c(nbseriesscunique))) {projcor <- projcor %>% left_join(tabindics[[i]] %>% select(-scenario) %>% distinct(),by=c("annee"))}
}

usethis::use_data(projcor, overwrite=TRUE)



