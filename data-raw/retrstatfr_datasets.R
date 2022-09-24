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

# note : des données partielles sur les taux de retraités entre 54 et 59 ans, rétropolés entre 2000 et 2004 par le SGCOR à partir de l'EIR, sont disponibles dans les données de la figure 3.14 du rapport du COR de juin 2014

# ===================================================================================
# données des projections du COR

  # extraction de tous les noms des séries de données tirées du rapport du COR
indiccor <- sources_opendata %>%
  filter(producteur=="COR",reference=="rapport annuel") %>%
  select(intitulecourt,datepubli)

  # extraction de toutes les séries
tabindics <- map2(indiccor$intitulecourt,indiccor$datepubli,
                    function(x,y){
                      tab <- extrait_opendata(intitulecourt=x,datepubli=y) %>%
                        select(-sexe,-geo) %>%
                        filter(!is.na(valeurs)) %>%
                        rename(scenario=x1) %>%
                        mutate(scenario = recode(
                          scenario %>% tolower() %>% trimws(),
                          "observé" = "obs",
                          "observée" = "obs",
                          "observations" = "obs",
                          "projetée" = "tous scénarios",
                          "1.7999999999999999e-2"="+1,8%/an",
                          "1.4999999999999999e-2"="+1,5%/an",
                          "1.2999999999999999e-2"="+1,3%/an",
                          "1.6e-2"="+1,6%/an",
                          "7.0000000000000001e-3"="+0,7%/an",
                          "0.016"="+1,6%/an",
                          "0.013"="+1,3%/an",
                          "0.007"="+0,7%/an",
                          "0.01"="+1%/an"
                        ))
                      names(tab) <- recode(names(tab),"valeurs"=x)
                      return(tab)
                      })
#names(tabindics) <- indiccor

  # table mise en forme longue
tablong <- do.call("bind_rows",tabindics) %>%
  # correction à la main ...
  mutate(scenario = if_else(is.na(scenario) & !is.na(x2),x2,scenario) %>%
           recode("Obs" = "obs")) %>%
  select(-x2) %>%
  # mise en forme longue
  pivot_longer(cols=-c("scenario","annee","datepubli"),names_to="serie",values_to="val") %>%
  filter(!is.na(val))

# verif :
# unique(tablong$scenario )

# part des salaires dans la VA : on duplique jusqu'à l'horizon de projection la dernière valeur indiquée (supposée constante en projection)
tablong <- bind_rows(
  tablong %>% filter(serie!="partsalva" | scenario=="obs"),
  tablong %>% filter(serie=="partsalva" & scenario!="obs") %>%
    group_by(datepubli) %>% filter(annee<max(annee)) %>% ungroup(),
  tablong %>% filter(serie=="partsalva" & scenario!="obs") %>%
    group_by(datepubli) %>%
    filter(annee==max(annee)) %>%
    slice(rep(1:n(),each=2070-annee+1)) %>%
    mutate(annee=c(min(annee):2070)) %>%
    ungroup()
  ) %>%
  arrange(datepubli,serie,scenario,annee)

  # identification des séries pour lesquelles les valeurs en projection sont identiques dans tous les scénarios
nbseriesscunique <- (tablong %>%
  filter(scenario!="obs")  %>%
  select(scenario,serie,datepubli) %>%
  distinct() %>%
  group_by(serie,datepubli) %>% summarize(nbsc=n()) %>% ungroup() %>%
  filter(nbsc<4) %>%
  select(-nbsc))

  # duplication des projections en "tous scénarios" pour chacun des scénarios de projection
scenariosproj <- tablong %>% filter(!(scenario %in% c("obs","tous scénarios"))) %>%
  select(datepubli,scenario) %>% distinct() %>%
  rename(datepublisc=datepubli)

tablong <- bind_rows(
  tablong %>% filter(scenario=="obs"),
  tablong %>% filter(scenario!="obs") %>%
    anti_join(nbseriesscunique, by=c("datepubli","serie")),
  tablong %>% filter(scenario!="obs") %>%
    inner_join(nbseriesscunique, by=c("datepubli","serie")) %>%
    group_by(serie,datepubli,annee) %>%
    slice(rep(1:n(),each=4) ) %>%
    mutate(scenario = (scenariosproj %>% filter(datepublisc==min(datepubli)))$scenario ) %>%
    ungroup()
)

  # création de la table complète

#for (i in c(1:length(tabindics))) {
#  if (i==1) {projcor  <- tabindics[[i]]}
#  else if ((i>1) & !(indiccor[i] %in% c(nbseriesscunique))) {projcor <- projcor %>% left_join(tabindics[[i]],by=c("scenario","annee","datepubli"))}
#  else if ((i>1) & (indiccor[i] %in% c(nbseriesscunique))) {projcor <- projcor %>% left_join(tabindics[[i]] %>% select(-scenario) %>% distinct(),by=c("annee","datepubli"))}
#}

projcor <- tablong %>%
  pivot_wider(id_cols=c("datepubli","scenario","annee"),
              names_from="serie",values_from="val") %>%
  mutate(txprelevconvepr = if_else(
    is.na(txprelevconvepr) & (scenario=="obs"),txprelev,txprelevconvepr),
    txprelevconveec = if_else(
      is.na(txprelevconveec) & (scenario=="obs"),txprelev,txprelevconveec),
    partressourcespibconvepr = if_else(
      is.na(partressourcespibconvepr) & (scenario=="obs"),partressourcespib,partressourcespibconvepr),
    partressourcespibconveec = if_else(
      is.na(partressourcespibconveec) & (scenario=="obs"),partressourcespib,partressourcespibconveec) ) %>%
  arrange(datepubli,scenario,annee)

usethis::use_data(projcor, overwrite=TRUE)



