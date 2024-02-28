library(tidyverse)
# library(openxlsx)
library(devtools)
# devtools::load_all()

# ===================================================================================
# sources open data
# ===================================================================================

sources_opendata <- read.csv2("data-raw/sources_open_data.csv", encoding="UTF-8")

usethis::use_data(sources_opendata, overwrite=TRUE)

# ===================================================================================
# taux de retraités


txretr <- extrait_opendata("txretr")
txretr <- txretr %>% filter(datepubli==max(datepubli)) %>% rename(txretr=valeurs) %>% mutate(txretr = txretr/100)

txnvretr <- extrait_opendata("taux de nouveaux retraités par âge")
txnvretr <- txnvretr %>% filter(datepubli==max(datepubli)) %>% rename(txnouvretr=valeurs,age=x1) %>% mutate(txnouvretr = txnouvretr/100)

txretr <- full_join(txretr,txnvretr, by=c("age","annee","sexe","geo","datepubli"))

usethis::use_data(txretr, overwrite=TRUE)

# note : des données partielles sur les taux de retraités entre 54 et 59 ans, rétropolés entre 2000 et 2004 par le SGCOR à partir de l'EIR, sont disponibles dans les données de la figure 3.14 du rapport du COR de juin 2014

# ===================================================================================
# données des projections du COR

  # extraction de tous les noms des séries de données tirées du rapport du COR
indiccor <- sources_opendata %>%
  filter(producteur=="COR",reference=="rapport annuel") %>%
  select(intitulecourt,datepubli)

  # list des divers noms de scenarios (yc pb de format de lecture)
list_noms_sc <- list(
  "observé" = "obs",
  "observée" = "obs",
  "observations" = "obs",
  "Obs" = "obs",
  "projetée" = "tous scénarios",
  "1.7999999999999999e-2"="+1,8%/an",
  "1.4999999999999999e-2"="+1,5%/an",
  "1.2999999999999999e-2"="+1,3%/an",
  "1.7999999999999999E-2"="+1,8%/an",
  "1.4999999999999999E-2"="+1,5%/an",
  "1.2999999999999999E-2"="+1,3%/an",
  "1.6e-2"="+1,6%/an",
  "7.0000000000000001e-3"="+0,7%/an",
  "0.02"="+2%/an",
  "0.016"="+1,6%/an",
  "0.013"="+1,3%/an",
  "0.007"="+0,7%/an",
  "0.01"="+1%/an",
  "a'"="+2%/an",
  "a"="+1,8%/an",
  "b"="+1,5%/an",
  "c"="+1,3%/an",
  "c'"="+1%/an",
  "2%"="+2%/an",
  "1,8%"="+1,8%/an",
  "1,5%"="+1,5%/an",
  "1,3%"="+1,3%/an",
  "1%"="+1%/an"
)

  # fonction pour l'extraction des projections du cor
extrait_projcor <- function(intitule,date){
  tab <- extrait_opendata(intitulecourt=intitule,datepubli=date) %>%
    select(-sexe,-geo) %>%
    filter(!is.na(valeurs)) %>%
    rename(scenario=x1) %>%
    mutate(scenario = recode(
      scenario %>% tolower() %>% trimws(),
      !!! list_noms_sc
    ))
  names(tab) <- recode(names(tab),"valeurs"=intitule)
  return(tab)
}

  # extraction de toutes les séries
#tabindics <- map2(indiccor$intitulecourt,indiccor$datepubli, extrait_projcor)
#names(tabindics) <- indiccor
tabindics <- list()
for (i in c(1:nrow(indiccor))){
# for (i in c(56:67)){
  tabindics[[paste0(indiccor$intitulecourt[i],paste0(indiccor$datepubli[i]))]] <- extrait_projcor(indiccor$intitulecourt[i],paste0(indiccor$datepubli[i]))
}

  # table mise en forme longue
tablong <- do.call("bind_rows",tabindics) %>%
  mutate(scenario = scenario %>%
            str_replace("^(scénario|variante|gains de productivité annuels tendanciels de) ","") %>%
            str_replace_all("(?<=[[:digit:]])[[:space:]](?=%)","") %>%
            recode(!!! list_noms_sc))
if ("x2" %in% names(tablong)){
  # correction à la main ...
  tablong <- tablong  %>%
    mutate(
      x2 = x2 %>%
        str_replace("^(scénario|variante|gains de productivité annuels tendanciels de) ","") %>%
        str_replace_all("(?<=[[:digit:]])[[:space:]](?=%)","") %>%
        recode(!!! list_noms_sc),
      scenario = case_when(
      !is.na(scenario) & !is.na(x2) & !(scenario %in% unlist(list_noms_sc)) & (x2 %in% unlist(list_noms_sc)) ~ x2,
      !is.na(scenario) ~ scenario,
      !is.na(x2) ~ x2,
      TRUE ~ scenario) )
}
if ("x3" %in% names(tablong)){
  tablong <- tablong  %>%
    mutate(
      x3 = x3 %>%
        str_replace("^(scénario|variante|gains de productivité annuels tendanciels de) ","") %>%
        str_replace_all("(?<=[[:digit:]])[[:space:]](?=%)","") %>%
        recode(!!! list_noms_sc),
      scenario = case_when(
        !is.na(scenario) & !is.na(x3) & !(scenario %in% unlist(list_noms_sc)) & (x3 %in% unlist(list_noms_sc)) ~ x3,
        !is.na(scenario) ~ scenario,
        !is.na(x3) ~ x3,
      TRUE ~ scenario) )
}
tablong <- tablong  %>%
  mutate(scenario = scenario %>% recode("Obs" = "obs")) %>%
  select(-starts_with("x")) %>%
  # mise en forme longue
  pivot_longer(cols=-c("scenario","annee","datepubli"),names_to="serie",values_to="val") %>%
  filter(!is.na(val)) %>%
  # nouvelle correction à la main
  filter(!(grepl("^2015",datepubli) & grepl("convention cor",scenario))) %>%
  filter(!(grepl("^2016",datepubli) & grepl("conv\\. cor",scenario))) %>%
  mutate(scenario = case_when(
    grepl("^2015",datepubli) ~ str_replace(scenario,"[[:space:]]*\\(convention.+$",""),
    grepl("^2016",datepubli) ~ str_replace(scenario,"[[:space:]]*\\(conv.+$",""),
    TRUE ~ scenario   ) %>%
      str_replace("^(scénario|variante|gains de productivité annuels tendanciels de) ","") %>%
      str_replace_all("(?<=[[:digit:]])[[:space:]](?=%)","") %>%
      recode(!!! list_noms_sc))

# verif :
# unique(tablong$scenario )

# verif <- tablong %>% count(datepubli,scenario)

# part des salaires dans la VA : on duplique jusqu'à l'horizon de projection la dernière valeur indiquée (supposée constante en projection)
if ("partsalva" %in% names(tablong)){
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
}

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

#sc_pour_dupli <- (scenariosproj %>% filter(datepublisc==min(tablong$datepubli)))$scenario

tablong <- bind_rows(
  tablong %>% filter(scenario=="obs"),
  tablong %>% filter(scenario!="obs") %>%
    anti_join(nbseriesscunique, by=c("datepubli","serie")),
  tablong %>% filter(scenario!="obs") %>%
    inner_join(nbseriesscunique, by=c("datepubli","serie")) %>%
    group_by(serie,datepubli,annee) %>%
    slice(rep(1:n(),each=NROW((scenariosproj %>% filter(datepublisc==datepubli[1]))$scenario)) ) %>%
    mutate(scenario = (scenariosproj %>% filter(datepublisc==datepubli[1]))$scenario ) %>%
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
              names_from="serie",values_from="val")
projcor <- projcor %>%
  mutate(txprelevconvepr = if_else(
    is.na(txprelevconvepr) & (scenario=="obs"),txprelevconvtcc,txprelevconvepr))
if ("txprelevconveec" %in% names(projcor)){
  projcor <- projcor %>%
    mutate(txprelevconveec = if_else(
      is.na(txprelevconveec) & (scenario=="obs"),txprelevconvtcc,txprelevconveec))
}
if ("partressourcespibconvepr" %in% names(projcor)){
  projcor <- projcor %>%
    mutate(partressourcespibconvepr = case_when(
      !is.na(partressourcespibconvepr) ~ partressourcespibconvepr,
      is.na(partressourcespibconvepr) & (scenario=="obs") & !is.na(partressourcespibconvtcc) ~ partressourcespibconvtcc,
      is.na(partressourcespibconvepr) & (scenario=="obs") & !is.na(partressourcespibconveec) ~ partressourcespibconveec,
      is.na(partressourcespibconvepr) & (scenario!="obs") ~ partretrpib + soldeconvepr,
      TRUE ~ partressourcespibconvepr),
      soldeconvepr = case_when(
        !is.na(soldeconvepr) ~ soldeconvepr,
        is.na(soldeconvepr) ~ partressourcespibconvepr-partretrpib)
      )
}
if ("partressourcespibconveec" %in% names(projcor)){
  projcor <- projcor %>%
    mutate(partressourcespibconveec = case_when(
      !is.na(partressourcespibconveec) ~ partressourcespibconveec,
      is.na(partressourcespibconveec) & (scenario=="obs") ~ partressourcespibconvtcc,
      #is.na(partressourcespibconveec) & (scenario!="obs") ~ partretrpib + soldeconveec,
      TRUE ~ partressourcespibconveec)#,
      #soldeconveec = case_when(
      #  !is.na(soldeconveec) ~ soldeconveec,
      #  is.na(soldeconveec) ~ partressourcespibconveec-partretrpib
      #)
      )
}
projcor <- projcor %>%
  arrange(datepubli,scenario,annee)

usethis::use_data(projcor, overwrite=TRUE)



