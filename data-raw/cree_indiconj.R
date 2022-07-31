library(devtools)
#load_all()

library(tidyverse)
# library(openxlsx)

# ===================================================================================
# crée la base "indicconj" dans le package, qui rassemble tous les indicateurs conjoncturels
# ===================================================================================

# ========= âge conjoncturel de départ à la retraite

# RQ : l'âge conjoncturel est ici recalculé à partir de taux de retraités diffusés par la DREES dans son panorama annuel "Retraités et retraite".

conj_ageretr <- txretr %>% select(annee,txretr,sexe,geo) %>%
  group_by(annee,sexe,geo) %>% summarise_all(sum) %>% ungroup() %>%
  mutate(valeur=71-txretr,
         indicateur = "Âge moyen de départ à la retraite",
         unite="années") %>%
  select(-txretr) %>%
  arrange(geo,sexe,annee)

# ========= durées moyennes conjoncturelles de retraite, d'emploi, d'activité, ratio des durées

# tx de nouveaux retraités une année donnée (pour la génération fictive) => servira à calculer     la part d'une génération qui atteint la retraite

txnvretr <- txretr %>% select(age,annee,sexe,txretr)
txnvretr <- bind_rows(txnvretr,
                      txnvretr %>% filter(age==70) %>% mutate(age=71,txretr=1))
txnvretr <- txnvretr %>%
  left_join(txnvretr %>% mutate(age=age+1) %>% rename(txretr_1=txretr),
            by=c("age","annee","sexe")) %>%
  mutate(txnvretr=txretr-ifelse(is.na(txretr_1),0,txretr_1)) %>%
  select(-txretr,-txretr_1)

# verif <- txnvretr %>% select(-age) %>% group_by(annee,sexe) %>% summarise_all(sum) %>%     ungroup()

# taux d'emploi et d'activité moyens par âge quinquenal (au sens du BIT : source Insee)
# source : Activité, emploi et chômage en 2020 et en séries longues Enquête emploi en continu - Insee Résultats
# Paru le : 11/05/2021
# extrait le : 31/07/2022

temp <- tempfile()
download.file("https://www.insee.fr/fr/statistiques/fichier/5359500/T207.zip",temp)
txemploi <- read.table(unz(temp, "t207.csv"),header=TRUE,sep=";")
unlink(temp)

txemploi <- txemploi %>%
  clean_names() %>%
  filter(annee>=2004,grepl("^T[[:digit:]]{2}$",ageq)) %>%
  mutate(sexe = recode(sexe,"H"="hommes","F"="femmes","E"="ensemble"),
         txempbit=txempbit/100,
         ageq=ageq %>% str_extract("[[:digit:]]{2}$") %>% as.numeric())

temp <- tempfile()
download.file("https://www.insee.fr/fr/statistiques/fichier/5359497/t101.zip",temp)
txact <- read.table(unz(temp, "t101.csv"),header=TRUE,sep=";")
unlink(temp)

txact <- txact %>%
  clean_names() %>%
  filter(annee>=2004,grepl("^T[[:digit:]]{2}$",ageq)) %>%
  mutate(sexe = recode(sexe,"H"="hommes","F"="femmes","E"="ensemble"),
         tactbit=tactbit/100,
         ageq=ageq %>% str_extract("[[:digit:]]{2}$") %>% as.numeric())

# coefficient de mortalité

qmort_t69 <- healthexpectancies::FRInseeMortalityrates_t69 %>%
  filter(age>=15,def.age=="current age (approx)",year>=2004,year<=2020) %>%
  rename(annee=year,sexe=sex) %>%
  select(-def.age) %>%
  mutate(sexe = recode(sexe,"male"="hommes","female"="femmes","all"="ensemble"),
         ageq = 5*floor(age/5))

#qmort_t69lisse <- bind_rows(
#  qmort_t69 %>% mutate(annee=annee-1),
#  qmort_t69,
#  qmort_t69 %>% mutate(annee=annee+1)
#) %>%
#  filter(annee>=2004,annee<=2020) %>%
#  group_by(annee,age,ageq,sexe) %>% summarise_all(mean) %>% ungroup()

#qmort_t68 <- healthexpectancies::FRInseeMortalityrates %>%
#  filter(age>=15,geo=="france",year>=2004,year<=2020) %>%
#  rename(annee=year,sexe=sex) %>%
#  select(-geo) %>%
#  mutate(sexe = recode(sexe,"male"="hommes","female"="femmes","all"="ensemble"),
#         ageq = 5*floor(age/5))

qmort <- bind_rows(
  qmort_t69 %>% mutate(categ="mortalité non-lissée") %>% filter(annee<=2020)#,
  #qmort_t69lisse %>% mutate(categ="mortalité lissée sur 3 années consécutives"),
  #qmort_t68 %>% mutate(categ="mort. triennale")
)

# == calcul des durées

duremploi <- qmort %>%
  left_join(txemploi, by=c("annee","sexe","ageq")) %>%
  rename(pix=txempbit) %>%
  mutate(pix = ifelse(!is.na(pix),pix,0)) %>% rename(year=annee,sex=sexe) %>%
  healthexpectancies::CompleteDFLEtable() %>%
  rename(annee=year,sexe=sex) %>%
  filter(age==min(age)) %>%
  select(annee,sexe,categ,age,ex,DLEx,DFLEx) %>%
  rename(duremploi=DLEx) %>%
  select(annee,sexe,categ,duremploi)

duract <- qmort %>%
  left_join(txact, by=c("annee","sexe","ageq")) %>%
  rename(pix=tactbit) %>%
  mutate(pix = ifelse(!is.na(pix),pix,0)) %>%
  #mutate(sexe = recode(sexe,"male"="hommes","female"="femmes","all"="ensemble")) %>%
  rename(year=annee,sex=sexe) %>%
  healthexpectancies::CompleteDFLEtable() %>%
  rename(annee=year,sexe=sex) %>%
  filter(age==min(age)) %>%
  select(annee,sexe,categ,age,ex,DLEx,DFLEx) %>%
  rename(duract=DLEx) %>%
  select(annee,sexe,categ,duract)

durretr <- qmort %>%
  left_join(txretr, by=c("annee","sexe","age")) %>%
  rename(pix=txretr) %>%
  mutate(pix = case_when(!is.na(pix) ~ pix,
                         is.na(pix) & age<=50 ~ 0,
                         is.na(pix) & age>=70 ~ 1)) %>%
  #mutate(sexe = recode(sexe,"male"="hommes","female"="femmes","all"="ensemble")) %>%
  rename(year=annee,sex=sexe) %>%
  healthexpectancies::CompleteDFLEtable() %>%
  rename(annee=year,sexe=sex)

surv <- durretr %>% select(annee,age,categ,sexe,Lx) # table intermédiaire qui servira pour le calcul de la proportion d'une génération (fictive) qui liquide une retraite

durretr <- durretr %>%
  filter(age==min(age)) %>%
  select(annee,sexe,categ,age,ex,DLEx,DFLEx) %>%
  rename(durretr=DLEx,ev15=ex) %>%
  select(annee,sexe,categ,durretr,ev15)

durees <- durretr %>%
  left_join(duremploi, by=c("annee","sexe","categ")) %>%
  left_join(duract, by=c("annee","sexe","categ")) %>%
  mutate(ratio.emploi = durretr/duremploi,
         ratio.act=durretr/duract,
         ratio.vie=durretr/ev15)

conj_durees <- durees %>%
  pivot_longer(cols=starts_with(c("dur","ratio","ev")),
               names_to="indicateur",values_to="valeur") %>%
  filter(indicateur != "ev15") %>%
  mutate(geo = "résidents en france",
         unite = ifelse(grepl("^ratio",indicateur),"%","années"),
         indicateur = indicateur %>% recode(
           "durretr" = "Durée espérée de retraite pour une personne en vie à  15 ans",
           "duremploi"  = "Durée espérée en emploi (au sens du BIT) pour une personne en vie à  15 ans",
           "duract"  = "Durée espérée d'activité (au sens du BIT)  pour une personne en vie à  15 ans",
           "ratio.emploi"= "Rapport entre la durée espérée de retraite et la durée espérée d'emploi (à 15 ans)",
           "ratio.act" = "Rapport entre la durée espérée de retraite et la durée espérée d'activité (à 15 ans)",
           "ratio.vie" = "Part de la durée espérée de retraite dans l'espérance de vie à 15 ans"
         )) %>%
  select(-categ) %>%
  arrange(indicateur,geo,sexe,annee)

#durees100 <- durees %>%
#  select(-starts_with("ratio")) %>%
#  pivot_longer(cols=-c("annee","sexe","categ"),
#               names_to="type.duree",values_to="durees")
#durees100 <- durees100 %>%
#  left_join(durees100 %>% filter(annee==min(annee)) %>% select(-annee) %>%
#              rename(dureesdeb=durees),
#            by=c("sexe","categ","type.duree")) %>%
#  mutate(durees_base100=100*durees/dureesdeb,
#         durees_evol=durees-dureesdeb)
#
# == proportion d'une génération (fictive) ayant liquidé un droit à retraite

conj_atteinteretr <- surv %>%
  left_join(txnvretr, by=c("annee","sexe","age")) %>%
  filter(!is.na(txnvretr)) %>%
  mutate(partnvretr=Lx/100000*txnvretr) %>%
  select(annee,sexe,categ,partnvretr) %>%
  group_by(annee,sexe,categ) %>% summarise_all(sum) %>% ungroup() %>%
  rename(valeur=partnvretr) %>%
  select(-categ) %>%
  mutate(geo = "résidents en france",
         unite = "%",
         indicateur = "Probabilité de décéder avant de liquider un droit direct de retraite (à 15 ans)",
         valeur = 1 - valeur)

# ========= synthèse

indicconj <- bind_rows(
  conj_ageretr,
  conj_durees,
  conj_atteinteretr
) %>%
  arrange(indicateur,geo,sexe,annee)

indicconj <- indicconj[,c("indicateur","sexe","annee","valeur","unite","geo"  )]

usethis::use_data(indicconj, overwrite=TRUE)
