library(devtools)
load_all()

library(tidyverse)
# library(openxlsx)

# A FAIRE !

# renommer taux_plein_duree en taux_plein_duree_strict
# créer agrégat : invalides (1+23), taux plein durée (=tx plein durée strict + racl + surcote)
# vérifier ruptures de séries diverses
# supprimer croisements avec aucune donnée
# supprimer certaines pensions moyenne par sous groupe ?
# créer pension EQCC
# mettre noms courts pour "indicateur" + créer variable "intitulé"


# ===================================================================================
# crée la base "indicgen" dans le package, qui rassemble des indicateurs par génération
# ===================================================================================

# ==== extraction de données de l'EACR

repeacr <- "C:/Users/PA/Documents/bases_de_donnees/EACR/brut/"
eacr1 <- paste0(repeacr,"EACR diffusée Part 1 - version du 24 novembre 2022.xlsx")
eacr2 <- paste0(repeacr,"EACR diffusée Part 2 - version du 24 novembre 2022.xlsx")

tabseacr <- extrait_eacr(c(eacr1,eacr2),c("C-","H-") )

# ===== récupération des données

# -- données de la table C

tabc <- tabseacr[["C_Droits_directs"]]  %>%
  filter(champ=="ddir" & liq=="Ensemble" & statut_sncf=="Ensemble" & age >= 67) %>%
  # pour l'AGIRC et l'ARRCO, on supprime les observations des EACR 2008 et 2009, pour éviter le doublon avec l'EACR "rétrospective historique" de 2009
  filter(!(cc %in% c("5000","6000") & source %in% c("EACR 2008","EACR 2009") )) %>%
  # de même, pour l'IRCANTEC on supprime l'observation de l'EACR 2008 (doublon avec EACR_HISTO de 2009)
  filter(!(cc %in% c("1000") & source %in% c("EACR 2008") )) %>%
  # on supprime la catégorie d'âge maximale (variable selon les années) car il s'agit en réalité d'un regroupement de toutes les générations plus âgées
  group_by(annee,sexe,naiss,cc) %>% filter(age < max(age)) %>% ungroup() %>%
  select(annee,caisse,cc,source,sexe,naiss, age,effectifs,eqcc,m1) %>%
  mutate(generation=annee-age) %>%
  # on crée les variables d'intérêt
  mutate(coefprorat = eqcc/effectifs) %>%
  arrange(cc,sexe,naiss,age,generation)

# -- données de la table H

tabh <- tabseacr[["H_Conditions_liq"]]  %>%
  filter(grepl("^[[:digit:]]{2}$",age)) %>%
  mutate(age = as.numeric(age)) %>%
  filter(age >= 67)  %>%
  # on supprime la catégorie d'âge maximale (variable selon les années) car il s'agit en réalité d'un regroupement de toutes les générations plus âgées
  group_by(annee,sexe,cc) %>% filter(age < max(age)) %>% ungroup() %>%
  # on ajoute les effectifs totaux pour pouvoir calculer des parts
  left_join(tabc %>% filter(naiss=="Ensemble") %>% select(annee,source,cc,caisse,sexe,age,effectifs) %>% rename(effectifstot=effectifs),
            by=c("annee","source","cc","caisse","sexe","age") ) %>%
  mutate(part = effectifs/effectifstot) %>% select(-effectifs, -effectifstot) %>%
  mutate(generation=annee-age)

# -- ajout de certains groupes de régimes (en supposant que l'affiliation à un régime au sein de chaque groupe est exclusive)

# à faire ultérieurement

# -- mise en forme avec indicateurs en lignes

tab <- bind_rows(
  tabc %>%
    select(-effectifs,-eqcc) %>%
    pivot_longer(cols=c("coefprorat","m1"),names_to="indicateur",values_to="valeur") %>%
    filter(!is.na(valeur)),
  tabh %>%
    pivot_longer(cols=c("part","m1"),names_to="indicateur",values_to="valeur") %>%
    mutate(indicateur = paste(indicateur,type)) %>% select(-type) %>% filter(!is.na(valeur)) %>%
    mutate(naiss = "Ensemble")
  )  %>%
  arrange(cc,indicateur,sexe,age,generation)

# -- pension revalorisée à fin 2022

revalo <- revalopensions %>%
  filter(mois==12 & jour==31) %>%
  select(cc,annee,indicerevalo) %>%
  left_join(revalopensions %>% filter(annee==2022 & mois==12 & jour==31) %>% select(cc,indicerevalo) %>%
              rename(revalo2022=indicerevalo),
            by="cc") %>%
  mutate(revalo2022=revalo2022/indicerevalo) %>% select(-indicerevalo)

tab <- tab %>%
  left_join(revalo, by=c("cc","annee")) %>%
  # si l'indicateur est un montant de pension, on ne garde que les valeurs pour lesquelles un coefficient de revalorisation est disponible
  filter(!grepl("^m1",indicateur) | !is.na(revalo2022)) %>%
  # on exprime les pensions revalorisées à la date de fin 2022
  mutate(valeur = ifelse(grepl("^m1",indicateur), valeur*revalo2022,valeur),
         indicateur = indicateur %>% str_replace("^m1","pension revalorisée 2022")) %>%
  select(-revalo2022)

# -- écart (en %) par rapport à la génération née 1 an plus tard, à âge donné

vargen <- tab %>% select(-annee,-source) %>%
  left_join(tab %>% select(caisse,cc,sexe,naiss,age,generation,indicateur,valeur) %>%
              mutate(generation=generation-1) %>%
              rename(valeur_g_p1 = valeur),
            by=c("caisse","cc","sexe","naiss","age","generation","indicateur")) %>%
  mutate(var_g_p1=valeur/valeur_g_p1) %>%
  filter(!is.na(var_g_p1))

# -- indicateurs sur le champ des retraités observés à 67 ans

tab_67 <- tab %>% filter(age==67) %>%
  full_join(vargen %>%
              filter(age>=67) %>%
              group_by(caisse,cc,sexe,naiss,generation,indicateur) %>%
              # méthode 1 = on retient l'écart observé à l'âge le plus bas observé
              #filter(age==min(age)) %>%
              # méthode 2 = on retient l'écart moyen sur toutes les années observées
              # summarise(varageliq_g_p1 = mean(varageliq_g_p1)) %>%
              # méthode 3 = on retient l'écart median sur toutes les années observées
              summarise(var_g_p1 = quantile(var_g_p1,0.5)) %>%
              ungroup() %>%
              select(caisse,cc,sexe,naiss,generation,indicateur,var_g_p1),
            by=c("caisse","cc","sexe","naiss","generation","indicateur") ) %>%
  arrange(caisse,cc,sexe,naiss,indicateur,-generation) %>%
  # on estime la valeur de l'indicateur parmi les retraités observés à 67 ans en chaînant les évolutions d'une année sur l'autre
  mutate(var_g_p1 = ifelse(is.na(var_g_p1),1,var_g_p1),
         var_g_p1 = ifelse(!is.na(age),1,var_g_p1),
         source = ifelse(is.na(source),"chaînage rétrospectif",source)) %>%
  group_by(caisse,cc,sexe,naiss,indicateur) %>%
  fill(valeur,.direction="down") %>%
  fill(age,.direction="down") %>%
  mutate(var_g_p1 = cumprod(var_g_p1),
         valeur = valeur * var_g_p1 ) %>%
  ungroup() %>%
  select(caisse,cc,source,sexe,naiss,generation,indicateur,valeur)

# -- indicateurs sur le champ des retraités observés à 70 ans

tab_70 <- tab %>% filter(age==70) %>%
  full_join(vargen %>%
              filter(age>=70) %>%
              group_by(caisse,cc,sexe,naiss,generation,indicateur) %>%
              # méthode 1 = on retient l'écart observé à l'âge le plus bas observé
              #filter(age==min(age)) %>%
              # méthode 2 = on retient l'écart moyen sur toutes les années observées
              # summarise(varageliq_g_p1 = mean(varageliq_g_p1)) %>%
              # méthode 3 = on retient l'écart median sur toutes les années observées
              summarise(var_g_p1 = quantile(var_g_p1,0.5)) %>%
              ungroup() %>%
              select(caisse,cc,sexe,naiss,generation,indicateur,var_g_p1),
            by=c("caisse","cc","sexe","naiss","generation","indicateur") ) %>%
  arrange(caisse,cc,sexe,naiss,indicateur,-generation) %>%
  # on estime la valeur de l'indicateur parmi les retraités observés à 67 ans en chaînant les évolutions d'une année sur l'autre
  mutate(var_g_p1 = ifelse(is.na(var_g_p1),1,var_g_p1),
         var_g_p1 = ifelse(!is.na(age),1,var_g_p1),
         source = ifelse(is.na(source),"chaînage rétrospectif",source)) %>%
  group_by(caisse,cc,sexe,naiss,indicateur) %>%
  fill(valeur,.direction="down") %>%
  fill(age,.direction="down") %>%
  mutate(var_g_p1 = cumprod(var_g_p1),
         valeur = valeur * var_g_p1 ) %>%
  ungroup() %>%
  select(caisse,cc,source,sexe,naiss,generation,indicateur,valeur)


# -- agrégation des deux tables

indicgen <- bind_rows(
  tab_67 %>% mutate(champ_obs = "Retraités à 67 ans"),
  tab_70 %>% mutate(champ_obs = "Retraités à 70 ans")
  )


# == tests

# indicgen %>% filter(cc=="0010" & naiss=="Ensemble" & indicateur=="part liq_racl") %>% ggplot(aes(y=valeur,x=generation,colour=sexe,linetype=champ_obs,group=paste(sexe,champ_obs))) + geom_line() + labs(title="Valeur de l'indicateur",caption = "Champ : retraités de droit direct.\nSource : DREES, EACR.")
# indicgen %>% filter(cc=="0012" & naiss=="Ensemble"  & indicateur=="part liq_motif_fam") %>% ggplot(aes(y=valeur,x=generation,colour=sexe,linetype=champ_obs,group=paste(sexe,champ_obs))) + geom_line() + labs(title="Valeur de l'indicateur",caption = "Champ : retraités, tous lieux de naissance.\nSource : DREES, EACR.")

# ========= sauvegarde des tables

usethis::use_data(indicgen, overwrite=TRUE)

# -- sauvegarde d'une version .csv

write.csv2(indicgen,
           file=gzfile("data-raw/indicgen.csv.gz"),
           row.names = FALSE, fileEncoding = "UTF-8")

