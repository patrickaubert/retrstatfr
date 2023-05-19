library(devtools)
load_all()

library(tidyverse)
# library(openxlsx)
library(broom)

# A FAIRE !

# vérifier ruptures de séries diverses
# supprimer croisements avec aucune donnée
# supprimer certaines pensions moyenne par sous groupe ?
# mettre noms courts pour "indicateur" + créer variable "intitulé"
# signaler pb à Agnès : pour décote 0032 (Femmes et Ensemble) : valeurs 2020 = celles de 2021 !! (idem divers motifs de liq)
# signaler pb : coefprorat femmes CNAV ??
# part décote : AGIRC-ARRCO pas cohérente avec RG ? -> regarder autres incohérences du même type

# ===================================================================================
# crée la base "indicgen" dans le package, qui rassemble des indicateurs par génération
# ===================================================================================

# ==== extraction de données de l'EACR

repeacr <- "C:/Users/PA/Documents/bases_de_donnees/EACR/brut/"
eacr1 <- paste0(repeacr,"EACR diffusée Part 1 - version du 20 février 2023.xlsx")
eacr2 <- paste0(repeacr,"EACR diffusée Part 2 - version du 20 février 2023.xlsx")

tabseacr <- extrait_eacr(c(eacr1,eacr2),c("C-","H-") )

# ===== récupération des données

# -- données de la table C

tabc <- tabseacr[["C_Droits_directs"]]  %>%
  filter(champ=="ddir" & liq=="Ensemble" & statut_sncf=="Ensemble" & age >= 67) %>%
  # pour l'AGIRC et l'ARRCO, on supprime les observations des EACR 2007 à 2009, pour éviter le doublon avec l'EACR "rétrospective historique" de 2009
  filter(!(cc %in% c("5000","6000") & source %in% c("EACR 2007","EACR 2008","EACR 2009") )) %>%
  # de même, pour l'IRCANTEC on supprime l'observation de l'EACR 2008 (doublon avec EACR_HISTO de 2009)
  filter(!(cc %in% c("1000") & source %in% c("EACR 2008") )) %>%
  # on supprime la catégorie d'âge maximale (variable selon les années) car il s'agit en réalité d'un regroupement de toutes les générations plus âgées
  group_by(annee,sexe,naiss,cc) %>% filter(age < max(age)) %>% ungroup() %>%
  select(annee,caisse,cc,source,sexe,naiss, age,effectifs,eqcc,m1) %>%
  mutate(generation=annee-age) %>%
  # on crée les variables d'intérêt
  mutate(coefprorat = eqcc/effectifs,
         m1eqcc = ifelse(!is.na(coefprorat) & coefprorat>0,m1/coefprorat,NA)) %>%
  arrange(cc,sexe,naiss,age,generation)

# -- données de la table H

tabh <- tabseacr[["H_Conditions_liq"]]  %>%
  filter(grepl("^[[:digit:]]{2}$",age)) %>%
  mutate(age = as.numeric(age)) %>%
  filter(age >= 67)  %>%
  # on supprime la part de décès dans l'année, qui n'a pas de sens en tant qu'indicateur générationnel
  filter(!(type %in% c("deces_anc_retraites"))) %>%
  # on supprime la catégorie d'âge maximale (variable selon les années) car il s'agit en réalité d'un regroupement de toutes les générations plus âgées
  group_by(annee,sexe,cc) %>% filter(age < max(age)) %>% ungroup() %>%
  # on ajoute les effectifs totaux pour pouvoir calculer des parts
  left_join(tabc %>% filter(naiss=="Ensemble") %>% select(annee,cc,caisse,sexe,age,effectifs) %>% rename(effectifstot=effectifs),
            by=c("annee","cc","caisse","sexe","age") ) %>%
  mutate(part = effectifs/effectifstot) %>% select(-effectifs, -effectifstot) %>%
  mutate(generation=annee-age) %>%
  # on renomme certaines modalités
  mutate(type = recode(type,"taux_plein_duree" = "taux_plein_duree_strict"))

# -- suppression des valeurs avant ruptures de série (après analyse graphique des ruptures apparentes)

tabh <- tabh %>%
  # taux de décote à l'AGIRC-ARRCO : rupture entre 2019 et 2020
  filter(!(cc=="5600" & grepl("decote",type) & annee<=2019)) %>%
  # CNAV : pensions par type de départ : rupture de série entre 2010 et 2011
  mutate(m1 = ifelse(cc=="0010" & annee==2010, NA, m1)) %>%
  # CNAV : année 2017 atypique pour la part de décote
  mutate(part = ifelse(cc=="0010" & type=="decote" & annee==2017,NA, part))

tabc <- tabc %>%
  # CAVIMAC : rupture de série sur les EQCC entre 2019 et 2020, mais uniquement sur les générations récentes ??? -> dans le doute on supprime
  mutate(coefprorat = ifelse(cc=="0090",NA, coefprorat)) %>%
  # RG : valeur bizarre du coefprorat en 2018, sauf pour les Hommes ?
  mutate(coefprorat = ifelse(cc=="0010" & sexe!="Hommes" & annee==2018,NA, coefprorat)) %>%
  mutate(coefprorat = ifelse(cc=="0015" & sexe!="Hommes" & naiss!="France" & annee==2020,NA, coefprorat)) %>%
  # CNIEG : rupture de série sur eqcc entre 2017 et 2018 (surtout pour les femmes)
  mutate(coefprorat = ifelse(cc=="0100" & annee<2018,NA, coefprorat))

# -- création de nouveaux indicateurs par agrégats de parts

agr_type <- function(type_in,type_out) {
  tabh %>%
    filter(type %in% c(type_in) ) %>%
    mutate(type = type_out) %>%
    group_by(annee,source,cc,caisse,sexe,type,age,generation) %>%
    summarise(m1 = weighted.mean(m1,w=part),
              part = sum(part),
              nb = n() ) %>%
    #filter(nb == NROW(type_in)) %>%
    ungroup() %>%
    select(-nb)
}

tabh <- bind_rows(
  # indicateurs déjà présents dans la base
  tabh,
  # invalides (catégories 1+2+3)
  agr_type(c("ex_inval1","ex_inval23"),"ex_inval"),
  # invalides et inaptes
  agr_type(c("ex_inval1","ex_inval23","inaptes"),"ex_inval_et_inaptes"),
  # ensemble des dispositifs au titre de l'incapacité, du handicap ou de la pénibilité

  # départs au titre de la durée (yc racl et surcote)
  agr_type(c("taux_plein_duree_strict","liq_racl","surcote"),"taux_plein_duree_yc_racl_surcote") %>%
    filter(cc %in% c("0022","0015") & annee>=2020),
)


# -- ajout de certains groupes de régimes (en supposant que l'affiliation à un régime au sein de chaque groupe est exclusive)

# à faire ultérieurement

# -- mise en forme avec indicateurs en lignes

tab <- bind_rows(
  tabc %>%
    select(-effectifs,-eqcc) %>%
    pivot_longer(cols=c("coefprorat","m1","m1eqcc"),names_to="indicateur",values_to="valeur") %>%
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
         indicateur = indicateur %>% str_replace("^m1","pension revalorisée 2022 ")) %>%
  select(-revalo2022)

# -- écart (en %) par rapport à la génération née 1 an plus tard, à âge donné

vargen <- tab %>% select(-annee,-source) %>%
  left_join(tab %>% select(caisse,cc,sexe,naiss,age,generation,indicateur,valeur) %>%
              mutate(generation=generation-1) %>%
              rename(valeur_g_p1 = valeur),
            by=c("caisse","cc","sexe","naiss","age","generation","indicateur")) %>%
  mutate(var_g_p1=valeur/valeur_g_p1) %>%
  filter(!is.na(var_g_p1))

# -- décomposition des variations en effets génération et effets année, pour la détection des ruptures de série

effets <- vargen %>%
  mutate(lvar = log(var_g_p1),
         annee = factor(generation+age),
         generation = factor(generation)) %>%
  filter(!is.na(lvar)) %>%
  group_by(cc,sexe,naiss,indicateur) %>%
  filter( NROW(unique(annee))>1 & NROW(unique(generation))>1 )  %>%
  ungroup() %>%
  nest_by(cc,sexe,naiss,indicateur) %>%
  mutate(model = list(lm(lvar ~ 0 + annee + generation, data=data) ) )

coeff_effets <- effets %>% summarise(tidy(model)) %>%
  filter(grepl("[[:digit:]]$",term)) %>%
  mutate(type = str_extract(term,"[^[:digit:]]+"),
         valeur = str_extract(term,"[[:digit:]]+") )

quantiles_effetsannee <- coeff_effets %>% filter(type=="annee" ) %>%
  group_by(cc,naiss,sexe,indicateur) %>%
  nest() %>%
  mutate(
    ret = map(data,~quantile(x=.$estimate, probs = c(0.25,0.5,0.75) )),
    ret = invoke_map(data.frame, ret)
  ) %>%
  unnest(ret) %>% ungroup() %>%
  select(-data) %>%
  janitor::clean_names()

pb_effetsannee <- coeff_effets %>%
  filter(type=="annee" ) %>%
  left_join(quantiles_effetsannee, by=c("cc","naiss","sexe","indicateur")) %>%
  filter(estimate < x50-2*(x75-x25) | estimate > x50+2*(x75-x25)) %>%
  rename(annee=valeur) %>%
  #mutate(cat = paste(sexe,naiss)) %>% select(-sexe,-naiss) %>%
  group_by(indicateur,naiss,cc,annee) %>%
  summarise(nb_pb = n(),
            cat_pb = paste(sexe,collapse="+")) %>%
  ungroup() %>%
  mutate(annee=as.numeric(annee))

filtre_an_pb <- bind_rows(
  pb_effetsannee #%>% filter(naiss!="France"),
  #pb_effetsannee %>% filter(naiss=="Ensemble")  %>% mutate(naiss="France")
) %>%
  filter(nb_pb>=2) %>%
  select(cc,naiss,annee)

if (FALSE) {

  # pour représentation graphique

  reg <- "0010"
  coeff_effets %>% filter(cc==reg & type=="annee" & indicateur=="part decote" ) %>% # & naiss=="Ensemble"
    ggplot(aes(y=estimate,x=valeur,group=paste(sexe,naiss),linetype=naiss,colour=sexe)) +
    geom_line() +
    theme(legend.position="top") +
    facet_grid( ~naiss) +
    labs(subtitle=paste("Régime = ",reg))

}

# -- correction des ruptures de séries manifestes (par suppression des années avant la dernière rupture)

# A FAIRE


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

