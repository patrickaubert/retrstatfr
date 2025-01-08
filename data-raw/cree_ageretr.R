library(devtools)
load_all()

library(tidyverse)
# library(openxlsx)
library(broom)


# ===================================================================================
# crée la base "ageretr" dans le package, qui rassemble des indicateurs d'âges moyen de départ à la retraite
# ===================================================================================

# ==== âge moyen par génération (source EACR)

repeacr <- "C:/Users/PA/Documents/bases_de_donnees/EACR/brut/"
eacr1 <- paste0(repeacr,"EACR diffusée Part 1 - version du 20 février 2023.xlsx")
eacr2 <- paste0(repeacr,"EACR diffusée Part 2 - version du 20 février 2023.xlsx")

tabseacr <- extrait_eacr( eacr1,c("C-") )

# -- données de l'EACR avec les âges moyens de liquidation observés à divers âges, et les liquidations tardives
tabc <- tabseacr[["C_Droits_directs"]] %>%
  filter(( (champ=="ddir" & !is.na(ageliq))  | (champ=="nl_ddir" ) ) &
           age >= case_when( annee-age<=1950 ~ 66, (annee-age) %in% c(1951,1952) ~ 67, TRUE ~ 68) &
           liq=="Ensemble" & statut_sncf=="Ensemble" &
           !is.na(effectifs)
  ) %>%
  # pour l'AGIRC et l'ARRCO, on supprime les observations des EACR 2007 à 2009, pour éviter le doublon avec l'EACR "rétrospective historique" de 2009
  filter(!(cc %in% c("5000","6000") & source %in% c("EACR 2007","EACR 2008","EACR 2009") )) %>%
  # de même, pour l'IRCANTEC on supprime l'observation de l'EACR 2008 (doublon avec EACR_HISTO de 2009)
  filter(!(cc %in% c("1000") & source %in% c("EACR 2008") )) %>%
  # CNAVPL : ageliq non renseigné (mis à 0)
  #filter(cc!="2100") %>%
  # CNBF : valeurs manifestement aberrantes
  #filter(!(cc %in% c("2201","2202"))) %>%
  mutate(ageliq = ifelse(cc %in% c("2201","2202"), NA_real_,ageliq)) %>%
  # on supprime la catégorie d'âge maximale (variable selon les années) car il s'agit en réalité d'un regroupement de toutes les générations plus âgées
  group_by(annee,cc) %>% filter(age < max(age)) %>% ungroup()  %>%
  mutate(generation=annee-age)

# -- mise à NA des valeurs nulls

tabc <- tabc %>%
  mutate(eqcc = ifelse(eqcc==0, NA, eqcc),
         ageliq = ifelse(ageliq==0, NA, ageliq),
         m1 = ifelse(m1==0, NA, m1) )

# -- revalorisation des montants de pension en euros fin décembre 2022

#revalo <- retrstatfr::revalopensions %>%
revalo <- revalopensions %>%
  filter(mois==12 ) %>%
  select(cc,annee,indicerevalo) %>%
  #left_join(retrstatfr::revalopensions %>% filter(annee==2022 & mois==12 & jour==31) %>% select(cc,indicerevalo) %>%
  left_join(revalopensions %>% filter(annee==2022 & mois==12 ) %>% select(cc,indicerevalo) %>%
              rename(revalo2022=indicerevalo),
            by="cc") %>%
  mutate(revalo2022=revalo2022/indicerevalo) %>% select(-indicerevalo)

tabc <- tabc %>%
  mutate(ccrevalo = case_when(
    cc %in% c("0021","0040","0050","0042","0015") ~ "0010",
    cc %in% c('0012','0013','0015','0022','0032','0033','0060','0061','0070','0080','0090','0100','0300','0301','0500','0600','2100','2201') & annee>2003 ~ "0010",
    TRUE ~ cc  )) %>%
  left_join(revalo, by=c("ccrevalo"="cc","annee")) %>%
  # on exprime les pensions revalorisées à la date de fin 2022
  mutate(m1_e2022 = m1 * revalo2022,
         mont_e2022 = mont * revalo2022) %>%
  select(-revalo2022,-ccrevalo)

# -- création des indicateurs EQCC et de diverses parts

tabc <- tabc %>%
  # création des indicateurs EQCC
  mutate(coefprorat = eqcc/effectifs,
         m1eqcc_e2022 = m1_e2022/coefprorat) %>%
  # création part par sexe # A REFAIRE : prise en compte "ensemble"
  group_by(annee,cc,naiss,champ,age) %>%
  mutate(poids_sexe = 100*effectifs/sum(effectifs)) %>% ungroup() %>%
  # création part par lieu de naissance # A REFAIRE : prise en compte "ensemble"
  group_by(annee,cc,sexe,champ,age) %>%
  mutate(poids_naiss = 100*effectifs/sum(effectifs)) %>% ungroup() %>%
  # création poids du régime (de base) dans le nombre total de pensions
  group_by(annee,naiss,sexe,champ,age) %>%
  mutate(
    rb = 1*(!(cc %in% c("1000","3000","5000","5600","6000","0000","0041","0051","0043"))),
    poids_regime_effectifs = 100*effectifs*rb/sum(effectifs*rb),
    poids_regime_eqcc = ifelse(
      # régimes ayant fourni l'information sur les EQCC : ATTENTION, il manque notamment les régimes de la MSA !
      annee >= 2017 & cc %in% c('0012','0013','0032','0033','0010','0042','0060','0100','0500','0015'),
      100 * eqcc / sum(eqcc),
      0) ) %>%
  ungroup() %>%
  select(-eqcc,-rb)

# -- séparation en deux tables : table sur les âges moyens observés parmi les vivants à un âge donné + table sur les départs tardifs

tabc_liqtardives <- tabc %>% filter(champ=="nl_ddir")  %>%
  select(cc,sexe,naiss, age,generation,effectifs,m1_e2022,m1eqcc_e2022) %>%
  rename(effectifsliq=effectifs) %>%
  # par approximation, on considère que les nouveaux retraités au cours de l'année de leur âge A (en différence de millésime) ont liquidé en moyenne à l'âge A (en âge exact)
  #mutate(ageliq = age) %>%
  arrange(cc,sexe,naiss,generation,age)

tabc_ageobs <- tabc %>% filter(champ=="ddir") %>%
  select(annee,caisse,cc,source,sexe,naiss, age,generation,
         ageliq,effectifs,m1_e2022,coefprorat,m1eqcc_e2022,poids_regime_effectifs,poids_regime_eqcc) %>%
  rename(ageobs = age) %>%
  arrange(cc,sexe,naiss,generation,ageobs)

tabc_ageobs <- bind_rows(
  # générations observées après l'âge considéré comme l'âge maximal de départ à la retraite => conservées telles quelles
  tabc_ageobs %>% filter(ageobs>=agemaxdepart) %>%
    mutate(age = ageobs),
  # générations observées avant cet âge => on ajoute les lignes pour les liquidations tardives
  tabc_ageobs %>% filter(ageobs<agemaxdepart) %>%
    group_by(cc,sexe,naiss,generation,ageobs) %>%
    slice(rep(1:n(), each=(agemaxdepart-ageobs+1))) %>%
    mutate(age = c(min(ageobs):agemaxdepart)) %>%
    ungroup() ) %>%
  arrange(cc,sexe,naiss,generation,ageobs,age)

# -- extrapolation des parts de liquidations tardives pour les âges qui ne sont pas encore observés
#          (ie. pour les générations pour lesquelles la dernière observation disponible est avant l'âge max de départ)
#          (on fait l'hypothèse que le ratio effectifs liquidant dans le régime / population à l'âge donné reste constant par rapport à sa dernière valeur observée)

annees_dispo_liqtardives <- tabc_liqtardives %>% filter(!is.na(effectifsliq)) %>% mutate(annee = generation+age) %>% distinct(cc,sexe,naiss,annee)

tabc_liqtardives <- bind_rows(
  # liquidations après l'âge max de départ à la retraite supposé
  tabc_liqtardives %>% filter(age>agemaxdepart) ,
  # liquidations avec cet âge max de départ => on ajoute des lignes pour prévoir, pour chaque génération, les liquidations jusqu'à 80 ans (y compris pour les années pas encore observées)
  tabc_liqtardives %>% filter(age<=agemaxdepart) %>% select(-age) %>%
    distinct(cc,sexe,naiss,generation) %>%
    group_by(cc,sexe,naiss,generation) %>%
    slice(rep(1:n(), each = (agemaxdepart-ageref+1))) %>%
    mutate(age = c(ageref:agemaxdepart)) %>%
    ungroup()
) %>%
  select(cc,sexe,naiss,generation,age) %>%
  left_join(tabc_liqtardives, by=c("cc","sexe","naiss","generation","age")) %>%
  mutate(annee = age + generation) %>%
  left_join(annees_dispo_liqtardives %>% mutate(anneedispo = TRUE), by=c("cc","sexe","naiss","annee")) %>%
  mutate(effectifsliq = ifelse(is.na(effectifsliq) & !is.na(anneedispo) & anneedispo, 0 , effectifsliq)) %>% select(-anneedispo)  %>%
  arrange(cc,sexe,naiss,generation,age)

pop <- healthexpectancies::FRInseePopulationForecast2021 %>%  #healthexpectancies::FRInseePopulation %>%
  filter(geo == "france" & age0101 >= ageref & age0101 <= agemaxdepart) %>%
  mutate(annee = year-1,
         sexe = recode(sex, "female"="Femmes", "male"="Hommes", "all"="Ensemble")) %>%
  rename(age = age0101, popx = popx0101) %>%
  select(annee,sexe,age,popx)

# pop <- bind_rows( pop , pop %>% mutate(sexe = "Ensemble")) %>% group_by(annee,sexe,age) %>% summarise_all(sum) %>% ungroup()

tabc_liqtardives <- tabc_liqtardives %>%
  left_join(pop, by=c("annee","sexe","age")) %>%
  mutate(ratio_effectifs_pop = effectifsliq/popx ) %>%
  arrange(cc,sexe,naiss,age,generation) %>%
  group_by(cc,sexe,naiss,age) %>%
  fill(ratio_effectifs_pop, .direction="down") %>%
  mutate(effectifsliqimput = ifelse(is.na(effectifsliq),ratio_effectifs_pop*popx,NA_real_)) %>%
  ungroup() %>%
  arrange(cc,sexe,naiss,generation,age) %>%
  select(-annee) %>%
  mutate(effectifsliq = ifelse(!is.na(effectifsliq),effectifsliq, effectifsliqimput)) %>% filter(!is.na(effectifsliq)) %>%
  select(-effectifsliqimput,-popx,-ratio_effectifs_pop) %>%
  rename(m1_e2022_liq = m1_e2022,
         m1eqcc_e2022_liq = m1eqcc_e2022)

# -- calcul de l'âge de liquidation "corrigé des liquidations tardives" (ie après âge d'observation et avant l'âge maximal de départ à la retraite)
#    pour les besoins de la comparaison avec l'EIR 2016, on calcule aussi un indicateur "corrigé des liquidations tardives jusqu'à 2016"

tabc_ageobs <- tabc_ageobs %>%
  left_join(tabc_liqtardives, by=c("cc","sexe","naiss","generation","age")) %>%
  mutate(effectifs = ifelse(age == ageobs, effectifs, effectifsliq),
         ageliq = ifelse(age == ageobs, ageliq, age),
         ageliqhorsnvx = case_when(
           age==ageobs & !is.na(effectifsliq) ~ (effectifs*ageliq-effectifsliq*age)/(effectifs-effectifsliq),
           age==ageobs & is.na(effectifsliq) ~ ageliq,
           age>ageobs ~ NA_real_   ),
         m1_e2022 = ifelse(age == ageobs | is.na(m1_e2022_liq), m1_e2022, m1_e2022_liq ), # pour l'instant, on ne cherche pas à imputer les valeurs manquantes (notamment les cas où il y a moins de 10 observations)
         m1eqcc_e2022 = ifelse(age == ageobs | is.na(m1eqcc_e2022_liq), m1eqcc_e2022, m1eqcc_e2022_liq )
  ) %>%
  select(-effectifsliq, -m1_e2022_liq, -m1eqcc_e2022_liq) #%>%
#group_by(cc,sexe,naiss,generation,ageobs) %>%
#mutate( effectifs = ifelse(is.na(effectifs) & age < max(age[!is.na(effectifs)]), 0, effectifs)) %>%
#ungroup()

tabc_ageobs <- tabc_ageobs %>%
  pivot_longer(cols=c('ageliq','ageliqhorsnvx',
                      'm1_e2022','m1eqcc_e2022',
                      'coefprorat',
                      'poids_regime_effectifs','poids_regime_eqcc'),
               names_to = "indicateur", values_to = "val") %>%
  filter(!is.na(val))  %>%
  arrange(cc,sexe,naiss,generation,indicateur,ageobs,age) %>%
  filter(indicateur != "ageliqhorsnvx") # à ce stade, on ne fait rien de cet indicateur

tabc_ageobs <- tabc_ageobs %>%
  group_by(cc,sexe,naiss,generation,indicateur,ageobs) %>%
  mutate(
    # effectifs de liquidants (y compris liquidations tardives observées et imputées jusqu'à l'âge max de départ à la retraite) en vie à l'âge de référence
    effectifscorr = sum(effectifs[!is.na(effectifs)]),
    #effectifscorr2016 = ifelse(generation+ageobs>2016, effectifs, sum(effectifs[!is.na(effectifs) & generation+age<=2016])),
    effectifscorr2016 = sum(effectifs[!is.na(effectifs) & generation+age<=pmax(generation+ageobs,2016)]) ,
    # part des départs à l'âge d'observation parmi les départs (y compris liquidations tardives) des personnes en vie à l'âge de référence
    partliq = 100,
    partliqcorr = 100*effectifs/effectifscorr,
    partliqcorr2016 = 100*effectifs/effectifscorr2016 ,
    # âge moyen de départ à la retraite corrigé des liquidations tardives (observées ou imputées jusqu'à l'âge max) après l'âge de référence
    valcorr = weighted.mean(val[!is.na(effectifs)], w=effectifs[!is.na(effectifs)], na.rm=TRUE),
    #ageliqcorr2016 = ifelse(generation+ageobs>2016, ageliq, weighted.mean(ageliq[!is.na(effectifs) & annee<=2016], w=effectifs[!is.na(effectifs) & annee<=2016], na.rm=TRUE) )
    valcorr2016 = weighted.mean(val[!is.na(effectifs) & generation+age<=pmax(generation+ageobs,2016)], w=effectifs[!is.na(effectifs) & generation+age<=pmax(generation+ageobs,2016)], na.rm=TRUE) ,
  ) %>%
  filter(age == ageobs) %>%
  ungroup()

#verif <- tabc_ageobs %>%
#  group_by(annee,caisse,cc,source,sexe,naiss,ageobs) %>% filter(n()>1) %>% arrange(annee,caisse,cc,source,sexe,naiss,ageobs)

# -- mise en forme de la base, en ajoutant un critère de ventilation supplémentaire : variable "type_correction"

tabc_ageobs <- tabc_ageobs %>%
  select(-ageobs) %>%
  pivot_longer(cols=c(starts_with("effectifs"),
                      starts_with("partliq"),
                      starts_with("val")
  ),names_to="variable",values_to="valeur") %>%
  mutate(type_correction = variable %>% str_extract("corr.*$"),
         type_correction = case_when(
           type_correction == "corr" ~ paste0("corrigé des liquidations tardives (jusqu'à ",agemaxdepart," ans)"),
           type_correction == "corr2016" ~ paste0("corrigé des liquidations tardives antérieures à 2016 (jusqu'à ",agemaxdepart," ans)"),
           is.na(type_correction) ~ paste0("non corrigé des liquidations après l'AAD + 1 an"),
           TRUE ~"?" ),
         variable = variable %>% str_replace("corr.*$","")) %>%
  pivot_wider(names_from="variable",values_from="valeur")

# on supprime les indicateurs "corrigé des liquidations tardives" lorsque ce n'est pas pertinent
tabc_ageobs <- tabc_ageobs %>%
  filter(indicateur %in% c("ageliq","m1_e2022","") | grepl("^non corrigé",type_correction))

# -- ajout des sexe == "Ensemble" pour les régimes où c'est manquant

tabc_ageobs <- bind_rows(
  # données de base
  tabc_ageobs,
  # ajout de sexe = Ensemble pour les ventilations par lieu de naissance
  tabc_ageobs %>%
    filter(naiss != "Ensemble") %>%
    mutate(sexe = "Ensemble") %>%
    group_by(annee,caisse,cc,source,sexe,naiss,age,generation,indicateur,type_correction) %>%
    summarise(val = mean(val,w=effectifs),
              partliq = mean(partliq,w=effectifs),
              effectifs = sum(effectifs) ) %>%
    ungroup()
)

# -- ajout de certains groupes de régimes (en supposant que l'affiliation à un régime au sein de chaque groupe est exclusive)

# verif <- tabc %>% count(cc,annee) %>% pivot_wider(names_from="cc",values_from="n") %>% arrange(annee)

tabc_ageobs <- bind_rows(
  # données de base
  tabc_ageobs,
  # régimes de la Fonction publique civile
  tabc_ageobs %>% filter(cc %in% c("0012","0032","0033") & annee>=2014) %>%
    mutate(cc = "_FPc", caisse = "Fonction publique civile", source = "Recalculé") %>%
    group_by(cc,caisse,source,sexe,naiss,generation,annee,age,indicateur,type_correction) %>%
    summarise(val = mean(val,w=effectifs),
              partliq = mean(partliq,w=effectifs),
              effectifs = sum(effectifs) ,
              nbreg = n() ) %>%
    filter(nbreg == 3) %>%
    ungroup(),
  # régimes de la Fonction publique y compris militaire
  tabc_ageobs %>% filter(cc %in% c("0012","0013","0032","0033") & annee>=2014) %>%
    mutate(cc = "__FP", caisse = "Fonction publique y compris militaires", source = "Recalculé") %>%
    group_by(cc,caisse,source,sexe,naiss,generation,annee,age,indicateur,type_correction) %>%
    summarise(val = mean(val,w=effectifs),
              partliq = mean(partliq,w=effectifs),
              effectifs = sum(effectifs) ,
              nbreg = n()) %>%
    filter(nbreg == 4) %>%
    ungroup(),
  # régimes spéciaux (SNCF, RATP, CNIEG, CRPCEN)
  tabc_ageobs %>% filter(cc %in% c("0060","0100","0300","0500") & annee>=2011) %>%
    mutate(cc = "RSpé", caisse = "Régimes spéciaux (SNCF, RATP, CNIEG, CRPCEN)", source = "Recalculé") %>%
    group_by(cc,caisse,source,sexe,naiss,generation,annee,age,indicateur,type_correction) %>%
    summarise(val = mean(val,w=effectifs),
              partliq = mean(partliq,w=effectifs),
              effectifs = sum(effectifs) ,
              effectifs = sum(effectifs),
              nbreg = n()) %>%
    filter(nbreg == 4) %>%
    ungroup(),
  # régimes d'indépendants (hors PL)
  tabc_ageobs %>% filter(cc %in% c("0022","0040","0050","0042") & annee>=2015 & annee<=2019) %>%
    mutate(cc = "Indp", caisse = "Indépendants (hors libéraux)", source = "Recalculé") %>%
    group_by(cc,caisse,source,sexe,naiss,generation,annee,age,indicateur,type_correction) %>%
    summarise(val = mean(val,w=effectifs),
              partliq = mean(partliq,w=effectifs),
              effectifs = sum(effectifs) ,
              nbreg = n()) %>%
    filter(nbreg %in% c(2,3) ) %>%
    ungroup()
)  %>%
  select(-effectifs, -nbreg)

# écart (en %) par rapport à la génération née 1 an plus tard, à âge donné
#            pour les retraités en vie à l'âge de référence (non corrigé des liquidations tardives) : on compare l'âge moyen de la génération G observée à l'âge A-1 avec celui de la même génération observée à l'âge A HORS LIQUIDANTS DE L'ANNEE A ! (=> seul l'effet des décès joue)
#            pour les indicateurs corrigés des liquidations tardives : on compare les âges moyens corrigés en A-1 et A

vargen <- tabc_ageobs %>% select(-annee,-source) %>%
  left_join(tabc_ageobs %>% select(caisse,cc,sexe,naiss,age,generation,indicateur,type_correction,val) %>%
              mutate(generation=generation-1) %>%
              rename(val_g_p1 = val ),
            by=c("caisse","cc","sexe","naiss","age","generation","indicateur","type_correction")) %>%
  mutate(varval_g_p1 = val / val_g_p1 ) %>%
  filter(!is.na(varval_g_p1))

# tests sur les évolutions par régime x année, afin de détecter d'éventuelle ruptures de séries

effets <- vargen %>%
  mutate(lvarage = log(varval_g_p1),
         annee = factor(generation+age),
         generation = factor(generation)) %>%
  filter(!is.na(lvarage)) %>%
  group_by(cc,sexe,naiss,indicateur,type_correction) %>%
  filter( NROW(unique(annee))>1 & NROW(unique(generation))>1 )  %>%
  ungroup() %>%
  nest_by(cc,sexe,naiss,indicateur,type_correction) %>%
  mutate(model = list(lm(lvarage ~ 0 + annee + generation, data=data) ) )

coeff_effets <- effets %>% summarise(tidy(model)) %>%
  filter(grepl("[[:digit:]]$",term)) %>%
  mutate(type = str_extract(term,"[^[:digit:]]+"),
         valeur = str_extract(term,"[[:digit:]]+") )

quantiles_effetsannee <- coeff_effets %>% filter(type=="annee" ) %>%
  group_by(cc,naiss,sexe,indicateur,type_correction) %>%
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
  left_join(quantiles_effetsannee, by=c("cc","naiss","sexe","indicateur","type_correction")) %>%
  filter(estimate < x50-2*(x75-x25) | estimate > x50+2*(x75-x25)) %>%
  rename(annee=valeur) %>%
  #mutate(cat = paste(sexe,naiss)) %>% select(-sexe,-naiss) %>%
  group_by(naiss,cc,annee,indicateur,type_correction) %>%
  summarise(nb_pb = n(),
            cat_pb = paste(sexe,collapse="+")) %>%
  ungroup() %>%
  mutate(annee=as.numeric(annee))

filtre_an_pb <- bind_rows(
  pb_effetsannee #%>% filter(naiss!="France"),
  #pb_effetsannee %>% filter(naiss=="Ensemble")  %>% mutate(naiss="France")
) %>%
  filter(nb_pb>=2) %>%
  select(cc,naiss,annee,indicateur,type_correction)

if (FALSE) {

  # pour représentation graphique

  reg <- "0012"
  coeff_effets %>% filter(cc==reg & type=="annee" ) %>% # & naiss=="Ensemble"
    ggplot(aes(y=estimate,x=valeur,group=paste(sexe,naiss),linetype=naiss,colour=sexe)) +
    geom_line() +
    theme(legend.position="top") +
    facet_grid( ~naiss) +
    labs(subtitle=paste("Régime = ",reg))

}

# on tronque les observations pour éliminer les années pour lesquelles il y a manifestement des ruptures de série

agetabc <- tabc_ageobs %>%
  left_join(filtre_an_pb %>%
              group_by(cc,naiss,indicateur,type_correction) %>% summarise(an_pb_max=max(annee)) %>% ungroup(),
            by=c("cc","naiss","indicateur","type_correction")) %>%
  filter(is.na(an_pb_max) | annee > an_pb_max ) %>%
  # == on conserve quelques cas traités à la mains
  # MSA : profils très bizarres (âges arrondis à l'entier ??), sauf pour les années récentes
  filter(!(cc %in% c("0022","0021","Indp") & generation+age<2015)) %>%
  # AGIRC-ARRCO : évolution 2019-2020 semble non cohérente avec 2020-2021
  filter(!(cc=="5600" & generation+age<2020))

vargencorr <- vargen %>%
  mutate(annee = generation+age) %>%
  anti_join(filtre_an_pb, by=c("cc","naiss","annee","indicateur","type_correction")) %>%
  select(-annee) %>%
  filter(!(cc %in% c("0022","0021","Indp") & generation+age<2015))  %>%
  filter(!(cc=="5600" & generation+age<2020))

# (ancienne version : fait manuellement)
if (FALSE) {
  agetabc <- tabc %>%
    # ARRCO : plusieurs ruptures apparentes avant 2013
    filter(!(cc=="6000" & generation+age<2013)) %>%
    # AGIRC-ARRCO : évolution 2019-2020 semble non cohérente avec 2020-2021
    filter(!(cc=="5600" & generation+age<2020)) %>%
    # CNAV : rupture apparemment entre 2004 et 2005
    filter(!(cc=="0010" & generation+age<2005)) %>%
    # MSA : profils très bizarres (âges arrondis à l'entier ??), sauf pour les années récentes
    filter(!(cc %in% c("0022","0021","Indp") & generation+age<2015)) %>%
    # RCI complémentaire : forte hausse parmi les générations anciennes => à vérifier (rattrapage de stock ?)
    filter(!(cc=="0043")) %>%
    # ENIM : seules 2 années disponibles (2020 et 2021), et il y a manifestement une rupture de série entre ces deux années => chaînage impossible
    filter(!(cc=="0070")) %>%
    # CAVIMAC : âge de liquidation arrondi à 0.5 an près pour le détail par lieu de naissance
    filter(!(cc=="0090" & naiss != "Ensemble"))  %>%
    # RATP
    filter(!(cc %in% c("0300","RSpé") & generation+age<2011))  %>%
    # IRCANTEC : rupture de série apparent entre 2007-2008 et entre 2013-2014
    filter(!(cc=="1000" & generation+age<2014))

  vargen <- vargen %>%
    filter(!(cc=="0010" & (generation+age %in% c(2004,2007,2008) )) ) %>%
    #filter(!(cc=="6000" & (generation+age) %in% c(2005,2009,2012)))  %>%
    #filter(!(cc=="6000" & (generation+age<2013) )) %>%
    filter(!(cc=="6000" & (generation+age) %in% c(2006,2009,2012)))  %>%
    filter(!(cc=="5600" & generation+age<2020)) %>%
    filter(!(cc=="5000" & (generation+age %in% c(2007,2011,2012,2013) )) ) %>%
    filter(!(cc %in% c("0022","0021","Indp") & generation+age<2015))  %>%
    filter(!(cc %in% c("0300","RSpé") & generation+age<2011)) %>%
    #filter(!(cc=="1000" & ((generation+age)<2008))) %>%
    #filter(!(cc=="1000" & ((generation+age)==2013))) %>%
    filter(!(cc=="1000" & (generation+age) %in% c(2007,2010,2013,2019)))
}

# âge moyen de départ à la retraite parmi les retraités observés à l'âge de référence

ageretr <- agetabc %>%
  #filter(age==ageref) %>%
  # âge de référence = âge d'annulation de la décote (en partie entière) + 1 an
  filter(age == case_when(generation<=1950 ~ 66, generation %in% c(1951,1952) ~ 67, generation>=1953 ~ 68)) %>%
  full_join(vargencorr %>%
              filter(age >= case_when(generation<=1950 ~ 66, generation %in% c(1951,1952) ~ 67, generation>=1953 ~ 68)) %>%
              group_by(caisse,cc,sexe,naiss,generation,indicateur,type_correction) %>%
              # méthode 1 = on retient l'écart observé à l'âge le plus bas observé
              #filter(age==min(age)) %>%
              # méthode 2 = on retient l'écart moyen sur toutes les années observées
              # summarise(varageliq_g_p1 = mean(varageliq_g_p1)) %>%
              # méthode 3 = on retient l'écart median sur toutes les années observées
              summarise(varval_g_p1 = quantile(varval_g_p1,0.5) ) %>%
              ungroup() %>%
              select(caisse,cc,sexe,naiss,generation,indicateur,type_correction,varval_g_p1),
            by=c("caisse","cc","sexe","naiss","generation","indicateur","type_correction") ) %>%
  arrange(caisse,cc,sexe,naiss,indicateur,type_correction,-generation) %>%
  # on estime l'âge moyen de liquidation parmi les retraités observés à l'âge de référence ans en chaînant les évolutions d'une année sur l'autre
  mutate(varval_g_p1 = ifelse(is.na(varval_g_p1),1,varval_g_p1),
         varval_g_p1 = ifelse(!is.na(age),1,varval_g_p1) ,
         source = ifelse(is.na(source),"chaînage rétrospectif",source)) %>%
  group_by(caisse,cc,sexe,naiss,indicateur,type_correction) %>%
  fill(val,.direction="down") %>%
  fill(age,.direction="down") %>%
  mutate(varval_g_p1 = cumprod(varval_g_p1),
         val = val * varval_g_p1 ) %>%
  ungroup() %>%
  select(caisse,cc,source,sexe,naiss,generation,indicateur,type_correction,val)

# == tests (sur les âges moyens de départ)

# ageretr %>% filter(cc=="0010" & naiss=="France") %>% ggplot(aes(y=ageliq,x=generation,colour=sexe,linetype=champ_obs,group=paste(sexe,champ_obs))) + geom_line() + labs(title="Âge moyen de départ à la retraite",caption = "Champ : retraités, nés en France.\nSource : DREES, EACR.")
# ageretr %>% filter(cc=="0012" & naiss=="Ensemble") %>% ggplot(aes(y=ageliq,x=generation,colour=sexe,linetype=champ_obs,group=paste(sexe,champ_obs))) + geom_line() + labs(title="Âge moyen de départ à la retraite",caption = "Champ : retraités, tous lieux de naissance.\nSource : DREES, EACR.")

# tabc %>% filter(cc=="5600" & naiss=="Ensemble") %>% ggplot(aes(y=ageliq,x=generation,colour=age,group=age)) + geom_line() + facet_grid(~sexe) + labs(title="Âge moyen de départ à la retraite, selon l'âge d'observation",caption = "Champ : nés en France.\nSource : DREES, EACR.")
# tabc %>% filter(cc=="5600" & naiss=="Ensemble") %>% ggplot(aes(y=ageliq,x=annee,colour=age,group=age)) + geom_line() + facet_grid(~sexe) + labs(title="Âge moyen de départ à la retraite, selon l'âge d'observation",caption = "Champ : nés en France.\nSource : DREES, EACR.")

# ageretr %>% filter(cc %in% c("0010","0015") & naiss=="Ensemble" & champ_obs=="Retraités à 67 ans") %>% ggplot(aes(y=ageliq,x=generation,colour=sexe,linetype=cc,group=paste(sexe,cc))) + geom_line() + labs(title="Âge moyen de départ à la retraite",caption = "Champ : retraités, nés en France.\nSource : DREES, EACR.")
# ageretr %>% filter(cc %in% c("6000","5600") & naiss=="Ensemble" & champ_obs=="Retraités à 67 ans") %>% ggplot(aes(y=ageliq,x=generation,colour=sexe,linetype=cc,group=paste(sexe,cc))) + geom_line() + labs(title="Âge moyen de départ à la retraite",caption = "Champ : retraités, nés en France.\nSource : DREES, EACR.")
# ageretr %>% filter(cc %in% c("6000","5600","5000") & naiss=="Ensemble" & champ_obs=="Retraités à 67 ans") %>% ggplot(aes(y=ageliq,x=generation,colour=sexe,linetype=cc,group=paste(sexe,cc))) + geom_line() + facet_grid(~sexe) + labs(title="Âge moyen de départ à la retraite",caption = "Champ : retraités, nés en France.\nSource : DREES, EACR.")
# ageretr %>% filter(cc %in% c("6000","5600","0010") & naiss=="Ensemble" & champ_obs=="Retraités à 67 ans") %>% ggplot(aes(y=ageliq,x=generation,colour=sexe,linetype=cc,group=paste(sexe,cc))) + geom_line() + facet_grid(~sexe) + labs(title="Âge moyen de départ à la retraite",caption = "Champ : retraités, nés en France.\nSource : DREES, EACR.")
# ageretr %>% filter(cc %in% c("0010","0015","0042") & naiss=="Ensemble" & champ_obs=="Retraités à 67 ans") %>% ggplot(aes(y=ageliq,x=generation,colour=sexe,linetype=cc,group=paste(sexe,cc))) + geom_line() + facet_grid(~sexe) + labs(title="Âge moyen de départ à la retraite",caption = "Champ : retraités, nés en France.\nSource : DREES, EACR.")
# ageretr %>% filter(cc %in% c("0012","0032","0033","_FPc") & champ_obs=="Retraités à 67 ans") %>% ggplot(aes(y=ageliq,x=generation,colour=cc,group=paste(sexe,cc))) + geom_line() + facet_grid(naiss~sexe) + labs(title="Âge moyen de départ à la retraite",caption = "Champ : retraités, nés en France.\nSource : DREES, EACR.")
# ageretr %>% filter(cc %in% c("0012","0032","0013","__FP") & champ_obs=="Retraités à 67 ans") %>% ggplot(aes(y=ageliq,x=generation,colour=cc,group=paste(sexe,cc))) + geom_line() + facet_grid(naiss~sexe) + labs(title="Âge moyen de départ à la retraite",caption = "Champ : retraités, nés en France.\nSource : DREES, EACR.")
# ageretr %>% filter(cc %in% c("0022","0040","0050","Indp") & champ_obs=="Retraités à 67 ans") %>% ggplot(aes(y=ageliq,x=generation,colour=cc,group=paste(sexe,cc))) + geom_line() + facet_grid(naiss~sexe) + labs(title="Âge moyen de départ à la retraite",caption = "Champ : retraités, nés en France.\nSource : DREES, EACR.")
# ageretr %>% filter(cc %in% c("0060","0100","0300","0500","RSpé") & champ_obs=="Retraités à 67 ans") %>% ggplot(aes(y=ageliq,x=generation,colour=cc,group=paste(sexe,cc))) + geom_line() + facet_grid(naiss~sexe) + labs(title="Âge moyen de départ à la retraite",caption = "Champ : retraités, nés en France.\nSource : DREES, EACR.")

# ageretr %>% filter(naiss=="Ensemble") %>% ggplot(aes(y=ageliq,x=generation,colour=sexe,linetype=champ_obs,group=paste(sexe,champ_obs))) + geom_line() + facet_wrap(~caisse,scales="free") + labs(title="Âge moyen de départ à la retraite",caption = "Champ : nés en France.\nSource : DREES, EACR.")

# == tests (sur les évolutions entre générations d'âges moyens de départ)

# vargen %>% filter(cc=="0010" & naiss=="Ensemble") %>% mutate(annee=generation+age) %>% ggplot(aes(y=varageliq_g_p1,x=annee,colour=age,group=age)) + geom_line() + facet_grid(~sexe) + labs(title="Taux d'évolution de l'âge moyen de départ à la retraite, selon l'âge d'observation",caption = "Champ : nés en France.\nSource : DREES, EACR.")
# vargen %>% filter(cc=="6000" & naiss=="Ensemble") %>% mutate(annee=generation+age) %>% ggplot(aes(y=varageliq_g_p1,x=generation,colour=age,group=age)) + geom_line() + facet_grid(~sexe) + labs(title="Taux d'évolution de l'âge moyen de départ à la retraite, selon l'âge d'observation",caption = "Champ : nés en France.\nSource : DREES, EACR.")


# == sortie graphique pour l'analyse des résultats

if (FALSE) {

    library(patchwork)

    tabloc <- ageretr %>%
      filter(champ_obs=="Retraités à 67 ans")

    graphsloc <- tabloc %>% distinct(cc,caisse,naiss) %>% arrange(cc,naiss)

    theme_set(theme_bw())

    graph_loc <- function(k) {
      tabloc %>%
        filter(cc==graphsloc$cc[k] & naiss==graphsloc$naiss[k]) %>%
        ggplot( aes(y=ageliq,x=generation,group=sexe) ) +
        geom_line(size=1.25,colour="darkblue") +
        geom_line(data = agetabc %>%
                    filter(cc==graphsloc$cc[k] & naiss==graphsloc$naiss[k]),
                  aes(group=paste(age,sexe)),
                  size=0.25,colour="gray") +
        facet_grid(~sexe) +
        theme(legend.position="top",  plot.title.position="plot") +
        labs( x=NULL,y=NULL,caption=NULL,
              title=NULL,
              subtitle=paste0("Régime = ",graphsloc$caisse[k]," (",graphsloc$cc[k],"). Lieu de naissance = ",graphsloc$naiss[k]))
    }

    graph_verif <- tabloc %>%
      filter(indicateur==indicloc & naiss=="Ensemble") %>%
      ggplot( aes(y=valeur,x=generation,group=cc,colour=cc) ) +
      geom_line() +
      facet_grid(~sexe) +
      theme(legend.position="top",  plot.title.position="plot") +
      labs( x=NULL,y=NULL,caption=NULL,
            title=indicloc,
            subtitle=paste0("Lieu de naissance = ",graphsloc$naiss[k]))

    graph_verif <- graph_loc(1)
    for (i in c(2:nrow(graphsloc))) {graph_verif <- graph_verif /  graph_loc(i)}

    dirtempsave <- "C:/Users/PA/Downloads/"
    ggsave(paste0(dirtempsave,"graph_verif_ageretr.pdf"),
           plot = graph_verif,
           width = 16, height = 8*nrow(graphsloc), dpi = 320, units = "cm",limitsize=FALSE)

}


# ========= sauvegarde des tables

usethis::use_data(ageretr, overwrite=TRUE)

# -- sauvegarde d'une version .csv

write.csv2(ageretr,
           file=gzfile("data-raw/ageretr.csv.gz"),
           row.names = FALSE, fileEncoding = "UTF-8")
