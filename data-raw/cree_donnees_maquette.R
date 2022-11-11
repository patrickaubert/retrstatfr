
library(tidyverse)
library(healthexpectancies)
library(janitor)
library(openxlsx)
library(broom)

library(devtools)
devtools::load_all()


# =========  1) récupération des données

genmin <- 1942
genmax <- 2010

# -- taux de retraités (DREES, modèle ANCETRE, publiés dans le panorama R&R)

# txretrgen <- txretr %>% select(-geo,-txnouvretr) %>% mutate(generation=annee-age)
# txretrgen %>% filter(age<64,age>=60) %>% left_join(txretrgen %>% filter(age==64) %>% select(generation,sexe,txretr) %>% rename(txretr64=txretr), by=c("generation","sexe")) %>% mutate(txretrrel=100*txretr/txretr64) %>% ggplot(aes(y=txretrrel,x=generation,group=age,colour=age)) + geom_line() + facet_grid(~sexe)

# -- paramètres déterminantes l'âge : AOD, AAD, durée requise (source : barèmes IPP)

#url_aod <- "https://www.ipp.eu/wp-content/themes/ipp/baremes/regimes-de-retraites/0/0/aod/aod.csv"
#url_aad <- "https://www.ipp.eu/wp-content/themes/ipp/baremes/regimes-de-retraites/0/0/aad/aad.csv"
#url_dureetp <- "https://www.ipp.eu/wp-content/themes/ipp/baremes/regimes-de-retraites/0/0/trimtp/trimtp.csv"

ageslegaux <- data.frame(
  generation=c(genmin,1951:1955),
  moisnaiss=c(1,7,rep(1,4)),
  aod=c(60,60+4/12,60+9/12,60+14/12,60+19/12,62),
  aad=c(65,65+4/12,65+9/12,65+14/12,65+19/12,67),
  stringsAsFactors = FALSE)%>%
  mutate_all(as.numeric)

dureereq <- data.frame(
  generation=c(1910,1934:1943,1949:1953,1955,1958,1961,1964,1967,1970,1973),
  dureetp=c(150:172),
  stringsAsFactors = FALSE)%>%
  mutate_all(as.numeric)
dureereq <- data.frame(generation=c(1910:genmax)) %>%
  left_join(dureereq, by=c("generation")) %>%
  fill(dureetp,.direction="down") %>%
  filter(generation>=genmin)

indicages <- expand.grid(
  generation=c(genmin:genmax),
  moisnaiss=c(1:12),
  age=c(60:67),
  stringsAsFactors = FALSE) %>%
  mutate_all(as.numeric) %>%
  left_join(ageslegaux, by=c("generation","moisnaiss")) %>%
  arrange(age,generation,moisnaiss) %>%
  group_by(age) %>% fill(aod,aad, .direction="down") %>% ungroup() %>%
  mutate(i_aod=1*((1-moisnaiss/12)-1/12+age>=aod),   # RQ : le -1/12 est là pour tenir compte du fait qu'on ne peut liquider qu'au 1er du mois SUIVANT sa date anniversaire (donc dans l'année des 61 ans si l'AOD est à 60 ans pour les nés en décembre)
         i_aad=1*((1-moisnaiss/12)-1/12+age>=aad),
         i_aodp1=1*((1-moisnaiss/12)-1/12+age>=aod+1), # car la hausse du taux de retraité entre AOD et AOD+1 semble plus forte qu'aux âges suivant + idem entre AAD et AAD+1 an
         i_aadp1=1*((1-moisnaiss/12)-1/12+age>=aad+1)) %>%
  select(-aod,-aad,-moisnaiss) %>%
  group_by(age,generation) %>% summarise_all(mean) %>% ungroup()


# -- durées validées par génération, année, sexe et tranche d'âge quinquenal (EIC2017, publié sous data.drees)

dureesval_ <- extrait_opendata("les droits validés en cours de carrière (EIC 2017)")

dureesval <- dureesval_ %>%
  janitor::clean_names() %>%
  filter(champ=="Aff_avant_23ans",
         #grepl("^Aff_avant_201(3|7)",champ),
         type_trim %in% c("Validé","Cotisé"),
         ((age==min(dureesval_$Âge) & type_indic=="Cum_trim") | (age>min(dureesval_$Âge) & type_indic=="Nb_trim")),
         generation+age<=2017) %>%
  select(generation,type_trim,age,sexe,valeur) %>%
  mutate(valeur=(valeur %>%
                   str_replace(",",".") %>% str_replace("#NOMBRE\\!","0") %>%
                   as.numeric())/4)

gendispo <- unique(dureesval$generation)

durees_agefin <- expand.grid(generation=c(genmin:genmax),age=c(16:65),sexe=unique(dureesval$sexe),type_trim=c("Validé","Cotisé")) %>%
  filter(generation+age<=2017) %>%
  left_join(dureesval, by=c("age","generation","sexe","type_trim")) %>%
  mutate(generationavt = lapply(
    generation, function(x){max(gendispo[gendispo<=x])}) %>%  unlist() ,
    generationapr = lapply(
      generation, function(x){min(gendispo[gendispo>=x])}) %>%  unlist() ) %>%
  left_join(dureesval %>% rename(valeurapr=valeur),
            by=c("age","generationapr"="generation","sexe","type_trim")) %>%
  left_join(dureesval %>% rename(valeuravt=valeur),
            by=c("age","generationavt"="generation","sexe","type_trim")) %>%
  mutate(valeurobs = valeur,
         valeur = case_when(
           !is.na(valeur) ~ valeur,
           # traitement de l'obligation de scolarité jusqu'à 16 ans à partir de la génération 1953
           !is.na(valeuravt) & (age==16) & (generation %in% c(1950:1952)) ~  valeuravt,
           !is.na(valeuravt) & (age==16) & (generation %in% c(1953:1954)) ~  valeurapr,
           # cas génération sinon : moyenne pondérée des valeurs avant et après
           !is.na(valeuravt) & !is.na(valeurapr) ~  valeuravt*(generationapr-generation)/(generationapr-generationavt)+valeurapr*(generation-generationavt)/(generationapr-generationavt),
           !is.na(valeuravt) ~ valeuravt) ) %>%
  select(-ends_with("avt"),-ends_with("apr"))

durees <- durees_agefin %>%
  mutate(fintranche=5*floor(age/5)+4,
         age=cut(age,breaks=seq(15,70,5),include_lowest=TRUE,right=FALSE)    ) %>%
  filter(generation+fintranche<=2017) %>% select(-fintranche) %>%
  group_by(type_trim,generation,age,sexe) %>% summarise_all(sum) %>% ungroup() %>%
  arrange(type_trim,sexe,age,generation)

#durees %>% filter(type_trim=="Validé") %>% ggplot(aes(y=valeur,x=generation,colour=age,group=age)) + geom_line() + facet_grid(~sexe)


# -- durées cumulées avant 55 ans

dureesgen55 <- durees %>%
  arrange(sexe,generation,age) %>%
  filter(age %in% c("[15,20)", "[20,25)", "[25,30)", "[30,35)","[35,40)", "[40,45)", "[45,50)", "[50,55)")) %>%
  select(generation,sexe,type_trim, age,valeur) %>%
  pivot_wider(id_cols=c("generation","sexe","age"),names_from="type_trim",values_from="valeur") %>%
  select(-age) %>%
  group_by(generation,sexe) %>% summarise(dureeval55=sum(Validé),dureecot55=sum(Cotisé)) %>% ungroup()

#dureesgen55 %>% ggplot(aes(x=generation,colour=sexe,group=sexe)) + geom_line(aes(y=dureeval55),linetype="solid") + geom_line(aes(y=dureecot55),linetype="dashed")
#dureesgen55 %>% ggplot(aes(y=dureecot55,x=generation,colour=sexe,group=sexe)) + geom_line()
#dureesgen55 %>% left_join(dureereq, by=c("generation")) %>% ggplot(aes(y=dureeval55/(dureetp/4),x=generation,colour=sexe,group=sexe)) + geom_line()

# -- durées validées avant 20 ans (cumulées), par sexe et génération

dureesavt20 <- durees_agefin %>%
  janitor::clean_names() %>%
  filter(age<=20,   generation+age<=2017,type_trim=="Validé") %>%
  select(generation,sexe,age,valeur) %>%
  arrange(sexe,generation,age) %>%
  group_by(sexe,generation) %>% mutate(cumnb=cumsum(valeur)) %>% ungroup() %>%
  select(sexe,generation,age,cumnb) %>%
  filter(age %in% c(16,17,20))

# dureesavt20 %>% ggplot(aes(y=cumnb,x=generation,colour=age,group=age)) + geom_line() + facet_grid(~sexe)

# dureesval_ %>% filter(Type_trim=="Validé",Champ=="Aff_avant_23ans",Type_indic=="Part_aff",Âge==19) %>% ggplot(aes(y=valeur,x=Génération,colour=Sexe,group=Sexe)) + geom_line()


# -- taux d'emploi et d'activité, par sexe et âge quinquennal (observations et projections Insee)

url_projpa <- "https://www.insee.fr/fr/statistiques/fichier/6472810/PPA-2022.xlsx"

ppa <- read.xlsx(xlsxFile = url_projpa,  sheet="taux_activité")

nbtr <- (ncol(ppa)-1)/3
names(ppa) <- paste(c("",rep("Ensemble",nbtr),rep("Femmes",nbtr),rep("Hommes",nbtr)),ppa[1,])

ppa <- ppa[2:nrow(ppa),] %>%
  pivot_longer(cols=-" Année",names_to="sexeage",values_to="txact") %>%
  janitor::clean_names() %>%
  mutate(txact=as.numeric(txact)/100,
         annee=as.numeric(annee),
         sexe = str_extract(sexeage,"^[^[:space:]]+(?=[[:space:]])"),
         debage = str_extract(sexeage,"(?<=[[:space:]])[[:digit:]]{2}") %>% as.numeric(),
         age = paste0("[",debage,",",ifelse(debage=="70","Inf",as.numeric(debage)+5),")") %>%
           factor() )

ppagen <- expand.grid(generation=c(1946:2010),age=c(16:65),sexe=unique(dureesval$sexe)) %>%
  mutate(debage=5*floor(age/5),
         annee=generation+age) %>%
  filter(generation+debage>=1975) %>% select(-age) %>%
  left_join(ppa, by=c("annee","sexe","debage")) %>%
  select(-annee,-sexeage,-debage) %>%
  group_by(generation,age,sexe) %>% summarise(cumtxact=sum(txact)) %>% ungroup() %>%
  arrange(sexe,age,generation)

# -- population observée et projetée (Insee)

#durees %>% filter(sexe=="Ensemble") %>%   ggplot(aes(y=valeur,x=generation,colour=age,group=age)) + geom_line(aes(y=valeur)) + geom_point(aes(y=valeurobs))
#ppagen %>% filter(sexe=="Ensemble") %>%   ggplot(aes(y=cumtxact,x=generation,colour=age,group=age)) + geom_line(aes(y=cumtxact))
#bind_rows(durees %>% filter(type_trim=="Validé") %>% mutate(source="valid EIC"),ppagen %>% mutate(source="cum txact") %>% rename(valeur=cumtxact)) %>% filter(sexe=="Hommes") %>%   ggplot(aes(x=generation,colour=age,linetype=source,group=paste0(age,source))) + geom_line(aes(y=valeur)) + scale_linetype_manual(values=c("dotted","solid"))+ geom_point(aes(y=valeurobs))


# =========  2) bases complètes et estimations


# -- taux de retraités, en fonction des indicatrices vis-à-vis des paramètres légaux et de la durée validée

debobsan <- min(txretr$annee)
finobsan <- max(txretr$annee)

base_genage <- expand.grid(generation=c((debobsan-70):(2017-55)),
                           sexe=c("Ensemble","Femmes","Hommes"),
                           age=c(50:70) ) %>%
  mutate_at(vars("generation","age"),as.numeric) %>%
  mutate(annee=generation+age) %>%
  left_join( txretr %>%
               select(-geo,-txnouvretr) %>%
               mutate(sexe = recode(sexe, "ensemble"="Ensemble","hommes"="Hommes","femmes"="Femmes"),
                      generation=annee-age),
             by=c("generation","age","annee","sexe")) %>%
  #filter(generation>=genmin) %>%
  left_join(dureesgen55, by=c("generation","sexe")) %>%
  left_join(dureesavt20 %>% pivot_wider(id_cols=-c("age","cumnb"),names_from="age",names_prefix="cumnbavt",values_from="cumnb"),
            by=c("generation","sexe")) %>%
  filter( annee>=debobsan, annee<=2025) %>% #2017-generation>=55,
  left_join(indicages, by=c("generation","age")) %>%
  left_join(dureereq, by=c("generation")) %>%
  mutate(# indicateur de la durée validée par génération, relative à la durée requise
         cp=dureeval55/(dureetp/4),
         cpage=(dureeval55+(age-55))/(dureetp/4),
         agexcp=(age-60)*cp,
         cpcotage=(dureecot55+(age-55))/(dureetp/4),
         # indicatrices de position par rapport aux bornes d'âge
         i_aod = case_when(
           !is.na(i_aod) ~ i_aod,
           is.na(i_aod) & age<60 ~ 0,
           is.na(i_aod) & age>62 ~ 1 ),
         i_aad = case_when(
           !is.na(i_aad) ~ i_aad,
           is.na(i_aad) & age<65 ~ 0,
           is.na(i_aad) & age>67 ~ 1 ),
         i_aodp1= case_when(
           !is.na(i_aodp1) ~ i_aodp1,
           is.na(i_aodp1) & age<61 ~ 0,
           is.na(i_aodp1) & age>63 ~ 1 ),
         i_aadp1= case_when(
           !is.na(i_aadp1) ~ i_aadp1,
           is.na(i_aadp1) & age<66 ~ 0,
           is.na(i_aadp1) & age>68 ~ 1 ),
         i_int = i_aod-i_aad,
         # RACL et cumul de trimestres avant l'âge considéré
         i_racl60=(1-i_aod)*(age==60 & annee>=2012),
         cumt_racl60=i_racl60*cumnbavt20,
         i_racl61=(1-i_aod)*(age==61 & annee>=2012),
         cumt_racl61=i_racl61*cumnbavt20,
         i_racl2004=1*(age>=56 & age<=59 & annee>=2004),
         age_racl2004=(age-55)*i_racl2004,
         cumt_racl2004=i_racl2004*((age %in% c(56:58))*cumnbavt16 + (age==59)*cumnbavt17),
         # indicateur d'âge (observé) croisé avec les indicatrices de position par rapport aux bornées d'âge
         age_int=(age-60)*i_int,
         age_avtaod=(age-55)*(1-i_aod),
         # croisement des indicateurs de durée validée (relative) et de position par rapport aux bornes d'âge
         cp_int=cp*i_int,
         cp_avtaod=cp*(1-i_aod),
         cpage_int=cpage*i_int,
         agexcp_int=agexcp*i_int,
         cpage_avtaod=cpage*(1-i_aod),
         cpcotage_avtaod=cpcotage*(1-i_aod),
         cpage_avt60=cpage*(age<60),
         cpcotage_avt60=cpcotage*(age<60),
         cpage_racl60=cpage*(1-i_aod)*(age>=60 & annee>=2012),
         cpcotage_racl60=cpcotage*(1-i_aod)*(age>=60 & annee>=2012)
        ) %>%
  select(-dureeval55,-dureecot55,-dureetp)


# inspiration = https://bitcoden.com/answers/fitting-several-regression-models-with-dplyr
#version par sexe
#fitted_model_txretr <- base_genage %>%
#  filter(generation>=genmin, age>=50, annee<=finan ) %>%
#  nest_by(sexe) %>%
#  mutate(model = list(lm(txretr ~ i_int + #i_aodp1 +
#                           cpage_int + #age_int + cp_int +
#                           i_aad + #i_aadp1 +
#                           cpage_avt60 + # age_avtaod + cp_avaod +
#                           i_racl60 + cumt_racl60 + i_racl61 + cumt_racl61 + cpcotage_racl60 +
#                           i_racl2004 + age_racl2004 + cumt_racl2004 ,
#                         data = data)))

# version unisexe
fitted_model_txretr_unisx <- lm(txretr ~ i_int + agexcp_int + #cpage_int + #i_aodp1 + age_int + cp_int +
                                  i_aad + #i_aadp1 +
                                  cpcotage_avt60 + # age_avtaod + cp_avaod +
                                  i_racl60 + cumt_racl60 + i_racl61 + cumt_racl61 + cpcotage_racl60 +
                                  i_racl2004 + age_racl2004 + cumt_racl2004 ,
                                data = base_genage %>%  filter(generation>=genmin,age>=50, annee<=finobsan, sexe!="Ensemble", generation %in% unique(dureesval$generation) ) )


#fitted_model_txretr <- base_genage %>%
#  filter(age>=55) %>%
#  mutate(model = list(lm(txretr ~ i_aod + i_aodp1 + age_int + cp_int +  i_aad + i_aadp1 + age_avtaod + cp_avaod, data = data)))
# faire tous sexes confondus, en croisant toutes les indicatrices (aod, aad, aod+1 etc) avec le CP ?
# idem en ajoutant 2*8 trimestres de MDA pour les femmes ?
# idem en enlevant l'effet âge, maisa avec CP calculé comme : duree55+(age-55) + MDA ?

#txpred <- fitted_model_txretr %>% summarize(augment(model))
#txpred <- fitted_model_txretr %>% summarize(augment_columns(model))

  # coefficients : régression par sexe
#coeff <- fitted_model_txretr %>% summarize(tidy(model)) %>%
#  select(sexe,term,estimate) %>%
#  mutate(term=paste("coeff",term),sep="_") %>%
#  pivot_wider(id_cols="sexe",values_from=estimate,names_from=term) %>%
#  janitor::clean_names()
#coeff_long <- coeff %>%
#  pivot_longer(cols=-"sexe",names_to="nomvar",values_to="coeff") %>%
#  mutate(nomvar = nomvar %>% str_replace("^coeff_",""))
#visu_genage <- base_genage %>%
#  filter(age>=50) %>%
#  mutate(intercept=1) %>%
#  pivot_longer(cols=-c("annee","age","generation","sexe","txretr"),names_to="nomvar",values_to="vals") %>%
#  full_join(coeff_long, by=c("sexe","nomvar")) %>%
#  filter(!is.na(coeff)) %>%
#  group_by(annee,age,generation,sexe) %>% summarise(txretr=mean(txretr),txretr_pred=sum(coeff*vals)) %>% ungroup()


  # coefficients : régression unisexe
coeff <- fitted_model_txretr_unisx %>% broom::tidy() %>%
  select(term,estimate) %>%
  mutate(term=paste("coeff",term),sep="_") %>%
  pivot_wider(id_cols=c(),values_from=estimate,names_from=term) %>%
  janitor::clean_names()
coeff_long <- coeff %>%
  pivot_longer(cols=names(coeff),names_to="nomvar",values_to="coeff") %>%
  mutate(nomvar = nomvar %>% str_replace("^coeff_",""))
visu_genage <- base_genage %>%
  filter(age>=50) %>%
  mutate(intercept=1) %>%
  pivot_longer(cols=-c("annee","age","generation","sexe","txretr"),names_to="nomvar",values_to="vals") %>%
  full_join(coeff_long, by=c("nomvar")) %>%
  filter(!is.na(coeff)) %>%
  group_by(annee,age,generation,sexe) %>% summarise(txretr=mean(txretr),txretr_pred=sum(coeff*vals)) %>% ungroup()


#visu_genage <- base_genage %>%
#  filter(age>=55) %>%
#  left_join(coeff, by="sexe") %>%
#  mutate(txretr_pred=coeff_intercept +
#           coeff_age_avtaod*age_avtaod + coeff_cp_avaod*cp_avaod +
#           coeff_i_aod*i_aod +coeff_i_aodp1*i_aodp1 +
#           coeff_age_int*age_int + coeff_cp_int*cp_int +
#           coeff_i_aad*i_aad+  coeff_i_aadp1*i_aadp1)

visu_genage %>%
  mutate(age=as.factor(age)) %>%
  ggplot(aes(y=txretr,x=annee,colour=age,group=age)) +
  geom_line() + geom_line(aes(y=txretr_pred),linetype="dotted") +
  facet_grid(~sexe)

visu_genage %>%
  mutate(age=as.factor(age)) %>%
  filter(generation %in% c(1942:1962)) %>%
  ggplot(aes(y=txretr,x=generation,colour=age,group=age)) +
  geom_line() + geom_line(aes(y=txretr_pred),linetype="dashed") +
  geom_point(data=visu_genage %>%
               mutate(age=as.factor(age))%>% filter(generation %in% intersect(c(1942:1960),as.numeric(unique(dureesval_$Génération)))) ) +
  geom_vline(xintercept = 1952.5) +
  facet_grid(~sexe)

# A FAIRE !!
# à vérifier : pourquoi baisse à 60,61,62 à partir de la génération 1958 ??? (qqch sur cat actives FP ?)
# restreindre l'estimation aux générations réellement observées dans l'EIC
# calculer d'abord un part des départs à l'AAD par génération*sexe à partir des Txretr, puis le simuler en fonction des durées EIC, puis mettre à la fois cette part (par sexe) et la durée moyenne + (age/2) (ou bien simplement : l'âge depuis l'AOD ?) comme facteur explicatif
# il semble qu'il y ait un effet "âge depuis l'AOD" : les taux de retraités à 62, 63 et 64 ans baissent de façon quasi parallèle, pour les deux sexes, entre les gén 1950 et 1955, d'un façon trop forte pour être expliquée par l'évol de la seule durée moyenne/durée req
# vérifier ce dernier effet avec taux de retraités calculés à partir des données annuaire GIP, récupérées dans panorama R&R (fiche sur les affiliés)

# =========== sauvegarde des bases

# --- données sources : sous la forme maquette_data_genmens (données par génération*mois de naissance), ..._gen, ..._genan, etc.
