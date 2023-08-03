library(devtools)
load_all()
library(tidyverse)

# ===================================================================================
# crée une base avec les coefficients de revalorisation par régimes (source : barèmes IPP)
# ===================================================================================

# remarque : cette table est destinée en premier lieu à être utilisée en complément des données tous régimes
# diffusées par la DREES. On utilise donc la même nomenclature pour les régimes : code "CC"

# -- régimes de base

tab_rev_rg <- read.csv2(  "https://www.ipp.eu/wp-content/themes/ipp/baremes/regimes-de-retraites/0/0/revalorisation_pension/revalorisation_pension.csv",sep=",",header=TRUE)
names(tab_rev_rg) <- c("date","coeff")
tab_rev_rg <- tab_rev_rg %>% mutate( date = as.Date(date , format="%Y-%m-%d"))

tab_rev_fp <- read.csv2("https://www.ipp.eu/wp-content/themes/ipp/baremes/regimes-de-retraites/1/pension_civile/revalorisation_pension/revalorisation_pension.csv",sep=",",header=TRUE)
names(tab_rev_fp) <- c("date","coeff")
tab_rev_fp <- tab_rev_fp %>% mutate( date = as.Date(date , format="%Y-%m-%d"))

# on complète les valeurs manquantes pour la FP
tab_rev_fp <- bind_rows(tab_rev_rg %>% filter(date>"2022-01-01"), tab_rev_fp)

tab_rev_base <- bind_rows(tab_rev_rg %>% mutate(cc="0010"), tab_rev_fp %>% mutate(cc="0012")) %>%
  arrange(cc,date) %>%
  group_by(cc) %>% mutate(indicerevalo=cumprod(coeff)) %>% ungroup() %>%
  select(-coeff)

# -- régimes complémentaires

#tab_rev_agircarrco <- read.csv2("https://www.ipp.eu/wp-content/themes/ipp/baremes/regimes-de-retraites/0/1/0/agirc_arrco/point/point.csv",sep=",",header=TRUE)
tab_rev_arrco <- read.csv2("https://www.ipp.eu/wp-content/themes/ipp/baremes/regimes-de-retraites/0/1/0/arrco/point/point.csv",sep=",",header=TRUE)
tab_rev_agirc <- read.csv2("https://www.ipp.eu/wp-content/themes/ipp/baremes/regimes-de-retraites/0/1/0/agirc/point/point.csv",sep=",",header=TRUE)
tab_rev_unirs <- read.csv2("https://www.ipp.eu/wp-content/themes/ipp/baremes/regimes-de-retraites/0/1/0/unirs/point/point.csv",sep=",",header=TRUE)
tab_rev_ircantec <- read.csv2("https://www.ipp.eu/wp-content/themes/ipp/baremes/regimes-de-retraites/1/regimes_complementaires/ircantec/ircantec.csv",sep=",",header=TRUE)
tab_rev_rci <- read.csv2("https://www.ipp.eu/wp-content/themes/ipp/baremes/regimes-de-retraites/2/pt_rci/pt_rci.csv",sep=",",header=TRUE)
tab_rev_rcia <- read.csv2("https://www.ipp.eu/wp-content/themes/ipp/baremes/regimes-de-retraites/2/pt_rc_art/pt_rc_art.csv",sep=",",header=TRUE)
tab_rev_rcic <- read.csv2("https://www.ipp.eu/wp-content/themes/ipp/baremes/regimes-de-retraites/2/pt_rc_com/pt_rc_com.csv",sep=",",header=TRUE)
tab_rev_rafp <- read.csv2("https://www.ipp.eu/wp-content/themes/ipp/baremes/regimes-de-retraites/1/regimes_complementaires/rafp/rafp.csv",sep=",",header=TRUE)

# -- on complète les valeurs manquantes (à enlever quand les séries auront été actualisées dans les barèmes IPP)

tab_rev_agircarrco <- data.frame(
  date= c("2019-11-01","2021-11-01","2022-11-01"),
  valeur_point_en_euros=c(1.2714,1.2841, 1.3498)
)

tab_rev_ircantec <- bind_rows(
  data.frame(
    date= c("2022-07-01","2023-01-01"),
    valeur_du_point=c(0.51211 , 0.51621),
    stringsAsFactors = FALSE
  ) %>%
    mutate(valeur_du_point= as.numeric(valeur_du_point)),
  tab_rev_ircantec  %>%
    mutate(valeur_du_point= as.numeric(valeur_du_point))
)

tab_rev_rafp <- bind_rows(
  data.frame(
    date= c("2023-01-01","2022-01-01"),
    valeur_service_point_rafp=c(0.05036,0.04764 ),
    valeur_acquisition_point_rafp=c(1.3466 ,1.2740),
    stringsAsFactors = FALSE   ) ,
  tab_rev_rafp  %>%
    mutate(valeur_service_point_rafp= as.numeric(valeur_service_point_rafp),
           valeur_acquisition_point_rafp= as.numeric(valeur_acquisition_point_rafp)) %>%
    # correction d'une erreur
    mutate(
      valeur_acquisition_point_rafp = ifelse(
        date=="2021-01-01",1.2502,
        valeur_acquisition_point_rafp )
    )
)

# -- on concatène tous les régimes complémentaires

tab_rev_compl <- bind_rows(

  # AGIRC-ARRCO après la fusion (2019) => on duplique les lignes pour les deux régimes + on crée le code caisse pour le régime fusionné
  tab_rev_agircarrco  %>%
    mutate(cc="6000"),
  tab_rev_agircarrco  %>%
    mutate(cc="5000"),
  tab_rev_agircarrco  %>%
    mutate(cc="5600"),

  # ARRCO jusqu'en 2019, précédé par l'UNIRS
  tab_rev_arrco %>% select(date,valeur_point_en_euros) %>%
    mutate(cc="6000",valeur_point_en_euros=as.numeric(valeur_point_en_euros)),
  tab_rev_unirs %>% select(date,valeur_point_en_euros) %>%
    mutate(cc="6000",valeur_point_en_euros=as.numeric(valeur_point_en_euros)/0.3874644222106022),

  # AGIRC jusqu'en 2019
  tab_rev_agirc %>% select(date,valeur_point_en_euros) %>%
    mutate(cc="5000",valeur_point_en_euros=as.numeric(valeur_point_en_euros)) %>%
    # conversion en valeur du point Agirc-Arrco après la fusion de 2019
    mutate(valeur_point_en_euros=valeur_point_en_euros*1.2588/0.4378),

  # IRCANTEC
  tab_rev_ircantec %>% select(date,valeur_du_point) %>%
    mutate(cc="1000",valeur_point_en_euros=as.numeric(valeur_du_point)),

  # RAFP
  tab_rev_rafp %>% select(date,valeur_service_point_rafp) %>%
    mutate(cc="3000",valeur_point_en_euros=as.numeric(valeur_service_point_rafp)) %>%
    filter(!is.na(valeur_point_en_euros)),

  # RCI
  tab_rev_rci %>% select(date,valeur_point_rci_date) %>%
    mutate(cc="0043",valeur_point_en_euros=as.numeric(valeur_point_rci_date)) %>%
    filter(!is.na(valeur_point_en_euros)),

  # RCI commerçant
  tab_rev_rci %>% select(date,valeur_point_rci_date) %>%
    mutate(cc="0041",valeur_point_en_euros=as.numeric(valeur_point_rci_date)) %>%
    filter(!is.na(valeur_point_en_euros)),
  tab_rev_rcic %>% select(date,valeur_points_acquis_depuis_1973) %>%
    mutate(cc="0041",valeur_point_en_euros=as.numeric(valeur_points_acquis_depuis_1973)) %>%
    filter(!is.na(valeur_point_en_euros))#,

  # RSI artisans (0051)
  # (RQ : on laisse de côté ce régime pour l'instant, car la revalorisation est hétérogène est dépend de la date d'acquisition des points)

  ) %>%
  mutate( date = as.Date(date , format="%Y-%m-%d")) %>%
  rename(indicerevalo=valeur_point_en_euros) %>%
  select(date,cc,indicerevalo) %>%
  filter(!is.na(indicerevalo)) %>%
  arrange(cc,date)

# -- ensemble des régimes

tab_rev <- expand.grid(
  annee=c(1949:2023) ,
  cc=as.vector(unique(  c(tab_rev_base$cc, tab_rev_compl$cc)) ),
  mois=c(1:12),
  stringsAsFactors = FALSE )  %>%
  #mutate(date = paste0(as.character(annee),"-",as.character(mois),"-",case_when(mois %in% c(1,3,5,7,8,10,12) ~ "31", mois==2 ~"28", TRUE ~"30" )) %>%
  mutate(date = paste0(as.character(annee),"-",as.character(mois),"-01") %>%
           as.Date( format="%Y-%m-%d")) %>%
  arrange(cc,date) %>%
  filter(date<=Sys.Date())

revalopensions <- tab_rev %>%
  left_join(bind_rows(tab_rev_base,tab_rev_compl), by=c("cc","date")) %>%
  arrange(cc,date) %>%
  group_by(cc) %>% fill(indicerevalo,.direction="down") %>% ungroup() %>%
  mutate(annee = format(date,format="%Y") %>% as.numeric(),
         mois = format(date,format="%m") %>% as.numeric(),
         jour = format(date,format="%d") %>% as.numeric()) %>%
  relocate("annee",.after="date")  %>%
  relocate("indicerevalo",.after="jour") %>%
  select(-jour) %>%
  filter(!is.na(indicerevalo))


# == sauvegarde des bases


usethis::use_data( revalopensions ,      overwrite=TRUE)

