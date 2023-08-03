library(devtools)
# load_all()

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

# -- données de l'EACR avec les âges moyens de liquidation observé à divers âges
tabc <- tabseacr[["C_Droits_directs"]] %>%
  filter(champ=="ddir" & liq=="Ensemble" & statut_sncf=="Ensemble" &
           !is.na(ageliq) &
           age >= 67) %>%
  # pour l'AGIRC et l'ARRCO, on supprime les observations des EACR 2008 et 2009, pour éviter le doublon avec l'EACR "rétrospective historique" de 2009
  filter(!(cc %in% c("5000","6000") & source %in% c("EACR 2008","EACR 2009") )) %>%
  # de même, pour l'IRCANTEC on supprime l'observation de l'EACR 2008 (doublon avec EACR_HISTO de 2009)
  filter(!(cc %in% c("1000") & source %in% c("EACR 2008") )) %>%
  # CNAVPL : ageliq non renseigné (mis à 0)
  filter(cc!="2100") %>%
  # CNBF : valeurs manifestement aberrantes
  filter(!(cc %in% c("2201","2202"))) %>%
  # on supprime la catégorie d'âge maximale (variable selon les années) car il s'agit en réalité d'un regroupement de toutes les générations plus âgées
  group_by(annee,cc) %>% filter(age < max(age)) %>% ungroup() %>%
  select(annee,caisse,cc,source,sexe,naiss, age,ageliq,effectifs) %>%
  mutate(generation=annee-age) %>%
  arrange(cc,sexe,naiss,age,generation)

# -- ajout des sexe == "Ensemble" pour les régimes où c'est manquant

tabc <- bind_rows(
  # données de base
  tabc,
  # ajout de sexe = Ensemble pour les ventilations par lieu de naissance
  tabc %>%
    filter(naiss != "Ensemble") %>%
    mutate(sexe = "Ensemble") %>%
    group_by(annee,caisse,cc,source,sexe,naiss,age,generation) %>%
    summarise(ageliq = mean(ageliq,w=effectifs),
              effectifs = sum(effectifs)) %>%
    ungroup()
  )

# -- ajout de certains groupes de régimes (en supposant que l'affiliation à un régime au sein de chaque groupe est exclusive)

# verif <- tabc %>% count(cc,annee) %>% pivot_wider(names_from="cc",values_from="n") %>% arrange(annee)

tabc <- bind_rows(
  # données de base
  tabc,
  # régimes de la Fonction publique civile
 tabc %>% filter(cc %in% c("0012","0032","0033") & annee>=2014) %>%
    mutate(cc = "_FPc", caisse = "Fonction publique civile", source = "Recalculé") %>%
    group_by(cc,caisse,source,sexe,naiss,generation,annee,age) %>%
    summarise(ageliq = weighted.mean(ageliq,w=effectifs),
              effectifs = sum(effectifs),
              nbreg = n() ) %>%
    filter(nbreg == 3) %>%
    ungroup(),
 # régimes de la Fonction publique y compris militaire
 tabc %>% filter(cc %in% c("0012","0013","0032","0033") & annee>=2014) %>%
   mutate(cc = "__FP", caisse = "Fonction publique y compris militaires", source = "Recalculé") %>%
   group_by(cc,caisse,source,sexe,naiss,generation,annee,age) %>%
   summarise(ageliq = weighted.mean(ageliq,w=effectifs),
             effectifs = sum(effectifs),
             nbreg = n()) %>%
   filter(nbreg == 4) %>%
   ungroup(),
 # régimes spéciaux (SNCF, RATP, CNIEG, CRPCEN)
 tabc %>% filter(cc %in% c("0060","0100","0300","0500") & annee>=2011) %>%
   mutate(cc = "RSpé", caisse = "Régimes spéciaux (SNCF, RATP, CNIEG, CRPCEN)", source = "Recalculé") %>%
   group_by(cc,caisse,source,sexe,naiss,generation,annee,age) %>%
   summarise(ageliq = weighted.mean(ageliq,w=effectifs),
             effectifs = sum(effectifs),
             nbreg = n()) %>%
   filter(nbreg == 4) %>%
   ungroup(),
 # régimes d'indépendants (hors PL)
 tabc %>% filter(cc %in% c("0022","0040","0050","0042") & annee>=2015 & annee<=2019) %>%
   mutate(cc = "Indp", caisse = "Indépendants (hors libéraux)", source = "Recalculé") %>%
   group_by(cc,caisse,source,sexe,naiss,generation,annee,age) %>%
   summarise(ageliq = weighted.mean(ageliq,w=effectifs),
             effectifs = sum(effectifs),
             nbreg = n()) %>%
   filter(nbreg %in% c(2,3) ) %>%
   ungroup()
  )  %>%
  select(-effectifs, -nbreg)

# écart (en %) par rapport à la génération née 1 an plus tard, à âge donné

vargen <- tabc %>% select(-annee,-source) %>%
  left_join(tabc %>% select(caisse,cc,sexe,naiss,age,generation,ageliq) %>%
              mutate(generation=generation-1) %>%
              rename(ageliq_g_p1 = ageliq),
            by=c("caisse","cc","sexe","naiss","age","generation")) %>%
  mutate(varageliq_g_p1=ageliq/ageliq_g_p1) %>%
  filter(!is.na(varageliq_g_p1))

# tests sur les évolutions par régime x année, afin de détecter d'éventuelle ruptures de séries

  effets <- vargen %>%
    mutate(lvarage = log(varageliq_g_p1),
           annee = factor(generation+age),
           generation = factor(generation)) %>%
    filter(!is.na(lvarage)) %>%
    group_by(cc,sexe,naiss) %>%
    filter( NROW(unique(annee))>1 & NROW(unique(generation))>1 )  %>%
    ungroup() %>%
    nest_by(cc,sexe,naiss) %>%
    mutate(model = list(lm(lvarage ~ 0 + annee + generation, data=data) ) )

  coeff_effets <- effets %>% summarise(tidy(model)) %>%
    filter(grepl("[[:digit:]]$",term)) %>%
    mutate(type = str_extract(term,"[^[:digit:]]+"),
           valeur = str_extract(term,"[[:digit:]]+") )

  quantiles_effetsannee <- coeff_effets %>% filter(type=="annee" ) %>%
    group_by(cc,naiss,sexe) %>%
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
    left_join(quantiles_effetsannee, by=c("cc","naiss","sexe")) %>%
    filter(estimate < x50-2*(x75-x25) | estimate > x50+2*(x75-x25)) %>%
    rename(annee=valeur) %>%
    #mutate(cat = paste(sexe,naiss)) %>% select(-sexe,-naiss) %>%
    group_by(naiss,cc,annee) %>%
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

  reg <- "0012"
  coeff_effets %>% filter(cc==reg & type=="annee" ) %>% # & naiss=="Ensemble"
    ggplot(aes(y=estimate,x=valeur,group=paste(sexe,naiss),linetype=naiss,colour=sexe)) +
    geom_line() +
    theme(legend.position="top") +
    facet_grid( ~naiss) +
    labs(subtitle=paste("Régime = ",reg))

}

# on tronque les observations pour éliminer les années pour lesquelles il y a manifestement des ruptures de série

agetabc <- tabc %>%
  left_join(filtre_an_pb %>%
              group_by(cc,naiss) %>% summarise(an_pb_max=max(annee)) %>% ungroup(),
            by=c("cc","naiss")) %>%
  filter(is.na(an_pb_max) | annee > an_pb_max ) %>%
  # == on conserve quelques cas traités à la mains
  # MSA : profils très bizarres (âges arrondis à l'entier ??), sauf pour les années récentes
  filter(!(cc %in% c("0022","0021","Indp") & generation+age<2015)) %>%
  # AGIRC-ARRCO : évolution 2019-2020 semble non cohérente avec 2020-2021
  filter(!(cc=="5600" & generation+age<2020))

vargen <- vargen %>%
  mutate(annee = generation+age) %>%
  anti_join(filtre_an_pb, by=c("cc","naiss","annee")) %>%
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

# âge moyen de départ à la retraite parmi les retraités observés à 67 ans

# ATTENTION : la méthode recale les séries sur le champ des retraités observés à 67 ans (donc hors personnes liquidant un droit après cet âge), mais ne permet pas totalement de neutraliser l'effet des différences entre générations d'âge moyen de départ liées aux personnes liquidant après 67 ans. Cela est lié au fait qu'on chaîne des âges moyens observés après 67 ans pour les générations anciennes.

ageretr_67 <- agetabc %>% filter(age==67) %>%
  full_join(vargen %>%
              filter(age>=67) %>%
              group_by(caisse,cc,sexe,naiss,generation) %>%
              # méthode 1 = on retient l'écart observé à l'âge le plus bas observé
              #filter(age==min(age)) %>%
              # méthode 2 = on retient l'écart moyen sur toutes les années observées
              # summarise(varageliq_g_p1 = mean(varageliq_g_p1)) %>%
              # méthode 3 = on retient l'écart median sur toutes les années observées
              summarise(varageliq_g_p1 = quantile(varageliq_g_p1,0.5)) %>%
              ungroup() %>%
              select(caisse,cc,sexe,naiss,generation,varageliq_g_p1),
            by=c("caisse","cc","sexe","naiss","generation") ) %>%
  arrange(caisse,cc,sexe,naiss,-generation) %>%
  # on estime l'âge moyen de liquidation parmi les retraités observés à 67 ans en chaînant les évolutions d'une année sur l'autre
  mutate(varageliq_g_p1 = ifelse(is.na(varageliq_g_p1),1,varageliq_g_p1),
         varageliq_g_p1 = ifelse(!is.na(age),1,varageliq_g_p1),
         source = ifelse(is.na(source),"chaînage rétrospectif",source)) %>%
  group_by(caisse,cc,sexe,naiss) %>%
  fill(ageliq,.direction="down") %>%
  fill(age,.direction="down") %>%
  mutate(varageliq_g_p1 = cumprod(varageliq_g_p1),
         ageliq = ageliq * varageliq_g_p1 ) %>%
  ungroup() %>%
  select(caisse,cc,source,sexe,naiss,generation,ageliq)

# âge moyen de départ à la retraite parmi les retraités observés à 70 ans

ageretr_70 <- agetabc %>% filter(age==70) %>%
  full_join(vargen %>%
              filter(age>=70) %>%
              group_by(caisse,cc,sexe,naiss,generation) %>%
              # méthode 1 = on retient l'écart observé à l'âge le plus bas observé
              #filter(age==min(age)) %>%
              # méthode 2 = on retient l'écart moyen sur toutes les années observées
              # summarise(varageliq_g_p1 = mean(varageliq_g_p1)) %>%
              # méthode 3 = on retient l'écart median sur toutes les années observées
              summarise(varageliq_g_p1 = quantile(varageliq_g_p1,0.5)) %>%
              ungroup() %>%
              select(caisse,cc,sexe,naiss,generation,varageliq_g_p1),
            by=c("caisse","cc","sexe","naiss","generation") ) %>%
  arrange(caisse,cc,sexe,naiss,-generation) %>%
  # on estime l'âge moyen de liquidation parmi les retraités observés à 67 ans en chaînant les évolutions d'une année sur l'autre, observées à l'âge le plus petit disponible, par rapport à la génération la plus ancienne observée à 67 ans dans l'EACR
  mutate(varageliq_g_p1 = ifelse(is.na(varageliq_g_p1),1,varageliq_g_p1),
         varageliq_g_p1 = ifelse(!is.na(age),1,varageliq_g_p1),
         source = ifelse(is.na(source),"chaînage rétrospectif",source)) %>%
  group_by(caisse,cc,sexe,naiss) %>%
  fill(ageliq,.direction="down") %>%
  fill(age,.direction="down") %>%
  mutate(varageliq_g_p1 = cumprod(varageliq_g_p1),
         ageliq = ageliq * varageliq_g_p1 ) %>%
  ungroup() %>%
  select(caisse,cc,source,sexe,naiss,generation,ageliq)

# agrégation des deux tables

ageretr <- bind_rows(
  ageretr_67 %>% mutate(champ_obs = "Retraités à 67 ans"),
  ageretr_70 %>% mutate(champ_obs = "Retraités à 70 ans") )

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
