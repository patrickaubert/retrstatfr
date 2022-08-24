library(devtools)
#load_all()

library(tidyverse)
library(rvest)
# library(openxlsx)
# install.packages("pdftools")
library(pdftools)

# ===================================================================================
# crée la base "docs_cor" des titres et url de tous les documents du COR
# ===================================================================================

# == extrait les titres et url des séances mensuelles

urlcor <- "https://www.cor-retraites.fr"
urlrechseance <- "https://www.cor-retraites.fr/documents/reunions-du-cor?items_per_page=10&page="

# npage <- 31 #31

seances <- function(npage) {
  page <- read_html( paste0(urlrechseance,npage) )
  page.seance <- data.frame(
    txt = page %>%  html_nodes("h3,span,a") %>% html_text(),
    ref = page %>%  html_nodes("h3,span,a") %>% html_attr("href"),
    type = page %>%  html_nodes("h3,span,a") %>% html_attr("class")
    ) %>%
    #filter(grepl("^(/documents/|/index\\.php/node)",ref) | type %in% c("panel__title","panel__meta")) %>%
    filter(txt=="Lire" | type %in% c("panel__title","panel__meta")) %>%
    mutate(txt = ifelse(txt=="Lire",ref,txt),
           type = type %>% recode("panel__title" = "titre.seance",
                                  "panel__meta" = "date.seance",
                                  "btn btn-primary-light" = "url.seance"),
           nb = ifelse(type=="titre.seance",1,0) %>% cumsum()) %>%
    select(txt,type,nb) %>%
    pivot_wider(id_cols="nb",names_from="type",values_from="txt")
  return(page.seance)
}

extrait_page.seances <- do.call("bind_rows",lapply(0:31,seances))

page.seances_brut <- extrait_page.seances %>%
  mutate(annee = date.seance %>% str_extract("[[:digit:]]{4}$"),
         mois = date.seance %>% str_extract("(?<=[[:space:]])[[:alpha:]]+(?=[[:space:]][[:digit:]]{4}$)"),
         jour = date.seance %>% str_extract("(?<=[[:space:]])[[:digit:]]+(?=[[:space:]]*[[:alpha:]]+[[:space:]]*[[:digit:]]{4}$)"))

page.seances <- page.seances_brut %>% select(-nb) %>% distinct()

# == fonction d'extraction des informations relatives à la séance (utile pour les pages html séances n'ayant aucun document à télécharger, pour lesquelles la fonction "document" ci-après ne renvoie rien)

infoseance <- function(urlloc) {
  #lpagedocs <- list()
  #for (i in 1:nrow(page.seances)) {
  pagedocs <- read_html( paste0(urlcor,urlloc) )
  #champ.a.extraire <- "h4,h6,span,a,p"
  champ.a.extraire <- ".panel__title,.media__file_author,.action__btn,.btn,.field h4,.field__item p,.field__item a,.field__item span"
  page.docs <- data.frame(
    txt = pagedocs %>%  html_nodes(champ.a.extraire) %>% html_text() %>% trimws(),
    ref = pagedocs %>%  html_nodes(champ.a.extraire) %>% html_attr("href"),
    type = pagedocs %>%  html_nodes(champ.a.extraire) %>% html_attr("class")
  ) %>%
    #distinct() %>%
    mutate(txt = ifelse(txt=="Lien de téléchargement",ref,txt) %>%
             str_replace_all("[[:space:]]+"," "),
           type = case_when(
             #type=="field__item" & grepl("^[IVX]+[[:space:]]*(\\.|\\-|–)",txt ~  "partie",
             grepl("^field",type) ~  "partie",
             is.na(type) ~ "txt",
             TRUE ~  type) ) %>%
    filter(!grepl("^Lien",txt),txt!="Accueil",txt!="") %>%
    select(-ref) #%>%
  #mutate(type = ifelse(is.na(type),"texte",type) %>% str_replace("[[:space:]].*$",""))
  # texte introductif et mots-clés de la séance
  texte.introductif <- (page.docs %>% filter(type=="txt",txt!="",txt!=" ") )$txt %>% paste(collapse="\n")
  if (texte.introductif=="") {
    autrestxts <- pagedocs %>%  html_nodes(".field__item") %>% html_text() %>% trimws() %>%
      data.frame() %>%
      filter(!(. %in% c(""," ")))
    texte.introductif <- autrestxts$.[1]
  }
  #texte.introductif <- (page.docs %>% filter(type=="txt") %>% arrange(-nchar(txt)))$txt[1]
  mots.cles <- (page.docs %>% filter(type=="btn btn-secondary",txt!="RECHERCHE AVANCÉE"))$txt %>% paste(collapse=" / ")
  # documents
  infoseance <- data.frame(
    url.seance = urlloc,
    texte.introductif = texte.introductif,
    mots.cles = mots.cles
  )
  #lpagedocs[[i]] <- page.docs %>% mutate(i = i)
  return(infoseance)
}

# == extrait les principales informations relatives aux documents de chaque séance (dont l'url des fichiers pdf)

documents <- function(i) {
  #lpagedocs <- list()
  #for (i in 1:nrow(page.seances)) {
  pagedocs <- read_html( paste0(urlcor,page.seances$url.seance[i]) )
  # pagedocs <- read_html( paste0(urlcor,urlloc) )
  # -- première lecture : pages "standards" avec des boutons pour le téléchargement des PDF
  #champ.a.extraire <- "h4,h6,span,a,p"
  champ.a.extraire <- ".panel__title,.media__file_author,.action__btn,.btn,.field h4,.field__item p,.field__item a,.field__item span"
  page.docs <- data.frame(
    txt = pagedocs %>%  html_nodes(champ.a.extraire) %>% html_text() %>% trimws(),
    ref = pagedocs %>%  html_nodes(champ.a.extraire) %>% html_attr("href"),
    type = pagedocs %>%  html_nodes(champ.a.extraire) %>% html_attr("class")
    ) %>%
    #distinct() %>%
    mutate(txt = ifelse(txt=="Lien de téléchargement",ref,txt) %>%
             str_replace_all("[[:space:]]+"," "),
           type = case_when(
             #type=="field__item" & grepl("^[IVX]+[[:space:]]*(\\.|\\-|–)",txt ~  "partie",
             grepl("^field",type) ~  "partie",
             is.na(type) ~ "txt",
             TRUE ~  type) ) %>%
    filter(!grepl("^Lien",txt),txt!="Accueil",txt!="") %>%
    select(-ref) #%>%
    #mutate(type = ifelse(is.na(type),"texte",type) %>% str_replace("[[:space:]].*$",""))
  # -- deuxième lecture : pages sans boutons pour le téléchargement des PDF
  if (!("panel__title" %in% unique(page.docs$type))) {
    champ.a.extraire.bis <- ".text__element_in"
  }
  # -- texte introductif et mots-clés de la séance
  texte.introductif <- (page.docs %>% filter(type=="txt",txt!="",txt!=" ") )$txt %>% paste(collapse="\n")
  if (texte.introductif=="") {
    autrestxts <- pagedocs %>%  html_nodes(".field__item") %>% html_text() %>% trimws() %>%
      data.frame() %>%
      filter(!(. %in% c(""," ")))
    texte.introductif <- autrestxts$.[1]
  }
  #texte.introductif <- (page.docs %>% filter(type=="txt") %>% arrange(-nchar(txt)))$txt[1]
  mots.cles <- (page.docs %>% filter(type=="btn btn-secondary",txt!="RECHERCHE AVANCÉE"))$txt %>% paste(collapse=" / ")
  # documents
  page.docs <- page.docs %>%
    filter(type %in% c("panel__title","media__file_author","action__btn","partie")) %>%
    mutate(type = type %>% recode("panel__title" = "titre",
                                  "media__file_author" = "auteur",
                                  "action__btn" = "url.doc",
                                  "partie" = "partie"),
           seance = page.seances$titre.seance[i],
           date.seance = page.seances$date.seance[i],
           url.seance = page.seances$url.seance[i],
           annee = page.seances$annee[i],
           mois = page.seances$mois[i],
           jour = page.seances$jour[i],
           texte.introductif = texte.introductif,
           mots.cles = mots.cles)
#lpagedocs[[i]] <- page.docs %>% mutate(i = i)
return(page.docs)
}

# essai <- documents(40)
# docs_cor <- do.call("bind_rows",lapply(1:5,documents))
extrait_docs_cor <- do.call("bind_rows",lapply(1:nrow(page.seances),documents))

# correction des quelques cas problématiques + mise en forme de la table

# pb avec réunion du 18 mars 2009 = titres faux sur page html ("etats-unis" pour tous les docs)

docs_cor_brut <- extrait_docs_cor %>%
  mutate(type = ifelse(txt=="Projections à l'horizon 2060 - des actifs plus nombreux et plus âgés",
                       "titre",type),
         type = ifelse(txt=="Projections 2005-2050, des actifs en nombre stable",
                       "titre",type))  %>%
  group_by(seance,date.seance,url.seance,annee,mois,jour,texte.introductif,mots.cles) %>%
  mutate(nb.partie = ifelse(type=="partie",1,0) %>% cumsum(),
         nb.doc = ifelse(type=="titre",1,0) %>% cumsum() ) %>%
  ungroup() %>%
  pivot_wider(id_cols=-c("txt","type"),names_from="type",values_from="txt") %>% # ,values_fn=length
  select(-nb.partie,-nb.doc) %>%
  group_by(seance,date.seance,url.seance,annee,mois,jour,texte.introductif,mots.cles) %>%
  fill(partie, .direction="down") %>%
  ungroup() %>%
  filter(!is.na(titre))

# verif <- docs_cor_brut %>% filter(titre>1 | auteur>1 | url.doc>1)

docs_cor <- docs_cor_brut %>%
  rename(titre_complet = titre) %>%
  mutate(type = case_when(
        grepl("^Document",titre_complet) ~ "document",
        grepl("^Note de présentation générale",titre_complet) ~ "document",
        grepl("^Annexe",titre_complet) ~ "document",
        grepl("^Diaporama",titre_complet) ~ "diaporama",
        grepl("^Présentation",titre_complet) ~ "diaporama",
        grepl("^(Le d|D)ossier en bref",titre_complet) ~ "dossier en bref",
        grepl("\\.(xlsx|xls)$",url.doc) ~ "fichier excel",
        grepl("\\.(pptx|ppt)$",url.doc) ~ "diaporama" ,
        grepl("^Dossier complet$",titre_complet) ~ "dossier complet"),
        # correction du cas où la 2e partie du titre est dans la ligne auteur
        titre_complet = ifelse(grepl("^[[:lower:]]",auteur),paste(titre_complet,auteur),titre_complet),
        auteur = ifelse(grepl("^[[:lower:]]",auteur),NA,auteur),
        # extraction du n° de document
        titre = titre_complet %>%
          str_replace_all("\\\t"," ") %>%
          str_replace("(?<=(Diaporama|Document)( n[°[:space:][:digit:]\\.]{0,6}| N[°[:space:][:digit:]\\.]{0,6}|))[[:space:]](?=[[:upper:]][[:lower:]]+)"," - ") %>%
          str_replace_all(" \\- ","#"),
        num_document = titre %>% str_extract("^([[:digit:]]+(_|#)|)(Document|Diaporama)[^#]*(?=(#|$))"),
        titre = titre %>%
          str_replace("^([[:digit:]]+(_|#)|)(Document|Diaporama)[^#]*(#|$)","") %>%
          str_replace_all("#"," - ")
        #num_document = titre_complet %>%
        #  str_extract("^([[:digit:]]+_|)(Document|Diaporama)( n| N|)[°[:space:][:digit:]\\-\\.]*([Bb]is|)(?=($|[[:space:]]\\-[[:space:]]))"),
        #titre = titre_complet %>%
        #  str_replace("^([[:digit:]]+_|)(Document|Diaporama)( n| N|)[°[:space:][:digit:]\\-\\.]*([Bb]is|)(|[[:space:]]\\-[[:space:]])","")
        )

# verif <- docs_cor %>% filter(grepl("[Dd](iaporama|ocument)",titre))
# verif <- docs_cor %>% filter(!is.na(num_document)) %>% arrange(-nchar(num_document))
# => cas à corriger à la main
# verif <- docs_cor %>% filter(grepl("^[[:lower:]]",auteur))
# verif <- docs_cor %>% filter(is.na(type))

# == complète l'information sur les documents à partir des premières pages des pdf

docs_cor_ <- docs_cor
# docs_cor <- docs_cor_

pdf_page1 <- function(pdf){
  tmp <- tempfile()
  on.exit(unlink(tmp))
  tempfile <- pdftools::pdf_subset(pdf, tmp, pages = 1)
  #pdftools::pdf_text(tmp)
  pdftools::pdf_text(tempfile)
}

info_pdf_page1 <- function(urlloc) {
  #lextrait <- list()
  #for (i in c(1:NROW(urls))) {
  #urlloc<-urls[i]
  page1 <- pdf_page1(pdf = paste0(urlcor,urlloc))
  txtpage1 <- page1 %>%
    str_replace_all("[\\\n][[:space:]]*(?=Secrétariat général du (Conseil|COR))","\n\n") %>%
    str_replace_all("[\\\n][[:space:]]*(?=Séance plénière du (Conseil|COR))","\n\n") %>%
    str_replace_all("[\\\n]{2,}","\n\n") %>%
    str_replace_all("[\\\n]{2,}[[:space:]]*(?=[[:lower:]])"," ") %>%
    strsplit(split="\n\n") %>% unlist() %>%
    str_replace_all("[[:space:]\\\n]+"," ") %>%
    trimws()
  tabpage1 <- data.frame(info = txtpage1[!(txtpage1 %in% c("","1"))]) %>%
    filter(!grepl("^[[:space:][:punct:][:digit:]]+$",info),
           nchar(info)<=500)
  if (nrow(tabpage1)==0) {return(data.frame() )}
  if (nrow(tabpage1)>=1) {
  tabpage1 <- tabpage1  %>%
    mutate(info = info %>% str_replace("[[:space:]]*Document de travail, n’engage pas le Conseil",""),
           nbligne = 1:n(),
           nominfo = case_when(
             grepl("^CONSEIL D(’|')ORIENTATION DES RETRAITES",info) ~ "seance_dapres_pdf",
             grepl("^Conseil d(’|')orientation des retraites (Réunion|Séance) plénière",info) ~ "seance_dapres_pdf",
             grepl("^Séance plénière[[:alpha:][:space:]]+[[:digit:]]+ [[:alpha:]]+ 20[[:digit:]]{2}",info) ~ "seance_dapres_pdf",
             grepl("^([IVX[:space:]\\-]+|)Document (n|N)",info) ~ "numdocument_dapres_pdf",
             grepl("^(Document|Annexe) [[:digit:]\\.]+$",info) ~ "numdocument_dapres_pdf",
             grepl("^((Le |)[[:digit:]]{1,2} |)(janvier|février|mars|avril|mai|juin|juillet|août|septembre|octobre|novembre|décembre) 20[[:digit:]]{2}",tolower(info)) ~ "date_dapres_pdf",
             grepl("^Le dossier en bref Préparé par le secrétariat général du Conseil",info) ~ "titre_dapres_pdf"
              ),
           nbinfo = sum(is.na(nominfo)),
           ranginfovide = cumsum(is.na(nominfo)),
           nominfo = case_when(
             !is.na(nominfo) ~ nominfo,
             (nbinfo==2 & ranginfovide==1 & is.na(nominfo)) ~ "titre_dapres_pdf",
             (nbinfo==2 & ranginfovide==2 & is.na(nominfo)) ~ "auteur_dapres_pdf"
           ) ) %>%
    select(-nbligne) %>%
    group_by(nominfo) %>% mutate(nb=paste0("info",1:n())) %>% ungroup() %>%
    mutate(nominfo = ifelse(is.na(nominfo),nb,nominfo)) %>%
    pivot_wider(id_cols="nbinfo",names_from="nominfo",values_from="info",values_fn = function(x){paste(x,collapse = " / ")}) %>%
    mutate(url.doc = urlloc )
   return(tabpage1)
  }
  #lextrait[[i]] <- tabpage1
}

# verif <- do.call("bind_rows",lextrait)


urls <- (docs_cor %>%
           filter(is.na(type) | !(type %in% c("dossier en bref","diaporama"))) %>%
           filter(grepl("pdf$",url.doc)))$url.doc

# essai <- do.call("bind_rows",lapply(urls[268:277],info_pdf_page1))
extrait_info_page1 <- do.call("bind_rows",lapply(urls,info_pdf_page1))

# verif <- extrait_info_page1 %>% filter(!is.na(info1))
# verif <- extrait_info_page1 %>% filter(nchar(auteur_dapres_pdf)>=200) %>% mutate(ltitre=nchar(auteur_dapres_pdf))
# taillechaine <- extrait_info_page1 %>% summarise_all(function(x){max(nchar(x),na.rm=TRUE)})


docs_cor <- docs_cor %>% full_join(extrait_info_page1, by="url.doc")

# == complète la table des séances du COR avec les informations tirées des pages par séance

# attention : certaines séances plénières ont plusieurs pages web, donc la clé est bien année+mois+jour+url.seance (ne pas se contenter de la date !!)

textes.seances <- docs_cor_brut %>%
  select(annee,mois,jour,url.seance,texte.introductif,mots.cles) %>%
  distinct()

seances_cor <- page.seances %>%
  left_join(textes.seances ,   by=c("annee","mois","jour","url.seance"))

textes.seances.retrouves <- seances_cor %>% filter(!is.na(texte.introductif))
textes.seances.missing <- seances_cor %>% filter(is.na(texte.introductif))

compl.seances <- do.call("bind_rows",lapply(textes.seances.missing$url.seance,infoseance)) %>%
  distinct()

seances_cor <- bind_rows(
  textes.seances.retrouves,
  textes.seances.missing %>%
    select(-texte.introductif,-mots.cles) %>%
    left_join(compl.seances, by="url.seance")
  )

# == sauvegarde des bases

usethis::use_data(docs_cor,
                  seances_cor,
                  overwrite=TRUE)

## A FAIRE
# - document la base
# - créer variable "num_doc", "titre_complet"
# - aller chercher titre et auteur dans le pdf si manquant

# == sauvegarde des bases en format .csv

write.csv2(seances_cor,
           file=gzfile("data-raw/seances_cor.csv.gz"),
           #"data-raw/seances_cor.csv",
           row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(docs_cor %>% select(-texte.introductif,-mots.cles),
           file=gzfile("data-raw/docs_cor.csv.gz"),
           #"data-raw/docs_cor.csv",
           row.names = FALSE, fileEncoding = "UTF-8")

