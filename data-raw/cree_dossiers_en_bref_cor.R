library(devtools)
#load_all()

library(tidyverse)
# library(openxlsx)
# install.packages("pdftools")
library(pdftools)

# ===================================================================================
# crée la base "dossiers_en_bref_cor" des documents "dossiers en bref" publiés par le COR
# ===================================================================================

# == extrait les textes des documents "le dossier en bref" et met en forme la base

basedossiers <- docs_cor %>% filter(type=="dossier en bref")
# urlloc <- basedossiers$url.doc[9]

extrait_question_dossierenbref <- function(urlloc) {
  #for (i in 1:nrow(basedossiers)) {
  #urlloc <- basedossiers$url.doc[i]
  txt <- pdftools::pdf_text(paste0("https://www.cor-retraites.fr",urlloc) ) %>% paste(collapse=" ")
  if (!grepl("",txt)) { return(data.frame() )}
  txt <- txt %>%
    str_replace_all("\n[[:space:]]*[[:digit:]]\n","\n") %>%
    str_replace_all("","\n\n") %>%
    str_replace_all("(?<=Pourquoi ce (dossier|sujet)( |)\\?)[\\\n[:space:]]+"," ") %>%
    strsplit(split="\n\n") %>% unlist() %>%
    str_replace_all("[[:space:]\\\n]+"," ") %>%
    trimws()
  tab <- data.frame(info = txt[!(txt %in% c("","1"))]) %>%
    filter(!grepl("^[[:space:][:punct:][:digit:]]+$",info)) %>%
    mutate(nligne = 1:n(),
           type = case_when(
             grepl("^[IVX]+(\\.|–| –|\\-| \\-) ",info) ~ "partie",
             grepl("\\?",info) & (nligne>1) ~ "question",
             TRUE ~ "autre"    ),
           nbpartie = cumsum(type=="partie"),
           nbquestion = cumsum(type=="question"))  %>%
    filter(!(type=="autre" & nligne>2)) %>% select(-nligne) %>%
    pivot_wider(id_cols=-c("type","info"),names_from="type",values_from="info",values_fn = function(x){paste(x,collapse = " / ")}) %>%
    fill(c("autre","partie"),.direction="down") %>%
    mutate(url.doc = urlloc ) %>%
    filter(!is.na(question)) %>%
    select(-nbpartie,-nbquestion)
  return(tab)
}

extrait_dossiers_en_bref <- do.call("bind_rows",lapply(basedossiers$url.doc,extrait_question_dossierenbref))

dossiers_en_bref <- extrait_dossiers_en_bref %>%
  mutate(partie = ifelse(is.na(partie) & grepl("^Pourquoi ce (dossier|sujet)",question),"Pourquoi ce dossier ?",partie),
         date.seance = autre %>% str_extract("Séance[^«]+(?=«)"),
         titre.seance = autre %>% str_extract("(?<=«)[^»]+(?=»)") %>% trimws(),
         reponse = question %>% str_replace("^[^\\?]+\\?","") %>% trimws(),
         question = question %>% str_extract("^[^\\?]+\\?")   ) %>%
  select(-autre)

# == enrichit les textes avec des liens hypertextes vers les documents mentionnés

basereponses <- dossiers_en_bref %>% select(url.doc,reponse) %>%
  mutate(reponse_avec_url = reponse %>%
           str_replace_all("(?<=documents n°[[:space:][:digit:]\\.]{1,6}(et|\\&)[[:space:]]{0,2})(?=(n°|[[:digit:]]))","#") %>%
           str_replace_all("(?<=document(s|) n°[[:space:][:digit:]\\.]{0,6}[:digit:])(?=[^[:digit:]])","#") %>%
           str_replace_all("(?=(D|d)ocument(s|) n°)","#") %>%
           str_replace_all("(?<=#(n°|n° |)[[:digit:]]{1,2})","#") )

eclate_reponse <- function(i){
  eclate <- basereponses$reponse_avec_url[i] %>% strsplit(split="#") %>% unlist()
  data.frame(
    url.doc = rep(basereponses$url.doc[i],NROW(eclate)),
    reponse = rep(basereponses$reponse[i],NROW(eclate)),
    reponse_avec_url = eclate
  ) %>% return()
}

basereponses_ <- do.call("bind_rows",lapply(1:nrow(basereponses),eclate_reponse)) %>%
  mutate(num_document = reponse_avec_url %>%
           str_replace("^documents","document") %>%
           str_replace("(?=^[[:digit:]])","document n° ")) %>%
  left_join(docs_cor %>% select(url.doc,annee,mois,jour), by="url.doc")

liens <- docs_cor %>%
  select(url.doc,num_document,annee,mois,jour) %>%
  filter(!is.na(num_document)) %>%
  rename(url.lien = url.doc) %>%
  mutate(num_document = tolower(num_document))

basereponses_ <- basereponses_ %>%
  left_join(liens, by=c("annee","mois","jour","num_document")) %>%
  mutate(reponse_avec_url = ifelse(
    !is.na(url.lien),
    paste0("[",reponse_avec_url,"](https://www.cor-retraites.fr",url.lien,")"),
    reponse_avec_url
  )) %>%
  select(-url.lien,-num_document,-annee,-mois,-jour) %>%
  group_by(url.doc,reponse) %>% summarise_all(function(x){paste(x,collapse="")}) %>% ungroup()

dossiers_en_bref <- dossiers_en_bref %>%
  left_join(basereponses_, by=c("url.doc","reponse"))

# == sauvegarde des bases

dossiers_en_bref_cor <- dossiers_en_bref

usethis::use_data(dossiers_en_bref_cor,       overwrite=TRUE)

# A FAIRE : ajouter dans document !!

# == sauvegarde des bases en format .csv

write.csv2(dossiers_en_bref_cor,
           file=gzfile("data-raw/dossiers_en_bref_cor.csv.gz"),
           row.names = FALSE, fileEncoding = "UTF-8")
