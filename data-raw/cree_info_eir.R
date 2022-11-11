library(devtools)
#load_all()
library(tidyverse)

# ===================================================================================
# crée une base des informations sur les variables de l'EIR
# ===================================================================================

# == récupération des informations

urlcasd <- "https://www.casd.eu/generate-csv/?language=fr&id_src=445"
urlprogedo <- "https://data.progedo.fr/documents/adisp/lil-1535/EIR_2016_Dictionnaire_des_variables_selon_diffusion_pourProgedo.xlsx"
sheetprogedo <- "INDIV16"

# du fait de son format spécial, on lit le fichier progedo comme un fichier texte

infocasd <- read.csv2(file=urlcasd,header=FALSE,sep="#",encoding="UTF-8")

variables_eir <- infocasd %>%
  mutate(variable = ifelse(grepl(";",V1),str_extract(V1,"^[^;]+(?=;)"),NA),
         libelle = ifelse(grepl(";",V1),str_extract(V1,"(?<=^[^;]{0,40};)[^;]+(?=;)"),NA),
         modalites = ifelse(grepl(";",V1),str_replace(V1,"^[^;]*;[^;]*;",""),NA),
         table = ifelse(!grepl(";|[[:space:]]",V1),V1,NA),
         info = ifelse(!grepl(";",V1) & grepl("[[:space:]]",V1),V1,NA)) %>%
  select(-V1,-info) %>%
  fill(table,.direction="down") %>%
  filter(!is.na(variable) & variable!="Nom de la variable") %>%
  mutate(table = table %>%
           str_replace_all("2012|(?<!0)12","") %>%
           str_replace_all("eir_|_eir",""))

# == modalités des variables

modalites_eir <- variables_eir %>%
  filter(grepl(";",modalites)) %>%
  select(table,variable,modalites) %>%
  mutate(nbmodalites=nchar(str_replace_all(modalites,"[^;]",""))+1) %>%
  group_by(table,variable) %>%
  slice(rep(1:n(), each=nbmodalites)) %>%
  mutate(modalite = unlist(strsplit(modalites[1],split=";"))) %>%
  ungroup() %>%
  mutate(intitule = modalite %>% str_replace("^[^\\-]*\\-[[:space:]]*",""),
         modalite = modalite %>% str_extract("^[^\\-]*") %>% trimws()) %>%
  select(table,variable,modalite,intitule)

# == sauvegarde des bases


usethis::use_data(variables_eir, modalites_eir,      overwrite=TRUE)

# == sauvegarde des bases en format .csv

write.csv2(variables_eir,
           file=gzfile("data-raw/variables_eir.csv.gz"),
           row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(modalites_eir,
           file=gzfile("data-raw/modalites_eir.csv.gz"),
           row.names = FALSE, fileEncoding = "UTF-8")
