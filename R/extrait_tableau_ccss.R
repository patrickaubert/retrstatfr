#' Extrait des informations d'un tableau du rapport de la CCSS sous forme d'une table de données
#'
#' Lit une page donnée d'un rapport de la CCSS, et en extrait les données chiffrées issues d'un tableau,
#' pour les restituer sous la forme d'un data.frame
#'
#' @param pdf_ccss emplacement du rapport CCSS (fichier PDF)
#' @param page page du rapport sur laquelle se situe le tableau qu'on souhaite extraire
#' @param position position (dans l'ordre : 1, 2 ...) du tableau sur la page
#'
#' @importFrom magrittr `%>%`
#'
#' @return un dataframe
#' @export extrait_tableau_ccss
extrait_tableau_ccss <- function(
    pdf_ccss = NULL,
    page = 1,
    position = 1) {

  # *** ci-dessous :divers exemples pour tests :

  # pdf_ccss <- "C:/Users/PA/Documents/bases_de_donnees/rapports_CCSS/rapports/2022-09-CCSS.pdf"
  # page <- 167
  # pdf_ccss <- "C:/Users/PA/Documents/bases_de_donnees/rapports_CCSS/rapports/CCSS-octobre 2024.pdf"

  # --- récupération du tableau mis en forme

  tab <- tabulapdf::extract_tables(
    file = pdf_ccss,
    pages = page
  )[[position]]

  names(tab)[1] <- "type"

  tab <- tab %>% select(type, starts_with("2"))

  tab <- tab %>%
    mutate_at(vars(starts_with("2")), ~as.numeric(str_replace_all(.,"[[:space:]]","")))

  # --- extraction complémentaire avec pdftools, pour récupérer le niveau d'indentation

  text <- pdftools::pdf_text(pdf_ccss)[167]

  txt_tab <- data.frame(
    txt_init = strsplit(text,"\n")[[1]],
    stringsAsFactors = FALSE   ) %>%
    mutate(
      niv_indentation = nchar(str_extract(txt_init,"^[[:space:]]+")),
      niv_indentation = niv_indentation-2,
      txt_init = str_replace_all(tolower(txt_init),"[^[:alpha:]]","")
    ) %>%
    #distinct() %>%
    # ci-dessous : pour la gestion des doublons de lignes (ayant le même intitulé : par exemple "autres transferts")
    # (on utilise le fait que ces intitules identiques apparaissent dans le même ordre dans les deux tables)
    group_by(txt_init) %>% mutate(txt_init=paste0(txt_init,"#",1:n())) %>% ungroup()

  tab <- tab %>%
    mutate(txt_init = str_replace_all(tolower(type),"[^[:alpha:]]","") ) %>%
    group_by(txt_init) %>% mutate(txt_init=paste0(txt_init,"#",1:n())) %>% ungroup() %>%
    left_join(txt_tab, by="txt_init") %>%
    relocate("niv_indentation",.before="type") %>%
    select(-txt_init)

  # --- création d'intitulés indentés

  nb_indentations <- sort(unique(tab$niv_indentation))

  tab <- tab %>% mutate(niv_indentation_brut=niv_indentation)

  for (i in c(1:NROW(nb_indentations))){

    tab <- tab %>%
      mutate(niv_indentation = ifelse(niv_indentation_brut==nb_indentations[i],i,
                                      niv_indentation))

    tab[[paste0("type_",i)]] <- if_else(
      tab$niv_indentation_brut==nb_indentations[i],
      tab$type,
      NA_character_
    )

  }

  tab <- tab %>%
    select(-niv_indentation_brut) %>%
    relocate(c(starts_with("type_")), .after="type") %>%
    relocate("niv_indentation", .before = "type")

  tab <- tab %>%
    fill("type_1", .direction="down")

  for (i in c(2:NROW(nb_indentations))){

    tab <- tab %>%
      group_by( !!sym(paste0("type_",i-1)) ) %>%
      fill( !!sym(paste0("type_",i)) , .direction="down") %>%
      ungroup()

  }

  # --- finalisation de la table extraire

  return(tab)

}
