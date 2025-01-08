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

  # pdf_ccss <- "C:/Users/PA/Documents/bases_de_donnees/rapports_CCSS/rapports/2022-09-CCSS.pdf"
  # pdf_ccss <- "C:/Users/PA/Documents/bases_de_donnees/rapports_CCSS/rapports/CCSS-octobre 2024.pdf"
  # page <- 167

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
    distinct()

  # attention : il reste un risque de création de doublon, si plusieurs lignes ont le même intitulés (par exemple "autres transferts")

  tab <- tab %>%
    mutate(txt_init = str_replace_all(tolower(type),"[^[:alpha:]]","") ) %>%
    left_join(txt_tab, by="txt_init") %>%
    relocate("niv_indentation",.before="type") %>%
    select(-txt_init)

  return(tab)

}
