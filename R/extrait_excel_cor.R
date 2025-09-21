#' Extrait les informations d'un fichier Excel au format de ceux diffusés par le COR
#'
#' Récupère des données sur la retraite diffusées en open data au format Excel par le COR
#' et les restitue sous la forme d'un dataframe.
#'
#' @param url adresse du fichier Excel téléchargeable en ligne
#' @param onglet onglet à extraire dans le fichier Excel
#'
#' @importFrom dplyr recode
#' @importFrom openxlsx read.xlsx, getSheetNames
#' @importFrom httr GET, stop_for_status
#' @importFrom janitor clean_names
#' @importFrom tidyr pivot_longer
#'
#' @return un dataframe
#' @export extrait_opendata
extrait_excel_cor <- function(url = NULL,
                              onglet = NULL) {

  if (is.null(url)){
    warning("Fichier Excel non spécifié")
  }

  # == pour test

  if (FALSE){
    url <- "https://www.cor-retraites.fr/sites/default/files/2025-06/Donn%C3%A9es_compl%C3%A9mentaires_RA2025.xlsx"
    onglet <- "PIB"
  }

  # extraction des informations

  if (is.null(onglet)){

    #  == récupération des noms des onglets

    # Télécharger en mémoire
    resp <- httr::GET(url)
    httr::stop_for_status(resp)

    # Créer un fichier temporaire
    tmp <- tempfile(fileext = ".xlsx")
    writeBin(resp$content, tmp)

    # Lire les noms de feuilles
    onglets <- openxlsx::getSheetNames(tmp)

    # Nettoyer
    unlink(tmp)

    # suppression de l'onglet sommaire
    onglets <- onglets[tolower(onglets) != "sommaire"]

    # == récupération des données de tous les onglets

    tablong <- do.call(
      "bind_rows",
      lapply(
        onglets,
        function(x){
          extrait_excel_cor(url=url, onglet = x)
          }
      )
    )

    # verif <- tablong %>% filter(nchar(str_replace_all(val,"[[:digit:],e\\.\\-]",""))>0)


  } else {

    # == lecture du fichier Excel sur le site du COR

    tab <- read.xlsx(
      xlsxFile = url,
      sheet = onglet
    ) %>%
      janitor::clean_names()

    # == repérage des lignes avec les années
    lignes_annees <- rowSums(tab %>% mutate_all( ~as.numeric(.) %in% (2004:2050)))
    lignes_annees <- c(1:NROW(lignes_annees))[lignes_annees>0]
    noms <- unname(as.vector(t(tab[lignes_annees[1],])))

    noms <- ifelse( !is.na(noms) & grepl("^2",noms),
                    noms,
                    paste0("x",1:NROW(names(tab))))

    names(tab) <- noms

    # == mise en forme

    tablong <- tab[(lignes_annees[1]+1:nrow(tab)),2:ncol(tab)] %>%
      fill(x2 , .direction="down") %>%
      filter(!is.na(x2) & !is.na(x3)) %>%
      mutate_all( as.character ) %>%
      pivot_longer(cols=c(starts_with("2")), names_to="annee",values_to="val") %>%
      mutate(annee = as.numeric(annee)   ) %>%
      filter( nchar(str_replace_all(tolower(val),"[[:digit:],e\\.\\-]",""))==0) %>%
      mutate(val = as.numeric(val)) %>%
      filter(!is.na(val)) %>%
      rename(indicateur = x2, scenario = x3) %>%
      filter( ! is.na(val) )  %>%
      mutate( onglet_fichier_excel = onglet)

  }

  return(tablong)



}
