#' Extrait les données opendata de l'enquête annuelle auprès des caisses de retraite
#'
#' Si l'adresse du fichier n'est pas indiquée, la fonction lit par défaut la dernière version des données disponibles sur l'espace data.Drees (version diffusée le 10 octobre 2022).
#'
#' @param fichier emplacement du fichier Excel contenant les données
#' @param tableaux nom des tableaux (onglets du fichier Excel)
#'
#' @importFrom openxlsx getSheetNames
#' @importFrom openxlsx read.xlsx
#' @importFrom janitor clean_names
#'
#' @return une liste de data.frame
#' @export extrait_eacr
extrait_eacr <- function(fichier=c("https://data.drees.solidarites-sante.gouv.fr/api/datasets/1.0/donnes_eacr/attachments/eacr_diffusee_part_2_version_du_20_septembre_2022_xlsx/",
                                "https://data.drees.solidarites-sante.gouv.fr/api/datasets/1.0/donnes_eacr/attachments/eacr_diffusee_part_1_version_du_20_septembre_2022_xlsx/"),
                         tableaux=NULL) {

  if (NROW(fichier)>1) {
    return(
      do.call("append",
              lapply(1:NROW(fichier),function(x){extrait_eacr(fichier[x],tableaux)}))
    )
  } else {

    # récupération des tableaux extraits

    tabdispo <- getSheetNames(fichier)

    if (is.null(tableaux)) {
      tabextraits<-tabdispo
    } else {

      tabextraits <-intersect(tabdispo,tableaux)

      if (NROW(tabextraits)==0) {
        tabextraits <- tabdispo[grepl(paste(tableaux,collapse="|"),tabdispo)]
      }
      if (NROW(tabextraits)==0) {
        warning(paste("Les tableaux demandés ne sont pas retrouvés. Les tableaux disponibles dans le fichier sont :",
                    paste(tabdispo,collapse = " ; ")))
        return(list())
        }

    }

    # extraction des données proprement dites

    extrait <- lapply(tabextraits,
                      function(onglet){
                        read.xlsx(
                          xlsxFile = fichier,
                          sheet=onglet) %>% clean_names()
                      })

    names(extrait) <- tabextraits %>% str_replace_all("\\-","_")

    return(extrait)

  }

}
