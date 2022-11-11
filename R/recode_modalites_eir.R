#' Recode une variable de l'EIR en remplaçant ses modalités par leurs intitulés
#'
#' @param var la variable à recoder
#' @param nomvar le nom de la variable parmi les noms des variables de l'EIR (character)
#'
#' @return une variable dont les modalités sont des intitulés en clair
#' @export recode_modalites_eir
recode_modalites_eir <- function(var,nomvar) {
  tab <- modalites_eir %>% filter(tolower(variable)==tolower(nomvar)) %>%
    select(modalite,intitule) %>%
    distinct()
  return( recode(as.character(var), !!! setNames(tab$intitule , tab$modalite) ) )
}
