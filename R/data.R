#' Une table contenant les informations permettant d'extraire des données sur la retraite diffusées en open data
#'
#' Les données proviennent du site d'open data de la DREES (data.drees), du site internet du COR, etc.
#'
#' @format A data frame with 14036 observations and 4 variables:
#' \describe{
#'   \item{intitule}{Intitulé de l'indicateur}
#'   \item{producteur}{Organisme producteur des données}
#'   \item{reference}{Ouvrage de référence ou modalité de diffusion des données}
#'   \item{reference2}{Référence complémentaire (ex. n° de fiche)}
#'   \item{datepubli}{Année d'édition ou date de publication}
#'   \item{champ_sexe}{Champ de l'indicateur en termes de sexe}
#'   \item{champ_geo}{Champ de l'indicateur en termes géographique}
#'   \item{champ_autre}{Autres indications de champ de l'indicateur}
#'   \item{onglet}{Nom de l'onglet dans le fichier excel}
#'   \item{zone}{Délimitations de la zone dans l'onglet excel}
#'   \item{url}{Adresse url du fichier}
#'   \item{annee_en_colonnes}{Vaut TRUE si les colonnes dans l'onglet Excel correspondent aux années}
#' }
"sources_opendata"
