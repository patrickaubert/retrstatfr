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

#' Taux de retraités par âge fin entre 50 et 70 ans (source DREES, modèle ANCETRE)
#'
#' Les données proviennent du panorama annuel de la DREES "retraités et retraites", à partir du modèle ANCETRE.
#'
#' @format A data frame with 1071 observations and 6 variables:
#' \describe{
#'   \item{age}{Age, entre 50 et 70 ans}
#'   \item{annee}{Année}
#'   \item{txretr}{Taux de retraités, en % (chiffre en 0 et 1)}
#'   \item{txnouvretr}{Taux de nouveaux retraités au cours de l'année, en % (chiffre en 0 et 1)}
#'   \item{sexe}{ensemble, femmes, hommes}
#'   \item{geo}{lieu de résidence}
#' }
"txretr"
