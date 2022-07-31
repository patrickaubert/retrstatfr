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


#' Projections du conseil d'orientation des retraites (COR)
#'
#' Résultats des projections du COR, publiés dans son rapport annuel, pour les principaux indicateurs (pensions moyennes relatives, part des dépenses de retraite dans le PIB, etc.)
#'
#' @format Un data frame avec 223 observations et 16 variables:
#' \describe{
#'   \item{scenario}{Scénario de projection : "obs" pour l'observé, "+1,x%/an" pour les projections}
#'   \item{annee}{Année}
#'   \item{partretrpib}{Part des dépenses de retraite dans le PIB (en %)}
#'   \item{pensmoyrel}{Pension moyenne relative au revenu d'activité moyen (en %)}
#'   \item{pensmoynetterel}{Pensionne moyenne nette relative (en %)}
#'   \item{ratiocotretr}{Rapport entre le nombre de cotisants et le nombre de retraités}
#'   \item{ageconjretr}{Âge conjoncturel de départ à la retraite}
#'   \item{partressourcespibconveec}{Part des ressources du système de retraite dans le PIB (en %), convention EEC}
#'   \item{partressourcespibconvtcc}{Part des ressources du système de retraite dans le PIB (en %), convention TCC}
#'   \item{partressourcespubconvepr}{Part des ressources du système de retraite dans le PIB (en %), convention EPR}
#'   \item{nbretr}{Nombre de retraités (en millions)}
#'   \item{nbcotis}{Nombre de cotisants (en millions)}
#'   \item{partsalvapa}{Part des revenus d'activité dans la valeur ajoutée}
#'   \item{pensmoynette}{Pension moyenne nette (en euros 2019)}
#'   \item{revactmoynet}{Revenu d'activité moyen net (en euros 2019)}
#'   \item{rationnivvie}{}
#' }
"projcor"

#' Indicateurs conjoncturels de suivi des retraites en France
#'
#' Cette base rassemble divers indicateurs "conjoncturels" de suivi du système de retraite français.
#'
#' Les indicateurs dits "conjoncturels" calculent pour une année donnée les valeurs d'indicateurs qui se calculent normalement par génération.
#' Le principe du calcul consiste à considérer la situation d'une génération fictive qui aurait, à chaque âge, les caractéristiques
#' (taux de mortalité, probabilité d'être en emploi ou à la retraite, etc.) observées pour les personnes de cet âge au cours de l'année d'observation.
#' Le calcul utilise la méthode de Sullivan.
#'
#' Les indicateurs diffusés dans la base sont soit repris des sites de diffusions de la DREES ou du COR, soit calculés à partir des données disponibles en open data.
#'
#' @format A data frame with 408 observations and 6 variables:
#' \describe{
#'   \item{indicateur}{intitulé de l'indicateur}
#'   \item{sexe}{ensemble, femmes, hommes}
#'   \item{annee}{année}
#'   \item{valeur}{valeur de l'indicateur}
#'   \item{unite}{unité de l'indicateur (%, années, etc.)}
#'   \item{geo}{lieu de résidence}
#' }
"indicconj"
