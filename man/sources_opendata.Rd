% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{sources_opendata}
\alias{sources_opendata}
\title{Une table contenant les informations permettant d'extraire des données sur la retraite diffusées en open data}
\format{
A data frame with 125 observations and 14 variables:
\describe{
\item{intitule}{Intitulé de l'indicateur}
\item{producteur}{Organisme producteur des données}
\item{reference}{Ouvrage de référence ou modalité de diffusion des données}
\item{reference2}{Référence complémentaire (ex. n° de fiche)}
\item{datepubli}{Année d'édition ou date de publication}
\item{champ_sexe}{Champ de l'indicateur en termes de sexe}
\item{champ_geo}{Champ de l'indicateur en termes géographique}
\item{champ_autre}{Autres indications de champ de l'indicateur}
\item{onglet}{Nom de l'onglet dans le fichier excel}
\item{zone}{Délimitations de la zone dans l'onglet excel}
\item{url}{Adresse url du fichier}
\item{annee_en_colonnes}{Vaut TRUE si les colonnes dans l'onglet Excel correspondent aux années}
}
}
\usage{
sources_opendata
}
\description{
Les données proviennent du site d'open data de la DREES (data.drees), du site internet du COR, etc.
}
\keyword{datasets}
