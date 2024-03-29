% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extrait_opendata.R
\name{extrait_opendata}
\alias{extrait_opendata}
\title{Extrait des données diffusées en open data}
\usage{
extrait_opendata(
  intitule = NULL,
  intitulecourt = NULL,
  datepubli = NA,
  champ_sexe = NA,
  champ_geo = NA,
  champ_autre = NA
)
}
\arguments{
\item{intitule}{intitulé de la série en open data}

\item{datepubli}{date de publication}

\item{champ_sexe}{champ retenu par sexe}

\item{champ_geo}{champ géographique retenu}

\item{champ_autre}{autre indication du champ retenu}
}
\value{
un dataframe
}
\description{
Récupère des données sur la retraite diffusées en open data et référencées dans la table sources_opendata,
et les restitue sous la forme d'un dataframe.
}
