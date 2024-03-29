% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{modalites_eir}
\alias{modalites_eir}
\title{Une table des modalités des variables de l'EIR}
\format{
A data frame with 582 observations and 7 variables:
\describe{
\item{table}{Table dans laquelle la variable est disponible (avant, ddir, etc.)}
\item{variable}{Nom de la variable}
\item{modalite}{Valeur de chaque modalité de la variable (séparées par un ;)}
\item{modalite}{Libellé en clair de la modalité}
}
}
\usage{
modalites_eir
}
\description{
La table contient une ligne par modalité de chaque variable (une variable apparaît donc autant de fois qu'elle a de modalités distinctes),
contrairement à la table 'variables_eir', qui contient une ligne par variable.
}
\details{
Données récupérées sur le site du CASD.
}
\keyword{datasets}
