library(devtools)
library(usethis)
library(pkgdown)

# == 23/04/2022 (création du package)

usethis::use_build_ignore("devtools_history.R")

# == 21/05/2022 : création du repo github

use_readme_rmd()

#  == 28/05/2022 : ajout données en open data

usethis::use_import_from("openxlsx", "read.xlsx")

# == 23/06/2022 : création du site web avec pkgdown

usethis::use_pkgdown()
pkgdown::build_site()
#usethis::use_pkgdown_github_pages()

# == 11/11/2022 : ajout des modalités des variables de l'EIR

# == 13/02/2023 : ajout de la table 'ageretr' (à partir des données opendata EACR)

# == 17/02/2023 : (EN COURS) ajout de la table 'indicgen' (à partir des données opendata EACR)

# == 08/01/2025 : création de la fonction 'extrait_tableau_ccss'