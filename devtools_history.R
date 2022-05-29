library(devtools)
library(usethis)

# == 23/04/2022 (création du package)

usethis::use_build_ignore("devtools_history.R")

# == 21/05/2022 : création du repo github

use_readme_rmd()

#  == 28/05/2022 : ajout données en open data

usethis::use_import_from("openxlsx", "read.xlsx")
