#' Extrait des données diffusées en open data
#'
#' Récupère des données sur la retraite diffusées en open data et référencées dans la table sources_opendata,
#' et les restitue sous la forme d'un dataframe.
#'
#' @param intitule intitulé de la série en open data
#' @param datepubli date de publication
#' @param champ_sexe champ retenu par sexe
#' @param champ_geo champ géographique retenu
#' @param champ_autre autre indication du champ retenu
#'
#' @importFrom dplyr recode
#' @importFrom openxlsx read.xlsx
#' @importFrom cellranger as.cell_limits
#' @importFrom janitor clean_names
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace
#'
#' @return un dataframe
#' @export
#'
#' @examples
extrait_opendata <- function(intitule = NULL,
                             intitulecourt = NULL,
                             datepubli = NA,
                             champ_sexe = NA, champ_geo = NA, champ_autre = NA) {

  # == on prévoit des formes simplifiées des intitulés disponibles dans sources_opendata

  intitulecourtloc <- intitulecourt

  if (!is.null(intitule)) {
    intituleloc <- recode(enc2utf8(intitule),
                          "taux de retraités" = "taux de retraités par âge",
                          "txretr" = "taux de retraités par âge",
                          "taux de nouveaux retraités" = "taux de nouveaux retraités par âge",
                          "tauxnouvretr" = "taux de nouveaux retraités par âge")
    if (!(intituleloc %in% sources_opendata$intitule)) {warning(paste0("Intitulé '",intituleloc,"' non retrouvé"))}
  }

  datepubliloc <- datepubli
  champ_sexeloc <- champ_sexe
  champ_geoloc <- champ_geo
  champ_autreloc <- champ_autre

  # == extraction des données

  if (!is.null(intitulecourt)) { donnees <- sources_opendata %>% filter(intitulecourt==intitulecourtloc)}
  else { donnees <- sources_opendata %>% filter(intitule==intituleloc) }
  if (!is.na(datepubliloc)) {donnees <- donnees %>% filter(datepubli==datepubliloc)}
  if (!is.na(champ_sexeloc)) {donnees <- donnees %>% filter(champ_sexe==champ_sexeloc)}
  if (!is.na(champ_geoloc)) {donnees <- donnees %>% filter(champ_geo==champ_geoloc)}
  if (!is.na(champ_autreloc)) {donnees <- donnees %>% filter(champ_autre==champ_autreloc)}

  if (nrow(donnees)>1) {
    vals <- do.call(
      "bind_rows",
      lapply(c(1:nrow(donnees)),
             function(i){extrait_opendata(donnees[i,]$intitule,donnees[i,]$intitulecourt,donnees[i,]$datepubli,donnees[i,]$champ_sexe,donnees[i,]$champ_geo,donnees[i,]$champ_autre)})
    )
  } else {

    if (!(length(donnees$zone)==1L)) {warning(paste("Problème rencontré pour la série ",donnees$intitulecourt))}
    rangecells <- cellranger::as.cell_limits(donnees$zone)

    vals <- read.xlsx(
      xlsxFile = donnees$url,
      sheet = donnees$onglet,
      rows = c(rangecells$ul[1]:rangecells$lr[1]),
      cols = c(rangecells$ul[2]:rangecells$lr[2]),
      colNames = TRUE, rowNames = FALSE
    ) %>%
      janitor::clean_names()

    # == cas où plusieurs séries étaient dans le fichier excel initial

    if (!(is.na(donnees$complzone))) {
      vals <- vals %>%
        mutate(x1=factor(x1,levels=unique(vals$x1)) %>% as.numeric() ) %>%
        fill(x1,.direction="downup") %>%
        filter(x1==donnees$complzone) %>%
        select(-x1) %>%
        rename(x1=x2)
    }

  if (donnees$annees_en_colonnes) {

    cols_an <- names(vals)[grepl("^x[[:digit:]]{4}$",names(vals))]

    cols_error <- names(vals)[!grepl("^x[[:digit:]]+$",names(vals))]

    vals <- vals %>%
      select(-all_of(cols_error)) %>%
      pivot_longer(cols=cols_an,names_to="annee",values_to="valeurs") %>%
      mutate(annee = annee %>% str_replace("[^[:digit:]]","") %>% as.numeric() )

  }

    if (!is.na(donnees$champ_sexe)) {vals <- vals %>% mutate(sexe=donnees$champ_sexe)}
    if (!is.na(donnees$champ_geo)) {vals <- vals %>% mutate(geo=donnees$champ_geo)}
    if (!is.na(donnees$champ_autre)) {vals <- vals %>% mutate(champ_autre=donnees$champ_autre)}

  }

  # == retourne la table

  return(vals)

}
