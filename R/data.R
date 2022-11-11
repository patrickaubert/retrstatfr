#' Une table contenant les informations permettant d'extraire des données sur la retraite diffusées en open data
#'
#' Les données proviennent du site d'open data de la DREES (data.drees), du site internet du COR, etc.
#'
#' @format A data frame with 32 observations and 14 variables:
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
#' @format Un data frame avec 491 observations et 21 variables:
#' \describe{
#'   \item{scenario}{Scénario de projection : "obs" pour l'observé, "+1,x%/an" pour les projections}
#'   \item{annee}{Année}
#'   \item{partretrpib}{Part des dépenses de retraite dans le PIB (en %)}
#'   \item{pensmoyrel}{Pension moyenne relative au revenu d'activité moyen (en %)}
#'   \item{pensmoynetterel}{Pensionne moyenne nette relative (en %)}
#'   \item{ratiocotretr}{Rapport entre le nombre de cotisants et le nombre de retraités}
#'   \item{ageconjretr}{Âge conjoncturel de départ à la retraite}
#'   \item{partressourcespib}{Part des ressources du système de retraite dans le PIB (en %), observé}
#'   \item{partressourcespibconveec}{Part des ressources du système de retraite dans le PIB (en %), convention EEC}
#'   \item{partressourcespibconvtcc}{Part des ressources du système de retraite dans le PIB (en %), convention TCC}
#'   \item{partressourcespubconvepr}{Part des ressources du système de retraite dans le PIB (en %), convention EPR}
#'   \item{txprelev}{Taux de prélèvement global (en % de la masse des revenus d'activité), observé}
#'   \item{txprelevconveec}{Taux de prélèvement global (en % de la masse des revenus d'activité), convention EEC}
#'   \item{txprelevconvepr}{Taux de prélèvement global (en % de la masse des revenus d'activité), convention EPR}
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

#' Index des titres et dates des séances mensuelles du COR
#'
#' (version provisoire, encore en cours de construction)
#'
#' Données extraites fin août 2022, s'arrêtant à la séance de juillet 2022.
#'
#' @format A data frame with 304 observations and 8 variables:
#' \describe{
#'   \item{titre.seance}{Titre de la séance}
#'   \item{date.seance}{Date de la séance}
#'   \item{url.seance}{url de la page présentant la séance sur le site internet du COR}
#'   \item{annee}{Année de la séance}
#'   \item{mois}{Mois de la séance}
#'   \item{jour}{Jour de la séance}
#'   \item{texte.introductif}{Texte de présentation de la séance}
#'   \item{mots.cles}{Mots clés de la séance (tels que fournis par le COR)}
#' }
"seances_cor"

#' Table des documents diffusés dans le cadre des séances mensuelles du COR
#'
#' (version provisoire, encore en cours de construction)
#'
#' Données extraites fin août 2022, s'arrêtant à la séance de juillet 2022.
#'
#' @format A data frame with 2332 observations and 19 variables:
#' \describe{
#'   \item{seance}{Titre de la séance}
#'   \item{date.seance}{Date de la séance}
#'   \item{url.seance}{url de la page présentant la séance sur le site internet du COR}
#'   \item{annee}{Année de la séance}
#'   \item{mois}{Mois de la séance}
#'   \item{jour}{Jour de la séance}
#'   \item{texte.introductif}{Texte de présentation de la séance}
#'   \item{mots.cles}{Mots clés de la séance (tels que fournis par le COR)}
#'   \item{partie}{Titre de la partie}
#'   \item{titre_complet}{Titre complet du document}
#'   \item{auteur}{Auteur(s) du document}
#'   \item{url.doc}{Url du document sur le site internet du COR}
#'   \item{type}{Type de document (document, diaporama, fichier excel...)}
#'   \item{titre}{Titre du document}
#'   \item{num_document}{Numéro du document}
#'   \item{seance_dapres_pdf}{Titre de la séance, lu dans le fichier pdf}
#'   \item{numdocument_dapres_pdf}{Numero du document, lu dans le fichier pdf}
#'   \item{titre_dapres_pdf}{Titre du document, lu dans le fichier pdf}
#'   \item{auteur_dapres_pdf}{Auteur(s) du document, lu dans le fichier pdf}
#' }
"docs_cor"

#' Une base des questions/réponses extraites des documents "Le dossier en bref" du COR
#'
#' (version provisoire, encore en cours de construction)
#'
#' Données extraites fin août 2022, s'arrêtant à la séance de juillet 2022.
#'
#' @format A data frame with 582 observations and 7 variables:
#' \describe{
#'   \item{question}{Texte de la question extrait du document}
#'   \item{reponse}{Réponse à la question extrait du document}
#'   \item{reponse_avec_url}{Réponse à la question extrait du document, avec lien hypertexte vers les documents cités}
#'   \item{partie}{Titre intermédiaire extrait du document}
#'   \item{url.doc}{url du document sur le site internet du COR}
#'   \item{date.seance}{Date de la séance}
#'   \item{titre.seance}{Titre de la séance}
#' }
"dossiers_en_bref_cor"

#' Une table des variables de l'EIR et de leurs modalités
#'
#' Données récupérées sur le site du CASD.
#'
#' Voir aussi la table 'modalites_eir'.
#'
#' @format A data frame with 582 observations and 7 variables:
#' \describe{
#'   \item{variable}{Nom de la variable}
#'   \item{libelle}{Libellé de la variable}
#'   \item{modalites}{Liste des modalités de la variable (séparées par un ;)}
#'   \item{table}{Table dans laquelle la variable est disponible (avant, ddir, etc.)}
#' }
"variables_eir"

#' Une table des modalités des variables de l'EIR
#'
#' La table contient une ligne par modalité de chaque variable (une variable apparaît donc autant de fois qu'elle a de modalités distinctes),
#' contrairement à la table 'variables_eir', qui contient une ligne par variable.
#'
#' Données récupérées sur le site du CASD.
#'
#' @format A data frame with 582 observations and 7 variables:
#' \describe{
#'   \item{table}{Table dans laquelle la variable est disponible (avant, ddir, etc.)}
#'   \item{variable}{Nom de la variable}
#'   \item{modalite}{Valeur de chaque modalité de la variable (séparées par un ;)}
#'   \item{modalite}{Libellé en clair de la modalité}
#' }
"modalites_eir"
