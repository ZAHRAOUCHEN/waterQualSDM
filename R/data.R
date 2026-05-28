#' Données de qualité de l'eau simulées
#'
#' Un jeu de données simulé contenant les mesures de qualité
#' de l'eau pour 50 stations de mesure fictives.
#'
#' @format Un data frame avec 50 lignes et 6 variables :
#' \describe{
#'   \item{station_id}{Identifiant de la station}
#'   \item{longitude}{Longitude de la station}
#'   \item{latitude}{Latitude de la station}
#'   \item{date}{Date de prélèvement}
#'   \item{nitrates}{Concentration en nitrates (mg/L)}
#'   \item{phosphates}{Concentration en phosphates (mg/L)}
#' }
#' @source Données simulées
"water_quality"


#' Bassins versants simulés
#'
#' Un objet sf contenant 5 bassins versants fictifs.
#'
#' @format Un objet sf avec 5 lignes et 3 variables :
#' \describe{
#'   \item{basin_id}{Identifiant du bassin}
#'   \item{basin_name}{Nom du bassin}
#'   \item{geometry}{Géométrie du polygone}
#' }
#' @source Données simulées
"watershed_sf"


#' Cours d'eau simulés
#'
#' Un objet sf contenant 2 cours d'eau fictifs.
#'
#' @format Un objet sf avec 2 lignes et 3 variables :
#' \describe{
#'   \item{river_id}{Identifiant du cours d'eau}
#'   \item{river_name}{Nom du cours d'eau}
#'   \item{geometry}{Géométrie de la ligne}
#' }
#' @source Données simulées
"rivers_sf"
