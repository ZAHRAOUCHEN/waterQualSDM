#' Données de qualité du sol simulées pour le Maroc
#'
#' Un jeu de données contenant les mesures de Soil Organic Carbon (SOC)
#' pour 200 stations d'échantillonnage au Maroc, extraites depuis
#' SoilGrids et des données environnementales réelles.
#'
#' @format Un data frame avec 200 lignes et 9 variables :
#' \describe{
#'   \item{station_id}{Identifiant de la station (MAR001, MAR002, ...)}
#'   \item{longitude}{Longitude de la station (degrés décimaux)}
#'   \item{latitude}{Latitude de la station (degrés décimaux)}
#'   \item{date}{Date de prélèvement}
#'   \item{soc}{Soil Organic Carbon en g/kg (source: SoilGrids 0-5cm)}
#'   \item{landuse}{Classe d'occupation du sol (1=Agriculture, 2=Forêt, 3=Urbain, 4=Autre)}
#'   \item{slope}{Pente en degrés (source: SRTM)}
#'   \item{altitude}{Altitude en mètres (source: SRTM)}
#'   \item{precipitation}{Précipitations annuelles en mm (source: WorldClim)}
#' }
#' @source SoilGrids, ESA World Cover, SRTM, WorldClim
"water_quality"


#' Régions administratives du Maroc
#'
#' Un objet sf contenant les régions administratives du Maroc
#' et le Sahara Occidental (source: GADM 4.1).
#'
#' @format Un objet sf avec 16 lignes et 2 variables :
#' \describe{
#'   \item{NAME_1}{Nom de la région}
#'   \item{geometry}{Géométrie du polygone}
#' }
#' @source GADM 4.1 (https://gadm.org)
"watershed_sf"


#' Cours d'eau simulés
#'
#' Un objet sf contenant 2 cours d'eau fictifs pour démonstration.
#'
#' @format Un objet sf avec 2 lignes et 3 variables :
#' \describe{
#'   \item{river_id}{Identifiant du cours d'eau}
#'   \item{river_name}{Nom du cours d'eau}
#'   \item{geometry}{Géométrie de la ligne}
#' }
#' @source Données simulées
"rivers_sf"
