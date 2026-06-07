#' Import des données qualité de l'eau
#'
#' @param file Chemin vers le fichier CSV ou Excel
#' @param lat Nom de la colonne latitude (défaut: "latitude")
#' @param lon Nom de la colonne longitude (défaut: "longitude")
#' @param date_col Nom de la colonne date (défaut: "date")
#' @param crs Système de coordonnées (défaut: 4326)
#' @return Une liste avec un objet sf et un dataframe qualité eau
#' @export
#' @examples
#' \dontrun{
#' data <- import_water_quality_data("stations.csv")
#' sf_obj <- data$sf
#' df     <- data$dataframe
#' }
import_water_quality_data <- function(file, lat = "latitude", lon = "longitude",
                                      date_col = "date", crs = 4326) {

  # Détection du format
  ext <- tools::file_ext(file)

  if (ext == "csv") {
    df <- read.csv(file, stringsAsFactors = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(file)
  } else {
    stop("Format non supporté. Utilisez CSV ou Excel.")
  }

  # Gestion des dates
  if (date_col %in% names(df)) {
    df[[date_col]] <- as.Date(df[[date_col]])
    message("Colonne date détectée et convertie : ", date_col)
  } else {
    warning("Colonne date non trouvée. Vérifiez le paramètre date_col.")
  }

  # Gestion des phosphates (optionnels)
  if ("phosphates" %in% names(df)) {
    df$phosphates <- as.numeric(df$phosphates)
    message("Colonne phosphates détectée.")
  }

  # Nettoyage
  df <- df[!duplicated(df), ]
  df <- df[complete.cases(df[, c(lat, lon)]), ]

  # Vérification colonne nitrates
  if (!"nitrates" %in% names(df) & !"soc" %in% names(df)) {
    warning("Aucune colonne de qualité (nitrates ou soc) trouvée dans les données.")
  }

  # Conversion en objet spatial sf
  sf_obj <- sf::st_as_sf(df, coords = c(lon, lat), crs = crs)

  # Retourner les deux outputs demandés
  result <- list(
    sf        = sf_obj,
    dataframe = df
  )

  return(result)
}




#' Import des données d'occupation du sol
#'
#' @param file Chemin vers le fichier raster d'occupation du sol
#' @param watershed Objet sf pour découper le raster (optionnel)
#' @param reclassify Logical, reclassifier en grandes classes (défaut: TRUE)
#' @param target_res Résolution cible en mètres pour harmonisation (défaut: NULL)
#' @return Un objet SpatRaster avec les classes d'occupation du sol
#' @export
#' @examples
#' \dontrun{
#' landuse <- import_landuse_data("corine_land_cover.tif", target_res = 100)
#' }
import_landuse_data <- function(file, watershed = NULL, reclassify = TRUE, target_res = NULL) {

  # Chargement du raster
  landuse <- terra::rast(file)

  # Découpage si bassin versant fourni
  if (!is.null(watershed)) {
    watershed <- sf::st_transform(watershed, terra::crs(landuse))
    landuse   <- terra::crop(landuse, terra::vect(watershed))
    landuse   <- terra::mask(landuse, terra::vect(watershed))
  }

  # Harmonisation de la résolution si demandée
  if (!is.null(target_res)) {
    landuse <- terra::disagg(landuse,
                             fact = round(target_res / terra::res(landuse)[1]))
    message("Résolution harmonisée à : ", target_res, " mètres")
  }

  # Reclassification en grandes classes
  # 1 = Agriculture, 2 = Forêt, 3 = Urbain, 4 = Eau
  if (reclassify) {

    # Matrice pour Corine Land Cover (codes CLC)
    reclass_matrix <- matrix(c(
      100, 199, 3,  # Urbain
      200, 299, 1,  # Agriculture
      300, 399, 2,  # Forêt
      400, 499, 4,  # Zones humides / Eau
      500, 599, 4   # Eau
    ), ncol = 3, byrow = TRUE)

    landuse <- terra::classify(landuse, reclass_matrix)

    # Nommer les classes
    levels(landuse) <- data.frame(
      id    = 1:4,
      label = c("Agriculture", "Foret", "Urbain", "Eau")
    )

    message("Reclassification effectuée : Agriculture / Forêt / Urbain / Eau")
  }

  names(landuse) <- "landuse"

  return(landuse)
}





#' Import des bassins versants
#'
#' @param file Chemin vers le shapefile ou GeoJSON
#' @param crs Système de coordonnées cible (défaut: 4326)
#' @return Un objet sf des bassins versants
#' @export
#' @examples
#' \dontrun{
#' ws <- import_watershed("bassins.shp")
#' }
import_watershed <- function(file, crs = 4326) {

  ws <- sf::st_read(file, quiet = TRUE)
  ws <- sf::st_transform(ws, crs = crs)
  ws <- sf::st_make_valid(ws)

  # Calcul surface
  ws$area_km2 <- as.numeric(sf::st_area(ws)) / 1e6

  return(ws)
}




#' Import du Modèle Numérique de Terrain
#'
#' @param file Chemin vers le fichier raster (GeoTIFF)
#' @param watershed Objet sf pour découper le raster (optionnel)
#' @param target_res Résolution cible en mètres pour harmonisation (défaut: NULL)
#' @return Un objet SpatRaster avec altitude et pente
#' @export
#' @examples
#' \dontrun{
#' dem <- load_dem("mnt.tif", target_res = 100)
#' }
load_dem <- function(file, watershed = NULL, target_res = NULL) {

  # Chargement du raster
  dem <- terra::rast(file)

  # Découpage si bassin versant fourni
  if (!is.null(watershed)) {
    watershed <- sf::st_transform(watershed, terra::crs(dem))
    dem       <- terra::crop(dem, terra::vect(watershed))
    dem       <- terra::mask(dem, terra::vect(watershed))
  }

  # Harmonisation de la résolution si demandée
  if (!is.null(target_res)) {
    fact <- round(target_res / terra::res(dem)[1])
    if (fact > 1) {
      dem <- terra::disagg(dem, fact = fact)
    } else if (fact < 1) {
      dem <- terra::aggregate(dem, fact = round(1 / fact))
    }
    message("Résolution harmonisée à : ", target_res, " mètres")
  }

  # Calcul de la pente en degrés
  slope <- terra::terrain(dem, v = "slope", unit = "degrees")

  # Assemblage altitude + pente
  result        <- c(dem, slope)
  names(result) <- c("altitude", "slope")

  message("MNT chargé : altitude + pente calculée")

  return(result)
}
