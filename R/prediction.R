#' Prédiction spatiale de la qualité de l'eau
#'
#' @param model_result Liste issue de train_rf_model
#' @param landuse SpatRaster d'occupation du sol
#' @param hydro SpatRaster des variables hydrologiques
#' @param output_file Chemin pour exporter le GeoTIFF (optionnel)
#' @return Un SpatRaster des concentrations en nitrates prédites
#' @export
#' @examples
#' \dontrun{
#' nitrates_map <- predict_water_quality(model_result, landuse, hydro)
#' }
predict_water_quality <- function(model_result, landuse, hydro, output_file = NULL) {

  message("Prédiction spatiale en cours...")

  # Assemblage des rasters en un stack
  predictors <- c(landuse, hydro)

  # Conversion en dataframe pour la prédiction
  pred_df <- as.data.frame(predictors, na.rm = FALSE)

  # Garder seulement les variables utilisées par le modèle
  features <- model_result$features
  pred_df  <- pred_df[, features, drop = FALSE]

  # Prédiction
  predictions <- predict(model_result$model, data = pred_df)$predictions

  # Remettre dans un raster
  result_raster        <- landuse[[1]]
  terra::values(result_raster) <- predictions
  names(result_raster)  <- "nitrates_predicted"

  # Export GeoTIFF si demandé
  if (!is.null(output_file)) {
    terra::writeRaster(result_raster, output_file, overwrite = TRUE)
    message("Carte exportée : ", output_file)
  }

  return(result_raster)
}


#' Calcul de l'indice de vulnérabilité
#'
#' @param nitrates_raster SpatRaster des nitrates prédits
#' @param landuse SpatRaster d'occupation du sol
#' @param hydro SpatRaster des variables hydrologiques
#' @param output_file Chemin pour exporter le GeoTIFF (optionnel)
#' @return Un SpatRaster de l'indice de vulnérabilité (1=faible, 2=moyen, 3=élevé)
#' @export
#' @examples
#' \dontrun{
#' vuln <- calculate_vulnerability_index(nitrates_map, landuse, hydro)
#' }
calculate_vulnerability_index <- function(nitrates_raster, landuse, hydro, output_file = NULL) {

  message("Calcul de l'indice de vulnérabilité...")

  # Normalisation de chaque variable entre 0 et 1
  normalize <- function(x) {
    (x - terra::global(x, "min", na.rm = TRUE)[[1]]) /
      (terra::global(x, "max", na.rm = TRUE)[[1]] -
         terra::global(x, "min", na.rm = TRUE)[[1]])
  }

  nitrates_norm  <- normalize(nitrates_raster)
  slope_norm     <- normalize(hydro[["slope"]])
  dist_river_inv <- normalize(1 / (hydro[["dist_river"]] + 1))

  # Agriculture = 1 dans notre classification
  agriculture <- landuse == 1

  # Indice composite (pondéré)
  vulnerability <- (0.4 * nitrates_norm) +
    (0.3 * agriculture) +
    (0.2 * slope_norm) +
    (0.1 * dist_river_inv)

  # Classification en 3 classes
  vuln_class <- terra::classify(vulnerability, matrix(c(
    0,   0.33, 1,   # Faible
    0.33, 0.66, 2,  # Moyen
    0.66, 1,    3   # Élevé
  ), ncol = 3, byrow = TRUE))

  levels(vuln_class) <- data.frame(
    id    = 1:3,
    label = c("Faible", "Moyen", "Élevé")
  )
  names(vuln_class) <- "vulnerability"

  # Export si demandé
  if (!is.null(output_file)) {
    terra::writeRaster(vuln_class, output_file, overwrite = TRUE)
    message("Carte exportée : ", output_file)
  }

  return(vuln_class)
}


#' Résumé statistique par bassin versant
#'
#' @param watershed Objet sf des bassins versants
#' @param nitrates_raster SpatRaster des nitrates prédits
#' @param vuln_raster SpatRaster de vulnérabilité
#' @param landuse SpatRaster d'occupation du sol
#' @return Un dataframe avec les statistiques par bassin
#' @export
#' @examples
#' \dontrun{
#' summary <- summarize_watersheds(watershed, nitrates_map, vuln)
#' }
summarize_watersheds <- function(watershed, nitrates_raster, vuln_raster, landuse) {

  message("Calcul des statistiques par bassin versant...")

  ws_vect <- terra::vect(watershed)

  # Nitrates moyens par bassin
  nitrates_mean <- terra::extract(nitrates_raster, ws_vect, fun = mean, na.rm = TRUE)

  # Vulnérabilité moyenne par bassin
  vuln_mean <- terra::extract(vuln_raster, ws_vect, fun = mean, na.rm = TRUE)

  # % agriculture par bassin
  agri_pct <- terra::extract(landuse == 1, ws_vect, fun = mean, na.rm = TRUE)

  # Assemblage
  result <- sf::st_drop_geometry(watershed)
  result$nitrates_mean_mgl <- round(nitrates_mean[, 2], 2)
  result$vulnerability_mean <- round(vuln_mean[, 2], 2)
  result$agriculture_pct    <- round(agri_pct[, 2] * 100, 1)
  result$risk_class <- ifelse(
    result$vulnerability_mean >= 0.66, "Élevé",
    ifelse(result$vulnerability_mean >= 0.33, "Moyen", "Faible")
  )

  return(result)
}
