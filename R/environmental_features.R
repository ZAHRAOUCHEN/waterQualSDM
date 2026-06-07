#' Calcul des variables hydrologiques
#'
#' @param dem SpatRaster avec altitude et pente (issu de load_dem)
#' @param rivers Objet sf des cours d'eau (optionnel)
#' @param watershed Objet sf des bassins versants (optionnel)
#' @return Une liste avec le raster hydrologique et les variables par bassin
#' @export
#' @examples
#' \dontrun{
#' hydro <- calculate_hydrological_variables(dem, rivers, watershed)
#' }
calculate_hydrological_variables <- function(dem, rivers = NULL, watershed = NULL) {

  # Extraction de l'altitude et pente
  altitude <- dem[["altitude"]]
  slope    <- dem[["slope"]]

  # Accumulation de flux simple
  flow_acc        <- terra::terrain(altitude, v = "flowdir")
  names(flow_acc) <- "flow_accumulation"

  # Distance aux cours d'eau
  if (!is.null(rivers)) {
    rivers_vect <- terra::vect(sf::st_transform(rivers, terra::crs(altitude)))
    dist_river  <- terra::distance(altitude, rivers_vect)
    names(dist_river) <- "dist_river"
    message("Distance aux cours d'eau calculée.")
  } else {
    dist_river        <- altitude * 0
    names(dist_river) <- "dist_river"
    warning("Aucune donnée rivière fournie. dist_river mis à 0.")
  }

  # Assemblage raster hydrologique
  hydro_raster        <- c(slope, flow_acc, dist_river)
  names(hydro_raster) <- c("slope", "flow_accumulation", "dist_river")

  # Extraction des valeurs par bassin versant
  if (!is.null(watershed)) {
    ws_vect    <- terra::vect(watershed)
    basin_vals <- terra::extract(hydro_raster, ws_vect, fun = mean, na.rm = TRUE)
    basin_df   <- cbind(
      sf::st_drop_geometry(watershed),
      slope_mean        = round(basin_vals$slope, 2),
      flow_acc_mean     = round(basin_vals$flow_accumulation, 2),
      dist_river_mean   = round(basin_vals$dist_river, 2)
    )
    message("Variables extraites par bassin versant.")
  } else {
    basin_df <- NULL
    warning("Aucun bassin versant fourni. Extraction par bassin non effectuée.")
  }

  # Retourner raster + variables par bassin
  result <- list(
    raster     = hydro_raster,
    basin_vars = basin_df
  )

  return(result)
}




#' Extraction des variables environnementales aux stations
#'
#' @param water_quality Objet sf des stations qualité eau
#' @param landuse SpatRaster d'occupation du sol
#' @param hydro Liste issue de calculate_hydrological_variables
#' @param rainfall SpatRaster de pluie (optionnel)
#' @return Un dataframe avec toutes les variables par station
#' @export
#' @examples
#' \dontrun{
#' features <- extract_environmental_features(stations, landuse, hydro)
#' }
extract_environmental_features <- function(water_quality, landuse, hydro,
                                           rainfall = NULL) {

  # Reprojection des points dans le CRS des rasters
  wq_proj <- sf::st_transform(water_quality, terra::crs(landuse))
  pts     <- terra::vect(wq_proj)

  # Extraction occupation du sol
  vals_landuse <- terra::extract(landuse, pts)

  # Extraction variables hydrologiques
  hydro_raster <- hydro$raster
  vals_hydro   <- terra::extract(hydro_raster, pts)

  # Assemblage de base
  df <- cbind(
    sf::st_drop_geometry(water_quality),
    landuse           = vals_landuse[, 2],
    slope             = vals_hydro[["slope"]],
    flow_accumulation = vals_hydro[["flow_accumulation"]],
    dist_river        = vals_hydro[["dist_river"]]
  )

  # Pluie optionnelle
  if (!is.null(rainfall)) {
    vals_rainfall  <- terra::extract(rainfall, pts)
    df$rainfall_mm <- vals_rainfall[, 2]
    message("Variable pluie ajoutée.")
  } else {
    message("Aucune donnée pluie fournie. Variable ignorée.")
  }

  # Garder landuse comme numérique
  df$landuse <- as.numeric(vals_landuse[, 2])

  message("Extraction environnementale terminée : ", ncol(df), " variables, ",
          nrow(df), " stations.")

  return(df)
}





#' Prétraitement des données pour la modélisation
#'
#' @param df Dataframe issu de extract_environmental_features
#' @param target Nom de la colonne cible (défaut: "nitrates")
#' @param test_size Proportion pour le jeu de test (défaut: 0.2)
#' @param normalize Logical, normaliser les variables (défaut: TRUE)
#' @param vif_threshold Seuil VIF pour suppression (défaut: 5)
#' @return Une liste avec train, test et dataset propre
#' @export
#' @examples
#' \dontrun{
#' data_prep <- preprocess_data(features, target = "nitrates")
#' }
preprocess_data <- function(df, target = "nitrates", test_size = 0.2,
                            normalize = TRUE, vif_threshold = 5) {

  # Suppression des NA
  df <- df[complete.cases(df), ]
  message("Lignes après suppression NA : ", nrow(df))

  # Séparation variables explicatives / cible
  y <- df[[target]]
  X <- df[, !names(df) %in% target]

  # Garder uniquement les colonnes numériques
  X <- X[, sapply(X, is.numeric)]

  # Suppression des variables trop corrélées (corrélation Pearson > 0.95)
  cor_matrix <- cor(X, use = "complete.obs")
  high_cor   <- caret::findCorrelation(cor_matrix, cutoff = 0.95)
  if (length(high_cor) > 0) {
    message("Variables corrélées supprimées : ",
            paste(names(X)[high_cor], collapse = ", "))
    X <- X[, -high_cor]
  }

  # Calcul VIF simple et suppression des variables avec VIF élevé
  if (ncol(X) > 1) {
    vif_vals <- tryCatch({
      vif_df <- as.data.frame(X)
      vif_df$y <- y
      vif_result <- sapply(names(X), function(var) {
        formula_vif <- as.formula(paste(var, "~ ."))
        r2_vif <- summary(lm(formula_vif, data = vif_df))$r.squared
        1 / (1 - r2_vif)
      })
      vif_result
    }, error = function(e) NULL)

    if (!is.null(vif_vals)) {
      high_vif <- names(vif_vals[vif_vals > vif_threshold])
      if (length(high_vif) > 0) {
        message("Variables supprimées par VIF > ", vif_threshold, " : ",
                paste(high_vif, collapse = ", "))
        X <- X[, !names(X) %in% high_vif]
      }
    }
  }

  # Normalisation (min-max) si demandée
  if (normalize) {
    X <- as.data.frame(lapply(X, function(col) {
      min_val <- min(col, na.rm = TRUE)
      max_val <- max(col, na.rm = TRUE)
      if (max_val == min_val) return(col)
      (col - min_val) / (max_val - min_val)
    }))
    message("Variables normalisées (min-max).")
  }

  # Split train / test
  set.seed(42)
  n         <- nrow(df)
  test_idx  <- sample(1:n, size = floor(test_size * n))
  train_idx <- setdiff(1:n, test_idx)

  train_df <- cbind(X[train_idx, ], y[train_idx])
  test_df  <- cbind(X[test_idx, ],  y[test_idx])
  clean_df <- cbind(X, y)

  names(train_df)[ncol(train_df)] <- target
  names(test_df)[ncol(test_df)]   <- target
  names(clean_df)[ncol(clean_df)] <- target

  result <- list(
    train         = train_df,
    test          = test_df,
    feature_names = names(X),
    clean_data    = clean_df
  )

  message("Split train/test : ", length(train_idx), " train / ",
          length(test_idx), " test.")

  return(result)
}
