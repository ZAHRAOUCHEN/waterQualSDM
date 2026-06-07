# Tests pour les fonctions de prédiction
library(testthat)
library(terra)
library(sf)
devtools::load_all()

# Créer des données de test simples
set.seed(42)

# Raster de test simple
r <- terra::rast(nrows = 10, ncols = 10,
                 xmin = -6, xmax = -4,
                 ymin = 32, ymax = 34,
                 crs = "EPSG:4326")

# Landuse raster
landuse_test <- r
terra::values(landuse_test) <- sample(1:4, 100, replace = TRUE)
names(landuse_test) <- "landuse"

# Slope raster
slope_test <- r
terra::values(slope_test) <- abs(rnorm(100, 5, 2))
names(slope_test) <- "slope"

# Altitude raster
altitude_test <- r
terra::values(altitude_test) <- abs(rnorm(100, 500, 200))
names(altitude_test) <- "altitude"

# Precipitation raster
precip_test <- r
terra::values(precip_test) <- abs(rnorm(100, 300, 100))
names(precip_test) <- "precipitation"

# Données d'entraînement
df_train <- data.frame(
  soc           = abs(rnorm(50, 25, 10)),
  landuse       = sample(1:4, 50, replace = TRUE),
  slope         = abs(rnorm(50, 5, 2)),
  altitude      = abs(rnorm(50, 500, 200)),
  precipitation = abs(rnorm(50, 300, 100))
)

data_prep    <- preprocess_data(df_train, target = "soc", normalize = TRUE)
model_result <- train_rf_model(data_prep, target = "soc", ntrees = 100)

# ============================================================
# Tests predict_water_quality()
# ============================================================

test_that("predict_water_quality retourne un SpatRaster", {
  result <- predict_water_quality(
    model_result = model_result,
    landuse      = landuse_test,
    hydro        = c(slope_test, altitude_test, precip_test)
  )
  expect_s4_class(result, "SpatRaster")
})

test_that("predict_water_quality retourne une couche nommée", {
  result <- predict_water_quality(
    model_result = model_result,
    landuse      = landuse_test,
    hydro        = c(slope_test, altitude_test, precip_test)
  )
  expect_equal(names(result), "nitrates_predicted")
})

test_that("predict_water_quality valeurs positives", {
  result <- predict_water_quality(
    model_result = model_result,
    landuse      = landuse_test,
    hydro        = c(slope_test, altitude_test, precip_test)
  )
  min_val <- terra::global(result, "min", na.rm = TRUE)[[1]]
  expect_true(min_val >= 0)
})

# ============================================================
# Tests calculate_vulnerability_index()
# ============================================================

test_that("calculate_vulnerability_index retourne un SpatRaster", {
  soc_map <- predict_water_quality(
    model_result = model_result,
    landuse      = landuse_test,
    hydro        = c(slope_test, altitude_test, precip_test)
  )
  result <- calculate_vulnerability_index(
    nitrates_raster = soc_map,
    landuse         = landuse_test,
    hydro           = c(slope_test, altitude_test, precip_test)
  )
  expect_s4_class(result, "SpatRaster")
})

test_that("calculate_vulnerability_index retourne 3 classes", {
  soc_map <- predict_water_quality(
    model_result = model_result,
    landuse      = landuse_test,
    hydro        = c(slope_test, altitude_test, precip_test)
  )
  result <- calculate_vulnerability_index(
    nitrates_raster = soc_map,
    landuse         = landuse_test,
    hydro           = c(slope_test, altitude_test, precip_test)
  )
  freq_df <- terra::freq(result)
  expect_true(nrow(freq_df) <= 3)
})

# ============================================================
# Tests summarize_watersheds()
# ============================================================

test_that("summarize_watersheds retourne un dataframe", {
  soc_map <- predict_water_quality(
    model_result = model_result,
    landuse      = landuse_test,
    hydro        = c(slope_test, altitude_test, precip_test)
  )
  vuln_map <- calculate_vulnerability_index(
    nitrates_raster = soc_map,
    landuse         = landuse_test,
    hydro           = c(slope_test, altitude_test, precip_test)
  )
  ws <- sf::st_sf(
    NAME_1   = c("Region1", "Region2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(-6,32), c(-5,32), c(-5,33), c(-6,33), c(-6,32)))),
      sf::st_polygon(list(rbind(c(-5,32), c(-4,32), c(-4,33), c(-5,33), c(-5,32))))
    ),
    crs = 4326
  )
  result <- summarize_watersheds(ws, soc_map, vuln_map, landuse_test)
  expect_s3_class(result, "data.frame")
})

test_that("summarize_watersheds contient les bonnes colonnes", {
  soc_map <- predict_water_quality(
    model_result = model_result,
    landuse      = landuse_test,
    hydro        = c(slope_test, altitude_test, precip_test)
  )
  vuln_map <- calculate_vulnerability_index(
    nitrates_raster = soc_map,
    landuse         = landuse_test,
    hydro           = c(slope_test, altitude_test, precip_test)
  )
  ws <- sf::st_sf(
    NAME_1   = c("Region1", "Region2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(-6,32), c(-5,32), c(-5,33), c(-6,33), c(-6,32)))),
      sf::st_polygon(list(rbind(c(-5,32), c(-4,32), c(-4,33), c(-5,33), c(-5,32))))
    ),
    crs = 4326
  )
  result <- summarize_watersheds(ws, soc_map, vuln_map, landuse_test)
  expect_true("nitrates_mean_mgl" %in% names(result))
  expect_true("vulnerability_mean" %in% names(result))
  expect_true("agriculture_pct" %in% names(result))
  expect_true("risk_class" %in% names(result))
})
