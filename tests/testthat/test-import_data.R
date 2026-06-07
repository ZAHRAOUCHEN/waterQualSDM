# Tests pour les fonctions d'import
library(testthat)
library(sf)
library(terra)
devtools::load_all()

# ============================================================
# Tests import_water_quality_data()
# ============================================================

test_that("import_water_quality_data retourne une liste", {
  file <- system.file("extdata", "water_quality.csv", package = "waterQualSDM")
  result <- import_water_quality_data(file)
  expect_type(result, "list")
})

test_that("import_water_quality_data retourne un objet sf", {
  file <- system.file("extdata", "water_quality.csv", package = "waterQualSDM")
  result <- import_water_quality_data(file)
  expect_s3_class(result$sf, "sf")
})

test_that("import_water_quality_data retourne un dataframe", {
  file <- system.file("extdata", "water_quality.csv", package = "waterQualSDM")
  result <- import_water_quality_data(file)
  expect_s3_class(result$dataframe, "data.frame")
})

test_that("import_water_quality_data contient colonne soc", {
  file <- system.file("extdata", "water_quality.csv", package = "waterQualSDM")
  result <- import_water_quality_data(file)
  expect_true("soc" %in% names(result$dataframe))
})

test_that("import_water_quality_data erreur si format inconnu", {
  expect_error(import_water_quality_data("fichier.txt"))
})

# ============================================================
# Tests import_watershed()
# ============================================================

test_that("import_watershed retourne un objet sf", {
  file <- system.file("extdata", "watersheds.geojson", package = "waterQualSDM")
  result <- import_watershed(file)
  expect_s3_class(result, "sf")
})

test_that("import_watershed contient colonne area_km2", {
  file <- system.file("extdata", "watersheds.geojson", package = "waterQualSDM")
  result <- import_watershed(file)
  expect_true("area_km2" %in% names(result))
})

# ============================================================
# Tests load_dem()
# ============================================================

test_that("load_dem retourne un SpatRaster", {
  file <- system.file("extdata", "dem.tif", package = "waterQualSDM")
  result <- load_dem(file)
  expect_s4_class(result, "SpatRaster")
})

test_that("load_dem retourne altitude et slope", {
  file <- system.file("extdata", "dem.tif", package = "waterQualSDM")
  result <- load_dem(file)
  expect_true("altitude" %in% names(result))
  expect_true("slope" %in% names(result))
})

# ============================================================
# Tests import_landuse_data()
# ============================================================

test_that("import_landuse_data retourne un SpatRaster", {
  file <- system.file("extdata", "landuse.tif", package = "waterQualSDM")
  result <- import_landuse_data(file)
  expect_s4_class(result, "SpatRaster")
})

test_that("import_landuse_data retourne une couche nommée landuse", {
  file <- system.file("extdata", "landuse.tif", package = "waterQualSDM")
  result <- import_landuse_data(file)
  expect_equal(names(result), "landuse")
})
