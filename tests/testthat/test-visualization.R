# Tests pour les fonctions de visualisation
library(testthat)
library(terra)
library(sf)
library(ggplot2)
devtools::load_all()

# Créer des données de test simples
set.seed(42)

# Raster de test
r <- terra::rast(nrows = 10, ncols = 10,
                 xmin = -6, xmax = -4,
                 ymin = 32, ymax = 34,
                 crs = "EPSG:4326")

# SOC raster
soc_test <- r
terra::values(soc_test) <- abs(rnorm(100, 25, 10))
names(soc_test) <- "soc_predicted"

# Vulnerability raster
vuln_test <- r
terra::values(vuln_test) <- sample(1:3, 100, replace = TRUE)
names(vuln_test) <- "vulnerability"
levels(vuln_test) <- data.frame(id = 1:3, label = c("Faible", "Moyen", "Élevé"))

# Landuse raster
landuse_test <- r
terra::values(landuse_test) <- sample(1:4, 100, replace = TRUE)
names(landuse_test) <- "landuse"

# Watershed sf
watershed_test <- sf::st_sf(
  NAME_1   = c("Region1", "Region2"),
  area_km2 = c(100, 200),
  geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(-6,32), c(-5,32), c(-5,33), c(-6,33), c(-6,32)))),
    sf::st_polygon(list(rbind(c(-5,32), c(-4,32), c(-4,33), c(-5,33), c(-5,32))))
  ),
  crs = 4326
)

# Summary dataframe
summary_test <- data.frame(
  NAME_1             = c("Region1", "Region2"),
  soc_mean_gkg       = c(25.5, 35.2),
  vulnerability_mean = c(0.4, 0.7),
  agriculture_pct    = c(45.0, 60.0),
  risk_class         = c("Moyen", "Élevé")
)

# ============================================================
# Tests plot_water_quality_map()
# ============================================================

test_that("plot_water_quality_map retourne une liste", {
  result <- plot_water_quality_map(
    quality_raster = soc_test,
    vuln_raster    = vuln_test,
    watershed      = watershed_test
  )
  expect_type(result, "list")
})

test_that("plot_water_quality_map retourne 3 cartes", {
  result <- plot_water_quality_map(
    quality_raster = soc_test,
    vuln_raster    = vuln_test,
    watershed      = watershed_test
  )
  expect_true("carte_soc" %in% names(result))
  expect_true("carte_vulnerabilite" %in% names(result))
  expect_true("carte_bassins" %in% names(result))
})

test_that("plot_water_quality_map cartes sont des ggplot", {
  result <- plot_water_quality_map(
    quality_raster = soc_test,
    vuln_raster    = vuln_test,
    watershed      = watershed_test
  )
  expect_s3_class(result$carte_soc, "ggplot")
  expect_s3_class(result$carte_vulnerabilite, "ggplot")
  expect_s3_class(result$carte_bassins, "ggplot")
})

# ============================================================
# Tests generate_recommendations()
# ============================================================

test_that("generate_recommendations retourne un dataframe", {
  result <- generate_recommendations(summary_test)
  expect_s3_class(result, "data.frame")
})

test_that("generate_recommendations contient colonne recommandations", {
  result <- generate_recommendations(summary_test)
  expect_true("recommandations" %in% names(result))
})

test_that("generate_recommendations une recommandation par bassin", {
  result <- generate_recommendations(summary_test)
  expect_equal(nrow(result), nrow(summary_test))
})
