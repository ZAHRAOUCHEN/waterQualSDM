# Script de génération des données simulées
# pour le package waterQualSDM
install.packages("here")
library(sf)
library(terra)
library(dplyr)
library(here)
library(usethis)

# S'assurer qu'on est dans le bon répertoire
setwd(here::here())

set.seed(42)

# ============================================================
# 1. Données qualité de l'eau (stations de mesure)
# ============================================================

n_stations <- 50

water_quality <- data.frame(
  station_id  = paste0("ST", sprintf("%03d", 1:n_stations)),
  longitude   = runif(n_stations, -2.5, -1.5),
  latitude    = runif(n_stations, 47.0, 48.0),
  date        = sample(
    seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"),
    n_stations
  ),
  nitrates    = round(abs(rnorm(n_stations, mean = 25, sd = 10)), 2),
  phosphates  = round(abs(rnorm(n_stations, mean = 0.5, sd = 0.2)), 3)
)

write.csv(water_quality,
          "inst/extdata/water_quality.csv",
          row.names = FALSE)

message("✅ Données qualité eau créées : ", nrow(water_quality), " stations")

# ============================================================
# 2. Bassins versants (polygones sf simulés)
# ============================================================

watersheds <- list(
  sf::st_polygon(list(rbind(c(-2.5,47.0), c(-2.2,47.0),
                            c(-2.2,47.3), c(-2.5,47.3), c(-2.5,47.0)))),
  sf::st_polygon(list(rbind(c(-2.2,47.0), c(-1.9,47.0),
                            c(-1.9,47.3), c(-2.2,47.3), c(-2.2,47.0)))),
  sf::st_polygon(list(rbind(c(-1.9,47.0), c(-1.6,47.0),
                            c(-1.6,47.3), c(-1.9,47.3), c(-1.9,47.0)))),
  sf::st_polygon(list(rbind(c(-2.5,47.3), c(-2.2,47.3),
                            c(-2.2,47.6), c(-2.5,47.6), c(-2.5,47.3)))),
  sf::st_polygon(list(rbind(c(-2.2,47.3), c(-1.9,47.3),
                            c(-1.9,47.6), c(-2.2,47.6), c(-2.2,47.3))))
)

watershed_sf <- sf::st_sf(
  basin_id   = paste0("BV", 1:5),
  basin_name = paste("Bassin", 1:5),
  geometry   = sf::st_sfc(watersheds, crs = 4326)
)

sf::st_write(watershed_sf,
             "inst/extdata/watersheds.geojson",
             delete_dsn = TRUE)

message("✅ Bassins versants créés : ", nrow(watershed_sf), " bassins")

# ============================================================
# 3. Rivières (lignes sf simulées)
# ============================================================

rivers <- list(
  sf::st_linestring(rbind(c(-2.5,47.15), c(-2.3,47.15),
                          c(-2.1,47.2),  c(-1.9,47.1), c(-1.6,47.1))),
  sf::st_linestring(rbind(c(-2.5,47.45), c(-2.3,47.5),
                          c(-2.1,47.45), c(-1.9,47.4)))
)

rivers_sf <- sf::st_sf(
  river_id   = paste0("RV", 1:2),
  river_name = c("Rivière principale", "Affluent nord"),
  geometry   = sf::st_sfc(rivers, crs = 4326)
)

sf::st_write(rivers_sf,
             "inst/extdata/rivers.geojson",
             delete_dsn = TRUE)

message("✅ Rivières créées : ", nrow(rivers_sf), " cours d'eau")

# ============================================================
# 4. Raster MNT simulé (altitude)
# ============================================================

target_resolution <- 100  # mètres

dem_raster <- terra::rast(
  nrows = 100, ncols = 100,
  xmin  = -2.5, xmax = -1.5,
  ymin  = 47.0, ymax = 48.0,
  crs   = "EPSG:4326"
)

terra::values(dem_raster) <- matrix(
  50 + 200 * (
    sin(seq(0, pi, length.out = 100)) %o%
      cos(seq(0, pi, length.out = 100))
  ) + rnorm(10000, 0, 10),
  ncol = 100
)
names(dem_raster) <- "altitude"

terra::writeRaster(dem_raster,
                   "inst/extdata/dem.tif",
                   overwrite = TRUE)

message("✅ MNT créé : 100x100 pixels, résolution cible = ",
        target_resolution, "m")

# ============================================================
# 5. Raster occupation du sol simulé
# ============================================================

landuse_raster <- terra::rast(
  nrows = 100, ncols = 100,
  xmin  = -2.5, xmax = -1.5,
  ymin  = 47.0, ymax = 48.0,
  crs   = "EPSG:4326"
)

terra::values(landuse_raster) <- sample(
  c(1, 2, 3, 4),
  10000,
  replace = TRUE,
  prob    = c(0.5, 0.3, 0.15, 0.05)
)
names(landuse_raster) <- "landuse"

terra::writeRaster(landuse_raster,
                   "inst/extdata/landuse.tif",
                   overwrite = TRUE)

message("✅ Occupation du sol créée : 100x100 pixels")

# ============================================================
# 6. Raster hydrologie simple simulé
# ============================================================

hydro_raster <- terra::rast(
  nrows = 100, ncols = 100,
  xmin  = -2.5, xmax = -1.5,
  ymin  = 47.0, ymax = 48.0,
  crs   = "EPSG:4326"
)

terra::values(hydro_raster) <- abs(rnorm(10000, mean = 500, sd = 200))
names(hydro_raster) <- "flow_accumulation"

terra::writeRaster(hydro_raster,
                   "inst/extdata/hydro.tif",
                   overwrite = TRUE)

message("✅ Raster hydrologique créé : 100x100 pixels")

# ============================================================
# 7. Paramètres utilisateur
# ============================================================

user_params <- list(
  study_basin       = "BV1",
  target_res_meters = target_resolution,
  crs               = 4326,
  test_size         = 0.2,
  ntrees            = 500
)

saveRDS(user_params, "inst/extdata/user_params.rds")

message("✅ Paramètres utilisateur sauvegardés")

# ============================================================
# 8. Sauvegarder comme objets R (.rda)
# ============================================================

usethis::use_data(water_quality, overwrite = TRUE)
usethis::use_data(watershed_sf,  overwrite = TRUE)
usethis::use_data(rivers_sf,     overwrite = TRUE)

message("✅ Objets R sauvegardés dans data/")
message("🎉 Terminé ! Toutes les données simulées ont été créées.")
