
# waterQualSDM

## Modélisation de la qualité de l’eau en bassins versants agricoles

`waterQualSDM` est un package R qui permet d’analyser et modéliser la
qualité de l’eau dans les bassins versants agricoles, avec un focus sur
les **concentrations en nitrates**.

------------------------------------------------------------------------

## Fonctionnalités principales

-  **Import** des données qualité eau, occupation du sol, MNT et
  bassins versants
-  **Calcul** des variables environnementales spatiales
-  **Modélisation** par Random Forest pour prédire les nitrates
-  **Cartographie** des zones vulnérables à la pollution
-  **Recommandations** automatiques de gestion durable

------------------------------------------------------------------------

## Installation

``` r
# Installer depuis GitHub
devtools::install_github("ZAHRAOUCHEN/waterQualSDM")
```

------------------------------------------------------------------------

## Pipeline complet

``` r
library(waterQualSDM)
library(sf)
library(terra)

# 1. Import des données
wq_data  <- import_water_quality_data("stations.csv")
watershed <- import_watershed("bassins.geojson")
dem       <- load_dem("mnt.tif")
landuse   <- import_landuse_data("landuse.tif")

# 2. Variables environnementales
hydro    <- calculate_hydrological_variables(dem, rivers, watershed)
features <- extract_environmental_features(wq_data$sf, landuse, hydro)

# 3. Prétraitement
data_prep <- preprocess_data(features, target = "nitrates")

# 4. Modélisation Random Forest
model_result <- train_rf_model(data_prep)

# 5. Évaluation
eval_result <- evaluate_model(model_result, data_prep)

# 6. Prédiction spatiale
nitrates_map <- predict_water_quality(model_result, landuse, hydro$raster)

# 7. Indice de vulnérabilité
vuln_map <- calculate_vulnerability_index(nitrates_map, landuse, hydro$raster)

# 8. Résumé par bassin
summary_df <- summarize_watersheds(watershed, nitrates_map, vuln_map, landuse)

# 9. Recommandations
recommendations <- generate_recommendations(summary_df)

# 10. Cartographie
cartes <- plot_water_quality_map(nitrates_map, vuln_map, watershed)
print(cartes$carte_nitrates)
print(cartes$carte_vulnerabilite)
print(cartes$carte_bassins)
```

------------------------------------------------------------------------

## Fonctions du package

| Fonction | Description |
|----|----|
| `import_water_quality_data()` | Import des données qualité eau (CSV/Excel) |
| `import_landuse_data()` | Import de l’occupation du sol (raster) |
| `import_watershed()` | Import des bassins versants (shapefile/GeoJSON) |
| `load_dem()` | Import du Modèle Numérique de Terrain |
| `calculate_hydrological_variables()` | Calcul des variables hydrologiques |
| `extract_environmental_features()` | Extraction des variables aux stations |
| `preprocess_data()` | Prétraitement et split train/test |
| `train_rf_model()` | Entraînement du Random Forest |
| `evaluate_model()` | Évaluation RMSE, MAE, R² |
| `predict_water_quality()` | Prédiction spatiale des nitrates |
| `calculate_vulnerability_index()` | Calcul de l’indice de vulnérabilité |
| `summarize_watersheds()` | Résumé statistique par bassin |
| `generate_recommendations()` | Recommandations de gestion |
| `plot_water_quality_map()` | Cartographie qualité eau et vulnérabilité |
| `generate_report()` | Rapport HTML/PDF automatique |

------------------------------------------------------------------------

## Structure du package

    waterQualSDM/
    ├── R/                        # Fonctions du package
    │   ├── import_data.R
    │   ├── environmental_features.R
    │   ├── modeling.R
    │   ├── prediction.R
    │   └── visualization.R
    ├── data/                     # Données simulées (.rda)
    ├── data-raw/                 # Script de génération des données
    ├── inst/extdata/             # Fichiers exemples
    ├── tests/testthat/           # Tests unitaires
    ├── vignettes/                # Vignette reproductible
    ├── man/                      # Documentation
    ├── DESCRIPTION
    └── README.md

------------------------------------------------------------------------

## Données d’exemple

Le package inclut des données simulées pour tester toutes les fonctions
:

``` r
library(waterQualSDM)

# Charger les données d'exemple
data(water_quality)
data(watershed_sf)
data(rivers_sf)

# Aperçu des stations de mesure
head(water_quality)
#>   station_id longitude latitude       date nitrates phosphates
#> 1      ST001 -1.585194 47.33343 2020-03-22    22.83      0.477
#> 2      ST002 -1.562925 47.34675 2022-07-02    23.17      0.740
#> 3      ST003 -2.213860 47.39849 2020-11-20    34.33      0.406
#> 4      ST004 -1.669552 47.78469 2023-10-15    33.22      0.490
#> 5      ST005 -1.858254 47.03894 2022-08-08    38.92      0.483
#> 6      ST006 -1.980904 47.74880 2020-10-22    20.24      0.322
```

------------------------------------------------------------------------

## Auteur

- **ZAHRAOUCHEN**
- Email : <zahra.ouchen@iav.ac.com>

------------------------------------------------------------------------

## Licence

MIT
