
# waterQualSDM

## Modélisation de la qualité de l’eau en bassins versants agricoles

`waterQualSDM` est un package R qui permet d’analyser et modéliser la
qualité de l’eau dans les bassins versants agricoles, avec un focus sur
les **concentrations en nitrates**.

------------------------------------------------------------------------

## Fonctionnalités principales

- **Import** des données qualité eau, occupation du sol, MNT et bassins
  versants
- **Calcul** des variables environnementales spatiales
- **Modélisation** par Random Forest pour prédire les nitrates
- ️ **Cartographie** des zones vulnérables à la pollution
- **Recommandations** automatiques de gestion durable

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

# 1. Import des données d'exemple
file_wq  <- system.file("extdata", "water_quality.csv", 
                         package = "waterQualSDM")
file_ws  <- system.file("extdata", "watersheds.geojson", 
                         package = "waterQualSDM")
file_dem <- system.file("extdata", "dem.tif", 
                         package = "waterQualSDM")
file_lu  <- system.file("extdata", "landuse.tif", 
                         package = "waterQualSDM")
file_rv  <- system.file("extdata", "rivers.geojson", 
                         package = "waterQualSDM")

wq_data  <- import_water_quality_data(file_wq)
watershed <- import_watershed(file_ws)
dem       <- load_dem(file_dem)
landuse   <- import_landuse_data(file_lu)
rivers    <- sf::st_read(file_rv, quiet = TRUE)

# 2. Variables environnementales
hydro    <- calculate_hydrological_variables(dem, rivers, watershed)
features <- extract_environmental_features(wq_data$sf, landuse, hydro)

# Exclure les variables non spatiales
features_spatial <- features[, !names(features) %in% 
                               c("phosphates", "station_id", "date")]

# 3. Prétraitement
data_prep <- preprocess_data(features_spatial, target = "nitrates")

# 4. Modélisation Random Forest
model_result <- train_rf_model(data_prep, ntrees = 500, tune = TRUE)

# 5. Évaluation
eval_result <- evaluate_model(model_result, data_prep)
print(eval_result$metrics)

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

## Exemples de résultats

### Carte des concentrations en nitrates

<figure>
<img
src="https://raw.githubusercontent.com/ZAHRAOUCHEN/waterQualSDM/master/man/figures/carte_nitrates.png"
alt="Carte des nitrates" />
<figcaption aria-hidden="true">Carte des nitrates</figcaption>
</figure>

### Carte de vulnérabilité

<figure>
<img
src="https://raw.githubusercontent.com/ZAHRAOUCHEN/waterQualSDM/master/man/figures/carte_vulnerabilite.png"
alt="Carte de vulnérabilité" />
<figcaption aria-hidden="true">Carte de vulnérabilité</figcaption>
</figure>

### Carte des bassins versants

<figure>
<img
src="https://raw.githubusercontent.com/ZAHRAOUCHEN/waterQualSDM/master/man/figures/carte_bassins.png"
alt="Carte des bassins versants" />
<figcaption aria-hidden="true">Carte des bassins versants</figcaption>
</figure>

### Évaluation du modèle — Observed vs Predicted

<figure>
<img
src="https://raw.githubusercontent.com/ZAHRAOUCHEN/waterQualSDM/master/man/figures/obs_vs_pred.png"
alt="Observed vs Predicted" />
<figcaption aria-hidden="true">Observed vs Predicted</figcaption>
</figure>

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
```

------------------------------------------------------------------------

## Auteur

- **ZAHRAOUCHEN**
- Email : <ouchenzahra2005@gmail.com>

------------------------------------------------------------------------

## Licence

MIT
