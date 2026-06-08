
# waterQualSDM

## Modélisation de la qualité de l’eau en bassins versants agricoles

`waterQualSDM` est un package R qui permet d’analyser et modéliser la
qualité du sol dans les bassins versants agricoles au Maroc, avec un
focus sur le **Soil Organic Carbon (SOC)** comme indicateur de qualité
environnementale.

------------------------------------------------------------------------

## Fonctionnalités principales

- **Import** des données qualité eau, occupation du sol, MNT et bassins
  versants
- **Calcul** des variables environnementales spatiales
- **Modélisation** par Random Forest pour prédire le Soil Organic Carbon
  (SOC)
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
features_spatial <- features[, c("soc", "landuse", "slope", 
                                  "altitude", "precipitation")]
features_spatial <- features_spatial[, !duplicated(names(features_spatial))]]

# 3. Prétraitement
data_prep <- preprocess_data(features_spatial, target = "soc")

# 4. Modélisation Random Forest
model_result <- train_rf_model(data_prep, ntrees = 500, tune = TRUE)

# 5. Évaluation
eval_result <- evaluate_model(model_result, data_prep)
print(eval_result$metrics)

# 6. Prédiction spatiale du SOC
soc_map <- predict_water_quality(model_result, landuse, hydro$raster)

# 7. Indice de vulnérabilité
vuln_map <- calculate_vulnerability_index(soc_map, landuse, hydro$raster)

# 8. Résumé par bassin
summary_df <- summarize_watersheds(watershed, soc_map, vuln_map, landuse)

# 9. Recommandations
recommendations <- generate_recommendations(summary_df)

# 10. Cartographie
cartes <- plot_water_quality_map(soc_map, vuln_map, watershed)
print(cartes$carte_soc)
print(cartes$carte_vulnerabilite)
print(cartes$carte_bassins)
```

> > **Données utilisées** : Le package utilise de vraies données
> > environnementales du Maroc téléchargées depuis des sources
> > officielles : - **Variable cible** : Soil Organic Carbon (SOC)
> > depuis SoilGrids (0-5cm, g/kg) - **Occupation du sol** : ESA World
> > Cover (arbres, cultures, urbain) - **Topographie** : SRTM 30s
> > (altitude et pente) - **Précipitations** : WorldClim 2.5min
> > (précipitations annuelles) - **Zone d’étude** : Maroc incluant le
> > Sahara Occidental (source : GADM) - **Stations de mesure** : 200
> > points d’échantillonnage - **Performances du modèle** : R² = 0.82,
> > RMSE = 5.66 g/kg

## Exemples de résultats

### Carte du Soil Organic Carbon prédit (g/kg)

<figure>
<img
src="https://raw.githubusercontent.com/ZAHRAOUCHEN/waterQualSDM/master/man/figures/carte_soc.png"
alt="Carte SOC" />
<figcaption aria-hidden="true">Carte SOC</figcaption>
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

![Observed vs
Predicted](https://raw.githubusercontent.com/ZAHRAOUCHEN/waterQualSDM/master/man/figures/obs_vs_pred.png)
\### Distribution du SOC par occupation du sol

<figure>
<img
src="https://raw.githubusercontent.com/ZAHRAOUCHEN/waterQualSDM/master/man/figures/soc_distribution.png"
alt="Distribution SOC" />
<figcaption aria-hidden="true">Distribution SOC</figcaption>
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
| `predict_water_quality()` | Prédiction spatiale de la qualité du sol (SOC) |
| `calculate_vulnerability_index()` | Calcul de l’indice de vulnérabilité |
| `summarize_watersheds()` | Résumé statistique par bassin |
| `generate_recommendations()` | Recommandations de gestion |
| `plot_water_quality_map()` | Cartographie qualité eau et vulnérabilité |
| `generate_report()` | Rapport HTML/PDF automatique |
| `plot_soc_distribution()` | Distribution du SOC par occupation du sol |

------------------------------------------------------------------------

## Structure du package

    waterQualSDM/
    ├── R/                        # Fonctions du package
    │   ├── import_data.R
    │   ├── environmental_features.R
    │   ├── modeling.R
    │   ├── prediction.R
    │   └── visualization.R
    ├── data/                     # Données réelles du Maroc (.rda)
    ├── data-raw/                 # Script de génération des données
    ├── inst/extdata/             # Fichiers exemples
    ├── tests/testthat/           # Tests unitaires
    ├── vignettes/                # Vignette reproductible
    ├── man/                      # Documentation
    ├── DESCRIPTION
    └── README.md

------------------------------------------------------------------------

## Données d’exemple

Le package inclut des données environnementales réelles du Maroc :

``` r
library(waterQualSDM)
# Charger les données réelles du Maroc
data(water_quality)   # 200 stations SOC - SoilGrids
data(watershed_sf)    # Régions administratives du Maroc - GADM
data(rivers_sf)       # Cours d'eau simulés
# Aperçu des stations de mesure SOC
head(water_quality)
```

------------------------------------------------------------------------

## Auteur

- **ZAHRAOUCHEN**
- Email : <ouchenzahra2005@gmail.com>

------------------------------------------------------------------------

## Licence

MIT
