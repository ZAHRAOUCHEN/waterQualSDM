#' Cartographie de la qualité de l'eau
#'
#' @param nitrates_raster SpatRaster des nitrates prédits
#' @param vuln_raster SpatRaster de vulnérabilité
#' @param watershed Objet sf des bassins versants
#' @param output_file Chemin pour exporter la carte PNG ou PDF (optionnel)
#' @return Une liste de 3 cartes ggplot2
#' @export
#' @examples
#' \dontrun{
#' cartes <- plot_water_quality_map(nitrates_map, vuln, watershed)
#' }
plot_water_quality_map <- function(nitrates_raster, vuln_raster, watershed, output_file = NULL) {

  # --- Carte 1 : Nitrates ---
  nitrates_df <- as.data.frame(nitrates_raster, xy = TRUE)
  names(nitrates_df) <- c("x", "y", "nitrates")
  nitrates_df <- nitrates_df[!is.na(nitrates_df$nitrates), ]

  p1 <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = nitrates_df,
      ggplot2::aes(x = x, y = y, fill = nitrates)
    ) +
    ggplot2::scale_fill_gradientn(
      colors = c("darkgreen", "yellow", "orange", "red"),
      name   = "Nitrates\n(mg/L)"
    ) +
    ggplot2::geom_sf(
      data = watershed,
      fill = NA,
      color = "black",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "Concentration en nitrates prédite",
      x     = "Longitude",
      y     = "Latitude"
    ) +
    ggplot2::theme_minimal()

  # --- Carte 2 : Vulnérabilité ---
  vuln_df <- as.data.frame(vuln_raster, xy = TRUE)
  names(vuln_df) <- c("x", "y", "vulnerability")
  vuln_df <- vuln_df[!is.na(vuln_df$vulnerability), ]
  vuln_df$vulnerability <- factor(vuln_df$vulnerability,
                                  levels = 1:3,
                                  labels = c("Faible", "Moyen", "Élevé"))

  p2 <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = vuln_df,
      ggplot2::aes(x = x, y = y, fill = vulnerability)
    ) +
    ggplot2::scale_fill_manual(
      values = c("Faible" = "green3", "Moyen" = "orange", "Élevé" = "red3"),
      name   = "Vulnérabilité"
    ) +
    ggplot2::geom_sf(
      data = watershed,
      fill = NA,
      color = "black",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "Indice de vulnérabilité aux nitrates",
      x     = "Longitude",
      y     = "Latitude"
    ) +
    ggplot2::theme_minimal()

  # --- Carte 3 : Bassins versants ---
  p3 <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = watershed,
      ggplot2::aes(fill = area_km2),
      color = "black",
      linewidth = 0.8
    ) +
    ggplot2::scale_fill_viridis_c(name = "Surface\n(km²)") +
    ggplot2::labs(
      title = "Bassins versants",
      x     = "Longitude",
      y     = "Latitude"
    ) +
    ggplot2::theme_minimal()

  # --- Export PNG ou PDF si demandé ---
  if (!is.null(output_file)) {
    ext <- tools::file_ext(output_file)

    if (ext == "pdf") {
      pdf(output_file, width = 12, height = 5)
      print(p1)
      print(p2)
      print(p3)
      dev.off()
    } else {
      # PNG par défaut
      ggplot2::ggsave(
        paste0(tools::file_path_sans_ext(output_file), "_nitrates.png"),
        plot = p1, width = 10, height = 8, dpi = 300
      )
      ggplot2::ggsave(
        paste0(tools::file_path_sans_ext(output_file), "_vulnerabilite.png"),
        plot = p2, width = 10, height = 8, dpi = 300
      )
      ggplot2::ggsave(
        paste0(tools::file_path_sans_ext(output_file), "_bassins.png"),
        plot = p3, width = 10, height = 8, dpi = 300
      )
    }
    message("Cartes exportées : ", output_file)
  }

  return(list(
    carte_nitrates      = p1,
    carte_vulnerabilite = p2,
    carte_bassins       = p3
  ))
}








#' Génération de recommandations de gestion
#'
#' @param summary_df Dataframe issu de summarize_watersheds
#' @return Un dataframe avec les recommandations par bassin
#' @export
#' @examples
#' \dontrun{
#' reco <- generate_recommendations(summary_df)
#' }
generate_recommendations <- function(summary_df) {

  recommandations <- sapply(1:nrow(summary_df), function(i) {

    risk  <- summary_df$risk_class[i]
    agri  <- summary_df$agriculture_pct[i]
    nitro <- summary_df$nitrates_mean_mgl[i]

    if (risk == "Élevé") {
      paste(
        "PRIORITÉ HAUTE :",
        "- Réduire immédiatement les apports en fertilisants azotés.",
        "- Mettre en place des bandes tampons riveraines (5-10m).",
        "- Installer une couverture végétale hivernale.",
        "- Contrôler les périodes d'épandage.",
        sep = "\n"
      )
    } else if (risk == "Moyen") {
      paste(
        "PRIORITÉ MOYENNE :",
        "- Optimiser les doses de fertilisation (agriculture de précision).",
        "- Favoriser les prairies en bordure de cours d'eau.",
        "- Surveiller régulièrement la qualité de l'eau.",
        sep = "\n"
      )
    } else {
      paste(
        "PRIORITÉ FAIBLE :",
        "- Maintenir les bonnes pratiques agricoles actuelles.",
        "- Continuer le suivi de la qualité de l'eau.",
        sep = "\n"
      )
    }
  })

  summary_df$recommandations <- recommandations
  return(summary_df)
}




#' Génération d'un rapport HTML ou PDF automatique
#'
#' @param eval_result Liste issue de evaluate_model
#' @param summary_df Dataframe issu de generate_recommendations
#' @param cartes Liste issue de plot_water_quality_map
#' @param data_info Liste avec infos sur les données utilisées
#' @param output_file Chemin du rapport (défaut: "rapport_waterQualSDM.html")
#' @return Le chemin du rapport généré
#' @export
#' @examples
#' \dontrun{
#' generate_report(eval_result, summary_df, cartes, data_info)
#' }
generate_report <- function(eval_result, summary_df, cartes, data_info,
                            output_file = "rapport_waterQualSDM.html") {

  template <- '
---
title: "Rapport waterQualSDM - Qualité de leau en bassins versants agricoles"
date: "`r Sys.Date()`"
output:
  html_document:
    toc: true
    toc_float: true
    theme: flatly
---

## 1. Données utilisées

```{r echo=FALSE}
knitr::kable(
  data.frame(
    Donnée = c("Stations qualité eau", "Bassins versants",
               "Occupation du sol", "MNT"),
    Description = c(
      paste0(data_info$n_stations, " stations de mesure"),
      paste0(data_info$n_watersheds, " bassins versants"),
      data_info$landuse_source,
      data_info$dem_source
    )
  ),
  caption = "Données utilisées dans lanalyse"
)
```

## 2. Performances du modèle Random Forest

```{r echo=FALSE}
knitr::kable(eval_result$metrics, caption = "Métriques de performance")
```

### Observed vs Predicted

```{r echo=FALSE, fig.width=8, fig.height=5}
print(eval_result$plot_obs_pred)
```

### Importance des variables

```{r echo=FALSE, fig.width=8, fig.height=5}
print(eval_result$plot_importance)
```

## 3. Carte de la qualité de leau (Nitrates)

```{r echo=FALSE, fig.width=10, fig.height=7}
print(cartes$carte_nitrates)
```

## 4. Carte de vulnérabilité

```{r echo=FALSE, fig.width=10, fig.height=7}
print(cartes$carte_vulnerabilite)
```

## 5. Recommandations de gestion

```{r echo=FALSE}
knitr::kable(
  summary_df[, c("nitrates_mean_mgl", "agriculture_pct",
                 "risk_class", "recommandations")],
  caption = "Recommandations par bassin versant"
)
```
'

# Écriture du template temporaire
tmp_rmd <- tempfile(fileext = ".Rmd")
writeLines(template, tmp_rmd)

# Rendu du rapport
rmarkdown::render(
  input       = tmp_rmd,
  output_file = output_file,
  envir       = list(
    eval_result = eval_result,
    summary_df  = summary_df,
    cartes      = cartes,
    data_info   = data_info
  )
)

message("Rapport généré : ", output_file)
return(output_file)
}
