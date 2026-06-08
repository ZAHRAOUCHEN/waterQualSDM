#' Cartographie de la qualité du sol et de la vulnérabilité
#'
#' @param quality_raster SpatRaster de la variable qualité prédite (SOC en g/kg)
#' @param vuln_raster SpatRaster de vulnérabilité
#' @param watershed Objet sf des bassins versants
#' @param output_file Chemin pour exporter la carte PNG ou PDF (optionnel)
#' @return Une liste de 3 cartes ggplot2
#' @export
#' @examples
#' \dontrun{
#' cartes <- plot_water_quality_map(soc_map, vuln, watershed)
#' }
plot_water_quality_map <- function(quality_raster, vuln_raster, watershed, output_file = NULL) {

  # --- Carte 1 : SOC ---
  soc_df <- as.data.frame(quality_raster, xy = TRUE)
  names(soc_df) <- c("x", "y", "soc")
  soc_df <- soc_df[!is.na(soc_df$soc), ]

  p1 <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = soc_df,
      ggplot2::aes(x = x, y = y, fill = soc)
    ) +
    ggplot2::scale_fill_gradientn(
      colors = c("darkgreen", "yellow", "orange", "red"),
      name   = "SOC\n(g/kg)"
    ) +
    ggplot2::geom_sf(
      data      = watershed,
      fill      = NA,
      color     = "black",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "Soil Organic Carbon prédit (g/kg)",
      x     = "Longitude",
      y     = "Latitude"
    ) +
    ggplot2::theme_minimal()

  # --- Carte 2 : Vulnérabilité ---
  vuln_df <- as.data.frame(vuln_raster, xy = TRUE)
  names(vuln_df) <- c("x", "y", "vulnerability")
  vuln_df <- vuln_df[!is.na(vuln_df$vulnerability), ]
  vuln_df$vulnerability <- factor(
    as.numeric(vuln_df$vulnerability),
    levels = 1:3,
    labels = c("Faible", "Moyen", "Élevé")
  )

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
      data      = watershed,
      fill      = NA,
      color     = "black",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "Indice de vulnérabilité au Soil Organic Carbon",
      x     = "Longitude",
      y     = "Latitude"
    ) +
    ggplot2::theme_minimal()

  # --- Carte 3 : Bassins versants ---
  p3 <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = watershed,
      ggplot2::aes(fill = area_km2),
      color     = "black",
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
      ggplot2::ggsave(
        paste0(tools::file_path_sans_ext(output_file), "_soc.png"),
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
    carte_soc           = p1,
    carte_vulnerabilite = p2,
    carte_bassins       = p3
  ))
}


#' Génération automatique de recommandations de gestion du SOC
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

    risk <- summary_df$risk_class[i]
    agri <- summary_df$agriculture_pct[i]
    soc  <- summary_df$soc_mean_gkg[i]

    if (risk == "Élevé") {
      paste(
        "PRIORITÉ HAUTE - Sol très appauvri en matière organique :",
        "- Apporter du compost ou du fumier organique.",
        "- Pratiquer l'agriculture de conservation (zéro labour).",
        "- Planter des cultures de couverture hivernales.",
        "- Éviter le brûlage des résidus de culture.",
        "- Mettre en place des haies et bandes boisées.",
        sep = "\n"
      )
    } else if (risk == "Moyen") {
      paste(
        "PRIORITÉ MOYENNE - Sol modérément appauvri :",
        "- Maintenir les apports de matière organique.",
        "- Pratiquer la rotation des cultures.",
        "- Réduire le travail du sol au minimum.",
        "- Favoriser les légumineuses pour enrichir le sol.",
        sep = "\n"
      )
    } else {
      paste(
        "PRIORITÉ FAIBLE - Sol bien pourvu en matière organique :",
        "- Maintenir les bonnes pratiques agricoles actuelles.",
        "- Continuer le suivi régulier du SOC.",
        "- Préserver la végétation naturelle existante.",
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
title: "Rapport waterQualSDM - Qualité du sol en bassins versants agricoles"
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
    Donnée = c("Stations de mesure SOC", "Bassins versants",
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

## 3. Carte du Soil Organic Carbon prédit

```{r echo=FALSE, fig.width=10, fig.height=7}
print(cartes$carte_soc)
```

## 4. Carte de vulnérabilité au SOC

```{r echo=FALSE, fig.width=10, fig.height=7}
print(cartes$carte_vulnerabilite)
```

## 5. Recommandations de gestion

```{r echo=FALSE}
knitr::kable(
  summary_df[, c("soc_mean_gkg", "agriculture_pct",
                 "risk_class", "recommandations")],
  caption = "Recommandations par bassin versant"
)
```
'

tmp_rmd <- tempfile(fileext = ".Rmd")
writeLines(template, tmp_rmd)

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

#' Distribution du SOC par classe d'occupation du sol
#'
#' @param water_quality Dataframe des stations de mesure
#' @param soc_col Nom de la colonne SOC (défaut: "soc")
#' @param landuse_col Nom de la colonne occupation du sol (défaut: "landuse")
#' @return Un objet ggplot2
#' @export
#' @examples
#' \dontrun{
#' data(water_quality)
#' plot_soc_distribution(water_quality)
#' }
plot_soc_distribution <- function(water_quality,
                                  soc_col     = "soc",
                                  landuse_col = "landuse") {

  # Convertir landuse en labels
  water_quality$landuse_label <- factor(
    water_quality[[landuse_col]],
    levels = 1:4,
    labels = c("Agriculture", "Forêt", "Urbain", "Autre")
  )

  # Boxplot SOC par occupation du sol
  p <- ggplot2::ggplot(
    water_quality,
    ggplot2::aes(x = landuse_label,
                 y = .data[[soc_col]],
                 fill = landuse_label)
  ) +
    ggplot2::geom_boxplot(alpha = 0.7) +
    ggplot2::geom_jitter(width = 0.2, alpha = 0.5, size = 1.5) +
    ggplot2::scale_fill_manual(
      values = c(
        "Agriculture" = "orange",
        "Forêt"       = "darkgreen",
        "Urbain"      = "grey50",
        "Autre"       = "steelblue"
      )
    ) +
    ggplot2::labs(
      title    = "Distribution du SOC par classe d'occupation du sol",
      subtitle = "Maroc - SoilGrids 0-5cm",
      x        = "Occupation du sol",
      y        = "SOC (g/kg)",
      fill     = "Occupation du sol"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  return(p)
}
