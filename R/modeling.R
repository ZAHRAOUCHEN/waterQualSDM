#' Entraînement du modèle Random Forest
#'
#' @param data_prep Liste issue de preprocess_data
#' @param target Nom de la colonne cible (défaut: "nitrates")
#' @param ntrees Nombre d'arbres (défaut: 500)
#' @param tune Logical, effectuer un tuning simple (défaut: TRUE)
#' @return Une liste avec le modèle et l'importance des variables
#' @export
#' @examples
#' \dontrun{
#' model <- train_rf_model(data_prep)
#' }
train_rf_model <- function(data_prep, target = "nitrates",
                           ntrees = 500, tune = TRUE) {

  train_data <- data_prep$train
  test_data  <- data_prep$test

  # Formule du modèle
  features <- names(train_data)[names(train_data) != target]
  formula  <- as.formula(paste(target, "~", paste(features, collapse = " + ")))

  # Tuning simple : tester différentes valeurs de mtry
  if (tune) {
    message("Tuning du modèle en cours...")

    mtry_values <- c(
      floor(sqrt(length(features))),
      floor(length(features) / 3),
      floor(length(features) / 2)
    )
    mtry_values <- unique(mtry_values)

    best_rmse <- Inf
    best_mtry <- mtry_values[1]

    for (mtry in mtry_values) {
      rf_tmp <- ranger::ranger(
        formula   = formula,
        data      = train_data,
        num.trees = 100,
        mtry      = mtry,
        seed      = 42
      )
      preds <- predict(rf_tmp, data = test_data)$predictions
      rmse  <- sqrt(mean((test_data[[target]] - preds)^2))

      if (rmse < best_rmse) {
        best_rmse <- rmse
        best_mtry <- mtry
      }
    }
    message("Meilleur mtry : ", best_mtry, " (RMSE = ", round(best_rmse, 3), ")")
  } else {
    best_mtry <- floor(sqrt(length(features)))
  }

  # Entraînement final avec le meilleur mtry
  message("Entraînement du Random Forest final...")
  rf_model <- ranger::ranger(
    formula    = formula,
    data       = train_data,
    num.trees  = ntrees,
    mtry       = best_mtry,
    importance = "impurity",
    seed       = 42
  )

  # Importance des variables
  importance_df <- data.frame(
    variable   = names(rf_model$variable.importance),
    importance = rf_model$variable.importance
  )
  importance_df <- importance_df[order(-importance_df$importance), ]

  # Validation train/test
  pred_train <- predict(rf_model, data = train_data)$predictions
  pred_test  <- predict(rf_model, data = test_data)$predictions

  validation <- data.frame(
    RMSE_train = round(sqrt(mean((train_data[[target]] - pred_train)^2)), 3),
    RMSE_test  = round(sqrt(mean((test_data[[target]] - pred_test)^2)), 3),
    R2_train   = round(1 - sum((train_data[[target]] - pred_train)^2) /
                         sum((train_data[[target]] - mean(train_data[[target]]))^2), 3),
    R2_test    = round(1 - sum((test_data[[target]] - pred_test)^2) /
                         sum((test_data[[target]] - mean(test_data[[target]]))^2), 3)
  )

  message("Modèle entraîné avec succès !")

  result <- list(
    model      = rf_model,
    importance = importance_df,
    features   = features,
    validation = validation,
    best_mtry  = best_mtry
  )

  return(result)
}




#' Évaluation des performances du modèle
#'
#' @param model_result Liste issue de train_rf_model
#' @param data_prep Liste issue de preprocess_data
#' @param target Nom de la colonne cible (défaut: "nitrates")
#' @return Une liste avec métriques et graphiques
#' @export
#' @examples
#' \dontrun{
#' eval <- evaluate_model(model_result, data_prep)
#' }
evaluate_model <- function(model_result, data_prep, target = "nitrates") {

  test_data   <- data_prep$test
  model       <- model_result$model

  # Prédictions sur le jeu de test
  predictions <- predict(model, data = test_data)$predictions
  observed    <- test_data[[target]]
  residuals   <- observed - predictions

  # Calcul des métriques
  rmse <- sqrt(mean(residuals^2))
  mae  <- mean(abs(residuals))
  r2   <- 1 - sum(residuals^2) / sum((observed - mean(observed))^2)

  metrics <- data.frame(
    Métrique = c("RMSE", "MAE", "R²"),
    Valeur   = round(c(rmse, mae, r2), 4)
  )

  # Dataframe pour les graphiques
  plot_df <- data.frame(
    observed    = observed,
    predicted   = predictions,
    residuals   = residuals
  )

  # Graphique 1 : Observed vs Predicted
  plot_obs_pred <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = observed, y = predicted)
  ) +
    ggplot2::geom_point(color = "steelblue", alpha = 0.7, size = 2) +
    ggplot2::geom_abline(
      slope     = 1,
      intercept = 0,
      color     = "red",
      linetype  = "dashed",
      linewidth = 1
    ) +
    ggplot2::labs(
      title    = "Observed vs Predicted - Nitrates",
      subtitle = paste0("R² = ", round(r2, 3), " | RMSE = ", round(rmse, 3)),
      x        = "Observé (mg/L)",
      y        = "Prédit (mg/L)"
    ) +
    ggplot2::theme_minimal()

  # Graphique 2 : Résidus vs Predicted
  plot_residuals <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = predicted, y = residuals)
  ) +
    ggplot2::geom_point(color = "darkorange", alpha = 0.7, size = 2) +
    ggplot2::geom_hline(
      yintercept = 0,
      color      = "red",
      linetype   = "dashed",
      linewidth  = 1
    ) +
    ggplot2::labs(
      title    = "Graphique des résidus",
      subtitle = "Les résidus doivent être centrés autour de 0",
      x        = "Valeurs prédites (mg/L)",
      y        = "Résidus (mg/L)"
    ) +
    ggplot2::theme_minimal()

  # Graphique 3 : Importance des variables
  plot_importance <- ggplot2::ggplot(
    model_result$importance,
    ggplot2::aes(x = reorder(variable, importance), y = importance)
  ) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Importance des variables",
      x     = "Variable",
      y     = "Importance"
    ) +
    ggplot2::theme_minimal()

  result <- list(
    metrics         = metrics,
    plot_obs_pred   = plot_obs_pred,
    plot_residuals  = plot_residuals,
    plot_importance = plot_importance
  )

  return(result)
}
