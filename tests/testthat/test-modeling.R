# Tests pour les fonctions de modélisation
library(testthat)
devtools::load_all()

# Données de test simples
set.seed(42)
df_test <- data.frame(
  nitrates          = abs(rnorm(50, 25, 10)),
  landuse           = sample(1:4, 50, replace = TRUE),
  slope             = abs(rnorm(50, 5, 2)),
  flow_accumulation = abs(rnorm(50, 500, 100)),
  dist_river        = abs(rnorm(50, 1000, 300))
)

# ============================================================
# Tests preprocess_data()
# ============================================================

test_that("preprocess_data retourne une liste", {
  result <- preprocess_data(df_test, target = "nitrates")
  expect_type(result, "list")
})

test_that("preprocess_data retourne train et test", {
  result <- preprocess_data(df_test, target = "nitrates")
  expect_true("train" %in% names(result))
  expect_true("test" %in% names(result))
})

test_that("preprocess_data split est correct", {
  result  <- preprocess_data(df_test, target = "nitrates", test_size = 0.2)
  n_total <- nrow(result$train) + nrow(result$test)
  expect_equal(n_total, nrow(df_test))
})

test_that("preprocess_data retourne clean_data", {
  result <- preprocess_data(df_test, target = "nitrates")
  expect_true("clean_data" %in% names(result))
})

# ============================================================
# Tests train_rf_model()
# ============================================================

test_that("train_rf_model retourne une liste", {
  data_prep <- preprocess_data(df_test, target = "nitrates")
  result    <- train_rf_model(data_prep, target = "nitrates")
  expect_type(result, "list")
})

test_that("train_rf_model retourne un modèle ranger", {
  data_prep <- preprocess_data(df_test, target = "nitrates")
  result    <- train_rf_model(data_prep, target = "nitrates")
  expect_s3_class(result$model, "ranger")
})

test_that("train_rf_model retourne importance des variables", {
  data_prep <- preprocess_data(df_test, target = "nitrates")
  result    <- train_rf_model(data_prep, target = "nitrates")
  expect_true("importance" %in% names(result))
  expect_s3_class(result$importance, "data.frame")
})

test_that("train_rf_model retourne validation train/test", {
  data_prep <- preprocess_data(df_test, target = "nitrates")
  result    <- train_rf_model(data_prep, target = "nitrates")
  expect_true("validation" %in% names(result))
})

# ============================================================
# Tests evaluate_model()
# ============================================================

test_that("evaluate_model retourne les métriques", {
  data_prep    <- preprocess_data(df_test, target = "nitrates")
  model_result <- train_rf_model(data_prep, target = "nitrates")
  eval_result  <- evaluate_model(model_result, data_prep)
  expect_true("metrics" %in% names(eval_result))
})

test_that("evaluate_model retourne RMSE MAE R2", {
  data_prep    <- preprocess_data(df_test, target = "nitrates")
  model_result <- train_rf_model(data_prep, target = "nitrates")
  eval_result  <- evaluate_model(model_result, data_prep)
  expect_true("RMSE" %in% eval_result$metrics$Métrique)
  expect_true("MAE"  %in% eval_result$metrics$Métrique)
  expect_true("R²"   %in% eval_result$metrics$Métrique)
})

test_that("evaluate_model retourne les graphiques", {
  data_prep    <- preprocess_data(df_test, target = "nitrates")
  model_result <- train_rf_model(data_prep, target = "nitrates")
  eval_result  <- evaluate_model(model_result, data_prep)
  expect_true("plot_obs_pred"    %in% names(eval_result))
  expect_true("plot_residuals"   %in% names(eval_result))
  expect_true("plot_importance"  %in% names(eval_result))
})

test_that("evaluate_model RMSE est positif", {
  data_prep    <- preprocess_data(df_test, target = "nitrates")
  model_result <- train_rf_model(data_prep, target = "nitrates")
  eval_result  <- evaluate_model(model_result, data_prep)
  rmse <- eval_result$metrics$Valeur[eval_result$metrics$Métrique == "RMSE"]
  expect_true(rmse > 0)
})
