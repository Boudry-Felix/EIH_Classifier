# Informations ------------------------------------------------------------
# Title: Supervised.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: GPLv3
# Description: Analyze data using supervised algorithms.

# Libraries ---------------------------------------------------------------
# List of used libraries.
require(tidyverse)
require(caret)
require(gbm)
require(lightgbm)
require(reticulate)
require(shapviz)
require(DiagrammeR)
require(missRanger)
require(fs)
require(xgboost)

# Data preparation --------------------------------------------------------
analysis_data$eih <- analysis_data$eih - 1
ml_data <-
  gbm_data_partition(input = analysis_data,
                     sep_col = "eih",
                     sep_prop = ml_split)
ml_train <<- ml_data$train_data
ml_test <<- ml_data$test_data

ml_train_data <-
  split.default(x = ml_data$train_data,
                f = names(ml_data$train_data) == "eih") %>%
  `names<-`(value = c("values", "label"))

ml_test_data <-
  split.default(x = ml_data$test_data,
                f = names(ml_data$test_data) == "eih") %>%
  `names<-`(value = c("values", "label"))

# Needed to use variables with reticulate
ml_train_data <<- ml_train_data
ml_test_data <<- ml_test_data

source_python("./Scripts/ML.py")
source_python("./Scripts/DL.py")

# Light GBM analysis ------------------------------------------------------
lgbm_model <- lgb.load("lgbm_model.txt")
lgbm_test_data_pred <- as.matrix(x = ml_test_data$values)

## Plotting ---------------------------------------------------------------
## Feature importance
lgbm_importance <-
  lgb.importance(model = lgbm_model, percentage = TRUE)
lgbm_importance_plot <-
  lgb.plot.importance(tree_imp = lgbm_importance,
                      measure = "Gain",
                      top_n = 10)
lgbm_importance_plot_multi <-
  lgb.plot.interpretation(tree_interpretation_dt = lgbm_importance_plot)
lgbm_plot <-
  lgbm_plots(lgbm_model = lgbm_model, lgbm_test_data_pred = lgbm_test_data_pred)

lgbm_model_results <-
  lst(
    lgbm_model,
    lgbm_confusion,
    lgbm_accuracy,
    lgbm_best_accuracy,
    lgbm_kappa,
    lgbm_f1,
    lgbm_importance_plot,
    lgbm_importance_plot_multi,
    lgbm_plot
  )

# XGBoost analysis --------------------------------------------------------
xgboost_model <- xgb.load("xgboost_model.txt")
xgboost_model$feature_names <- feature_names
xgboost_train <- data.matrix(select(analysis_data, -"eih"))
xgboost_test_data_pred <- as.matrix(x = ml_test_data$values)

## Plotting ---------------------------------------------------------------
## Feature importance
xgboost_importance <-
  xgb.importance(model = xgboost_model, feature_names = compute_env$feature_names)
xgboost_importance_plot <-
  xgb.ggplot.importance(xgboost_importance, top_n = 10)
xgboost_summary_plot <-
  xgb.ggplot.shap.summary(xgboost_train, model = xgboost_model)

xgboost_model_results <-
  lst(xgboost_model,
      xgboost_confusion,
      xgboost_accuracy,
      xgboost_best_accuracy,
      xgboost_kappa,
      xgboost_f1,
      xgboost_importance_plot,
      xgboost_summary_plot)

# Export data -------------------------------------------------------------
# Save environment to avoid recomputing
dir_create(path = paste0("Output/", analysis_date, "/params/"))
dir_create(path = paste0("Output/", analysis_date, "/models/"))
file_move(path = "lgbm_model.txt", new_path = paste0("Output/", analysis_date, "/models/lgbm_model.txt"))
file_move(path = "xgboost_model.txt", new_path = paste0("Output/", analysis_date, "/models/xgboost_model.txt"))
# lgbm_export(lgbm_model_results = lgbm_model_results)
xgboost_export(xgboost_model_results = xgboost_model_results)
save.image(file = paste0("./Output/", analysis_date, "/supervised.RData"))
