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
require(fs)
require(xgboost)

# Data preparation --------------------------------------------------------
analysis_data$eih <- analysis_data$eih - 1
ml_data <-
  gbm_data_partition(input = analysis_data,
                     sep_col = "eih",
                     sep_prop = ml_split)
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
dl_train <<- ml_data$train_data
dl_test <<- ml_data$test_data

# Source python scripts
source_python("./Scripts/ML.py")
source_python("./Scripts/DL.py")

# Light GBM analysis ------------------------------------------------------
lgbm_model <- lgb.load("lgbm_model.txt")
lgbm_test_data_pred <- as.matrix(x = ml_test_data$values)

## Result metrics ---------------------------------------------------------
## Compute confusion matrix
lgbm_confusion <- confusionMatrix(
  (lgbm_pred_y + 1) %>% as.vector() %>%
    inverse.transform(enc = .GlobalEnv$convert_dic$eih) %>%
    as.factor(),
  (ml_test_data$label + 1)[[1]] %>% as.vector() %>%
    inverse.transform(enc = .GlobalEnv$convert_dic$eih) %>%
    as.factor()
)

## Plotting ---------------------------------------------------------------
## Feature importance
lgbm_shap_plot <-
  shap_plots(model = lgbm_model, test_data_pred = lgbm_test_data_pred)

lgbm_model_results <-
  lst(optuna_lgbm_best_accuracy,
      lgbm_model,
      lgbm_confusion,
      lgbm_shap_plot)

# XGBoost analysis --------------------------------------------------------
xgboost_model <- xgb.load("xgboost_model.txt")
xgboost_model$feature_names <- feature_names
xgboost_train <- data.matrix(select(analysis_data, -"eih"))
xgboost_test_data_pred <- as.matrix(x = ml_test_data$values)

## Result metrics ---------------------------------------------------------
## Compute confusion matrix
xgboost_confusion <- confusionMatrix(
  (xgboost_pred_y + 1) %>% as.vector() %>%
    inverse.transform(enc = convert_dic$eih) %>%
    as.factor(),
  (ml_test_data$label + 1)[[1]] %>% as.vector() %>%
    inverse.transform(enc = convert_dic$eih) %>%
    as.factor()
)

## Plotting ---------------------------------------------------------------
## Feature importance
xgboost_shap_plot <-
  shap_plots(model = xgboost_model,
             test_data_pred = xgboost_test_data_pred)

xgboost_model_results <-
  lst(optuna_xgb_best_accuracy,
      xgboost_model,
      xgboost_confusion,
      xgboost_shap_plot)

# Dense NN ----------------------------------------------------------------
# dense_confusion <- confusionMatrix(
#   factor((dense_pred_y + 1) %>% as.vector() %>%
#            inverse.transform(enc = .GlobalEnv$convert_dic$eih),
#          levels = c("EIH", "NEIH")
#   ),
#   factor((dl_test$eih + 1) %>% as.vector() %>%
#            inverse.transform(enc = .GlobalEnv$convert_dic$eih),
#          levels = c("EIH", "NEIH")
#   )
# )

# NODE NN -----------------------------------------------------------------
node_confusion <- confusionMatrix(
  factor((node_pred_y + 1) %>% as.vector() %>%
           inverse.transform(enc = .GlobalEnv$convert_dic$eih),
         levels = c("EIH", "NEIH")
  ),
  factor((dl_test$eih + 1) %>% as.vector() %>%
           inverse.transform(enc = .GlobalEnv$convert_dic$eih),
         levels = c("EIH", "NEIH")
  )
)

# Gandalf NN --------------------------------------------------------------
gandalf_confusion <- confusionMatrix(
  factor((gandalf_pred_y + 1) %>% as.vector() %>%
           inverse.transform(enc = .GlobalEnv$convert_dic$eih),
         levels = c("EIH", "NEIH")
  ),
  factor((dl_test$eih + 1) %>% as.vector() %>%
           inverse.transform(enc = .GlobalEnv$convert_dic$eih),
         levels = c("EIH", "NEIH")
  )
)

# Export data -------------------------------------------------------------
# Save environment to avoid recomputing
dir_create(path = paste0("Output/", analysis_date, "/params/"))
dir_create(path = paste0("Output/", analysis_date, "/models/"))
file_move(
  path = "lgbm_model.txt",
  new_path = paste0("Output/", analysis_date, "/models/lgbm_model.txt")
)
file_move(
  path = "xgboost_model.txt",
  new_path = paste0("Output/", analysis_date, "/models/xgboost_model.txt")
)
lgbm_export(lgbm_model_results = lgbm_model_results)
xgboost_export(xgboost_model_results = xgboost_model_results)
