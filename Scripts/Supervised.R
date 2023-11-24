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

lgbm_rounds <<- params$lgbm_rounds

# Analysis ----------------------------------------------------------------
gbm_data <-
  analysis_data %>%
  select(.data = ., -any_of(c("subject", "sex", "V1", discriminating_variables)))
gbm_data$eih <-
  factor(gbm_data$eih) %>%
  as.numeric() - 1
gbm_data$eih_severity <-
  factor(gbm_data$eih_severity) %>%
  as.numeric() - 1
gbm_data_status <- gbm_data %>%
  select(.data = ., -any_of(c("eih_severity"))) %>%
  gbm_data_partition(sep_col = "eih",
                     sep_prop = params$lgbm_split)
gbm_data_intensity <- gbm_data %>%
  select(.data = ., -any_of(c("eih"))) %>%
  gbm_data_partition(sep_col = "eih_severity",
                     sep_prop = params$lgbm_split)

## Light GBM analysis -----------------------------------------------------
compute_env$lgbm_train_data_status <-
  split.default(x = gbm_data_status$train_data,
                f = names(gbm_data_status$train_data) == "eih") %>%
  `names<-`(value = c("values", "label"))
compute_env$lgbm_train_data_intensity <-
  split.default(x = gbm_data_intensity$train_data,
                f = names(gbm_data_intensity$train_data) == "eih_severity") %>%
  `names<-`(value = c("values", "label"))
lgbm_dtrain_status <-
  lgb.Dataset(data = lgbm_train_data_status[["values"]] %>% as.matrix(),
              label = lgbm_train_data_status[["label"]] %>% as.matrix())
lgbm_dtrain_intensity <-
  lgb.Dataset(data = lgbm_train_data_intensity[["values"]] %>% as.matrix(),
              label = lgbm_train_data_intensity[["label"]] %>% as.matrix())

lgbm_test_data_status <-
  split.default(x = gbm_data_status$test_data,
                f = names(gbm_data_status$test_data) == "eih") %>%
  `names<-`(value = c("values", "label"))
lgbm_test_data_intensity <-
  split.default(x = gbm_data_intensity$test_data,
                f = names(gbm_data_intensity$test_data) == "eih_severity") %>%
  `names<-`(value = c("values", "label"))

lgbm_dtest_status <- lgb.Dataset.create.valid(
  dataset = lgbm_dtrain_status,
  data = lgbm_test_data_status[["values"]] %>% as.matrix(),
  label = lgbm_test_data_status[["label"]] %>% as.matrix()
)
lgbm_dtest_intensity <- lgb.Dataset.create.valid(
  dataset = lgbm_dtrain_intensity,
  data = lgbm_test_data_intensity[["values"]] %>% as.matrix(),
  label = lgbm_test_data_intensity[["label"]] %>% as.matrix()
)

### Configure -------------------------------------------------------------
lgbm_train_data_status <<- compute_env$lgbm_train_data_status
lgbm_train_data_intensity <<- compute_env$lgbm_train_data_intensity

lgbm_test_data_status <<- compute_env$lgbm_test_data_status
lgbm_test_data_intensity <<- compute_env$lgbm_test_data_intensity

if (params$new_LGBM_params) {
  source_python("./Scripts/Hyperparameters_tune.py")
  my_params_status <- study$best_params %>%
    lapply(FUN = gsub,
           pattern = ",",
           replacement = ".")
  source_python("./Scripts/Hyperparameters_tune2.py")
  my_params_intensity <- study$best_params %>%
    lapply(FUN = gsub,
           pattern = ",",
           replacement = ".")
} else {
  my_config <- paste0("params/LightGBM_model", ".rds")
  my_params <- readRDS(file = my_config)
}

lgbm_params_status <- c(list(
  # Define parameters for lightGBM training
  objective = 'binary',
  boosting = "dart",
  metric = "binary_logloss"
),
my_params_status)
lgbm_params_intensity <- c(list(
  # Define parameters for lightGBM training
  objective = 'multiclass',
  boosting = "dart",
  metric = "multiclass"
),
my_params_intensity)
lgbm_valids_status <-
  list(test = lgbm_dtest_status) # Create a valid (reference) data set
lgbm_valids_intensity <-
  list(test = lgbm_dtest_intensity) # Create a valid (reference) data set

### Train model -----------------------------------------------------------
# source(file = "./Scripts/LGBM_rounds_tune.R") # Compute optimal nrounds
lgbm_model_status <- lgb.load("lgbm_model.txt")
lgbm_model_intensity <- lgb.load("lgbm_model2.txt")

### Test model ------------------------------------------------------------
lgbm_test_data_pred_status <- as.matrix(x = lgbm_test_data_status$values)
lgbm_test_data_pred_intensity <- as.matrix(x = lgbm_test_data_intensity$values)
lgbm_pred_y_status <- predictions_status
lgbm_pred_y_intensity <- predictions_intensity
lgbm_confusion_status <- conf_matrix_status
lgbm_confusion_intensity <- conf_matrix_intensity

### Plotting --------------------------------------------------------------
### Feature importance
lgbm_importance_status <- lgb.importance(lgbm_model_status, percentage = TRUE)
lgbm_importance_intensity <- lgb.importance(lgbm_model_intensity, percentage = TRUE)
lgbm_importance_plot_status <-
  lgb.plot.importance(lgbm_importance_status, measure = "Gain", top_n = 10)
lgbm_importance_plot_intensity <-
  lgb.plot.importance(lgbm_importance_intensity, measure = "Gain", top_n = 10)
lgbm_importance_plot_multi_status <-
  lgb.plot.interpretation(lgbm_importance_plot_status)
lgbm_importance_plot_multi_intensity <-
  lgb.plot.interpretation(lgbm_importance_plot_intensity)
lgbm_plot_status <-
  lgbm_plots(lgbm_model = lgbm_model_status, lgbm_test_data_pred = lgbm_test_data_pred_status)
lgbm_plot_intensity <-
  lgbm_plots(lgbm_model = lgbm_model_intensity, lgbm_test_data_pred = lgbm_test_data_pred_intensity)

lgbm_model_results_status <-
  lst(
    lgbm_model_status,
    lgbm_confusion_status,
    lgbm_importance_plot_status,
    lgbm_importance_plot_multi_status,
    lgbm_plot_status
  )

lgbm_model_results_intensity <-
  lst(
    lgbm_model_intensity,
    lgbm_confusion_intensity,
    lgbm_importance_plot_intensity,
    lgbm_importance_plot_multi_intensity,
    lgbm_plot_intensity
  )

# Data structure ----------------------------------------------------------
dir_create(path = paste0("Output/", analysis_date, "/params/"))
lgbm_export(#study = study,
  lgbm_model_results = lgbm_model_results_status)
lgbm_export(#study = study,
  lgbm_model_results = lgbm_model_results_intensity)

# Export data -------------------------------------------------------------
# Save environment to avoid recomputing
save.image(file = paste0("./Output/", analysis_date, "/supervised.RData"))
