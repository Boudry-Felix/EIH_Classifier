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
  analysis_data
gbm_data$eih <-
  factor(gbm_data$eih) %>%
  as.numeric() - 1
gbm_data_status <- gbm_data %>%
  # select(.data = ., -any_of(c("eih_severity"))) %>%
  gbm_data_partition(sep_col = "eih",
                     sep_prop = lgbm_split)

## Light GBM analysis -----------------------------------------------------
compute_env$lgbm_train_data_status <-
  split.default(x = gbm_data_status$train_data,
                f = names(gbm_data_status$train_data) == "eih") %>%
  `names<-`(value = c("values", "label"))
lgbm_dtrain_status <-
  lgb.Dataset(data = lgbm_train_data_status[["values"]] %>% as.matrix(),
              label = lgbm_train_data_status[["label"]] %>% as.matrix())

lgbm_test_data_status <-
  split.default(x = gbm_data_status$test_data,
                f = names(gbm_data_status$test_data) == "eih") %>%
  `names<-`(value = c("values", "label"))

lgbm_dtest_status <- lgb.Dataset.create.valid(
  dataset = lgbm_dtrain_status,
  data = lgbm_test_data_status[["values"]] %>% as.matrix(),
  label = lgbm_test_data_status[["label"]] %>% as.matrix()
)

### Configure -------------------------------------------------------------
lgbm_train_data_status <<- compute_env$lgbm_train_data_status

lgbm_test_data_status <<- compute_env$lgbm_test_data_status

if (params$new_LGBM_params) {
  source_python("./Scripts/Hyperparameters_tune.py")
  my_params_status <- study$best_params %>%
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
lgbm_valids_status <-
  list(test = lgbm_dtest_status) # Create a valid (reference) data set

### Train model -----------------------------------------------------------
# source(file = "./Scripts/LGBM_rounds_tune.R") # Compute optimal nrounds
lgbm_model_status <- lgb.load("lgbm_model.txt")

### Test model ------------------------------------------------------------
lgbm_test_data_pred_status <- as.matrix(x = lgbm_test_data_status$values)
lgbm_pred_y_status <- predictions_status
lgbm_confusion_status <- conf_matrix_status

### Plotting --------------------------------------------------------------
### Feature importance
lgbm_importance_status <- lgb.importance(lgbm_model_status, percentage = TRUE)
lgbm_importance_plot_status <-
  lgb.plot.importance(lgbm_importance_status, measure = "Gain", top_n = 10)
lgbm_importance_plot_multi_status <-
  lgb.plot.interpretation(lgbm_importance_plot_status)
lgbm_plot_status <-
  lgbm_plots(lgbm_model = lgbm_model_status, lgbm_test_data_pred = lgbm_test_data_pred_status)

lgbm_model_results_status <-
  lst(
    lgbm_model_status,
    lgbm_confusion_status,
    lgbm_importance_plot_status,
    lgbm_importance_plot_multi_status,
    lgbm_plot_status
  )

# Data structure ----------------------------------------------------------
dir_create(path = paste0("Output/", analysis_date, "/params/"))
lgbm_export(#study = study,
  lgbm_model_results = lgbm_model_results_status)

# Export data -------------------------------------------------------------
# Save environment to avoid recomputing
save.image(file = paste0("./Output/", analysis_date, "/supervised.RData"))
