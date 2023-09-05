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

# Analysis ----------------------------------------------------------------
# source(file = "./Scripts/LGBM_rounds_tune.R") # Compute optimal nrounds
gbm_data <- analysis_data %>% missRanger()
gbm_data$eih <- as.numeric(gbm_data$eih %>% as.factor()) - 1
gbm_data <- gbm_data %>%
  select(.data = ., -any_of(c("sujet", "subject", "sex", "eih_severity"))) %>%
  gbm_data_partition(sep_col = "eih",
                     sep_prop = lgbm_split)

## Light GBM analysis -----------------------------------------------------
lgbm_train_data <-
  split.default(x = gbm_data$train_data,
                f = names(gbm_data$train_data) == "eih") %>%
  `names<-`(value = c("values", "label"))
lgbm_dtrain <-
  lgb.Dataset(data = lgbm_train_data[["values"]] %>% as.matrix(),
              label = lgbm_train_data[["label"]] %>% as.matrix())

lgbm_test_data <-
  split.default(x = gbm_data$test_data,
                f = names(gbm_data$test_data) == "eih") %>%
  `names<-`(value = c("values", "label"))
lgbm_dtest <- lgb.Dataset.create.valid(
  dataset = lgbm_dtrain,
  data = lgbm_test_data[["values"]] %>% as.matrix(),
  label = lgbm_test_data[["label"]] %>% as.matrix()
)

### Configure -------------------------------------------------------------
if (new_LGBM_params) {
  source_python("./Scripts/Hyperparameters_tune.py")
  my_params <- study$best_params %>%
    lapply(FUN = gsub,
           pattern = ",",
           replacement = ".")
} else {
  my_config <- paste0("Params/Best_params", ".rds")
  my_params <- readRDS(file = my_config)
}

lgbm_params <- c(list(
  # Define parameters for lightGBM training
  objective = 'binary',
  boosting = "dart",
  metric = "binary_logloss"
),
my_params)
lgbm_valids <-
  list(test = lgbm_dtest) # Create a valid (reference) data set

### Train model -----------------------------------------------------------
lgbm_model <- lgb.train(
  # Train model
  params = lgbm_params,
  data = lgbm_dtrain,
  nrounds = lgbm_rounds,
  valids = lgbm_valids
)

### Test model ------------------------------------------------------------
lgbm_test_data_pred <- as.matrix(x = lgbm_test_data$values)
lgbm_pred <-
  predict(object = lgbm_model, lgbm_test_data_pred, reshape = TRUE)
lgbm_pred_y = ifelse(lgbm_pred > median(lgbm_pred), 1, 0)

lgbm_confusion <-
  confusionMatrix(as.factor(x = lgbm_test_data$label[["eih"]]),
                  as.factor(x = lgbm_pred_y),
                  mode = "everything")

### Plotting --------------------------------------------------------------
### Feature importance
lgbm_importance = lgb.importance(lgbm_model, percentage = TRUE)
lgbm_importance_plot <-
  lgb.plot.importance(lgbm_importance, measure = "Gain", top_n = 10)
lgbm_importance_plot_multi <-
  lgb.plot.interpretation(lgbm_importance_plot)
lgbm_plot <-
  lgbm_plots(lgbm_model = lgbm_model, lgbm_test_data_pred = lgbm_test_data_pred)

lgbm_model_results <-
  lst(
    lgbm_model,
    lgbm_confusion,
    lgbm_importance_plot,
    lgbm_importance_plot_multi,
    lgbm_plot
  )

# Data structure ----------------------------------------------------------
dir_create(path = paste0("Output/", analysis_date, "/params/"))
lgbm_export(study = study,
            lgbm_model_results = lgbm_model_results)

# Export data -------------------------------------------------------------
# Save environment to avoid recomputing
save.image(file = paste0("./Output/", analysis_date, "/predictive.RData"))
