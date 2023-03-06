# Informations ------------------------------------------------------------
# Title: lightGBM.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: GPLv3
# Description: Analyse the data to create predictive models

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
## List of used libraries.
require(tidyverse)
require(caret)
require(gbm)
require(lightgbm)
require(reticulate)
require(shapviz)
require(DiagrammeR)

# Environment -------------------------------------------------------------
# Define vectors used in entire script.
rm(list = setdiff(x = ls(), y = lsf.str())) # Clean environment
load(file = "./Environments/descriptive.RData") # Load environment
my_date <- format(Sys.time(), "%Y-%m-%d_%H.%M")
if (!dir.exists("./Output")) {
  dir.create("./Output")
}
if (!dir.exists("./Params")) {
  # Create directory to store results
  dir.create(path = paste0("./Output/Model_", my_date, "/"))
}

# Analysis ----------------------------------------------------------------
# source(file = "./Scripts/LGBM_rounds_tune.R") # Compute optimal nrounds
for (name_seq in names(my_data$summaries)) {
  gbm_data <-
    merge(
      x = select_if(.tbl = my_data$summaries[[name_seq]], .predicate = is.numeric),
      y = my_data$encoded_summaries$absolute[[2]]["eih"] - 1,
      by = "row.names"
    ) %>%
    select(.data = ., -any_of(
      c(
        "Row.names",
        "saturation_rest",
        "saturation_end",
        "saturation_delta"
      )
    )) %>%
    gbm_data_partition(sep_col = "eih",
                       sep_prop = 0.65)

  ## GBM analysis -----------------------------------------------------------
  ## Analysis using a classical GBM model
  gbm_model <- # Train a GBM model
    gbm(
      formula = eih ~ .,
      distribution = "bernoulli",
      data = gbm_data$train_data,
      n.trees = 3500,
      interaction.depth = 2,
      n.minobsinnode = 3,
      shrinkage = 0.01,
      bag.fraction = 0.9,
      cv.folds = 2,
      class.stratify.cv = NULL,
    )
  gbm_result <- # Compute results
    gbm_pred(
      input_model = gbm_model,
      input_data = gbm_data,
      predicted_var = "eih",
      threshold = 0.5
    )

  gbm_model_results <-
    lst(gbm_model, gbm_result)

  ## GBM caret analysis -----------------------------------------------------
  param_grid <- expand.grid(
    # Define parameter grid
    n.trees = seq(100, 1000, by = 50),
    interaction.depth = seq(1, 7, by = 1),
    n.minobsinnode = 3,
    shrinkage = c(0.001, 0.01, 0.1)
  )
  gbm_model <- train(
    # Train GBM model with grid parameters
    as.factor(eih) ~ .,
    data = gbm_data$train_data,
    method = "gbm",
    distribution = "bernoulli",
    tuneGrid = param_grid,
    trControl = trainControl(
      method = "cv",
      number = 2,
      returnResamp = "all"
    ),
    na.action = na.pass
  )
  gbm_result <- # Compute GBM results
    gbm_pred(
      input_model = gbm_model,
      input_data = gbm_data,
      predicted_var = "eih"
    )

  gbm_caret_model_results <-
    lst(gbm_model, gbm_result)

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
  if (dir.exists("./Params")) {
    my_config <- paste0("Params/Best_params_", name_seq, ".rds")
    my_params <- readRDS(file = my_config)
  } else {
    source_python("./Scripts/Optuna_tune.py")
    my_params <- study$best_params %>%
      lapply(FUN = gsub,
             pattern = ",",
             replacement = ".")
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
  optimal_rounds <-
    lgbm_round_tune(
      my_rounds = seq(300, 1500, by = 100),
      lgbm_params = lgbm_params,
      lgbm_dtrain = lgbm_dtrain,
      lgbm_valids = lgbm_valids,
      lgbm_test_data = lgbm_test_data$values,
      lgbm_test_data_label = lgbm_test_data$label
    )

  ### Train model -----------------------------------------------------------
  lgbm_model <- lgb.train(
    # Train model
    params = lgbm_params,
    data = lgbm_dtrain,
    nrounds = optimal_rounds$best_round,
    valids = lgbm_valids
  )

  ### Test model ------------------------------------------------------------
  lgbm_test_data_pred <- as.matrix(x = lgbm_test_data$values)
  lgbm_pred <-
    predict(object = lgbm_model, lgbm_test_data_pred, reshape = TRUE)
  lgbm_pred_y = ifelse(lgbm_pred > 0.5, 1, 0)

  lgbm_confusion <-
    confusionMatrix(as.factor(x = lgbm_test_data$label[["eih"]]),
                    as.factor(x = lgbm_pred_y))

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
      lgbm_plot,
      optimal_rounds
    )

  # Data structure ----------------------------------------------------------
  if (!dir.exists("./Params")) {
    lgbm_export(
      study = study,
      my_date = my_date,
      name_seq = name_seq,
      lgbm_model_results = lgbm_model_results
    )
  }

  assign(
    paste0("my_models_", name_seq),
    sapply(
      # Put results in a list
      X = ls(pattern = ".*model_results"),
      FUN = get,
      simplify = FALSE,
      USE.NAMES = TRUE
    )
  )
}

my_results <- append(x = my_results,
                     values = sapply(
                       X = ls(pattern = "my_models.*"),
                       FUN = get,
                       simplify = FALSE,
                       USE.NAMES = TRUE
                     ))

# Remove temporary variables
rm(list = setdiff(x = ls(), y = c(
  lsf.str(), ls(pattern = "my_data|my_results")
)))

# Export data -------------------------------------------------------------
# Save environment to avoid recomputing
save.image(file = "./Environments/predictive.RData")
