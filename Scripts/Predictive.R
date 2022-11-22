# Informations ------------------------------------------------------------

# Title: lightGBM.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Analyse the data to create predictive models

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
## List of used libraries.
require(dplyr)
require(janitor)
require(caret)
require(gbm)
require(lightgbm)
require(reticulate)

# Environment -------------------------------------------------------------
# Define vectors used in entire script.
rm(list = ls()) # Clean environment
load(file = "./Environments/descriptive.RData") # Load environment
my_date <- format(Sys.time(), "%d-%m-%Y_%H.%M")
dir.create(path = paste0("./Output/Model_", my_date, "/")) # Create directory to store results

# Select data ------------------------------------------------------------
analysis_data <- # Put all data to analyze in a list
  lst(my_data$summary, my_data$summary_relative, my_data$PCA_summary) %>%
  `names<-`(c("absolute", "relative", "PCA"))

# Analysis ----------------------------------------------------------------
my_counter <- 1
for (my_analysis_data in analysis_data) {
  # Setting general variables
  my_analysis_data <-
    merge(x = my_analysis_data,
          y = my_data$labels[c("eih", "subject")],
          by = "subject") %>%
    select(.data = ., -c("subject"))

  ## GBM analysis -----------------------------------------------------------
  ## Analysis using a classical GBM model

  ### Shape data ------------------------------------------------------------
  gbm_split_indexes <- # Separate data in two using p
    createDataPartition(y = my_analysis_data$eih, p = 0.65, list = FALSE)
  gbm_train_data <- # Create a train data set
    my_analysis_data[gbm_split_indexes, ]
  gbm_test_data <- # Create a test data set
    my_analysis_data[-gbm_split_indexes, ]

  gbm_train_data$eih <- # Factorize EIH column
    as.numeric(x = as.factor(x = gbm_train_data$eih))

  ### Train model -----------------------------------------------------------
  gbm_model <- # Train a GBM model
    gbm(
      formula = eih ~ .,
      distribution = "tdist",
      data = gbm_train_data,
      var.monotone = NULL,
      n.trees = 140,
      interaction.depth = 2,
      n.minobsinnode = 3,
      shrinkage = 0.01,
      bag.fraction = 0.9,
      train.fraction = 1,
      cv.folds = 2,
      keep.data = TRUE,
      verbose = TRUE,
      class.stratify.cv = NULL,
      n.cores = NULL
    )

  gbm_ntree_optimal_oob <-
    gbm.perf(object = gbm_model, method = "OOB") # Compute optimal tree number
  gbm_ntree_optimal_cv <-
    gbm.perf(object = gbm_model, method = "cv") # Compute optimal tree number

  print(x = gbm_ntree_optimal_oob)
  print(x = gbm_ntree_optimal_cv)

  print(x = gbm_model)
  gbm_importance = summary(object = gbm_model, las = 1)
  gbm_importance_plot = summary.gbm(gbm_model, plotit = TRUE) %>% recordPlot()

  ### Test model ------------------------------------------------------------
  gbm_prediction <-
    predict(
      # Test model by making prediction on test data set
      object = gbm_model,
      newdata = gbm_test_data,
      n.trees = gbm_ntree_optimal_oob,
      type = "response"
    )

  gbm_prediction_binaries <-
    as.factor(x = ifelse(
      test = gbm_prediction > 1.7,
      yes = 2,
      no = 1
    ))
  gbm_eih_binaries <-
    as.factor(x = as.numeric(x = as.factor(x = gbm_test_data$eih)))
  gbm_confusion <-
    confusionMatrix(gbm_prediction_binaries, gbm_eih_binaries)

  gbm_model_results <- sapply(
    # Put results in a list
    X = ls(pattern = ".*gbm.*"),
    FUN = get,
    simplify = FALSE,
    USE.NAMES = TRUE
  )

  ## GBM (caret) analysis ---------------------------------------------------

  ### Shape data ------------------------------------------------------------
  gbm_caret_split_indexes <- # Separate data in two using p
    createDataPartition(y = my_analysis_data$eih, p = 0.65, list = FALSE)
  gbm_caret_train_data <- # Create a train data set
    my_analysis_data[gbm_caret_split_indexes, ]
  gbm_caret_test_data <- # Create a test data set
    my_analysis_data[-gbm_caret_split_indexes, ]

  gbm_caret_train_data$eih <- # Factorize EIH column
    as.numeric(x = as.factor(x = gbm_caret_train_data$eih))

  gbm_caret_train_data <- na.omit(gbm_caret_train_data)
  gbm_caret_test_data <- na.omit(gbm_caret_test_data)

  ### Train model -----------------------------------------------------------
  gbm_caret_param_grid <- expand.grid(
    n.trees = seq(100, 1000, by = 50),
    interaction.depth = seq(1, 7, by = 1),
    n.minobsinnode = 3,
    shrinkage = c(0.001, 0.01, 0.1)
  )

  gbm_caret_model <- train(
    as.factor(eih) ~ .,
    data = gbm_caret_train_data,
    method = "gbm",
    distribution = "bernoulli",
    tuneGrid = gbm_caret_param_grid,
    trControl = trainControl(
      method = "cv",
      number = 2,
      returnResamp = "all"
    )
  )
  gbm_caret_importance = summary(object = gbm_caret_model, las = 1)

  ### Test model ------------------------------------------------------------
  gbm_caret_prediction <-
    predict(# Test model by making prediction on test data set
      object = gbm_caret_model,
      newdata = gbm_caret_test_data)

  gbm_caret_prediction_binaries <- gbm_caret_prediction
  gbm_caret_eih_binaries <-
    as.factor(x = as.numeric(x = as.factor(x = gbm_caret_test_data$eih)))
  gbm_caret_confusion <-
    confusionMatrix(gbm_caret_prediction_binaries, gbm_caret_eih_binaries)

  gbm_caret_model_results <- sapply(
    # Put results in a list
    X = ls(pattern = ".*gbm_caret.*"),
    FUN = get,
    simplify = FALSE,
    USE.NAMES = TRUE
  )

  ## Light GBM analysis -----------------------------------------------------
  ## Analysis using lightGBM algorithm

  ### Shape data ------------------------------------------------------------
  light_gbm_split_indexes <- # Separate data in two using p
    createDataPartition(y = my_analysis_data$eih, p = 0.70, list = FALSE)
  light_gbm_train_data <-
    # Create a train data set and remove unused data
    my_analysis_data[light_gbm_split_indexes, ]
  light_gbm_test_data <-
    # Create a test data set and remove unused data
    my_analysis_data[-light_gbm_split_indexes, ]

  light_gbm_train_data_label <-
    light_gbm_train_data %>% # Create training labels
    select("eih")
  light_gbm_train_data <-
    light_gbm_train_data %>% # Create training data frame
    select(-c("eih")) %>%
    lapply(X = ., FUN = as.numeric) %>%
    as.data.frame(x = .)
  light_gbm_train_data_label$eih <-
    as.numeric(x = as.factor(x = light_gbm_train_data_label$eih)) - 1 # Transform label as factors

  light_gbm_test_data_label <-
    light_gbm_test_data %>% # Create training labels
    select("eih")
  light_gbm_test_data <-
    light_gbm_test_data %>% # Create testing data frame
    select(-c("eih")) %>%
    lapply(X = ., FUN = as.numeric) %>%
    as.data.frame(x = .)
  light_gbm_test_data_label$eih <-
    as.numeric(x = as.factor(x = light_gbm_test_data_label$eih)) - 1 # Transform label as factors

  light_gbm_dtrain <- # Create training dataset used by lightGBM
    lgb.Dataset(
      data = as.matrix(x = light_gbm_train_data),
      label = as.matrix(x = light_gbm_train_data_label)
    )
  light_gbm_dtest <- # Create testing dataset used by lightGBM
    lgb.Dataset.create.valid(
      dataset = light_gbm_dtrain,
      data = as.matrix(x = light_gbm_test_data),
      label = as.matrix(x = light_gbm_test_data_label)
    )

  ### Configure -------------------------------------------------------------
  my_config <- NULL
  my_config <- paste0("Params/Best_params", my_counter, ".rds")
  if (is.null(my_config)) {
    source_python("./Scripts/LGBM_optuna_tune.py")
    my_params <- study$best_params
  } else {
    my_params <- readRDS(file = my_config)
  }

  light_gbm_params <- c(list(
    # Define parameters for lightGBM training
    objective = 'binary',
    boosting = "dart",
    metric = "binary_logloss"
  ),
  my_params)

  light_gbm_valids <-
    list(test = light_gbm_dtest) # Create a valid (reference) dataset

  ### Train model -----------------------------------------------------------
  light_gbm_model <- lgb.train(
    # Train model
    params = light_gbm_params,
    data = light_gbm_dtrain,
    nrounds = 1500L,
    valids = light_gbm_valids
  )

  ### Test model ------------------------------------------------------------
  light_gbm_test_data <- as.matrix(x = light_gbm_test_data)
  light_gbm_pred <-
    predict(object = light_gbm_model, light_gbm_test_data, reshape = TRUE)
  light_gbm_pred_y = ifelse(light_gbm_pred > 0.505, 1, 0)

  light_gbm_confusion <-
    confusionMatrix(as.factor(x = light_gbm_test_data_label$eih),
                    as.factor(x = light_gbm_pred_y))

  ### Plotting --------------------------------------------------------------
  ### Feature importance
  light_gbm_tree_imp = lgb.importance(light_gbm_model, percentage = TRUE)
  light_gbm_importance_plot <-
    lgb.plot.importance(light_gbm_tree_imp, measure = "Gain", top_n = 10)
  light_gbm_importance_plot_multi <-
    lgb.plot.interpretation(light_gbm_importance_plot)

  light_gbm_model_results <- sapply(
    # Put results in a list
    X = ls(pattern = ".*light_gbm.*"),
    FUN = get,
    simplify = FALSE,
    USE.NAMES = TRUE
  )

  # Data structure ----------------------------------------------------------
  if (is.null(my_config)) {
    saveRDS(
      object = as.list(study$best_params),
      file = paste0(
        "Output/Model_",
        my_date,
        "/Best_params",
        my_counter,
        ".rds"
      )
    )
    saveRDS(
      object = light_gbm_model_results,
      file = paste0(
        "Output/Model_",
        my_date,
        "/LightGBM_model",
        my_counter,
        ".rds"
      )
    )
    saveRDS(
      object = study,
      file = paste0(
        "Output/Model_",
        my_date,
        "/Optune_study",
        my_counter,
        ".rds"
      )
    )
  }

  assign(
    paste0("my_models_", names(analysis_data[my_counter])),
    sapply(
      # Put results in a list
      X = ls(pattern = ".*model_results"),
      FUN = get,
      simplify = FALSE,
      USE.NAMES = TRUE
    )
  )
  my_counter <- my_counter + 1
  rm(list = setdiff(
    x = ls(),
    y = ls(pattern = "my_data|my_results|my_models.*|my_counter|analysis_data|my_date")
  ))
}

my_results <- append(x = my_results,
                     values = sapply(
                       X = ls(pattern = "my_models.*"),
                       FUN = get,
                       simplify = FALSE,
                       USE.NAMES = TRUE
                     ))

# Advanced plotting -------------------------------------------------------
source(file = "./Scripts/Visualization.R", echo = TRUE)

# Remove temporary variables
rm(list = setdiff(x = ls(), y = ls(pattern = "my_data|my_results")))

# Export data -------------------------------------------------------------
# Save environment to avoid recomputing
save.image(file = "./Environments/predictive.RData")
