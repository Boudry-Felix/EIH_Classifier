# Informations ------------------------------------------------------------

# Title: lightGBM.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Run the predictive analysis several times to find optimal nrounds.

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
load(file = "./Environments/predictive.RData") # Load environment

# Select data ------------------------------------------------------------
analysis_data <- # Put all data to analyze in a list
  lst(my_data$summary, my_data$summary_relative) %>%
  `names<-`(c("absolute", "relative"))

# Analysis ----------------------------------------------------------------
my_counter <- 1
for (my_analysis_data in analysis_data) {
  # Setting general variables
  my_analysis_data <-
    merge(x = my_analysis_data,
          y = my_data$labels[c("eih", "subject")],
          by = "subject") %>%
    select(.data = ., -c("subject"))

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
  my_params <- paste0("Params/Best_params", my_counter, ".rds")
  my_params <- readRDS(file = my_params)

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
  assign(paste0("Results", my_counter),
         as.data.frame(x = matrix(ncol = 2)) %>%
           `colnames<-`(c("rounds", "accuracy")))
  my_rounds <- seq(300, 1500, by = 100)
  for (test_rounds in my_rounds) {
    light_gbm_model <- lgb.train(
      # Train model
      params = light_gbm_params,
      data = light_gbm_dtrain,
      nrounds = test_rounds,
      valids = light_gbm_valids,
      verbose = -1
    )
    ### Test model --------------------------------------------------------
    light_gbm_test_data <- as.matrix(x = light_gbm_test_data)
    light_gbm_pred <-
      predict(object = light_gbm_model,
              light_gbm_test_data,
              reshape = TRUE)
    if (my_counter == 1) {
      light_gbm_pred_y = ifelse(test = light_gbm_pred > 0.495,
                                yes = 1,
                                no = 0)
    } else {
      light_gbm_pred_y = ifelse(test = light_gbm_pred > 0.5,
                                yes = 1,
                                no = 0)
    }
    light_gbm_confusion <-
      confusionMatrix(as.factor(x = light_gbm_test_data_label$eih),
                      as.factor(x = light_gbm_pred_y))
    ### Save results ---------------------------------------------------------
    assign(x = paste0("Results", my_counter),
           value = rbind(
             get(x = paste0("Results", my_counter)),
             c(test_rounds, light_gbm_confusion$overall[[1]])
           ))
  }
  my_counter <- my_counter + 1
}

# Remove temporary variables
rm(list = setdiff(
  x = ls(),
  y = ls(pattern = "my_data|my_results|Results.*")
))
