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

# Data preparation --------------------------------------------------------
ml_data <-
  gbm_data_partition(input = analysis_data,
                     sep_col = "eih",
                     sep_prop = lgbm_split)
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

# Light GBM analysis ------------------------------------------------------
source_python("./Scripts/ML.py")

## Plotting ---------------------------------------------------------------
## Feature importance
lgbm_model <- lgb.load("lgbm_model.txt")
lgbm_test_data_pred <- as.matrix(x = ml_test_data$values)

lgbm_importance <- lgb.importance(lgbm_model, percentage = TRUE)
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

# Export data -------------------------------------------------------------
# Save environment to avoid recomputing
dir_create(path = paste0("Output/", analysis_date, "/params/"))
lgbm_export(lgbm_model_results = lgbm_model_results)
save.image(file = paste0("./Output/", analysis_date, "/supervised.RData"))
