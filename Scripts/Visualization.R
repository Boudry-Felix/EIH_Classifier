# Informations ------------------------------------------------------------

# Title: Visualization.R
# Author: Félix Boudry
# Contact: <felix.boudry@laposte.net>
# License: Private
# Description: Visualize computed data and models.

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
## List of used libraries.
library(shapviz)
library(janitor)
library(dplyr)
library(reticulate)

# Python visualization ----------------------------------------------------
lgbm_params <-
  my_results$my_models_absolute$light_gbm_model_results$light_gbm_params
light_gbm_train_data <-
  my_results$my_models_absolute$light_gbm_model_results$light_gbm_train_data
light_gbm_train_data_label <-
  my_results$my_models_absolute$light_gbm_model_results$light_gbm_train_data_label
light_gbm_test_data <-
  my_results$my_models_absolute$light_gbm_model_results$light_gbm_test_data
light_gbm_test_data_label <-
  my_results$my_models_absolute$light_gbm_model_results$light_gbm_test_data_label
viz_data <- merge(x = my_data$summary,
                  y = my_data$labels[c("eih", "subject")],
                  by = "subject") %>% na.omit()
viz_data$eih <- as.numeric(as.factor(viz_data$eih))
source_python("./Scripts/Visualization.py")

# R visualization ---------------------------------------------------------
viz_sample <- merge(x = my_data$summary,
                    y = my_data$labels[c("eih", "subject")],
                    by = "subject") %>%
  select(.data = ., -c("subject", "eih"))
viz_sample <- viz_sample[sample(nrow(viz_sample), 127), ]

shap_data <-
  shapviz(
    my_results$my_models_absolute$light_gbm_model_results$light_gbm_model,
    X_pred = data.matrix(viz_sample),
    X = viz_sample
  )

sv_waterfall(shap_data, row_id = 1)
sv_force(shap_data)
sv_importance(shap_data, kind = "beeswarm")
sv_dependence(shap_data, v = "vco2_max", "auto")

# Remove temporary variables
rm(list = setdiff(x = ls(), y = ls(pattern = "my_data|my_results")))
