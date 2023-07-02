# Informations ------------------------------------------------------------
# Title: Import.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: GPLv3
# Description: Process used to import data and transforme them to be used in this study

imported_data <- project_import(project_path = "Data")

my_data <- imported_data %$%
  my_data %>%
  common_col()

infos <- imported_data$my_data_infos

summary_simple <- my_summary(my_data, my_data)
