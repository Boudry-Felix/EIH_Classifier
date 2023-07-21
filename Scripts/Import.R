# Informations ------------------------------------------------------------
# Title: Import.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: GPLv3
# Description: Process used to import data and transform them to be used in
# this study

# Libraries ---------------------------------------------------------------
require(dplyr)
require(tibble)
require(FactoMineR)
require(missMDA)

imported_data <- project_import(project_path = "Data_raw")

infos <- imported_data$infos
my_data <- imported_data$data %>%
  common_col()

my_colnames <- colnames(my_data[[1]])

summary_simple <- my_summary(my_data, my_data)
summary_relative <- compute_relative(summary_simple)

keeped_rows <- summary_simple %>%
  as.data.frame() %>%
  select_if(is.numeric) %>%
  na.omit() %>%
  rownames()

PCA_summary <- lapply(my_data[keeped_rows %>% as.numeric()], imputePCA) %>%
  lapply(FUN = as.data.frame) %>%
  lapply(FUN = select, contains(my_colnames)) %>%
  lapply(FUN = PCA, graph = FALSE) %>%
  lapply(FUN = "[", "eig") %>%
  lapply(FUN = unlist) %>%
  do.call(what = rbind, args = .) %>%
  as.data.frame() %>%
  rownames_to_column(.data = ., var = "subject")


# Cleaning ----------------------------------------------------------------
# Removes outliers in summaries
my_summaries <-
  lst(summary_simple, summary_relative, PCA_summary) %>%
  `names<-`(value = c("absolute", "relative", "PCA"))

# Labelling ---------------------------------------------------------------
encoded_summaries <- lapply(X = my_summaries, FUN = df_encode)
