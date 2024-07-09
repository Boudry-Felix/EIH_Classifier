# Informations ------------------------------------------------------------
# Title: Functions.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: GPLv3
# Description: Functions used for this project

# Libraries ---------------------------------------------------------------
require(magrittr)

# Pre-processing ----------------------------------------------------------
col_encode <- function(my_col) {
  convert_dic <- dplyr::lst()
  if (is.numeric(x = my_col)) {
    my_col
  } else {
    label <- CatEncoders::LabelEncoder.fit(y = my_col)
    convert_dic <<-
      append(x = .GlobalEnv$convert_dic, values = label)
    CatEncoders::transform(enc = label, my_col)
  }
}

df_encode <- function(input = my_data, list_names) {
  # Encode (labeling) entire data frames
  encoded_data <- lapply(X = input, FUN = col_encode) %>% as.data.frame()
  output <- dplyr::lst(encoded_data)
  if (!missing(x = list_names)) {
    names(output) <- list_names
  }
  return(output)
}

gbm_data_partition <- function(input, sep_col, sep_prop) {
  split_indexes <- # Separate data in two using p
    caret::createDataPartition(y = input[[sep_col]], p = sep_prop, list = FALSE)
  train_data <- # Create a train data set
    input[split_indexes, ]
  test_data <- # Create a test data set
    input[-split_indexes, ]
  return(dplyr::lst(train_data, test_data))
}

clean_dataset <- function(input) {
  output <- select(.data = input, -any_of(excluded_variables)) %>%
    as.data.frame()
  output[output == 0] <- NA
  output[output == Inf] <- NA
  output[output == "-Inf"] <- NA
  output <-
    select(.data = output, -nearZeroVar(output, freqCut = 99 / 1)) %>%  # Detects columns without variations
    dplyr::mutate(dplyr::across(dplyr::where(is.double), ~ tidyr::replace_na(., median(., na.rm = TRUE)))) %>%
    dplyr::mutate(dplyr::across(
      dplyr::where(is.integer),
      ~ tidyr::replace_na(., median(., na.rm = TRUE) %>% as.integer())
    ))
  # Scale data between 0 and 1
  tmp <- output[, c("eih")]
  process <- caret::preProcess(output, method = c("range"))
  output <- stats::predict(process, output)
  output$eih <- tmp
  return(output)
}

# Formats -----------------------------------------------------------------
my_table <- function(input, ...) {
  # Custom kable table style
  kableExtra::kable(x = input, ... = ...) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped"),
                              full_width = FALSE)
}

eval_knit_param <- function(input) {
  # Parse and evaluate text used as parameter to be executed
  output <- parse(text = input) %>%
    eval()
  return(output)
}

# Plots -------------------------------------------------------------------
## Clusters ---------------------------------------------------------------
hclust_plot <- function(clust_data) {
  clust_data <- as.dendrogram(clust_data)
  clust_color <- ifelse(analysis_data$eih == 1, "skyblue", "orange")
  par(mar = c(8, 2, 2, 2))

  clust_data %>%
    dendextend::set("labels_col",
                    value = c("skyblue", "orange"),
                    k = cluster_number) %>%
    dendextend::set("branches_k_color",
                    value = c("skyblue", "orange"),
                    k = cluster_number) %>%
    dendextend::set("leaves_pch", 15)  %>%
    dendextend::set("nodes_cex", 0.4) %>%
    dendextend::set("labels_cex", 0.6) %>%
    plot(axes = FALSE)

  dendextend::colored_bars(
    colors = clust_color,
    dend = clust_data,
    rowLabels = "EIH",
    text_shift = -1
  )
  grDevices::recordPlot()
}

## Boxplots ---------------------------------------------------------------
boxplots_by_clust <- function(data_col, cluster_col) {
  ggplot2::ggplot(plot_df, aes(x = !!sym(cluster_col), y = !!sym(paste(data_col)))) +
    ggplot2::geom_boxplot(aes(
      group = !!sym(cluster_col),
      fill = as.factor(!!sym(cluster_col))
    )) +
    ggplot2::ggtitle(label = paste(data_col, "by cluster")) +
    ggplot2::labs(fill = cluster_col)
}

## GBM --------------------------------------------------------------------
shap_plots <- function(model, test_data_pred) {
  shap_data <-
    shapviz::shapviz(object = model, X_pred = test_data_pred)

  importance_plot <-
    shapviz::sv_importance(object = shap_data,
                           kind = "bar",
                           max_display = 10L)
  waterfall_plot <-
    shapviz::sv_waterfall(object = shap_data,
                          row_id = 1,
                          max_display = 10L)
  force_plot <-
    shapviz::sv_force(object = shap_data, max_display = 10L)
  beeswarm_plot <-
    shapviz::sv_importance(object = shap_data,
                           kind = "beeswarm",
                           max_display = 10L)

  return(dplyr::lst(importance_plot, waterfall_plot, force_plot, beeswarm_plot))
}

# Export ------------------------------------------------------------------
# Export data functions
result_save <- function() {
  # Save results and clean base directory
  ## Savings
  fs::dir_copy(path = "Models",
               new_path = paste0("Output/", analysis_date, "/Models"))
  fs::dir_copy(path = "./EIH_Modeling_Classification_files/figure-html/",
               new_path = paste0("Output/", analysis_date))
  file.rename(
    from = "EIH_Modeling_Classification.md",
    to = paste0(
      "./Output/",
      analysis_date,
      "/EIH_Modeling_Classification.md"
    )
  )
  save.image(file = paste0("./Output/", analysis_date, "/global.RData"))
}
