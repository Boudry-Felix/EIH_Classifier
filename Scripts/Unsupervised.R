# Informations ------------------------------------------------------------
# Title: Unsupervised.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: GPLv3
# Description: Analyze data using unsupervised algorithms.

# Libraries ---------------------------------------------------------------
# List of used libraries.
require(tidyverse)
require(factoextra)
require(clusplus)
require(magrittr)
require(missRanger)
require(caret)

# Clustering --------------------------------------------------------------
# Compute and plot clusters
cluster_data <- # Clean and select usable data
  select(.data = analysis_data, -any_of(predict_label))

## Cluster computation ----------------------------------------------------
## Compute clusters with different methods
kclust_data <-
  eclust(cluster_data,
         k = cluster_number,
         FUNcluster = "kmeans",
         graph = FALSE)
hclust_bu_data <-
  eclust(
    cluster_data,
    k = cluster_number,
    FUNcluster = "hclust",
    hc_method = "ward.D2",
    graph = FALSE
  )
hclust_td_data <-
  eclust(
    cluster_data,
    k = cluster_number,
    FUNcluster = "diana",
    hc_method = "ward.D2",
    graph = FALSE
  )

## Result metrics ---------------------------------------------------------
## Compute confusion matrix to assert accuracy
kclust_confusion <-
  confusionMatrix(kclust_data$cluster %>% as.factor(),
                  analysis_data$eih %>% as.factor())
hclust_bu_confusion <-
  confusionMatrix(hclust_bu_data$cluster %>% as.factor(),
                  analysis_data$eih %>% as.factor())
hclust_td_confusion <-
  confusionMatrix(hclust_td_data$cluster %>% as.factor(),
                  analysis_data$eih %>% as.factor())

## Plotting ---------------------------------------------------------------

### Cluster plots ---------------------------------------------------------
### Graphical representations of the computed clusters
kclust_graph <-
  fviz_cluster(
    object = kclust_data,
    data = analysis_data,
    geom = NULL,
    show.clust.cent = FALSE
  ) +
  geom_point(aes(shape = analysis_data$eih %>% as.factor())) +
  ggtitle("K-means clustering for EIH status") +
  labs(shape = "Status")
kclust_coord <-
  plot_clus_coord(cluster_model = kclust_data, data = cluster_data) +
  ggtitle("K-means feature importance for EIH status") +
  theme(axis.text.x.bottom = element_text(angle = 45, size = 5))
my_label_cols <-
  analysis_data$eih %>%
  as.factor() %>%
  as.numeric()
hclust_bu_graph <-
  fviz_dend(
    x = hclust_bu_data,
    cex = 0.5,
    label_cols = my_label_cols,
    horiz = TRUE,
    guides = "none",
    type = "circular"
  ) +
  ggtitle("Hierarchical clustering for EIH status")
hclust_td_graph <-
  fviz_dend(
    x = hclust_td_data,
    cex = 0.5,
    label_cols = my_label_cols,
    horiz = TRUE,
    guides = "none",
    type = "circular"
  ) +
  ggtitle("Hierarchical clustering for EIH status")

### Boxplots --------------------------------------------------------------
### Boxplots of analyzed data by cluster

## Adding cluster group to data
plot_df <-
  do.call(
    "cbind",
    list(
      cluster_data,
      kclust = kclust_data$cluster,
      hclust_td = hclust_bu_data$cluster,
      hclust_bu = hclust_td_data$cluster
    ),
    envir = .GlobalEnv
  )

cluster_columns <-
  # Select cluster columns
  colnames(x = .GlobalEnv$plot_df[grepl(pattern = "clust", x = colnames(.GlobalEnv$plot_df))])

# Plotting boxplots for each variable in each cluster
cluster_boxplots <- lapply(
  X = cluster_columns,
  FUN = \(my_col)
  sapply(
    X = colnames(.GlobalEnv$plot_df),
    FUN = boxplots_by_clust,
    cluster_col = my_col,
    used_env = compute_env,
    simplify = FALSE,
    USE.NAMES = TRUE
  )
) %>% `names<-`(value = cluster_columns)
