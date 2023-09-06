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
require(dbscan)
require(clusplus)
require(magrittr)
require(missRanger)
require(caret)
require(fossil)

# Clustering --------------------------------------------------------------
# Compute and plot clusters
cluster_data <- # Clean and select usable data
  analysis_data %>%
  select_if(is.numeric) %>%
  select(.data = ., -any_of(discriminating_variables)) %>%
  select(
    where(
      ~!all(is.na(.x))
    )
  ) %>%
  missRanger() %>%
  {
    if (scale_data)
      scale(x = .) %>% as.data.frame()
    else
      .
  }

## Optimal cluster choice -------------------------------------------------
cluster_number_graph <-
  optimal_clust(input_data = cluster_data, cluster_method = kmeans)

## Cluster computation ----------------------------------------------------
## Compute clusters with different methods
kclust_data <-
  lapply(cluster_number,
         \(x)
         eclust(
           cluster_data,
           k = x,
           FUNcluster = "kmeans",
           graph = FALSE
         )) %>%
  `names<-`(value = cluster_number_names)
hclust_bu_data <-
  lapply(
    cluster_number,
    \(x)
    eclust(
      cluster_data,
      k = x,
      FUNcluster = "hclust",
      hc_method = "ward.D2",
      graph = FALSE
    )
  ) %>%
  `names<-`(value = cluster_number_names)
hclust_td_data <-
  lapply(
    cluster_number,
    \(x)
    eclust(
      cluster_data,
      k = x,
      FUNcluster = "diana",
      hc_method = "ward.D2",
      graph = FALSE
    )
  ) %>%
  `names<-`(value = cluster_number_names)
dbscan_data <-
  dbscan(x = cluster_data,
         eps = dbscan_eps,
         minPts = dbscan_minPts)
optics_data <-
  optics(x = cluster_data,
         eps = optics_eps,
         minPts = optics_minPts) %>%
  extractXi(xi = optics_xi)

## Result accuracy --------------------------------------------------------
## Compute confusion matrix to assert accuracy
kclust_confusion <-
  mapply(
    function(x, y) {
      confusionMatrix(x[["cluster"]] %>% as.factor(),
                      y %>% as.factor() %>%
                        as.numeric() %>%
                        as.factor() %>%
                        rev())
    } %>% list,
    x = kclust_data,
    y = c(
      analysis_data$eih %>% as.data.frame(),
      analysis_data$eih_severity %>% as.data.frame()
    )
  )
hclust_bu_confusion <-
  mapply(
    function(x, y) {
      confusionMatrix(x[["cluster"]] %>% as.factor(),
                      y %>% as.factor() %>%
                        as.numeric() %>%
                        as.factor() %>%
                        rev())
    } %>% list,
    x = hclust_bu_data,
    y = c(
      analysis_data$eih %>% as.data.frame(),
      analysis_data$eih_severity %>% as.data.frame()
    )
  )
hclust_td_confusion <-
  mapply(
    function(x, y) {
      confusionMatrix(x[["cluster"]] %>% as.factor(),
                      y %>% as.factor() %>%
                        as.numeric() %>%
                        as.factor() %>%
                        rev())
    } %>% list,
    x = hclust_td_data,
    y = c(
      analysis_data$eih %>% as.data.frame(),
      analysis_data$eih_severity %>% as.data.frame()
    )
  )
dbscan_rand <-
  rand.index(
    analysis_data$eih %>% as.factor() %>% as.numeric() %>% replace(is.na(.), 0),
    dbscan_data$cluster %>% as.numeric()
  )
optics_rand <-
  rand.index(
    analysis_data$eih %>% as.factor() %>% as.numeric() %>% replace(is.na(.), 0),
    optics_data$cluster %>% as.numeric()
  )

## Plotting ---------------------------------------------------------------

### Cluster plots ---------------------------------------------------------
### Graphical representations of the computed clusters
kclust_graph <-
  lapply(kclust_data,
         function(x) {
           x <- append(x = x, values = analysis_data)
           fviz_cluster(
             object = x,
             data = analysis_data,
             geom = NULL,
             show.clust.cent = FALSE
           ) +
             {
               if (length(unique(x[["cluster"]])) == 2)
                 geom_point(aes(shape = analysis_data$eih))
             } +
             {
               if (length(unique(x[["cluster"]])) == 4)
                 geom_point(aes(shape = analysis_data$eih_severity))
             } +
             labs(shape = "Status")
         }) %>%
  `names<-`(value = cluster_number_names)
kclust_coord <-
  lapply(X = kclust_data, FUN = plot_clus_coord, data = cluster_data) %>%
  `names<-`(value = cluster_number_names)
hclust_bu_graph <- lapply(X = hclust_bu_data,
                          function(x) {
                            if (length(unique(x[["cluster"]])) == 2)
                            {
                              my_label_cols <-
                                analysis_data$eih %>%
                                as.factor() %>%
                                as.numeric()
                            }
                            else if (length(unique(x[["cluster"]])) == 4)
                            {
                              my_label_cols <-
                                analysis_data$eih_severity %>%
                                as.factor() %>%
                                as.numeric()
                            }
                            fviz_dend(
                              x = x,
                              cex = 0.5,
                              label_cols = my_label_cols,
                              horiz = TRUE,
                              guides = "none"
                            )
                          }) %>%
  `names<-`(value = cluster_number_names)
hclust_td_graph <- lapply(X = hclust_td_data,
                          function(x) {
                            if (length(unique(x[["cluster"]])) == 2)
                            {
                              my_label_cols <-
                                analysis_data$eih %>%
                                as.factor() %>%
                                as.numeric()
                            }
                            else if (length(unique(x[["cluster"]])) == 4)
                            {
                              my_label_cols <-
                                analysis_data$eih_severity %>%
                                as.factor() %>%
                                as.numeric()
                            }
                            fviz_dend(
                              x = x,
                              cex = 0.5,
                              label_cols = my_label_cols,
                              horiz = TRUE,
                              guides = "none"
                            )
                          }) %>%
  `names<-`(value = cluster_number_names)
dbscan_graph <- fviz_cluster(dbscan_data,
                             cluster_data,
                             geom = "point",
                             show.clust.cent = FALSE) +
  guides(shape = FALSE)
optics_graph <- plot(optics_data)
optics_graph <- recordPlot()

### Boxplots --------------------------------------------------------------
### Boxplots of analyzed data by cluster

## Adding cluster group to data
plot_df <-
  do.call("cbind",
          list(
            cluster_data,
            kclust = lapply(X = kclust_data, FUN = "[[", "cluster"),
            hclust_td = lapply(X = hclust_bu_data, FUN = "[[", "cluster"),
            hclust_bu = lapply(X = hclust_td_data, FUN = "[[", "cluster")
          ))

## Creating boxplots
cluster_columns <- # Select cluster columns
  colnames(x = plot_df[grepl(pattern = "clust", x = colnames(plot_df))])

# Dimensionality reduction ------------------------------------------------
cluster_data2 <- cluster_data %>%
  mutate(ID = row_number())

tsne_results <- Rtsne::Rtsne(cluster_data2 %>% scale(), dims = 2, pca = F) %$%
  .$Y %>%
  as.data.frame() %>%
  rename(tSNE1 = "V1", tSNE2 = "V2") %>%
  mutate(ID = row_number()) %>%
  inner_join(cluster_data2, by = "ID") %>%
  left_join(analysis_data %>% mutate(ID = row_number()), by = "ID")

tsne_results %>%
  ggplot(aes(x = tSNE1,
             y = tSNE2,
             color = eih)) +
  geom_point()

# Plotting boxplots for each variable in each cluster
cluster_boxplots <- lapply(
  X = cluster_columns,
  FUN = \(my_col)
  sapply(
    X = colnames(plot_df),
    FUN = boxplots_by_clust,
    cluster_col = my_col,
    simplify = FALSE,
    USE.NAMES = TRUE
  )
) %>% `names<-`(value = cluster_columns)

# Export data -------------------------------------------------------------
# Save environment to avoid recomputing
save.image(file = paste0("./Output/", analysis_date, "/descriptive.RData"))
