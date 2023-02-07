# Informations ------------------------------------------------------------
# Title: Functions.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Functions used for this project

# Libraries ---------------------------------------------------------------
require(kableExtra)
require(grDevices)


# Formats -----------------------------------------------------------------
my_table <- function(input, ...) {
  # Custom kable table
  kable(x = input, ... = ...) %>%
    kable_styling(bootstrap_options = c("striped"),
                  full_width = FALSE)
}

# Computations ------------------------------------------------------------
remove_outliers <- function(input) {
  # Removes outliers based on boxplot.stats
  input <-
    lapply(
      X = input,
      FUN = function(my_dataframe)
        lapply(
          X = my_dataframe,
          FUN = function(my_col)
            if (is.numeric(my_col)) {
              lapply(
                X = my_col,
                FUN = function(my_value)
                  ifelse(
                    test = my_value %in% boxplot.stats(my_col)$out,
                    yes = NA,
                    no = as.numeric(my_value)
                  )
              )
            }
        )
    ) %>%
    lapply(
      FUN = function(my_dataframe)
        data.frame(sapply(X = my_dataframe, FUN = c))
    ) %>%
    lapply(
      FUN = function(x)
        lapply(X = x, FUN = as.numeric)
    ) %>%
    lapply(FUN = as.data.frame)
}

compute_relative <- function(input) {
  maximum_columns <-
    grep(pattern = "max", x = colnames(x = input)) # List max columns
  for (my_column in maximum_columns) {
    my_variable_name <- colnames(x = input[my_column]) %>%
      sub(pattern = "_max", replacement = "") # remove "_max" from column name
    max_colname <- paste0(my_variable_name, "_max")
    mean_colname <- paste0(my_variable_name, "_mean")
    min_colname <- paste0(my_variable_name, "_min")
    input[min_colname] <- # Compute minimum values as %
      100 * input[min_colname] / input[max_colname]
    input[mean_colname] <- # Compute mean values as %
      100 * input[mean_colname] / input[max_colname]
  }
  return(input)
}


# Clusters ----------------------------------------------------------------
optimal_clust <- function(input_data, cluster_method) {
  # Choose number of cluster for analysis
  elbow_graph <-
    fviz_nbclust(input_data, cluster_method, method = "wss") +
    labs(subtitle = "Elbow method") +
    ggtitle(label = "Optimal number of cluster")
  silhouette_graph <-
    fviz_nbclust(input_data, cluster_method, method = "silhouette") +
    labs(subtitle = "Silhouette method") +
    ggtitle(label = "Optimal number of cluster")
  gap_graph <-
    fviz_nbclust(
      input_data,
      cluster_method,
      nstart = 25,
      method = "gap_stat",
      nboot = 50
    ) +
    labs(subtitle = "Gap statistic method") +
    ggtitle(label = "Optimal number of cluster")

  cluster_number_graph <- # Put graphs in list
    lst(elbow_graph, silhouette_graph, gap_graph)
  names(x = cluster_number_graph) <- c("elbow", "silhouette", "gap")
  return(cluster_number_graph)
}

compute_kclust <- function(input, cluster_number) {
  kclust_data <- lapply(
    X = cluster_number,
    FUN = function(nclust) {
      kmeans(input, nclust)
    }
  ) %>%
    `names<-`(value = paste0("kclust_", cluster_number))
}

boxplots_by_clust <- function(data_col, cluster_col) {
  ggplot(plot_df, aes(x = !!sym(cluster_col), y = !!sym(paste(data_col)))) +
    geom_boxplot(aes(
      group = !!sym(cluster_col),
      fill = as.factor(!!sym(cluster_col))
    )) +
    ggtitle(label = paste(data_col, "by cluster")) +
    labs(fill = cluster_col)
}

subject_repartition <- function(data_col, cluster_col) {
  ggplot(data = plot_df, mapping = aes(x = !!(1:nrow(x = plot_df)),
                                       y = !!sym(paste(data_col)))) +
    geom_point(mapping = aes(color = as.factor(!!sym(
      paste(cluster_col)
    )))) +
    ggtitle(label = "Subject repartition in clusters") +
    labs(fill = cluster_col)
}
