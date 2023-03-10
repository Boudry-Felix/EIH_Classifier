# Informations ------------------------------------------------------------
# Title: Functions.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: GPLv3
# Description: Functions used for this project

# Libraries ---------------------------------------------------------------
require(kableExtra)
require(grDevices)
require(CatEncoders)


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

col_encode <- function(my_col) {
  convert_dic <- lst()
  if (is.numeric(x = my_col)) {
    my_col
  } else {
    label <- LabelEncoder.fit(y = my_col)
    convert_dic <<- append(x = convert_dic, values = label)
    transform(enc = label, my_col)
  }
}

df_encode <- function(input = my_data, list_names) {
  # Encode (labeling) entire data frames
  encoded_data <- lapply(X = input,
                         FUN = col_encode) %>% as.data.frame()
  output <- lst(convert_dic, encoded_data)
  if (!missing(x = list_names)) {
    names(output) <- list_names
  }
  return(output)
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
  kclust_data <- lapply(X = cluster_number,
                        FUN = kmeans, x = input) %>%
    `names<-`(value = paste0("kclust_", cluster_number))
}

hcl_plot <- function(input_data, nclust) {
  plot(input_data, cex = 0.6)
  rect.hclust(tree = input_data, k = nclust)
  recordPlot()
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

# GBM ---------------------------------------------------------------------
gbm_data_partition <- function(input, sep_col, sep_prop) {
  split_indexes <- # Separate data in two using p
    createDataPartition(y = input[[sep_col]], p = sep_prop, list = FALSE)
  train_data <- # Create a train data set
    input[split_indexes,]
  test_data <- # Create a test data set
    input[-split_indexes,]
  return(lst(train_data, test_data))
}

lgbm_round_tune <-
  function(my_rounds,
           lgbm_params,
           lgbm_dtrain,
           lgbm_valids,
           lgbm_test_data,
           lgbm_test_data_label) {
    optimal_rounds_result <-
      matrix(nrow = 0, ncol = 2) %>% as.data.frame()
    for (test_rounds in my_rounds) {
      lgbm_model <- lgb.train(
        # Train model
        params = lgbm_params,
        data = lgbm_dtrain,
        nrounds = test_rounds,
        valids = lgbm_valids,
        verbose = -1
      )
      ### Test model --------------------------------------------------------
      lgbm_test_data <- as.matrix(x = lgbm_test_data)
      lgbm_pred <-
        predict(object = lgbm_model,
                lgbm_test_data,
                reshape = TRUE)
      lgbm_pred_y = ifelse(test = lgbm_pred > median(lgbm_pred),
                           yes = 1,
                           no = 0)
      lgbm_confusion <-
        confusionMatrix(as.factor(x = lgbm_test_data_label$eih),
                        as.factor(x = lgbm_pred_y))
      ### Save results ---------------------------------------------------------
      optimal_rounds_result <-
        rbind(optimal_rounds_result,
              c(test_rounds, lgbm_confusion$overall[[1]]))
    }
    colnames(optimal_rounds_result) <- c("rounds", "results")
    best_round <- which.max(optimal_rounds_result[["results"]])
    best_round <- optimal_rounds_result[["rounds"]][best_round]
    return(lst(optimal_rounds_result, best_round))
  }

gbm_pred <-
  function(input_model,
           input_data,
           predicted_var,
           threshold = NULL) {
    importance <-
      summary(object = input_model,
              las = 1,
              cBars = 10)
    importance_plot <- recordPlot()
    gbm_prediction <-
      predict(
        object = input_model,
        newdata = input_data$test_data,
        na.action = na.pass
      )
    if (!is.factor(gbm_prediction)) {
      gbm_prediction <- ifelse(test = gbm_prediction > threshold,
                               yes = 1,
                               no = 0) %>%
        as.factor()
    }
    gbm_predicted <-
      as.factor(x = input_data[["test_data"]][[predicted_var]])
    gbm_confusion <-
      confusionMatrix(gbm_prediction, gbm_predicted, mode = "everything")
    return(lst(stat = gbm_confusion, importance, importance_plot))
  }

lgb.plot.tree <- function(model = NULL,
                          tree = NULL,
                          rules = NULL) {
  # check model is lgb.Booster
  if (!inherits(model, "lgb.Booster")) {
    stop("model: Has to be an object of class lgb.Booster")
  }
  # check DiagrammeR is available
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("DiagrammeR package is required for lgb.plot.tree",
         call. = FALSE)
  }
  # tree must be numeric
  if (!inherits(tree, 'numeric')) {
    stop("tree: Has to be an integer numeric")
  }
  # tree must be integer
  if (tree %% 1 != 0) {
    stop("tree: Has to be an integer numeric")
  }
  # extract data.table model structure
  dt <- lgb.model.dt.tree(model)
  # check that tree is less than or equal to the maximum tree index in the model
  if (tree > max(dt$tree_index)) {
    stop("tree: has to be less than the number of trees in the model")
  }
  # filter dt to just the rows for the selected tree
  dt <- dt[tree_index == tree, ]
  # change the column names to shorter more diagram friendly versions
  data.table::setnames(
    dt,
    old = c('tree_index', 'split_feature', 'threshold', 'split_gain'),
    new = c('Tree', 'Feature', 'Split', 'Gain')
  )
  dt[, Value := 0.0]
  dt[, Value := leaf_value]
  dt[is.na(Value), Value := internal_value]
  dt[is.na(Gain), Gain := leaf_value]
  dt[is.na(Feature), Feature := 'Leaf']
  dt[, Cover := internal_count][Feature == 'Leaf', Cover := leaf_count]
  dt[, c('leaf_count',
         'internal_count',
         'leaf_value',
         'internal_value') := NULL]
  dt[, Node := split_index]
  max_node <- max(dt[['Node']], na.rm = TRUE)
  dt[is.na(Node), Node := max_node + leaf_index + 1]
  dt[, ID := paste(Tree, Node, sep = '-')]
  dt[, c('depth', 'leaf_index') := NULL]
  dt[, parent := node_parent][is.na(parent), parent := leaf_parent]
  dt[, c('node_parent', 'leaf_parent', 'split_index') := NULL]
  dt[, Yes := dt$ID[match(dt$Node, dt$parent)]]
  dt <- dt[nrow(dt):1, ]
  dt[, No := dt$ID[match(dt$Node, dt$parent)]]
  # which way do the NA's go (this path will get a thicker arrow)
  # for categorical features, NA gets put into the zero group
  dt[default_left == TRUE, Missing := Yes]
  dt[default_left == FALSE, Missing := No]
  zero_present <-
    function(x) {
      sapply(strsplit(as.character(x), '||', fixed = TRUE), function(el) {
        any(el == '0')
      })
    }
  dt[zero_present(Split), Missing := Yes]
  #dt[, c('parent', 'default_left') := NULL]
  #data.table::setcolorder(dt, c('Tree','Node','ID','Feature','decision_type','Split','Yes','No','Missing','Gain','Cover','Value'))
  # create the label text
  dt[, label := paste0(
    Feature,
    "\nCover: ",
    Cover,
    ifelse(Feature == "Leaf", "", "\nGain: "),
    ifelse(Feature == "Leaf", "", round(Gain, 4)),
    "\nValue: ",
    round(Value, 4)
  )]
  # style the nodes - same format as xgboost
  dt[Node == 0, label := paste0("Tree ", Tree, "\n", label)]
  dt[, shape := "rectangle"][Feature == "Leaf", shape := "oval"]
  dt[, filledcolor := "Beige"][Feature == "Leaf", filledcolor := "Khaki"]
  # in order to draw the first tree on top:
  dt <- dt[order(-Tree)]
  nodes <- DiagrammeR::create_node_df(
    n         = nrow(dt),
    ID        = dt$ID,
    label     = dt$label,
    fillcolor = dt$filledcolor,
    shape     = dt$shape,
    data      = dt$Feature,
    fontcolor = "black"
  )
  # round the edge labels to 4 s.f. if they are numeric
  # as otherwise get too many decimal places and the diagram looks bad
  # would rather not use suppressWarnings
  numeric_idx <- suppressWarnings(!is.na(as.numeric(dt[['Split']])))
  dt[numeric_idx, Split := round(as.numeric(Split), 4)]
  # replace indices with feature levels if rules supplied
  levels.to.names <- function(x, feature_name, rules) {
    lvls <- sort(rules[[feature_name]])
    result <- strsplit(x, '||', fixed = TRUE)
    result <- lapply(result, as.numeric)
    levels_to_names <- function(x) {
      names(lvls)[as.numeric(x)]
    }
    result <- lapply(result, levels_to_names)
    result <- lapply(result, paste, collapse = '\n')
    result <- as.character(result)
  }
  if (!is.null(rules)) {
    for (f in names(rules)) {
      dt[Feature == f &
           decision_type == '==', Split := levels.to.names(Split, f, rules)]
    }
  }
  # replace long split names with a message
  dt[nchar(Split) > 500, Split := 'Split too long to render']
  # create the edge labels
  edges <- DiagrammeR::create_edge_df(
    from  = match(dt[Feature != "Leaf", c(ID)] %>% rep(2), dt$ID),
    to    = match(dt[Feature != "Leaf", c(Yes, No)], dt$ID),
    label = dt[Feature != "Leaf", paste(decision_type, Split)] %>%
      c(rep("", nrow(dt[Feature != "Leaf"]))),
    style = dt[Feature != "Leaf", ifelse(Missing == Yes, "bold", "solid")] %>%
      c(dt[Feature != "Leaf", ifelse(Missing == No, "bold", "solid")]),
    rel   = "leading_to"
  )
  # create the graph
  graph <- DiagrammeR::create_graph(nodes_df = nodes,
                                    edges_df = edges,
                                    attr_theme = NULL) %>%
    DiagrammeR::add_global_graph_attrs(
      attr_type = "graph",
      attr  = c("layout", "rankdir"),
      value = c("dot", "LR")
    ) %>%
    DiagrammeR::add_global_graph_attrs(
      attr_type = "node",
      attr  = c("color", "style", "fontname"),
      value = c("DimGray", "filled", "Helvetica")
    ) %>%
    DiagrammeR::add_global_graph_attrs(
      attr_type = "edge",
      attr  = c("color", "arrowsize", "arrowhead", "fontname"),
      value = c("DimGray", "1.5", "vee", "Helvetica")
    )
  # render the graph
  DiagrammeR::render_graph(graph)
}

lgbm_plots <- function(lgbm_model, lgbm_test_data_pred) {
  shap_data <-
    shapviz(object = lgbm_model, X_pred = lgbm_test_data_pred)

  WF <- sv_waterfall(shap_data, row_id = 1)
  SF <- sv_force(shap_data)
  SI <- sv_importance(shap_data, kind = "beeswarm", groupOnX = TRUE)
  # SD <- sv_dependence(shap_data, v = "eig5", "auto")

  TR <- lgb.plot.tree(lgbm_model, tree = 0)
  return(lst(WF, SF, SI, TR))
}

# File management ---------------------------------------------------------
# All functions related to folder and file creation, deletion or import
init_folder <- function(folder_list, folder_root = NULL) {
  if (is.null(folder_root)) {
    folder_root <- "./"
  }
  stopifnot(
    "Error in root folder format (you probably forgot the \"/\" at the end of your path)" =
      str_ends(string = folder_root, pattern = "/")
  )
  for (my_folder in folder_list) {
    if (!dir.exists(paste0(folder_root, my_folder))) {
      dir.create(paste0(folder_root, my_folder))
    }
  }
}

# Export ------------------------------------------------------------------
# Export data functions
lgbm_export <-
  function(study,
           my_date,
           name_seq,
           lgbm_model_results) {
    saveRDS(
      object = as.list(study[["best_params"]]),
      file = paste0("Output/Model_",
                    my_date,
                    "/Best_params_",
                    name_seq,
                    ".rds")
    )
    saveRDS(
      object = lgbm_model_results,
      file = paste0(
        "Output/Model_",
        my_date,
        "/LightGBM_model_",
        name_seq,
        ".rds"
      )
    )
    saveRDS(
      object = study,
      file = paste0("Output/Model_",
                    my_date,
                    "/Optune_study_",
                    name_seq,
                    ".rds")
    )
  }
