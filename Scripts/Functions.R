# Informations ------------------------------------------------------------
# Title: Functions.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: GPLv3
# Description: Functions used for this project

# Libraries ---------------------------------------------------------------
require(magrittr)

# Formats -----------------------------------------------------------------
my_table <- function(input, ...) {
  # Custom kable table
  kableExtra::kable(x = input, ... = ...) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped"),
                              full_width = FALSE)
}

# Computations ------------------------------------------------------------
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
  convert_dic <- dplyr::lst()
  if (is.numeric(x = my_col)) {
    my_col
  } else {
    label <- CatEncoders::LabelEncoder.fit(y = my_col)
    convert_dic <<- append(x = convert_dic, values = label)
    CatEncoders::transform(enc = label, my_col)
  }
}

df_encode <- function(input = my_data, list_names) {
  # Encode (labeling) entire data frames
  encoded_data <- lapply(X = input,
                         FUN = col_encode) %>% as.data.frame()
  output <- dplyr::lst(convert_dic, encoded_data)
  if (!missing(x = list_names)) {
    names(output) <- list_names
  }
  return(output)
}

common_col <- function(df_list) {
  cleaned_df <- lapply(X = df_list, FUN = colnames) %>%
    Reduce(f = intersect) %>%
    lapply(X = df_list, FUN = "[", .)
  return(cleaned_df)
}

my_summary <- function(df_list, df_names) {
  mapply(
    FUN = function(df_input, name_input)
      dplyr::summarise(
        df_input,
        dplyr::across(
          # Compute.fns for each column
          .cols = everything(),
          # Columns to compute
          .fns = list(
            # Functions to apply on columns
            mean = \(x) base::mean(x = x, na.rm = TRUE),
            max = \(x) base::max(x = x, na.rm = TRUE),
            min = \(x) base::min(x = x, na.rm = TRUE),
            median = \(x) stats::median(x = x, na.rm = TRUE)
          ),
          .names = "{.col}_{.fn}"
        )
      ) %>%
      cbind("subject" = name_input),
    df_input = df_list,
    name_input = names(df_names)
  ) %>%
    t() %>%
    as.data.frame() %>%
    tidyr::unnest(cols = colnames(x = .)) %>%
    merge(y = infos, by = "subject", all = TRUE) %>%
    dplyr::select(-any_of(
      c(
        "train_years",
        "data_type",
        "type",
        "environment",
        "intensity"
      )
    ))
}

# Clusters ----------------------------------------------------------------
optimal_clust <- function(input_data, cluster_method) {
  # Choose number of cluster for analysis
  elbow_graph <-
    factoextra::fviz_nbclust(input_data, cluster_method, method = "wss") +
    ggplot2::labs(subtitle = "Elbow method") +
    ggplot2::ggtitle(label = "Optimal number of cluster")
  silhouette_graph <-
    factoextra::fviz_nbclust(input_data, cluster_method, method = "silhouette") +
    ggplot2::labs(subtitle = "Silhouette method") +
    ggplot2::ggtitle(label = "Optimal number of cluster")
  gap_graph <-
    factoextra::fviz_nbclust(
      input_data,
      cluster_method,
      nstart = 25,
      method = "gap_stat",
      nboot = 50
    ) +
    ggplot2::labs(subtitle = "Gap statistic method") +
    ggplot2::ggtitle(label = "Optimal number of cluster")

  cluster_number_graph <- # Put graphs in list
    dplyr::lst(elbow_graph, silhouette_graph, gap_graph)
  names(x = cluster_number_graph) <- c("elbow", "silhouette", "gap")
  return(cluster_number_graph)
}

boxplots_by_clust <- function(data_col, cluster_col) {
  ggplot2::ggplot(plot_df, aes(x = !!sym(cluster_col), y = !!sym(paste(data_col)))) +
    ggplot2::geom_boxplot(aes(
      group = !!sym(cluster_col),
      fill = as.factor(!!sym(cluster_col))
    )) +
    ggplot2::ggtitle(label = paste(data_col, "by cluster")) +
    ggplot2::labs(fill = cluster_col)
}

# GBM ---------------------------------------------------------------------
gbm_data_partition <- function(input, sep_col, sep_prop) {
  split_indexes <- # Separate data in two using p
    caret::createDataPartition(y = input[[sep_col]], p = sep_prop, list = FALSE)
  train_data <- # Create a train data set
    input[split_indexes,]
  test_data <- # Create a test data set
    input[-split_indexes,]
  return(dplyr::lst(train_data, test_data))
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
  dt <- lightgbm::lgb.model.dt.tree(model)
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
      sapply(base::strsplit(as.character(x), '||', fixed = TRUE), function(el) {
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
    result <- base::strsplit(x, '||', fixed = TRUE)
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
    shapviz::shapviz(object = lgbm_model, X_pred = lgbm_test_data_pred)

  WF <- shapviz::sv_waterfall(shap_data, row_id = 1)
  SF <- shapviz::sv_force(shap_data)
  SI <- shapviz::sv_importance(shap_data, kind = "beeswarm")
  # SD <- sv_dependence(shap_data, v = "eig5", "auto")

  TR <- lgb.plot.tree(lgbm_model, tree = 0)
  return(dplyr::lst(WF, SF, SI, TR))
}

# File management ---------------------------------------------------------
# All functions related to folder and file creation, deletion or import
project_import <- function(project_path) {
  imported_data <- dplyr::lst() # Create a list for data
  imported_data_infos <-
    data.table::data.table() # Create a table for informations about samples
  studies_list <-
    fs::dir_info(path = paste(project_path), recurse = FALSE) %>%
    dplyr::filter(type == "directory") %$%
    path

  for (my_study in studies_list) {
    information_files_list <-
      fs::dir_info(path = my_study, recurse = TRUE) %>%
      dplyr::filter(type == "file") %$%
      path %>%
      grep(pattern = "Informations",
           x = .,
           value = TRUE)

    subject_informations <-
      # Import subjects informations and clean column names
      data.table::fread(file = grep(
        pattern = "Informations_subjects.*",
        x = information_files_list,
        value = TRUE
      )) %>%
      janitor::clean_names()
    test_informations <-
      # Import test informations and clean column names
      data.table::fread(file = grep(
        pattern = "Informations_tests.*",
        x = information_files_list,
        value = TRUE
      )) %>%
      janitor::clean_names()
    data_infos <- subject_informations %>% # Merge informations
      append(x = ., values = test_informations[1, ]) %>%
      data.table::as.data.table()
    files_list <- fs::dir_info(my_study, recurse = TRUE) %>%
      dplyr::filter(type == "file")
    files_list <- files_list$path
    files_list <-
      grep(pattern = ".xlsx",
           x = files_list,
           value = TRUE)
    # Importing data
    my_study <-
      gsub(pattern = project_path,
           replacement = "",
           x = my_study)
    my_list <-
      sapply(
        files_list,
        readxl::read_excel,
        .name_repair = "minimal",
        na = c("", " ", "NA"),
        col_type = "numeric",
        simplify = FALSE,
        USE.NAMES = TRUE
      ) %>%
      lapply(janitor::clean_names, sep_out = "")
    imported_data <- append(x = imported_data, values = my_list)
    imported_data_infos <-
      rbind(x = imported_data_infos, values = data_infos)
  }
  names(imported_data) <-
    gsub(
      pattern = paste0(project_path, ".*/.*/"),
      replacement = "",
      x = names(imported_data)
    ) %>%
    gsub(pattern = ".xlsx",
         replacement = "")
  remove_names <-
    setdiff(names(imported_data), imported_data_infos$subject) %>%
    append(setdiff(imported_data_infos$subject, names(imported_data)))
  imported_data <-
    imported_data[names(imported_data) %in% remove_names == FALSE]
  imported_data_infos <-
    imported_data_infos[imported_data_infos$subject %in% remove_names == FALSE]
  return(dplyr::lst("data" = imported_data, "infos" = imported_data_infos))
}

import_data <- function() {
  my_date <- format(Sys.time(), "%Y-%m-%d_%H.%M")

  imported_data <-
    project_import(project_path = easycsv::choose_dir())

  infos <- imported_data$infos
  my_data <- imported_data$data %>%
    common_col()

  my_colnames <- colnames(my_data[[1]])

  summary_simple <- my_summary(my_data, my_data)
  summary_relative <- compute_relative(summary_simple)

  keeped_rows <- summary_simple %>%
    as.data.frame() %>%
    dplyr::select_if(is.numeric) %>%
    na.omit() %>%
    rownames()

  PCA_summary <-
    lapply(my_data[keeped_rows %>% as.numeric()], missMDA::imputePCA) %>%
    lapply(FUN = as.data.frame) %>%
    lapply(FUN = dplyr::select, contains(my_colnames)) %>%
    lapply(FUN = FactoMineR::PCA, graph = FALSE) %>%
    lapply(FUN = "[", "eig") %>%
    lapply(FUN = unlist) %>%
    do.call(what = rbind, args = .) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(.data = ., var = "subject")


  # Cleaning ----------------------------------------------------------------
  # Removes outliers in summaries
  my_summaries <-
    dplyr::lst(summary_simple, summary_relative, PCA_summary) %>%
    `names<-`(value = c("absolute", "relative", "PCA"))

  # Labelling ---------------------------------------------------------------
  encoded_summaries <- lapply(X = my_summaries, FUN = df_encode)

  dir.create(path = "Data")
  rio::export_list(x = my_summaries, file = "Data/summary_%s.csv")
}

# Export ------------------------------------------------------------------
# Export data functions
lgbm_export <-
  function(study,
           # name_seq,
           lgbm_model_results) {
    saveRDS(
      object = as.list(study[["best_params"]]),
      file = paste0("Output/",
                    analysis_date,
                    "/params/Bast_params",
                    ".rds")
    )
    saveRDS(
      object = lgbm_model_results,
      file = paste0("Output/",
                    analysis_date,
                    "/params/LightGBM_model",
                    ".rds")
    )
    saveRDS(
      object = study,
      file = paste0("Output/",
                    analysis_date,
                    "/params/Optuna_study",
                    ".rds")
    )
  }
