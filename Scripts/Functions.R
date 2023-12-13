# Informations ------------------------------------------------------------
# Title: Functions.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: GPLv3
# Description: Functions used for this project

# Libraries ---------------------------------------------------------------
require(magrittr)

# Import and transform ----------------------------------------------------
summary_gen <- function() {
  imported_data <-
    project_import(project_path = easycsv::choose_dir())

  infos <- imported_data$infos
  my_data <- imported_data$data %>%
    common_col()

  # Compute ratios
  my_data <- lapply(my_data, \(x) {
    x <- cbind(x, `ve/vo2` = x$ve / x$vo2) %>%
      cbind(`ve/vco2` = x$ve / x$vco2) %>%
      cbind(`vco2/vo2` = x$vco2 / x$vo2) %>%
      cbind(`vo2/fc` = x$vo2 / x$fc)
    return(x)
  })

  my_colnames <- colnames(my_data[[1]])

  summary_simple <-
    my_summary(my_data, my_data) %>% missRanger::missRanger()
  summary_relative <-
    compute_relative(summary_simple, cols = c("vo2")) %>%
    missRanger::missRanger()

  colnames(summary_relative) <-
    paste0(colnames(summary_relative), "_rel")

  summary_full <-
    merge(
      summary_simple,
      summary_relative,
      by.x = "subject",
      by.y = "subject_rel",
      all.x = TRUE
    )

  keeped_rows <- summary_full %>%
    as.data.frame() %>%
    dplyr::select_if(is.numeric) %>%
    na.omit() %>%
    rownames()

  my_summaries <-
    dplyr::lst(summary_simple, summary_relative, summary_full) %>%
    `names<-`(value = c("absolute", "relative", "full"))

  # Labeling
  encoded_summaries <- lapply(X = my_summaries, FUN = df_encode)

  dir.create(path = "Data")
  rio::export_list(x = my_summaries, file = "Data/summary_%s.csv")
}

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
      data.table::fread(
        file = grep(
          pattern = "Informations_subjects.*",
          x = information_files_list,
          value = TRUE
        )
      ) %>%
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
            median = \(x) stats::median(x = x, na.rm = TRUE),
            sd = \(x) stats::sd(x = x, na.rm = TRUE)
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

compute_relative <- function(input, cols) {
  maximum_columns <-
    grep(pattern = cols,
         x = colnames(x = input),
         value = TRUE) %>%
    grep(pattern = "max",
         x = .,
         value = TRUE) # List max columns
  for (my_column in maximum_columns) {
    study_rel()
  }
  out_col <- grep(pattern = cols,
                  x = colnames(input),
                  value = TRUE)
  output <- input[, c("subject", out_col)]
  return(output)
}

study_rel <- function() {
  my_variable_name <- colnames(x = input[my_column]) %>%
    sub(pattern = "_max", replacement = "") # remove "_max" from column name
  max_colname <- paste0(my_variable_name, "_max")
  mean_colname <- paste0(my_variable_name, "_mean")
  median_colname <- paste0(my_variable_name, "_median")
  min_colname <- paste0(my_variable_name, "_min")
  sd_colname <- paste0(my_variable_name, "_sd")
  input[min_colname] <- # Compute minimum values as %
    100 * input[min_colname] / input[max_colname]
  input[mean_colname] <- # Compute mean values as %
    100 * input[mean_colname] / input[max_colname]
  input[median_colname] <- # Compute median values as %
    100 * input[median_colname] / input[max_colname]
  input[sd_colname] <- # Compute sd values as %
    100 * input[sd_colname] / input[max_colname]
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

data_read <- function(fun_params = params) {
  if (fun_params$data != "none") {
    imported_data_names <<-
      basename(fun_params$data) %>%
      file_path_sans_ext()
    imported_data <<-
      fun_params$data %>%
      fread(na.strings = c("NA", "na", "", "Inf")) %>%
      lst() %>%
      `names<-`(value = imported_data_names)
  } else if (fun_params$data_list != "none") {
    imported_data_names <<-
      fun_params$data_list %>%
      get_knit_param() %>%
      basename() %>%
      file_path_sans_ext()
    imported_data <<-
      lapply(
        X = fun_params$data_list %>%
          get_knit_param(),
        FUN = fread,
        na.strings = c("NA", "na", "", "Inf")
      ) %>%
      `names<-`(value = imported_data_names)
  } else {
    imported_data_names <<-
      fun_params$data_folder %>%
      list.files() %>%
      file_path_sans_ext()
    imported_data <<-
      list.files(path = fun_params$data_folder,
                 full.names = TRUE) %>%
      lapply(fread, na.strings = c("NA", "na", "", "Inf")) %>%
      `names<-`(value = imported_data_names)
  }
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

confusion_export <- function(my_pat) {
  # Generates a complete confusion matrix with various metrics
  my_result <-
    ls(pattern = paste0(my_pat, "_confusion"), envir = my_env) %>%
    get(envir = my_env)
  z <- lst()
  z$table <-
    my_result$table %>% as.table() %>% my_table(row.names = TRUE, caption = "Confusion matrix")
  z$overall <-
    my_result$overall %>% as.matrix() %>% my_table(caption = "Precision metrics")
  z$class <-
    my_result$byClass %>% as.matrix() %>% my_table(caption = "Metrics by class")
  walk(z, print)
}

confusion_list_export <- function(my_pat) {
  my_result <-
    ls(pattern = paste0(my_pat, "_confusion"), envir = my_env) %>%
    get(envir = my_env) %>%
    lapply(FUN = \(x) {
      z <- lst()
      z$table <-
        x$table %>% as.table() %>% my_table(row.names = TRUE, caption = "Confusion matrix")
      z$overall <-
        x$overall %>% as.matrix() %>% my_table(caption = "Precision metrics")
      z$class <-
        x$byClass %>% as.matrix() %>% my_table(caption = "Metrics by class")
      return(z)
    })
  for (confusion_result in my_result) {
    walk(confusion_result, print)
  }
}

# Compute -----------------------------------------------------------------
gbm_data_partition <- function(input, sep_col, sep_prop) {
  split_indexes <- # Separate data in two using p
    caret::createDataPartition(y = input[[sep_col]], p = sep_prop, list = FALSE)
  train_data <- # Create a train data set
    input[split_indexes,]
  test_data <- # Create a test data set
    input[-split_indexes,]
  return(dplyr::lst(train_data, test_data))
}

clean_dataset <- function(input) {
  output <- select(.data = input, -any_of(excluded_variables)) %>%
    # scale(x = .) %>%
    as.data.frame()
  output[output == 0] <- NA
  output[output == Inf] <- NA
  output <- select(.data = output, -nearZeroVar(output, freqCut = 99/1))
  return(output)
}

# Plots -------------------------------------------------------------------
## Clusters ---------------------------------------------------------------
boxplots_by_clust <- function(data_col, cluster_col, used_env) {
  ggplot2::ggplot(plot_df, aes(x = !!sym(cluster_col), y = !!sym(paste(data_col))), environment = used_env) +
    ggplot2::geom_boxplot(aes(
      group = !!sym(cluster_col),
      fill = as.factor(!!sym(cluster_col))
    )) +
    ggplot2::ggtitle(label = paste(data_col, "by cluster")) +
    ggplot2::labs(fill = cluster_col)
}

## GBM --------------------------------------------------------------------
lgb.plot.tree <- function(model = NULL,
                          tree = NULL,
                          rules = NULL) {
  ### Args:
  ###   model: LGBM booster
  ###   tree: id of the tree to plot

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

  WF <-
    shapviz::sv_waterfall(shap_data, row_id = 1) +
    ggtitle("Waterfall plot of used features")
  SF <- shapviz::sv_force(shap_data) +
    ggtitle("Force plot of used features")
  SI <- shapviz::sv_importance(shap_data, kind = "beeswarm") +
    ggtitle("Beeswarm plot of used features")

  return(dplyr::lst(WF, SF, SI))
}

# Export ------------------------------------------------------------------
# Export data functions
result_save <- function() {
  dir_copy(path = "./EIH_Modeling_Classification_files/figure-html/",
           new_path = paste0("Output/", analysis_date))
  save.image(file = paste0("./Output/", analysis_date, "/global.RData"))
}

lgbm_export <-
  function(#study,
    # name_seq,
    lgbm_model_results) {
    saveRDS(
      object = lgbm_model_results,
      file = paste0("Output/",
                    analysis_date,
                    "/params/lgbm_model",
                    ".rds")
    )
    if (exists("study_lgbm", envir = compute_env)) {
      saveRDS(
        object = as.list(compute_env$study_lgbm[["best_params"]]),
        file = paste0("Output/",
                      analysis_date,
                      "/params/lgbm_best_params",
                      ".rds")
      )
      saveRDS(
        object = compute_env$study_lgbm,
        file = paste0("Output/",
                      analysis_date,
                      "/params/lgbm_optuna_study",
                      ".rds")
      )
    }
  }

xgboost_export <-
  function(#study,
    # name_seq,
    xgboost_model_results) {
    saveRDS(
      object = xgboost_model_results,
      file = paste0("Output/",
                    analysis_date,
                    "/params/xgboost_model",
                    ".rds")
    )
    if (exists("study_xgboost", envir = compute_env)) {
      saveRDS(
        object = as.list(compute_env$study_xgboost[["best_params"]]),
        file = paste0("Output/",
                      analysis_date,
                      "/params/xgboost_best_params",
                      ".rds")
      )
      saveRDS(
        object = compute_env$study_xgboost,
        file = paste0("Output/",
                      analysis_date,
                      "/params/xgboost_optuna_study",
                      ".rds")
      )
    }
  }
