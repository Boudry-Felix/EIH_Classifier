# Informations ------------------------------------------------------------
# Title: Functions.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: GPLv3
# Description: Functions used for this project

# Libraries ---------------------------------------------------------------
require(magrittr)

# Import ------------------------------------------------------------------
summary_gen <- function() {
  imported_data <- # Import all data and it's informations
    project_import(project_path = easycsv::choose_dir())

  summary_abs <- imported_data$data %>%
    common_col() # Keeping only common columns

  # Compute ratios
  summary_abs <- lapply(summary_abs, \(x) {
    x <- cbind(x, `ve/vo2` = x$ve / x$vo2) %>%
      cbind(`ve/vco2` = x$ve / x$vco2) %>%
      cbind(`vco2/vo2` = x$vco2 / x$vo2) %>%
      cbind(`vo2/hr` = x$vo2 / x$hr)
    return(x)
  })

  summary_abs <-
    my_summary(summary_abs, summary_abs, infos_df = imported_data$infos)

  dir.create(path = "Data")
  write.csv(x = summary_abs, file = "Data/summary.csv")
}

project_import <- function(project_path) {
  imported_data <- dplyr::lst() # Create a list for data
  imported_data_infos <-
    data.table::data.table() # Create a table for informations about samples
  studies_list <- # Lists the folders corresponding to projects
    fs::dir_info(path = paste(project_path), recurse = FALSE) %>%
    dplyr::filter(type == "directory") %$%
    path

  for (my_study in studies_list) {
    information_files_list <- # List informations files
      fs::dir_info(path = my_study, recurse = TRUE) %>%
      dplyr::filter(type == "file") %$%
      path %>%
      grep(pattern = "Informations",
           x = .,
           value = TRUE)

    subject_informations <-
      # Get subjects informations and clean column names
      data.table::fread(
        file = grep(
          pattern = "Informations_subjects.*",
          x = information_files_list,
          value = TRUE
        )
      ) %>%
      janitor::clean_names()
    test_informations <-
      # Get test informations and clean column names
      data.table::fread(file = grep(
        pattern = "Informations_tests.*",
        x = information_files_list,
        value = TRUE
      )) %>%
      janitor::clean_names()
    data_infos <- subject_informations %>% # Merge informations
      append(x = ., values = test_informations[1,]) %>%
      data.table::as.data.table()
    files_list <-
      fs::dir_info(my_study, recurse = TRUE) %>% # List all files
      dplyr::filter(type == "file")
    files_list <- files_list$path
    files_list <- # Keeping only xlsx files (data files)
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
        col_type = "numeric",
        simplify = FALSE,
        USE.NAMES = TRUE
      ) %>%
      lapply(janitor::clean_names, sep_out = "")
    imported_data <- append(x = imported_data, values = my_list)
    imported_data_infos <-
      rbind(x = imported_data_infos, values = data_infos)
  }
  names(imported_data) <- # Renaming objects
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
  df_list <- lapply(X = df_list, \(x) {
    input_col <- colnames(x = x)
    output_col <-
      stringi::stri_replace_all_regex(
        str = input_col,
        pattern = c("Rf|rf|fr|Fr|FR|f_r", "Hr|hr|FC|fc|Fc|f_c"),
        replacement = c("RF", "HR"),
        vectorize = FALSE
      )
    colnames(x = x) <- output_col
    x <- janitor::clean_names(x)
    return(x)
  })
  cleaned_df <-
    lapply(X = df_list, FUN = colnames) %>% # Find columns present in all df
    Reduce(f = intersect) %>%
    lapply(X = df_list, FUN = "[", .)
  return(cleaned_df)
}

my_summary <- function(df_list, df_names, infos_df = infos) {
  mapply(
    FUN = function(df_input, name_input)
      dplyr::summarise(
        df_input,
        dplyr::across(
          # Compute .fns for each column
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
    merge(y = infos_df, by = "subject", all = TRUE)
}

data_read <- function(fun_params = params) {
  if (fun_params$data != "none") {
    imported_data_names <<-
      basename(fun_params$data) %>%
      file_path_sans_ext()
    imported_data <<-
      fun_params$data %>%
      fread(na.strings = c("NA", "na", "", "Inf", "-Inf")) %>%
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
        na.strings = c("NA", "na", "", "Inf", "-Inf")
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
      lapply(fread, na.strings = c("NA", "na", "", "Inf", "-Inf")) %>%
      `names<-`(value = imported_data_names)
  }
}

# Pre-processing ----------------------------------------------------------
compute_new_features <- function(input) {
  vo2_colnames <- grep("^vo2_", colnames(input), value = TRUE)
  vo2_columns <- # List max columns
    vo2_colnames %>%
    lapply(\(x) {
      assign(x = paste0(x, "_rel"),
             value = input[[x]] / input[["weight"]])
    }) %>%
    as.data.frame() %>%
    `colnames<-`(value = paste0(vo2_colnames, "_rel"))

  hr_colnames <- grep("^hr_", colnames(input), value = TRUE)
  hr_columns <- # List max columns
    hr_colnames %>%
    lapply(\(x) {
      hr_theo <- 211 - (0.64 * input[["age"]])
      assign(x = paste0(x, "_rel"),
             value = input[[x]] * 100 / hr_theo)
    }) %>%
    as.data.frame() %>%
    `colnames<-`(value = paste0(hr_colnames, "_rel"))

  output <- cbind(input, vo2_columns) %>%
    cbind(hr_columns)

  return(output)
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
    dplyr::mutate(dplyr::across(dplyr::where(is.double), ~ tidyr::replace_na(., median(., na.rm =
                                                                                         TRUE)))) %>%
    dplyr::mutate(dplyr::across(
      dplyr::where(is.integer),
      ~ tidyr::replace_na(., median(., na.rm =
                                      TRUE) %>% as.integer())
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
  dt <- dt[tree_index == tree,]
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
  dt <- dt[nrow(dt):1,]
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
  function(lgbm_model_results) {
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
        file = paste0(
          "Output/",
          analysis_date,
          "/params/lgbm_best_params",
          ".rds"
        )
      )
      saveRDS(
        object = compute_env$study_lgbm,
        file = paste0(
          "Output/",
          analysis_date,
          "/params/lgbm_optuna_study",
          ".rds"
        )
      )
    }
  }

xgboost_export <-
  function(xgboost_model_results) {
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
        file = paste0(
          "Output/",
          analysis_date,
          "/params/xgboost_best_params",
          ".rds"
        )
      )
      saveRDS(
        object = compute_env$study_xgboost,
        file = paste0(
          "Output/",
          analysis_date,
          "/params/xgboost_optuna_study",
          ".rds"
        )
      )
    }
  }
