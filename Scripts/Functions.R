# Informations ------------------------------------------------------------
# Title: Functions.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Functions used for this project

# Libraries ---------------------------------------------------------------
require(kableExtra)
require(grDevices)

# Tables ------------------------------------------------------------------
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
            if (is.numeric(my_col)) {lapply(
              X = my_col,
              FUN = function(my_value)
                ifelse(
                  test = my_value %in% boxplot.stats(my_col)$out,
                  yes = NA,
                  no = as.numeric(my_value)
                )
            )}
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
