# Informations ------------------------------------------------------------
# Title: Functions.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Functions used for this project


# Tables ------------------------------------------------------------------
my_table <- function(input, ...) {
  # Custom kable table
  kable(x = input, ... = ...) %>%
    kable_styling(bootstrap_options = c("striped"),
                  full_width = FALSE)
}
