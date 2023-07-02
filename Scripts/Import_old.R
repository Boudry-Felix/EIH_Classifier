# Informations ------------------------------------------------------------
# Title: Import.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: GPLv3
# Description: Import the data needed to create a model of the EIH phenomenon.

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
## List of used libraries.
require(tidyverse)
require(readxl)
require(data.table)
require(janitor)
require(fs)

## Global vectors ---------------------------------------------------------
## Define vectors used in entire script.
rm(list = setdiff(x = ls(), y = lsf.str())) # Clean environment
my_data <- lst() # Create a list for data
my_data_infos <-
  data.table() # Create a table for informations about subjects

# Importation -------------------------------------------------------------
# Import and structure the data that will be used in other scripts.


names(my_data) <-
  gsub(pattern = "Data/.*/.*/",
       replacement = "",
       x = names(my_data)) %>%
  gsub(pattern = ".xlsx",
       replacement = "")
remove_names <- setdiff(names(my_data), my_data_infos$subject) %>%
  append(setdiff(my_data_infos$subject, names(my_data)))
my_data <- my_data[names(my_data) %in% remove_names == FALSE]
my_data_infos <-
  my_data_infos[my_data_infos$subject %in% remove_names == FALSE]
# Remove variables not containing "my_data" & my_data_frame
rm(list = setdiff(ls(), c(lsf.str(), ls(pattern = "my_data"))), my_data_frame)

# Data structure ----------------------------------------------------------
# Structure all data in a list
my_data <- lst(my_data, my_data_infos) %>%
  `names<-`(c("all", "infos"))
rm(my_data_infos)

# Export data -------------------------------------------------------------
if (!dir.exists("./Environments")) {
  dir.create("./Environments")
}
save.image(file = "./Environments/import.RData")
