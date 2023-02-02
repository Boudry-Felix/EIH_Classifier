# Informations ------------------------------------------------------------
# Title: Import.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Import the data needed to create a model of the EIH phenomenon.

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
## List of used libraries.
require(tidyverse)
require(readxl)
require(data.table)
require(janitor)
require(fs)
require(stringi)

## Global vectors ---------------------------------------------------------
## Define vectors used in entire script.
rm(list = setdiff(x = ls(), y = lsf.str())) # Clean environment
my_data <- lst() # Create a list for data
my_data_infos <-
  data.table() # Create a table for informations about subjects

# Importation -------------------------------------------------------------
# Import and structure the data that will be used in other scripts.
studies_list <-
  dir_info(path = "Data", recurse = FALSE) %>% # List directories
  filter(type == "directory") %>%
  "[["("path")

for (my_study in studies_list) {
  file_list <-
    # List all files in "my_study" directory
    dir_info(path = my_study, recurse = TRUE) %>%
    filter(type == "file") %>%
    "[["("path")
  subject_informations <-
    # Import subjects informations and clean column names
    fread(file = grep(
      pattern = ".*subject.*",
      x = file_list,
      value = TRUE
    )) %>%
    clean_names()
  test_informations <-
    # Import test informations and clean column names
    fread(file = grep(
      pattern = ".*tests.*",
      x = file_list,
      value = TRUE
    )) %>%
    clean_names()
  data_list <-
    # Importing stress test data
    grep(pattern = ".xlsx",
         x = file_list,
         value = TRUE) %>%
    sapply(
      FUN = read_excel,
      .name_repair = "minimal",
      na = c("", " ", "NA"),
      col_type = "numeric",
      simplify = FALSE,
      USE.NAMES = TRUE
    ) %>%
    lapply(clean_names, sep_out = "")
  data_infos <- subject_informations %>% # Merge informations
    append(values = test_informations[1,])
  my_data <- append(x = my_data, values = data_list)
  my_data_infos <- rbind(x = my_data_infos, values = data_infos)
}

names(x = my_data) <-
  stri_replace_all_regex(
    str = names(x = my_data),
    pattern = c(".*/", ".xlsx"),
    replacement = "",
    vectorize_all = FALSE
  )
remove_names <-
  setdiff(x = names(x = my_data), y = my_data_infos$subject) %>%
  append(values = setdiff(x = my_data_infos$subject, y = names(x = my_data)))
my_data <- my_data[names(x = my_data) %in% remove_names == FALSE]
my_data_infos <-
  my_data_infos[my_data_infos$subject %in% remove_names == FALSE]

# Data structure ----------------------------------------------------------
# Structure all data in a list
my_data <- lst(my_data, my_data_infos) %>%
  `names<-`(c("all", "infos"))
# Remove variables not containing "my_data" & my_data_frame
rm(list = setdiff(x = ls(), y = c(lsf.str(), ls(pattern = "my_data$"))))

# Export data -------------------------------------------------------------
if (!dir.exists("./Environments")) {
  dir.create("./Environments")
}
save.image(file = "./Environments/import.RData")
