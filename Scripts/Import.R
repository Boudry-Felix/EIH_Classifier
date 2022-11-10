# Informations ------------------------------------------------------------

# Title: Import.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Import the data needed to create a model of the EIH phenomenon.

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
## List of used libraries.
require(readxl)
require(dplyr)
require(data.table)
require(janitor)
require(tibble)
require(fs)

## Global vectors ---------------------------------------------------------
## Define vectors used in entire script.
rm(list = ls()) # Clean environment
my_data <- lst() # Create a list for data
my_data_infos <-
  data.table() # Create a table for informations about subjects

# Importation -------------------------------------------------------------
# Import and structure the data that will be used in other scripts.
studies_list <- dir_info(path = "Data", recurse = FALSE) %>%
  filter(type == "directory")
studies_list <- studies_list$path

for (my_study in studies_list) {
  information_files_list <-
    dir_info(path = my_study, recurse = TRUE) %>%
    filter(type == "file")
  information_files_list <- information_files_list$path
  information_files_list <-
    grep(pattern = ".csv",
         x = information_files_list,
         value = TRUE)
  subject_informations <-
    # Import subjects informations and clean column names
    fread(file = grep(
      pattern = ".*subject.*",
      x = information_files_list,
      value = TRUE
    )) %>%
    clean_names()
  test_informations <-
    # Import test informations and clean column names
    fread(file = grep(
      pattern = ".*tests.*",
      x = information_files_list,
      value = TRUE
    )) %>%
    clean_names()
  data_infos <- subject_informations %>% # Merge informations
    append(x = ., values = test_informations[1,]) %>%
    as.data.table()
  files_list <- dir_info(my_study, recurse = TRUE) %>%
    filter(type == "file")
  files_list <- files_list$path
  files_list <-
    grep(pattern = ".xlsx",
         x = files_list,
         value = TRUE)
  # Importing data
  my_study <-
    gsub(pattern = "Data/",
         replacement = "",
         x = my_study)
  my_list <-
    sapply(
      files_list,
      read_excel,
      .name_repair = "minimal",
      na = c("", " ", "NA"),
      col_type = "numeric",
      simplify = FALSE,
      USE.NAMES = TRUE
    ) %>%
    lapply(clean_names, sep_out = "")

  my_data <- append(x = my_data, values = my_list)
  my_data_infos <- rbind(x = my_data_infos, values = data_infos)
  # Remove variables not containing "my_data" & my_data_frame
  rm(list = setdiff(ls(), ls(pattern = "my_data|clean.*")), my_data_frame)
}

names(my_data) <-
  gsub(pattern = "Data/.*/.*/",
       replacement = "",
       x = names(my_data))
names(my_data) <-
  gsub(pattern = ".xlsx",
       replacement = "",
       x = names(my_data))
remove_names <- setdiff(names(my_data), my_data_infos$subject) %>%
  append(setdiff(my_data_infos$subject, names(my_data)))
my_data <- my_data[names(my_data) %in% remove_names == FALSE]
my_data_infos <-
  my_data_infos[my_data_infos$subject %in% remove_names == FALSE]
# Remove variables not containing "my_data" & my_data_frame
rm(list = setdiff(ls(), ls(pattern = "my_data")), my_data_frame)

# Data structure ----------------------------------------------------------
# Structure all data in a list
my_data <- lst(my_data, my_data_infos) %>%
  `names<-`(c("all", "infos"))
rm(my_data_infos)

# Export data -------------------------------------------------------------
save.image(file = "./Environments/import.RData")
