# Informations ------------------------------------------------------------
# Title: run.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: GPLv3
# Description: Run this file to execute all code and generate a report. Used to
# knit the Rmd file while having an interactive session

analysis_date <- format(Sys.time(), "%Y-%m-%d_%H.%M")
fs::dir_create(path = paste0("Output/", analysis_date))

knitr::knit(input = "EIH_Modeling_Classification.Rmd",
            output = paste0("Output/", analysis_date, "/Report.md"))

save.image(file = paste0("./Output/", analysis_date, "/global.RData"))
