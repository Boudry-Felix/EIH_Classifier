# Informations ------------------------------------------------------------
# Title: run.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: GPLv3
# Description: Run this file to execute all code and generate a report. Used to
# knit the Rmd file while having an interactive session

library(magrittr)

report <- # Choose to render a report
  tcltk::tkmessageBox(type = "yesno",
                      message = "Generate a report?",
                      default = "no") %>%
  tcltk::tclvalue() %>%
  easyr::tobool()

analysis_date <- format(Sys.time(), "%Y-%m-%d_%H.%M")
fs::dir_create(path = paste0("Output/", analysis_date))

ifelse(report, # Execute code with or without generating a report
       {
         knitr::knit(input = "EIH_Modeling_Classification.Rmd",
                     output = paste0("Output/", analysis_date, "/Report.md"))
         fs::dir_copy(path = "./figure/",
                      new_path = paste0("Output/", analysis_date))
       },
       {
         knitr::purl("EIH_Modeling_Classification.Rmd") %>% source()
         file_delete(path = "EIH_Modeling_Classification.R")
       })

fs::dir_delete(path = "./figure/")

save.image(file = paste0("./Output/", analysis_date, "/global.RData"))
