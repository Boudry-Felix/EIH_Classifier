# Informations ------------------------------------------------------------
# Title: run.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: GPLv3
# Description: Run this file to execute all code and generate a report. Used to
# knit the Rmd file while having an interactive session

library(magrittr)

report <-
  tcltk::tkmessageBox(type = "yesno",
                      message = "Generate a report?",
                      default = "no") %>%
  tcltk::tclvalue() %>%
  easyr::tobool()# Choose to render a report

analysis_date <- format(Sys.time(), "%Y-%m-%d_%H.%M")
fs::dir_create(path = paste0("Output/", analysis_date))

ifelse(report,
       knitr::knit(
         input = "EIH_Modeling_Classification.Rmd",
         output = paste0("Output/", analysis_date, "/Report.md")
       ),
       {
         knitr::purl("EIH_Modeling_Classification.Rmd") %>% source()
         file_delete(path = "EIH_Modeling_Classification.R")
       })

save.image(file = paste0("./Output/", analysis_date, "/global.RData"))
