# Setup -------------------------------------------------------------------


  rm(list = ls())
  library(magrittr)
  
  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  setwd()


# Cleanup -----------------------------------------------------------------

  
  list.files(pattern = "^wfdb.*zip$", full.names = TRUE, recursive = TRUE) %>%
  file.remove(.)

  unlink(c("database", "mcode"), recursive = TRUE)
