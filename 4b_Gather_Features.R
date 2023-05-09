# Setup -------------------------------------------------------------------


  rm(list = ls())
  library(magrittr)
  
  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  setwd()


# Execute -----------------------------------------------------------------


  list.files("data-raw/8s_epochs", full.names = TRUE) %>%
  purrr::map_df(readRDS) %>%
  saveRDS("4c_Features.rds")
