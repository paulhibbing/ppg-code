# Setup -------------------------------------------------------------------


  rm(list = ls())
  library(magrittr)
  
  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  setwd(.)

  
# Download the ZIP archive ------------------------------------------------

    
  data_link <- file.path(
    "https://physionet.org/static/published-projects",
    "wrist/wrist-ppg-during-exercise-1.0.0.zip"
  )
  
  zip_file <- "data-raw/physionet.zip"
  
  download.file(data_link, zip_file)
  

# Extract the contents ----------------------------------------------------
  
  
  unzip(zip_file, exdir = "data-raw/physionet_wrist")
  
  contents <- list.files(
    "data-raw/physionet_wrist",
    recursive = TRUE,
    full.names = TRUE
  )
  
  basename(contents) %>%
  file.path("data-raw/physionet_wrist", .) %>%
  file.rename(contents, .)
  
  
# Clean up after the above messy process ----------------------------------
  
  
  unlink(
    "data-raw/physionet_wrist/wrist-ppg-during-exercise-1.0.0",
    recursive = TRUE
  )
  
  file.remove(zip_file)
  