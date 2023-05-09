# Setup -------------------------------------------------------------------


  rm(list = ls())
  library(magrittr)
  
  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  setwd()

  source("zz_Internal.R")

  
# Implementation ----------------------------------------------------------

  
  files <-
    file.path("data-raw/physionet_wrist/rds") %>%
    list.files("rds$", full.names = TRUE)

  for (f in files) {

    out_file <-
      basename(f) %>%
      file.path("data-raw/8s_epochs", .)

    if (file.exists(out_file)) {
      warning("Skipping ", basename(f), " (already processed)", call. = FALSE)
      next
    }

    print(paste0(
      "File ", match(f, files), " of ",
      length(files), " (", basename(f), ")"
    ))

    d <- try(
      read_format(f, verbose = TRUE),
      TRUE
    )

    if (inherits(d, "try-error")) {
      warning("Error for ", basename(f), call. = FALSE)
      next
    }

    saveRDS(d, out_file)

  }
