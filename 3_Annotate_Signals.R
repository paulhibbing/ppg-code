# Setup -------------------------------------------------------------------


  rm(list = ls())
  library(magrittr)
  
  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  setwd()


# Run the annotation ------------------------------------------------------

  
  files <- list.files("data-raw/physionet_wrist", "hea$", full.names = TRUE)
  
  for (f in files) {
    # f <- files[1]
  
    cat("\nProcessing", basename(f), "\n")
  
    rds_out <-
      gsub("hea$", "rds", f) %>%
      {file.path(dirname(.), "rds", basename(.))}
  
    if (file.exists(rds_out)) {
      message("Skipping (already processed)")
      next
    }
  
    d <- try(
      wfdb::wfdb_wrap(f),
      TRUE
    )
  
    if (inherits(d, "try-error")) {
      warning(
        "`wfdb_wrap` failed on ", basename(f),
        " (#", match(f, files), ")", call. = FALSE
      )
      next
    }
  
    saveRDS(d, rds_out)

    rm(f)
    gc()
  
  }
  
  ## Warning message:
  # Removing 1041 row(s) from `hea` data for “s3_walk.hea” because it/they
  #   fall(s) outside the 2-second halo around the range of `atr`
