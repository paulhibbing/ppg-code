# Setup -------------------------------------------------------------------


  rm(list = ls())
  library(magrittr)

  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  setwd()

  ## Order of attachment ensures proper masking and method dispatch
  ## (lots of red ink about to pop up if session is fresh)
  library(caret)
  library(randomForest)
  source("zz_Internal.R")

  features <- readRDS("4c_Features.rds")

  rf_control <- caret::trainControl(
    method = "cv",
    number = 10,
    search = "grid",
    verboseIter = TRUE,
    returnData = TRUE,
    returnResamp = "final",
    savePredictions = "final",
    summaryFunction = caret_summary,
    selectionFunction = "best",
    preProcOptions = NULL,
    predictionBounds = c(.min_hr, .max_hr),
    seeds = rep(
      list(rep(1919, 10)),
      ## ^^ Just put 10 to make sure it's enough -- See `trainControl` documentation
      840
    ),
    trim = TRUE,
    allowParallel = TRUE
  )


# Governing function ------------------------------------------------------

  
  run_caret <- function(
    sensors = c("ppg", "acc", "gyro"),
    variables = c("freq", "ppg_spec"),
    ...,
    features = globalenv()$features,
    ctrl = globalenv()$rf_control
  ) {

    set.seed(1919)

    x <-
      features %>%
      dplyr::select(dplyr::matches(variables)) %>%
      dplyr::select(dplyr::matches(sensors))

    caret::train(
      x = x,
      y = features$hr_ecg,
      method = "rf",
      ...,
      replace = FALSE,
      importance = TRUE,
      do.trace = FALSE,
      keep.forest = TRUE,
      metric = "RMSE",
      maximize = FALSE,
      trControl = ctrl,
      tuneGrid = data.frame(mtry = 2)
    )

  }


# Implementation ----------------------------------------------------------


  results <- list(

    m1 = run_caret("ppg",  ntree = 100),
    m2 = run_caret(c("ppg", "acc"), ntree = 100),
    m3 = run_caret(c("ppg", "gyro"), ntree = 100),
    m4 = run_caret(c("ppg", "acc", "gyro"), ntree = 100)

  )

  info <- purrr::map_df(
    results, `[[`, "results", .id = "model"
  )


# Save results and models -------------------------------------------------


  list(results = results, info = info) %>%
  saveRDS("5b_Results.rds")
