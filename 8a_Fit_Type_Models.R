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

  features <-
    readRDS("4c_Features.rds") %>%
    balance_features(.)

  rf_control <- caret::trainControl(
    method = "cv",
    number = 10,
    search = "grid",
    verboseIter = TRUE,
    returnData = TRUE,
    returnResamp = "final",
    savePredictions = "final",
    selectionFunction = "best",
    preProcOptions = NULL,
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
    variables = c("^vm_q", "^gvm_q", "ppg_hr"),
    ...,
    m = readRDS("5b_Results.rds")$results$m2,
    features = globalenv()$features,
    ctrl = globalenv()$rf_control
  ) {

    set.seed(1919)

    x <-
      features %>%
      dplyr::mutate(ppg_hr = predict(m, features)) %>%
      dplyr::select(dplyr::matches(variables))

    caret::train(
      x = x,
      y = features$activity,
      method = "rf",
      ...,
      replace = FALSE,
      importance = TRUE,
      do.trace = FALSE,
      keep.forest = TRUE,
      metric = "Accuracy",
      maximize = TRUE,
      trControl = ctrl,
      tuneGrid = data.frame(mtry = 2)
    )

  }


# Implementation ----------------------------------------------------------


  results <- list(

    m1 = run_caret("^vm_q",  ntree = 100),
    m2 = run_caret("^gvm_q", ntree = 100),
    m3 = run_caret("^g*vm_q", ntree = 100),
    m4 = run_caret(c("^g*vm_q", "ppg_hr"), ntree = 100)

  )

  info <- purrr::map_df(
    results, `[[`, "results", .id = "model"
  )


# Save results ------------------------------------------------------------


  list(results = results, info = info) %>%
  saveRDS("8b_results.rds")
