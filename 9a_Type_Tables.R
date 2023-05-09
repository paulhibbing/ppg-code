# Setup -------------------------------------------------------------------


  rm(list = ls())
  library(magrittr)
  
  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  setwd()

  source("zz_Internal.R")

  readRDS("8b_Results.rds") %>%
  list2env(globalenv())

  d <-
    results %>%
    purrr::map_df(`[[`, "pred", .id = "model")
  
  ## Following sections are in reverse order
  ## to avoid having to reformat the dataset

  
# Confusion matrices ------------------------------------------------------
  
  
  d %>%
  dplyr::group_split(model) %>%
  purrr::map_df(function(x) {
    xtabs(~pred+obs, x) %>%
    data.frame(model = unique(x$model), .) %>%
    tidyr::pivot_wider(names_from = obs, values_from = Freq) %>%
    dplyr::mutate(pred = as.character(pred)) %>%
    rbind("", .)
  }) %>%
  flextable::regulartable(.) %>%
  flextable::save_as_docx(
    path = "9c_Confusion_Matrix.docx",
    pr_section = officer::prop_section(
      officer::page_size(orient = "landscape")
    )
  )
  
  
# 10-fold CV summary table ------------------------------------------------


  d %<>%
    dplyr::group_split(Resample, model, .keep = TRUE) %>%
    purrr::map_df(cm, .id = "fold")


  top <-
    d %>%
    dplyr::filter(activity == "Overall") %>%
    tidyr::pivot_wider(names_from = name, values_from = value) %>%
    dplyr::group_by(activity, model) %>%
    dplyr::summarise(
      accuracy = paste0(
        local_format(mean(accuracy)*100, 1), "% \u00B1 ",
        local_format(sd(accuracy)*100, 1), "%"
      ),
      kappa = paste0(
        local_format(mean(kappa), 2), " \u00B1 ",
        local_format(sd(kappa), 2)
      ),
      .groups = "drop"
    ) %>%
    reshape2::recast(...~model, id.var = c("activity", "model"))


  bottom <-
    d %>%
    dplyr::filter(activity != "Overall") %>%
    dplyr::mutate(activity = factor_activity(activity, .activity_labels)) %>%
    tidyr::pivot_wider(names_from = name, values_from = value) %>%
    dplyr::group_by(activity, model) %>%
    dplyr::summarise(dplyr::across(
      where(is.numeric),
      ~ paste0(
        local_format(mean(.x)*100, 1), "% \u00B1 ",
        local_format(sd(.x)*100, 1), "%"
      )
    ), .groups = "drop") %>%
    reshape2::recast(...~model, id.var = c("activity", "model"))


  rbind(top, bottom) %>%
  flextable::regulartable(.) %>%
  flextable::save_as_docx(path = "9b_10-fold_Results.docx")
