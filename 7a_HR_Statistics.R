# Setup -------------------------------------------------------------------


  rm(list = ls())
  library(magrittr)
  
  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  setwd()
  
  source("zz_Internal.R")

  library(gtsummary)

  library(ggplot2)

  readRDS("5b_Results.rds") %>%
  list2env(globalenv())

  features <- readRDS("4c_Features.rds")


# Statistics --------------------------------------------------------------


  lm_table <-
    paste0("m", 1:4) %>%
    purrr::map_df(
      ~scatter_prep(.x) %>%
      lm(obs_mean ~ pred_mean, .) %>%
      to_df(.),
      .id = "model"
    )

  paste0("m", 1:4) %>%
  purrr::map_df(
    ~ ba_prep(.) %>%
      get_ba(.),
    .id = "model"
  ) %>%
  dplyr::mutate(
    mean_loa = paste0(
      local_format(mean_bias, 1),
      " (", local_format(loa_lower, 1), ", ",
      local_format(loa_upper, 1), ")"
    )
  ) %>%
  dplyr::select(model, mean_loa, loa_width) %>%
  merge(lm_table, .) %>%
  dplyr::mutate(model = paste0("m", model)) %T>%
  saveRDS("7b_stats.rds") %>%
  dplyr::select(-c(Estimate:high, p)) %>%
  tidyr::pivot_wider(
    names_from = parameter,
    values_from = c(b_ci, sig)
  ) %>%
  dplyr::relocate(!r2:loa_width) %>%
  flextable::regulartable(.) %>%
  flextable::set_table_properties("fixed") %>%
  flextable::set_header_labels(
    model = "", b_ci_Intercept = "Intercept,\nb (95% CI)",
    b_ci_Slope = "Slope,\nb (95% CI)", sig_Intercept = "Intercept,\nP",
    sig_Slope = "Slope,\nP", r2 = "R2", mean_loa = "Mean Bias (LOA)",
    loa_width = "LOA Width"
  ) %>%
  flextable::width(2:8, 1.5, "in") %>%
  flextable::save_as_docx(
    path = "7c_stats.docx",
    pr_section = officer::prop_section(officer::page_size(orient = "landscape"))
  )
