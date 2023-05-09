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


# Data summary tables -----------------------------------------------------


  #* Activity summaries

    features %>%
    dplyr::select(
      id, activity, vm_mean, gvm_mean, hr_ecg
    ) %>%
    dplyr::group_by(id, activity) %>%
    dplyr::summarise(
      duration = dplyr::n()*8/60,
      dplyr::across(everything(), mean, .names = "{.col}"),
      .groups = "drop"
    ) %>%
    tbl_summary(
      by = activity,
      include = !id,
      statistic = list(dplyr::everything() ~ "{mean} \u00B1 {sd}"),
      type = list(duration ~ "continuous"),
      digits = list(duration:gvm_mean ~ 1),
      label = list(
        duration ~ "Duration (min)",
        vm_mean ~ "Accelerometer Vector Magnitude (m/s/s)",
        gvm_mean ~ "Gyroscope Vector Magnitude (\u00B0/s)"
      )
    ) %>%
    modify_footnote(list(everything() ~ NA)) %>%
    as_flex_table(.) %>%
    flextable::set_table_properties("fixed") %>%
    flextable::save_as_docx(
      path = "6b1_Summaries.docx",
      pr_section = officer::prop_section(
        officer::page_size(orient = "landscape")
      )
    )


  #* Expanded summary

    d1 <-
      results %>%
      purrr::map_df(~ .x$pred, .id = "model") %>%
      dplyr::mutate(
        id = features$id[rowIndex],
        activity = features$activity[rowIndex]
      ) %>%
      dplyr::select(id:activity, model, pred, obs) %>%
      dplyr::group_by(id, activity, model) %>%
      dplyr::summarise(
        dplyr::across(everything(), mean, .names = "{.col}"),
        .groups = "drop"
      ) %>%
      tidyr::pivot_wider(names_from = model, values_from = pred)


    features %>%
    dplyr::select(-c(samp_rate:time)) %>%
    dplyr::group_by(id, activity) %>%
    dplyr::summarise(
      duration = dplyr::n()*8/60,
      dplyr::across(everything(), mean, .names = "{.col}"),
      .groups = "drop"
    ) %>%
    merge(d1) %>%
    dplyr::select(
      id, activity,
      dplyr::matches("^vm"),
      dplyr::matches("^acc"),
      dplyr::matches("^gvm"),
      dplyr::matches("^gyro"),
      dplyr::matches("^ppg"),
      -dplyr::matches("norm_spec"),
      obs:m4
    ) %>%
    tbl_summary(
      by = activity,
      include = -c(id, vm_mean:vm_sd, gvm_mean:gvm_sd),
      statistic = list(dplyr::everything() ~ "{mean} \u00B1 {sd}"),
      digits = list(
        everything() ~ 2,
        obs:m4 ~ 0
      ),
      label = list(

        vm_q10 ~ "VM 10th percentile (m/s/s)", vm_q25 ~ "VM 25th percentile (m/s/s)",
        vm_q50 ~ "VM 50th percentile (m/s/s)", vm_q75 ~ "VM 75th percentile (m/s/s)",
        vm_q90 ~ "VM 90th percentile (m/s/s)",
        acc_freq1 ~ "Accelerometer peak frequency (Hz)",
        acc_freq2 ~ "Accelerometer second peak frequency (Hz)",

        gvm_q10 ~ "GVM 10th percentile (\u00B0/s)", gvm_q25 ~ "GVM 25th percentile (\u00B0/s)",
        gvm_q50 ~ "GVM 50th percentile (\u00B0/s)", gvm_q75 ~ "GVM 75th percentile (\u00B0/s)",
        gvm_q90 ~ "GVM 90th percentile (\u00B0/s)",
        gyro_freq1 ~ "Gyroscope peak frequency (Hz)",
        gyro_freq2 ~ "Gyroscope second peak frequency (Hz)",

        ppg_freq1 ~ "PPG peak frequency (Hz)",
        ppg_freq2 ~ "PPG second peak frequency (Hz)",
        ppg_spec1 ~ "PPG peak density",
        ppg_spec2 ~ "PPG second peak density",

        obs ~ "ECG heart rate (beats/min)",
        m1 ~ "Model 1 predicted heart rate (beats/min)",
        m2 ~ "Model 2 predicted heart rate (beats/min)",
        m3 ~ "Model 3 predicted heart rate (beats/min)",
        m4 ~ "Model 4 predicted heart rate (beats/min)"

      )
    ) %>%
    modify_footnote(list(everything() ~ NA)) %>%
    as_flex_table(.) %>%
    flextable::set_table_properties("fixed") %>%
    flextable::save_as_docx(
      path = "6b2_Expanded_Summaries.docx",
      pr_section = officer::prop_section(
        officer::page_size(orient = "landscape")
      )
    )


# 10-fold CV summary table ------------------------------------------------


  info %>%
  dplyr::select(-mtry) %>%
  dplyr::mutate(
    dplyr::across(
      !dplyr::matches(c("model", "Rsquared")),
      ~ local_format(.x, 1)
    ),
    dplyr::across(
      dplyr::matches("Rsquared"),
      ~ local_format(.x, 2)
    )
  ) %>%
  tidyr::pivot_longer(!model) %>%
  dplyr::mutate(
    metric = ifelse(grepl("SD$", name), "sd", "mean"),
    name = gsub("SD$", "", name)
  ) %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  dplyr::mutate(value = paste0(mean, " \u00B1 ", sd)) %>%
  dplyr::select(!mean:sd) %>%
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  dplyr::relocate(MAE, RMSE, MAPE, .after = mean_bias) %>%
  flextable::regulartable(.) %>%
  flextable::set_table_properties("fixed") %>%
  flextable::width(1:6, 1.5) %>%
  flextable::set_header_labels(values = c(
    model = "", mean_bias = "Mean Bias (bpm)",
    MAE = "Mean Absolute Error (bpm)", RMSE = "Root Mean Squared Error (bpm)",
    MAPE = "Mean Absolute Percentage Error (%)", Rsquared = "R2"
  )) %>%
  flextable::save_as_docx(
    path = "6c_10-fold_Results.docx",
    pr_section = officer::prop_section(
      officer::page_size(orient = "landscape")
    )
  )


# 10-fold CV scatter plots ------------------------------------------------


  top <- cowplot::plot_grid(
    scatter("m1", legend.position = "none"),
    scatter("m2", legend.position = "none"),
    scatter("m3", legend.position = "none"),
    scatter("m4", legend.position = "none"),
    scale = 0.9, labels = "AUTO", label_size = 20
  )

  tiff(
    "6d_scatter.tif", 12, 10, "in",
    res = 1200, compression = "lzw"
  )

    cowplot::plot_grid(
      top,
      cowplot::get_legend(scatter("m1", legend.position = "bottom")),
      scale = 0.95,
      ncol = 1,
      rel_heights = c(1, 0.05)
    )

  dev.off()


# 10-fold CV BA plots -----------------------------------------------------


  top <- cowplot::plot_grid(
    ba("m1", legend.position = "none"),
    ba("m2", legend.position = "none"),
    ba("m3", legend.position = "none"),
    ba("m4", legend.position = "none"),
    scale = 0.9, labels = "AUTO", label_size = 20
  )

  tiff(
    "6e_ba.tif", 12, 8, "in",
    res = 1200, compression = "lzw"
  )

    cowplot::plot_grid(
      top,
      cowplot::get_legend(ba("m1", legend.position = "bottom")),
      scale = 0.95,
      ncol = 1,
      rel_heights = c(1, 0.05)
    )

  dev.off()
