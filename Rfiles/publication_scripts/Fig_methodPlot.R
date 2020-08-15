
### =======================================================
#### Additional plots for contact tracing simulations
### =======================================================

pdfdir <- "C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/project_notes/publications/covid_model_IL/pdfs_ais/"

#### Plots edited for publication
library(tidyverse)
library(cowplot)
library(scales)
library(viridis)
library(data.table)

theme_set(theme_cowplot())

# setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("ct_analysis/helper_functions_CT.R")
# region_cols <- c()
restoreRegion_cols <- c("Central" = "red2", "Northcentral" = "dodgerblue3", "Northeast" = "chartreuse4", "Southern" = "orchid4")

startdate <- "2020-06-15"
stopdate <- "2020-12-30"
reopen <- c(0, 0.05, 0.1)
customTheme <- f_getCustomTheme()
ct_startdate <- as.Date("2020-07-30")

## ================================================
###  Plot individual trajectories over time for method plot
## ================================================

methodPlot <- FALSE
if (methodPlot) {
  customTheme <- f_getCustomTheme(fontscl = 3)
  
  exp_name <- "20200731_IL_reopen_contactTracing"
  source("C:/Users/mrm9534/gitrepos/covid-chicago/Rfiles/ct_analysis/loadData_defineParam.R")
  
  selected_ems <- c(1:11)
  emsvars_temp <- c("critical_EMS.")
  emsvars <- NULL
  for (ems in selected_ems) {
    emsvars <- c(emsvars, paste0(emsvars_temp, ems))
  }
  
  groupvars <- c("startdate", "Date", "time", "scen_num", "sample_num", "run_num", "reopening_multiplier_4", "detection_success", "isolation_success", "grpvar")
  (keepvars <- c(groupvars, emsvars))
  
  ### Aggregate for all IL
  subdat <- trajectoriesDat %>%
    dplyr::select(keepvars) %>%
    pivot_longer(cols = -c(groupvars)) %>%
    dplyr::mutate(name = gsub("All", "EMS.IL", name)) %>%
    dplyr::mutate(name = gsub("[.]", "_", name)) %>%
    separate(name, into = c("outcome", "region"), sep = "_EMS_") %>%
    dplyr::filter(Date >= reopeningdate - 60 & Date <= as.Date("2021-01-15")) %>%
    dplyr::select(-c(time)) %>%
    filter(region == 11) %>%
    dplyr::group_by(startdate, region, Date, reopening_multiplier_4, scen_num, sample_num, run_num, detection_success, isolation_success, grpvar) %>%
    dplyr::summarize(value = sum(value))
  
  capacity <- load_capacity(unique(subdat$region)) %>%
    dplyr::rename(capacity = critical)
  
  subdat <- subdat %>%
    merge(capacity, by.x = "region", by.y = "geography_name") %>%
    filter(grpvar == 0.05)
  
  ##### SMooth, calculate weekly average
  subdatWklAvr_lines <- subdat %>%
    mutate(week = week(Date)) %>%
    group_by(week, scen_num, grpvar, detection_success, isolation_success, capacity) %>%
    summarize(
      value = mean(value),
      Date = max(Date)
    ) %>%
    ungroup() %>%
    group_by(scen_num, grpvar, capacity) %>%
    mutate(peak = max(value)) %>%
    f_valuefct()
  
  
  subdatWklAvr <- subdat %>%
    dplyr::filter(Date >= reopeningdate & Date <= as.Date("2021-02-01")) %>%
    mutate(week = week(Date)) %>%
    group_by(week, scen_num, grpvar, detection_success, isolation_success, capacity) %>%
    summarize(
      value = mean(value),
      Date = max(Date)
    ) %>%
    ungroup() %>%
    group_by(scen_num, grpvar, capacity) %>%
    mutate(peak = max(value)) %>%
    f_valuefct()
  
  
  #### Generate factor variable with custom cuts
  summary(subdatWklAvr$value)
  
  table(subdatWklAvr$value_fct, exclude = NULL)
  tapply(subdatWklAvr$value, subdatWklAvr$value_fct, summary)
  
  
  maxval <- max(subdatWklAvr$value, na.rm = TRUE)
  
  l_plot <- ggplot(data = subdatWklAvr_lines) +
    theme_cowplot() +
    geom_line(
      data = subset(subdatWklAvr_lines),
      aes(x = Date, y = value, group = scen_num), col = "azure4", size = 1, alpha = 0.5
    ) +
    # geom_line(data = subset(subdatWklAvr, grpvar == unique(subdat$grpvar)[1] & scen_num %in% selectedScens),
    #          aes(x = Date, y = value, group=scen_num), col = "brown3", size = 1, alpha=0.8)  +
    geom_point(
      data = subset(subdatWklAvr, value == peak),
      aes(x = Date, y = value, group = scen_num, fill = value_fct), size = 4, alpha = 1, shape = 21
    ) +
    # geom_point(data = subset(subdatWklAvr, value == peak & grpvar == unique(subdat$grpvar)[1] & scen_num %in% selectedScens),
    #          aes(x = Date, y = value, group=scen_num), fill = "brown3", size =4, alpha=1, shape=21)  +
    geom_hline(aes(yintercept = capacity)) +
    scale_color_viridis(option = "C", discrete = TRUE) +
    scale_fill_viridis(option = "C", discrete = TRUE) +
    labs(
      title = "",
      subtitle = "", y = "Predicted ICU bed demand\n per 1000 population/n",
      x = ""
    ) +
    customTheme +
    scale_x_date(breaks = "30 days", labels = date_format("%b"), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, maxval + 100), expand = c(0, 0)) +
    theme(legend.position = "none")
  
  
  ggsave(paste0("scenarios_timeline_wklAvr.png"),
         plot = l_plot, path = file.path(exp_dir), width = 10, height = 6, device = "png"
  )
  
  ggsave(paste0("scenarios_timeline_wklAvr.pdf"),
         plot = l_plot, path = file.path(pdfoutdir), width = 7, height = 5.5, device = "pdf"
  )
  
  
  #### Generate scatter plot
  peakdat <- subset(subdatWklAvr, value == peak & !is.na(value_fct))
  
  s_plot <- ggplot(data = peakdat) +
    theme_minimal() +
    geom_point(aes(x = detection_success, y = isolation_success, fill = value_fct), size = 4, alpha = 1, shape = 21) +
    scale_fill_viridis(option = "C", discrete = TRUE) +
    labs(
      title = "",
      subtitle = "",
      x = "Fraction detected",
      y = "Fraction detected that isolate\n",
      fill = "Predicted ICU bed demand\nper 1000 population"
    ) +
    customTheme +
    theme(legend.position = "none")
  
  
  
  ggsave(paste0("scenarios_scatter_wklAvr_scl.png"),
         plot = s_plot, path = file.path(exp_dir), width = 10, height = 6, device = "png"
  )
  
  ggsave(paste0("scenarios_scatter_wklAvr_scl.pdf"),
         plot = s_plot, path = file.path(pdfoutdir), width = 6, height = 5.5, device = "pdf"
  )
  
  
  #### Heatmap - loess
  heatmapPlot <- TRUE
  if (heatmapPlot) {
    
    
    #### Do loess regression
    detection_success <- seq(0, 1, 0.001)
    isolation_success <- seq(0, 1, 0.001)
    t_matdat <- expand.grid(detection_success = detection_success, isolation_success = isolation_success)
    
    m <- loess(value ~ detection_success * isolation_success,
               span = 0.5,
               degree = 2, data = peakdat
    )
    
    temp_fit_mat <- predict(m, t_matdat)
    dtfit <- melt(temp_fit_mat)
    dtfit$detection_success <- gsub("detection_success=", "", temp_fit$detection_success)
    dtfit$isolation_success <- gsub("isolation_success=", "", temp_fit$isolation_success)
    
    dtfit$detection_success <- as.numeric(dtfit$detection_success)
    dtfit$isolation_success <- as.numeric(dtfit$isolation_success)
    dtfit <- f_valuefct(dtfit)
    
    
    dtfit$value_fct2 <- cut(dtfit$value, 10)
    
    
    ### Extract  minimum isolation_success for each detection_success
    thresholdDat <- dtfit %>%
      filter(value <= capacity$capacity) %>%
      group_by(detection_success, grpvar) %>%
      filter(isolation_success == min(isolation_success))
    
    ### Plot contour-heatmap plot
    p1 <- ggplot(data = subset(dtfit, !is.na(value_fct)), aes(x = detection_success, y = isolation_success)) +
      theme_minimal() +
      geom_tile(aes(fill = value_fct), alpha = 0.8) +
      scale_fill_viridis(option = "C", discrete = TRUE) +
      # scale_fill_viridis(option = "C", discrete = FALSE) +
      labs(
        x = "Fraction detected",
        y = "Fraction detected that isolate\n",
        col = "",
        shape = "",
        linetype = ""
      ) +
      scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), expand = c(0, 0)) +
      scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), expand = c(0, 0)) +
      customThemeNoFacet +
      theme(panel.spacing = unit(1.5, "lines")) +
      theme(legend.position = "none")
    
    p1_withPoints <- ggplot() +
      geom_line(
        data = subset(thresholdDat, isolation_success != min(isolation_success)),
        aes(x = detection_success, y = isolation_success), size = 1.3
      ) +
      geom_point(data = peakdat, aes(x = detection_success, y = isolation_success, fill = value_fct), size = 3, shape = 21, show.legend = FALSE) +
      scale_fill_viridis(option = "C", discrete = TRUE) +
      # scale_fill_viridis(option = "C", discrete = FALSE) +
      labs(
        x = "Fraction detected",
        y = "Fraction detected that isolate\n",
        col = "",
        shape = "",
        linetype = ""
      ) +
      scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), expand = c(0, 0)) +
      scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), expand = c(0, 0)) +
      customThemeNoFacet +
      theme(panel.spacing = unit(1.5, "lines")) +
      theme(legend.position = "none")
    
    ggsave("ICUcapacity_heatmap_loess.png",
           plot = p1, path = file.path(pdfoutdir), width = 6, height = 5.5, device = "png"
    )
    
    
    ggsave("ICUcapacity_heatmap_loess_layers.pdf",
           plot = p1_withPoints, path = file.path(pdfoutdir), width = 6, height = 5.5, device = "pdf"
    )
  }
}


methodPlot_Rt <- FALSE
if (methodPlot) {
  customTheme <- f_getCustomTheme(fontscl = 3)
  
  # exp_name <- "20200731_IL_reopen_contactTracing"
  Rtdat <- read.csv(ct_dir, simdate, exp_name, "estimatedRt/EMS_combined_estimated_Rt.csv")
  
  
  selected_ems <- c(1:11)
  emsvars_temp <- c("critical_EMS.")
  emsvars <- NULL
  for (ems in selected_ems) {
    emsvars <- c(emsvars, paste0(emsvars_temp, ems))
  }
  
  ### Aggregate for all IL
  Rtsubdat <- Rtdat %>%
    filter(region == 11) %>%
    mutate(
      Date = as.Date(Date),
      capacity = 1,
      value = Mean
    ) %>%
    filter(grpvar == 0.05) %>%
    filter(Date >= as.Date(reopeningdate - 60) & Date <= as.Date("2020-10-01"))
  
  ##### SMooth, calculate weekly average
  subdatAvr <- Rtsubdat %>%
    dplyr::filter(Date >= as.Date(reopeningdate) & Date <= as.Date(reopeningdate + 30)) %>%
    dplyr::group_by(scen_num, detection_success, isolation_success, capacity) %>%
    dplyr::summarize(
      value = mean(value),
      Date = mean(Date)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scen_num, capacity) %>%
    mutate(peakavrg = mean(value)) %>%
    f_valuefctRt()
  
  subdatAvr2 <- Rtsubdat %>%
    dplyr::filter(Date >= as.Date(reopeningdate + 29) & Date <= as.Date(reopeningdate + 30)) %>%
    dplyr::group_by(scen_num, detection_success, isolation_success, capacity) %>%
    dplyr::summarize(
      value = mean(value),
      Date = mean(Date)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scen_num, capacity) %>%
    mutate(peakafter1mth = mean(value)) %>%
    f_valuefctRt()
  
  
  #### Generate factor variable with custom cuts
  summary(Rtsubdat$value)
  summary(subdatAvr$value)
  
  l_plot <- ggplot(data = subdatAvr) +
    theme_cowplot() +
    annotate("rect", xmin = reopeningdate, xmax = reopeningdate + 30, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "azure3") +
    geom_line(
      data = Rtsubdat,
      aes(x = Date, y = value, group = scen_num), col = "azure4", size = 1, alpha = 0.5
    ) +
    geom_point(
      data = subdatAvr,
      aes(x = Date, y = value, group = scen_num, fill = value_fct), shape = 21, size = 4, alpha = 0.3
    ) +
    geom_point(
      data = subdatAvr2,
      aes(x = Date, y = value, group = scen_num, fill = value_fct), shape = 21, size = 4, alpha = 1
    ) +
    geom_hline(aes(yintercept = capacity)) +
    scale_color_viridis(option = "C", discrete = TRUE) +
    scale_fill_viridis(option = "C", discrete = TRUE) +
    labs(
      title = "",
      subtitle = "", y = expr("Estimated" * " " * italic(R[t])),
      x = ""
    ) +
    customTheme +
    scale_x_date(breaks = "1 month", labels = date_format("%b"), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.position = "none")
  
  
  ggsave(paste0("scenarios_timeline_Rt.png"),
         plot = l_plot, path = file.path(exp_dir), width = 10, height = 6, device = "png"
  )
  
  ggsave(paste0("scenarios_timeline_Rt.pdf"),
         plot = l_plot, path = file.path(pdfoutdir), width = 7, height = 5.5, device = "pdf"
  )
  
  
  #### Generate scatter plot
  peakdat <- subset(subdatAvr2, value == peakafter1mth & !is.na(value_fct))
  
  s_plot <- ggplot(data = peakdat) +
    theme_minimal() +
    geom_point(aes(x = detection_success, y = isolation_success, fill = value_fct), size = 4, alpha = 1, shape = 21) +
    scale_fill_viridis(option = "C", discrete = TRUE) +
    labs(
      title = "",
      subtitle = "",
      x = "Fraction detected",
      y = "Fraction detected that isolate\n",
      fill = "Predicted ICU bed demand\nper 1000 population"
    ) +
    customTheme +
    theme(legend.position = "none")
  
  
  
  ggsave(paste0("scenarios_scatter_after1mth_Rt.png"),
         plot = s_plot, path = file.path(exp_dir), width = 10, height = 6, device = "png"
  )
  
  ggsave(paste0("scenarios_scatter_after1mth_Rt.pdf"),
         plot = s_plot, path = file.path(pdfoutdir), width = 6, height = 5.5, device = "pdf"
  )
  
  
  #### Heatmap - loess
  heatmapPlot <- TRUE
  if (heatmapPlot) {
    
    
    #### Do loess regression
    detection_success <- seq(0, 1, 0.001)
    isolation_success <- seq(0, 1, 0.001)
    t_matdat <- expand.grid(detection_success = detection_success, isolation_success = isolation_success)
    
    m <- loess(value ~ detection_success * isolation_success,
               span = 0.5,
               degree = 2, data = peakdat
    )
    
    temp_fit_mat <- predict(m, t_matdat)
    dtfit <- melt(temp_fit_mat)
    dtfit$detection_success <- gsub("detection_success=", "", temp_fit$detection_success)
    dtfit$isolation_success <- gsub("isolation_success=", "", temp_fit$isolation_success)
    
    dtfit$detection_success <- as.numeric(dtfit$detection_success)
    dtfit$isolation_success <- as.numeric(dtfit$isolation_success)
    dtfit <- f_valuefctRt(dtfit)
    dtfit$value_fct2 <- cut(dtfit$value, 10)
    
    
    ### Extract  minimum isolation_success for each detection_success
    thresholdDat <- dtfit %>%
      filter(value <= 1) %>%
      group_by(detection_success) %>%
      filter(isolation_success == min(isolation_success))
    
    ### Plot contour-heatmap plot
    p1 <- ggplot(data = subset(dtfit, !is.na(value_fct)), aes(x = detection_success, y = isolation_success)) +
      theme_minimal() +
      geom_tile(aes(fill = value_fct), alpha = 0.8) +
      scale_fill_viridis(option = "C", discrete = TRUE) +
      # scale_fill_viridis(option = "C", discrete = FALSE) +
      labs(
        x = "Fraction detected",
        y = "Fraction detected that isolate\n",
        col = "",
        shape = "",
        linetype = ""
      ) +
      scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), expand = c(0, 0)) +
      scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), expand = c(0, 0)) +
      customThemeNoFacet +
      theme(panel.spacing = unit(1.5, "lines")) +
      theme(legend.position = "none")
    
    p1_withPoints <- ggplot() +
      geom_line(
        data = subset(thresholdDat, isolation_success != min(isolation_success)),
        aes(x = detection_success, y = isolation_success), size = 1.3
      ) +
      geom_point(data = peakdat, aes(x = detection_success, y = isolation_success, fill = value_fct), size = 3, shape = 21, show.legend = FALSE) +
      scale_fill_viridis(option = "C", discrete = TRUE) +
      # scale_fill_viridis(option = "C", discrete = FALSE) +
      labs(
        x = "Fraction detected",
        y = "Fraction detected that isolate\n",
        col = "",
        shape = "",
        linetype = ""
      ) +
      scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), expand = c(0, 0)) +
      scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), expand = c(0, 0)) +
      customThemeNoFacet +
      theme(panel.spacing = unit(1.5, "lines")) +
      theme(legend.position = "none")
    
    ggsave("ICUcapacity_heatmap_loess_Rt.png",
           plot = p1, path = file.path(pdfoutdir), width = 6, height = 5.5, device = "png"
    )
    
    
    ggsave("ICUcapacity_heatmap_loess_Rt_layers.pdf",
           plot = p1_withPoints, path = file.path(pdfoutdir), width = 6, height = 5.5, device = "pdf"
    )
  }
}

