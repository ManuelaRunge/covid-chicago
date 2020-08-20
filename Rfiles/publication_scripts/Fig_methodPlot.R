
### =======================================================
#### Additional plots for contact tracing simulations
### =======================================================


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

pdfdir <- "C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/project_notes/publications/covid_model_IL/pdfs_ais/"
ct_dir <- file.path(simulation_output, "contact_tracing")

# region_cols <- c()
restoreRegion_cols <- c("Central" = "red2", "Northcentral" = "dodgerblue3", "Northeast" = "chartreuse4", "Southern" = "orchid4")

simdate ="20200731"
startdate <- "2020-06-15"
stopdate <- "2020-12-30"
reopen <- c(0, 0.05, 0.1)
customTheme <- f_getCustomTheme()
ct_startdate <- as.Date("2020-07-30")

reopeningdate = as.Date("2020-07-22")
## ================================================
###  Plot individual trajectories over time for method plot
## ================================================

methodPlot <- FALSE
if (methodPlot) {
  customTheme <- f_getCustomTheme(fontscl = 3)
  
  exp_name <- "20200731_IL_reopen_contactTracing"
  exp_dir <- file.path(file.path(simulation_output, "contact_tracing",simdate, exp_name)) 
  
  #source("C:/Users/mrm9534/gitrepos/covid-chicago/Rfiles/ct_analysis/loadData_defineParam.R")
  
  selected_ems <- c(1:11)
  emsvars_temp <- c("critical_EMS.")   ### c("critical_det_EMS.")
  emsvars <- NULL
  for (ems in selected_ems) {
    emsvars <- c(emsvars, paste0(emsvars_temp, ems))
  }
  
  groupvars <- c("startdate", "Date", "time", "scen_num",  "detection_success", "isolation_success", "grpvar")
  (keepvars <- c(groupvars, emsvars))
  
  
  trajectoriesDat <- read_csv(file.path(simulation_output, "contact_tracing",simdate, exp_name, "trajectoriesDat.csv"), 
                              col_types = cols_only(time = col_guess(), 
                                                    startdate = col_guess(),
                                                    scen_num = col_guess(),
                                                    d_AsP_ct1 = col_guess(),
                                                    reduced_inf_of_det_cases_ct1 = col_guess(),
                                                    reopening_multiplier_4 = col_guess(),
                                                    critical_All = col_guess(),
                                                    critical_det_All = col_guess(),
                                                    `critical_EMS-1` = col_guess(),
                                                    `critical_det_EMS-1` = col_guess() ,
                                                    `critical_EMS-2` = col_guess(),
                                                    `critical_det_EMS-2` = col_guess() ,
                                                    `critical_EMS-3` = col_guess(),
                                                    `critical_det_EMS-3` = col_guess() ,
                                                    `critical_EMS-4` = col_guess(),
                                                    `critical_det_EMS-4` = col_guess() ,
                                                    `critical_EMS-5` = col_guess(),
                                                    `critical_det_EMS-5` = col_guess() ,
                                                    `critical_EMS-6` = col_guess(),
                                                    `critical_det_EMS-6` = col_guess() ,
                                                    `critical_EMS-7` = col_guess(),
                                                    `critical_det_EMS-7` = col_guess() ,
                                                    `critical_EMS-8` = col_guess(),
                                                    `critical_det_EMS-8` = col_guess() ,
                                                    `critical_EMS-9` = col_guess(),
                                                    `critical_det_EMS-9` = col_guess() ,
                                                    `critical_EMS-10` = col_guess(),
                                                    `critical_det_EMS-10` = col_guess() ,
                                                    `critical_EMS-11` = col_guess(),
                                                    `critical_det_EMS-11` = col_guess() 
                              )) %>%
                              rename(detection_success=d_AsP_ct1,
                                     grpvar = reopening_multiplier_4) %>%
                              mutate( isolation_success = 1-(reduced_inf_of_det_cases_ct1))
  
  

  ### Aggregate for all IL
  subdat <- trajectoriesDat %>%
    mutate(Date = as.Date(startdate)+ time) %>%
    pivot_longer(cols = -c(groupvars)) %>%
    dplyr::mutate(name = gsub("All", "EMS-IL", name)) %>%
    dplyr::mutate(name = gsub("-", "_", name)) %>%
    separate(name, into = c("outcome", "region"), sep = "_EMS_") %>%
    dplyr::filter(Date >= reopeningdate - 60 & Date <= as.Date("2021-01-15")) %>%
    dplyr::select(-c(time)) %>%
    filter(region == 11) %>%
    dplyr::group_by(startdate, region, Date,  scen_num,   detection_success, isolation_success, grpvar) %>%
    dplyr::summarize(value = sum(value))
  
  capacityDat <- load_capacity(unique(subdat$region)) %>%
    dplyr::rename(capacity = critical,
                  region=geography_name)
  
  popdat <- load_population() %>% rename(region=geography_name) %>%filter(region %in% unique(subdat$region)) 
  
  
  subdat <- subdat %>%
    left_join(capacityDat, by = "region") %>%
    left_join(popdat, by = "region") %>%
    filter(grpvar == 0.05) %>%
    mutate(value=as.numeric(value),
           pop = as.numeric(pop))
  
  ##### SMooth, calculate weekly average
  subdatWklAvr_lines <- subdat %>%
    mutate(week = week(Date)) %>%
    group_by(week, scen_num, grpvar, detection_success, isolation_success, capacity, pop) %>%
    summarize(
      value = mean(value),
      Date = max(Date)
    ) %>%
    ungroup() %>%
    group_by(scen_num, grpvar, capacity, pop) %>%
    mutate(percCapacity = (value-capacity)/capacity,
           peak = max(value),
           value_pop1000 = (value /pop )*1000,
           capacity_pop1000 = (capacity /pop )*1000) %>%
    f_valuefct()
  
  
  subdatWklAvr <- subdat %>%
    dplyr::filter(Date >= reopeningdate & Date <= as.Date("2021-02-01")) %>%
    mutate(week = week(Date)) %>%
    group_by(week, scen_num, grpvar, detection_success, isolation_success, capacity, pop) %>%
    summarize(
      value = mean(value),
      Date = max(Date)
    ) %>%
    ungroup() %>%
    group_by(scen_num, grpvar, capacity, pop) %>%
    mutate(percCapacity = (value-capacity)/capacity,
           peak = max(value),
           value_pop1000 = (value /pop )*1000,
           capacity_pop1000 = (capacity /pop )*1000) %>%
    f_valuefct()
  
  
  
  labs=quantile(subdatWklAvr_lines$percCapacity, probs = seq(0.1,1, 0.1), na.rm=FALSE)
  subdatWklAvr_lines <- subdatWklAvr_lines %>%    f_valuefct_cap() #mutate(value_cut =cut(percCapacity, 10, labels=labs ) )
   
  labs=quantile(subdatWklAvr$percCapacity, probs = seq(0.1,1, 0.1), na.rm=FALSE)
  subdatWklAvr <- subdatWklAvr %>%    f_valuefct_cap() #mutate(value_cut =cut(percCapacity, 10, labels=labs ) )
  
  table(subdatWklAvr_lines$value_fct)
  table(subdatWklAvr$value_fct)
  
  #### Generate factor variable with custom cuts
  summary(subdatWklAvr$value)
  summary(subdatWklAvr$value_pop1000)
  
  table(subdatWklAvr$value_fct, exclude = NULL)
  tapply(subdatWklAvr$value, subdatWklAvr$value_fct, summary)
  
  
  maxval <- max(subdatWklAvr$value_pop1000, na.rm = TRUE)
  
  l_plot <- ggplot(data = subdatWklAvr_lines) +
    theme_cowplot() +
    geom_line(
      data = subset(subdatWklAvr_lines),
      aes(x = Date, y = value_pop1000, group = scen_num), col = "azure4", size = 1, alpha = 0.5
    ) +
    geom_point(
      data = subset(subdatWklAvr, value == peak),
      aes(x = Date, y = value_pop1000, group = scen_num, fill = value_fct), size = 4, alpha = 1, shape = 21
    ) +
    # geom_point(data = subset(subdatWklAvr, value == peak & grpvar == unique(subdat$grpvar)[1] & scen_num %in% selectedScens),
    #          aes(x = Date, y = value, group=scen_num), fill = "brown3", size =4, alpha=1, shape=21)  +
    geom_hline(aes(yintercept = capacity_pop1000)) +
    scale_color_viridis(option = "C", discrete = TRUE, drop = F) +
    scale_fill_viridis(option = "C", discrete = TRUE, drop = F) +
    labs(
      title = "",
      subtitle = "", y = "Predicted ICU bed demand\n per 1000 population/n",
      x = ""
    ) +
    customTheme +
    scale_x_date(breaks = "30 days", labels = date_format("%b"), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1.2), expand = c(0, 0), breaks=seq(0,1.2,0.2), labels=seq(0,1.2,0.2)) +
    theme(legend.position = "none")
  
  
  ggsave(paste0("scenarios_timeline_wklAvr.png"),
         plot = l_plot, path = file.path(exp_dir), width = 10, height = 6, device = "png"
  )
  
  ggsave(paste0("scenarios_timeline_wklAvr.pdf"),
         plot = l_plot, path = file.path(pdfdir), width = 7, height = 5.5, device = "pdf"
  )
  
  
  #### Generate scatter plot
  peakdat <- subset(subdatWklAvr, value == peak & !is.na(value_fct))
  
  s_plot <- ggplot(data = peakdat) +
    theme_minimal() +
    geom_point(aes(x = detection_success, y = isolation_success, fill = value_fct), size = 4, alpha = 1, shape = 21) +
    scale_fill_viridis(option = "C", discrete = TRUE, drop = F) +
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
         plot = s_plot, path = file.path(pdfdir), width = 6, height = 5.5, device = "pdf"
  )
  
  
  #### Heatmap - loess
  heatmapPlot <- TRUE
  if (heatmapPlot) {
    
    
    #### Do loess regression
    detection_success <- seq(0, 1, 0.005)
    isolation_success <- seq(0, 1, 0.005)
    t_matdat <- expand.grid(detection_success = detection_success, isolation_success = isolation_success)
    
    m <- loess(value ~ detection_success * isolation_success,
               span = 0.5,
               degree = 2, data = peakdat
    )
    
    temp_fit_mat <- predict(m, t_matdat)
    dtfit <- melt(temp_fit_mat)

    dtfit <- dtfit %>% 
             mutate(detection_success =  as.numeric(gsub("detection_success=", "", detection_success)),
                    isolation_success =  as.numeric(gsub("isolation_success=", "", isolation_success)),
                    grpvar = unique(peakdat$grpvar) , 
                    capacity=unique(peakdat$capacity) ) %>%
            f_valuefct_cap()
    
  
    ### Extract  minimum isolation_success for each detection_success
    thresholdDat <- dtfit %>%
      filter(value <= capacity) %>%
      dplyr::group_by(detection_success, grpvar) %>%
      filter(isolation_success == min(isolation_success))
    
    
    summary(dtfit$value)
    summary(thresholdDat$isolation_success)
    summary(thresholdDat$detection_success)
    
    ### Plot contour-heatmap plot
    p1 <- ggplot(data = subset(dtfit, !is.na(value_fct)), aes(x = detection_success, y = isolation_success)) +
      theme_minimal() +
      geom_tile(aes(fill = value_fct), alpha = 0.8) +
      scale_fill_viridis(option = "C", discrete = TRUE, drop = F) +
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
      #geom_tile(data = subset(dtfit, !is.na(value_fct)), aes(x = detection_success, y = isolation_success, fill = value_fct), alpha = 0.8) +
      geom_line(
        data = subset(thresholdDat),
        aes(x = detection_success, y = isolation_success), size = 1.3
      ) +
      geom_point(data = peakdat, aes(x = detection_success, y = isolation_success, fill = value_fct), size = 3, shape = 21, show.legend = FALSE) +
      scale_fill_viridis(option = "C", discrete = TRUE, drop = F) +
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
           plot = p1, path = file.path(pdfdir), width = 6, height = 5.5, device = "png"
    )
    
    
    ggsave("ICUcapacity_heatmap_loess_layers.pdf",
           plot = p1_withPoints, path = file.path(pdfdir), width = 6, height = 5.5, device = "pdf"
    )
  }
}


methodPlot_Rt <- FALSE
if (methodPlot) {
  customTheme <- f_getCustomTheme(fontscl = 3)
  
  # exp_name <- "20200731_IL_reopen_contactTracing"
  Rtdat <- read.csv(file.path(ct_dir, simdate, exp_name, "estimatedRt/EMS_combined_estimated_Rt.csv"))
  
  
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
      value = Mean,
      percCapacity = (value-capacity)/capacity
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
    mutate(peakavrg = mean(value)) #%>%
   # f_valuefctRt()
  
  subdatAvr2 <- Rtsubdat %>%
    dplyr::filter(Date >= as.Date(reopeningdate + 29) & Date <= as.Date(reopeningdate + 30)) %>%
    dplyr::group_by(scen_num, detection_success, isolation_success, capacity) %>%
    dplyr::summarize(
      value = mean(value),
      Date = mean(Date)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scen_num, capacity) %>%
    mutate(peakafter1mth = mean(value)) #%>%
   # f_valuefctRt()
  
  subdatAvr <- subdatAvr %>%    f_valuefct_cap(fewerClasses=T) 
  subdatAvr2 <- subdatAvr2 %>%    f_valuefct_cap(fewerClasses=T) 
  
  
  #### Generate factor variable with custom cuts
  summary(Rtsubdat$value)
  summary(subdatAvr$value)
  
  l_plot <- ggplot(data = subdatAvr) +
    theme_cowplot() +
   # annotate("rect", xmin = reopeningdate, xmax = reopeningdate + 30, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "azure3") +
    geom_line(
      data = Rtsubdat,
      aes(x = Date, y = value, group = scen_num), col = "azure4", size = 1, alpha = 0.5
    ) +
    geom_point(
      data = subdatAvr2,
      aes(x = Date, y = value, group = scen_num, fill = value_fct), shape = 21, size = 4, alpha = 1
    ) +
    geom_point(
      data = subdatAvr,
      aes(x = Date, y = value, group = scen_num, fill = value_fct), shape = 21, size = 4, alpha = 0.3
    ) +
    geom_hline(aes(yintercept = capacity)) +
    scale_color_viridis(option = "C", discrete = TRUE, drop=F) +
    scale_fill_viridis(option = "C", discrete = TRUE, drop=F) +
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
         plot = l_plot, path = file.path(pdfdir), width = 7, height = 5.5, device = "pdf"
  )
  
  
  #### Generate scatter plot
  peakdat <- subset(subdatAvr2, value == peakafter1mth & !is.na(value_fct))
  
  s_plot <- ggplot(data = peakdat) +
    theme_minimal() +
    geom_point(aes(x = detection_success, y = isolation_success, fill = value_fct), size = 4, alpha = 1, shape = 21) +
    scale_fill_viridis(option = "C", discrete = TRUE, drop=F) +
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
         plot = s_plot, path = file.path(pdfdir), width = 6, height = 5.5, device = "pdf"
  )
  
  
  #### Heatmap - loess
  heatmapPlot <- TRUE
  if (heatmapPlot) {
    
    
    #### Do loess regression
    detection_success <- seq(0, 1, 0.002)
    isolation_success <- seq(0, 1, 0.002)
    t_matdat <- expand.grid(detection_success = detection_success, isolation_success = isolation_success)
    
    m <- loess(value ~ detection_success * isolation_success,
               span = 0.5,
               degree = 2, data = peakdat
    )
    
    temp_fit_mat <- predict(m, t_matdat)
    dtfit <- melt(temp_fit_mat)
    dtfit$detection_success <- gsub("detection_success=", "", dtfit$detection_success)
    dtfit$isolation_success <- gsub("isolation_success=", "", dtfit$isolation_success)
    
    dtfit$detection_success <- as.numeric(dtfit$detection_success)
    dtfit$isolation_success <- as.numeric(dtfit$isolation_success)
    
    dtfit <- dtfit %>% mutate(capacity=1) %>%   f_valuefct_cap(fewerClasses = T) 
    
    
    ### Extract  minimum isolation_success for each detection_success
    thresholdDat <- dtfit %>%
      filter(value <= 1) %>%
      group_by(detection_success) %>%
      filter(isolation_success == min(isolation_success))
    
    ### Plot contour-heatmap plot
    p1 <- ggplot(data = subset(dtfit, !is.na(value_fct)), aes(x = detection_success, y = isolation_success)) +
      theme_minimal() +
      geom_tile(aes(fill = value_fct), alpha = 0.8) +
      scale_fill_viridis(option = "C", discrete = TRUE, drop=F) +
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
      #geom_tile(data = subset(dtfit, !is.na(value_fct)), aes(x = detection_success, y = isolation_success, fill = value_fct), alpha = 0.8) +
      geom_line(
        data = subset(thresholdDat, isolation_success != min(isolation_success)),
        aes(x = detection_success, y = isolation_success), size = 1.3
      ) +
      geom_point(data = peakdat, aes(x = detection_success, y = isolation_success, fill = value_fct), size = 3, shape = 21, show.legend = FALSE) +
      scale_fill_viridis(option = "C", discrete = TRUE, drop = F) +
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
    
    ggsave("capacity_heatmap_loess_Rt.png",
           plot = p1, path = file.path(pdfdir), width = 6, height = 5.5, device = "png"
    )
    
    
    ggsave("capacity_heatmap_loess_Rt_layers.pdf",
           plot = p1_withPoints, path = file.path(pdfdir), width = 6, height = 5.5, device = "pdf"
    )
  }
}

