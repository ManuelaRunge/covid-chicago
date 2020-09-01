## --------------------------------------------
## Generate heatmap and supplementary plots per EMS or aggregated region
### Using loess regression per EMS and grpvar
## --------------------------------------------


f_runHeatmapAnalysis_region <- function(ems, geography="Region", enddate="2021-12-31"){
  
  selected_ems <- as.numeric(regions[[ems]])
  regname = ems
  
  tempdat <- trajectoriesDat %>%
    filter(Date >= as.Date(interventionstart)) %>%
    getdata(selected_ems) %>%
    filter(outcome == "crit_det") %>%
    as.data.frame()
  
  capacityDat <- load_new_capacity(ems) %>% dplyr::rename(region=geography_name, capacity = icu_available)
  if(length(selected_ems)>1){
    capacityDat <- load_new_capacity(tolower(ems)) %>% dplyr::rename(region=geography_name, capacity = icu_available)
    tempdat$region = regname
  }
  
  tempdat$capacity <- capacityDat$capacity
  
  ### Take weekly average before filtering for maximum value
  library(lubridate)
  
  subdatWklAvr <- tempdat %>%
    dplyr::filter(Date >= reopeningdate & Date <= as.Date(enddate)) %>%
    dplyr::mutate(week = week(Date)) %>%
    # dplyr::group_by( region, week, scen_num, grpvar, detection_success, isolation_success, capacity) %>%
    dplyr::group_by(Date, region, week, scen_num, grpvar, detection_success, isolation_success, capacity) %>%
    # dplyr::summarize(
    #   value = mean(value, na.rm=TRUE),
    #   Date = max(Date)
    # ) %>%
    dplyr::summarize(
      value = mean(value, na.rm=TRUE),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scen_num, grpvar, capacity) %>%
    dplyr::mutate(peak = max(value)) %>%
    f_valuefct_cap(fewerClasses=F)
  
  
  peakdat <- subset(subdatWklAvr, value == peak & !is.na(value_fct)) 
  
  ### Scatter plot  of outcome variable per CT parameter at peak
  showScatter <- TRUE
  if (showScatter) {
    s_plot <-   ggplot(data = peakdat) +
      theme_minimal() +
      geom_point(aes(x = detection_success, y = isolation_success, fill = value_fct), size = 4, alpha = 1, shape = 21) +
      scale_fill_viridis(option = "C", discrete = T, drop = F) +
      facet_wrap(~grpvar, nrow=2) +
      labs(
        title = "",
        subtitle = "",
        x = "Fraction detected",
        y = "Fraction detected that isolate\n",
        fill = "Predicted ICU bed demand\nper 1000 population"
      ) +
      theme(legend.position = "none")+
      geom_hline(yintercept = c(-Inf, Inf))+
      geom_vline(xintercept = c(-Inf, Inf))+
      theme(legend.position = "right",
            panel.spacing = unit(2, "lines"))+
      customTheme
    
    ggsave(paste0(ems, "_scatterplot.png"),
           plot = s_plot, path = file.path(heatmapICUDir), width = 12, height = 10, device = "png"
    )
    
    ggsave(paste0(ems, "_scatterplot.pdf"),
           plot = s_plot, path = file.path(heatmapICUDir), width = 12, height = 10, device = "pdf"
    )
  }
  
  fitlist <- list()
  for (grp in unique(peakdat$grpvar)) {
    # grp  <- unique(peakdat$grpvar)[1]
    #### Do loess regression
    detection_success <- seq(0, 1, 0.005)
    isolation_success <- seq(0, 1, 0.005)
    t_matdat <- expand.grid(detection_success = detection_success, isolation_success = isolation_success)
    
    m <- loess(value ~ detection_success * isolation_success,
               span = 0.7,
               degree = 2, data = subset(peakdat, grpvar == grp)
    )
    
    temp_fit_mat <- predict(m, t_matdat)
    temp_fit <- melt(temp_fit_mat)
    temp_fit$detection_success <- gsub("detection_success=", "", temp_fit$detection_success)
    temp_fit$isolation_success <- gsub("isolation_success=", "", temp_fit$isolation_success)
    
    
    showPlot=FALSE
    if(showPlot){
      library(plotly)
      fig <- plot_ly(
        x = temp_fit$detection_success,
        y = temp_fit$isolation_success,
        z =  temp_fit$value, 
        type = "contour"
      )
      print(fig)
    }
    
    
    temp_fit$detection_success <- as.numeric(temp_fit$detection_success)
    temp_fit$isolation_success <- as.numeric(temp_fit$isolation_success)
    temp_fit$grpvar <- grp
    temp_fit$capacity <- unique(peakdat$capacity)
    #temp_fit <- na.omit(temp_fit)
    temp_fit <- f_valuefct_cap(temp_fit)
    
    fitlist[[length(fitlist) + 1]] <- temp_fit
    
    rm(temp_fit_mat, temp_fit)
  }
  
  dtfit <- bind_rows(fitlist)
  rm(fitlist)
  
  peakdat <- f_valuefct_cap(peakdat)
  
  ### Extract  minimum isolation_success for each detection_success
  thresholdDat <- dtfit %>%
    dplyr::filter(value <= capacityDat$capacity) %>%
    dplyr::group_by(detection_success, grpvar) %>%
    dplyr::filter(isolation_success == min(isolation_success)) %>%
    dplyr::mutate(region = ems)
  
  ### Plot contour-heatmap plot
  p1 <- ggplot(data = subset(dtfit, !is.na(value_fct)), aes(x = detection_success, y = isolation_success)) +
    theme_minimal() +
    geom_tile(aes(fill = value_fct), alpha = 0.8) +
    # geom_point(data=subset(peakdat, grpvar==0.17),  aes(x = detection_success, y = isolation_success, fill = value_fct, group = scen_num), size = 3, shape = 21, show.legend = FALSE) +
    scale_fill_viridis(option = "C", discrete = T, drop = F) +
    labs(
      x = "detections (%)",
      y = "isolation success (%)",
      col = "",
      fill = "Critical",
      shape = "",
      linetype = ""
    ) +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    facet_wrap(~grpvar, nrow=1) +
    customThemeNoFacet +
    theme(panel.spacing = unit(1.5, "lines"))
  
  p1_capacity <- p1 + geom_line(
    data = subset(thresholdDat, isolation_success != min(isolation_success)),
    aes(x = detection_success, y = isolation_success), size = 1.3
  )
  
  
  p2_modelfit <- p1 + geom_point(
    data = peakdat, aes(x = detection_success, y = isolation_success, fill = value_fct),
    shape = 21, size = 3, show.legend = FALSE
  )+ geom_line(
    data = subset(thresholdDat, isolation_success != min(isolation_success)),
    aes(x = detection_success, y = isolation_success), size = 1.3
  )
  
  
  plotname_capacity <- paste0("Region_", ems, "_ICUcapacity_heatmap")
  plotname_model <- paste0( "Region_", ems, "_ICUcapacity_heatmap_wP")
  
  SAVE_png <- TRUE
  if (SAVE_png) {
    ggsave(paste0(plotname_capacity, ".png"),
           plot = p1_capacity, path = file.path(heatmapICUDir), width = 13, height = 4, device = "png"
    )
    
    
    ggsave(paste0(plotname_model, ".png"),
           plot = p2_modelfit, path = file.path(heatmapICUDir), width = 13, height = 4, device = "png"
    )
  }
  
  
  SAVE_pdf <- TRUE
  if (SAVE_pdf) {
    ggsave(paste0(plotname_capacity, ".pdf"),
           plot = p1_capacity, path = file.path(heatmapICUDir), width = 13, height = 4, device = "pdf"
    )
    
    
    
    ggsave(paste0(plotname_model, ".pdf"),
           plot = p2_modelfit, path = file.path(heatmapICUDir), width = 13, height = 4, device = "pdf"
    )
  }
  
  
  save(dtfit, file = file.path(heatmapICUDir, paste0(ems, "_dtfit.Rdata")))
  if(file.exists(file.path(heatmapICUDir, paste0(ems, "_dtfit.Rdata")))) print("Rdata saved")
  
  
  write.csv(thresholdDat, file = file.path(heatmapICUDir, paste0(ems, "_loess_ICUcapacity.csv")), row.names = FALSE)
  if(file.exists(file.path(heatmapICUDir, paste0(ems, "_loess_ICUcapacity.csv")))) print("CSVs saved")
  
  
  rm(selected_ems,thresholdDat, p1_capacity, p2_modelfit,dtfit, peakdat,  tempdat)
}


f_runHeatmapAnalysis_ems <- function(ems, geography="EMS", enddate="2021-12-31"){
  
  selected_ems <- ems
  
  tempdat <- trajectoriesDat %>%
    filter(Date >= as.Date(interventionstart)) %>%
    getdata(selected_ems) %>%
    filter(outcome == "crit_det") %>%
    as.data.frame()
  
  capacityDat <- load_new_capacity(ems) %>% dplyr::rename(region=geography_name, capacity = icu_available)
  if(length(selected_ems)>1){
    capacityDat <- load_new_capacity(tolower(ems)) %>% dplyr::rename(region=geography_name, capacity = icu_available)
    tempdat$region = regname
  }
  
  tempdat$capacity <- capacityDat$capacity
  
  ### Take weekly average before filtering for maximum value
  library(lubridate)
  
  subdatWklAvr <- tempdat %>%
    dplyr::filter(Date >= reopeningdate & Date <= as.Date(enddate)) %>%
    dplyr::mutate(week = week(Date)) %>%
   # dplyr::group_by( region, week, scen_num, grpvar, detection_success, isolation_success, capacity) %>%
    dplyr::group_by(Date, region, week, scen_num, grpvar, detection_success, isolation_success, capacity) %>%
   # dplyr::summarize(
   #   value = mean(value, na.rm=TRUE),
   #   Date = max(Date)
   # ) %>%
    dplyr::summarize(
      value = mean(value, na.rm=TRUE),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scen_num, grpvar, capacity) %>%
    dplyr::mutate(peak = max(value)) %>%
    f_valuefct_cap(fewerClasses=F)
  
  
  peakdat <- subset(subdatWklAvr, value == peak & !is.na(value_fct)) 
  
  ### Scatter plot  of outcome variable per CT parameter at peak
  showScatter <- TRUE
  if (showScatter) {
    s_plot <-   ggplot(data = peakdat) +
      theme_minimal() +
      geom_point(aes(x = detection_success, y = isolation_success, fill = value_fct), size = 4, alpha = 1, shape = 21) +
      scale_fill_viridis(option = "C", discrete = T, drop = F) +
      facet_wrap(~grpvar, nrow=2) +
      labs(
        title = "",
        subtitle = "",
        x = "Fraction detected",
        y = "Fraction detected that isolate\n",
        fill = "Predicted ICU bed demand\nper 1000 population"
      ) +
      theme(legend.position = "none")+
      geom_hline(yintercept = c(-Inf, Inf))+
      geom_vline(xintercept = c(-Inf, Inf))+
      theme(legend.position = "right",
            panel.spacing = unit(2, "lines"))+
      customTheme
    
    ggsave(paste0(ems, "_scatterplot.png"),
           plot = s_plot, path = file.path(heatmapICUDir), width = 12, height = 10, device = "png"
    )
    
    ggsave(paste0(ems, "_scatterplot.pdf"),
           plot = s_plot, path = file.path(heatmapICUDir), width = 12, height = 10, device = "pdf"
    )
  }
  
  fitlist <- list()
  for (grp in unique(peakdat$grpvar)) {
    # grp  <- unique(peakdat$grpvar)[1]
    #### Do loess regression
    detection_success <- seq(0, 1, 0.005)
    isolation_success <- seq(0, 1, 0.005)
    t_matdat <- expand.grid(detection_success = detection_success, isolation_success = isolation_success)
    
    m <- loess(value ~ detection_success * isolation_success,
               span = 0.7,
               degree = 2, data = subset(peakdat, grpvar == grp)
    )
    
    temp_fit_mat <- predict(m, t_matdat)
    temp_fit <- melt(temp_fit_mat)
    temp_fit$detection_success <- gsub("detection_success=", "", temp_fit$detection_success)
    temp_fit$isolation_success <- gsub("isolation_success=", "", temp_fit$isolation_success)
    
    
    showPlot=FALSE
    if(showPlot){
      library(plotly)
      fig <- plot_ly(
        x = temp_fit$detection_success,
        y = temp_fit$isolation_success,
        z =  temp_fit$value, 
        type = "contour"
      )
      print(fig)
    }
    

    temp_fit$detection_success <- as.numeric(temp_fit$detection_success)
    temp_fit$isolation_success <- as.numeric(temp_fit$isolation_success)
    temp_fit$grpvar <- grp
    temp_fit$capacity <- unique(peakdat$capacity)
    #temp_fit <- na.omit(temp_fit)
    temp_fit <- f_valuefct_cap(temp_fit)
    
    fitlist[[length(fitlist) + 1]] <- temp_fit
    
    rm(temp_fit_mat, temp_fit)
  }
  
  dtfit <- bind_rows(fitlist)
  rm(fitlist)
  
  peakdat <- f_valuefct_cap(peakdat)
  
  ### Extract  minimum isolation_success for each detection_success
  thresholdDat <- dtfit %>%
    dplyr::filter(value <= capacityDat$capacity) %>%
    dplyr::group_by(detection_success, grpvar) %>%
    dplyr::filter(isolation_success == min(isolation_success)) %>%
    dplyr::mutate(region = ems)
  
  ### Plot contour-heatmap plot
  p1 <- ggplot(data = subset(dtfit, !is.na(value_fct)), aes(x = detection_success, y = isolation_success)) +
    theme_minimal() +
    geom_tile(aes(fill = value_fct), alpha = 0.8) +
    # geom_point(data=subset(peakdat, grpvar==0.17),  aes(x = detection_success, y = isolation_success, fill = value_fct, group = scen_num), size = 3, shape = 21, show.legend = FALSE) +
    scale_fill_viridis(option = "C", discrete = T, drop = F) +
    labs(
      x = "detections (%)",
      y = "isolation success (%)",
      col = "",
      fill = "Critical",
      shape = "",
      linetype = ""
    ) +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    facet_wrap(~grpvar, nrow=1) +
    customThemeNoFacet +
    theme(panel.spacing = unit(1.5, "lines"))
  
  p1_capacity <- p1 + geom_line(
    data = subset(thresholdDat, isolation_success != min(isolation_success)),
    aes(x = detection_success, y = isolation_success), size = 1.3
  )
  
  
  p2_modelfit <- p1 + geom_point(
    data = peakdat, aes(x = detection_success, y = isolation_success, fill = value_fct),
    shape = 21, size = 3, show.legend = FALSE
  )+ geom_line(
    data = subset(thresholdDat, isolation_success != min(isolation_success)),
    aes(x = detection_success, y = isolation_success), size = 1.3
  )
  
  
  plotname_capacity <- paste0("Region_", ems, "_ICUcapacity_heatmap")
  plotname_model <- paste0( "Region_", ems, "_ICUcapacity_heatmap_wP")
  
  SAVE_png <- TRUE
  if (SAVE_png) {
    ggsave(paste0(plotname_capacity, ".png"),
           plot = p1_capacity, path = file.path(heatmapICUDir), width = 13, height = 4, device = "png"
    )
    
    
    ggsave(paste0(plotname_model, ".png"),
           plot = p2_modelfit, path = file.path(heatmapICUDir), width = 13, height = 4, device = "png"
    )
  }
  
  
  SAVE_pdf <- TRUE
  if (SAVE_pdf) {
    ggsave(paste0(plotname_capacity, ".pdf"),
           plot = p1_capacity, path = file.path(heatmapICUDir), width = 13, height = 4, device = "pdf"
    )
    
    
    
    ggsave(paste0(plotname_model, ".pdf"),
           plot = p2_modelfit, path = file.path(heatmapICUDir), width = 13, height = 4, device = "pdf"
    )
  }
  
  
  save(dtfit, file = file.path(heatmapICUDir, paste0(ems, "_dtfit.Rdata")))
  if(file.exists(file.path(heatmapICUDir, paste0(ems, "_dtfit.Rdata")))) print("Rdata saved")
  
  
  write.csv(thresholdDat, file = file.path(heatmapICUDir, paste0(ems, "_loess_ICUcapacity.csv")), row.names = FALSE)
  if(file.exists(file.path(heatmapICUDir, paste0(ems, "_loess_ICUcapacity.csv")))) print("CSVs saved")
  
  
  rm(selected_ems,thresholdDat, p1_capacity, p2_modelfit,dtfit, peakdat,  tempdat)
}


#### Run either local (for loop) or on NUCLUSTER

if(!exists("Location")){
  Location=="LOCAL"
  runinBatchMode = FALSE
} 
if(Location=="LOCAL"){
  runinBatchMode = FALSE
} 
if(Location!="LOCAL"){
  runinBatchMode = TRUE
} 


if(runinBatchMode){
  cmd_agrs <- commandArgs()
  length(cmd_agrs)
  
  ems <- cmd_agrs[length(cmd_agrs)]
  task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
  print(task_id)
  print(ems)
  #geography = "EMS"
  # ems <- 1
  geography = "Region"
  emsregions <- names(regions)
  
  ## Load packages
  packages_needed <- c( 'tidyverse','reshape', 'cowplot', 'scales', 'readxl', 'viridis', 'stringr', 'broom') 
  lapply(packages_needed, require, character.only = TRUE) 
  
  ## Load directories and custom objects and functions
  setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
  source("load_paths.R")
  source("processing_helpers.R")
  source("ct_analysis/helper_functions_CT.R")
  
  
  simdate <- "20200731"
  exp_names <- list.dirs(file.path(ct_dir, simdate), recursive = FALSE, full.names = FALSE)
  exp_names <- exp_names[grep("reopen_contact",exp_names)]
  
  for(exp_name in exp_names){
    #  exp_name  <-  exp_names[1]
    source('ct_analysis/loadData_defineParam.R')
    heatmapICUDir <- file.path(exp_dir, "heatmap_ICU")
    
    if(!dir.exists(dir.create(heatmapICUDir)))
      
      for(ems in emsregions ){
        #ems = emsregions[1]
        ## Run analysis
        print(ems)
        f_runHeatmapAnalysis(ems)
      }
    
  }
  
} else {
  
  if (geography == "EMS") {
    emsregions <- c(1:11)
    
    for (ems in emsregions) {
      f_runHeatmapAnalysis_ems(ems)
      
    }
  
  }
  
  if (geography == "Region") {
    
    emsregions <- names(regions)
  # emsregions <- "Illinois"
    
    for (ems in emsregions) {
      f_runHeatmapAnalysis_ems(ems)
      
    }
    

  

  
}

}

