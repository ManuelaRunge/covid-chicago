## --------------------------------------------
## Generate heatmap and supplementary plots per EMS or aggregated region
### Using loess regression per EMS and grpvar
## --------------------------------------------


regions <- list(
  "Northcentral" = c(1, 2),
  "Northeast" = c(7, 8, 9, 10, 11),
  "Central" = c(3, 6),
  "Southern" = c(4, 5),
  "Illinois" = c(1:11)
)



f_runHeatmapAnalysis <- function(ems, geography="Region"){
  
  # ems <- emsregions[1]
  if (geography == "Region") {
    selected_ems <- as.numeric(regions[[ems]])
    regname = ems
  } else {
    selected_ems <- ems
  }
  
 
  tempdat <- trajectoriesDat %>%
    filter(time >= as.Date(reopeningdate) - as.Date(max(startdate)) - 30) %>%
    getdata(selected_ems) %>%
    filter(outcome == "critical") %>%
    as.data.frame()
  
  capacity <- load_capacity(ems) %>% dplyr::rename(capacity = critical)
  if(length(selected_ems)>1){
  capacity <- load_capacity(tolower(ems)) %>% dplyr::rename(capacity = critical)
  tempdat$region = regname
  }
  
  tempdat$capacity <- capacity$capacity
  
  ### Identify peak at least 10 days after reopening
  peakTimes <- tempdat %>%
    filter(Date > min(Date) + 10) %>%
    dplyr::group_by(N, Ki, region, isolation_success, detection_success, grpvar, scen_num, sample_num, run_num) %>%
    dplyr::filter(value == max(value)) %>%
    dplyr::rename("Date_peak" = Date) %>%
    dplyr::select(N, Ki, region, Date_peak, outcome, isolation_success, detection_success, grpvar, scen_num, sample_num, run_num)
  
  ## Add peak date to plotdat
  tempdat <- tempdat %>%
    left_join(peakTimes, by = c(
      "N", "Ki", "region", "outcome", "scen_num", "sample_num",
      "run_num", "isolation_success", "detection_success", "grpvar"
    ))
  
  peakdat <- tempdat %>% filter(Date == tempdat$Date_peak)
  
  ### Scatter plot  of outcome variable per CT parameter at peak
  showScatter <- TRUE
  if (showScatter) {
    scatterplot <- tempdat %>%
      filter(Date == Date_peak) %>%
      mutate(
        belowCapacity = ifelse(value <= capacity, "yes", "no")
      ) %>%
      ggplot() +
      theme_minimal() +
      geom_point(aes(x = detection_success, y = isolation_success, fill = value, group = scen_num), size = 3, shape = 21) +
      scale_fill_viridis(option = "C", discrete = FALSE, direction = -1) +
      customThemeNoFacet
    
    ggsave(paste0(ems, "_scatterplot.pdf"),
           plot = scatterplot, path = file.path(ems_dir), width = 5, height = 4, device = "pdf"
    )
  }
  # plotdat %>% filter(Date == plotdat$Date_peak) %>% write.csv(file.path(ems_dir,paste0(ems, "_scatterplot_dat.csv")), row.names = FALSE)
  

  fitlist <- list()
  for (grp in unique(peakdat$grpvar)) {
    # grp  <- unique(peakdat$grpvar)[1]
    #### Do loess regression
    detection_success <- seq(0, 1, 0.001)
    isolation_success <- seq(0, 1, 0.001)
    t_matdat <- expand.grid(detection_success = detection_success, isolation_success = isolation_success)
    
    m <- loess(value ~ detection_success * isolation_success,
               span = 0.8,
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
    
    temp_fit <- f_valuefct(temp_fit)
    
    temp_fit$detection_success <- as.numeric(temp_fit$detection_success)
    temp_fit$isolation_success <- as.numeric(temp_fit$isolation_success)
    
    temp_fit$grpvar <- grp
    fitlist[[length(fitlist) + 1]] <- temp_fit
    
    rm(temp_fit_mat, temp_fit)
  }
  
  
  dtfit <- bind_rows(fitlist)
  rm(fitlist)
  
  peakdat <- f_valuefct(peakdat)
  
  ### Extract  minimum isolation_success for each detection_success
  thresholdDat <- dtfit %>%
    filter(value <= capacity$capacity) %>%
    group_by(detection_success, grpvar) %>%
    filter(isolation_success == min(isolation_success)) %>%
    mutate(region = ems)
    
  ### Plot contour-heatmap plot
  p1 <- ggplot(data = subset(dtfit, !is.na(value_fct)), aes(x = detection_success, y = isolation_success)) +
    theme_minimal() +
    geom_tile(aes(fill = value_fct), alpha = 0.8) +
    # geom_point(data=subset(peakdat, grpvar==0.17),  aes(x = detection_success, y = isolation_success, fill = value_fct, group = scen_num), size = 3, shape = 21, show.legend = FALSE) +
    scale_fill_viridis(option = "C", discrete = TRUE) +
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
  )
  
  
  plotname_capacity <- paste0(geography, "_", ems, "_ICUcapacity_heatmap_loess")
  plotname_model <- paste0(geography, "_", ems, "_heatmap_with_points_loess")
  
  
  SAVE_png <- TRUE
  if (SAVE_png) {
    ggsave(paste0(plotname_capacity, ".png"),
           plot = p1_capacity, path = file.path(ems_dir), width = 12, height = 4, device = "png"
    )
    
    
    ggsave(paste0(plotname_model, ".png"),
           plot = p2_modelfit, path = file.path(ems_dir), width = 12, height = 4, device = "png"
    )
  }
  
  
  SAVE_pdf <- TRUE
  if (SAVE_pdf) {
    ggsave(paste0(plotname_capacity, ".pdf"),
           plot = p1_capacity, path = file.path(ems_dir), width = 12, height = 4, device = "pdf"
    )
    
    
    
    ggsave(paste0(plotname_model, ".pdf"),
           plot = p2_modelfit, path = file.path(ems_dir), width = 12, height = 4, device = "pdf"
    )
  }
  
  
  write.csv(thresholdDat, file = file.path(ems_dir, paste0(ems, "_loess_ICUcapacity.csv")), row.names = FALSE)
  
  if(file.exists(file.path(ems_dir, paste0(ems, "_loess_ICUcapacity.csv")))) print("CSVs saved")
  
}


#### Run either local (for loop) or on NUCLUSTER

if(!exists("LOCAL")){
  runinBatchMode = TRUE
} else {
  runinBatchMode = FALSE
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
  
  for(ems in emsregions ){
  #ems = emsregions[1]
  ## Run analysis
  print(ems)
  f_runHeatmapAnalysis(ems)
  }
  
  }
  
} else {
  
  if (geography == "EMS") emsregions <- c(1:11)
  if (geography == "Region") emsregions <- names(regions)
  # emsregions <- "Illinois"
  
  for (ems in emsregions) {
    
    f_runHeatmapAnalysis(ems)
    
  }
  
  
}



