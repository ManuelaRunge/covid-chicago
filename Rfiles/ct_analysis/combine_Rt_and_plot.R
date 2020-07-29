## ============================================================
## Combine and plot Rt estimates 
## ============================================================


f_combineRtdats <- function(){
  ### add variables from simulation dataset
  selected_ems <- c(1:11)
  emsvars_temp <- c(paste0('critical', "_EMS."), "N_EMS_", "Ki_EMS_")
  
  emsvars <- NULL
  for (ems in selected_ems) {
    emsvars <- c(emsvars, paste0(emsvars_temp, ems))
  }
  
  
  groupvars <- c("time", "Date", "startdate", "scen_num", "sample_num", "run_num", "grpvar", "detection_success", "isolation_success")
  (keepvars <- c(groupvars, emsvars))
  
  #source('ct_analysis/loadData_defineParam.R')
  dat <- trajectoriesDat %>%
    dplyr::select(Date, scen_num, isolation_success, detection_success, grpvar) %>%
    dplyr::arrange(Date) %>%
    dplyr::group_by(scen_num) %>%
    dplyr::mutate(date = as.Date(Date), time = c(1:n_distinct(date))) %>%
    unique()
  
  
  # #### Combine Rt estimates per region
  load(file.path(Rt_dir, "1_temp_Rt_tempdat_All.Rdata"))
  Rt_dat <- Rt_tempdat_All
  rm(Rt_tempdat_All)
  
  for (i in c(2:11)) {
    load(file.path(Rt_dir, paste0(i, "_temp_Rt_tempdat_All.Rdata")))
    Rt_dat <- rbind(Rt_dat, Rt_tempdat_All)
    rm(Rt_tempdat_All)
  }
  
  
  # ### Edit dataframe
  Rt_dat2 <- Rt_dat %>%
    merge(unique(dat[, c("time", "Date", "scen_num", "isolation_success", "detection_success", "grpvar")]),
          by.x = c("t_start", "scen_num"), by.y = c("time", "scen_num")
    )
  
  colnames(Rt_dat2) <- gsub("[(R]", "", colnames(Rt_dat2))
  colnames(Rt_dat2) <- gsub("[)]", "", colnames(Rt_dat2))
  
  Rt_dat2 <- Rt_dat2 %>% mutate(meanRtLE1 = ifelse(Median < 1, 1, 0),
                                Date = as.Date(Date))
  

  write.csv(Rt_dat2, file = file.path(Rt_dir, paste0("EMS_combined_estimated_Rt.csv")), row.names = FALSE)
}


f_Rt_descriptive_plots <- function(){
  
  Rt_dat2 <- read.csv(file.path(Rt_dir,"EMS_combined_estimated_Rt.csv" ))
  
  summary(Rt_dat2$Date)
  summary(Rt_dat2$t_start)
  
  df <- Rt_dat2 %>%
    dplyr::filter(Date >= as.Date("2020-07-01") & Date < as.Date("2020-08-01")) %>%
    dplyr::group_by(region, region_label, Date, t_start, scen_num, t_end, isolation_success, detection_success, grpvar) %>%
    dplyr::summarize(average_median_Rt = mean(Mean)) %>%
    mutate(Rt_fct = ifelse(average_median_Rt < 1, "<1", ">=1"))
  
  df$region_label <- factor(df$region, levels = c(1:11), labels = paste0("EMS_", c(1:11), "\n"))
  df$capacity <- 1
  
  summary(df$average_median_Rt)
  summary(df$Date)
  tapply(df$average_median_Rt, df$region, summary)
  
  ### Readjust date for 2 weeks estimation of Rt (to do double check)
  #Rt_dat2$Date <- Rt_dat2$Date+14 # TODO check date!! 
  
  Rt_dat2$region_label <- factor(Rt_dat2$region, levels = c(1:11), labels = paste0("EMS ", c(1:11), "\n"))
  
  pplot <- ggplot(data = subset(Rt_dat2, Date >= "2020-05-01" & Date < as.Date("2020-09-01"))) +
    theme_minimal() +
    geom_vline(xintercept = c(as.Date("2020-07-01"), as.Date("2020-08-01")), linetype = "dashed", size = 0.7, col = "darkgrey") +
    geom_line(aes(x = Date, y = Mean, group = scen_num), col = "deepskyblue3", size = 0.7) +
    # geom_smooth(aes(x=Date, y =Mean ),col="darkred") +
    scale_x_date(breaks = "2 weeks", labels = date_format("%b%d")) +
    geom_hline(yintercept = 1) +
    facet_wrap(~region_label) +
    scale_y_continuous(lim = c(0.5, 1.5)) +
    labs(y = expression(italic(R[t])), x = "") +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  
  
  ggsave(paste0(selected_outcome, "_Rt_over_time.png"),
         plot = pplot, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
  )
  

  pplot <- ggplot(data = subset(df, grpvar == 0.17)) +
    theme_classic() +
    geom_point(aes(x = detection_success, y = isolation_success, fill = Rt_fct), shape = 21, size = 2) +
    scale_fill_viridis(option = "C", discrete = TRUE, direction = -1) +
    scale_color_viridis(option = "C", discrete = TRUE, direction = -1) +
    customThemeNoFacet +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    theme(panel.spacing = unit(2, "lines")) +
    labs(
      color = groupVar_label,
      subtitle = "",
      fill = groupVar_label,
      x = detectionVar_label,
      y = isolationVar_label
    ) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    theme(legend.position = "right") +
    facet_wrap(~region_label) +
    theme(panel.spacing = unit(1.5, "lines"))
  
  ggsave(paste0(selected_outcome, "_Rt_sample_scatterplot.png"),
         plot = pplot, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
  )
  
  
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
  
  exp_name <- cmd_agrs[length(cmd_agrs)]
  task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
  print(task_id)
  print(exp_name)

  ## Load packages
  packages_needed <- c( 'tidyverse','reshape', 'cowplot', 'scales', 'readxl', 'viridis', 'stringr', 'broom') 
  lapply(packages_needed, require, character.only = TRUE) 
  
  ## Load directories and custom objects and functions
  setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
  source("load_paths.R")
  source("processing_helpers.R")
  source("ct_analysis/helper_functions_CT.R")
  
  
  simdate <- "20200728"
  #exp_name <- list.dirs(file.path(ct_dir, simdate), recursive = FALSE, full.names = FALSE)[1]
  source('ct_analysis/loadData_defineParam.R')
  

}


### Combine files
if(!file.exists(file.path(Rt_dir,"EMS_combined_estimated_Rt.csv" )))f_combineRtdats()

### Descriptive plots 
f_Rt_descriptive_plots()



