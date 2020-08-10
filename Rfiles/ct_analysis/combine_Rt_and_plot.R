## ============================================================
## Combine and plot Rt estimates 
## ============================================================



regions <- list(
  "Northcentral" = c(1, 2),
  "Northeast" = c(7, 8, 9, 10, 11),
  "Central" = c(3, 6),
  "Southern" = c(4, 5),
  "Illinois" = c(1:11)
)


f_addRestoreRegion <- function(dat){
  
  dat$restore_region <- NA
  dat$restore_region[dat$region %in%  regions$Northcentral ] <- "Northcentral"
  dat$restore_region[dat$region %in%  regions$Northeast ] <- "Northeast"
  dat$restore_region[dat$region %in%  regions$Central ] <- "Central"
  dat$restore_region[dat$region %in%  regions$Southern ] <- "Southern"
  
  
  return(dat)
  
}


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
  load(file.path(Rt_dir, "1_estimated_Rt.Rdata"))
  Rt_dat <- Rt_tempdat_All
  rm(Rt_tempdat_All)
  
  for (i in c(2:11)) {
    load(file.path(Rt_dir, paste0(i, "_estimated_Rt.Rdata")))
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
  save(Rt_dat2, file = file.path(Rt_dir, paste0("EMS_combined_estimated_Rt.Rata")))
  
  return(Rt_dat2)
}


f_Rt_descriptive_plots <- function(Rt_dat2){
  
  #Rt_dat2 <- read.csv(file.path(Rt_dir,"EMS_combined_estimated_Rt.csv" ))
  
  summary(Rt_dat2$Date)
  summary(Rt_dat2$t_start)
  
 
  ### Readjust date for 2 weeks estimation of Rt (to do double check)
  #Rt_dat2$Date <- Rt_dat2$Date+14 # TODO check date!! 
  
  Rt_dat2$region_label <- factor(Rt_dat2$region, levels = c(1:11), labels = paste0("EMS ", c(1:11), "\n"))
  
  Rt_dat2 <- Rt_dat2 %>%    f_addRestoreRegion()
  
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
  
  
  ggsave(paste0( "_Rt_over_time.png"),
         plot = pplot, path = file.path(exp_dir), width = 12, height = 7,  device = "png"
  )
    ggsave(paste0( "_Rt_over_time.pdf"),
         plot = pplot, path = file.path(exp_dir), width = 12, height = 7,  device = "pdf"
  )


  
  pplot <- Rt_dat2 %>%
    filter(Date >= "2020-03-01" & Date < as.Date("2020-09-01"))%>%
    group_by(Date,restore_region) %>%
    summarize(Mean=mean(Mean)) %>%
    ggplot() + 
    theme_minimal() +
    geom_line(aes(x = Date, y = Mean), col = "deepskyblue3", size = 0.7) +
    scale_x_date(breaks = "2 weeks", labels = date_format("%b%d")) +
    geom_hline(yintercept = 1) +
    facet_wrap(~restore_region) +
    scale_y_continuous(lim = c(0.5, 1.5)) +
    labs(y = expression(italic(R[t])), x = "") +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  
  
  ggsave(paste0( "Rt_over_time_restoreRegion.png"),
         plot = pplot, path = file.path(exp_dir), width = 12, height = 7,  device = "png"
  )
    ggsave(paste0( "Rt_over_time_restoreRegion.pdf"),
         plot = pplot, path = file.path(exp_dir), width = 12, height = 7,  device = "pdf"
  )


  pplot <- Rt_dat2 %>%
    filter(Date >= "2020-03-01" & Date < as.Date("2020-09-01"))%>%
    group_by(Date,restore_region, scen_num) %>%
    summarize(Mean=mean(Mean)) %>%
    ggplot() + 
    theme_minimal() +
    geom_line(aes(x = Date, y = Mean, group=scen_num), col = "deepskyblue3", size = 0.7) +
    scale_x_date(breaks = "2 weeks", labels = date_format("%b%d")) +
    geom_hline(yintercept = 1) +
    facet_wrap(~restore_region) +
    scale_y_continuous(lim = c(0.5, 1.5)) +
    labs(y = expression(italic(R[t])), x = "") +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  
  
  ggsave(paste0( "Rt_over_time_restoreRegion_2.png"),
         plot = pplot, path = file.path(exp_dir), width = 12, height = 7,  device = "png"
  )
    ggsave(paste0( "Rt_over_time_restoreRegion_2.pdf"),
         plot = pplot, path = file.path(exp_dir), width = 12, height = 7,  device = "pdf"
  )
  

  df <- Rt_dat2 %>%
    dplyr::filter(Date >= as.Date("2020-07-01") & Date < as.Date("2020-08-01")) %>%
    dplyr::group_by(region, Date, t_start, scen_num, t_end, isolation_success, detection_success, grpvar) %>%
    dplyr::summarize(average_median_Rt = mean(Mean)) %>%
    mutate(Rt_fct = ifelse(average_median_Rt < 1, "<1", ">=1"))%>%
    f_addRestoreRegion()
  
  df$region_label <- factor(df$region, levels = c(1:11), labels = paste0("EMS_", c(1:11), "\n"))
  df$capacity <- 1

  pplot <- ggplot(data = subset(df, grpvar ==min(grpvar))) +
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
    facet_wrap(grpvar~region_label) +
    theme(panel.spacing = unit(1.5, "lines"))
  
  ggsave(paste0( "_Rt_sample_scatterplot.png"),
         plot = pplot, path = file.path(exp_dir), width = 14, height = 10,  device = "png"
  )
    ggsave(paste0( "_Rt_sample_scatterplot.pdf"),
         plot = pplot, path = file.path(exp_dir), width = 14, height = 10, device = "pdf"
  )
  
  
  
  pplot <- df %>%
    f_addRestoreRegion() %>%
    dplyr::group_by(Date, t_start, scen_num,  t_end, isolation_success, detection_success, grpvar, restore_region) %>% 
    dplyr::summarize(average_median_Rt=mean(average_median_Rt)) %>%
    dplyr::mutate(Rt_fct = ifelse(average_median_Rt < 1, "<1", ">=1"))   %>%
    ggplot() +
    theme_classic() +
    geom_point(aes(x = detection_success, y = isolation_success, fill = Rt_fct), shape = 21, size = 2) +
    scale_fill_viridis(option = "C", discrete = TRUE, direction = -1) +
    scale_color_viridis(option = "C", discrete = TRUE, direction = -1) +
    customThemeNoFacet +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    theme(panel.spacing = unit(2, "lines")) +
    labs(
      color = "Rt",
      subtitle = "",
      fill = groupVar_label,
      x = detectionVar_label,
      y = isolationVar_label
    ) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    theme(legend.position = "right") +
    facet_wrap(grpvar~restore_region) +
    theme(panel.spacing = unit(1.5, "lines"))
  
  ggsave(paste0( "restore_region_Rt_sample_scatterplot.png"),
         plot = pplot, path = file.path(exp_dir), width = 14, height = 10,  device = "png"
  )
    ggsave(paste0( "restore_region_Rt_sample_scatterplot.pdf"),
         plot = pplot, path = file.path(exp_dir), width = 14, height = 10, device = "pdf"
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
  simdate = cmd_agrs[length(cmd_agrs)-1]
  exp_pattern = cmd_agrs[length(cmd_agrs)]
  
  ## Load packages
  packages_needed <- c( 'tidyverse','reshape', 'cowplot', 'scales', 'readxl', 'viridis', 'stringr', 'broom') 
  lapply(packages_needed, require, character.only = TRUE) 
  
  ## Load directories and custom objects and functions
  setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
  source("load_paths.R")
  source("processing_helpers.R")
  source("ct_analysis/helper_functions_CT.R")
  
  exp_names <- list.dirs(file.path(ct_dir, simdate), recursive = FALSE, full.names = FALSE)
  exp_names <- exp_names[grep(exp_pattern,exp_names)]
  #exp_names ="20200731_IL_reopen_counterfactual"
 }
  
for(exp_name in exp_names){

  source('ct_analysis/loadData_defineParam.R')
  if(!file.exists(file.path(Rt_dir,"11_estimated_Rt.Rdata" ))) next
 
  ### Combine files
  #if(!file.exists(file.path(Rt_dir,"EMS_combined_estimated_Rt.csv" )))
  Rt_dat2 <- f_combineRtdats()

### Descriptive plots 
f_Rt_descriptive_plots(Rt_dat2=Rt_dat2)

}







