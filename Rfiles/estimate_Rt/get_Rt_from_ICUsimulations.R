## ============================================================
## R script to get R(t) from simulation outputs
## ============================================================

# install.packages("devtools")
# library(devtools)
# install_github("annecori/EpiEstim", force = TRUE)
library(tidyverse)
library(data.table)
library(cowplot)
library(EpiEstim)


runInBatchMode <- FALSE

if (runInBatchMode) {
  cmd_agrs <- commandArgs()
  length(cmd_agrs)
  exp_name <-  cmd_agrs[length(cmd_agrs)] # cmd_agrs[length(cmd_agrs) - 2]
  Location <-  cmd_agrs[length(cmd_agrs) - 1]
  workingDir <- cmd_agrs[length(cmd_agrs)] #"/home/mrm9534/gitrepos/covid-chicago/Rfiles/"
} else {
  exp_name <- "20201212_IL_regreopen50perc_1daysdelay_pr6"
  Location <-  "Local" 
  workingDir <- getwd()
}
print(exp_name)

setwd(workingDir)
print(workingDir)
source(file.path("load_paths.R"))
source(file.path("estimate_Rt/getRt_function.R"))

run_Rt_estimation <- function(exp_dir, method = "uncertain_si", weekwindow = 13) {
  Rt_list <- list()
  si_list <- list()
  dat <- fread(file.path(exp_dir, paste0("trajectories_aggregated.csv")))
  
  for (region in  c("EMS-1","EMS-4","EMS-11")) {
    
    print(paste0("start processing for ", region))
    
    disease_incidence_data <- dat %>%
      filter(ems == region) %>%
      rename(I = new_infected_median)
    
    count=0
    for (selected_capacity_multiplier in unique(disease_incidence_data$capacity_multiplier)) {
      count = count+1
      disease_incidence_data_sub <- subset(disease_incidence_data, capacity_multiplier==selected_capacity_multiplier)
      res <- getRt(disease_incidence_data_sub, method = method, weekwindow = weekwindow)
      pplot <- plot(res)
      
      if(!dir.exists(file.path(exp_dir, "_plots")))dir.create(file.path(exp_dir, "_plots"))
      ggsave(paste0(region, "_EpiEstim_default_", method, ".pdf"),
             plot = pplot, path = file.path(exp_dir, "_plots"), width = 6, height = 10, dpi = 300, device = "pdf"
      )
      Rt_dat_temp <- res$R %>% mutate(region = region, weekwindow = weekwindow)
      si_dat_temp <- res$SI.Moments %>% mutate(region = region, weekwindow = weekwindow)
      
      Rt_dat_temp$capacity_multiplier <-  selected_capacity_multiplier
      Rt_dat_temp$region <-  region
      
      si_dat_temp$capacity_multiplier <-  selected_capacity_multiplier
      si_dat_temp$region <-  region
      
      if(count==1){
        Rt_dat_All <- Rt_dat_temp
        si_dat_All <- si_dat_temp
      }
      if(count>1){
        Rt_dat_All <- rbind(Rt_dat_All, Rt_dat_temp)
        si_dat_All <- rbind(si_dat_temp, si_dat_temp)
      }
      
    }
    
    Rt_list[[region]] <- as.data.frame(Rt_dat_All)
    si_list[[region]] <- as.data.frame(si_dat_All)
  }
  
  ### Combine list to dataframe
  save(Rt_list, file=file.path(exp_dir, "Rt_list.Rdata"))
  Rt_dat <- Rt_list %>%
    bind_rows() %>%
    dplyr::mutate(time = t_end) %>%
    dplyr::rename(geography_modeled = region) %>%
    dplyr::rename(
      rt_median = `Median(R)`,
      rt_lower = `Quantile.0.025(R)`,
      rt_upper = `Quantile.0.975(R)`
    ) %>%
    mutate(date = as.Date("2020-01-01") + time)
  save(Rt_dat, file = file.path(exp_dir, "Rt_dat.Rdata"))
  fwrite(Rt_dat, file = file.path(exp_dir, "Rt_dat.csv"), row.names = FALSE, quote = FALSE)
  #return(RtdatCombined)
}


#### Run code
#### Experiment settings
expsplit <- strsplit(exp_name, "_")[[1]]
simdate <- expsplit[1]
simulation_output <- file.path(simulation_output,'_overflow_simulations', simdate)

exp_scenario <- expsplit[length(expsplit)]
exp_dir <- file.path(simulation_output, exp_name)

run_Rt_estimation(exp_dir, method = "uncertain_si", weekwindow = 13)

