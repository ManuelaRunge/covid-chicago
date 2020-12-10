library(tidyverse)
library(cowplot)
library(data.table)
library(raster)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")

f_save_ICU_peak <- function(exp_dir, counterfactual=FALSE, filter_capacity_multiplier=FALSE){
  
  dat1 <-  fread(file.path(exp_dir, "trajectories_aggregated.csv")) 

  if(counterfactual){
    ICU_peak <- dat1 %>% filter(date >= as.Date('2020-10-01') , date <= as.Date("2020-12-31")) %>% 
      group_by(ems, capacity_multiplier) %>% 
      filter(crit_det_median == max(crit_det_median)) %>% 
      filter(date == min(date)) %>% 
      dplyr::select(ems,date, capacity_multiplier, crit_det_median, crit_det_50CI_lower, crit_det_50CI_upper, crit_det_95CI_lower, crit_det_95CI_upper) %>%
      mutate(geography_name =gsub("All","illinois",gsub("EMS-","",ems))) %>%
      left_join(load_new_capacity( filedate = '20200915'), by='geography_name') %>%
      mutate(perc_ICU_occup_median =( crit_det_median /icu_available) *100  )
  }else{
    ICU_peak <- dat1 %>% filter(date >= as.Date('2020-10-01') , date <= as.Date("2020-12-31")) %>% 
      group_by(ems, capacity_multiplier) %>% 
      filter(crit_det_median == max(crit_det_median)) %>% 
      filter(date == min(date)) %>% 
      dplyr::select(ems,date, capacity_multiplier, crit_det_median, crit_det_50CI_lower, crit_det_50CI_upper, crit_det_95CI_lower, crit_det_95CI_upper) %>%
     # filter(capacity_multiplier >=0.88 & capacity_multiplier<1) %>%
      mutate(geography_name =gsub("All","illinois",gsub("EMS-","",ems))) %>%
      left_join(load_new_capacity( filedate = '20200915'), by='geography_name') %>%
      mutate(perc_ICU_occup_median =( crit_det_median /icu_available) *100  )
  }
  
  return(ICU_peak)

}

f_save_ICU_peak_past <- function(exp_dir, counterfactual=FALSE, filter_capacity_multiplier=FALSE){
  
  dat1 <-  fread(file.path(exp_dir, "trajectories_aggregated.csv")) 
  ICU_peak_past <- dat1 %>% filter(date >= as.Date('2020-01-01') , date < as.Date("2020-09-01")) %>% 
    group_by(ems, date) %>% 
    summarize(crit_det_median =mean(crit_det_median),
              crit_det_50CI_lower =mean(crit_det_50CI_lower),
              crit_det_50CI_upper =mean(crit_det_50CI_upper),
              crit_det_95CI_lower =mean(crit_det_95CI_lower),
              crit_det_95CI_upper =mean(crit_det_95CI_upper)) %>% 
    group_by(ems) %>% 
    filter(crit_det_median == max(crit_det_median)) %>% 
    filter(date == min(date)) %>% 
    dplyr::select(ems, date, crit_det_median, crit_det_50CI_lower, crit_det_50CI_upper, crit_det_95CI_lower, crit_det_95CI_upper) %>%
    mutate(geography_name =gsub("All","illinois",gsub("EMS-","",ems))) %>%
    left_join(load_new_capacity( filedate = '20200915'), by='geography_name') %>%
    mutate(perc_ICU_occup_median =( crit_det_median /icu_available) *100  )
  
  return(ICU_peak_past)
  
}


## -------------------------------
## Run script 
## -------------------------------

#sim_dir <- file.path(simulation_output,'_overflow_simulations')
simdate <-'20200919'
simdate <-'20201121'
sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen",exp_names)]
exp_names <- exp_names[c(grep("daysdelay",exp_names),grep("counterfactual",exp_names))]


for(exp_name in exp_names){
  
  exp_dir <- file.path(sim_dir, exp_name)
  if(!file.exists(file.path(exp_dir, "trajectories_aggregated.csv")))next
  
  is.counterfactual=FALSE
  if(length(grep("counterfactual",exp_name ))>0)is.counterfactual=TRUE
  ICU_peak <-f_save_ICU_peak(exp_dir,counterfactual=is.counterfactual)
  fwrite(ICU_peak, file.path(exp_dir,'ICU_peak.csv')) 
  
  #ICU_peak_past <-f_save_ICU_peak_past(exp_dir)
  #fwrite(ICU_peak_past, file.path(exp_dir,'ICU_peak_past.csv')) 
  
}

