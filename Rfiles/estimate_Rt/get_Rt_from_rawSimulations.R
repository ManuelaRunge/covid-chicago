## ============================================================
## R script to get R(t) from simulation outputs
## ============================================================

# install.packages("devtools")
# library(devtools)
# install_github("annecori/EpiEstim", force = TRUE)
library(tidyverse)
library(EpiEstim)

runinBatchMode = TRUE


if(runinBatchMode){
  Location = "NUCLUSTER"
  setwd("/projects/p30781/covidproject/covid-chicago/Rfiles/")

  cmd_agrs <- commandArgs()
  length(cmd_agrs)
  ems <- cmd_agrs[length(cmd_agrs)-1]
  exp_name = cmd_agrs[length(cmd_agrs)]
  
  task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
  print(task_id)
  ems <- task_id
} else {
  Location = "Local"
  ems <- "11"
  exp_name = "20200831_IL_regreopen50perc_7daysdelay_sm6"
}


print(ems)

source("load_paths.R")
source("processing_helpers.R")
source("estimate_Rt/getRt_function.R")

if(Location=="NUCLUSTER")simulation_output ="/projects/p30781/covidproject/projects/covid_chicago/cms_sim/simulation_output/"
exp_dir <- file.path(simulation_output, exp_name)

Rt_dir <- file.path(simulation_output, exp_name, "estimatedRt")
if (!dir.exists(Rt_dir)) dir.create(Rt_dir)


### Load simulation outputs
fname =  "trajectoriesDat.csv"
if(!exists("useTrim"))useTrim=TRUE
if(useTrim) fname == "trajectoriesDat_trim.csv"

tempdat <- read.csv(file.path(exp_dir, fname)) %>% 
  dplyr::mutate(
  startdate = as.Date(startdate),
  Date = as.Date(time + startdate)
)

tempdat <- subset(tempdat, capacity_multiplier==1)

colnames(tempdat)[colnames(tempdat)== paste0( "infected_cumul_EMS.",ems)]  = "infected_cumul"
colnames(tempdat)[colnames(tempdat)== paste0( "infected_cumul_EMS-",ems)]  = "infected_cumul"
colnames(tempdat)

tempdat <- tempdat %>% 
  dplyr::mutate(
    startdate = as.Date(startdate),
    Date = as.Date(time + startdate),
  ) %>%
  dplyr::group_by(scen_num) %>%
  dplyr::arrange(scen_num, Date) %>%
  dplyr::mutate(new_infections = infected_cumul - lag(infected_cumul) )


method <- "uncertain_si"
weekwindow=13
Rt_list <- list()
si_list <- list()
count=0
for (scen in unique(tempdat$scen_num)) {
  count = count + 1
  # scen = unique(dat$scen_num)[1]
  disease_incidence_data <- tempdat %>%
    dplyr::filter(scen_num == scen) %>%
    dplyr::rename(I = new_infections) %>%
    dplyr::mutate(I = ifelse(I <0,0,I)) %>%
    dplyr::select(Date, I ,  infected_cumul) %>%
    dplyr::filter(!is.na(I))
  
  
  res <- getRt(disease_incidence_data, method=method, weekwindow=weekwindow)
  

  Rt_tempdat  <- res$R %>% mutate(region = ems, weekwindow=weekwindow )
  Rt_tempdat$scen_num = scen
  
  if(count==1)Rt_tempdat_All  <- Rt_tempdat
  if(count!=1)Rt_tempdat_All  <- rbind(Rt_tempdat_All,Rt_tempdat)
  
  SI_tempdat  <- res$SI.Moments %>% mutate(region = ems, weekwindow=weekwindow )
  SI_tempdat$scen_num = scen
  
  if(count==1)SI_tempdat_All  <- SI_tempdat
  if(count!=1)SI_tempdat_All  <- rbind(SI_tempdat_All,SI_tempdat) 
  
  rm(Rt_tempdat, SI_tempdat)
}

save(Rt_tempdat_All, file=file.path(Rt_dir, paste0(ems,"_estimated_Rt.Rdata")))



