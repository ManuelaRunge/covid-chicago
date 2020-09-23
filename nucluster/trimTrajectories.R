library(tidyverse)
library(readr)
source('load_paths.R')
#simdir = "/home/mrm9534/gitrepos/covid-chicago/_temp/"
simdir = simulation_output
simdate ="20200915"

#exp_names <- list.dirs( file.path(simulation_output,'contact_tracing',simdate), recursive = FALSE, full.names = FALSE)
#exp_names <- exp_names[grep("reopen",exp_names)]
exp_names =c(paste0(simdate, "_IL_RR_baseline_0"),paste0(simdate,"_IL_RR_baseline_1"))

for(exp_name in exp_names){
  expDIR <- file.path(simdir, exp_name )
  trajectoriesDat <- read_csv(file.path(expDIR, "trajectoriesDat.csv"))

  
  region_names <- c("All",paste0("EMS-",c(1:11)))

  ### per restore region
  outcomevars <- c(
    paste0('susceptible_', region_names),
    paste0('infected_' , region_names),
    paste0('recovered_', region_names),
    paste0('infected_cumul_' , region_names),
    paste0('asymp_cumul_' , region_names),
    paste0('asymp_det_cumul_', region_names),
    paste0('symp_mild_cumul_' , region_names),
    paste0('symp_severe_cumul_' , region_names),
    paste0('symp_mild_det_cumul_' , region_names),
    paste0('symp_severe_det_cumul_' , region_names),
    paste0('hosp_det_cumul_', region_names),
    paste0('hosp_cumul_' , region_names),
    paste0('detected_cumul_' , region_names),
    paste0('crit_cumul_' , region_names),
    paste0('crit_det_cumul_' , region_names),
    paste0('death_det_cumul_', region_names),
    paste0('deaths_' , region_names),
    paste0('crit_det_' , region_names),
    paste0('critical_' , region_names),
    paste0('hosp_det_' , region_names),
    paste0('hospitalized_' , region_names)
  )
  
  
  keepvars <- c("time", "startdate", "scen_num","sample_num", "run_num" , outcomevars)

  trajectoriesDat <- trajectoriesDat %>%
    dplyr::select(keepvars) %>%
	filter(time >1)

save(trajectoriesDat, file=file.path(expDIR,  "trajectoriesDat_trim.Rdata"))
write.csv(trajectoriesDat, file = file.path(expDIR, "trajectoriesDat_trim.csv"))
rm(expDIR, trajectoriesDat )
}	



combineExps=FALSE
if(combineExps){

  load(file.path(simdir, exp_names[1] , "trajectoriesDat_trim.Rdata"))
  trajectoriesDat_0 = trajectoriesDat %>% mutate(scen_num_orig = scen_num) %>% mutate(batch="0")
  maxScen1 = max(trajectoriesDat_0$scen_num)
  
  load(file.path(simdir, exp_names[2] , "trajectoriesDat_trim.Rdata"))
  trajectoriesDat_1 = trajectoriesDat %>% mutate(scen_num_orig = scen_num) %>% mutate(scen_num=scen_num + maxScen1 , batch="1")
  
  trajectoriesDat <- rbind(trajectoriesDat_0, trajectoriesDat_1)

  
  length(unique(trajectoriesDat$scen_num))
  summary(trajectoriesDat$scen_num)

  
  save(trajectoriesDat, file=file.path(simdir, paste0(simdate,"_IL_RR_combined_baseline" ),   "trajectoriesDat_trim.Rdata"))
  write.csv(trajectoriesDat, file = file.path(simdir, paste0(simdate,"_IL_RR_combined_baseline") ,  "trajectoriesDat_trim.csv"))
  
  
}
