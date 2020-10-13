library(tidyverse)
library(data.table)

exp_name <- "20201010_IL_run_fitting_567"
exp_dir <- file.path("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output","forFitting",exp_name)

#load(file.path(exp_dir,"trajectoriesDat.Rdata"))

trajectoriesDat <- fread(file.path(exp_dir,"trajectoriesDat.csv"))
  
f_save_per_grp <- function(dat, i,SAVE="csv"){
  
  fittingParam <-  colnames(dat)[c(grep("social",tolower(colnames(dat))),grep("ki",tolower(colnames(dat))))]
  fittingParam <- fittingParam[c(grep("multiplier",tolower(fittingParam)),grep("time",tolower(fittingParam)))]
  fittingParam <- fittingParam[!(grepl("EMS",fittingParam))]
  
  
  outcomeParam <- paste0(c("death_det_cumul", "crit_det", "hosp_det", "hosp_det_cumul", "infected_cumul"), paste0("_EMS-", i))
  KeepCols <- c("time", "startdate", "scen_num", "sample_num", fittingParam, outcomeParam)
  
  subdat <- dat %>% select_at(.vars=KeepCols)
 if(tolower(SAVE)=="csv")fwrite(subdat, file.path(exp_dir, paste0("trajectoriesDat_region_",i,".csv")))
  if(tolower(SAVE)=="rdata")save(subdat,file=file.path(exp_dir, paste0("trajectoriesDat_region_",i,".RData")))
}


for(i in c(1:11)){
  f_save_per_grp(dat=trajectoriesDat, i,SAVE="RData")
}