
Location = "LOCAL"

library(tidyverse)
library(cowplot)
library(scales)
library(viridis)

source("load_paths.R")
source("processing_helpers.R")


dat <- read.csv("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/20200807_IL_reopen10_Rollback4/trajectoriesDat.csv")


colnames(dat) <- gsub("EMS.","EMS-",colnames(dat))
table(dat$socialDistance_rollback_time)


if(saveSub){
  simdate = "20200807_IL_reopen10_Rollback4"
  simdir = "C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/"
  
  dir.create(file.path(simdir, "20200807_IL_reopen10_Rollback_sub1" ))
  dir.create(file.path(simdir, "20200807_IL_reopen10_Rollback_sub2" ))
  dir.create(file.path(simdir, "20200807_IL_reopen10_Rollback_sub3" ))
  dir.create(file.path(simdir, "20200807_IL_reopen10_Rollback_sub4" ))
  
  dat_sub1 = subset(dat,socialDistance_rollback_time==184 )
  dat_sub2 = subset(dat,socialDistance_rollback_time==199 )
  dat_sub3 = subset(dat,socialDistance_rollback_time==215 )
  dat_sub4 = subset(dat,socialDistance_rollback_time==230 )
  
  write.csv(dat_sub1, file.path(simdir,"20200807_IL_reopen10_Rollback_sub1", "trajectoriesDat.csv" ), row.names = FALSE, delim = ";")
  write.csv(dat_sub2, file.path(simdir,"20200807_IL_reopen10_Rollback_sub2", "trajectoriesDat.csv" ), row.names = FALSE, delim = ";")
  write.csv(dat_sub3, file.path(simdir,"20200807_IL_reopen10_Rollback_sub3", "trajectoriesDat.csv" ), row.names = FALSE, delim = ";")
  write.csv(dat_sub4, file.path(simdir,"20200807_IL_reopen10_Rollback_sub4", "trajectoriesDat.csv" ), row.names = FALSE, delim = ";")
  
  
}




EMSvars <- colnames(dat)[ grep("EMS-", colnames(dat))]
keepvars <- c("time", "startdate", "socialDistance_rollback_time", EMSvars)


subdat <- dat %>%
  select(keepvars) %>%
  mutate(date = as.Date(startdate) + time) %>%
  pivot_longer(cols = -c("time", "date", "startdate"), names_to = "region") %>%
  mutate(
    region = gsub(emsname, "", gsub(paramname, "", region)),
    region = as.numeric(region),
    exp_name = exp_name,
  ) %>%
  group_by(date, region,exp_name) %>%
  summarize(value = mean(value))

