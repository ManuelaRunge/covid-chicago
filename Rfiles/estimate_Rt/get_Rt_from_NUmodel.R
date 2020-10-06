## ============================================================
## R script to get R(t) from simulation outputs
## ============================================================

# install.packages("devtools")
# library(devtools)
# install_github("annecori/EpiEstim", force = TRUE)
library(tidyverse)
library(EpiEstim)
library(data.table)

runinBatchMode = FALSE


if(runinBatchMode){
  cmd_agrs <- commandArgs()
  length(cmd_agrs)
  ems <- cmd_agrs[length(cmd_agrs)]
  
  task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
  print(task_id)
  ems <- task_id
  
  setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
} else {
  ems <- "11"
}

print(ems)


source("load_paths.R")
source("processing_helpers.R")
source("estimate_Rt/getRt_function.R")

exp_name = "20201001_NU_RR_NU_low_effective_0"
#exp_name = "20201001_NU_RR_NU_high_effective_0"
exp_dir <- file.path(simulation_output, exp_name)

Rt_dir <- file.path("C:/Users/mrm9534/Box/MR_archive/NU/", exp_name)
if (!dir.exists(Rt_dir)) dir.create(Rt_dir)


### Load simulation outputs
trajectoriesDat <- fread(file.path(exp_dir, "trajectoriesDat.csv"), 
                 select = c('time','startdate','Ki_NU', 'scen_num','infected','infected_campus','infected_evanston','infected_unofficial'))


tempdat_full <- trajectoriesDat %>% 
  dplyr::mutate(
    startdate = as.Date(startdate),
    Date = as.Date(time + startdate)
  ) %>%
  rename(infected_all = infected)%>%
  pivot_longer(cols=-c('time','startdate','Ki_NU','Date', 'scen_num')) %>%
  separate(name, into=c("outcome","NUgrp")) %>%
  rename(infected = value)%>%
  dplyr::group_by(scen_num,Ki_NU, NUgrp) %>%
  dplyr::arrange(scen_num, Ki_NU,NUgrp, Date) %>%
  dplyr::mutate(infected_cumul =  cumsum(infected)) %>%
  dplyr::mutate(new_infections = infected_cumul- lag(infected_cumul)) 
    
#ggplot(data=tempdat_full) + geom_smooth(aes(x=Date, y=new_infections, group=Ki_NU))+facet_wrap(~NUgrp, scales="free")



tempdat <- tempdat_full %>% 
  dplyr::group_by(Date, Ki_NU, NUgrp) %>%
  dplyr::summarize(new_infections = mean(new_infections, na.rm=TRUE),
                   infected_cumul = mean(infected_cumul, na.rm=TRUE))

ggplot(data=tempdat) + geom_line(aes(x=Date, y=new_infections, col=as.factor(Ki_NU), group=Ki_NU))+facet_wrap(~NUgrp, scales="free")



for (i in unique(tempdat$NUgrp)) {
 
  
  method <- "uncertain_si"
  weekwindow=13
  
  Rt_list <- list()
  si_list <- list()
  count=0 
for (j in unique(tempdat$Ki_NU)) {
  count = count + 1
  # scen = unique(tempdat$reopening_multiplier_4)[1]
  disease_incidence_data <- tempdat %>%
    dplyr::filter( NUgrp == i) %>%
    dplyr::filter( Ki_NU == j) %>%
    dplyr::rename(I = new_infections) %>%
    dplyr::mutate(I = ifelse(I <0,0,I)) %>%
    dplyr::select(Date, I ,  infected_cumul) %>%
    dplyr::filter(!is.na(I))
  
  res <- getRt(disease_incidence_data, method=method, weekwindow=weekwindow)

  Rt_tempdat  <- res$R %>% mutate(weekwindow=weekwindow )
  Rt_tempdat$Ki_NU = j
  Rt_tempdat$NUgrp = i
  
  if(count==1)Rt_tempdat_All  <- Rt_tempdat
  if(count!=1)Rt_tempdat_All  <- rbind(Rt_tempdat_All,Rt_tempdat)
  
  SI_tempdat  <- res$SI.Moments %>% mutate( weekwindow=weekwindow )
  SI_tempdat$Ki_NU = j
  SI_tempdat$NUgrp = i
  
  if(count==1)SI_tempdat_All  <- SI_tempdat
  if(count!=1)SI_tempdat_All  <- rbind(SI_tempdat_All,SI_tempdat) 
  
  rm(Rt_tempdat, SI_tempdat)
}

save(Rt_tempdat_All, file=file.path(Rt_dir, paste0(i,"_estimated_Rt.Rdata")))

}




##### Combine and compare



exp_name = "20201001_NU_RR_NU_low_effective_0"
exp_name = "20201001_NU_RR_NU_high_effective_0" 
Rt_dir1 <- file.path("C:/Users/mrm9534/Box/MR_archive/NU/", "20201001_NU_RR_NU_low_effective_0")
Rt_dir2 <- file.path("C:/Users/mrm9534/Box/MR_archive/NU/", "20201001_NU_RR_NU_high_effective_0" )

sim1 <- list.files(Rt_dir1)
sim2 <- list.files(Rt_dir2)


datlist <- list()
for(i in sim1){
  load(file.path(Rt_dir1,i))
  datlist[[length(datlist)+1]] <- Rt_tempdat_All %>% mutate(exp_name= "20201001_NU_RR_NU_low_effective_0")
  load(file.path(Rt_dir1,i))
  datlist[[length(datlist)+1]] <- Rt_tempdat_All %>%  mutate(exp_name= "20201001_NU_RR_NU_high_effective_0" )
}

Rtdat <- datlist %>% bind_rows()
Rtdat$exp <- gsub("20201001_NU_RR_NU_","",Rtdat$exp_name)

Rtdat$date <- Rtdat$t_end + as.Date("2020-01-01")

pplot <- ggplot(data=subset(Rtdat ,  exp=="low_effective_0")) +theme_minimal() +
  geom_hline(yintercept=1, col="black")+
 # geom_ribbon(aes(x=date, ymin =`Quantile.0.025(R)`, ymax =`Quantile.0.975(R)`, fill=as.factor(Ki_NU)),size=1, alpha=0.2) +
  geom_line(aes(x=date, y =`Median(R)`, col=as.factor(Ki_NU)),size=1) +
  facet_wrap(exp~NUgrp, scales="free_x") +
  labs(color="Ki_NU",x="")+
  scale_x_date(date_breaks = "30 days", date_labels = "%d\n%b")


ggsave(paste0("NU_median_Rt_low_effective.png"), plot = pplot, 
       path = file.path("C:/Users/mrm9534/Box/MR_archive/NU"), width = 8, height = 5,  device = "png"
)



pplot <- ggplot(data=subset(Rtdat ,  exp=="high_effective_0")) +theme_minimal() +
  geom_hline(yintercept=1, col="black")+
  # geom_ribbon(aes(x=date, ymin =`Quantile.0.025(R)`, ymax =`Quantile.0.975(R)`, fill=as.factor(Ki_NU)),size=1, alpha=0.2) +
  geom_line(aes(x=date, y =`Median(R)`, col=as.factor(Ki_NU)),size=1) +
  facet_wrap(exp~NUgrp, scales="free_x") +
  labs(color="Ki_NU",x="")+
  scale_x_date(date_breaks = "30 days", date_labels = "%d\n%b")


ggsave(paste0("NU_median_Rt_high_effective.png"), plot = pplot, 
       path = file.path("C:/Users/mrm9534/Box/MR_archive/NU"), width = 8, height = 5,  device = "png"
)


pplot <- ggplot(data=subset(Rtdat)) +theme_minimal() +
  geom_hline(yintercept=1, col="black")+
  # geom_ribbon(aes(x=date, ymin =`Quantile.0.025(R)`, ymax =`Quantile.0.975(R)`, fill=as.factor(Ki_NU)),size=1, alpha=0.2) +
  geom_line(aes(x=date, y =`Median(R)`, col=as.factor(Ki_NU), linetype=exp),size=1) +
  facet_wrap(~NUgrp, scales="free_x") +
  labs(color="Ki_NU",x="")+
  scale_x_date(date_breaks = "30 days", date_labels = "%d\n%b")


ggsave(paste0("NU_median_Rt.png"), plot = pplot, 
       path = file.path("C:/Users/mrm9534/Box/MR_archive/NU"), width = 8, height = 5,  device = "png"
)


ggplot(data=subset(Rtdat ,  exp=="low_effective_0")) +theme_minimal() +
  geom_hline(yintercept=1, col="red")+
  geom_line(aes(x=t_end, y =`Median(R)`, col=as.factor(NUgrp))) +
  facet_wrap(exp~Ki_NU) +
  labs(color="NUgrp")



ggplot(data=subset(Rtdat, t_end==15 &  exp=="low_effective_0")) +theme_minimal() +
  geom_hline(yintercept=1, col="red")+
  geom_line(aes(x=t_end, y =`Median(R)`, col=as.factor(Ki_NU))) +
  facet_wrap(exp~NUgrp) +
  labs(color="Ki_NU")

ggplot(data=subset(Rtdat, t_end >60 &  exp=="low_effective_0")) +theme_minimal() +
  geom_hline(yintercept=1, col="red")+
  geom_line(aes(x=t_end, y =`Median(R)`, col=as.factor(Ki_NU))) +
  facet_wrap(exp~NUgrp) +
  labs(color="Ki_NU")

