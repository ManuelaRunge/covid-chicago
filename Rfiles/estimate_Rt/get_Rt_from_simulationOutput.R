## ============================================================
## R script to get R(t) from simulation outputs
## ============================================================

# install.packages("devtools")
# library(devtools)
# install_github("annecori/EpiEstim", force = TRUE)
library(tidyverse)
library(EpiEstim)


runViaSource = TRUE

if(runViaSource){
  
  setwd("C:/Users/mrm9534/gitrepos/covid-chicago/Rfiles/")
  
  ## simdate and  exp_scenario  defined in NUcivis_filecopy.R

}else{
  
  simdate = "20200805"
  exp_scenario = "baseline"
}


source("load_paths.R")
source("processing_helpers.R")
source("estimate_Rt/getRt_function.R")

outdir <- file.path("estimate_Rt/from_simulations")


### Load simulation outputs
#dat <- read.csv(file.path(project_path, "NU_civis_outputs",simdate,paste0("csv/nu_il_", exp_scenario ,"_",simdate,".csv")))
Rt_dat <- read.csv(file.path(simulation_output, exp_name, 'estimatedRt', 'combined_temp_Rt_tempdat_All.csv'))


### Combine list to dataframe 
Rt_dat <- Rt_dat %>%  mutate( time =  t_end ) %>%
          f_addRestoreRegion() %>%
          select(time,scen_num, restore_region, region, Mean.R.  ,Median.R.) %>%
          rename(meanRt =Mean.R.  ,medianRt=Median.R. )

Rt_datRR <- Rt_dat %>%  group_by(restore_region, time, scen_num) %>% summarize(meanRt=mean(meanRt),medianRt=mean(medianRt) )  %>% rename(geography_modeled=restore_region)
Rt_datIL <- Rt_dat %>%  group_by( time, scen_num) %>% summarize(meanRt=mean(meanRt),medianRt=mean(medianRt) )  %>% mutate(geography_modeled="illinois")


Rt_dat <- Rt_dat %>% rename(geography_modeled=region) %>% select(-restore_region)%>% rbind(Rt_datRR)%>% rbind(Rt_datIL)
table(Rt_dat$geography_modeled)



RtdatCOmbined <- Rt_dat %>%
  dplyr::group_by(time,geography_modeled ) %>%
  dplyr::summarize(Median.of.covid.19.Rt= median(medianRt),
            Lower.error.bound.of.covid.19.Rt=  quantile(medianRt, probs=0.025, na.rm = TRUE),
            Upper.error.bound.of.covid.19.Rt = quantile(medianRt, probs=0.975, na.rm = TRUE)) %>%
  dplyr::mutate(Date = as.Date('2020-02-13') + time) %>%
dplyr::arrange(Date, geography_modeled) %>%
  filter(Date <= "2020-12-01") 



saveForCivis=FALSE
if(saveForCivis){
  fname =  paste0("nu_il_", exp_scenario ,"_estimated_Rt_",simdate,".csv")
  RtdatCOmbined %>% 
    dplyr::select(Date, geography_modeled, Median.of.covid.19.Rt, Lower.error.bound.of.covid.19.Rt, Upper.error.bound.of.covid.19.Rt) %>%
    write.csv(file.path(project_path, "NU_civis_outputs" ,simdate, 'csv',fname), row.names = FALSE)
  
}
if(saveInExpDir){
  fname =  paste0("estimated_Rt.csv")
  RtdatCOmbined %>% 
    dplyr::select(Date, geography_modeled, Median.of.covid.19.Rt, Lower.error.bound.of.covid.19.Rt, Upper.error.bound.of.covid.19.Rt) %>%
    write.csv(file.path(simulation_output, exp_name, 'estimatedRt', fname), row.names = FALSE)

  
  
  pplot <-  RtdatCOmbined %>% 
    filter(Date>= "2020-04-01" & Date<= "2020-09-01") %>%
    filter(geography_modeled %in% as.character(c(1:11))) %>%
    ggplot() + 
    theme_minimal() +
    geom_ribbon(aes(x=Date, ymin=Lower.error.bound.of.covid.19.Rt, ymax=Upper.error.bound.of.covid.19.Rt), fill="deepskyblue3", alpha=0.3) +
    geom_line(aes(x=Date, y=Median.of.covid.19.Rt), col="deepskyblue3") +
    geom_hline(yintercept = 1) +
    facet_wrap(~geography_modeled)
  
  ggsave(paste0("estimatedRt_overtime.png"),
         plot = pplot, path =file.path(simulation_output, exp_name, 'estimatedRt'), width =8, height = 6, device = "png"
  )
  rm(pplot)
  
  
 pplot <-  RtdatCOmbined %>% 
    filter(Date>= "2020-08-16" & Date< "2020-08-17") %>%
    filter(geography_modeled %in% as.character(c(1:11))) %>%
  ggplot() + 
    theme_cowplot() +
    geom_errorbar(aes(x=reorder(geography_modeled, Median.of.covid.19.Rt), ymin=Lower.error.bound.of.covid.19.Rt, ymax=Upper.error.bound.of.covid.19.Rt), width=0.3, alpha=0.3) +
    geom_point(aes(x=reorder(geography_modeled, Median.of.covid.19.Rt), y=Median.of.covid.19.Rt), col="deepskyblue3",size=2.5) +
    geom_hline(yintercept = 1)
  
  ggsave(paste0("estimatedRt_20200818.png"),
         plot = pplot,path =file.path(simulation_output, exp_name, 'estimatedRt'), width = 8, height =5, device = "png"
  )
  
}



  