library(tidyverse)
library(cowplot)
library(data.table)

setwd('/projects/p30781/covidproject/covid-chicago/Rfiles')
Location='NUCLUSTER'
source("load_paths.R")
source("setup.R")
source("processing_helpers.R")

theme_set(theme_bw())

f_initial_and_timevarying_Ki <- function(exp_dir, param = NULL) {
  library(tidyverse)
  library(data.table)
  
  keepVars <- paste0("Ki_EMS_", c(1:11))
  initialKi <- fread(file.path(exp_dir, "sampled_parameters.csv"), select = keepVars) %>%
    unique() %>%
    melt() %>%
    separate(variable, into = c("del", "region"), sep = "Ki_EMS_") %>%
    rename(Ki_initial = value) %>%
    dplyr::select(-del)
  
  if (is.null(param)) param <- c( "capacity_multiplier", "trigger_delay_days") #"reopening_multiplier_4",
  fname ="trajectoriesDat_trim.csv"
  if(!file.exists(file.path(exp_dir, fname)))fname="trajectoriesDat.csv"
  
  keepVars <- c("time", "startdate", "scen_num", "sample_num", param, paste0("Ki_t_EMS-", c(1:11)))
  timevaryingKi <- fread(file.path(exp_dir, fname), select = keepVars) %>%
    filter(as.numeric(time) < 365) %>%
    unique() %>%
    pivot_longer(cols = -c("time", "startdate", "scen_num", "sample_num", param)) %>%
    separate(name, into = c("del", "region"), sep = "Ki_t_EMS-") %>%
    rename(Ki_t = value) %>%
    mutate(date = as.Date(startdate) + time) %>%
    dplyr::select(-del, -time, -startdate)
  
  
  grpVars <- c("date", param, "region")
  Ki_dat <- timevaryingKi %>%
    left_join(initialKi, by = "region") %>%
    mutate(Ki_rebound = (Ki_t / Ki_initial)) %>%
    ungroup() %>%
    dplyr::group_by_at(.vars = grpVars) %>%
    summarize(
      Ki_t = median(Ki_t),
      Ki_initial = median(Ki_initial),
      Ki_rebound = median(Ki_rebound)
    )
  
  Ki_dat$region <- factor(Ki_dat$region, levels = c(1:11), labels = c(1:11))
  return(Ki_dat)
}
f_Ki_plot <- function(exp_dir, Kidat, selected_date=NULL){
  
  if(is.null(selected_date))selected_date = as.Date('2020-11-24')
  
  pplot <- ggplot(data=subset(Kidat, date >selected_date & date <=selected_date+1))+
    geom_bar(aes(x=as.factor(region), y=Ki_rebound*100, fill=as.factor(region)), stat='identity') +
    geom_text(aes(x=as.factor(region), y=Ki_rebound*100, label=Ki_rebound*100), vjust=-0.5) +
    scale_fill_viridis_d()+
    theme(legend.position = 'none')+
    labs(fill="COVID-19 Region",x='COVID-19 Region',
         y="Transmission level compared to\ninitial pre-lockdown transmission (%)",
         caption=selected_date)+
    customTheme
    
    ggsave(paste0("Ki_rebound_date.png"),
           plot = pplot, path = file.path(exp_dir, '_plots'), width = 10, height = 7, device = "png"
    )
    
    ggsave(paste0("Ki_rebound_date.pdf"),
           plot = pplot, path = file.path(exp_dir, '_plots/pdf'), width = 10, height = 7, device = "pdf"
    )
    
}


## -------------------------------
## Run script 
## -------------------------------

#simulation_output <- file.path(simulation_output,'_overflow_simulations')
#simulation_output <- file.path(simulation_output,'_overflow_simulations','20200919')
simulation_output <- file.path('/projects/p30781/covidproject/covid-chicago/_temp/') 

exp_names <- c("20201121_IL_regreopen50perc_0daysdelay_sm4",'20201121_IL_regreopen100perc_0daysdelay_sm4',
               "20201121_IL_regreopen50perc_3daysdelay_sm4",'20201121_IL_regreopen100perc_3daysdelay_sm4',
               "20201121_IL_regreopen50perc_7daysdelay_sm4",'20201121_IL_regreopen100perc_7daysdelay_sm4',
               "20201121_IL_regreopen50perc_0daysdelay_sm7",'20201121_IL_regreopen100perc_0daysdelay_sm7',
               "20201121_IL_regreopen50perc_3daysdelay_sm7",'20201121_IL_regreopen100perc_3daysdelay_sm7',
               "20201121_IL_regreopen50perc_7daysdelay_sm7",'20201121_IL_regreopen100perc_7daysdelay_sm7')

exp_names <- c("20201121_IL_regreopen100perc_3daysdelay_sm4")
for(exp_name in exp_names){
  print(exp_name)
  exp_dir=file.path(simulation_output,exp_name)
  if(!file.exists(file.path(exp_dir, 'trajectoriesDat.csv')))next
    
  Kidat <- f_initial_and_timevarying_Ki(exp_dir=exp_dir, param='capacity_multiplier') %>% mutate(exp_name=exp_name)
  Kidat %>% filter(date >=as.Date("2020-11-01") & date <= as.Date("2020-11-02"))  %>% arrange(Ki_rebound)
  fwrite(Kidat, file.path(simulation_output,exp_name,'Kidat.csv')) 
  
  f_Ki_plot(exp_dir=exp_dir, Kidat)
}

