library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

theme_set(theme_bw())

f_initial_and_timevarying_Ki <- function(exp_dir, param = NULL) {
  library(tidyverse)
  library(data.table)
  
  Ki_dat_list <- list()
  for(i in c(1:11)){
    keepVars <- paste0("Ki_EMS_",i)
    initialKi <- fread(file.path(exp_dir, "sampled_parameters.csv"), select = keepVars) %>%
      unique() %>%
      melt() %>%
      separate(variable, into = c("del", "region"), sep = "Ki_EMS_") %>%
      rename(Ki_initial = value) %>%
      dplyr::select(-del)
    
    if (is.null(param)) param <- c( "capacity_multiplier", "trigger_delay_days") #"reopening_multiplier_4",
    fname= paste0("trajectoriesDat_region_",i,".csv")
    if(!file.exists(file.path(exp_dir, fname))) fname ="trajectoriesDat.csv" 
    if(!file.exists(file.path(exp_dir, fname)))fname="trajectoriesDat.csv"
    if(!file.exists(file.path(exp_dir, fname)))break("file does not exist")
      
    keepVars <- c("time", "startdate", "scen_num", "sample_num", param, paste0("Ki_t_EMS-", i))
    timevaryingKi <- fread(file.path(exp_dir, fname), select = keepVars) %>%
      filter(as.numeric(time) < 365) %>%
      unique() %>%
      pivot_longer(cols = -c("time", "startdate", "scen_num", "sample_num", param)) %>%
      separate(name, into = c("del", "region"), sep = "Ki_t_EMS-") %>%
      rename(Ki_t = value) %>%
      mutate(date = as.Date(startdate) + time) %>%
      dplyr::select(-del, -time, -startdate)
    
    grpVars <- c("date", param, "region")
    Ki_dat_i <- timevaryingKi %>%
      left_join(initialKi, by = "region") %>%
      mutate(Ki_rebound = (Ki_t / Ki_initial)) %>%
      ungroup() %>%
      dplyr::group_by_at(.vars = grpVars) %>%
      summarize(
        Ki_t = median(Ki_t),
        Ki_initial = median(Ki_initial),
        Ki_rebound = median(Ki_rebound)
      )
    
    Ki_dat_list[[length(Ki_dat_list)+1]] <- Ki_dat_i
  }
  
  
  Ki_dat <- Ki_dat_list %>% bind_rows()
  Ki_dat$region <- factor(Ki_dat$region, levels = c(1:11), labels = c(1:11))
  
  return(Ki_dat)
}

f_Ki_plot <- function(exp_dir, Kidat, selected_date=NULL){
  
  if(is.null(selected_date))selected_date = as.Date('2020-12-31')
  
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
    
    library(RColorBrewer)
    getPalette = colorRampPalette(brewer.pal(8, "Dark2"))(12)
    pplot <- ggplot(data=subset(Kidat))+
      geom_line(aes(x=date, y=Ki_t, col=as.factor(capacity_multiplier))) +
      scale_color_manual(values=getPalette)+
      theme(legend.position = 'none')+
      labs(caption=exp_name,
           col="ICU trigger threshold",x='Date',
           y="Transmission level compared to\ninitial pre-lockdown transmission (%)")+
      customTheme+
      facet_wrap(~region, scales="free")+
      scale_x_date(date_breaks = "30 days", date_labels = "%b")
    
    ggsave(paste0("Ki_timeline.png"),
           plot = pplot, path = file.path(exp_dir, '_plots'),  width = 10, height = 7, device = "png"
    )
    
    ggsave(paste0("Ki_timeline.pdf"),
           plot = pplot, path = file.path(exp_dir, '_plots/pdf'), width = 10, height = 7, device = "pdf"
    )
    
    pplot <- pplot + 
      scale_x_date(lim = c(as.Date("2020-08-01"),as.Date("2021-01-30")), date_breaks = "30 days", date_labels = "%b")
    
    ggsave(paste0("Ki_timeline_zoom.png"),
           plot = pplot, path = file.path(exp_dir, '_plots'),  width = 10, height = 7, device = "png"
    )
    
    ggsave(paste0("Ki_timeline_zoom.pdf"),
           plot = pplot, path = file.path(exp_dir, '_plots/pdf'), width = 10, height = 7, device = "pdf"
    )
}


## -------------------------------
## Run script 
## -------------------------------

#simulation_output <- file.path(simulation_output,'_overflow_simulations')
simdate <-'20200919'
simdate <-'20201121'
simdate <-'20201209'
sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen",exp_names)]
exp_names <- exp_names[c(grep("daysdelay",exp_names),grep("counterfactual",exp_names))]


for(exp_name in exp_names){
  print(exp_name)
  exp_dir=file.path(sim_dir,exp_name)
  
  Kidat <- f_initial_and_timevarying_Ki(exp_dir=exp_dir, param='capacity_multiplier') %>% mutate(exp_name=exp_name)
  Kidat %>% filter(date >=as.Date("2020-11-01") & date <= as.Date("2020-11-02"))  %>% arrange(Ki_rebound)
  fwrite(Kidat, file.path(sim_dir,exp_name,'Kidat_v1.csv')) 
  
  f_Ki_plot(exp_dir=exp_dir, Kidat)
}

