library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

TwoCols_seq <- c("#00a79d", "#f7941d")
capacity_col <- "#2a3b90"
customTheme <- f_getCustomTheme()
theme_set(theme_minimal())
## -------------------------------
## Run script
## -------------------------------
capacityDat <- load_new_capacity(filedate = "20200915")
capacityDat <- capacityDat %>%
  filter(geography_name %in% c(1,4,11)) %>%
  rename(avg_resource_available=icu_available )
capacityDat$region <- factor(capacityDat$geography_name, levels = c(1,4,11), labels = paste0("Region ", c(1,4,11)))


simdate <-'20201212'
sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)
if (!dir.exists(file.path(sim_dir, "ICU_trajectories_plots"))) dir.create(file.path(sim_dir, "ICU_trajectories_plots"))
if (!dir.exists(file.path(sim_dir, "ICU_trajectories_plots", "pdf"))) dir.create(file.path(sim_dir, "ICU_trajectories_plots", "pdf"))

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen",exp_names)]
exp_names <- exp_names[c(grep("daysdelay",exp_names),grep("counterfactual",exp_names))]
exp_names <- exp_names[!(grepl("_reopen",exp_names))]

exp_names <- exp_names[(grep("100perc",exp_names))]
exp_names <- exp_names[!(grepl("counterfactual",exp_names))]
exp_names <- exp_names[!(grepl("7daysdelay",exp_names))]

peakDat_List <- list()
exceedDat_List <- list()
exceed70Dat_List <- list()
triggerDat_List <- list()

for(exp_name in exp_names){
  print(exp_name)
  
  if(sum(grep("counterfactual", exp_name))>1){
    temp_dat <- f_load_trajectories(sim_dir, exp_name, region_nr = 1) %>% 
      filter(date>=as.Date("2020-10-01")) %>% 
      dplyr::select(c(date, scen_num, sample_num, 
                      capacity_multiplier, region, 
                      nsamples, nmultiplier_per_sample, 
                      crit_det)) %>%
      mutate(exp_name=exp_name) 
    
  }else{
  temp_dat <- f_load_trajectories(sim_dir, exp_name, region_nr = 1) %>% 
                                    filter(date>=as.Date("2020-10-01")) %>% 
                                    dplyr::select(c(date, scen_num, sample_num, 
                                                    capacity_multiplier, region, 
                                                    nsamples, nmultiplier_per_sample, 
                                                    crit_det)) %>%
                                    mutate(exp_name=exp_name) 
  }

  temp_dat <-  temp_dat %>%
    left_join(capacityDat, by="region") %>%
    group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
    mutate(peak=max(crit_det)) %>% 
    mutate(above_yn = ifelse(peak > avg_resource_available & date <= as.Date("2020-12-31"), 1,0))
  
  peakDat <- temp_dat %>% 
    filter(date>=as.Date("2020-10-01")) %>%
    group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
    filter(crit_det==peak)  %>%
    mutate(peak_date=date) %>% 
    group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
    filter(peak_date==min(peak_date))  %>% 
    select(exp_name,capacity_multiplier, sample_num, scen_num, peak_date)
  
  
  exceedDat <- temp_dat %>% 
    filter(date>=as.Date("2020-10-01")) %>%
    group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
    filter(crit_det >= avg_resource_available)  %>%
    mutate(exceed_date_from=min(date),
           exceed_date_to=max(date),
           diff_exceed_date = as.numeric(exceed_date_to - exceed_date_from)) %>% 
    select(exp_name,capacity_multiplier, sample_num, scen_num, exceed_date_from , exceed_date_to, diff_exceed_date)%>% 
    unique()
  
  
  exceed70Dat <- temp_dat %>% 
    filter(date>=as.Date("2020-10-01")) %>%
    group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
    filter(crit_det >= avg_resource_available * 0.7)  %>%
    mutate(exceed70_date_from=min(date),
           exceed70_date_to=max(date),
           diff_exceed70_date = as.numeric(exceed70_date_to - exceed70_date_from)) %>% 
    select(exp_name,capacity_multiplier, sample_num, scen_num, exceed70_date_from ,exceed70_date_to, diff_exceed70_date) %>% 
    unique()
  
  trigger_capacity_Dat <- temp_dat %>% 
    filter(date >=as.Date("2020-10-01") & date <=as.Date("2020-12-31") ) %>%
    select(exp_name,capacity_multiplier, sample_num, scen_num, avg_resource_available, crit_det,date) %>%
    mutate(trigger_capacity_val =avg_resource_available*capacity_multiplier) %>%
    filter(crit_det > trigger_capacity_val)  %>% 
    group_by(exp_name,capacity_multiplier,sample_num, scen_num, avg_resource_available, trigger_capacity_val) %>% 
    filter(date==min(date)) %>%
    rename(trigger_capacity_date=date) %>%
    unique()
  

  peakDat_List[[length(peakDat_List)+1]] <- peakDat
  exceedDat_List[[length(exceedDat_List)+1]] <- exceedDat
  exceed70Dat_List[[length(exceed70Dat_List)+1]] <- exceed70Dat
  triggerDat_List[[length(triggerDat_List)+1]] <- trigger_capacity_Dat
  
  rm(temp_dat)
  
}


peakDat <- peakDat_List %>% bind_rows()
trigger_capacity_Dat <- triggerDat_List %>% bind_rows()
exceed70Dat <- exceed70Dat_List %>% bind_rows()
exceedDat <- exceedDat_List %>% bind_rows()

###--------------------------------
## exceed
###--------------------------------

plotdat <- exceedDat  %>% 
  group_by(exp_name, capacity_multiplier) %>% 
  add_tally() %>%
  f_get_scenVars %>%
  filter(delay!="7daysdelay")

tapply(plotdat$n, plotdat$capacity_multiplier, summary)

plotdatAggr <- plotdat %>% 
                group_by(exp_name, capacity_multiplier)  %>% 
                mutate(diff_exceed_date = ifelse(n <10, 0, diff_exceed_date)) %>%
                summarize(nsamples=sum(n),
                          diff_exceed_date_lower =  quantile(diff_exceed_date, probs = 0.25, na.rm = TRUE) ,
                          diff_exceed_date_mean = mean(diff_exceed_date, na.rm = TRUE),
                          diff_exceed_date_upper =  quantile(diff_exceed_date, probs = 0.75, na.rm = TRUE))%>%
                f_get_scenVars

pplot_with_trajectories <- 
  ggplot(data=plotdat)+
  geom_jitter(aes(x=as.factor(capacity_multiplier), 
                  y=diff_exceed_date, 
                  fill=as.factor(rollback)),
              width=0.1,shape=21,alpha=0.2, col="grey")  +
  geom_pointrange(data=plotdatAggr, aes(x=as.factor(capacity_multiplier),
                                y=diff_exceed_date_mean, 
                                ymin=diff_exceed_date_lower, 
                                ymax=diff_exceed_date_upper, fill=rollback), shape=21,size=0.7)  +
  facet_wrap(~reopen, ncol=1)


pplot <- ggplot(data=plotdatAggr)+
  geom_ribbon(data=subset(plotdatAggr),
              aes(x=as.factor(capacity_multiplier),
                  ymin=diff_exceed_date_lower ,
                  ymax=diff_exceed_date_upper ,
                  fill=rollback, group=rollback), alpha=0.3)  + 
  geom_line(data=subset(plotdatAggr), 
            aes(x=as.factor(capacity_multiplier),
                y=diff_exceed_date_mean ,
                col=rollback, group=rollback), size=1)  +
  facet_wrap(~reopen, ncol=1)+
  #scale_fill_manual(values = TwoCols_seq) +
  #scale_color_manual(values = TwoCols_seq) +
  scale_y_continuous(lim=c(0, 120))+
  customTheme+
  theme() + 
  labs( y = "Days above capacity",
        x = "Trigger threshold",
        caption="mean and IQR")



####

###--------------------------------
##  diff_trigger_peak
###--------------------------------

plotdat <- peakDat %>% 
  f_addVar(trigger_capacity_Dat) %>%
  group_by(exp_name, scen_num) %>%
  mutate(diff_trigger_peak = as.numeric(peak_date - trigger_capacity_date )) %>%
  f_get_scenVars

ggplot(data=plotdat)+
  # geom_jitter(aes(x=as.factor(capacity_multiplier), y=diff_trigger_peak, fill=as.factor(exp_name)),
  #            width=0.1,shape=21,alpha=0.5)  +
  geom_boxplot(aes(x=as.factor(capacity_multiplier), y=diff_trigger_peak, fill=as.factor(rollback)), width=0.3,shape=21,alpha=0.5)  +
  facet_wrap(~reopen, ncol=2)

###--------------------------------
##  trigger_to_exceed70_to
###--------------------------------
plotdat <- exceed70Dat %>% 
  f_addVar(trigger_capacity_Dat) %>%
  group_by(exp_name, scen_num) %>%
  mutate(trigger_to_exceed70_to = as.numeric(exceed70_date_to - trigger_capacity_date) ) %>%
  group_by(exp_name, capacity_multiplier) %>% add_tally()%>%
  f_get_scenVars


ggplot(data=plotdat)+
  geom_jitter(aes(x=as.factor(capacity_multiplier), 
                  y=trigger_to_exceed70_to,
                  fill=as.factor(exp_name)),
              width=0.1,shape=21,alpha=0.3)  +
  geom_boxplot(data=subset(plotdat, n>5), 
               aes(x=as.factor(capacity_multiplier),
                   y=trigger_to_exceed70_to, fill=as.factor(exp_name)),
               width=0.3,shape=21)  +
  facet_wrap(~exp_name, ncol=1)

