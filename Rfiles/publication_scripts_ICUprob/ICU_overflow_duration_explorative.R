library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")


## -------------------------------
## Run script
## -------------------------------

simdate <-'20200919'
simdate <-'20201121'
sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen",exp_names)]
exp_names <- exp_names[c(grep("daysdelay",exp_names),grep("counterfactual",exp_names))]

dat_ICUoverflow <- f_combineDat(sim_dir,exp_names, "hospitaloverflow_duration.csv")

dat_ICUoverflow <- dat_ICUoverflow %>%
  mutate(percICUexceeded = icu_availforcovid_threshold/ icu_availforcovid)

dat_ICUoverflow <- dat_ICUoverflow %>% filter(geography_modeled %in% c("covidregion_1","covidregion_4","covidregion_11"))
dat_ICUoverflow$scen_name <- gsub(paste0(simdate,"_IL_regreopen"), "", dat_ICUoverflow$exp_name)
dat_ICUoverflow <- dat_ICUoverflow %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat_ICUoverflow$rollback[is.na(dat_ICUoverflow$rollback)] <- "counterfactual"

ggplot(data=subset(dat_ICUoverflow, geography_modeled =='covidregion_1' & percICUexceeded==1))+
  geom_point(aes(x=capacity_multiplier, y=crit_det_exceed_duration_median, col=rollback))+
  facet_wrap(~delay)

#############################
datList <- list()
datList2 <- list()
for (exp_name in exp_names) {
  exp_dir <- file.path(simulation_output, exp_name)
  if (!file.exists(file.path(exp_dir, "exceed_df.csv"))) next
  datList[[length(datList) + 1]] <- fread(file.path(exp_dir, "exceed_df.csv")) %>%
    mutate(exp_name = exp_name) %>% 
    rename(icu_availforcovid=avg_resource_available) %>%
    mutate(percICUexceeded = icu_availforcovid_threshold/ icu_availforcovid) %>%
    pivot_longer(cols=-c('startdate' ,'geography_modeled' ,'scen_num','capacity_multiplier' , 'channel', 'icu_availforcovid', 'exp_name', 'icu_availforcovid_threshold','percICUexceeded')) %>%
    filter(!(name %in% c('time','region','variable','value','index'))) %>%
    mutate(name =gsub("durationCI","duration_CI", name)) %>%
    separate(name, into=c("channel","CI"),sep="_CI_") %>%
    unique() %>%
    pivot_wider(names_from='channel', values_from='value') %>%
    mutate( startdate=as.Date( startdate),
            exceed_date_from = startdate + exceed_time_from,
            exceed_date_to = startdate + exceed_time_to)
  
  
  datList2[[length(datList2) + 1]] <- fread(file.path(exp_dir, "peak_df.csv")) %>%
    mutate(exp_name = exp_name) %>% 
    rename(icu_availforcovid=avg_resource_available)%>%
    pivot_longer(cols=-c('startdate' ,'geography_modeled' ,'capacity_multiplier' , 'channel', 'icu_availforcovid', 'exp_name')) %>%
    separate(name, into=c("channel","CI"),sep="_CI_") %>%
    pivot_wider(names_from='channel', values_from='value') %>%
    mutate( startdate=as.Date( startdate),
            peak_date = startdate + peak_time)
}

dat_peak <- datList2 %>% bind_rows() 
dat_overflow <- datList %>% bind_rows() 

dat_ICUoverflow <-   dat_overflow %>% 
  left_join(dat_peak, by=c('startdate' ,'geography_modeled' ,'capacity_multiplier' , 'icu_availforcovid', 'exp_name','CI')) %>%
  mutate(
    capacity_multiplier = as.numeric(capacity_multiplier),
    exceed_to_peak = as.numeric( peak_date - exceed_date_from) ,
    reopen_to_exceed = as.numeric(exceed_date_from - as.Date('2020-10-01'))  ,
    reopen_to_peak = as.numeric(peak_date - as.Date('2020-10-01')))

tapply(dat_ICUoverflow$exceed_to_peak, dat_ICUoverflow$capacity_multiplier, summary)
tapply(dat_ICUoverflow$exceed_to_peak, dat_ICUoverflow$exp_name, summary)


dat_ICUoverflow <- dat_ICUoverflow %>% filter(geography_modeled %in% c("covidregion_1","covidregion_4","covidregion_11"))
dat_ICUoverflow$scen_name <- gsub("20200919_IL_regreopen", "", dat_ICUoverflow$exp_name)
dat_ICUoverflow <- dat_ICUoverflow %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat_ICUoverflow$rollback[is.na(dat_ICUoverflow$rollback)] <- "counterfactual"


ggplot(data=subset(dat_ICUoverflow, geography_modeled=='covidregion_1' & rollback !="counterfactual"& CI==50 & icu_availforcovid_threshold %in% c( 1) ))+
  geom_point(aes(x=capacity_multiplier, y=duration, group=rollback,col=rollback))+
  geom_line(aes(x=capacity_multiplier, y=duration, group=rollback,col=rollback))+
  facet_wrap(~reopen)



datAggr <- dat_ICUoverflow %>% 
  filter(geography_modeled %in% c("covidregion_1","covidregion_4","covidregion_11") &  CI==50 &   rollback !="counterfactual") %>% 
  group_by(geography_modeled, capacity_multiplier , icu_availforcovid_threshold, CI,exp_name,rollback,reopen,delay) %>%
  summarize(duration_median_mean=mean(duration),
            duration_median_low = min(duration),
            duration_median_high = max(duration))

ggplot(data=subset(datAggr, icu_availforcovid_threshold %in% c(0.75, 1) ))+
  geom_errorbar(aes(x=capacity_multiplier, y=duration_median_mean,ymin=duration_median_low,ymax=duration_median_high, col=as.factor(icu_availforcovid_threshold)),width=0) +
  geom_point(aes(x=capacity_multiplier, y=duration_median_mean, col=as.factor(icu_availforcovid_threshold))) +
  facet_wrap(exp_name~geography_modeled)+
  coord_flip()


ggplot(data=subset(datAggr, icu_availforcovid_threshold %in% c(0.75, 1) ))+
 # geom_errorbar(aes(x=capacity_multiplier, y=duration_median_mean,ymin=duration_median_low,ymax=duration_median_high, col=as.factor(icu_availforcovid_threshold)),width=0) +
  geom_bar(aes(x=capacity_multiplier, y=duration_median_mean, col=as.factor(icu_availforcovid_threshold)), stat='identity') +
  facet_wrap(~geography_modeled)+
  coord_flip()


ggplot(data=subset(datAggr, icu_availforcovid_threshold %in% c(0.75, 1) ))+
  # geom_errorbar(aes(x=capacity_multiplier, y=duration_median_mean,ymin=duration_median_low,ymax=duration_median_high, col=as.factor(icu_availforcovid_threshold)),width=0) +
  geom_area(data=subset(datAggr, icu_availforcovid_threshold %in% c( 0.25) ), aes(x=capacity_multiplier, y=duration_median_mean, fill=as.factor(icu_availforcovid_threshold)), stat='identity') +
  geom_area(data=subset(datAggr, icu_availforcovid_threshold %in% c( 0.5) ), aes(x=capacity_multiplier, y=duration_median_mean, fill=as.factor(icu_availforcovid_threshold)), stat='identity') +
  geom_area(data=subset(datAggr, icu_availforcovid_threshold %in% c( 0.75) ), aes(x=capacity_multiplier, y=duration_median_mean, fill=as.factor(icu_availforcovid_threshold)), stat='identity') +
  geom_area(data=subset(datAggr, icu_availforcovid_threshold %in% c(1) ),aes(x=capacity_multiplier, y=duration_median_mean, fill=as.factor(icu_availforcovid_threshold)), stat='identity') +
  facet_wrap(~geography_modeled)+
  coord_flip()


ggplot(data=subset(datAggr ))+
  # geom_errorbar(aes(x=capacity_multiplier, y=duration_median_mean,ymin=duration_median_low,ymax=duration_median_high, col=as.factor(icu_availforcovid_threshold)),width=0) +
  geom_line( aes(x=capacity_multiplier, y=duration_median_mean, fill=as.factor(icu_availforcovid_threshold)), stat='identity') +
  facet_wrap(~geography_modeled)+
  coord_flip()

unique(datAggr$icu_availforcovid_threshold)
datAggr_sub <- datAggr %>% filter(delay=="7daysdelay"  & rollback=="sm4")

ggplot(data=subset(datAggr_sub ))+
  # geom_errorbar(aes(x=capacity_multiplier, y=duration_median_mean,ymin=duration_median_low,ymax=duration_median_high, col=as.factor(icu_availforcovid_threshold)),width=0) +
  geom_line( aes(x=capacity_multiplier, y=duration_median_mean, group=as.factor(icu_availforcovid_threshold)), stat='identity') +
  facet_wrap(reopen~geography_modeled)+
  coord_flip()

datAggr_sub$region <- factor(datAggr_sub$geography_modeled, levels=c("covidregion_1","covidregion_4","covidregion_11"), labels=c("Region 1","Region 4","Region 11"))
datAggr_sub$reopen <- factor(datAggr_sub$reopen, levels=c("50perc","100perc"), labels=c("50perc","100perc"))

ggplot(data=datAggr_sub)+
  # geom_errorbar(aes(x=capacity_multiplier, y=duration_median_mean,ymin=duration_median_low,ymax=duration_median_high, col=as.factor(icu_availforcovid_threshold)),width=0) +
  geom_area(data=subset(datAggr_sub, icu_availforcovid_threshold %in% c( 0) ), aes(x=capacity_multiplier*100, y=duration_median_mean, group=as.factor(icu_availforcovid_threshold)), fill='#016c59', stat='identity') +
  geom_area(data=subset(datAggr_sub, icu_availforcovid_threshold %in% c( 0.11) ), aes(x=capacity_multiplier*100, y=duration_median_mean, group=as.factor(icu_availforcovid_threshold)), fill='#1c9099', stat='identity') +
  geom_area(data=subset(datAggr_sub, icu_availforcovid_threshold %in% c( 0.25) ), aes(x=capacity_multiplier*100, y=duration_median_mean, group=as.factor(icu_availforcovid_threshold)), fill='#67a9cf', stat='identity') +
  geom_area(data=subset(datAggr_sub, icu_availforcovid_threshold %in% c( 0.5) ), aes(x=capacity_multiplier*100, y=duration_median_mean, group=as.factor(icu_availforcovid_threshold)),  fill='#a6bddb',stat='identity') +
  geom_area(data=subset(datAggr_sub, icu_availforcovid_threshold %in% c( 0.75) ), aes(x=capacity_multiplier*100, y=duration_median_mean, group=as.factor(icu_availforcovid_threshold)), fill='#d0d1e6', stat='identity') +
  geom_area(data=subset(datAggr_sub, icu_availforcovid_threshold %in% c( 0.89) ),aes(x=capacity_multiplier*100, y=duration_median_mean, group=as.factor(icu_availforcovid_threshold)), fill='#f6eff7', stat='identity') +
  #geom_area(data=subset(datAggr, icu_availforcovid_threshold %in% c(1) ),aes(x=capacity_multiplier, y=duration_median_mean, fill=as.factor(icu_availforcovid_threshold)), stat='identity') +
  facet_wrap(reopen~region, scales="free")+
  geom_vline(xintercept = c(0,25,50,75,100),col='grey', alpha=0.5)+
  geom_hline(yintercept = c(30), col='red')+
  scale_y_continuous(breaks=seq(0,100, 15),labels=seq(0,100, 15), expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  coord_flip()+
  customTheme+
  geom_vline(xintercept = c(-Inf, Inf))+
  geom_hline(yintercept = c(-Inf, Inf))+
  labs(x="Trigger threshold\n(%) of ICUs filled", y="Time after reopening in days")



#####

unique(datAggr$icu_availforcovid_threshold)
datAggr_sub <- datAggr %>% filter(delay=="7daysdelay"  & reopen=="50perc")

datAggr_sub$region <- factor(datAggr_sub$geography_modeled, levels=c("covidregion_1","covidregion_4","covidregion_11"), labels=c("Region 1","Region 4","Region 11"))
datAggr_sub$reopen <- factor(datAggr_sub$reopen, levels=c("50perc","100perc"), labels=c("50perc","100perc"))

ggplot(data=datAggr_sub)+
  # geom_errorbar(aes(x=capacity_multiplier, y=duration_median_mean,ymin=duration_median_low,ymax=duration_median_high, col=as.factor(icu_availforcovid_threshold)),width=0) +
  geom_area(data=subset(datAggr_sub, icu_availforcovid_threshold %in% c( 0) ), aes(x=capacity_multiplier*100, y=duration_median_mean, group=as.factor(icu_availforcovid_threshold)), fill='#016c59', stat='identity') +
  geom_area(data=subset(datAggr_sub, icu_availforcovid_threshold %in% c( 0.11) ), aes(x=capacity_multiplier*100, y=duration_median_mean, group=as.factor(icu_availforcovid_threshold)), fill='#1c9099', stat='identity') +
  geom_area(data=subset(datAggr_sub, icu_availforcovid_threshold %in% c( 0.25) ), aes(x=capacity_multiplier*100, y=duration_median_mean, group=as.factor(icu_availforcovid_threshold)), fill='#67a9cf', stat='identity') +
  geom_area(data=subset(datAggr_sub, icu_availforcovid_threshold %in% c( 0.5) ), aes(x=capacity_multiplier*100, y=duration_median_mean, group=as.factor(icu_availforcovid_threshold)),  fill='#a6bddb',stat='identity') +
  geom_area(data=subset(datAggr_sub, icu_availforcovid_threshold %in% c( 0.75) ), aes(x=capacity_multiplier*100, y=duration_median_mean, group=as.factor(icu_availforcovid_threshold)), fill='#d0d1e6', stat='identity') +
  geom_area(data=subset(datAggr_sub, icu_availforcovid_threshold %in% c( 0.89) ),aes(x=capacity_multiplier*100, y=duration_median_mean, group=as.factor(icu_availforcovid_threshold)), fill='#f6eff7', stat='identity') +
  #geom_area(data=subset(datAggr, icu_availforcovid_threshold %in% c(1) ),aes(x=capacity_multiplier, y=duration_median_mean, fill=as.factor(icu_availforcovid_threshold)), stat='identity') +
  facet_wrap(rollback~region, scales="free")+
  geom_vline(xintercept = c(0,25,50,75,100),col='grey', alpha=0.5)+
  geom_hline(yintercept = c(30), col='red')+
  scale_y_continuous(breaks=seq(0,100, 15),labels=seq(0,100, 15), expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  coord_flip()+
  customTheme+
  geom_vline(xintercept = c(-Inf, Inf))+
  geom_hline(yintercept = c(-Inf, Inf))+
  labs(x="Trigger threshold\n(%) of ICUs filled", y="Time after triggered mitigation in days")




ggplot(data=subset(datAggr_sub, icu_availforcovid_threshold==0.67))+
  geom_line(aes(x=capacity_multiplier, y=duration_median_mean , col=rollback))+
  facet_wrap(~region, scales="free")



############################


unique(datAggr$icu_availforcovid_threshold)
datAggr_sub <- datAggr %>% filter(delay!="3daysdelay"  & reopen=="50perc" &  icu_availforcovid_threshold==0.67)

datAggr_sub$region <- factor(datAggr_sub$geography_modeled, levels=c("covidregion_1","covidregion_4","covidregion_11"), labels=c("Region 1","Region 4","Region 11"))
datAggr_sub$reopen <- factor(datAggr_sub$reopen, levels=c("50perc","100perc"), labels=c("50perc","100perc"))

ggplot(data=datAggr_sub)+
  # geom_errorbar(aes(x=capacity_multiplier, y=duration_median_mean,ymin=duration_median_low,ymax=duration_median_high, col=as.factor(icu_availforcovid_threshold)),width=0) +
  geom_area(data=subset(datAggr_sub, delay =="7daysdelay"), aes(x=capacity_multiplier*100, y=duration_median_mean, group=as.factor(icu_availforcovid_threshold)), fill='#016c59', stat='identity') +
  geom_area(data=subset(datAggr_sub, delay =="0daysdelay" ), aes(x=capacity_multiplier*100, y=duration_median_mean, group=as.factor(icu_availforcovid_threshold)), fill='#1c9099', stat='identity') +
  #geom_area(data=subset(datAggr, icu_availforcovid_threshold %in% c(1) ),aes(x=capacity_multiplier, y=duration_median_mean, fill=as.factor(icu_availforcovid_threshold)), stat='identity') +
  facet_wrap(rollback~region, scales="free")+
  geom_vline(xintercept = c(0,25,50,75,100),col='grey', alpha=0.5)+
  geom_hline(yintercept = c(30), col='red')+
  scale_y_continuous(breaks=seq(0,100, 15),labels=seq(0,100, 15), expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  coord_flip()+
  customTheme+
  geom_vline(xintercept = c(-Inf, Inf))+
  geom_hline(yintercept = c(-Inf, Inf))+
  labs(x="Trigger threshold\n(%) of ICUs filled", y="Time after reopening in days")

