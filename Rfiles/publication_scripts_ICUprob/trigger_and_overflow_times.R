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

dat_ICUoverflow <- f_combineDat(sim_dir,exp_names, "hospitaloverflow_dates.csv")

dat_ICUoverflow <- dat_ICUoverflow %>%
  mutate(percICUexceeded = icu_availforcovid_threshold/ icu_availforcovid,
         crit_det_exceed_date_median = ifelse(crit_det_exceed_date_median=="",NA,crit_det_exceed_date_median),
         crit_det_exceed_date_median = as.Date(crit_det_exceed_date_median),
         crit_det_peak_date_median = ifelse(crit_det_peak_date_median=="",NA,crit_det_peak_date_median),
         crit_det_peak_date_median = as.Date(crit_det_peak_date_median),
         exceed_to_peak = ifelse(!is.na(crit_det_exceed_date_median), crit_det_peak_date_median - crit_det_exceed_date_median,NA),
         reopen_to_exceed = crit_det_exceed_date_median- as.Date('2020-10-01')  ,
         crit_det_exceed_date_median_afterReopen = ifelse(crit_det_exceed_date_median >  as.Date('2020-10-01'), 1,0))

##trigger_to_exceed
dat_ICUtrigger_sub <- dat_ICUoverflow %>% 
  dplyr::select(exp_name,geography_modeled,icu_availforcovid,icu_availforcovid_threshold, capacity_multiplier, 
                crit_det_exceed_date_median,crit_det_exceed_date_95CI_lower,crit_det_exceed_date_95CI_upper,
                crit_det_peak_date_median,crit_det_peak_date_95CI_lower,crit_det_peak_date_95CI_upper,
                percICUexceeded,exceed_to_peak, reopen_to_exceed )  

dat_ICUoverflow_sub <- dat_ICUoverflow %>% 
  dplyr::select(exp_name, geography_modeled,icu_availforcovid, icu_availforcovid_threshold, capacity_multiplier, 
                crit_det_exceed_date_median,crit_det_exceed_date_95CI_lower,crit_det_exceed_date_95CI_upper,
                crit_det_peak_date_median,crit_det_peak_date_95CI_lower,crit_det_peak_date_95CI_upper,percICUexceeded ) %>%
  filter(percICUexceeded==1) %>%
  dplyr::select(-c(percICUexceeded, icu_availforcovid, icu_availforcovid_threshold))

colnames(dat_ICUoverflow_sub)[c(4:9)] <- gsub("crit_det_","overflow_",colnames(dat_ICUoverflow_sub)[c(4:9)])

datSub <- dat_ICUtrigger_sub %>%
            left_join(dat_ICUoverflow_sub, by=c("exp_name","geography_modeled","capacity_multiplier") ) %>%
            mutate(trigger_to_exceed = ifelse(!is.na(overflow_exceed_date_median),overflow_exceed_date_median - crit_det_exceed_date_median,NA) )

summary(datSub$trigger_to_exceed)
tapply(datSub$trigger_to_exceed, datSub$capacity_multiplier, summary)
tapply(datSub$trigger_to_exceed, datSub$exp_name, summary)

datSub <- datSub %>% filter(geography_modeled %in% c("covidregion_1","covidregion_4","covidregion_11"))
datSub$scen_name <- gsub(paste0(simdate,"_IL_regreopen"), "", datSub$exp_name)
datSub <- datSub %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
datSub$rollback[is.na(datSub$rollback)] <- "counterfactual"



ggplot(data=datSub) +
  geom_point(aes(x=capacity_multiplier, y=trigger_to_exceed, group=exp_name, col=percICUexceeded ))+
  facet_wrap(~geography_modeled)


ggplot(data=datSub) +
  geom_point(aes(x=capacity_multiplier, y=trigger_to_exceed, group=exp_name, col=as.factor(percICUexceeded) ))+
  facet_wrap(exp_name~geography_modeled)


### Look at counterfactual
cdat <- subset(datSub, rollback=='counterfactual')  %>% 
             group_by(geography_modeled,capacity_multiplier, exp_name) %>%
             arrange(percICUexceeded) %>%
             mutate(timediff = reopen_to_exceed- lag(reopen_to_exceed) )

ggplot(data=cdat) +
  geom_point(aes(x=as.factor(percICUexceeded), y=reopen_to_exceed, group=exp_name,col=reopen ))+
  geom_line(aes(x=as.factor(percICUexceeded), y=reopen_to_exceed, group=exp_name,col=reopen ))+
  geom_text(data=subset(cdat,timediff!=0),
            aes(x=as.factor(percICUexceeded), y=reopen_to_exceed, label=timediff, group=exp_name ), vjust=0.1, hjust=3,col='darkgrey')+
  geom_text(data=subset(cdat,timediff!=0),
            aes(x=as.factor(percICUexceeded), y=reopen_to_exceed, label=timediff, group=exp_name ), vjust=-0.1)+
  facet_wrap(~geography_modeled)



ggplot(data=cdat) +
  geom_point(aes(x=as.factor(percICUexceeded), y=reopen_to_exceed, group=exp_name,col=reopen ))+
  geom_line(aes(x=as.factor(percICUexceeded), y=reopen_to_exceed, group=exp_name,col=reopen ))+
  geom_text(data=subset(cdat,timediff!=0),
            aes(x=as.factor(percICUexceeded), y=reopen_to_exceed, label=timediff, group=exp_name ), vjust=0.1, hjust=3,col='darkgrey')+
  geom_text(data=subset(cdat,timediff!=0),
            aes(x=as.factor(percICUexceeded), y=reopen_to_exceed, label=timediff, group=exp_name ), vjust=-0.1)+
  facet_wrap(~geography_modeled)

### Look at 

