library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

## -------------------------------
## Run script
## -------------------------------

simdate <-'20200919'
#simdate <-'20201121'
sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen",exp_names)]
exp_names <- exp_names[c(grep("daysdelay",exp_names),grep("counterfactual",exp_names))]

dat_peak <- f_combineDat(sim_dir,exp_names, "ICU_peak.csv") %>% 
                mutate(geography_modeled = gsub("EMS-","covidregion_",ems)) %>% 
                filter(ems!="All") %>% 
                dplyr::select(-ems)
colnames(dat_peak) <- gsub("crit_det_","crit_det_peak_val_", colnames(dat_peak))

dat_prob <- f_combineDat(sim_dir,exp_names, "hospitaloverflow.csv") 
dat_dates <- f_combineDat(sim_dir,exp_names, "hospitaloverflow_dates.csv") 
#dat_dur <- f_combineDat(sim_dir,exp_names, "ICU_peak.csv")

dat_prob$capacity_multiplier <- as.factor( dat_prob$capacity_multiplier)
dat_peak$capacity_multiplier <- as.factor( dat_peak$capacity_multiplier)
dat_dates$capacity_multiplier <- as.factor( dat_dates$capacity_multiplier)

dat <- f_addVar(dat_peak, dat_prob) %>% 
        f_addVar(dat_dates)  %>% 
        filter(geography_modeled %in%  paste0("covidregion_",c(1,4,11))) 
table(dat$geography_modeled, exclude=NULL)
table(dat$capacity_multiplier, exclude=NULL)

dat$scen_name <- gsub(paste0(simdate,"_IL_regreopen"), "", dat$exp_name)
dat <- dat %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat$rollback[is.na(dat$rollback)] <- "counterfactual"

### TODO Outcome table

capacity <- dat %>% 
              dplyr::select(geography_name, icu_available) %>% 
              unique()

counterfactual <- dat %>%
                filter(rollback=='counterfactual') %>%
                filter(icu_availforcovid_threshold==icu_available) %>%
                dplyr::select(geography_name, reopen, delay, rollback, capacity_multiplier ,
                              crit_det_exceed_date_median, crit_det_exceed_date_95CI_lower, crit_det_exceed_date_95CI_upper,
                              crit_det_peak_date_median, crit_det_peak_date_95CI_lower, crit_det_peak_date_95CI_upper,
                              crit_det_peak_val_median, crit_det_peak_val_95CI_lower, crit_det_peak_val_95CI_upper) %>% 
                unique()


strong_mitigation <- dat %>%
  filter(rollback=='sm4') %>%
  filter(icu_availforcovid_threshold==icu_available) %>%
  dplyr::select(geography_name, reopen, delay, rollback, capacity_multiplier ,
                crit_det_exceed_date_median, crit_det_exceed_date_95CI_lower, crit_det_exceed_date_95CI_upper,
                crit_det_peak_date_median, crit_det_peak_date_95CI_lower, crit_det_peak_date_95CI_upper,
                crit_det_peak_val_median, crit_det_peak_val_95CI_lower, crit_det_peak_val_95CI_upper) %>% 
  unique()

weak_mitigation <- dat %>%
  filter(rollback=='sm7') %>%
  filter(delay == "0daysdelay") %>% 
  filter(icu_availforcovid_threshold==icu_available) %>%
  dplyr::select(geography_name, reopen, delay, rollback, capacity_multiplier ,
                crit_det_exceed_date_median, crit_det_exceed_date_95CI_lower, crit_det_exceed_date_95CI_upper,
                crit_det_peak_date_median, crit_det_peak_date_95CI_lower, crit_det_peak_date_95CI_upper,
                crit_det_peak_val_median, crit_det_peak_val_95CI_lower, crit_det_peak_val_95CI_upper) %>% 
  unique()


### TODO plot

dat_sub <- dat %>% filter(geography_name==1)
#table(dat_sub$prob, dat_sub$crit_det_peak_val_median, exclude = NULL)

#dat_sub$prob[is.na(dat_sub$prob)] <-0
tapply(dat_prob$prob, dat_prob$capacity_multiplier, summary)
tapply(dat_peak$perc_ICU_occup_median, dat_peak$capacity_multiplier, summary)


ggplot(data=subset(dat, perc_ICU_occup_median<=100 )) +
  geom_jitter(aes(x=prob, y=perc_ICU_occup_median, group=interaction(exp_name,capacity_multiplier ),
                 col=as.factor(capacity_multiplier)))+
  scale_color_viridis_d()+
  facet_wrap(~reopen)


ggplot(data=subset(dat, perc_ICU_occup_median<=100 )) +
  geom_jitter(aes(x=capacity_multiplier, y=perc_ICU_occup_median, group=interaction(exp_name,prob ),
                  col=prob))+
  scale_color_viridis_c()
