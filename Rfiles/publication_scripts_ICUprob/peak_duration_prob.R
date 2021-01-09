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
simdate <-'20201212'
sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)
if(!dir.exists(file.path(sim_dir, "ICU_bar_plots")))dir.create(file.path(sim_dir, "ICU_bar_plots"))


exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen",exp_names)]
exp_names <- exp_names[c(grep("daysdelay",exp_names),grep("counterfactual",exp_names))]
exp_names <- exp_names[!(grepl("_reopen",exp_names))]
exp_names <- exp_names[!(grepl("counterfactual",exp_names))]

#files <- list.files(sim_dir, recursive = TRUE, full.names = TRUE, pattern="peak_exceed_df.csv")
#dat_peak<-f_combine_csv_from_list(files)

dat_peak <- f_combineDat(sim_dir,exp_names, "peak_exceed_df.csv") %>%
  filter(geography_modeled %in%  paste0("covidregion_",c(1,4,11))) 

dat_prob <- f_combineDat(sim_dir,exp_names, "hospitaloverflow.csv")  %>% 
  mutate(geography_modeled=ifelse(nchar(geography_modeled)<10,paste0("covidregion_",geography_modeled),geography_modeled)) %>%
  filter(geography_modeled %in%  paste0("covidregion_",c(1,4,11))) 

dat_trigger <- f_combineDat(sim_dir,exp_names, "trigger_peak_exceed_df.csv")  %>%  
  filter(geography_modeled %in%  paste0("covidregion_",c(1,4,11))) 

table(dat_prob$geography_modeled, dat_prob$exp_name)

dat_prob$capacity_multiplier <- as.factor( dat_prob$capacity_multiplier)
dat_peak$capacity_multiplier <- as.factor( dat_peak$capacity_multiplier)
dat_trigger$capacity_multiplier <- as.factor( dat_trigger$capacity_multiplier)


dat_prob$scen_name <- gsub(paste0(simdate,"_IL_regreopen"), "", dat_prob$exp_name)
dat_prob <- dat_prob %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat_prob$rollback[is.na(dat_prob$rollback)] <- "counterfactual"

dat_peak$scen_name <- gsub(paste0(simdate,"_IL_regreopen"), "", dat_peak$exp_name)
dat_peak <- dat_peak %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat_peak$rollback[is.na(dat_peak$rollback)] <- "counterfactual"

dat_trigger$scen_name <- gsub(paste0(simdate,"_IL_regreopen"), "", dat_trigger$exp_name)
dat_trigger <- dat_peak %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat_trigger$rollback[is.na(dat_trigger$rollback)] <- "counterfactual"


### TODO Outcome table
capacity <- dat_prob %>% 
              dplyr::select(geography_modeled, icu_availforcovid) %>% 
              unique()

counterfactual <- dat_peak %>%
                filter(rollback=='counterfactual') %>%
                dplyr::select(geography_modeled, reopen, delay, rollback, capacity_multiplier ,Oct_to_peak,peak_date,
                              exceed_date_from_0.1_median, exceed_date_from_0.1_95CI_lower, exceed_date_from_0.1_95CI_upper,
                              exceed_date_to_0.1_median, exceed_date_to_0.1_95CI_lower, exceed_date_to_0.1_95CI_upper,
                              exceed_diff_0.1_median, exceed_diff_0.1_95CI_lower, exceed_diff_0.1_95CI_upper) %>% 
                unique() %>%
                arrange(geography_modeled, reopen)
print(counterfactual)

#unique(dat_peak$capacity_multiplier_fct)
strong_mitigation <- dat_peak %>%
  filter(rollback=='sm4' & delay =='0daysdelay' & capacity_multiplier >=0.4 ) %>%
  dplyr::select(geography_modeled, reopen, delay, rollback, capacity_multiplier ,Oct_to_peak,peak_date,
                exceed_date_from_0.1_median, exceed_date_from_0.1_95CI_lower, exceed_date_from_0.1_95CI_upper,
                exceed_date_to_0.1_median, exceed_date_to_0.1_95CI_lower, exceed_date_to_0.1_95CI_upper,
                exceed_diff_0.1_median, exceed_diff_0.1_95CI_lower, exceed_diff_0.1_95CI_upper) %>% 
  unique() %>%
  arrange(geography_modeled, reopen,capacity_multiplier,delay)
strong_mitigation


weak_mitigation <- dat_peak %>%
  filter(rollback!='sm4' & delay =='0daysdelay' & capacity_multiplier >=0.4 ) %>%
  dplyr::select(geography_modeled, reopen, delay, rollback, capacity_multiplier ,Oct_to_peak,peak_date,
                exceed_date_from_0.1_median, exceed_date_from_0.1_95CI_lower, exceed_date_from_0.1_95CI_upper,
                exceed_date_to_0.1_median, exceed_date_to_0.1_95CI_lower, exceed_date_to_0.1_95CI_upper,
                exceed_diff_0.1_median, exceed_diff_0.1_95CI_lower, exceed_diff_0.1_95CI_upper) %>% 
  unique() %>%
  arrange(geography_modeled, reopen,capacity_multiplier,delay)


### TODO plot
dat_sub <- dat_peak %>% filter(geography_modeled=="covidregion_1") %>% 
  dplyr::select(geography_modeled, capacity_multiplier,reopen, delay, rollback, capacity_multiplier ,Oct_to_peak)

ggplot(data=dat_sub)+
  geom_point(aes(x=capacity_multiplier,y=Oct_to_peak))

dat_sub <- dat_peak %>% filter(geography_modeled=="covidregion_1") %>% 
  filter(exceed_date_from_0.1_median!="")%>%
  mutate(exceed_date_from_0.1_median = as.Date(exceed_date_from_0.1_median) ,
         Oct_to_exceed= exceed_date_from_0.1_median - as.Date("2020-10-01")) %>%
  dplyr::select(geography_modeled, capacity_multiplier,reopen, delay, rollback, capacity_multiplier ,Oct_to_exceed)

ggplot(data=dat_sub)+
  geom_point(aes(x=capacity_multiplier,y=Oct_to_exceed))
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



##--------------------------------

#dat_dur <- dat
#dat_dur_t <- dat
#dat_peak <- dat
#dat_prob <- dat

dat_dur_reg1 <- dat_dur %>% 
  mutate(region =as.numeric(gsub("Region ","",region)))%>% 
                      filter(region==1) %>% 
                      dplyr::select(region, reopen, delay, rollback, capacity_multiplier, exceed_diff_1_median)
                      
dat_dur_t_reg1 <- dat_dur_t  %>% 
            mutate(region =as.numeric(gsub("Region ","",region)))%>% 
                 filter(region==1) %>% 
                dplyr::select(region, reopen, delay, rollback, capacity_multiplier, trigger_to_exceed_0.7_median)

dat_peak_reg1 <- dat_peak  %>% 
  mutate(region =as.numeric(gsub("Region ","",region)))%>% 
  filter(region==1) %>% 
  mutate(perc_ICU_occup_median= critical_median/ avg_resource_available) %>%
  dplyr::select(region,reopen, delay, rollback, capacity_multiplier, perc_ICU_occup_median)

dat_prob_reg1 <- dat_prob  %>% 
  mutate(region =as.numeric(gsub("Region ","",region)))%>% 
  filter(region==1) %>% 
  dplyr::select(region, reopen, delay, rollback, capacity_multiplier, prob)


datAll <- dat_prob_reg1 %>% f_addVar(dat_peak_reg1)  %>% f_addVar(dat_dur_t_reg1)  %>% f_addVar(dat_dur_reg1)

ggplot(data=datAll) +
  geom_point(aes(x=capacity_multiplier, y=exceed_diff_1_median, 
                 size=prob,col=prob))+
  facet_wrap(~reopen)
  
ggplot(data=datAll) +
  geom_point(aes(x=capacity_multiplier, y=trigger_to_exceed_0.7_median, 
                 size=prob,col=reopen))


ggplot(data=datAll) +
  geom_point(aes(x=prob, y=capacity_multiplier, 
                 size=trigger_to_exceed_0.7_median,col=reopen))



########################################


ggplot(data=subset(datAll, delay==delay_val)) +
  geom_point(aes(x=prob, y=capacity_multiplier, col=rollback))+
  geom_line(aes(x=prob, y=capacity_multiplier, col=rollback))+
 facet_wrap(~reopen)
