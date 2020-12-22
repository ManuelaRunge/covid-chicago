
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
capacityDat$region <- factor(capacityDat$geography_name, 
                             levels = c(1,4,11), 
                             labels = paste0("Region ", c(1,4,11)))


simdate <-'20201212'
sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)
if (!dir.exists(file.path(sim_dir, "ICU_samples_plots"))) dir.create(file.path(sim_dir, "ICU_samples_plots"))
if (!dir.exists(file.path(sim_dir, "ICU_samples_plots", "pdf"))) dir.create(file.path(sim_dir, "ICU_samples_plots", "pdf"))

exp_name1 <- "20201212_IL_regreopen100perc_1daysdelay_pr6"
exp_name2 <- "20201212_IL_regreopen50perc_1daysdelay_pr6"

dat1 <- f_load_trajectories(sim_dir, exp_name1, region_nr = 1) %>% mutate(exp_name=exp_name1)
dat2 <- f_load_trajectories(sim_dir, exp_name2, region_nr = 1) %>% mutate(exp_name=exp_name2)

dat <-  rbind(dat1,dat2) %>%
  left_join(capacityDat, by="region") %>%
  group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
  mutate(peak=max(crit_det)) %>% 
  mutate(above_yn = ifelse(peak > avg_resource_available & date <= as.Date("2020-12-31"), 1,0)) %>%
  f_get_scenVars %>%
rm(dat1, dat2)

rollback_val <- unique(dat$rollback)
delay_val <- unique(dat$delay)

### Load samples
samples_dat1 <- fread(file.path(sim_dir, exp_name1, "sampled_parameters.csv")) %>% mutate(exp_name=exp_name1)
samples_dat2 <- fread(file.path(sim_dir, exp_name2, "sampled_parameters.csv")) %>% mutate(exp_name=exp_name2)

samples_dat <-  rbind(samples_dat1,samples_dat2) 
rm(samples_dat1,samples_dat2)

EMSvars_keep <- c("EMS_1$","EMS_4","EMS_11")
sampleVars_exclude = c("EMS","capacity_multiplier","reopening","Ki","ki")
EMSvars <- colnames(samples_dat)[grep(paste0(EMSvars_keep,collapse = "|"), colnames(samples_dat))]
sampleVars <- colnames(samples_dat)[!(grepl(paste0(sampleVars_exclude,collapse = "|"),  colnames(samples_dat)))]

####---------------------------
### Explore sample Vars
####---------------------------
summary(dat$date)
dat_withSamples <- dat %>%
                  filter(region=="Region 1" &  
                           capacity_multiplier %in% c(0.4, 0.8) & 
                           date >= as.Date("2020-09-01")&
                           date <= as.Date("2020-09-02")) %>%
                  f_addVar(samples_dat %>% 
                  dplyr::select_at(sampleVars)) 

colnames(dat_withSamples)

### Explorative plots

#### Single parameters
sample_vars <- c("fraction_symptomatic","fraction_dead","cfr",  "d_Sys_incr6", 
                 "fraction_severe","fraction_hospitalized", "fraction_critical", "fraction_critical_incr3",
                 "time_to_detection", "time_to_infectious","time_to_symptoms", 
                 "time_to_hospitalization","time_to_death",
                 "recovery_time_hosp","recovery_time_crit","recovery_time_mild")


pdf(file.path(sim_dir, "ICU_samples_plots","samples_plots.pdf"))
for(sample_var in sample_vars){
  
dat_withSamples <- as.data.frame(dat_withSamples)
dat_withSamples$plotVar <- dat_withSamples[,colnames(dat_withSamples)==sample_var]

pp <- ggplot(dat_withSamples) +
  geom_point(aes(x=plotVar , y=crit_det_cumul ))+
  geom_smooth(aes(x=plotVar , y=crit_det_cumul ))+
  facet_grid(reopen~capacity_multiplier) +
  labs(title="", subtitle = paste0(sample_var,"\n capacity_multiplier"),
       x=sample_var) +
  customTheme
print(pp)
rm(pp)
}

dev.off()

#### Combined parameters
sample_vars <- c("fraction_symptomatic","fraction_dead","cfr",  "d_Sys_incr6",
                 "fraction_hospitalized", "fraction_critical", "fraction_critical_incr3",
                 "time_to_infectious","time_to_symptoms", "time_to_hospitalization","time_to_death",
                 "recovery_time_hosp","recovery_time_crit")

pdf(file.path(sim_dir, "ICU_samples_plots","samples_plots_v2.pdf"))
for(sample_var in sample_vars){
  
  dat_withSamples <- as.data.frame(dat_withSamples) %>%
                      filter(reopen=="100perc" & capacity_multiplier==0.8)
  
  dat_withSamples$plotVar <- dat_withSamples[,colnames(dat_withSamples)==sample_var]
  dat_withSamples$crit_det_cumul_fct <- cut(dat_withSamples$crit_det_cumul, 
                                            c(quantile(dat_withSamples$crit_det_cumul)))
  
  pp <- ggplot(data=subset(dat_withSamples, !is.na(crit_det_cumul_fct))) +
    geom_point(aes(x=fraction_severe , y=plotVar, col=crit_det_cumul_fct ))+
   # geom_smooth(aes(x=fraction_severe , y=plotVar,group=crit_det_cumul_fct ))+
    #facet_grid(reopen~capacity_multiplier) +
    labs(title="", subtitle = paste0(sample_var,"\n"),
         y=sample_var) +
    scale_color_viridis_d()+
    customTheme
  
  print(pp)
  rm(pp)
}

dev.off()

