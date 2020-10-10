####--------------------------------------
#### Single simulation analysis
####--------------------------------------

library(tidyverse)
library(cowplot)
library(data.table)
library(raster)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

theme_set(theme_cowplot())

outdir <- file.path("C:/Users/mrm9534/Box/MR_archive/testfigures2")
#simulation_output <- file.path(simulation_output, "overflow_simulations")


####--------------------------------------
#### Region characteristics
####--------------------------------------

dat <- f_region_characteristics()
summary(dat$icubeds_per10th)



####--------------------------------------
####  Baseline - predicted ICU 
####--------------------------------------

exp_name ="20201006_IL_baseline_oldsm8"
exp_dir <- file.path(simulation_output, exp_name)
simdat <- f_load_single_exp(exp_dir)
picu <- f_icu_timeline(dat=simdat, selected_channel="crit_det")


####--------------------------------------
####  Counterfactual - varying reopening
####--------------------------------------
exp_name ="20200919_IL_gradual_reopening_sm7"
exp_dir <- file.path(simulation_output, 'overflow_simulations',exp_name)
simdat <- f_load_single_exp(exp_dir=exp_dir, mainVars= c("date", "scen_num", "sample_num", "reopening_multiplier_4"))
unique(simdat$geography_name)
unique(simdat$reopening_multiplier_4)
picu <- f_icu_timeline(dat=simdat, subregions=c("1"), selected_channel="crit_det", facetVar="reopening_multiplier_4")

### Timeline plot 
for(i in c("illinois", c(1:11))){
  if(!dir.exists(file.path(outdir,"gradual_reopening")))dir.create(file.path(outdir,"gradual_reopening"))
  picu <- f_icu_timeline(dat=simdat, subregions=c(i), selected_channel="crit_det", facetVar="reopening_multiplier_4")
  ggsave(paste0("timeline_",selected_channel,"_region_",i,".png"),
         plot = picu, path = file.path(outdir,"gradual_reopening"), width = 14, height = 8, device = "png"
  )
}

##### Peak in ICU
ICUcumul_out <- f_describe_ICU_cumul()


##### More ICU beds needed at peak
ICUpeak_out <- f_describe_ICU_peak()
ICUpeak_out[[2]]  %>% as.data.frame()


####--------------------------------------
####  Triggered reopening - reopening 100% 
####--------------------------------------

exp_name ="20200919_IL_regreopen100perc_0daysdelay_sm7"
exp_dir <- file.path(simulation_output, 'overflow_simulations',exp_name)
simdat <- f_load_single_exp(exp_dir)
#picu <- f_icu_timeline(dat=simdat,subregions=c("1") ,selected_channel="crit_det", facetVar = "capacity_multiplier")

tab_cumul_100 <-  f_get_cumul_numbers(selected_channel="crit_det_cumul")
#### Get peak  numbers 
tab_peak_100 <-  f_get_peak_numbers(selected_channel="crit_det")



##
exp_name ="20200919_IL_regreopen50perc_0daysdelay_sm7"
exp_dir <- file.path(simulation_output, 'overflow_simulations',exp_name)
simdat <- f_load_single_exp(exp_dir)

#### Get cumulative numbers 
tab_cumul_50 <-  f_get_cumul_numbers(selected_channel="crit_det_cumul")
#### Get peak  numbers 
tab_peak_50 <-  f_get_peak_numbers(selected_channel="crit_det")


unique(tab_peak_50$capacity_multiplier)
tab_peak_50 %>% filter(capacity_multiplier>0.8 & capacity_multiplier <1) %>% as.data.frame() %>% arrange(geography_name )
tab_peak_50 %>% filter(capacity_multiplier>0.4 & capacity_multiplier <0.5) %>% as.data.frame()

tab_peak_50 %>% group_by(geography_name ) %>% filter(q97.5.aboveICU_ratio<1) %>%  filter(capacity_multiplier==max(capacity_multiplier)) %>% 
          mutate(capacity_multiplier=round(capacity_multiplier,1)) %>% arrange(capacity_multiplier )  %>% as.data.frame()

tab_peak_50 %>% filter(geography_name==5) %>% as.data.frame()



rbind(tab_cumul_100, tab_cumul_50) %>% fwrite(file.path(outdir,"icu_cumul.csv" ), quote=FALSE)

rbind(tab_peak_100, tab_peak_50) %>% fwrite(file.path(outdir,"icu_peak.csv" ), quote=FALSE)


