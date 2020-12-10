

library(tidyverse)
library(cowplot)
library(data.table)
library(raster)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

theme_set(theme_cowplot())
customTheme <- f_getCustomTheme(fontscl = 0)

SAVE <- FALSE
analysis_dir <- file.path(simulation_output, "_overflow_simulations")
civis_dir <- file.path(simulation_output, "_simulations_for_civis")
outdir <- file.path(file.path(project_path, "/project_notes/publications/covid_model_IL_overflow/out"))

library(RColorBrewer)
getPalette = colorRampPalette(brewer.pal(8, "Dark2"))(15)

#TwoCols <- c("firebrick3","steelblue3")
TwoCols <- c("indianred", "deepskyblue3")


# simulation_output <- file.path(simulation_output, "_overflow_simulations")


#### --------------------------------------
#### Experiment names and locations
#### --------------------------------------
exp_name_baseline <- "20201006_IL_baseline_oldsm8"
exp_dir_baseline <- file.path(civis_dir, exp_name_baseline)

exp_name_reopen <- "20201110_IL_mr_gradual_reopening_Sep2" #  "20200917_IL_gradual_reopening"
exp_dir_reopen <- file.path(analysis_dir, exp_name_reopen)


exp_names_sm4 <- c(
  "20200919_IL_regreopen50perc_0daysdelay_sm4", "20200919_IL_regreopen100perc_0daysdelay_sm4",
  "20200919_IL_regreopen50perc_3daysdelay_sm4", "20200919_IL_regreopen100perc_3daysdelay_sm4",
  "20200919_IL_regreopen50perc_7daysdelay_sm4", "20200919_IL_regreopen100perc_7daysdelay_sm4"
)

exp_names_sm7 <- c(
  "20200919_IL_regreopen50perc_0daysdelay_sm7", "20200919_IL_regreopen100perc_0daysdelay_sm7",
  "20200919_IL_regreopen50perc_3daysdelay_sm7", "20200919_IL_regreopen100perc_3daysdelay_sm7",
  "20200919_IL_regreopen50perc_7daysdelay_sm7", "20200919_IL_regreopen100perc_7daysdelay_sm7"
)

exp_name_regreopen_combined <- "20200919_IL_regreopen_combined"



### Calculate % rebound
## Load initial Ki and time-varying Ki

Ki_dat <- f_initial_and_timevarying_Ki(exp_dir=exp_dir_reopen, param=c("reopening_multiplier_4","capacity_multiplier", "trigger_delay_days"))
library(RColorBrewer)
getPalette = colorRampPalette(brewer.pal(8, "Dark2"))(15)

pplot <- ggplot(data=Ki_dat)+ 
  geom_line(aes(x=date, y= Ki_rebound, 
    col=as.factor(round(reopening_multiplier_4,2)), linetype=as.factor(trigger_delay_days)))+
  facet_wrap(~region)+
  labs(x="", y="", color="Reopen", linetype="")+
  background_grid()+
  customTheme+
  scale_color_manual(values=getPalette)

pplot <- ggplot(data=subset(Ki_dat, date==max(date)))+ 
  geom_bar(aes(x=as.factor(round(reopening_multiplier_4,2)), y= Ki_rebound, 
                fill=as.factor(round(reopening_multiplier_4,2))), stat="identity")+
  facet_wrap(~region)+
  labs(x="Region", y="", fill="Reopen", linetype="")+
  background_grid()+
  customTheme+
  scale_fill_manual(values=getPalette)



####
exp_name <- "20201111_IL_test_regreopen50perc_sm4"
exp_dir <- file.path(analysis_dir,exp_name)
trajectoriesDat <- f_merge_Rdata(exp_dir=exp_dir, paramvars=c("capacity_multiplier", "trigger_delay_days"))
table(trajectoriesDat$capacity_multiplier)
table(trajectoriesDat$trigger_delay_days)

simdat <- f_load_single_exp(exp_dir) %>% as.data.frame()
pplot <- f_icu_timeline(dat = simdat, subregions = unique(simdat$geography_name), selected_channel = "crit_det") +facet_wrap(~geography_name, scales="free")



###
#source("1_region_characteristics.R")
#source("1_region_baseline_characteristics.R")
#source("2_counterfactual_description.R")




