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

#simdate <-'20200919'
simdate <-'20201121'
sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen",exp_names)]
exp_names <- exp_names[c(grep("daysdelay",exp_names),grep("counterfactual",exp_names))]

dat <- f_combineDat(sim_dir,exp_names, "trigger_peak_exceed_df.csv")

subregions=c('covidregion_1','covidregion_4','covidregion_11')
dat <- dat %>% filter(geography_modeled %in% subregions)
dat$scen_name <- gsub(paste0(simdate, "_IL_regreopen"), "", dat$exp_name)
dat <- dat %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat$rollback[is.na(dat$rollback)] <- "counterfactual"


### for how long exceed capacity
ggplot(data=dat) + 
  geom_point(aes(x=capacity_multiplier, y =exceed_diff_1 ))+
  facet_wrap(~region)

  