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


dat <- f_combineDat(sim_dir, exp_names, "hospitaloverflow.csv") %>%
  filter(geography_modeled %in% c(1,4,11)) 

dat$region <- factor(dat$geography_modeled, levels = c(1,4,11), labels = paste0("Region ", c(1,4,11)))


table(dat$exp_name, dat$region)


dat %>% 
  group_by(exp_name) %>% 
  summarize(mean_n_scen = mean(nscen), 
            min_n_scen = min(nscen), 
            max_n_scen = max(nscen)) %>% 
  arrange(min_n_scen)


dat %>% 
  group_by(capacity_multiplier) %>% 
  summarize(mean_n_scen = mean(nscen), 
            min_n_scen = min(nscen), 
            max_n_scen = max(nscen)) %>% 
  arrange(min_n_scen)
