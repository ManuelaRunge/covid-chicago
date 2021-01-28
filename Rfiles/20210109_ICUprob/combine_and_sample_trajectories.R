## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## ICU overflow manuscript
## Assemble trajectories data
## January 2021
## Manuela Runge
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

## -------------------------------
## Settings and packages
## -------------------------------
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
set.seed(2053748)
simdate <- "20210107"

## -------------------------------
## Load capacity data
## -------------------------------
capacityDat <- load_new_capacity(filedate = "20200915")
capacityDat <- capacityDat %>%
  filter(geography_name %in% c(1, 4, 11)) %>%
  rename(avg_resource_available = icu_available)
capacityDat$region <- factor(capacityDat$geography_name, levels = c(1, 4, 11), labels = paste0("Region ", c(1, 4, 11)))


## -------------------------------
## Combine simulations data
## -------------------------------
sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]


datList_raw <- list()
datList <- list()
for (exp_name in exp_names) {
  for (reg_nr in c(1,4, 11)) {
    # load(file.path(sim_dir, exp_name,paste0("trajectories_extract_",reg_nr,".Rdata")))
    selected_region <- paste0("covidregion_", reg_nr)
    dat <- f_load_trajectories(sim_dir, exp_name, region_nr = reg_nr) %>%
      mutate(exp_name = exp_name) %>%
      unique()

    samplesDat <-  fread(file.path(sim_dir, exp_name , "sampled_parameters.csv")) %>% select(scen_num,startdate, time_of_trigger)
    dat <- f_addVar(dat, samplesDat)
    dat <- f_addVar(dat, capacityDat)
    
    triggerDat <- dat %>%
      dplyr::select(region, date,  startdate, exp_name, time_of_trigger, sample_num, scen_num, crit_det, avg_resource_available) %>%
      mutate(
             date_of_trigger = as.Date(startdate)+ time_of_trigger,
             date=as.character(date),
             date_of_trigger= as.character(date_of_trigger)) %>%
      filter(date  ==  date_of_trigger) %>%
      mutate(date=as.Date(date),
             date_of_trigger=as.Date(date_of_trigger),
             trigger_threshold = crit_det,
             trigger_threshold_perc = (crit_det/avg_resource_available)*100) %>%
      ungroup() %>%
      dplyr::select(region, exp_name, scen_num, trigger_threshold, trigger_threshold_perc)
    
    dat <-  f_addVar(dat, triggerDat)
    summary(dat$trigger_threshold)
    f_timeline_simple()
    
    plotdat <- dat %>%
      select(region, date, exp_name, time_of_trigger, sample_num, scen_num,
             avg_resource_available,crit_det,trigger_threshold, trigger_threshold_perc) %>%
      filter(date >= as.Date("2020-10-01")) %>%
      group_by(exp_name,region, time_of_trigger, sample_num, scen_num) %>%
      mutate(peak = max(crit_det)) %>%
      mutate(above_yn = ifelse(peak > avg_resource_available, 1, 0)) %>%
      group_by(scen_num,region, time_of_trigger, exp_name) %>%
      filter(crit_det == max(crit_det)) %>%
      filter(date == min(date)) %>%
      mutate(date=as.Date(date),peak_date = date)
    
    
    
    datList_raw[[length(datList_raw) + 1]] <- dat
    datList[[length(datList) + 1]] <- plotdat
  }
}



## -------------------------------
## Save as csv
## -------------------------------
if(!dir.exists(file.path(sim_dir, "csvs")))dir.create(file.path(sim_dir, "csvs"))

dat <- datList %>% bind_rows()
dim(dat)
table(dat$exp_name, dat$region)
table(dat$exp_name, dat$time_of_trigger)

dat <- dat %>% mutate(
  peak_before_2021 = ifelse(peak_date <= as.Date("2020-12-31"), "before", "after"),
  capacity_before_2021 = ifelse(crit_det >= avg_resource_available & peak_date <= as.Date("2020-12-31"), "before", "after")
)

fwrite(dat, file = file.path(sim_dir, "csvs", "dat_combined.csv"))
  
dat_raw <- datList_raw %>% bind_rows()
dim(dat_raw)
table(dat_raw$exp_name, dat_raw$region)
table(dat_raw$exp_name, dat_raw$time_of_trigger)
fwrite(dat_raw, file = file.path(sim_dir, "csvs", "dat_timeline_combined.csv"))



