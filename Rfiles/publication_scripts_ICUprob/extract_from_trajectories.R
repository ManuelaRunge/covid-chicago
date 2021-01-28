
#### Prepare new trajectories extract dataframes


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
  filter(geography_name %in% c(1, 4, 11)) %>%
  rename(avg_resource_available = icu_available)
capacityDat$region <- factor(capacityDat$geography_name, levels = c(1, 4, 11), labels = paste0("Region ", c(1, 4, 11)))


simdate <- "20201212"
sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (!dir.exists(file.path(sim_dir, "ICU_trajectories_plots"))) dir.create(file.path(sim_dir, "ICU_trajectories_plots"))
if (!dir.exists(file.path(sim_dir, "ICU_trajectories_plots", "pdf"))) dir.create(file.path(sim_dir, "ICU_trajectories_plots", "pdf"))

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c(grep("daysdelay", exp_names), grep("counterfactual", exp_names))]
exp_names <- exp_names[!(grepl("_reopen", exp_names))]

for (exp_name in exp_names[15:18]) {
  print(exp_name)
  plotdat_All_list <-list()
  for (reg_nr in c(1, 4, 11)) {
    print(reg_nr)
    selected_region <- paste0("covidregion_", reg_nr)
    dat <- f_load_trajectories(sim_dir, exp_name, region_nr = reg_nr) %>% mutate(exp_name = exp_name) %>% unique()

    dat <- dat %>%
      select(region ,date, exp_name, capacity_multiplier, sample_num, scen_num, crit_det) %>%
      left_join(capacityDat, by = "region") %>%
      filter(date >= as.Date("2020-09-01")) %>%
      group_by(exp_name, capacity_multiplier, sample_num, scen_num) %>%
      mutate(peak = max(crit_det)) %>%
      mutate(above_yn = ifelse(peak > avg_resource_available, 1, 0)) %>%
      f_get_scenVars() %>%
      unique()

    #### get filter for before after peak
    peakDate <- dat %>%
      group_by(exp_name, capacity_multiplier, sample_num, scen_num) %>%
      filter(crit_det == peak) %>%
      mutate(peak_date = date) %>%
      group_by(exp_name, capacity_multiplier, sample_num, scen_num) %>%
      filter(peak_date == min(peak_date)) %>%
     # filter(peak_date <= as.Date("2020-12-31")) %>%
      filter(peak_date >= as.Date("2020-10-01")) %>%
      select(exp_name, capacity_multiplier, sample_num, scen_num, peak_date) %>%
      unique()


    exceedDate <- dat %>%
      group_by(exp_name, capacity_multiplier, sample_num, scen_num) %>%
      filter(crit_det >= avg_resource_available) %>%
      mutate(
        exceed_date_from = min(date),
        exceed_date_to = max(date),
        exceed_diff = exceed_date_to - exceed_date_from
      ) %>%
     # filter(exceed_date_to <= as.Date("2020-12-31")) %>%
      filter(exceed_date_from >= as.Date("2020-10-01")) %>%
      group_by(exp_name, capacity_multiplier, sample_num, scen_num) %>%
      select(exp_name, capacity_multiplier, sample_num, scen_num, exceed_date_from, exceed_date_to, exceed_diff) %>%
      unique()


    plotdat <- dat %>%
      # filter(capacity_multiplier %in% c(0.4, 0.6, 0.8, 1)) %>%
      f_addVar(peakDate) %>%
      f_addVar(exceedDate) %>%
      group_by(exp_name, capacity_multiplier, sample_num, scen_num) %>%
      mutate(
        after_peak_yn = ifelse(date > peak_date, 1, 0),
        after_exceed_yn = ifelse(date > exceed_date_from, 1, 0)
      )
    rm(exceedDate,peakDate)

    #### Trajectory plot
    trigger_capacity_Dat <- plotdat %>%
      select(exp_name, capacity_multiplier, sample_num, scen_num, avg_resource_available, crit_det, date) %>%
      mutate(trigger_capacity_val = avg_resource_available * capacity_multiplier) %>%
      filter(crit_det > trigger_capacity_val) %>%
      group_by(exp_name, capacity_multiplier, sample_num, scen_num, avg_resource_available, trigger_capacity_val) %>%
      filter(date == min(date)) %>%
      rename(trigger_capacity_date = date) %>%
      unique()


    plotdat <- plotdat %>%
      f_addVar(trigger_capacity_Dat) %>%
      group_by(exp_name, region, capacity_multiplier) %>%
      mutate(after_trigger_capacity_yn = ifelse(date > trigger_capacity_date, 1, 0))
    rm(trigger_capacity_Dat)
    
    plotdat <- plotdat %>% filter(!is.na(peak_date))

    plotdat_time <- plotdat %>%  ungroup() %>% filter(capacity_multiplier %in% c(0.4, 0.6, 0.8, 1))
    plotdat_peak <- plotdat %>% ungroup() %>% group_by(exp_name, region, capacity_multiplier, scen_num) %>% filter(crit_det==max(crit_det))

    save(plotdat, file = file.path(sim_dir, exp_name, paste0("trajectories_extract_",reg_nr,".Rdata")))
    save(plotdat_time, file = file.path(sim_dir, exp_name, paste0("trajectories_extract_time_",reg_nr,".Rdata")))
    save(plotdat_peak, file = file.path(sim_dir, exp_name, paste0("trajectories_extract_peak_",reg_nr,".Rdata")))
  
    rm(reg_nr,plotdat,dat,plotdat_time, plotdat_peak, datesDat_long, datesDatAggr_long)
  }
  
  rm(exp_name)
}
