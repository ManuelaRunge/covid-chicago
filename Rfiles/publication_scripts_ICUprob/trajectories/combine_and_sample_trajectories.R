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
simdate <- "20201212"

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
exp_names <- exp_names[c(grep("daysdelay", exp_names), grep("counterfactual", exp_names))]
exp_names <- exp_names[!(grepl("_reopen", exp_names))]
exp_names <- exp_names[!(grepl("7daysdelay", exp_names))]

datList_raw <- list()
datList <- list()
for (exp_name in exp_names) {
  for (reg_nr in c(1, 4, 11)) {
    # load(file.path(sim_dir, exp_name,paste0("trajectories_extract_",reg_nr,".Rdata")))
    selected_region <- paste0("covidregion_", reg_nr)
    dat <- f_load_trajectories(sim_dir, exp_name, region_nr = reg_nr) %>%
      mutate(exp_name = exp_name) %>%
      unique()

    plotdat <- dat %>%
      select(region, date, exp_name, capacity_multiplier, sample_num, scen_num, crit_det) %>%
      left_join(capacityDat, by = "region") %>%
      filter(date >= as.Date("2020-10-01")) %>%
      group_by(exp_name, capacity_multiplier, sample_num, scen_num) %>%
      mutate(peak = max(crit_det)) %>%
      mutate(above_yn = ifelse(peak > avg_resource_available, 1, 0)) %>%
      f_get_scenVars() %>%
      mutate(trigger_capacity_val = (avg_resource_available * capacity_multiplier)) %>%
      group_by(scen_num, capacity_multiplier, exp_name) %>%
      filter(crit_det == max(crit_det)) %>%
      filter(date == min(date)) %>%
      mutate(peak_date = date)

    plotdat$reopen_fct <- factor(plotdat$reopen,
      levels = c("100perc", "50perc"),
      labels = c(
        "High\ntransmission\nincrease",
        "Low\ntransmission\nincrease"
      )
    )
    #datList_raw[[length(datList_raw) + 1]] <- dat
    datList[[length(datList) + 1]] <- plotdat
  }
}

dat <- datList %>% bind_rows()
dim(dat)
table(dat$exp_name, dat$region)
table(dat$exp_name, dat$capacity_multiplier)

dat <- dat %>% mutate(
  peak_before_2021 = ifelse(peak_date <= as.Date("2020-12-31"), "before", "after"),
  trigger_before_2021 = ifelse(crit_det >= trigger_capacity_val & peak_date <= as.Date("2020-12-31"), "before", "after"),
  capacity_before_2021 = ifelse(crit_det >= avg_resource_available & peak_date <= as.Date("2020-12-31"), "before", "after")
)

table(dat$peak_before_2021, dat$reopen_fct)
table(dat$trigger_before_2021, dat$reopen_fct)
table(dat$capacity_before_2021, dat$reopen_fct)


## -------------------------------
## Draw sub-samples
## -------------------------------
nsubsample_100 <- dat %>%
  group_by(region, exp_name, capacity_multiplier) %>%
  sample_n(100, replace = FALSE) %>%
  mutate(
    nsamples_sub = n_distinct(sample_num),
    scen_num_sel = scen_num
  ) %>%
  dplyr::select(region, exp_name, capacity_multiplier, scen_num, scen_num_sel, nsamples_sub) %>%
  unique()

nsubsample_50 <- dat %>%
  filter(crit_det >= trigger_capacity_val) %>%
  group_by(region, exp_name, capacity_multiplier) %>%
  sample_n(50, replace = FALSE) %>%
  mutate(
    nsamples_sub = n_distinct(sample_num),
    scen_num_sel = scen_num
  ) %>%
  dplyr::select(region, exp_name, capacity_multiplier, scen_num, scen_num_sel, nsamples_sub) %>%
  unique()


table(nsubsample_100$exp_name, nsubsample_100$region)
table(nsubsample_100$exp_name, nsubsample_100$capacity_multiplier)

table(nsubsample_50$exp_name, nsubsample_50$region)
table(nsubsample_50$exp_name, nsubsample_50$capacity_multiplier)


dat_subsample_100 <- dat %>%
  select(-sample_num) %>%
  left_join(nsubsample_100, by = c("region", "exp_name", "capacity_multiplier", "scen_num")) %>%
  filter(scen_num == scen_num_sel)

dat_subsample_50 <- dat %>%
  filter(crit_det >= trigger_capacity_val) %>%
  select(-sample_num) %>%
  left_join(nsubsample_50, by = c("region", "exp_name", "capacity_multiplier", "scen_num")) %>%
  filter(scen_num == scen_num_sel)


dim(dat)
dim(dat_subsample_100)
dim(dat_subsample_50)


summary(dat_subsample_100$peak_date)
summary(dat_subsample_50$peak_date)
summary(dat_subsample_100$crit_det)
summary(dat_subsample_50$crit_det)

table(dat_subsample_100$exp_name, dat_subsample_100$region)
table(dat_subsample_50$exp_name, dat_subsample_50$region)

table(dat_subsample_50$rollback, dat_subsample_50$region)

## -------------------------------
## Save as csv
## -------------------------------
SAVE <- FALSE
if (SAVE) {
  fwrite(dat, file = file.path(sim_dir, "csvs", "dat_1daysdelay.csv"))
  fwrite(dat_subsample_100, file = file.path(sim_dir, "csvs", "dat_subsample_100_1daysdelay.csv"))
  fwrite(dat_subsample_50, file = file.path(sim_dir, "csvs", "dat_subsample_50_1daysdelay.csv"))
  fwrite(nsubsample_100, file = file.path(sim_dir, "csvs", "nsubsample_100_1daysdelay.csv"))
  fwrite(nsubsample_50, file = file.path(sim_dir, "csvs", "nsubsample_50_1daysdelay.csv"))
}
