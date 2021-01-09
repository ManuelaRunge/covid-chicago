## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## ICU overflow manuscript
## Figure 7
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

## -------------------------------
## Load data
## -------------------------------
capacityDat <- load_new_capacity(filedate = "20200915")
capacityDat <- capacityDat %>%
  filter(geography_name %in% c(1, 4, 11)) %>%
  rename(avg_resource_available = icu_available)
capacityDat$region <- factor(capacityDat$geography_name,
  levels = c(1, 4, 11),
  labels = paste0("Region ", c(1, 4, 11))
)


#    dat <- f_load_trajectories(sim_dir, exp_name, region_nr = reg_nr) %>%
#mutate(exp_name = exp_name) %>%
#  unique()

dat <- fread(file.path(sim_dir, "csvs", "dat_1daysdelay.csv"))
dat_subsample_100 <- fread(file.path(sim_dir, "csvs", "dat_subsample_100_1daysdelay.csv"))
dat_subsample_50 <- fread(file.path(sim_dir, "csvs", "dat_subsample_50_1daysdelay.csv"))


## -------------------------------
## Plot probabilities
## -------------------------------
add_counterfactual_to_xaxis <- FALSE
dfList <- list(dat, dat_subsample_100, dat_subsample_50)
date_max_list <- c("2020-12-31", "2023-12-31")


for (date_max in date_max_list) {
  for (i in c(1:length(dfList))) {
    
    dat_df <- dfList[[i]]
    dat_df <- as.data.frame(dat_df)
    dat_df$date <- as.Date(dat_df$date)

    dat_prob <- f_calculate_prob(dfDat = dat_df, date_max = date_max)
    if (add_counterfactual_to_xaxis) dat_prob <- f_add_counterfactual_to_xaxis()
    
    pplot <- f_plot_prob(add_counterfactual_to_xaxis=add_counterfactual_to_xaxis)

    plot_name_stem <- "ICU_prob_trajectories_"
    if (i == 1) plot_name <- paste0(plot_name_stem, "full")
    if (i == 2) plot_name <- paste0(plot_name_stem, "sub100")
    if (i == 3) plot_name <- paste0(plot_name_stem, "sub50")

    if (add_counterfactual_to_xaxis) {
      plot_name <- paste0(plot_name, "_v2")
    } else {
      plot_name <- paste0(plot_name, "_v1")
    }
    plot_name <- paste0(plot_name, "_", gsub("-", "", date_max))

    f_save_plot(
      plot_name = plot_name, pplot = pplot,
      plot_dir = file.path(sim_dir, "ICU_prob_plots"), width = 12, height = 8
    )
    
    rm(plot_name, dat_prob, pplot)
  }
}


## -------------------------------
## Plot probabilities with resampling
## -------------------------------
#set.seed(2053748)

dat_sub_50_list <- list()
for(i in c(1:50)){
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


dat_subsample_50<- dat %>%
  filter(crit_det >= trigger_capacity_val) %>%
  select(-sample_num) %>%
  left_join(nsubsample_50, by = c("region", "exp_name", "capacity_multiplier", "scen_num")) %>%
  filter(scen_num == scen_num_sel)
  
  dat_sub_50_list[[length(dat_sub_50_list)+1]] <- f_calculate_prob(dfDat = dat_subsample_50, date_max = date_max)  %>%
    mutate(resample_n= i)
  
  rm(dat_subsample_50)
}


dat_prob <- dat_sub_50_list %>%
  bind_rows() %>%
  group_by(exp_name,region, capacity_multiplier) %>%
  summarize(prob_lower= min(prob),
            prob_upper = max(prob),
            prob=mean(prob)) %>%
  f_get_scenVars() %>%
  mutate(date=as.Date("2020-12-31")) %>%
  filter(!is.na(rollback))


pplot <- f_plot_prob(add_counterfactual_to_xaxis=FALSE,add_ribbon=TRUE)

pplot

f_save_plot(
  plot_name = "ICU_prob_trajectories_sub50_v1_resampled", pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_prob_plots"), width = 12, height = 8
)






