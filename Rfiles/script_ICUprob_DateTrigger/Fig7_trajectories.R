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

theme_set(theme_minimal())
capacity_col <- "#2a3b90"

Location <- "Local"
simdate <- "20210107"
exp_stem <- paste0(simdate, "_IL_regreopen")

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("script_ICUprob_DateTrigger/functions.R")
customTheme <- f_getCustomTheme()


if (Location == "Local") sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (Location == "NUCLUSTER") sim_dir <- "/projects/p30781/covidproject/covid-chicago/_temp"
if (!dir.exists(file.path(sim_dir, "ICU_bar_plots"))) dir.create(file.path(sim_dir, "ICU_bar_plots"))


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
# mutate(exp_name = exp_name) %>%
#  unique()

dat <- fread(file.path(sim_dir, "csvs", "dat_combined.csv"))
table(dat_subsample_50$exp_name, dat_subsample_50$region)

## -------------------------------
## Plot probabilities
## -------------------------------
dat$time_of_trigger <- factor(dat$time_of_trigger)

dat$region <- factor(dat$region, levels =  paste0("Region ", c(1,4,11)), labels = paste0("Region ",  c(1,4,11)))

dat <- as.data.frame(dat_df)
dat$date <- as.Date(dat$date)

  dat_prob <- f_calculate_prob(dfDat = dat)
  pplot <- f_plot_prob()



  f_save_plot(
    plot_name = "ICU_prob_trajectories_full_date", pplot = pplot,
    plot_dir = file.path(sim_dir, "ICU_prob_plots"), width = 16, height = 8
  )

  rm(plot_name, dat_prob, pplot)



## -------------------------------
## Plot probabilities with resampling
## -------------------------------
# set.seed(2053748)

dat_sub_50_list <- list()
for (i in c(1:100)) {
  nsubsample_50 <- dat %>%
    group_by(region, exp_name, time_of_trigger) %>%
    sample_n(35, replace = FALSE) %>%
    mutate(
      nsamples_sub = n_distinct(sample_num),
      scen_num_sel = scen_num
    ) %>%
    dplyr::select(region, exp_name, time_of_trigger, scen_num, scen_num_sel, nsamples_sub) %>%
    unique()


  dat_subsample_50 <- dat %>%
    select(-sample_num) %>%
    left_join(nsubsample_50, by = c("region", "exp_name", "time_of_trigger", "scen_num")) %>%
    filter(scen_num == scen_num_sel)

  dat_sub_50_list[[length(dat_sub_50_list) + 1]] <- f_calculate_prob(dfDat = dat_subsample_50) %>%
    mutate(resample_n = i)

  rm(dat_subsample_50)
}


dat_prob <- dat_sub_50_list %>%
  bind_rows() %>%
  group_by(exp_name, region, time_of_trigger) %>%
  summarize(
   # prob_lower = min(prob),
   # prob_upper = max(prob),
    prob_lower = quantile(prob, probs = 0.25, na.rm = TRUE),
    prob_upper = quantile(prob, probs = 0.75, na.rm = TRUE),
    prob = mean(prob)
  ) %>%
  f_get_scenVars() %>%
  mutate(date = as.Date("2020-12-31")) %>%
  filter(!is.na(rollback))


pplot <- f_plot_prob()
pplot <- pplot +     
  geom_ribbon(aes(
    x = time_of_trigger, 
    y = prob * 100,
    ymin = prob_lower * 100,
    ymax = prob_upper * 100,
    fill = reopen,
    group = interaction(reopen, rollback_fct)
  ), alpha=0.3) 

pplot

f_save_plot(
  plot_name = "ICU_prob_trajectories_sub50_v1_resampled", pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_prob_plots"), width = 12, height = 8
)



pplot <- ggplot(data = subset(dat_prob, rollback  %in% c("pr2","pr8"))) +
  geom_ribbon(aes(
    x = time_of_trigger, 
    y = prob * 100,
    ymin = prob_lower * 100,
    ymax = prob_upper * 100,
    fill = reopen,
    group = interaction(reopen,rollback) 
  ), alpha=0.3) +
  geom_line(aes(
    x = time_of_trigger, y = prob * 100,
    col = reopen,
    alpha = rollback_fct,
    linetype = rollback_fct,
    group = interaction(reopen, rollback_fct)
  ), size = 1) +
  facet_wrap( ~ region, scales = "free") +
  scale_y_continuous(
    lim = c(0, 102), expand = c(0, 0),
    breaks = seq(0, 100, 20),
    minor_breaks = seq(0, 100, 10)
  ) +
  scale_color_manual(values = c(TwoCols_seq)) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  scale_alpha_manual(values = c(1, 0.75, 0.5, 0.2, 0.1)) +
  customTheme +
  theme(
    panel.spacing = unit(2, "lines"),
    # legend.position = "None",
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(size = 0.75)
  ) +
  labs(
    y = "Probability of ICU overflow (%)",
    x = "Trigger threshold (% of available ICU beds)",
    color = "Transmission\nincrease", fill = "Transmission\nincrease",
    alpha = "Mitigation strengths", linetype = "Mitigation strengths"
  )
pplot

f_save_plot(
  plot_name = "ICU_prob_trajectories_sub50_v2_resampled", pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_prob_plots"), width = 12, height = 4
)



pplot <- ggplot(data = subset(dat_prob, rollback  %in% c("pr2","pr8"))) +
  geom_ribbon(aes(
    x = time_of_trigger, 
    y = prob * 100,
    ymin = prob_lower * 100,
    ymax = prob_upper * 100,
    fill = reopen,
    group = interaction(reopen, rollback, region) 
  ), alpha=0.3) +
  geom_line(aes(
    x = time_of_trigger, y = prob * 100,
    col = reopen,
    size = region,
    linetype=region,
    group = interaction(reopen, rollback_fct,region)
  )) +
  facet_wrap( ~ rollback, scales = "free") +
  scale_y_continuous(
    lim = c(0, 102), expand = c(0, 0),
    breaks = seq(0, 100, 20),
    minor_breaks = seq(0, 100, 10)
  ) +
  scale_color_manual(values = c(TwoCols_seq)) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  scale_size_manual(values = c(0.7, 1, 1.5)) +
  customTheme +
  theme(
    panel.spacing = unit(2, "lines"),
    # legend.position = "None",
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(size = 0.75)
  ) +
  labs(
    y = "Probability of ICU overflow (%)",
    x = "Trigger threshold (% of available ICU beds)",
    color = "Transmission\nincrease", fill = "Transmission\nincrease",
    size = "Region",    linetype = "Region"
  )


pplot



f_save_plot(
  plot_name = "ICU_prob_trajectories_sub50_v3_resampled", pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_prob_plots"), width = 10, height = 4
)


