

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

## -------------------------------
## Load data
## -------------------------------
simdate <- "20201212"
sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (!dir.exists(file.path(sim_dir, "ICU_bar_plots"))) dir.create(file.path(sim_dir, "ICU_bar_plots"))
if (!dir.exists(file.path(sim_dir, "ICU_reduction_plots"))) dir.create(file.path(sim_dir, "ICU_reduction_plots"))

exp_name1 =  "20201212_IL_regreopen100perc_counterfactual"
exp_name2 = "20201212_IL_regreopen50perc_counterfactual"
dat1 <- f_load_trajectories(sim_dir, exp_name1 , region_nr =11) %>% mutate(exp_name=exp_name1)
dat2 <- f_load_trajectories(sim_dir, exp_name2, region_nr =11) %>% mutate(exp_name=exp_name1)
dat_counterfactual <- rbind(dat1, dat2)

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c(grep("daysdelay", exp_names), grep("counterfactual", exp_names))]
exp_names <- exp_names[!(grepl("_reopen", exp_names))]

dat <- f_combineDat(sim_dir, exp_names, "peak_exceed_df.csv") %>%
  filter(ems %in% c("EMS-11")) %>%
  f_get_scenVars()

dat$region <- factor(dat$ems, levels = c(paste0("EMS-", 11)), labels = paste0("Region ", 11))

rollback_values <- unique(dat$rollback)
delay_values <- unique(dat$delay)
rollback_val <- rollback_values[2]
delay_val <- delay_values[1]

table(dat$capacity_multiplier_fct, exclude = NULL)
dat$capacity_multiplier_fct

dat$perc_ICU_occup_50CI_lower <- (dat$critical_50CI_lower / dat$avg_resource_available) * 100
dat$perc_ICU_occup_50CI_upper <- (dat$critical_50CI_upper / dat$avg_resource_available) * 100
dat$perc_ICU_occup_95CI_lower <- (dat$critical_95CI_lower / dat$avg_resource_available) * 100
dat$perc_ICU_occup_95CI_upper <- (dat$critical_95CI_upper / dat$avg_resource_available) * 100
dat$perc_ICU_occup_median <- (dat$critical_median / dat$avg_resource_available) * 100

## -------------------------------
## Assessing peak ICU occupancy
## -------------------------------
plotdat <- dat %>%
  filter((delay == "counterfactual" | delay == delay_val)) %>%
  filter((delay == "counterfactual" | capacity_multiplier %in% c(0, 0.2, 0.4, 0.6, 0.8, 1)))
annotationDat <- unique(plotdat[, c("region", "avg_resource_available")])

pplot <- ggplot(data = plotdat) +
  geom_bar( aes(
      x = capacity_multiplier_fct,
      y = critical_median, group = reopen, fill = reopen
    ),
    stat = "identity", position = position_dodge(width = 0.7), width = 0.8,
  ) +
  geom_errorbar(aes(
    x = capacity_multiplier_fct,
    ymin = critical_95CI_lower,
    ymax = critical_95CI_upper, group = reopen
  ),
  position = position_dodge(width = 0.7), width = 0
  ) +
  labs(
    y = "Predictd peak in ICU census\nuntil end of December",
    x = "Trigger threshold\n (% of ICU capacity)"
  ) +
  geom_hline(
    data = annotationDat,
    aes(yintercept = avg_resource_available),
    linetype = "dashed", col = capacity_col, size = 1
  ) +
  facet_wrap(reopen~rollback,  nrow=2) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  geom_hline(yintercept = c(0)) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  customTheme

pplot


plotdat %>% 
  filter(rollback %in% c("pr6", "counterfactual") & capacity_multiplier %in% c(-9,0,0.8)) %>% 
  select(exp_name, capacity_multiplier ,critical_median, perc_ICU_occup_median)


dat %>% ungroup() %>%
  filter(rollback !="counterfactual" & rollback=="pr8") %>% 
  dplyr::select(delay, rollback, reopen , capacity_multiplier ,critical_median) %>%
  pivot_wider(names_from=c("delay"), values_from= c("critical_median")) %>%
  mutate(delay_incr =`7daysdelay`- `1daysdelay` ) %>%
  group_by(reopen) %>%
  summarize(delay_incr=mean(delay_incr))



## -------------------------------
## Assessing proabilit
## -------------------------------
#dat <- f_load_trajectories(sim_dir, exp_name, region_nr = reg_nr) %>%
# mutate(exp_name = exp_name) %>%
#  unique()


dat <- fread(file.path(sim_dir, "csvs", "dat_1daysdelay.csv"))  %>% filter(region=="Region 11")
dat_subsample_100 <- fread(file.path(sim_dir, "csvs", "dat_subsample_100_1daysdelay.csv"))  %>% filter(region=="Region 11")
dat_subsample_50 <- fread(file.path(sim_dir, "csvs", "dat_subsample_50_1daysdelay.csv")) %>% filter(region=="Region 11")

## -------------------------------
## Plot probabilities
## -------------------------------
add_counterfactual_to_xaxis <- FALSE
dfList <- list(dat, dat_subsample_100, dat_subsample_50)
date_max_list <- c("2020-12-31", "2023-12-31")
date_max <- date_max_list[1]

dat_sub_50_list <- list()
for(i in c(1:100)){
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

pplot_top <- ggplot(data = subset(dat_prob, region=="Region 11" & rollback !="counterfactual")) +
  geom_ribbon(aes(
    x = capacity_multiplier *100, 
    y = prob * 100,
    ymin = prob_lower * 100,
    ymax = prob_upper * 100,
    fill = reopen_fct,
    group = interaction(reopen, rollback_fct)
  ), alpha=0.3) +
  geom_line(aes(
    x = capacity_multiplier*100, y = prob * 100,
    col = reopen_fct,
    group = interaction(reopen,rollback)
  ), size = 1.3) +
  facet_wrap( ~ rollback, scales = "free", nrow=1) +
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
    legend.position = "bottom",
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(size = 0.75)
  ) +
  labs(
    y = "Probability of ICU overflow (%)",
    x = "Trigger threshold\n(% of available ICU beds)",
    color = "Transmission increase", fill = "Transmission increase",
    alpha = "Mitigation strengths"
  )


pplot_bottom <- ggplot(data = subset(dat_prob, region=="Region 11" & rollback !="counterfactual")) +
  geom_ribbon(aes(
    x = capacity_multiplier *100, 
    y = prob * 100,
    ymin = prob_lower * 100,
    ymax = prob_upper * 100,
    fill = reopen_fct2,
    group = interaction(reopen, rollback_fct)
  ), alpha=0.3) +
  geom_line(aes(
    x = capacity_multiplier*100, y = prob * 100,
    col = reopen_fct2,
    alpha=rollback_fct,
    group = interaction(reopen,rollback)
  ), size = 1.3) +
  facet_wrap( ~ reopen_fct2, scales = "free", nrow=1) +
  scale_y_continuous(
    lim = c(0, 102), expand = c(0, 0),
    breaks = seq(0, 100, 20),
    minor_breaks = seq(0, 100, 10)
  ) +
  scale_color_manual(values = c(TwoCols_seq)) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  scale_alpha_manual(values = c(1, 0.75, 0.5, 0.2, 0.1)) +
  customTheme +
  guides(fill = FALSE, 
         color = FALSE)  +
  theme(
    panel.spacing = unit(2, "lines"),
    legend.position = "right",
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(size = 0.75)
  ) +
  labs(
    y = "Probability of ICU overflow (%)",
    x = "Trigger threshold\n(% of available ICU beds)",
    color = "Transmission increase", fill = "Transmission increase",
    alpha = "Mitigation strengths"
  )


pplot <- plot_grid(pplot_top, pplot_bottom, nrow=2)
pplot



dat_prob$capacity_multiplier_rev <- factor(dat_prob$capacity_multiplier, levels=rev(seq(0,1,0.1)), labels=rev(seq(0,1,0.1))*100)

pplot_bottom2 <- ggplot(data = subset(dat_prob, region=="Region 11" & rollback !="counterfactual")) +
  geom_line(aes(
    x = capacity_multiplier_rev, y = prob * 100,
    col = reopen_fct2,
    alpha=rollback_fct,
    group = interaction(reopen,rollback)
  ), size = 1.3) +
  facet_wrap( ~ reopen_fct2, scales = "free", nrow=1) +
  scale_y_continuous(
    lim = c(0, 102), expand = c(0, 0),
    breaks = seq(0, 100, 20),
    minor_breaks = seq(0, 100, 10)
  ) +
  scale_color_manual(values = c(TwoCols_seq)) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  scale_alpha_manual(values = c(1, 0.75, 0.5, 0.2, 0.1)) +
  customTheme +
  guides(fill = FALSE, 
         color = FALSE)  +
  theme(
    panel.spacing = unit(2, "lines"),
    legend.position = "right",
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(size = 0.75)
  ) +
  labs(
    y = "Probability of ICU overflow (%)",
    x = "Trigger threshold\n(% of available ICU beds)",
    color = "Transmission increase", fill = "Transmission increase",
    alpha = "Mitigation strengths"
  )


f_save_plot(
  plot_name = "ICU_prob_trajectories_sub50_v1_resampled_reg11", pplot = pplot_bottom2,
  plot_dir = file.path(sim_dir, "ICU_chicago_plots"), width = 14, height = 6
)


dat_prob %>% filter(region=="Region 11" & rollback !="counterfactual") %>%
  filter(capacity_multiplier==0.8) %>%
  select(exp_name, rollback, prob_lower, prob_upper,   prob) %>%
  as.data.frame()


dat_prob %>% filter(region=="Region 11" & rollback !="counterfactual") %>%
  filter(capacity_multiplier==0.8) %>%
  select(exp_name, rollback, prob_lower, prob_upper,   prob) %>%
  as.data.frame()



################################################################


plotdat <- dat %>%
  filter((delay == "counterfactual" | delay == delay_values[2])) %>%
  filter((delay == "counterfactual" | capacity_multiplier %in% c(0, 0.2, 0.4, 0.6, 0.8, 1)))
annotationDat <- unique(plotdat[, c("region", "avg_resource_available")])


pplot_bottom2 <- ggplot(data = subset(dat_prob, region=="Region 11" & rollback !="counterfactual")) +
  geom_line(aes(
    x = capacity_multiplier_rev, y = prob * 100,
    col = reopen_fct2,
    alpha=rollback_fct,
    group = interaction(reopen,rollback)
  ), size = 1.3) +
  facet_wrap( ~ reopen_fct2, scales = "free", nrow=1) +
  scale_y_continuous(
    lim = c(0, 102), expand = c(0, 0),
    breaks = seq(0, 100, 20),
    minor_breaks = seq(0, 100, 10)
  ) +
  scale_color_manual(values = c(TwoCols_seq)) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  scale_alpha_manual(values = c(1, 0.75, 0.5, 0.2, 0.1)) +
  customTheme +
  guides(fill = FALSE, 
         color = FALSE)  +
  theme(
    panel.spacing = unit(2, "lines"),
    legend.position = "right",
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(size = 0.75)
  ) +
  labs(
    y = "Probability of ICU overflow (%)",
    x = "Trigger threshold\n(% of available ICU beds)",
    color = "Transmission increase", fill = "Transmission increase",
    alpha = "Mitigation strengths"
  )


f_save_plot(
  plot_name = "ICU_prob_trajectories_sub50_v1_resampled_reg11", pplot = pplot_bottom2,
  plot_dir = file.path(sim_dir, "ICU_chicago_plots"), width = 14, height = 6
)











plotdat$capacity_multiplier_rev <- factor(plotdat$capacity_multiplier, levels=rev(c(seq(0,1, 0.2),-9)), labels=rev(c(seq(0,1, 0.2)*100,"none")))
  
pplot <- ggplot(data = subset(plotdat, (rollback=="pr6" | rollback=="counterfactual"))) +
  geom_bar( aes(
    x = capacity_multiplier_rev,
    y = critical_median, group = reopen, fill = reopen
  ),
  stat = "identity", position = position_dodge(width = 0.9), width = 0.8,
  ) +
  geom_errorbar(aes(
    x = capacity_multiplier_rev,
    ymin = critical_95CI_lower,
    ymax = critical_95CI_upper, group = reopen
  ),
  position = position_dodge(width = 0.9), width = 0
  ) +
  labs(
    y = "Predictd peak in ICU census\nuntil end of December",
    x = "Trigger threshold\n (% of ICU capacity)"
  ) +
  geom_hline(
    data = annotationDat,
    aes(yintercept = avg_resource_available),
    linetype = "dashed", col = capacity_col, size = 1
  ) +
  facet_wrap(~reopen,  nrow=2) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  geom_hline(yintercept = c(0)) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  customTheme

pplot



f_save_plot(
  plot_name = "ICU_barplot_reg11", pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_chicago_plots"), width = 14, height = 10
)