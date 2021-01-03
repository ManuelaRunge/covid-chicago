library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")
theme_set(theme_minimal())
## -------------------------------
## Run script
## -------------------------------
simdate <- "20201212"
sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (!dir.exists(file.path(sim_dir, "ICU_duration_plots"))) dir.create(file.path(sim_dir, "ICU_duration_plots"))

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c(grep("daysdelay", exp_names), grep("counterfactual", exp_names))]
exp_names <- exp_names[!(grepl("_reopen", exp_names))]

dat <- f_combineDat(sim_dir, exp_names, "peak_exceed_df.csv")

subregions <- c("covidregion_1", "covidregion_4", "covidregion_11")
dat$region <- factor(dat$region, levels = c(1:11), labels = paste0("Region ", c(1:11)))

dat <- dat %>% filter(geography_modeled %in% subregions)
dat$scen_name <- gsub(paste0(simdate, "_IL_regreopen"), "", dat$exp_name)
dat <- dat %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat$rollback[is.na(dat$rollback)] <- "counterfactual"

dat$capacity_multiplier_fct <- round(dat$capacity_multiplier * 100, 0)
fct_labels <- sort(unique(dat$capacity_multiplier_fct))
dat$capacity_multiplier_fct[dat$rollback == "counterfactual"] <- "counterfactual"
dat$capacity_multiplier_fct <- factor(dat$capacity_multiplier_fct,
  levels = c(fct_labels, "counterfactual"),
  labels = c(fct_labels, "counterfactual")
)
table(dat$capacity_multiplier_fct, exclude = NULL)

dat$reopen_fct <- factor(dat$reopen,
  levels = c("100perc", "50perc"),
  labels = c("High\ntransmission\nncrease", "Low\ntransmission\nincrese")
)

dat$reopen_fct2 <- factor(dat$reopen,
  levels = c("100perc", "50perc"),
  labels = c("High", "Low")
)

dat$rollback_fct <- factor(dat$rollback,
  levels = c("pr8", "pr6", "pr4", "pr2"),
  labels = rev(seq(20, 80, 20))
)

rollback_values <- unique(dat$rollback)
delay_values <- unique(dat$delay)

rollback_val <- rollback_values[2]
delay_val <- delay_values[1]


### for how long exceed capacity
dat$exceed_diff_1_median[is.na(dat$exceed_diff_1_median)] <- 0

pplot <- ggplot(data = subset(dat, delay == delay_val & reopen == "100perc")) +
  geom_line(aes(
    x = capacity_multiplier * 100,
    y = exceed_diff_1_median,
    group = rollback,
    col = reopen, alpha = rollback_fct
  ), size = 1.2) +
  geom_hline(data=subset(dat, delay == "counterfactual" & reopen=="100perc"), aes(yintercept = exceed_diff_1_median)) +
  facet_wrap(~region) +
  scale_color_manual(values = TwoCols_seq[2]) +
  scale_fill_manual(values = TwoCols_seq[2]) +
  scale_alpha_manual(values = c(1,0.75,0.5,0.25)) +
  scale_x_continuous(lim = c(0, 100), breaks = seq(0, 100, 20), labels = seq(0, 100, 20), expand=c(0,0)) +
  scale_y_continuous(lim = c(0, 140), breaks = seq(0, 130, 30), labels = seq(0, 130, 30), expand=c(0,0)) +
  labs(
    y = "Days above ICU capacity (median)",
    x = "Trigger threshold\n(% of ICU capacity)",
    alpha = "Mitigation\neffectiveness (%)"
  ) +
  customTheme +
  theme(
    #panel.grid.minor.y = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.ticks = element_line()
  ) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  guides(color = FALSE)

pplot

f_save_plot(
  plot_name = paste0("lineplot_duration_exceed"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_duration_plots"), width = 12, height = 4
)


testdat <- subset(dat, delay == delay_val & reopen == "100perc")
testplot <- ggplot(data = testdat) +
  geom_raster(aes(x=capacity_multiplier, y=rollback, fill=exceed_diff_1_median)) +
  facet_wrap(~region)+
  customTheme+
  scale_fill_viridis_c()+
  labs(x="Trigger threshold\n(% of ICU capacity)",
       y="Days",
       color="Duration",
       fill="Duration") +
  scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10), expand=c(0,0)) +
  scale_y_discrete( expand=c(0,0)) 
testplot

f_save_plot(
  plot_name = paste0("testplot_duration_exceed"), pplot = testplot,
  plot_dir = file.path(sim_dir, "ICU_duration_plots"), width = 12, height = 4
)

### for text
subset(dat, delay == delay_val) %>%
  filter(!is.na(exceed_diff_1_median)) %>%
  dplyr::select(region, reopen, rollback, capacity_multiplier_fct, exceed_diff_1_median) %>%
  unique() %>%
  as.data.frame() %>%
  arrange(region, reopen, rollback, capacity_multiplier_fct)

subset(dat, delay == "counterfactual") %>%
  filter(!is.na(exceed_diff_1_median)) %>%
  dplyr::select(region, reopen, rollback, capacity_multiplier_fct, exceed_diff_1_median) %>%
  unique() %>%
  as.data.frame() %>%
  arrange(region, reopen, rollback, capacity_multiplier_fct)


dat_dur <- dat

###########
dat <- f_combineDat(sim_dir, exp_names, "trigger_peak_exceed_df.csv")

subregions <- c("covidregion_1", "covidregion_4", "covidregion_11")
dat <- dat %>% filter(geography_modeled %in% subregions)
dat$scen_name <- gsub(paste0(simdate, "_IL_regreopen"), "", dat$exp_name)
dat <- dat %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat$rollback[is.na(dat$rollback)] <- "counterfactual"

dat$capacity_multiplier_fct <- round(dat$capacity_multiplier * 100, 0)
fct_labels <- sort(unique(dat$capacity_multiplier_fct))
dat$capacity_multiplier_fct[dat$rollback == "counterfactual"] <- "counterfactual"
dat$capacity_multiplier_fct <- factor(dat$capacity_multiplier_fct,
  levels = c(fct_labels, "counterfactual"),
  labels = c(fct_labels, "counterfactual")
)
table(dat$capacity_multiplier_fct, exclude = NULL)

dat <- dat %>%
  mutate(trigger_to_exceed_0.7_median = ifelse(is.na(trigger_to_exceed_0.7_median), 
                                               0, trigger_to_exceed_0.7_median)) 


pplot <- ggplot(data = unique(subset(dat, delay == delay_val))) +
  geom_line(aes(
    x = capacity_multiplier_fct,
    y = trigger_to_exceed_0.7_median, group = interaction(rollback, reopen),
    col = reopen, alpha=rollback
  ),size=1) +
  facet_wrap(reopen ~ region, scales="free") +
  scale_fill_manual(values = rev(TwoCols_seq)) +
  scale_color_manual(values = rev(TwoCols_seq)) +
  scale_alpha_manual(values =rev( c(1,0.75,0.5,0.25))) +
 # scale_x_continuous( breaks = seq(0, 1, 0.2), labels = seq(0, 100, 20), expand=c(0,0)) +
  scale_y_continuous(lim = c(0, 130), breaks = seq(0, 130, 30), labels = seq(0, 130, 30), expand=c(0,0)) +
  labs(
    title = "Time between triggered mitigation and reduced ICU census to 70% of capacity",
    subtitle = "",
    y = "Days between triggered mitigation and\nreduced ICU census to 70% of capacity",
    x = "Trigger threshold",
    caption = "Shading=mitigation strengths\nTimeframe beyond 2020, until Feb 2021"
  )+
  customTheme +
  theme(
    #panel.grid.minor.y = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.ticks = element_line()
  ) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  guides(color = FALSE)

pplot


f_save_plot(
  plot_name = paste0("lineplot_duration_trigger_to_70perc"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_duration_plots"), width = 12, height = 6
)



## =====================================
## When exceed



## =====================================

plotdat <- subset(dat, reopen == "100perc" & !(rollback %in% c("pr2", "pr4")))

pplot <- ggplot(data = plotdat) +
  geom_point(aes(x = capacity_multiplier, y = trigger_to_exceed_0.7_median, col = delay, shape = rollback)) +
  geom_line(aes(x = capacity_multiplier, y = trigger_to_exceed_0.7_median, col = delay, linetype = rollback)) +
  geom_hline(yintercept = 30, col = "red") +
  facet_grid(rollback ~ region) +
 # geom_hline(yintercept = 0)+
  scale_fill_manual(values = TwoCols_seq) +
  scale_color_manual(values = TwoCols_seq) +
  scale_y_continuous(lim = c(0, 100), breaks = seq(0, 100, 30)) +
  customTheme +
  theme(
    #panel.grid.minor.y = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.ticks = element_line()
  ) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  guides(color = FALSE)

pplot

f_save_plot(
  plot_name = paste0("lineplot_duration_trigger_to_70perc_delay"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_duration_plots"), width = 12, height = 6
)

###### time between recommended and currently used trigger
#dat_dur_t <- dat
