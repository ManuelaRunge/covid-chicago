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
simdate <- "20201212"
sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (!dir.exists(file.path(sim_dir, "ICU_duration_plots"))) dir.create(file.path(sim_dir, "ICU_duration_plots"))


exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c(grep("daysdelay", exp_names), grep("counterfactual", exp_names))]
exp_names <- exp_names[!(grepl("_reopen", exp_names))]

dat <- f_combineDat(sim_dir, exp_names, "peak_exceed_df.csv")

subregions <- c("covidregion_1", "covidregion_4", "covidregion_11")
dat$region <- factor(dat$region, levels = c(1:11), labels = paste0('Region ',c(1:11)))

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
                                   levels=c("100perc","50perc"),
                                   labels=c("High\ntransmission\nncrease","Low\ntransmission\nincrese"))

dat$reopen_fct2 <- factor(dat$reopen, 
                                    levels=c("100perc","50perc"),
                                    labels=c("High","Low"))

dat$rollback_fct <- factor(dat$rollback, 
                                     levels=c("pr8","pr6", "pr4","pr2"),
                                     labels=rev(seq(20,80,20)))

rollback_values <- unique(dat$rollback)
delay_values <- unique(dat$delay)

rollback_val <- rollback_values[2]
delay_val <- delay_values[1]


### for how long exceed capacity
pplot <- ggplot() +
  geom_bar(
    data = subset(dat, delay == delay_val & rollback == rollback_values[4]),
    aes(
      x = interaction(delay, round(capacity_multiplier * 100, 1)),
      y = exceed_diff_1_median, group = interaction(capacity_multiplier, reopen),
      fill = reopen
    ), stat = "identity", position = position_dodge(width = 1)
  ) +
  geom_bar(
    data = subset(dat, delay == delay_val & rollback == rollback_values[3]),
    aes(
      x = interaction(delay, round(capacity_multiplier * 100, 1)),
      y = exceed_diff_1_median, group = interaction(capacity_multiplier, reopen),
      fill = reopen
    ), alpha = 0.5, stat = "identity", position = position_dodge(width = 1)
  ) +
  geom_bar(
    data = subset(dat, delay == delay_val & rollback == rollback_values[2]),
    aes(
      x = interaction(delay, round(capacity_multiplier * 100, 1)),
      y = exceed_diff_1_median, group = interaction(capacity_multiplier, reopen),
      fill = reopen
    ), alpha = 0.4, stat = "identity", position = position_dodge(width = 1)
  ) +
  geom_bar(
    data = subset(dat, delay == delay_val & rollback == rollback_values[1]),
    aes(
      x = interaction(delay, round(capacity_multiplier * 100, 1)),
      y = exceed_diff_1_median, group = interaction(capacity_multiplier, reopen),
      fill = reopen
    ), alpha = 0.3, stat = "identity", position = position_dodge(width = 1)
  ) +
  geom_hline(data = subset(dat, delay == "counterfactual"), aes(yintercept = exceed_diff_1_median, col = reopen)) +
  facet_wrap(~region) +
  coord_flip() +
  scale_fill_manual(values = TwoCols_seq) +
  scale_color_manual(values = TwoCols_seq) +
  scale_y_continuous(lim = c(0, 130), breaks = seq(0, 130, 30)) +
  labs(
    title = "Days above ICU capacity",
    subtitle = "",
    y = "Days above ICU capacity",
    x = "Trigger treshold",
    caption = "Shading=mitigation strengths\nVertical line shows counterfactual (no mitigation)
       \nTimeframe beyond 2020, until Feb 2021"
  )

pplot

ggsave(paste0("barplot_duration_exceed.pdf"),
  plot = pplot,
  path = file.path(sim_dir, "ICU_duration_plots"), width = 10, height = 6, device = "pdf"
)


#### using upper values
dat$exceed_diff_1_median[is.na(dat$exceed_diff_1_median)] <- 0

pplot <- ggplot(data = subset(dat, delay == delay_val & reopen == "100perc")) +
  # geom_ribbon( aes(
  #  x = capacity_multiplier_fct,
  #  ymin = exceed_diff_1_95CI_lower, ymax = exceed_diff_1_95CI_upper,
  #  group = rollback,fill = rollback),
  #  position = position_dodge(width = 1)) +
  #geom_point(aes(
  #  x = capacity_multiplier_fct,
  #  y = exceed_diff_1_median,
  #  group = rollback, col = reopen, shape = rollback
  #),
  #position = position_dodge(width = 1)
  #) +
  #geom_label(aes(x=1.5, y=32, label="time\nlimit"), color="red",fill=NA)+
  geom_line(aes(
    x = capacity_multiplier*100,
    y = exceed_diff_1_median,
    group = rollback,
    col = reopen, linetype = rollback_fct
  ),size=1.2) +
  geom_hline(yintercept = 30, col="red") +
  facet_wrap( ~ region)+
  scale_color_manual(values=TwoCols_seq[2])+
  scale_fill_manual(values=TwoCols_seq[2])+
  scale_linetype_manual(values=c('solid','longdash','dotdash','dashed')) +
  scale_x_continuous(lim=c(0,100),breaks=seq(0,100,20),labels=seq(0,100,20))+
  scale_y_continuous(lim=c(0,120),breaks=seq(0,120,20),labels=seq(0,120,20))+
  labs(y="Days above ICU capacity (median)",
       x="Trigger threshold\n(% of ICU capacity)",
       linetype="Mitigation\neffectiveness (%)")+
  customTheme+
  theme(panel.grid.minor.y = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.ticks = element_line())+
  geom_hline(yintercept = c(-Inf, Inf)) + 
  geom_vline(xintercept = c(-Inf, Inf)) +
  guides(color = FALSE)

pplot


ggsave(paste0("lineplot_duration_exceed.pdf"),
       plot = pplot,
       path = file.path(sim_dir, "ICU_duration_plots"), width = 10, height = 6, device = "pdf"
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

plotdat <- subset(dat, capacity_multiplier == max(capacity_multiplier) & delay == delay_val & rollback == rollback_values[2])

pplot <- ggplot(plotdat) +
  geom_bar(
    data = subset(dat, delay == delay_val & rollback == rollback_values[4]),
    aes(
      x = interaction(delay, round(capacity_multiplier * 100, 1)),
      y = trigger_to_exceed_0.7_median, group = interaction(capacity_multiplier, rollback, reopen),
      fill = reopen
    ), stat = "identity", position = position_dodge(width = 1)
  ) +
  geom_bar(data = unique(subset(dat, delay == delay_val & rollback == rollback_values[3])), aes(
    x = interaction(delay, round(capacity_multiplier * 100, 1)),
    y = trigger_to_exceed_0.7_median, group = interaction(capacity_multiplier, rollback, reopen),
    fill = reopen
  ), alpha = 0.6, stat = "identity", position = position_dodge(width = 1)) +
  geom_bar(data = unique(subset(dat, delay == delay_val & rollback == rollback_values[2])), aes(
    x = interaction(delay, round(capacity_multiplier * 100, 1)),
    y = trigger_to_exceed_0.7_median, group = interaction(capacity_multiplier, rollback, reopen),
    fill = reopen
  ), alpha = 0.4, stat = "identity", position = position_dodge(width = 1)) +
  geom_bar(data = unique(subset(dat, delay == delay_val & rollback == rollback_values[1])), aes(
    x = interaction(delay, round(capacity_multiplier * 100, 1)),
    y = trigger_to_exceed_0.7_median, group = interaction(capacity_multiplier, rollback, reopen),
    fill = reopen
  ), alpha = 0.3, stat = "identity", position = position_dodge(width = 1)) +
  # geom_hline( aes(yintercept=trigger_to_exceed_1_median,col=reopen))+
  facet_wrap(~region) +
  coord_flip() +
  scale_fill_manual(values = TwoCols_seq) +
  scale_color_manual(values = TwoCols_seq) +
  scale_y_continuous(lim = c(0, 130), breaks = seq(0, 130, 30)) +
  labs(
    title = "Time between triggered mitigation and reduced ICU census to 70% of capacity",
    subtitle = "",
    y = "Days between triggered mitigation and\nreduced ICU census to 70% of capacity",
    x = "Trigger treshold",
    caption = "Shading=mitigation strengths\nTimeframe beyond 2020, until Feb 2021"
  )

pplot

ggsave(paste0("barplot_duration_trigger_to_70perc.pdf"),
  plot = pplot,
  path = file.path(sim_dir, "ICU_duration_plots"), width = 12, height = 12, device = "pdf"
)


## =====================================

plotdat <- subset(dat, reopen == "100perc" & !(rollback %in% c("pr2", "pr4")))

ggplot(data = plotdat) +
  # geom_errorbar(aes(x=capacity_multiplier, ymin=trigger_to_exceed_0.7_lower,
  #                   ymax=trigger_to_exceed_0.7_upper, col=delay, shape=rollback))+
  geom_point(aes(x = capacity_multiplier, y = trigger_to_exceed_0.7_median, col = delay, shape = rollback)) +
  geom_line(aes(x = capacity_multiplier, y = trigger_to_exceed_0.7_median, col = delay, linetype = rollback)) +
  geom_hline(yintercept = 30, col = "red") +
  facet_grid(rollback ~ region) +
  geom_line(yintercept = 0)




###### time between recommended and currently used trigger
dat_dur_t <- dat
