
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
## Run script
## -------------------------------
simdate <- "20201212"
sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (!dir.exists(file.path(sim_dir, "ICU_bar_plots"))) dir.create(file.path(sim_dir, "ICU_bar_plots"))
if (!dir.exists(file.path(sim_dir, "ICU_reduction_plots"))) dir.create(file.path(sim_dir, "ICU_reduction_plots"))


exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c(grep("daysdelay", exp_names), grep("counterfactual", exp_names))]
exp_names <- exp_names[!(grepl("_reopen", exp_names))]

dat <- f_combineDat(sim_dir, exp_names, "peak_exceed_df.csv") %>%
  filter(ems %in% c("EMS-1", "EMS-4", "EMS-11"))

dat$region <- factor(dat$ems, levels = c(paste0("EMS-", c(1:11))), labels = paste0("Region ", c(1:11)))
dat$scen_name <- gsub(paste0(simdate, "_IL_regreopen"), "", dat$exp_name)
dat <- dat %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
table(dat$rollback, dat$exp_name)
dat$rollback[is.na(dat$rollback)] <- "counterfactual"

rollback_values <- unique(dat$rollback)
delay_values <- unique(dat$delay)

rollback_val <- rollback_values[2]
delay_val <- delay_values[1]

dat$capacity_multiplier_fct <- round(dat$capacity_multiplier * 100, 0)
fct_labels <- sort(unique(dat$capacity_multiplier_fct))
dat$capacity_multiplier_fct[dat$rollback == "counterfactual"] <- "counterfactual"
dat$capacity_multiplier_fct <- factor(dat$capacity_multiplier_fct,
  levels = c(fct_labels, "counterfactual"),
  labels = c(fct_labels, "counterfactual")
)
dat$capacity_multiplier_fct2 <- factor(dat$capacity_multiplier_fct,
  levels = c(fct_labels, "counterfactual"),
  labels = c(fct_labels, "counter\nfactual")
)
table(dat$capacity_multiplier_fct, exclude = NULL)
dat$capacity_multiplier_fct

dat$perc_ICU_occup_50CI_lower <- (dat$critical_50CI_lower / dat$avg_resource_available) * 100
dat$perc_ICU_occup_50CI_upper <- (dat$critical_50CI_upper / dat$avg_resource_available) * 100
dat$perc_ICU_occup_95CI_lower <- (dat$critical_95CI_lower / dat$avg_resource_available) * 100
dat$perc_ICU_occup_95CI_upper <- (dat$critical_95CI_upper / dat$avg_resource_available) * 100
dat$perc_ICU_occup_median <- (dat$critical_median / dat$avg_resource_available) * 100


plotdat <- dat %>% filter((delay == "counterfactual" | delay == delay_val & rollback == rollback_val))
annotationDat <- unique(plotdat[, c("region", "avg_resource_available")])

pplot <- ggplot(data = plotdat) +
  geom_bar(aes(
    x = capacity_multiplier_fct2,
    y = critical_median, group = reopen, fill = reopen
  ),
  stat = "identity", position = position_dodge(width = 0.91), width = 0.7, alpha = 0.4
  ) +
  geom_bar(
    data = subset(dat, (delay == "counterfactual" |
      delay == delay_val & rollback == rollback_values[4])),
    aes(
      x = capacity_multiplier_fct2,
      y = critical_median,
      group = reopen, fill = reopen
    ),
    stat = "identity", position = position_dodge(width = 0.91), width = 0.7
  ) +
  geom_errorbar(aes(
    x = capacity_multiplier_fct2,
    ymin = critical_95CI_lower,
    ymax = critical_95CI_upper, group = reopen
  ),
  position = position_dodge(width = 0.91), width = 0
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
  geom_text(data = annotationDat, aes(
    x = 1.5, y = avg_resource_available + (avg_resource_available * 0.5),
    label = "ICU capacity"
  ), col = capacity_col) +
  facet_wrap(~region, scales = "free", ncol = 1, strip.position = "right") +
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
  plot_name = paste0("barplot_pall"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_bar_plots"), width = 6, height = 8
)


## ====================
dat_peak <- dat
## ====================

###### Text to describe figures
dat_peak %>%
  filter(delay == "counterfactual") %>%
  group_by(region, reopen, exp_name) %>%
  mutate(ratio = critical_median / avg_resource_available) %>%
  as.data.frame() %>%
  group_by(reopen, rollback) %>%
  summarize(ratio = mean(ratio))

counterfactualDat <- dat_peak %>%
  filter(delay == "counterfactual") %>%
  group_by(region, reopen, exp_name, rollback) %>%
  mutate(
    ratio = critical_median / avg_resource_available,
    diff = avg_resource_available - critical_median,
    red = (1 - (avg_resource_available / critical_median)) * 100
  ) %>%
  as.data.frame() %>%
  group_by(region, reopen) %>%
  rename(
    counter_ratio = ratio,
    counter_diff = diff,
    required_reduction = red,
    counter_critical_95CI_lower = critical_95CI_lower,
    counter_critical_95CI_upper = critical_95CI_upper,
    counter_critical_median = critical_median
  ) %>%
  dplyr::select(
    region, reopen, counter_ratio, counter_diff, required_reduction,
    counter_critical_95CI_lower, counter_critical_95CI_upper, counter_critical_median
  )
counterfactualDat

## -----------------------------------------------
### reduction required per region
## -----------------------------------------------
mitigationDat <- dat_peak %>%
  filter(delay == delay_val) %>%
  group_by(region, reopen, exp_name, rollback, capacity_multiplier) %>%
  mutate(
    ratio = critical_median / avg_resource_available,
    diff = avg_resource_available - critical_median
  ) %>%
  as.data.frame() %>%
  group_by(region, reopen, rollback, capacity_multiplier) %>%
  rename(
    mitigation_ratio = ratio,
    mitigation_diff = diff,
    mitigation_critical_median = critical_median,
    mitigation_critical_95CI_lower = critical_95CI_lower,
    mitigation_critical_95CI_upper = critical_95CI_upper
  ) %>%
  dplyr::select(
    region, reopen, rollback, capacity_multiplier,
    mitigation_ratio, mitigation_diff, mitigation_critical_95CI_lower, mitigation_critical_95CI_upper, mitigation_critical_median
  ) %>%
  f_addVar(counterfactualDat) %>%
  group_by(region, reopen, rollback, capacity_multiplier) %>%
  mutate(
    red_median = (1 - (mitigation_critical_median / counter_critical_median)) * 100,
    red_lower = (1 - (mitigation_critical_95CI_lower / counter_critical_95CI_lower)) * 100,
    red_upper = (1 - (mitigation_critical_95CI_upper / counter_critical_95CI_upper)) * 100
  )

mitigationDat %>%
  filter(rollback %in% c("pr4", "pr8")) %>%
  arrange(region, rollback, reopen)

mitigationDat %>%
  filter(rollback %in% c("pr4", "pr8")) %>%
  arrange(rollback, reopen, region)

mitigationDat %>%
  filter(capacity_multiplier == 1) %>%
  arrange(rollback, reopen, region, capacity_multiplier) %>%
  group_by(rollback, reopen, capacity_multiplier) %>%
  summarize(red_median = mean(red_median))


mitigationDat$reopen_fct <- factor(mitigationDat$reopen,
  levels = c("100perc", "50perc"),
  labels = c("High\ntransmission\nncrease", "Low\ntransmission\nincrese")
)

mitigationDat$reopen_fct2 <- factor(mitigationDat$reopen,
  levels = c("100perc", "50perc"),
  labels = c("High", "Low")
)

mitigationDat$rollback_fct <- factor(mitigationDat$rollback,
  levels = c("pr8", "pr6", "pr4", "pr2"),
  labels = rev(seq(20, 80, 20))
)


pplot <- ggplot(data = mitigationDat) +
  geom_rect(xmin = 80, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.01) +
  # geom_ribbon(aes(x=capacity_multiplier, ymin=red_lower, ymax=red_upper,  group=interaction(rollback,reopen), fill=as.factor(reopen)), alpha=0.4) +
  geom_line(aes(x = capacity_multiplier * 100, y = red_median, linetype = rollback_fct, col = reopen_fct2), size = 1) +
  facet_grid(reopen_fct ~ region) +
  geom_text(
    data = unique(mitigationDat[, c("region", "reopen_fct", "required_reduction")]),
    aes(x = 11, y = required_reduction, label = "required\nreduction"), col = "red"
  ) +
  geom_hline(aes(yintercept = required_reduction), linetype = "solid", col = "red") +
  scale_color_manual(values = TwoCols_seq) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash", "dashed")) +
  scale_y_continuous(lim = c(0, 100)) +
  theme(axis.ticks = element_line()) +
  scale_x_continuous(breaks = seq(0, 100, 20), labels = seq(0, 100, 20)) +
  customTheme +
  labs(
    x = "Trigger threshold\n(% of ICU capacity)",
    y = "Relative reduction\nin predicted ICU peak (%)",
    linetype = "Mitigation\neffectiveness (%)", col = "Transmission\nincrease"
  )

pplot

f_save_plot(
  plot_name = paste0("mitigation_effectiveness"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_reduction_plots"), width = 12, height = 8
)



### Relative reduction
# p1dat
colnames(p2dat)[c(4:8)] <- paste0("counter_", colnames(p2dat)[c(4:8)])

p1dat %>%
  left_join(p2dat[, c(1, 4:8, 14)], by = c("ems", "reopen")) %>%
  group_by(ems, exp_name, capacity_multiplier) %>%
  mutate(perc_red_median = (1 - (critical_median / counter_critical_median)) * 100) %>%
  dplyr::select(ems, exp_name, delay, reopen, capacity_multiplier, critical_median, counter_critical_median, perc_red_median) %>%
  as.data.frame() %>%
  arrange(capacity_multiplier, ems, reopen)

# palldat <- data.table(palldat, key =c( "geography_name","capacity_multiplier", "date",'reopen' ))
# palldat[,ICU_diff_median := crit_det_median[rollback=="sm4"] - crit_det_median[rollback=="counterfactual"], by = c( "geography_name","capacity_multiplier", "date",'reopen' ) ]
