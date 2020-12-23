

library(tidyverse)
library(cowplot)
library(data.table)
library(raster)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")
TwoCols_seq <- c("#00a79d", "#f7941d")
theme_set(theme_minimal())
customTheme <- f_getCustomTheme()

simdate <- "20201212"
sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (!dir.exists(file.path(sim_dir, "ICU_bar_plots"))) dir.create(file.path(sim_dir, "ICU_bar_plots"))


exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c(grep("daysdelay", exp_names), grep("counterfactual", exp_names))]
exp_names <- exp_names[!(grepl("_reopen", exp_names))]
exp_names <- exp_names[!(grepl("counterfactual", exp_names))]

dat <- f_combineDat(sim_dir, exp_names, "hospitaloverflow.csv")
table(dat$exp_name)
table(dat$exp_name, dat$geography_modeled)
unique(dat$geography_modeled)

# subregions=
subregions <- c(1, 4, 11) # ,c('covidregion_1','covidregion_4','covidregion_11'))
dat <- dat %>% filter(geography_modeled %in% subregions)
dat$region <- factor(dat$geography_modeled, levels = subregions, labels = paste0("Region ", subregions))
table(dat$exp_name, dat$geography_modeled)

dat$scen_name <- gsub(paste0(simdate, "_IL_regreopen"), "", dat$exp_name)
dat <- dat %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat$rollback[is.na(dat$rollback)] <- "counterfactual"

rollback_val <- unique(dat$rollback)
delay_val <- unique(dat$delay)

table(dat$geography_modeled)
table(dat$region)
table(dat$rollback)
table(dat$reopen)
table(dat$delay)


pplot <- ggplot(data = subset(dat, delay == "7daysdelay")) +
  theme_minimal() +
  geom_vline(xintercept = c(-Inf, Inf)) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  # geom_rect(xmin = 75, xmax = Inf, ymin = -Inf, ymax =Inf, fill = "grey", alpha = 0.01) +
  geom_line(aes(x = capacity_multiplier * 100, y = prob * 100, linetype = rollback, col = reopen), size = 1.1) +
  scale_y_continuous(lim = c(0, 101), expand = c(0, 0), breaks = seq(0, 100, 20), minor_breaks = seq(0, 100, 10)) +
  scale_x_continuous(lim = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 20), minor_breaks = seq(0, 100, 10)) +
  scale_color_manual(values = c(TwoCols_seq)) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  customTheme +
  facet_wrap(~region, scales = "free") +
  theme(
    panel.spacing = unit(2, "lines"),
    legend.position = "None",
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(size = 0.75)
  ) +
  labs(
    y = "Probability of ICU overflow (%)",
    x = "Trigger threshold (% of available ICU beds)"
  )

pplot


### with shading
dat_wide <- dat %>%
  dplyr::select(capacity_multiplier, rollback, prob, region, delay, reopen) %>%
  pivot_wider(names_from = "rollback", values_from = "prob")

pplot_f <- ggplot(data = dat_wide) +
  geom_ribbon(
    data = subset(dat_wide, delay == "7daysdelay"),
    aes(
      x = capacity_multiplier * 100, ymin = pr4 * 100,
      ymax = pr2 * 100, group = reopen, fill = reopen
    ), alpha = 0.2
  ) +
  geom_ribbon(
    data = subset(dat_wide, delay == "7daysdelay"),
    aes(
      x = capacity_multiplier * 100, ymin = pr6 * 100,
      ymax = pr4 * 100, group = reopen, fill = reopen
    ), alpha = 0.6
  ) +
  geom_ribbon(
    data = subset(dat_wide, delay == "7daysdelay"),
    aes(
      x = capacity_multiplier * 100, ymin = pr8 * 100,
      ymax = pr6 * 100, group = reopen, fill = reopen
    ), alpha = 1
  ) +
  geom_line(
    data = subset(dat, delay == "7daysdelay"),
    aes(
      x = capacity_multiplier * 100, y = prob * 100,
      linetype = rollback, col = reopen
    ), size = 1.1
  ) +
  customTheme +
  scale_linetype_manual(values = rev(c("solid", "longdash", "dotdash", "dashed"))) +
  scale_y_continuous(lim = c(0, 101), expand = c(0, 0), breaks = seq(0, 100, 20), minor_breaks = seq(0, 100, 10)) +
  scale_x_continuous(lim = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 20), minor_breaks = seq(0, 100, 10)) +
  scale_color_manual(values = c(TwoCols_seq)) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  customTheme +
  facet_wrap(~region, scales = "free") +
  theme(
    panel.spacing = unit(2, "lines"),
    legend.position = "None",
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(size = 0.75)
  ) +
  labs(
    y = "Probability of ICU overflow (%)",
    x = "Trigger threshold (% of available ICU beds)"
  )


pplot_f


# rollback=="sm4"
dat_wide <- dat %>%
  filter(rollback == "pr8") %>%
  dplyr::select(capacity_multiplier, rollback, prob, region, delay, reopen) %>%
  pivot_wider(names_from = "delay", values_from = "prob")

pplot2 <- ggplot(data = subset(dat, rollback == "pr8")) +
  theme_minimal() +
  geom_vline(xintercept = c(-Inf, Inf)) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  # geom_rect(xmin = 75, xmax = Inf, ymin = -Inf, ymax =Inf, fill = "grey", alpha = 0.01) +
  geom_line(aes(
    x = capacity_multiplier * 100, y = prob * 100, linetype = delay,
    col = reopen, group = interaction(delay, reopen)
  ), size = 1.1) +
  scale_y_continuous(lim = c(0, 101), expand = c(0, 0), breaks = seq(0, 100, 20), minor_breaks = seq(0, 100, 10)) +
  scale_x_continuous(lim = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 20), minor_breaks = seq(0, 100, 10)) +
  scale_color_manual(values = c("orange2", "#35978f")) +
  scale_fill_manual(values = c("orange2", "#35978f")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_linetype_manual(values = rev(c("solid", "longdash", "dotdash", "dashed"))) +
  customTheme +
  facet_wrap(~region, scales = "free") +
  theme(
    panel.spacing = unit(2, "lines"),
    legend.position = "None",
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(size = 0.75)
  ) +
  labs(
    y = "Probability of ICU overflow (%)",
    x = "Trigger threshold (% of available ICU beds)"
  )

pplot2_f <- pplot2 + geom_ribbon(
  data = dat_wide,
  aes(
    x = capacity_multiplier * 100,
    ymin = `7daysdelay` * 100,
    ymax = `1daysdelay` * 100,
    group = reopen, fill = reopen
  ), alpha = 0.4
)



pplotall <- plot_grid(pplot, pplot2, ncol = 1, labels = c("A", "B"))
pplotall_f <- plot_grid(pplot_f, pplot2_f, ncol = 1, labels = c("A", "B"))


f_save_plot(
  plot_name = paste0("ICU_prob_pplotall"), pplot = pplotall,
  plot_dir = file.path(sim_dir, "ICU_prob_plots"), width = 12, height = 8
)

f_save_plot(
  plot_name = paste0("ICU_prob_pplotall_filled"), pplot = pplotall_f,
  plot_dir = file.path(sim_dir, "ICU_prob_plots"), width = 12, height = 8
)



#### For text
dat_prob <- dat

dat %>%
  filter(delay == "7daysdelay") %>%
  filter(capacity_multiplier == 0.8) %>%
  dplyr::select(region, capacity_multiplier, rollback, reopen, delay, prob) %>%
  pivot_wider(names_from = "rollback", values_from = "prob")

dat %>%
  filter(delay == "7daysdelay") %>%
  filter(prob >= 0.45 & prob <= 0.55) %>%
  dplyr::select(region, capacity_multiplier, rollback, reopen, delay, prob) %>%
  pivot_wider(names_from = "rollback", values_from = "prob") %>%
  arrange(region, reopen, capacity_multiplier)

dat %>%
  filter(delay == "7daysdelay") %>%
  dplyr::group_by(region, rollback, reopen, delay) %>%
  summarize(prob = mean(prob)) %>%
  pivot_wider(names_from = "rollback", values_from = "prob")


dat %>%
  filter(delay == "7daysdelay") %>%
  dplyr::group_by(rollback, reopen, delay) %>%
  summarize(prob = mean(prob)) %>%
  pivot_wider(names_from = "rollback", values_from = "prob")


dat %>%
  filter(rollback == "pr8") %>%
  dplyr::group_by(region, rollback, reopen, delay) %>%
  summarize(prob = mean(prob)) %>%
  pivot_wider(names_from = "delay", values_from = "prob")

dat %>%
  filter(rollback == "pr8") %>%
  dplyr::group_by(rollback, reopen, delay) %>%
  summarize(prob = mean(prob)) %>%
  pivot_wider(names_from = "delay", values_from = "prob")

dat %>%
  filter(rollback == "pr8") %>%
  dplyr::group_by(rollback, capacity_multiplier, reopen, delay) %>%
  summarize(prob = mean(prob)) %>%
  pivot_wider(names_from = "delay", values_from = "prob") %>%
  mutate(diff = round(`1daysdelay` - `7daysdelay`, 3) * 100) %>%
  arrange(reopen, capacity_multiplier) %>%
  as.data.frame()


dat %>%
  filter(delay == "7daysdelay") %>%
  filter(prob >= 0.15 & prob <= 0.25) %>%
  dplyr::select(region, capacity_multiplier, rollback, reopen, delay, prob) %>%
  pivot_wider(names_from = "rollback", values_from = "prob") %>%
  arrange(reopen, region, capacity_multiplier)
