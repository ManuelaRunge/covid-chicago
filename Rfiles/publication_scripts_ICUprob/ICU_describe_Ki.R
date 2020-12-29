### Describe Rt


library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

theme_set(theme_minimal())
simcolor <- "#F6921E"
capacitycolor <- "dodgerblue2"
customTheme <- f_getCustomTheme()
custom_date_breaks <- c(
  as.Date("2020-08-01"), as.Date("2020-09-01"),
  as.Date("2020-10-01"), as.Date("2020-11-01"),
  as.Date("2020-12-01"), as.Date("2021-01-01")
)

custom_date_breaks_JanOct <- c(
  as.Date("2020-01-01"), as.Date("2020-02-01"), as.Date("2020-03-01"),
  as.Date("2020-04-01"), as.Date("2020-05-01"), as.Date("2020-06-01"),
  as.Date("2020-07-01"), as.Date("2020-08-01"), as.Date("2020-09-01"),
  as.Date("2020-10-01")
)

simdate <- "20201212"
sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (!dir.exists(file.path(sim_dir, "ICU_Ki_plots"))) dir.create(file.path(sim_dir, "ICU_Ki_plots"))


exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c(grep("daysdelay", exp_names), grep("counterfactual", exp_names))]
exp_names <- exp_names[!(grepl("_reopen", exp_names))]

dat <- f_combineDat(sim_dir, exp_names, "Ki_dat_All.csv") %>%
  f_get_scenVars()
dat$date <- as.Date(dat$date)
summary(dat$time)

dat <- dat %>% filter(region %in% c(1, 4, 11))
dat$region <- factor(dat$region, levels = c(1, 4, 11), labels = paste0("Region ", c(1, 4, 11)))
table(dat$capacity_multiplier_fct, exclude = NULL)


### ------------------------------
## Counterfactual
### ------------------------------
dat_counterfactual <- dat %>% filter(rollback == "counterfactual")

pplot <- ggplot(data = subset(dat_counterfactual, date <= as.Date("2020-10-01"))) +
  geom_line(aes(x = date, y = Ki_t, group = interaction(sample_num, reopen), col = reopen), size = 1) +
  facet_wrap(~region, ncol = 1, scales = "free") +
  geom_vline(xintercept = as.Date("2020-09-01")) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.1), labels = seq(0, 1.4, 0.1), lim = c(0, 1.2)) +
  scale_x_date(breaks = custom_date_breaks_JanOct, date_labels = "%b") +
  scale_fill_manual(values = TwoCols_seq) +
  scale_color_manual(values = TwoCols_seq) +
  labs(
    x = "", y = "Transmission rate\n",
    color = "Transmission\nincrease",
    fill = "Transmission\nincrease",
    linetype = "Mitigation strengths (%)"
  ) +
  customTheme +
  theme(
    panel.spacing = unit(1, "lines"),
    panel.grid.minor = element_blank()
  ) +
  guides(color = FALSE, fill = FALSE, linetype = guide_legend(reverse = TRUE))


f_save_plot(
  plot_name = paste0("Ki_timeline_Jan-Oct"),
  pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_Ki_plots"), width = 8, height = 10
)


dat_counterfactual %>%
  filter(date == min(date)) %>%
  select(region, reopen, Ki_t)

dat_counterfactual %>%
  filter(date %in% c(min(date), as.Date("2020-04-01"), as.Date("2020-09-01"), as.Date("2020-10-01"))) %>%
  select(date, region, reopen, Ki_t) %>%
  mutate(date_label = ifelse(date == as.Date("2020-09-01"), "current", "incr")) %>%
  mutate(date_label = ifelse(date == min(date), "initial", date_label)) %>%
  mutate(date_label = ifelse(date == as.Date("2020-04-01"), "lowest", date_label)) %>%
  select(-date) %>%
  pivot_wider(names_from = "date_label", values_from = "Ki_t") %>%
  mutate(
    Ki_incr_from_initial = ((incr / initial)) * 100,
    Ki_incr_from_lowest = (1 - (incr / lowest)) * 100,
    Ki_incr_from_current = (1 - (incr / current)) * 100
  )

dat_counterfactual %>%
  filter(reopen == "100perc", date %in% c(min(date), as.Date("2020-04-01"))) %>%
  select(date, region, reopen, Ki_t) %>%
  mutate(date_label = ifelse(date == min(date), "initial", "red")) %>%
  select(-date) %>%
  pivot_wider(names_from = "date_label", values_from = "Ki_t") %>%
  mutate(Ki_red = (1 - (red / initial)) * 100)


### ------------------------------
## Scenarios
### ------------------------------
dat_scenarios <- dat %>% filter(rollback != "counterfactual" & delay == "1daysdelay")
ggplot(data = subset(dat_scenarios, rollback == "pr6" &
  date >= as.Date("2020-07-01") &
  date <= as.Date("2020-12-31"))) +
  geom_line(aes(x = date, y = Ki_t, group = interaction(sample_num, reopen), col = reopen_fct2)) +
  facet_grid(reopen_fct ~ region, scales = "free") +
  scale_fill_manual(values = TwoCols_seq) +
  scale_color_manual(values = TwoCols_seq) +
  scale_x_date(breaks = custom_date_breaks, date_labels = "%b", expand = c(0, 0)) +
  labs(
    x = "", y = "Transmission rate",
    color = "Transmission\nincrease",
    fill = "Transmission\nincrease",
    linetype = "Mitigation strengths (%)"
  ) +
  customTheme +
  theme(panel.spacing = unit(1, "lines")) +
  guides(color = FALSE, fill = FALSE, linetype = guide_legend(reverse = TRUE))

ggplot(data = subset(
  dat_scenarios,
  date >= as.Date("2020-07-01") &
    date <= as.Date("2020-12-31")
)) +
  geom_line(aes(x = date, y = Ki_t, group = interaction(sample_num, rollback, reopen), col = rollback)) +
  facet_grid(reopen ~ region, scales = "free") +
  scale_x_date(date_breaks = "30 days", date_labels = "%b") +
  labs(
    x = "", y = "Transmission rate",
    color = "Transmission\nincrease",
    fill = "Transmission\nincrease",
    linetype = "Mitigation strengths (%)"
  ) +
  customTheme +
  theme(panel.spacing = unit(1, "lines")) +
  guides(color = FALSE, fill = FALSE, linetype = guide_legend(reverse = TRUE))


dat_scenarios %>%
  group_by(region, exp_name) %>%
  arrange(date, Ki_t) %>%
  tail()

uniqueKis <- dat_scenarios %>%
  select(region, exp_name, reopen, rollback, delay, Ki_t) %>%
  unique() %>%
  group_by(region, reopen, rollback, delay) %>%
  summarize(min_Ki = min(Ki_t), max_Ki = max(Ki_t)) %>%
  mutate(Ki_red = (1 - (min_Ki / max_Ki)) * 100)
