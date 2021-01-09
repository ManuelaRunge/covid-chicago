## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## ICU overflow manuscript
## Figure 6
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

custom_date_breaks <- c(
  as.Date("2020-08-01"), as.Date("2020-09-01"),
  as.Date("2020-10-01"), as.Date("2020-11-01"),
  as.Date("2020-12-01"), as.Date("2021-01-01"),
  as.Date("2021-02-01"), as.Date("2021-03-01")
)

custom_date_breaks_JanOct <- c(
  as.Date("2020-01-01"), as.Date("2020-02-01"), as.Date("2020-03-01"),
  as.Date("2020-04-01"), as.Date("2020-05-01"), as.Date("2020-06-01"),
  as.Date("2020-07-01"), as.Date("2020-08-01"), as.Date("2020-09-01"),
  as.Date("2020-10-01")
)


## -------------------------------
## Load data
## -------------------------------
capacityDat <- load_new_capacity(filedate = "20200915")
capacityDat <- capacityDat %>%
  filter(geography_name %in% c(1, 4, 11)) %>%
  rename(avg_resource_available = icu_available)
capacityDat$region <- factor(capacityDat$geography_name, levels = c(1, 4, 11), labels = paste0("Region ", c(1, 4, 11)))

dat <- fread(file.path(sim_dir, "csvs", "dat_1daysdelay.csv"))
dat_subsample_100 <- fread(file.path(sim_dir, "csvs", "dat_subsample_100_1daysdelay.csv"))
dat_subsample_50 <- fread(file.path(sim_dir, "csvs", "dat_subsample_50_1daysdelay.csv"))

### ---------------------------------------------
### Visualize peak dates
### ---------------------------------------------
pplot <- ggplot(data = subset(dat, region == "Region 11")) +
  geom_point(aes(x = peak_date, y = crit_det, group = scen_num, fill = capacity_multiplier_fct), alpha = 0.9, shape = 21) +
  geom_vline(xintercept = as.Date("2020-12-31")) +
  geom_hline(aes(yintercept = avg_resource_available), col = capacity_col, linetype = "dashed") +
  scale_color_viridis_d(option = "A", direction = -1) +
  scale_fill_viridis_d(option = "A", direction = -1) +
  scale_x_date(limits = c(as.Date("2020-10-01"), as.Date("2021-02-01")), breaks = custom_date_breaks, date_labels = "%b") +
  facet_wrap(reopen ~ rollback_fct, nrow = 2, scales = "free") +
  labs(x = "Peak date", y = "ICU census", color = "Trigger threshold", fill = "Trigger threshold") +
  customTheme

pplot

f_save_plot(
  plot_name = paste0("peak_dates_all_reg11_all"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_date_plots"), width = 16, height = 7
)
rm(pplot)

pplot <- ggplot(data = subset(dat_subsample_100, region == "Region 11")) +
  geom_point(aes(x = peak_date, y = crit_det, group = scen_num, fill = capacity_multiplier_fct), alpha = 0.9, shape = 21) +
  geom_vline(xintercept = as.Date("2020-12-31")) +
  geom_hline(aes(yintercept = avg_resource_available), col = capacity_col, linetype = "dashed") +
  scale_color_viridis_d(option = "A", direction = -1) +
  scale_fill_viridis_d(option = "A", direction = -1) +
  scale_x_date(limits = c(as.Date("2020-10-01"), as.Date("2021-02-01")), breaks = custom_date_breaks, date_labels = "%b") +
  facet_wrap(reopen ~ rollback_fct, nrow = 2, scales = "free") +
  labs(x = "Peak date", y = "ICU census", color = "Trigger threshold", fill = "Trigger threshold") +
  customTheme

pplot

f_save_plot(
  plot_name = paste0("peak_dates_all_reg11_sub100"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_date_plots"), width = 16, height = 7
)
rm(pplot)

pplot <- ggplot(data = subset(dat_subsample_50, region == "Region 11")) +
  geom_point(aes(x = peak_date, y = crit_det, group = scen_num, fill = capacity_multiplier_fct), alpha = 0.9, shape = 21) +
  geom_vline(xintercept = as.Date("2020-12-31")) +
  geom_hline(aes(yintercept = avg_resource_available), col = capacity_col, linetype = "dashed") +
  scale_color_viridis_d(option = "A", direction = -1) +
  scale_fill_viridis_d(option = "A", direction = -1) +
  scale_x_date(limits = c(as.Date("2020-10-01"), as.Date("2021-02-01")), breaks = custom_date_breaks, date_labels = "%b") +
  facet_wrap(reopen ~ rollback_fct, nrow = 2, scales = "free") +
  labs(x = "Peak date", y = "ICU census", color = "Trigger threshold", fill = "Trigger threshold") +
  customTheme

pplot

f_save_plot(
  plot_name = paste0("peak_dates_all_reg11_sub50"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_date_plots"), width = 16, height = 7
)
rm(pplot)

### ---------------------------------------------
### Aggregate for barplot
### ---------------------------------------------
datAggr <- dat_subsample_50 %>%
  group_by(exp_name, region, capacity_multiplier, reopen, avg_resource_available, delay, rollback) %>%
  summarize(
    min.value = min(crit_det, na.rm = TRUE),
    max.value = max(crit_det, na.rm = TRUE),
    median.value = median(crit_det, na.rm = TRUE),
    q25.value = quantile(crit_det, probs = 0.25, na.rm = TRUE),
    q75.value = quantile(crit_det, probs = 0.75, na.rm = TRUE),
    q2.5.value = quantile(crit_det, probs = 0.025, na.rm = TRUE),
    q97.5.value = quantile(crit_det, probs = 0.975, na.rm = TRUE)
  ) %>%
  f_get_scenVars() %>%
  ungroup()


### ---------------------------------------------
### Add counterfactual for all rollback scenarios
### ---------------------------------------------
datAggr2 <- datAggr %>% filter(delay == delay_val)
datAggr1 <- datAggr %>%
  filter(delay == "counterfactual") %>%
  dplyr::select(-c(rollback, delay, exp_name, rollback_fct, reopen_fct, reopen_fct2))
datAggr0 <- datAggr %>%
  filter(delay != "counterfactual") %>%
  dplyr::select(rollback, delay, exp_name, rollback_fct, reopen_fct, reopen_fct2) %>%
  unique()
datAggr10 <- datAggr1 %>%
  f_addVar(datAggr0) %>%
  dplyr::select(colnames(datAggr2))
datAggr_2 <- rbind(datAggr10, datAggr2)
annotationDat <- unique(datAggr[, c("region", "avg_resource_available")])
rm(datAggr0, datAggr1, datAggr2, datAggr10)

### ---------------------------------------------
### Plots
### ---------------------------------------------
pplot <- ggplot(data = subset(datAggr_2)) +
  # geom_ribbon(aes(x=capacity_multiplier_fct, y = median.value, ymin= q2.5.value, ymax = q97.5.value,
  #                fill=reopen, group=interaction(reopen, rollback)), alpha=0.3) +
  geom_ribbon(aes(
    x = capacity_multiplier_fct, y = median.value, ymin = q25.value, ymax = q75.value,
    fill = reopen, group = interaction(reopen, rollback)
  ), alpha = 0.3) +
  geom_line(aes(
    x = capacity_multiplier_fct,
    y = median.value, col = reopen, alpha = rollback, group = rollback
  ), size = 1) +
  geom_point(aes(
    x = capacity_multiplier_fct,
    y = median.value, fill = reopen
  ), shape = 21) +
  geom_pointrange(
    data = subset(datAggr_2, capacity_multiplier_fct == "counterfactual"),
    aes(
      x = capacity_multiplier_fct,
      y = median.value,
      ymin = q25.value, ymax = q75.value,
      fill = reopen
    ), shape = 21
  ) +
  geom_hline(aes(yintercept = avg_resource_available), col = capacity_col, linetype = "dashed") +
  facet_wrap(reopen ~ region, scales = "free") +
  scale_color_manual(values = TwoCols_seq) +
  scale_fill_manual(values = TwoCols_seq) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_alpha_manual(values = c(1, 0.75, 0.5, 0.3, 0.1)) +
  customTheme +
  labs(x = "Peak date", y = "ICU census", color = "Trigger threshold", fill = "Trigger threshold", alpha = "Mitigation\nstrengths")

pplot

f_save_plot(
  plot_name = paste0("mitigation_reduction_lines_sub50_v1"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_reduction_plots"), width = 12, height = 8
)


pplot <- ggplot(data = subset(datAggr, rollback != "counterfactual")) +
  geom_line(aes(x = capacity_multiplier_fct, y = median.value, col = reopen, alpha = rollback, group = rollback), size = 1) +
  geom_hline(data = datAggr1, aes(yintercept = median.value)) +
  geom_hline(aes(yintercept = avg_resource_available), col = capacity_col, linetype = "dashed") +
  facet_wrap(reopen ~ region, scales = "free") +
  scale_color_manual(values = TwoCols_seq) +
  scale_fill_manual(values = TwoCols_seq) +
  scale_alpha_manual(values = c(1, 0.75, 0.5, 0.3, 0.1)) +
  customTheme +
  labs(x = "Peak date", y = "ICU census", color = "Trigger threshold", fill = "Trigger threshold", alpha = "Mitigation\nstrengths")

pplot

f_save_plot(
  plot_name = paste0("mitigation_reduction_lines_sub50_v2"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_reduction_plots"), width = 12, height = 8
)
