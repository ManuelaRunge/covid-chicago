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
if (!dir.exists(file.path(sim_dir, "ICU_date_plots"))) dir.create(file.path(sim_dir, "ICU_bar_plots"))

## -------------------------------
## Load data
## -------------------------------
capacityDat <- load_new_capacity(filedate = "20200915")
capacityDat <- capacityDat %>%
  filter(geography_name %in% c(1, 4, 11)) %>%
  rename(avg_resource_available = icu_available)
capacityDat$region <- factor(capacityDat$geography_name, levels = c(1, 4, 11), labels = paste0("Region ", c(1, 4, 11)))

dat <- fread(file.path(sim_dir, "csvs", "dat_combined.csv")) %>%
          f_get_scenVars()
#dat_subsample_100 <- fread(file.path(sim_dir, "csvs", "dat_subsample_100_1daysdelay.csv"))
dat$date <- as.Date(dat$date)
dat$peak_date <- as.Date(dat$peak_date)
dat$date_of_trigger <- dat$time_of_trigger + as.Date("2020-01-01")

### ---------------------------------------------
### Visualize peak dates
### ---------------------------------------------
for(reg in c("Region 1","Region 4","Region 11")){
  
pplot <- ggplot(data = subset(dat, region == reg)) +
  geom_point(aes(x = peak_date, y = crit_det, group = scen_num, fill = as.factor(date_of_trigger)), alpha = 0.9, shape = 21) +
  geom_vline(xintercept = as.Date("2020-12-31")) +
  geom_hline(aes(yintercept = avg_resource_available), col = capacity_col, linetype = "dashed") +
  scale_color_viridis_d(option = "A", direction = -1) +
  scale_fill_viridis_d(option = "A", direction = -1) +
  scale_x_date(limits = c(as.Date("2020-10-01"), as.Date("2021-02-01")), breaks = custom_date_breaks, date_labels = "%d\n%b") +
  facet_wrap(reopen ~ rollback_fct, nrow = 2, scales = "free") +
  labs(x = "Peak date", y = "ICU census", color = "Trigger date", fill = "Trigger date") +
  customTheme

pplot

f_save_plot(
  plot_name = paste0("peak_dates_all_",reg,"_all"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_date_plots"), width = 16, height = 7
)
rm(pplot)
}


### ---------------------------------------------
### Aggregate for barplot
### ---------------------------------------------

every2ndweek <- sort(unique(dat$time_of_trigger))[seq(1,length(unique(dat$time_of_trigger)),2)]

datAggr <- dat %>%
  group_by(exp_name, region, date, time_of_trigger,date_of_trigger,avg_resource_available) %>%
  #filter(time_of_trigger %in% every2ndweek) %>%
  summarize(min.value = min(crit_det, na.rm = TRUE),
            max.value = max(crit_det, na.rm = TRUE),
            median.value = median(crit_det, na.rm = TRUE),
            mean.value = mean(crit_det, na.rm = TRUE),
            q25.value = quantile(crit_det, probs = 0.25, na.rm = TRUE),
            q75.value = quantile(crit_det, probs = 0.75, na.rm = TRUE),
            q2.5.value = quantile(crit_det, probs = 0.025, na.rm = TRUE),
            q97.5.value = quantile(crit_det, probs = 0.975, na.rm = TRUE)) %>%
  f_get_scenVars()


datAggr_hline <- datAggr %>% 
  group_by(region, exp_name,time_of_trigger,date_of_trigger,avg_resource_available) %>% 
  filter(date >= as.Date("2020-10-01")) %>%
  filter(as.character(date)==as.character(date_of_trigger)) %>%
  mutate(trigger_threshold = mean.value/avg_resource_available,
         trigger_threshold_median = median.value/avg_resource_available ) %>%
  dplyr::select(region,date, exp_name,time_of_trigger, date_of_trigger,avg_resource_available,trigger_threshold, trigger_threshold_median) 


datAggr_hline$trigger_label <- paste0( datAggr_hline$date_of_trigger , "\n (mean= ", round(datAggr_hline$trigger_threshold*100,0),  " %)" )
datAggr_hline$trigger_label_median <- paste0( datAggr_hline$date_of_trigger , "\n (median= ", round(datAggr_hline$trigger_threshold_median*100,0),  " %)" )
datAggr <- f_addVar(datAggr, datAggr_hline[,c("trigger_threshold","trigger_threshold_median","time_of_trigger","trigger_label",'trigger_label_median')])


#trigger_threshold, trigger_threshold_median, trigger_label,trigger_label_median

datAggr_peak <- dat %>% 
  group_by(exp_name, region, time_of_trigger,date_of_trigger,avg_resource_available) %>%
  filter(date >= as.Date("2020-10-01")) %>%
  filter(crit_det==max(crit_det)) %>%
  summarize(min.value = min(crit_det, na.rm = TRUE),
            max.value = max(crit_det, na.rm = TRUE),
            median.value = median(crit_det, na.rm = TRUE),
            mean.value = mean(crit_det, na.rm = TRUE),
            q25.value = quantile(crit_det, probs = 0.25, na.rm = TRUE),
            q75.value = quantile(crit_det, probs = 0.75, na.rm = TRUE),
            q2.5.value = quantile(crit_det, probs = 0.025, na.rm = TRUE),
            q97.5.value = quantile(crit_det, probs = 0.975, na.rm = TRUE)) %>%
  f_get_scenVars()

### ---------------------------------------------
### Plots
### ---------------------------------------------
datAggr_peak$region <- factor(datAggr_peak$region, 
                              levels=c("Region 1","Region 4","Region 11"),
                              labels=c("Region 1","Region 4","Region 11"))

pplot <- ggplot(data = subset(datAggr_peak  )) +
  geom_ribbon(aes(
    x = date_of_trigger, y = median.value, ymin = q25.value, ymax = q75.value,
    fill = reopen, group = interaction(reopen, rollback)
  ), alpha = 0.3) +
  geom_line(aes(
    x = date_of_trigger,
    y = median.value, col = reopen, alpha = rollback, group = rollback
  ), size = 1) +
  geom_point(aes(
    x = date_of_trigger,
    y = median.value, fill = reopen
  ), shape = 21) +
  geom_hline(aes(yintercept = avg_resource_available), col = capacity_col, linetype = "dashed") +
  facet_wrap(reopen ~ region, scales = "free") +
  scale_color_manual(values = TwoCols_seq) +
  scale_fill_manual(values = TwoCols_seq) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_alpha_manual(values = c(1, 0.75, 0.5, 0.3, 0.1)) +
  customTheme +
  labs(x = "Peak date", y = "ICU census", color ="Transmission increase",
       fill = "Transmission increase", alpha = "Mitigation\nstrengths")

pplot

f_save_plot(
  plot_name = paste0("mitigation_reduction_lines"), pplot = pplot,
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
