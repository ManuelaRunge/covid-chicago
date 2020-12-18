### =============================================
### Plot ICU over time per region
### =============================================

library(tidyverse)
library(cowplot)
library(data.table)

theme_set(theme_minimal())

runInBatchMode <- FALSE

if (runInBatchMode) {
  cmd_agrs <- commandArgs()
  length(cmd_agrs)
  exp_stem <- cmd_agrs[length(cmd_agrs) - 2]
  Location <- cmd_agrs[length(cmd_agrs) - 1]
  workingDir <- cmd_agrs[length(cmd_agrs)]
  simdate <- strsplit(exp_name, split = "_IL_")[[1]][1]
} else {
  Location <- "Local"
  simdate <- "20201212"
  exp_stem <- paste0(simdate, "_IL_regreopen")
}


setwd(workingDir)
source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")
customTheme <- f_getCustomTheme()

if (Location == "Local") sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (Location == "NUCLUSTER") sim_dir <- "/projects/p30781/covidproject/covid-chicago/_temp"
if (!dir.exists(file.path(sim_dir, "ICU_bar_plots"))) dir.create(file.path(sim_dir, "ICU_bar_plots"))

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep(exp_stem, exp_names)]
exp_names <- exp_names[c(grep("daysdelay", exp_names), grep("counterfactual", exp_names))]


if (length(exp_names) == 1) {
  sim_dir <- file.path(sim_dir, exp_names[1])
  dat <- fread(file.path(sim_dir, "trajectories_aggregated.csv"))
  dat$exp_name <- exp_names[1]
} else {
  dat <- f_combineDat(sim_dir, exp_names, "trajectories_aggregated.csv")
}


if (!dir.exists(file.path(sim_dir, "ICU_timeline_plots"))) dir.create(file.path(sim_dir, "ICU_timeline_plots"))
if (!dir.exists(file.path(sim_dir, "ICU_timeline_plots", "pdf"))) dir.create(file.path(sim_dir, "ICU_timeline_plots", "pdf"))

subregions <- c("covidregion_1", "covidregion_4", "covidregion_11")
regions <- paste0("covidregion_", c(1, 4, 11))

### Data processing before plotting
dat$date <- as.Date(dat$date)
tapply(dat$date, dat$exp_name, summary)

dat <- dat %>%
  filter(date >= as.Date("2020-07-01") & date <= as.Date("2021-02-01")) %>%
  mutate(geography_modeled = gsub("EMS-", "covidregion_", ems)) %>%
  filter(ems != "All") %>%
  dplyr::select(-ems)


dat <- dat %>% filter(geography_modeled %in% regions)
dat$scen_name <- gsub(paste0(simdate, "_IL_regreopen"), "", dat$exp_name)

dat <- dat %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat$rollback[is.na(dat$rollback)] <- "counterfactual"
dat$region <- factor(dat$geography_modeled, levels = regions, labels = gsub("covidregion_", "Region ", regions))

dat$capacity_multiplier_fct <- round(dat$capacity_multiplier * 100, 0)
fct_labels <- sort(unique(dat$capacity_multiplier_fct))
dat$capacity_multiplier_fct[dat$rollback == "counterfactual"] <- "counterfactual"
dat$capacity_multiplier_fct <- factor(dat$capacity_multiplier_fct,
  levels = c(fct_labels, "counterfactual"),
  labels = c(fct_labels, "counterfactual")
)

rollback_values <- unique(dat$rollback)
delay_values <- unique(dat$delay)

rollback_val <- rollback_values[2]
delay_val <- delay_values[1]

dat$reopen_fct <- factor(dat$reopen, 
                         levels=c("100perc","50perc"),
                         labels=c("High\ntransmission\nncrease",
                                  "Low\ntransmission\nincrese"))

dat$reopen_fct2 <- factor(dat$reopen, 
                          levels=c("100perc","50perc"),
                          labels=c("High","Low"))

dat$rollback_fct <- factor(dat$rollback, 
                           levels=c("pr8","pr6", "pr4","pr2"),
                           labels=rev(seq(20,80,20)))


### check
str(dat)
table(dat$region)
table(dat$capacity_multiplier, dat$capacity_multiplier_fct)
table(dat$region, dat$reopen)
table(dat$region, dat$delay)
table(dat$region, dat$rollback)

### load capacity
capacityDat <- load_new_capacity(filedate = "20200915") %>%
  mutate(geography_modeled = paste0("covidregion_", geography_name))

dat <- f_addVar(dat, capacityDat) %>% filter(date >= as.Date("2020-09-01"))


### prepare
dat_counterfactual <- dat %>%
  filter(rollback == "counterfactual") %>%
  dplyr::select(-rollback, -delay, -capacity_multiplier_fct)
dat_counterfactual$reopen_fct <- gsub(
  "100perc", "High\ntransmission increase",
  gsub(
    "50perc", "Low\ntransmission increase",
    dat_counterfactual$reopen
  )
)


dat_scen <- dat %>% filter(rollback != "counterfactual" & delay == unique(dat$delay)[1])

dat_scen$reopen_fct <- gsub(
  "100perc", "High\ntransmission increase",
  gsub(
    "50perc", "Low\ntransmission increase",
    dat_scen$reopen
  )
)

#### ====================================
### PLOTS - basic descriptive
#### ====================================


if (length(unique(dat$rollback)) > 1) {
  for (reg in subregions) {
    icu_available <- dat_scen %>%
      filter(geography_modeled == reg) %>%
      dplyr::select(icu_available) %>%
      unique() %>%
      as.numeric()

    pplot <-
      ggplot(data = subset(dat_scen, geography_modeled == reg)) +
      # geom_rect(xmin=-Inf, xmax=as.Date("2020-10-01"), ymin=-Inf, ymax-Inf, alpha=0.01, fill="lightgrey") +
      geom_line(
        data = subset(dat_counterfactual, geography_modeled == reg),
        aes(x = date, y = crit_det_median)
      ) +
      geom_ribbon(
        data = subset(dat_counterfactual, geography_modeled == reg),
        aes(x = date, ymin = crit_det_50CI_lower, ymax = crit_det_50CI_upper), alpha = 0.3
      ) +
      geom_ribbon(
        data = subset(dat_scen, geography_modeled == reg),
        aes(
          x = date, ymin = crit_det_50CI_lower, ymax = crit_det_50CI_upper,
          fill = capacity_multiplier_fct, group = capacity_multiplier_fct
        ), alpha = 0.3
      ) +
      geom_line(
        data = subset(dat_scen, geography_modeled == reg),
        aes(x = date, y = crit_det_median, col = capacity_multiplier_fct, group = capacity_multiplier_fct)
      ) +
      geom_hline(aes(yintercept = icu_available), linetype = "dashed", col = "dodgerblue3") +
      # geom_vline(xintercept=as.Date("2020-10-01"))+
      facet_grid(reopen_fct ~ rollback_fct, scales = "free") +
      scale_x_date(date_breaks = "30 days", date_labels = "%b") +
      scale_fill_viridis_d(option = "C", direction = 1) +
      scale_color_viridis_d(option = "C", direction = 1) +
      labs(
        title = paste0("Region ", gsub("covidregion_", "", reg)),
        subtitle = paste0("\nICU capacity: ", icu_available, "\n\nMitigation effectiveness"),
        x = "",
        y = "Predicted ICU census",
        col = "Trigger threshold",
        fill = "Trigger threshold",
        caption = "median and 50% observed range"
      ) +
      customTheme

    pplot
    ggsave(paste0("ICU_timelineplot_", reg, ".png"),
      plot = pplot,
      path = file.path(sim_dir, "ICU_timeline_plots"), width = 12, height = 6, device = "png"
    )
    ggsave(paste0("ICU_timelineplot_", reg, ".pdf"),
      plot = pplot,
      path = file.path(sim_dir, "ICU_timeline_plots", "pdf"), width = 12, height = 6, device = "pdf"
    )
    rm(pplot, reg)
  }
}

for (reg in subregions) {
  icu_available <- dat_counterfactual %>%
    filter(geography_modeled == reg) %>%
    dplyr::select(icu_available) %>%
    unique() %>%
    as.numeric()

  pplot <-
    ggplot(data = subset(dat_counterfactual, geography_modeled == reg)) +
    # geom_rect(xmin=-Inf, xmax=as.Date("2020-10-01"), ymin=-Inf, ymax=Inf, alpha=0.08, fill="lightgrey") +
    geom_vline(xintercept = as.Date("2020-10-01"), col = "lightgrey") +
    geom_line(
      data = subset(dat_counterfactual, geography_modeled == reg),
      aes(x = date, y = crit_det_median)
    ) +
    geom_ribbon(
      data = subset(dat_counterfactual, geography_modeled == reg),
      aes(x = date, ymin = crit_det_50CI_lower, ymax = crit_det_50CI_upper), alpha = 0.4
    ) +
    geom_ribbon(
      data = subset(dat_counterfactual, geography_modeled == reg),
      aes(x = date, ymin = crit_det_95CI_lower, ymax = crit_det_95CI_upper), alpha = 0.3
    ) +
    geom_hline(aes(yintercept = icu_available), linetype = "dashed", col = "dodgerblue3") +
    facet_wrap(~reopen, scales = "free") +
    scale_x_date(date_breaks = "30 days", date_labels = "%b") +
    labs(
      title = reg,
      subtitle = paste0("\nicu_available: ", icu_available),
      x = "",
      y = "predicted ICU census",
      col = "ICU trigger threshold",
      fill = "ICU trigger threshold",
      caption = "median and 50%, 95% IQR"
    ) +
    customTheme

  ggsave(paste0("ICU_timelineplot_counterfactual_", reg, ".pdf"),
    plot = pplot,
    path = file.path(sim_dir, "ICU_timeline_plots", "pdf"), width = 12, height = 9, device = "pdf"
  )
  ggsave(paste0("ICU_timelineplot_counterfactual_", reg, ".png"),
    plot = pplot,
    path = file.path(sim_dir, "ICU_timeline_plots"), width = 12, height = 9, device = "png"
  )
  rm(pplot, reg)
}

#### ====================================
## Recommended versus selected
#### ====================================
unique(dat_scen$capacity_multiplier_fct)
dat_scen <- dat_scen %>%
  mutate(
    trigger_recommended = ifelse(geography_modeled == "covidregion_1", "60", "80"),
    trigger_recommended = ifelse(geography_modeled == "covidregion_4", "70", trigger_recommended),
    trigger_recommended = ifelse(geography_modeled == "covidregion_11", "60", trigger_recommended)
  )

dat_counterfactual_sub <- dat_counterfactual %>%
  filter(date >= as.Date("2020-09-01") &
    date <= as.Date("2020-12-31")) %>%
  mutate(grp = "counterfactual\n(no mitigation)")

dat_scen_sub1 <- dat_scen %>%
  filter(capacity_multiplier_fct == trigger_recommended) %>%
  mutate(grp = "recommended\n(mitigaton triggered at\n 60% (based on high transmission incr.)))")

dat_scen_sub2 <- dat_scen %>%
  filter(capacity_multiplier_fct == "80") %>%
  mutate(grp = "default\n(mitigaton triggered at 80%)")


plotdat <- rbind(dat_scen_sub1, dat_scen_sub2) %>%
  filter(rollback == rollback_values[3] & delay == delay_val &
           date >= as.Date("2020-09-01") &
           date <= as.Date("2020-12-31"))

pplot_no_counterfactual <-
  ggplot(data = subset(plotdat)) +
   geom_ribbon(
    aes(
      x = date, ymin = crit_det_95CI_lower, ymax = crit_det_95CI_upper,
      fill = grp,group = grp ),alpha = 0.2
  ) +
  geom_ribbon(
    aes(
      x = date, ymin = crit_det_50CI_lower, ymax = crit_det_50CI_upper,
      fill = grp,group = grp  ), alpha = 0.4
  ) +
  geom_line(
    aes(
      x = date, y = crit_det_median,
      col = grp,  group = grp), size = 1.3
  ) +
  geom_hline(aes(yintercept = icu_available),
             linetype = "dashed", col = "dodgerblue3"
  ) +
  facet_wrap(reopen_fct ~ region, scales = "free") +
  scale_x_date(date_breaks = "30 days", date_labels = "%b") +
  labs(
    x = "",
    y = "Predicted ICU census",
    col = "ICU trigger threshold",
    fill = "ICU trigger threshold",
    caption = "median and 50% IQR"
  ) +
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  customTheme
pplot_no_counterfactual 


pplot <-
  ggplot(data = subset(plotdat)) +
  geom_ribbon(
    data = dat_counterfactual_sub,
    aes(
      x = date, ymin = crit_det_95CI_lower, ymax = crit_det_95CI_upper,
      fill = grp,    group = grp),alpha = 0.2
  ) +
  geom_ribbon(
    data = dat_counterfactual_sub,
    aes(
      x = date, ymin = crit_det_50CI_lower, ymax = crit_det_50CI_upper,
      fill = grp, group = grp ), alpha = 0.4) +
  geom_ribbon(
    aes(
      x = date, ymin = crit_det_95CI_lower, ymax = crit_det_95CI_upper,
      fill = grp, group = grp ), alpha = 0.2
  ) +
  geom_ribbon(
    aes(
      x = date, ymin = crit_det_50CI_lower, ymax = crit_det_50CI_upper,
      fill = grp, group = grp ), alpha = 0.4
  ) +
  geom_line(
    data = dat_counterfactual_sub,
    aes(
      x = date, y = crit_det_median,
      col = grp, group = grp), size = 1.3
  ) +
  geom_line(
    aes(
      x = date, y = crit_det_median,
      col = grp, group = grp), size = 1.3
  ) +
  geom_hline(aes(yintercept = icu_available),
    linetype = "dashed", col = "dodgerblue3"
  ) +
  # geom_vline(xintercept=as.Date("2020-10-01"))+
  facet_wrap(reopen_fct ~ region, scales = "free") +
  scale_x_date(date_breaks = "30 days", date_labels = "%b") +
  # scale_y_continuous(lim = c(0, 2), expand = c(0, 0)) +
  labs(
    x = "",
    y = "Predicted ICU census",
    col = "ICU trigger threshold",
    fill = "ICU trigger threshold",
    caption = "median and 50% IQR"
  ) +
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  customTheme

pplot


#### Add data points
ref_dat <- f_load_ref_data(subregions = c(1, 4, 11), 
                           startdate = as.Date("2020-09-01"), 
                           stopdate = as.Date("2020-12-12")) %>%
  filter(name == "confirmed_covid_icu" & source == "EMResource")


pplot_no_counterfactual_wdata <- pplot_no_counterfactual + 
  geom_point(
    data = ref_dat, aes(x = Date, y = value),
    size = 1
  ) +
  geom_line(
    data = ref_dat, aes(x = Date, y = value7),
    size = 1.2
  )


pplot_wdata <- pplot +
  geom_point(
    data = ref_dat, aes(x = Date, y = value),
    size = 1
  ) +
  geom_line(
    data = ref_dat, aes(x = Date, y = value7),
    size = 1.2
  )



pplot_wdata
pplot_no_counterfactual_wdata


f_save_plot(plot_name=paste0("comparison_predicted_to_actual_trend"), pplot = pplot_wdata, 
            plot_dir = file.path(sim_dir, "ICU_timeline_plots"), width = 12, height = 6)


f_save_plot(plot_name=paste0("comparison_predicted_to_actual_trend_no_counterfactual"), 
            pplot = pplot_no_counterfactual_wdata, 
            plot_dir = file.path(sim_dir, "ICU_timeline_plots"), width = 12, height = 6)


