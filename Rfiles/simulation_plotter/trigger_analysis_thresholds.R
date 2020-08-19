## -----------------------------------------
### Rsctipt to combine and analyse trigger analyses
## -----------------------------------------

library(tidyverse)
library(cowplot)
library(scales)
library(viridis)

source("load_paths.R")
source("processing_helpers.R")

plot_first_day <- "2020-08-01"
plot_last_day <- "2021-01-01"

outdir <- file.path("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/Plots + Graphs/simulated_scenarios/20200814_state_events")
cols <- rev(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99"))


theme_set(theme_cowplot())



## ------------------------------
## Define functions
## ------------------------------

#### Load data
f_loadDat <- function(exp_name) {
  capacitiesDat <- load_capacity() %>% mutate(region = ifelse(geography_name == "illinois", "All", geography_name))

  trajectoriesDat <- read.csv(file.path(simulation_output, exp_name, "trajectoriesDat.csv"))

  colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))
  outvars <- colnames(trajectoriesDat)[c(grep("_EMS-", colnames(trajectoriesDat)), grep("_All", colnames(trajectoriesDat)))]
  paramVars <- colnames(trajectoriesDat)[grep("Ki_t_", colnames(trajectoriesDat))]
  keepvars <- c("time", "startdate", "scen_num", "capacity_multiplier", outvars, paramVars)


  dat <- trajectoriesDat %>%
    dplyr::select(keepvars) %>%
    dplyr::mutate(date = as.Date(startdate) + time) %>%
    pivot_longer(cols = -c("time", "date", "startdate", "scen_num", "capacity_multiplier"), names_to = "region") %>%
    dplyr::mutate(
      region = gsub("_All", "_EMS-All", region),
      region = gsub("_EMS_", "_EMS-", region)
    ) %>%
    separate(region, into = c("outcome", "region"), sep = "_EMS-") %>%
    mutate(
      exp_name = exp_name,
    ) %>%
    pivot_wider(names_from = "outcome", values_from = "value") %>%
    left_join(capacitiesDat, by = "region")


  dat <- dat %>%
    dplyr::select(time, region, date, scen_num, critical, critical_det, hospitalized, hospitalized_det, capacity_multiplier, Ki_t) %>%
    filter(date > as.Date("2020-08-17") & date <= as.Date("2020-12-31")) %>%
    left_join(capacitiesDat, by = "region")

  dat$region <- factor(dat$region, levels = c("All", c(1:11)), labels = c("illinois", c(1:11)))

  return(dat)
}


f_processDat <- function(df) {
  dfAggr_crit <- df %>%
    dplyr::group_by(region, capacity_multiplier) %>%
    filter(critical == max(critical)) %>%
    dplyr::group_by(region, capacity_multiplier, icu_available, medsurg_available) %>%
    dplyr::summarize(
      median.critical_det = median(critical_det, na.rm = TRUE),
      q2.5.critical_det = quantile(critical_det, probs = 0.025, na.rm = TRUE),
      q97.5.critical_det = quantile(critical_det, probs = 0.975, na.rm = TRUE),
      median.critical = median(critical, na.rm = TRUE),
      q2.5.critical = quantile(critical, probs = 0.025, na.rm = TRUE),
      q97.5.critical = quantile(critical, probs = 0.975, na.rm = TRUE)
    ) %>%
    dplyr::group_by(region, icu_available, medsurg_available) %>%
    mutate(
      critical_BelowCapacity = ifelse(max(median.critical) < icu_available, 1, 0),
      critical_det_BelowCapacity = ifelse(max(median.critical_det) < icu_available, 1, 0)
    )

  minCapacity_crit <- dfAggr_crit %>%
    group_by(region, capacity_multiplier) %>%
    filter(critical_BelowCapacity == 0 & median.critical < icu_available) %>%
    group_by(region) %>%
    summarize(minCapacity = max(capacity_multiplier)) %>%
    mutate(maxVal = 0.75)

  minCapacity_crit <- dfAggr_crit %>%
    group_by(region, capacity_multiplier) %>%
    filter(critical_det_BelowCapacity == 0 & median.critical_det < icu_available) %>%
    group_by(region) %>%
    summarize(minCapacity_det = max(capacity_multiplier)) %>%
    left_join(minCapacity_crit, by = "region")

  df_crit <- dfAggr_crit %>% left_join(minCapacity_crit, by = "region")


  ### Hospitalized
  dfAggr_hosp <- df %>%
    dplyr::group_by(region, capacity_multiplier) %>%
    filter(hospitalized == max(hospitalized)) %>%
    dplyr::group_by(region, capacity_multiplier, icu_available, medsurg_available) %>%
    dplyr::summarize(
      median.hospitalized_det = median(hospitalized_det, na.rm = TRUE),
      q2.5.hospitalized_det = quantile(hospitalized_det, probs = 0.025, na.rm = TRUE),
      q97.5.hospitalized_det = quantile(hospitalized_det, probs = 0.975, na.rm = TRUE),
      median.hospitalized = median(hospitalized, na.rm = TRUE),
      q2.5.hospitalized = quantile(hospitalized, probs = 0.025, na.rm = TRUE),
      q97.5.hospitalized = quantile(hospitalized, probs = 0.975, na.rm = TRUE)
    ) %>%
    dplyr::group_by(region, icu_available, medsurg_available) %>%
    mutate(
      hospitalized_BelowCapacity = ifelse(max(median.hospitalized) < medsurg_available, 1, 0),
      hospitalized_det_BelowCapacity = ifelse(max(median.hospitalized_det) < medsurg_available, 1, 0)
    )


  minCapacity_hosp <- dfAggr_hosp %>%
    group_by(region, capacity_multiplier) %>%
    filter(hospitalized_BelowCapacity == 0 & median.hospitalized < medsurg_available) %>%
    group_by(region) %>%
    summarize(minCapacity = max(capacity_multiplier)) %>%
    mutate(maxVal = 0.75)

  minCapacity_hosp <- dfAggr_hosp %>%
    group_by(region, capacity_multiplier) %>%
    filter(hospitalized_det_BelowCapacity == 0 & median.hospitalized_det < medsurg_available) %>%
    group_by(region) %>%
    summarize(minCapacity_det = max(capacity_multiplier)) %>%
    left_join(minCapacity_hosp, by = "region")

  df_hosp <- dfAggr_hosp %>% left_join(minCapacity_hosp, by = "region")



  out <- list(df_crit, df_hosp)
  return(out)
}


f_thresholdScatterPlot <- function(channel = "critical", expDir = expDir, savePDF = FALSE, showDet = TRUE) {
  if (channel == "critical") {
    df_crit <- f_processDat(df.exp)[[1]]

    pplot <- df_crit %>%
      filter(region %in% c(1:11)) %>%
      ggplot() +
      annotate(geom = "rect", xmin = 0.75, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "azure4", alpha = 0.3) +
      geom_hline(aes(yintercept = icu_available), linetype = "dashed") +
      facet_wrap(~region, scales = "free") +
      scale_color_manual(values = c("red", "deepskyblue3")) +
      theme(legend.position = "none") +
      customThemeNoFacet +
      geom_hline(yintercept = c(-Inf, Inf)) +
      geom_vline(xintercept = c(-Inf, Inf)) +
      scale_x_continuous(expand = c(0, 0)) +
      labs(
        y = "Predicted ICU census",
        x = "% of available ICU beds at which 'trigger' is pulled"
      )

    if (showDet == FALSE) {
      fname <- "ICU_all"
      pplot <- pplot + geom_rect(mapping = aes(xmin = minCapacity, xmax = 0.75, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.03) +
        geom_errorbar(aes(x = capacity_multiplier, ymin = q2.5.critical, ymax = q97.5.critical), width = 0.05, alpha = 0.7) +
        geom_point(aes(x = capacity_multiplier, y = median.critical, col = as.factor(critical_BelowCapacity)), size = 2, alpha = 0.7) +
        labs(caption = fname)
    }
    if (showDet) {
      fname <- "ICU_det"
      pplot <- pplot + geom_rect(mapping = aes(xmin = minCapacity_det, xmax = 0.75, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.03) +
        geom_errorbar(aes(x = capacity_multiplier, ymin = q2.5.critical_det, ymax = q97.5.critical_det), width = 0.05) +
        geom_point(aes(x = capacity_multiplier, y = median.critical_det, col = as.factor(critical_det_BelowCapacity)), size = 2) +
        labs(caption = fname)
    }
  }



  if (channel == "hospitalized") {
    df_hosp <- f_processDat(df.exp)[[2]]

    pplot <- df_hosp %>%
      # filter(region %in% c(2, 3, 4)) %>%
      # filter(!(region %in% c("All", c(2,3,4)))) %>%
      filter(region %in% c(1:11)) %>%
      ggplot() +
      annotate(geom = "rect", xmin = 0.75, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "azure4", alpha = 0.3) +
      geom_hline(aes(yintercept = medsurg_available), linetype = "dashed") +
      facet_wrap(~region, scales = "free") +
      scale_color_manual(values = c("red", "deepskyblue3")) +
      theme(legend.position = "none") +
      customThemeNoFacet +
      geom_hline(yintercept = c(-Inf, Inf)) +
      geom_vline(xintercept = c(-Inf, Inf)) +
      scale_x_continuous(expand = c(0, 0)) +
      labs(
        y = "Predicted non-ICU census",
        x = "% of available ICU beds at which 'trigger' is pulled"
      )


    if (showDet == FALSE) {
      fname <- "nonICU_all"
      pplot <- pplot + geom_rect(mapping = aes(xmin = minCapacity, xmax = 0.75, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.03) +
        geom_errorbar(aes(x = capacity_multiplier, ymin = q2.5.hospitalized, ymax = q97.5.hospitalized), width = 0.05, alpha = 0.7) +
        geom_point(aes(x = capacity_multiplier, y = median.hospitalized, col = as.factor(hospitalized_BelowCapacity)), size = 2, alpha = 0.7) +
        labs(caption = fname)
    }
    if (showDet) {
      fname <- "nonICU_det"
      pplot <- pplot + geom_rect(mapping = aes(xmin = minCapacity_det, xmax = 0.75, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.03) +
        geom_errorbar(aes(x = capacity_multiplier, ymin = q2.5.hospitalized_det, ymax = q97.5.hospitalized_det), width = 0.05) +
        geom_point(aes(x = capacity_multiplier, y = median.hospitalized_det, col = as.factor(hospitalized_det_BelowCapacity)), size = 2) +
        labs(caption = fname)
    }
  }


  ggsave(paste0(fname, ".png"),
    plot = pplot, path = exp_dir, width = 12, height = 8, device = "png"
  )

  if (savePDF) {
    ggsave(paste0(fname, ".pdf"),
      plot = pplot, path = exp_dir, width = 12, height = 8, device = "pdf"
    )
  }


  return(pplot)
}


## ----------------------
### Generate plots
## ----------------------

#  "20200812_IL_MR_baseline",
exp_names <- c(
  "20200817_IL_criticalhospdet_vary0to1_triggeredrollback",
  "20200817_IL_hospdet_vary0to1_triggeredrollback",
  "20200817_IL_criticaldet_vary0to1_triggeredrollback"
)



for (exp_name in exp_names) {
  # exp_name <- exp_names[4]
  print(exp_name)
  exp_dir <- file.path(file.path(simulation_output, exp_name))

  df.exp <- f_loadDat(exp_name)

  f_thresholdScatterPlot(channel = "critical", expDir = expDir, savePDF = FALSE, showDet = T)
  f_thresholdScatterPlot(channel = "hospitalized", expDir = expDir, savePDF = FALSE, showDet = T)

  f_thresholdScatterPlot(channel = "critical", expDir = expDir, savePDF = FALSE, showDet = F)
  f_thresholdScatterPlot(channel = "hospitalized", expDir = expDir, savePDF = FALSE, showDet = F)
  rm(exp_name)
}
