## -----------------------------------------
### Rscript to combine and analyze trigger analyses
## -----------------------------------------

library(tidyverse)
library(cowplot)
library(scales)
library(viridis)

source("load_paths.R")
source("processing_helpers.R")


simdate <- "20200826"
simulation_iteration <- paste0(simdate, "_state_events")
# simulation_output <- file.path(simulation_output, "EMS", simulation_iteration)
simulation_output <- file.path(simulation_output)

# outdir <- file.path(project_path, "Plots + Graphs/simulated_scenarios", simulation_iteration)
outdir <- file.path(project_path, "Plots + Graphs/simulated_scenarios", paste0(simdate, "_state_events"))

plot_first_day <- "2020-08-01"
plot_last_day <- "2021-01-01"

cols <- rev(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99"))
theme_set(theme_cowplot())


## ------------------------------


## ------------------------------
## Define functions
## ------------------------------

#### Load data
f_loadDat <- function(exp_name) {

  # capacitiesDat <- load_capacity() %>% mutate(region = ifelse(geography_name == "illinois", "All", geography_name))
  capacitiesDat <- load_new_capacity() %>% mutate(region = ifelse(geography_name == "illinois", "All", geography_name))


  # trajectories_fname="trajectoriesDat.csv"
  trajectories_fname <- "trajectoriesDat_trim.csv"
  trajectoriesDat <- read_csv(file.path(simulation_output, exp_name, trajectories_fname))

  colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))
  outvars <- colnames(trajectoriesDat)[c(grep("_EMS-", colnames(trajectoriesDat)), grep("_All", colnames(trajectoriesDat)))]
  outvars <- outvars[c(grep("Ki", outvars), grep("crit", outvars), grep("hosp", outvars))]

  paramVars <- colnames(trajectoriesDat)[grep("Ki_t ", colnames(trajectoriesDat))]
  keepvars <- c("time", "startdate", "scen_num", "capacity_multiplier", outvars, paramVars)


  dat <- trajectoriesDat %>%
    dplyr::select(keepvars) %>%
    dplyr::mutate(date = as.Date(startdate) + time) %>%
    filter(date > as.Date("2020-08-17") & date <= as.Date("2020-12-31")) %>%
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
    dplyr::select(time, region, date, scen_num, critical, critical_det, hospitalized, hospitalized_det, capacity_multiplier, exp_name) %>%
    left_join(capacitiesDat, by = "region")

  dat$region <- factor(dat$region, levels = c("All", c(1:11)), labels = c("illinois", c(1:11)))

  return(dat)
}

f_processDat <- function(df) {
  dfAggr_crit <- df %>%
    dplyr::group_by(region, capacity_multiplier, scen_num, exp_nr, expLabel) %>%
    filter(critical == max(critical)) %>%
    dplyr::group_by(region, exp_nr, exp_name, expLabel, capacity_multiplier, icu_available, medsurg_available) %>%
    dplyr::summarize(
      median.critical_det = median(critical_det, na.rm = TRUE),
      q2.5.critical_det = quantile(critical_det, probs = 0.025, na.rm = TRUE),
      q97.5.critical_det = quantile(critical_det, probs = 0.975, na.rm = TRUE),
      median.critical = median(critical, na.rm = TRUE),
      q2.5.critical = quantile(critical, probs = 0.025, na.rm = TRUE),
      q97.5.critical = quantile(critical, probs = 0.975, na.rm = TRUE)
    ) %>%
    dplyr::group_by(region, exp_nr, exp_name, expLabel, icu_available, medsurg_available) %>%
    mutate(
      critical_BelowCapacity = ifelse(max(median.critical) < icu_available, 1, 0),
      critical_det_BelowCapacity = ifelse(max(median.critical_det) < icu_available, 1, 0)
    )

  minCapacity_crit <- dfAggr_crit %>%
    group_by(region, exp_nr, capacity_multiplier) %>%
    filter(critical_BelowCapacity == 0 & median.critical < icu_available) %>%
    group_by(region, exp_nr) %>%
    summarize(minCapacity = max(capacity_multiplier)) %>%
    mutate(maxVal = 0.75)

  minCapacity_crit <- dfAggr_crit %>%
    group_by(region, exp_nr, capacity_multiplier) %>%
    filter(critical_det_BelowCapacity == 0 & median.critical_det < icu_available) %>%
    group_by(region, exp_nr) %>%
    summarize(minCapacity_det = max(capacity_multiplier)) %>%
    left_join(minCapacity_crit, by = c("region", "exp_nr"))

  df_crit <- dfAggr_crit %>% left_join(minCapacity_crit, by = c("region", "exp_nr"))


  ### Hospitalized
  dfAggr_hosp <- df %>%
    dplyr::group_by(region, exp_name, capacity_multiplier, scen_num, exp_nr, expLabel) %>%
    filter(hospitalized == max(hospitalized)) %>%
    dplyr::group_by(region, exp_nr, exp_name, expLabel, capacity_multiplier, icu_available, medsurg_available) %>%
    dplyr::summarize(
      median.hospitalized_det = median(hospitalized_det, na.rm = TRUE),
      q2.5.hospitalized_det = quantile(hospitalized_det, probs = 0.025, na.rm = TRUE),
      q97.5.hospitalized_det = quantile(hospitalized_det, probs = 0.975, na.rm = TRUE),
      median.hospitalized = median(hospitalized, na.rm = TRUE),
      q2.5.hospitalized = quantile(hospitalized, probs = 0.025, na.rm = TRUE),
      q97.5.hospitalized = quantile(hospitalized, probs = 0.975, na.rm = TRUE)
    ) %>%
    dplyr::group_by(region, exp_nr, expLabel, exp_name, icu_available, medsurg_available) %>%
    mutate(
      hospitalized_BelowCapacity = ifelse(max(median.hospitalized) < medsurg_available, 1, 0),
      hospitalized_det_BelowCapacity = ifelse(max(median.hospitalized_det) < medsurg_available, 1, 0)
    )


  minCapacity_hosp <- dfAggr_hosp %>%
    group_by(region, exp_nr, capacity_multiplier) %>%
    filter(hospitalized_BelowCapacity == 0 & median.hospitalized < medsurg_available) %>%
    group_by(region, exp_nr) %>%
    summarize(minCapacity = max(capacity_multiplier)) %>%
    mutate(maxVal = 0.75)

  minCapacity_hosp <- dfAggr_hosp %>%
    group_by(region, exp_nr, capacity_multiplier) %>%
    filter(hospitalized_det_BelowCapacity == 0 & median.hospitalized_det < medsurg_available) %>%
    group_by(region, exp_nr) %>%
    summarize(minCapacity_det = max(capacity_multiplier)) %>%
    left_join(minCapacity_hosp, by = c("region", "exp_nr"))

  df_hosp <- dfAggr_hosp %>% left_join(minCapacity_hosp, by = c("region", "exp_nr"))



  out <- list(df_crit, df_hosp)
  return(out)
}

f_thresholdScatterPlot <- function(channel = "critical", savePDF = FALSE, expLabel = expLabel) {


  # library(RColorBrewer)
  # getPalette = colorRampPalette(brewer.pal(8, "RdYlBu"))(length(unique(df.exp$exp_name)))


  if (channel == "critical") {
    df_crit <- f_processDat(df.exp)[[1]]
    colnames(df_crit)[colnames(df_crit) == "expLabel.x"] <- "expLabel"
    exp_names_crit <- exp_names[grep("crit", exp_names)]

    pplot <- df_crit %>%
      filter(region %in% c(1:11)) %>%
      filter(exp_name %in% exp_names_crit) %>%
      ggplot() +
      # annotate(geom = "rect", xmin = 0.75, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "azure4", alpha = 0.3) +
      geom_hline(aes(yintercept = icu_available), linetype = "dashed") +
      facet_wrap(~region, scales = "free") +
      scale_color_viridis_d(option = "C") +
      scale_fill_viridis_d(option = "C") +
      theme(legend.position = "bottom") +
      customThemeNoFacet +
      geom_hline(yintercept = c(-Inf, Inf)) +
      geom_vline(xintercept = c(-Inf, Inf)) +
      scale_x_discrete(expand = c(0, 0)) +
      labs(
        color = "scenario", fill = "scenario",
        y = "Predicted ICU census",
        x = "% of available ICU beds at which 'trigger' is pulled"
      ) +
      geom_line(aes(
        x = as.factor(capacity_multiplier), y = median.critical_det,
        col = as.factor(expLabel), group = exp_name
      ),
      size = 1, position = position_dodge(width = 0.4)
      ) +
      geom_point(aes(
        x = as.factor(capacity_multiplier), y = median.critical_det,
        fill = as.factor(expLabel), group = exp_name
      ),
      size = 2, shape = 21, position = position_dodge(width = 0.4)
      )


    ggsave(paste0(expLabel, "_", fname, ".png"),
      plot = pplot, path = outdir, width = 12, height = 8, device = "png"
    )

    if (savePDF) {
      ggsave(paste0(expLabel, "_", fname, ".pdf"),
        plot = pplot, path = outdir, width = 12, height = 8, device = "pdf"
      )
    }
  }



  if (channel == "hospitalized") {
    df_hosp <- f_processDat(df.exp)[[2]]

    exp_names_hosp <- exp_names[grep("hosp", exp_names)]
    colnames(df_hosp)[colnames(df_hosp) == "expLabel.x"] <- "expLabel"


    pplot <- df_hosp %>%
      filter(region %in% c(1:11)) %>%
      filter(exp_name %in% exp_names_hosp) %>%
      ggplot() +
      # annotate(geom = "rect", xmin = 0.75, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "azure4", alpha = 0.3) +
      geom_hline(aes(yintercept = medsurg_available), linetype = "dashed") +
      facet_wrap(~region, scales = "free") +
      scale_color_viridis_d(option = "C") +
      scale_fill_viridis_d(option = "C") +
      theme(legend.position = "bottom") +
      customThemeNoFacet +
      geom_hline(yintercept = c(-Inf, Inf)) +
      geom_vline(xintercept = c(-Inf, Inf)) +
      scale_x_discrete(expand = c(0, 0)) +
      labs(
        color = "scenario", fill = "scenario",
        y = "Predicted non-ICU  census",
        x = "% of available non-ICU  beds at which 'trigger' is pulled"
      ) +
      geom_line(aes(
        x = as.factor(capacity_multiplier), y = median.hospitalized_det,
        col = as.factor(expLabel), group = exp_name
      ),
      size = 1, position = position_dodge(width = 0.4)
      ) +
      geom_point(aes(
        x = as.factor(capacity_multiplier), y = median.hospitalized_det,
        fill = as.factor(expLabel), group = exp_name
      ),
      size = 2, shape = 21, position = position_dodge(width = 0.4)
      )



    ggsave(paste0(expLabel, "_", fname, ".png"),
      plot = pplot, path = outdir, width = 12, height = 8, device = "png"
    )

    if (savePDF) {
      ggsave(paste0(expLabel, "_", fname, ".pdf"),
        plot = pplot, path = outdir, width = 12, height = 8, device = "pdf"
      )
    }
  }

  return(pplot)
}


f_regionalComparison_hospCrit_plot <- function(reg, expDir = expDir, SAVE = TRUE, savePDF = FALSE) {
  exp_names_crit <- exp_names[grep("crit", exp_names)]
  exp_names_hosp <- exp_names[grep("hosp", exp_names)]

  pplot1 <- df_crit %>%
    filter(region %in% reg) %>%
    filter(exp_name %in% exp_names_crit) %>%
    ggplot() +
    geom_hline(aes(yintercept = icu_available), linetype = "dashed") +
    scale_color_viridis_d(option = "C") +
    scale_fill_viridis_d(option = "C") +
    theme(legend.position = "none") +
    customThemeNoFacet +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    scale_x_discrete(expand = c(0, 0)) +
    labs(
      title = paste0("covid region ", reg),
      subtitle = "ICU",
      y = "Predicted ICU census",
      x = "% of available ICU beds at which 'trigger' is pulled"
    ) +
    geom_line(aes(
      x = as.factor(capacity_multiplier), y = median.critical_det,
      col = as.factor(exp_name), group = exp_name
    ),
    size = 1, position = position_dodge(width = 0.4)
    ) +
    geom_point(aes(
      x = as.factor(capacity_multiplier), y = median.critical_det,
      fill = as.factor(exp_name), group = exp_name
    ),
    size = 2, shape = 21, position = position_dodge(width = 0.4)
    ) +
    customThemeNoFacet +
    background_grid() +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))


  pplot2 <- df_hosp %>%
    filter(region %in% reg) %>%
    filter(exp_name %in% exp_names_hosp) %>%
    ggplot() +
    geom_hline(aes(yintercept = medsurg_available), linetype = "dashed") +
    scale_color_viridis_d(option = "C") +
    scale_fill_viridis_d(option = "C") +
    theme(legend.position = "none") +
    customThemeNoFacet +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    scale_x_discrete(expand = c(0, 0)) +
    labs(
      title = "",
      subtitle = "non-ICU",
      y = "Predicted non-ICU census",
      x = "% of available non-ICU beds at which 'trigger' is pulled"
    ) +
    geom_line(aes(
      x = as.factor(capacity_multiplier), y = median.hospitalized_det,
      col = as.factor(exp_name), group = exp_name
    ),
    size = 1, position = position_dodge(width = 0.4)
    ) +
    geom_point(aes(
      x = as.factor(capacity_multiplier), y = median.hospitalized_det,
      fill = as.factor(exp_name), group = exp_name
    ),
    size = 2, shape = 21, position = position_dodge(width = 0.4)
    ) +
    customThemeNoFacet +
    background_grid() +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))



  pplot <- plot_grid(pplot1, pplot2, align = "hv")


  if (SAVE) {
    ggsave(paste0(reg, "_combined.png"),
      plot = pplot, path = outdir, width = 10, height = 4, device = "png"
    )

    savePDF <- F
    if (savePDF) {
      ggsave(paste0(reg, "_combined.pdf"),
        plot = pplot, path = outdir, width = 10, height = 4, device = "pdf"
      )
    }
  }

  return(pplot)
}



## ----------------------
### Generate plots
## ----------------------

exp_names <- list.dirs(simulation_output, recursive = FALSE, full.names = FALSE)


exp_names_crit <- exp_names[grep("crit", exp_names)]
exp_names_hosp <- exp_names[grep("hosp", exp_names)]

exp_names_crit <- exp_names_crit[grep(simdate, exp_names_crit)]
exp_names_hosp <- exp_names_hosp[grep(simdate, exp_names_hosp)]

exp_names <- c(exp_names_crit, exp_names_hosp)

datList <- list()
for (exp_name in exp_names) {

  # exp_name <- exp_names[2]
  print(exp_name)
  exp_dir <- file.path(file.path(simulation_output, exp_name))

  if (sum(grep("crit", exp_name)) == 1) expLabel <- "critical"
  if (sum(grep("hosp", exp_name)) == 1) expLabel <- "hospitalized"

  df.exp <- f_loadDat(exp_name)
  datList[[length(datList) + 1]] <- df.exp

  rm(exp_name)
}


df.exp <- datList %>% bind_rows(.id = "exp_nr")
df.exp$expLabel <- gsub(paste0(simdate, "_IL_hospdet_reopen"), "reopen ", df.exp$exp_name)
df.exp$expLabel <- gsub(paste0(simdate, "_IL_critdet_reopen"), "reopen ", df.exp$expLabel)
df.exp$expLabel <- gsub("perc_TriggeredRollback", "%", df.exp$expLabel)
unique(df.exp$expLabel)

df.exp$expLabel <- factor(df.exp$expLabel,
  levels = c("reopen 0%", "reopen 5%", "reopen 10%", "reopen 15%", "reopen 20%"),
  labels = c("reopen 0%", "reopen 5%", "reopen 10%", "reopen 15%", "reopen 20%")
)

f_thresholdScatterPlot(channel = "critical", savePDF = FALSE, expLabel = "critical")
f_thresholdScatterPlot(channel = "hospitalized", savePDF = FALSE, expLabel = "hospitalized")


for (reg in c(1:11)) {
  f_regionalComparison_hospCrit_plot(reg, SAVE = T)
}
