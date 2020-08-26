## -----------------------------------------
### Rscript to combine and analyze trigger analyses
## -----------------------------------------

library(tidyverse)
library(cowplot)
library(scales)
library(viridis)

source("load_paths.R")
source("processing_helpers.R")


simulation_iteration <- "20200814_state_events"
simulation_output <- file.path(simulation_output, "EMS", simulation_iteration)
outdir <- file.path(project_path, "Plots + Graphs/simulated_scenarios", simulation_iteration)

plot_first_day <- "2020-08-01"
plot_last_day <- "2021-01-01"

cols <- rev(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99"))
theme_set(theme_cowplot())


## ------------------------------
## Define functions
## ------------------------------

#### Load data
f_loadDat <- function(exp_name) {
  
  trajectoriesDat <- read_csv(file.path(simulation_output, exp_name, "trajectoriesDat_trim.csv"))

  outvars <- colnames(trajectoriesDat)[c(grep("_EMS-", colnames(trajectoriesDat)), grep("_All", colnames(trajectoriesDat)))]
  paramVars <- colnames(trajectoriesDat)[grep("Ki_t", colnames(trajectoriesDat))]
  keepvars <- c("time", "startdate", "scen_num","sample_num", outvars, paramVars)


  dat <- trajectoriesDat %>%
    dplyr::select(keepvars) %>%
    dplyr::mutate(date = as.Date(startdate) + time) %>%
    pivot_longer(cols = -c("time", "date", "startdate", "scen_num","sample_num"), names_to = "region") %>%
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

  return(dat)
}

### Merge dat and ensure same scenarios are used for baseline and scenario experiment
f_mergeExps <- function(exp_names=NULL, exp_name = NULL, baselineExp = NULL, outcomesToKeep = c("critical", "hospitalized","critical_det", "hospitalized_det"), filterKi = FALSE) {
  
  if (is.null(baselineExp)) baselineExp <- exp_names[grep("baseline", exp_names)]
  if (is.null(exp_name)) exp_name <- exp_names[1]

  varsToKeep <- c("time", "region", "date", "scen_num","sample_num", "Ki_t", outcomesToKeep)

  df.base <- f_loadDat(baselineExp) %>%
    dplyr::select(time, region, date, scen_num,sample_num, critical_det, hospitalized_det, Ki_t) %>%
    dplyr::rename(
      critical_det_base = critical_det,
      hospitalized_det_base = hospitalized_det,
      Ki_t_base = Ki_t
    )

  df.exp <- f_loadDat(exp_name) %>%
    dplyr::select(time, region, date, scen_num, sample_num, critical_det, hospitalized_det, Ki_t)

  
  mergevars <- colnames(df.exp)[colnames(df.exp) %in% colnames(df.base)]
  df.merged <- merge(df.exp, df.base, by = mergevars) %>%
    mutate(
      exp_name = exp_name,
      changedKi = ifelse(Ki_t != Ki_t_base, 1, 0)
    )

  if (filterKi) {
    df.merged <- df.merged %>%
      filter(changedKi != 0)
  }

  return(df.merged)
}

f_plotBarplot <- function(df, baselineExp = NULL, exp_names, SAVE = TRUE) {
  
  library(data.table)
  if (is.null(baselineExp)) baselineExp <- exp_names[grep("baseline", exp_names)]

  ## Cumulative
  df <- df %>% filter(date >= as.Date("2021-01-01") & date <= as.Date("2021-01-02"))
  summary(df$date)


  df <- as.data.frame(df)
  df <- data.table(df, key = c("scen_num", "region"))
  df[, deathsAverted := deaths[exp_name == baselineExp] - deaths, by = c("scen_num", "region")]
  df[, hospitalized_cumulAverted := hospitalized_cumul[exp_name == baselineExp] - hospitalized_cumul, by = c("scen_num", "region")]
  df[, crit_cumulAverted := crit_cumul[exp_name == baselineExp] - crit_cumul, by = c("scen_num", "region")]
  df <- as.data.frame(df)

  df$region <- factor(df$region, levels = c("All", c(1:11)), labels = c("All", c(1:11)))
  df$expname <- gsub("20200812_IL_MR_", "", df$exp_name)
  df$expname <- gsub("_triggeredrollback", "", df$expname)


  p1 <- df %>%
    filter(region %in% c(1, 2, 3, 4, 5, 6, 9) & exp_name != baselineExp) %>%
    dplyr::group_by(region, expname, expname_fct) %>%
    dplyr::summarize(
      non_ICU_averted = mean(hospitalized_cumulAverted, na.rm = TRUE),
      ICU_averted = mean(crit_cumulAverted, na.rm = TRUE),
      deaths_averted = mean(deathsAverted, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = -c("region", "expname", "expname_fct")) %>%
    ggplot() +
    geom_bar(aes(x = reorder(expname_fct, value), y = value, fill = expname_fct, alpha = region), stat = "identity", position = "stack", col = "azure4") +
    # geom_label(aes(x=reorder(expname_fct, value) , y =value, label=region )) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_brewer(palette = "Dark2") +
    theme(legend.position = "right") +
    facet_wrap(~name, scales = "free_x") +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    customThemeNoFacet +
    guides(fill = FALSE, size = FALSE) +
    labs(x = "total averted")

  if (SAVE) {
    ggsave(paste0("Cumulative_Cases_averted.png"),
      plot = p1, path = outdir, width = 12, height = 8, device = "png"
    )
  }

  return(df)
}

f_timeLineplot <- function(df, selectedRegions = c(2, 3, 4, 5), outcome = "critical", SAVE = TRUE) {
  
  scennums <- unique(df$scen_num)

  trajectories_per_exp <- df %>%
    dplyr::select(scenario, scen_num) %>%
    unique() %>%
    group_by(scenario) %>%
    tally()

  nexp_per_trajectory <- df %>%
    dplyr::select(scenario, scen_num) %>%
    unique() %>%
    group_by(scen_num) %>%
    tally()

  scensToKeep <- nexp_per_trajectory %>%
    filter(n == max(n) - 1) %>%
    select(scen_num)

  scensToKeep <- scensToKeep$scen_num

  if (outcome == "hospitalized") {
    df$baseVar <- df$hospitalized_det_base
    df$scenVar <- df$hospitalized_det
    df$capacity <- df$medsurg_available
    selectedScenarios <- unique(df$scenario)[grep("hosp", unique(df$scenario))]
    cols <- rev(c("darkorange", "darkorange4"))
  }
  if (outcome == "critical") {
    df$baseVar <- df$critical_det_base
    df$scenVar <- df$critical_det
    df$capacity <- df$icu_available
    selectedScenarios <- unique(df$scenario)[grep("crit", unique(df$scenario))]
    cols <- rev(c("deepskyblue2", "deepskyblue4"))
  }


  pplot <-  df %>%
    filter(
      scen_num %in% scensToKeep,
      region %in% selectedRegions,
      scenario %in% selectedScenarios
    ) %>%
    ggplot() +
    geom_line(aes(x = date, y = baseVar, group = interaction(scen_num, scenario)), col = "grey", size = 1.1) +
    geom_line(aes(x = date, y = scenVar, group = interaction(scen_num, scenario), col = scenario), size = 1.1) +
    geom_hline(aes(yintercept = capacity), linetype = "solid", col = cols[1], size = 1) +
    geom_hline(aes(yintercept = capacity * 0.75), linetype = "dashed", col = cols[2], size = 1) +
    facet_wrap(~region, scales = "free_y", nrow = 1) +
    customThemeNoFacet +
    theme(legend.position = "bottom") +
    scale_color_manual(values = cols) +
    scale_x_date(date_breaks = "30 days", date_labels = "%d\n%b") +
    labs(x = "")


  if (SAVE) {
    pname <- paste0(outcome, "_trajectoryTimeline")

    ggsave(paste0(pname, ".png"),
      plot = pplot, path = outdir, width = 14, height = 4, device = "png"
    )

    ggsave(paste0(pname, ".pdf"),
      plot = pplot, path = outdir, width = 14, height = 4, device = "pdf"
    )
  }

  return(df)
}


## ------------------------------
## Run analysis
## ------------------------------

### Load capacity
capacitiesDat <- load_capacity() %>%
  mutate(region = ifelse(geography_name == "illinois", "All", geography_name))


## ----------------------
exp_names <- list.dirs(simulation_output, recursive = FALSE, full.names = FALSE)


datList <- list()
for (exp_name in exp_names) {
  print(exp_name)
  datList[[length(datList) + 1]] <- f_mergeExps(exp_names=exp_names, exp_name=exp_name)
}


datAll <- datList %>%
  bind_rows() %>%
  as.data.frame() %>%
  left_join(capacitiesDat, by = "region") %>%
  f_addRestoreRegion() %>%
  mutate(
    expname = gsub("_triggeredrollback", "", gsub("20200812_IL_MR_", "", exp_name)),
    scenario = expname
  )

unique(datAll$expname)
datAll$expname_fct <- factor(datAll$expname,
  levels = c("baseline", "critical100", "critical75", "hospitalizations100", "hospitalizations75"),
  labels = c(
    "None", "100% of ICU beds available", "75% of ICU beds available",
    "100% of non-ICU beds available", "75% of non-ICU beds available"
  )
)

### Plots
f_plotBarplot(df = datAll)

f_timeLineplot(df = datAll, selectedRegions = c(2, 3, 4, 5), outcome = "critical", SAVE = T)
f_timeLineplot(df = datAll, selectedRegions = c(2, 3, 4, 5), outcome = "hospitalized", SAVE = T)


## ----------------------
## Plot Ki over time
## ----------------------
pt <- datAll %>%
  filter(region != "All" & date >= as.Date(plot_first_day) & date <= as.Date(plot_last_day)) %>%
  dplyr::group_by(restore_region, region, expname, date, expname_fct) %>%
  dplyr::summarize(Ki_t = mean(Ki_t, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = date, y = Ki_t, col = expname_fct), size = 1.1) +
  scale_y_continuous(expand = c(0, 0)) +
  # scale_color_manual(values=cols) +
  facet_wrap(region ~ restore_region, scales = "free_y") +
  theme_cowplot() +
  labs(x = "", y = "change in transmission", col = "Trigger:") +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2")

ggsave(paste0("Change_in_transmission.png"),
  plot = pt, path = outdir, width = 12, height = 8, device = "png"
)
## ----------------------
