## -----------------------------------------
### Rscript to combine and analyze trigger analyses
## -----------------------------------------

library(tidyverse)
library(cowplot)
library(scales)
library(viridis)
library(lubridate)
library(broom)


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
  keepvars <- c("time", "startdate", "scen_num", "sample_num", "capacity_multiplier", outvars, paramVars)


  dat <- trajectoriesDat %>%
    dplyr::select(keepvars) %>%
    dplyr::mutate(date = as.Date(startdate) + time) %>%
    filter(date > as.Date("2020-08-17") & date <= as.Date("2020-12-31")) %>%
    pivot_longer(cols = -c("time", "date", "startdate", "scen_num", "sample_num", "capacity_multiplier"), names_to = "region") %>%
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
    group_by(startdate, region, scen_num, exp_name) %>%
    arrange(date) %>%
    mutate(changedKi = ifelse(Ki_t != lag(Ki_t), 1, 0))

  triggerDate <- dat %>%
    group_by(startdate, region, scen_num, exp_name) %>%
    filter(changedKi == 1) %>%
    summarize(triggerDate = min(date))

  dat <- dat %>%
    dplyr::select(time, region, date, scen_num, sample_num, Ki_t, critical, critical_det, hospitalized, hospitalized_det, capacity_multiplier, changedKi, exp_name) %>%
    left_join(capacitiesDat, by = "region") %>%
    left_join(triggerDate, by = c("startdate", "region", "scen_num", "exp_name"))

  dat$region <- factor(dat$region, levels = c("All", c(1:11)), labels = c("illinois", c(1:11)))



  dat <- dat %>% mutate(
    trigger = ifelse(is.na(triggerDate), 0, 1),
    timeBeforeTrigger = date - triggerDate
  )

  dat$timeBeforeTrigger <- as.numeric(dat$timeBeforeTrigger)

  dat$capacity_multiplier_fct <- factor(dat$capacity_multiplier,
    levels = rev(unique(dat$capacity_multiplier)),
    labels = rev(unique(dat$capacity_multiplier))
  )

  return(dat)
}

f_plot_prepostTrigger <- function(df, SAVE = T, savePDF = F) {
  pplot <- df %>%
    filter((timeBeforeTrigger >= -28 & timeBeforeTrigger < 21)) %>%
    ggplot() +
    geom_vline(xintercept = 0) +
    geom_hline(aes(yintercept = icu_available), linetype = "dashed") +
    geom_hline(aes(yintercept = icu_available * 0.875), linetype = "dashed") +
    geom_hline(aes(yintercept = icu_available * 0.75), linetype = "dashed") +
    geom_hline(aes(yintercept = icu_available * 0.625), linetype = "dashed") +
    geom_hline(aes(yintercept = icu_available * 0.5), linetype = "dashed") +
    geom_line(aes(x = timeBeforeTrigger, y = critical_det, group = interaction(scen_num, capacity_multiplier_fct)), col = "grey", alpha = 0.3) +
    geom_smooth(aes(x = timeBeforeTrigger, y = critical_det, group = interaction(capacity_multiplier_fct), col = as.factor(capacity_multiplier_fct))) +
    facet_wrap(~region, scales = "free") +
    labs(
      title = reopenLabel,
      subtitle = "",
      color = "Capacity threshold",
      x = "Time relative to Trigger"
    ) +
    scale_color_viridis_d(direction = -1) +
    customThemeNoFacet


  if (SAVE) {
    ggsave(paste0("trigger_ICU_timeline_", fname, ".png"),
      plot = pplot, path = outdir, width = 12, height = 8, device = "png"
    )

    if (savePDF) {
      ggsave(paste0("trigger_ICU_timeline_", fname, ".pdf"),
        plot = pplot, path = outdir, width = 12, height = 8, device = "pdf"
      )
    }
  }


  return(pplot)
}

f_plot_separateTrajectories <- function(df, reg = 1, samples = NULL, SAVE = T) {
  library(dplyr)
  library(zoo)

  if (is.null(samples)) samples <- unique(df$sample_num)

  for (samp in samples) {
    testdat <- subdat %>%
      filter(region == reg) %>%
      filter(sample_num %in% c(samp)) %>%
      filter((timeBeforeTrigger >= -28 & timeBeforeTrigger <= -21)) %>%
      arrange(region, scen_num, sample_num, date) # %>%
    # mutate(critical_det=rollapply(critical_det,7,mean,align='right',fill=NA))

    testdat2 <- subdat %>%
      filter(region == reg) %>%
      filter(sample_num %in% c(samp)) %>%
      arrange(region, scen_num, sample_num, date) # %>%
    # mutate(critical_det=rollapply(critical_det,7,mean,align='right',fill=NA))


    pplot <- ggplot(data = testdat) +
      geom_line(data = testdat2, aes(
        x = date, y = critical_det,
        group = capacity_multiplier_fct
      ), col = "grey", stat = "identity", size = 1.3) +
      geom_area(data = testdat2, aes(
        x = date, y = critical_det,
        group = capacity_multiplier_fct
      ), fill = "grey", stat = "identity", alpha = 0.3, size = 1.3) +
      geom_vline(xintercept = c(-Inf, Inf)) +
      # geom_hline(yintercept = c(-Inf)) +
      geom_hline(aes(yintercept = icu_available), linetype = "dashed") +
      geom_hline(aes(yintercept = icu_available * 0.875), linetype = "dashed") +
      geom_hline(aes(yintercept = icu_available * 0.75), linetype = "dashed") +
      geom_hline(aes(yintercept = icu_available * 0.625), linetype = "dashed") +
      geom_hline(aes(yintercept = icu_available * 0.5), linetype = "dashed") +
      geom_vline(data = testdat2, aes(xintercept = triggerDate, group = capacity_multiplier_fct), col = "black", stat = "identity") +
      geom_line(aes(
        x = date, y = critical_det, col = as.factor(capacity_multiplier_fct),
        group = capacity_multiplier
      ), stat = "identity", size = 1.3) +
      facet_wrap(~capacity_multiplier_fct, ncol = 1, strip.position = "right") +
      labs(color = "Capacity thresholds") +
      scale_color_viridis_d(direction = -1) +
      customThemeNoFacet

    if (SAVE) {
      ggsave(paste0("trigger_ICU_timeline_", fname, "_sample_", samp, ".png"),
        plot = pplot, path = file.path(outdir, paste0("covid_region_", reg, "_samples")), width = 12, height = 8, device = "png"
      )
    }
  }
}


f_slope_trigger_plot <- function(df, selected_exps = exp_names_crit, SAVE = T) {
  library(tidyverse)
  library(lubridate)
  library(broom)


  subdat <- subset(df, exp_name %in% selected_exps)
  table(subdat$exp_name)

  reg <- c(1:11)
  testdatCapacity <- subdat %>%
    filter(region %in% reg) %>%
    filter(trigger == 1) %>%
    group_by(region, scen_num, sample_num, capacity_multiplier, exp_name) %>%
    filter(critical_det == max(critical_det)) %>%
    filter(date == min(date)) %>%
    mutate(aboveCapacity = ifelse(critical_det > icu_available, "yes", "no")) %>%
    select(region, scen_num, sample_num, capacity_multiplier, aboveCapacity, exp_name)


  testdat <- subdat %>%
    filter(region %in% reg) %>%
    filter(trigger == 1) %>%
    group_by(region, scen_num, sample_num, capacity_multiplier, exp_name) %>%
    filter((timeBeforeTrigger >= -28 & timeBeforeTrigger <= -21)) %>%
    group_by(region, scen_num, sample_num, capacity_multiplier, exp_name) %>%
    nest() %>%
    mutate(model = map(data, ~ lm(critical_det ~ date, data = .x) %>% tidy())) %>%
    unnest(model) %>%
    filter(term == "date")

  testdat <- subdat %>%
    filter(region %in% reg) %>%
    filter(trigger == 1) %>%
    group_by(region, scen_num, sample_num, capacity_multiplier, exp_name) %>%
    filter((timeBeforeTrigger >= -28 & timeBeforeTrigger <= -21)) %>%
    group_by(region, scen_num, sample_num, capacity_multiplier, exp_name) %>%
    nest() %>%
    mutate(model = map(data, ~ lm(critical_det ~ date, data = .x) %>% tidy())) %>%
    unnest(model) %>%
    filter(term == "date")

  testdat <- testdat %>% left_join(testdatCapacity, by = c("region", "scen_num", "sample_num", "capacity_multiplier", "exp_name"))


  pplot <- ggplot(data = testdat) +
    geom_hline(yintercept = 0) +
    # geom_boxplot(aes(y = estimate, x = capacity_multiplier, col = as.factor(aboveCapacity), group = interaction(capacity_multiplier, aboveCapacity)), position = "dodge") +
    geom_point(aes(y = estimate, x = capacity_multiplier, col = as.factor(aboveCapacity), group = interaction(aboveCapacity, exp_name)), position = "dodge") +
    #    geom_point(aes(y= estimate, x = capacity_multiplier, col = as.factor(aboveCapacity),shape=exp_name, group = interaction(aboveCapacity, exp_name)), position = "dodge") +
    coord_flip() +
    labs(
      title = "all reopening scenarios",
      subtitle = "",
      y = "slope of critical detected 3 weeks preceeding capacity trigger",
      x = "capacity trigger",
      color = "above capacity"
    ) +
    facet_wrap(~region, scales = "free_x") +
    customThemeNoFacet +
    scale_color_manual(values = c("deepskyblue3", "orange"))

  if (SAVE) {
    fname <- "all_scens"
    ggsave(paste0("slope_trigger_capacity_", fname, ".png"),
      plot = pplot, path = file.path(outdir), width = 12, height = 8, device = "png"
    )
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
## -------------------------------
### COmbine data
## -------------------------------
datList <- list()
for (exp_name in exp_names_crit) {

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


## -------------------------------
## Generate plots
## -------------------------------
for (exp_name in exp_names_crit) {

  # exp_name <- exp_names_crit[2]
  print(exp_name)

  fname <- gsub(paste0(simdate, "_IL_critdet_"), "", exp_name)
  fname <- gsub(paste0(simdate, "_IL_hospdet_"), "", fname)
  fname <- gsub("_TriggeredRollback", "", fname)

  reopenLabel <- paste0(gsub("perc", "", gsub("reopen", "", fname)), "% reopening scenario")

  selectedEXP <- exp_name
  subdat <- df.exp %>%
    as.data.frame() %>%
    filter(exp_name %in% selectedEXP)

  ### Descriptives
  table(subdat$exp_name)
  summary(subdat$date)
  tapply(subdat$timeBeforeTrigger, subdat$trigger, summary)
  table(subdat$trigger, subdat$region)
  table(subdat$trigger, subdat$changedKi)

  f_plot_prepostTrigger(subdat)

  for (reg in c(1:11)) {
    f_plot_separateTrajectories()
  }
}

f_slope_trigger_plot(df = df.exp, selected_exps = exp_names_crit, SAVE = T)
