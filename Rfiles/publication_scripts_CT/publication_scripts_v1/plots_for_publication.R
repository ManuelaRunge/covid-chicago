### =======================================================
#### Additional plots for contact tracing simulations
### =======================================================

pdfdir <- "C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/project_notes/publications/covid_model_IL/pdfs_ais/"

#### Plots edited for publication
library(tidyverse)
library(cowplot)
library(scales)
library(viridis)
library(data.table)

theme_set(theme_cowplot())

# setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("ct_analysis/helper_functions_CT.R")
# region_cols <- c()
restoreRegion_cols <- c("Central" = "red2", "Northcentral" = "dodgerblue3", "Northeast" = "chartreuse4", "Southern" = "orchid4")

startdate <- "2020-06-15"
stopdate <- "2020-12-30"
reopen <- c(0, 0.05, 0.1)
customTheme <- f_getCustomTheme()
ct_startdate <- as.Date("2020-07-30")

## ================================================
###  Figure 1a Load true data and plot over time
## ================================================
f_data_plot_emr <- function() {
  emresource <- read.csv("C:/Users/mrm9534/Box/NU-malaria-team/data/covid_IDPH/Corona virus reports/emresource_by_region.csv") %>%
    dplyr::mutate(
      date_of_extract = as.Date(date_of_extract),
      month = month(date_of_extract),
      week = week(date_of_extract),
      suspected_and_confirmed_covid_icu = suspected_covid_icu + confirmed_covid_icu
    ) %>%
    dplyr::rename(
      date = date_of_extract,
      region = covid_region
    ) %>%
    f_addRestoreRegion() %>%
    filter(!is.na(restore_region)) %>%
    dplyr::select(
      date, month, week, restore_region, region, suspected_and_confirmed_covid_icu,
      confirmed_covid_deaths_prev_24h, confirmed_covid_icu, covid_non_icu
    )

  pplot <- emresource %>%
    dplyr::group_by(date, month, week, restore_region) %>%
    dplyr::summarize(
      suspected_and_confirmed_covid_icu = sum(suspected_and_confirmed_covid_icu),
      confirmed_covid_deaths_prev_24h = sum(confirmed_covid_deaths_prev_24h),
      confirmed_covid_icu = sum(confirmed_covid_icu),
      covid_non_icu = sum(covid_non_icu)
    ) %>%
    dplyr::group_by(month, week, restore_region) %>%
    dplyr::summarize(
      date = max(date),
      suspected_and_confirmed_covid_icu = mean(suspected_and_confirmed_covid_icu),
      confirmed_covid_deaths_prev_24h = mean(confirmed_covid_deaths_prev_24h),
      confirmed_covid_icu = mean(confirmed_covid_icu),
      covid_non_icu = mean(covid_non_icu)
    ) %>%
    ggplot() +
    theme_cowplot() +
    geom_line(aes(x = date, y = suspected_and_confirmed_covid_icu, group = restore_region, col = restore_region), size = 1.3) +
    scale_color_manual(values = restoreRegion_cols) +
    scale_y_log10() +
    scale_x_date(lim = c(as.Date("2020-03-01"), as.Date("2020-08-01")))


  ggsave(paste0("emresource_timeline", ".pdf"),
    plot = pplot, path = file.path(simulation_output, exp_name), width = 8, height = 4, device = "pdf"
  )
}

f_data_plot_LL <- function() {
  emresource <- read.csv("C:/Users/mrm9534/Box/NU-malaria-team/data/covid_IDPH/Cleaned Data/200727_jg_aggregated_covidregion.csv") %>%
    dplyr::mutate(
      date = as.Date(date),
      month = month(date),
      week = week(date)
    ) %>%
    dplyr::rename(region = covid_region) %>%
    f_addRestoreRegion() %>%
    filter(!is.na(restore_region)) %>%
    dplyr::select(date, month, week, restore_region, region, cases, deaths, admissions)

  pplot <- emresource %>%
    dplyr::group_by(date, month, week, restore_region) %>%
    dplyr::summarize(
      cases = sum(cases),
      deaths = sum(deaths),
      admissions = sum(admissions)
    ) %>%
    dplyr::group_by(month, week, restore_region) %>%
    dplyr::summarize(
      date = max(date),
      cases = mean(cases),
      deaths = mean(deaths),
      admissions = mean(admissions)
    ) %>%
    ggplot() +
    theme_cowplot() +
    geom_line(aes(x = date, y = deaths, group = restore_region, col = restore_region), size = 1.3) +
    scale_color_manual(values = restoreRegion_cols) +
    scale_y_log10() +
    scale_x_date(lim = c(as.Date("2020-03-01"), as.Date("2020-08-01")))


  ggsave(paste0("LL_deaths_timeline", ".pdf"),
    plot = pplot, path = file.path(simulation_output, exp_name), width = 8, height = 4, device = "pdf"
  )
}


## ================================================
###  Figure 1b  Parameter figure and transmission over time
## ================================================

f_parameter_figure <- function(exp_name) {
  trajectoriesDat <- read.csv(file.path(simulation_output, exp_name, "trajectoriesDat.csv"))

  ### Note use Ki_EMS-  or Ki_EMS.   not Ki_EMS_  (time varying vs input parameter)
  colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))

  paramvars <- c(
    paste0("backtonormal_multiplier_1_", emsname, c(1:11)),
    paste0("Ki_t_", emsname, c(1:11))
  )

  keepvars <- c("time", "startdate", paramvars)


  paramDat <- trajectoriesDat %>%
    dplyr::select(keepvars) %>%
    dplyr::mutate(date = as.Date(startdate) + time) %>%
    pivot_longer(cols = -c("time", "date", "startdate"), names_to = "name") %>%
    separate(name, into = c("param", "region"), sep = "_EMS_") %>%
    dplyr::mutate(
      region = as.numeric(region),
      exp_name = exp_name
    ) %>%
    dplyr::group_by(date, region, param, exp_name) %>%
    dplyr::summarize(value = mean(value, na.rm = TRUE)) %>%
    f_addRestoreRegion() %>%
    pivot_wider(names_from = "param", values_from = "value")

  paramDat <- data.table(paramDat, key = c("region", "exp_name", "restore_region"))
  paramDat[, social_multiplier := (1 - (Ki_t[date >= as.Date("2020-06-01") & date <= as.Date("2020-06-02")] /
    Ki_t[date >= as.Date("2020-03-15") & date <= as.Date("2020-03-16")])),
  by = c("region", "exp_name", "restore_region")
  ]

  paramDat <- as.data.frame(paramDat)
  paramDat <- paramDat %>%
    pivot_longer(cols = c("Ki_t", "backtonormal_multiplier_1", "social_multiplier"), names_to = "param")

  paramDatRR <- paramDat %>%
    dplyr::group_by(restore_region, date, param, exp_name) %>%
    dplyr::summarize(value = mean(value))


  if (timeVarying == FALSE) {
    pplot <- paramDat %>%
      filter(date >= as.Date("2020-08-01") & date <= as.Date("2020-08-02")) %>%
      ggplot() +
      theme_cowplot() +
      geom_bar(aes(x = as.factor(region), y = value, group = region), stat = "identity", size = 1.3, position = "dodge") +
      scale_color_viridis(discrete = TRUE) +
      labs(
        y = gsub("_", " ", paramname), # "% relaxation"
        title = "",
        # subtitle = "Estimated relaxation of shelter-in-place polices\n (reopening 21st June)",
        subtitle = "",
        x = "region"
      ) +
      customThemeNoFacet +
      scale_y_continuous(expand = c(0, 0)) +
      facet_grid(param ~ restore_region)

    pplot <- paramDatRR %>%
      filter(!(param %in% c("Ki_t", "d_Sym_t")) & date >= as.Date("2020-08-01") & date <= as.Date("2020-08-02")) %>%
      mutate(param = ifelse(param == "social_multiplier", "\nReduction during lockdown\n", "\nIncrease during reopening\n")) %>%
      ggplot(aes(x = restore_region, y = value, label = round(value * 100, 1), fill = restore_region)) +
      theme_cowplot() +
      geom_bar(stat = "identity", size = 1.3, position = "dodge", width = 0.7) +
      geom_text(fill = "white", vjust = 2, size = 6) +
      # geom_line(aes(x = date, y = value, col = as.factor(restore_region), group = restore_region), size = 1.3) +
      scale_fill_manual(values = restoreRegion_cols) +
      labs(
        y = "",
        subtitle = "", # Estimated change in transmission intensity\n
        title = "",
        x = "",
        col = ""
      ) +
      facet_wrap(~param, scales = "free", ncol = 1) +
      theme(
        legend.position = "None",
        strip.text.x = element_text(size = 18, face = "bold"),
        strip.text.y = element_text(size = 18, face = "bold"),
        strip.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 16)
      ) +
      scale_y_continuous(expand = c(0, 0))

    ggsave(paste0("Transmission_multiplier", ".pdf"),
      plot = pplot, path = file.path(simulation_output, exp_name), width = 10, height = 10, device = "pdf"
    )
  }




  if (timeVarying == TRUE) {
    pplot <- paramDat %>%
      filter(param %in% c("Ki_t", "d_Sym_t") & date <= as.Date("2020-09-02")) %>%
      ggplot() +
      theme_cowplot() +
      geom_line(aes(x = date, y = value, col = as.factor(restore_region), group = region), size = 1.3) +
      scale_color_manual(values = restoreRegion_cols) +
      labs(
        y = gsub("_", " ", paramname),
        subtitle = "", # Estimated change in transmission intensity\n
        title = "",
        x = "",
        col = "covid region"
      ) +
      customThemeNoFacet +
      scale_y_continuous(expand = c(0, 0)) +
      facet_grid(param ~ restore_region)

    KIplot <- paramDatRR %>%
      filter(param %in% c("Ki_t") & date <= as.Date("2020-07-15")) %>%
      ggplot() +
      theme_cowplot() +
      geom_line(aes(x = date, y = value, col = as.factor(restore_region), group = restore_region), size = 1.3) +
      scale_color_manual(values = restoreRegion_cols) +
      labs(
        y = "Transmission rate",
        subtitle = "", # Estimated change in transmission intensity\n
        title = "",
        x = "",
        col = "covid region"
      ) +
      customThemeNoFacet +
      theme(legend.position = "None") +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_date(lim = c(as.Date("2020-03-01"), as.Date("2020-08-01")))

    ggsave(paste0("Ki_timeline", ".pdf"),
      plot = KIplot, path = file.path(simulation_output, exp_name), width = 10, height = 6, device = "pdf"
    )
  }
}


### Load trajectories Dat
exp_name <- "20200731_IL_reopen_counterfactual"
source("load_paths.R")
simulation_output <- file.path(simulation_output, "contact_tracing/20200731/")
f_parameter_figure(exp_name)
source("load_paths.R")


## ================================================
### Figure 2 SImulation:  Baseline and reopening
## ================================================
f_getPredDat <- function(exp_name) {
  if (!(file.exists(file.path(simulation_output, exp_name, "predDate.csv")))) {
    trajectoriesDat <- read.csv(file.path(expDIR, "trajectoriesDat.csv"))
    unique(trajectoriesDat$reopening_multiplier_4)


    colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))


    ### per restore region
    if ("deaths_southern" %in% colnames(trajectoriesDat)) region_names <- c("southern", "central", "northcentral", "northeast")
    if (!("deaths_southern" %in% colnames(trajectoriesDat))) region_names <- paste0("EMS-", c(1:11))
    trajectoriesDat$Ki_t_southern <- (trajectoriesDat$Ki_t_EMS_4 + trajectoriesDat$Ki_t_EMS_5) / 2
    trajectoriesDat$Ki_t_central <- (trajectoriesDat$Ki_t_EMS_3 + trajectoriesDat$Ki_t_EMS_6) / 2
    trajectoriesDat$Ki_t_northcentral <- (trajectoriesDat$Ki_t_EMS_7 + trajectoriesDat$Ki_t_EMS_8 + trajectoriesDat$Ki_t_EMS_9 + trajectoriesDat$Ki_t_EMS_10 + trajectoriesDat$Ki_t_EMS_11) / 5
    trajectoriesDat$Ki_t_northeast <- (trajectoriesDat$Ki_t_EMS_4 + trajectoriesDat$Ki_t_EMS_5) / 2


    paramvars <- c(
      paste0("Ki_t_", region_names),
      paste0("deaths_", region_names),
      paste0("hosp_cumul_", region_names),
      paste0("hosp_det_", region_names),
      paste0("hospitalized_", region_names),
      paste0("infected_", region_names),
      paste0("deaths_det_", region_names),
      paste0("prevalence_", region_names),
      paste0("crit_cumul_", region_names),
      paste0("critical_", region_names)
    )


    keepvars <- c("time", "startdate", "scen_num", "reopening_multiplier_4", paramvars)


    predDat <- trajectoriesDat %>%
      dplyr::select(keepvars) %>%
      dplyr::mutate(date = as.Date(startdate) + time) %>%
      pivot_longer(cols = -c("time", "date", "startdate", "scen_num", "reopening_multiplier_4"), names_to = "name") %>%
      dplyr::mutate(
        name = gsub("_cumul_", ".cumul_", name),
        name = gsub("_det_", ".det_", name),
        name = gsub("Ki_t_", "Ki.t_", name)
      ) %>%
      separate(name, into = c("param", "restore_region"), sep = "_") %>%
      dplyr::mutate(
        param = gsub("[.]", "_", param)
      ) %>%
      dplyr::mutate(exp_name = exp_name)

    table(predDat$param)

    predDat <- predDat %>%
      dplyr::group_by(date, restore_region, param, exp_name, reopening_multiplier_4) %>%
      dplyr::summarize(
        mean.val = mean(value, na.rm = TRUE),
        sd.val = sd(value, na.rm = TRUE),
        n.val = n(),
      ) %>%
      dplyr::mutate(
        se.val = sd.val / sqrt(n.val),
        lower.ci.val = mean.val - qt(1 - (0.05 / 2), n.val - 1) * se.val,
        upper.ci.val = mean.val + qt(1 - (0.05 / 2), n.val - 1) * se.val
      )


    predDat$restore_region <- str_to_sentence(predDat$restore_region)
    # write.csv(predDat, file.path(simulation_output, exp_name, "predDat.csv"), row.names = FALSE)
    save(predDat, file = file.path(simulation_output, exp_name, "predDat.Rdata"))
  }


  if ((file.exists(file.path(expDIR, "predDate.csv")))) predDat <- load(file.path(expDIR, "predDat.Rdata"))

  # table( predDat$restore_region,  predDat$param)
  return(predDat)
}

#### SHow reopen

f_simulationTimelne_counterfactual <- function(df = predDat, expDIR, baselineDate = "2020-07-21") {
  customTheme <- f_getCustomTheme()

  df <- data.table(df, key = c("date", "restore_region", "param", "exp_name"))
  df[, additionalPred_mean.val := mean.val - mean.val[reopening_multiplier_4 == 0], by = c("date", "restore_region", "param", "exp_name")]
  df[, additionalPredperc_mean.val := (mean.val - mean.val[reopening_multiplier_4 == 0]) / mean.val[reopening_multiplier_4 == 0], by = c("date", "restore_region", "param", "exp_name")]


  df %>%
    dplyr::filter(date >= as.Date("2020-12-30") & date <= as.Date("2020-12-31")) %>%
    dplyr::group_by(restore_region, reopening_multiplier_4, param) %>%
    dplyr::summarize(additionalPredperc_mean.val = mean(additionalPredperc_mean.val)) %>%
    pivot_wider(names_from = "param", values_from = "additionalPredperc_mean.val")


  #### ---------------------------
  ###  Hospitalizations by restore region timeline
  #### ---------------------------

  plotdat <- df %>%
    dplyr::mutate(geography_name = tolower(restore_region)) %>%
    left_join(capacity, by = "geography_name") %>%
    dplyr::filter(param %in% c("critical") & date >= as.Date("2020-03-01") & date <= as.Date("2020-12-30"))


  pplot <- ggplot(data = plotdat) +
    # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_line(aes(x = date, y = mean.val, col = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), size = 1.3) +
    geom_line(data = subset(plotdat, date <= as.Date(baselineDate)), aes(x = date, y = mean.val, group = reopening_multiplier_4), col = "black", size = 1.3) +
    geom_hline(aes(yintercept = critical), linetype = "dashed", col = "grey", size = 1.7) +
    scale_y_continuous(labels = function(x) x / 1000, expand = c(0, 0)) + # label = comma
    scale_x_date(date_breaks = "60 days", date_labels = "%b", expand = c(0, 0)) +
    scale_color_brewer(palette = "Dark2") +
    customTheme +
    labs(x = "") +
    theme(legend.position = "None") +
    labs(y = "Cases requiring ICU beds\nper 1000 population") +
    facet_wrap(~restore_region, scales = "free_y", ncol = 1)

  ggsave(paste0("reopening_scenarios_restoreRegions", ".pdf"),
    plot = pplot, path = file.path(expDIR), width = 5, height = 7.5, device = "pdf"
  )
  ggsave(paste0("reopening_scenarios_restoreRegions", ".pdf"),
    plot = pplot, path = file.path(pdfdir), width = 5, height = 7.5, device = "pdf"
  )



  rm(pplot, plotdat)


  #### ---------------------------
  ###  Different outcome channels by timeline
  #### ---------------------------
  capacity_long <- capacity %>% pivot_longer(cols = -c("geography_name"), names_to = "param", values_to = "capacity")
  capacity_long <- capacity_long %>%
    filter(param != "ventilators") %>%
    mutate(param = case_when(
      param == "hospitalized" ~ "hosp_cumul",
      param == "critical" ~ "crit_cumul"
    )) %>%
    rbind(capacity_long)



  plotdat <- df %>%
    dplyr::mutate(geography_name = tolower(restore_region)) %>%
    dplyr::left_join(capacity_long, by = c("geography_name", "param")) %>%
    dplyr::filter(date >= as.Date("2020-03-01") & date <= as.Date("2020-12-31")) %>%
    dplyr::group_by(date, reopening_multiplier_4, param) %>%
    dplyr::summarize(
      capacity = sum(capacity),
      mean.val = sum(mean.val),
      upper.ci.val = sum(upper.ci.val),
      lower.ci.val = sum(lower.ci.val)
    ) %>%
    dplyr::filter(param %in% c("infected", "hospitalized", "critical", "deaths"))

  plotdat$param <- factor(plotdat$param,
    levels = c("infected", "hospitalized", "critical", "deaths"),
    labels = c("COVID-19 infections", "Cases requiring hospitalization", "Cases requiring intensive care", "Total deaths")
  )

  customTheme <- f_getCustomTheme()

  pplot <- ggplot(data = plotdat) +
    # geom_errorbar(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_line(aes(x = date, y = mean.val / 1000, col = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), size = 1.3) +
    geom_line(data = subset(plotdat, date <= as.Date(baselineDate)), aes(x = date, y = mean.val / 1000, group = reopening_multiplier_4), col = "black", size = 1.3) +
    geom_hline(aes(yintercept = capacity / 1000), linetype = "dashed", col = "grey", size = 1.7) +
    geom_hline(yintercept = c(-Inf)) +
    # geom_vline(xintercept =c( -Inf, Inf)) +
    scale_y_continuous(label = comma, expand = c(0, 0)) +
    scale_x_date(date_breaks = "30 days", date_labels = "%b", expand = c(0, 0)) +
    scale_color_brewer(palette = "Dark2") +
    customTheme +
    labs(x = "") +
    theme(legend.position = "None") +
    labs(y = "Predicted population per 1000") +
    facet_wrap(~param, scales = "free_y", ncol = 2)

  ggsave(paste0("reopening_scenarios_channels", ".pdf"),
    plot = pplot, path = file.path(expDIR), width = 12, height = 7, device = "pdf"
  )
  ggsave(paste0("reopening_scenarios_channels", ".pdf"),
    plot = pplot, path = file.path(pdfdir), width = 12, height = 7, device = "pdf"
  )
  rm(pplot, plotdat)


  #### ---------------------------
  ### Increase in critical cases
  #### ---------------------------

  pplot <- df %>%
    mutate(geography_name = tolower(restore_region)) %>%
    left_join(capacity, by = "geography_name") %>%
    filter(param %in% c("crit_cumul") & date >= as.Date("2020-12-30") & date <= as.Date("2020-12-31")) %>%
    ggplot() +
    theme_cowplot() +
    # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_bar(aes(
      x = reopening_multiplier_4, y = mean.val,
      fill = as.factor(reopening_multiplier_4),
      group = reopening_multiplier_4
    ), stat = "identity", size = 1.3) +
    facet_wrap(~restore_region, scales = "free_y", nrow = 2) +
    scale_y_continuous(labels = function(x) x / 1000, expand = c(0, 0)) +
    scale_fill_brewer(palette = "Dark2") +
    customTheme +
    labs(x = "") +
    theme(legend.position = "None") +
    labs(y = "Cumulative ICU bed demand by Dec 2020\n per 1000 population")


  ggsave(paste0("reopening_scenarios_critcumulDec2020", ".pdf"),
    plot = pplot, path = file.path(expDIR), width = 8, height = 6, device = "pdf"
  )


  #### ILLINOIS

  criticalIncrease <- df %>%
    dplyr::mutate(geography_name = tolower(restore_region)) %>%
    dplyr::left_join(capacity, by = "geography_name") %>%
    dplyr::filter(param %in% c("crit_cumul") & date >= as.Date("2020-12-30") & date <= as.Date("2020-12-31")) %>%
    dplyr::group_by(reopening_multiplier_4) %>%
    dplyr::summarize(percIncr = paste0(round(mean(additionalPredperc_mean.val) * 100, 0), " %"))

  ppred <- df %>%
    dplyr::mutate(geography_name = tolower(restore_region)) %>%
    # dplyr::left_join(capacity, by = "geography_name") %>%
    filter(param %in% c("crit_cumul") & date >= as.Date("2020-12-30") & date <= as.Date("2020-12-31")) %>%
    dplyr::group_by(reopening_multiplier_4) %>%
    dplyr::summarize(mean.val = sum(mean.val)) %>%
    dplyr::left_join(subset(criticalIncrease, reopening_multiplier_4 != 0), by = "reopening_multiplier_4") %>%
    ggplot() +
    theme_cowplot() +
    # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_bar(aes(
      x = reopening_multiplier_4, y = mean.val,
      fill = as.factor(reopening_multiplier_4),
      group = reopening_multiplier_4
    ), stat = "identity", size = 1.3) +
    geom_text(aes(
      x = reopening_multiplier_4, y = mean.val, label = percIncr,
      col = as.factor(reopening_multiplier_4)
    ), size = 5, vjust = -1) +
    scale_y_continuous(labels = function(x) x / 1000, expand = c(0, 0), lim = c(0, 200000)) +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") +
    customTheme +
    labs(x = "") +
    theme(
      legend.position = "None",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(y = "Cumulative ICU bed demand\n per 1000 population")


  ggsave(paste0("reopening_scenarios_IL_critcumulDec2020", ".pdf"),
    plot = ppred, path = file.path(pdfdir), width = 5, height = 2.5, device = "pdf"
  )


  ### PER RESTORE REGION
  ppred <- df %>%
    mutate(geography_name = tolower(restore_region)) %>%
    left_join(capacity, by = "geography_name") %>%
    filter(param %in% c("crit_cumul") & date >= as.Date("2020-12-30") & date <= as.Date("2020-12-31")) %>%
    ggplot() +
    theme_cowplot() +
    # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_bar(aes(
      x = reopening_multiplier_4,
      y = additionalPredperc_mean.val,
      fill = as.factor(reopening_multiplier_4),
      group = reopening_multiplier_4
    ), stat = "identity", size = 1.3) +
    facet_wrap(~restore_region, scales = "free_y", nrow = 2) +
    scale_y_continuous(label = comma, expand = c(0, 0)) +
    scale_fill_brewer(palette = "Dark2") +
    customThemeNoFacet +
    labs(x = "") +
    theme(legend.position = "None") +
    labs(y = "Cumulative ICU bed demand by Dec 2020\n per 1000 population")


  ggsave(paste0("reopening_scenarios_criticalDec2020_perc", ".pdf"),
    plot = ppred, path = file.path(expDIR), width = 8, height = 6, device = "pdf"
  )


  #### ---------------------------
  ### Transmission Rate Timeline
  #### ---------------------------
  KIplot <- TRUE
  if (KIplot) {
    ### Note use Ki_EMS-  or Ki_EMS.   not Ki_EMS_  (time varying vs input parameter)
    plotdat <- df %>%
      dplyr::filter(param == "Ki_t") %>%
      dplyr::group_by(date, reopening_multiplier_4, param) %>%
      dplyr::summarize(mean.val = mean(mean.val)) %>%
      filter(date >= as.Date("2020-03-01") & date <= as.Date("2020-09-31"))

    pplot <- ggplot(data = plotdat) +
      theme_cowplot() +
      geom_line(aes(x = date, y = mean.val, col = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), size = 1.3) +
      geom_line(
        data = subset(plotdat, reopening_multiplier_4 == 0 & date <= as.Date(baselineDate)),
        aes(x = date, y = mean.val, group = reopening_multiplier_4), col = "black", size = 1.3
      ) +
      # scale_color_viridis(discrete = TRUE) +
      scale_color_brewer(palette = "Dark2") +
      labs(
        y = gsub("_", " ", "Transmission rate"),
        subtitle = "", # Estimated change in transmission intensity\n
        title = "",
        x = "",
        col = "reopening"
      ) +
      customThemeNoFacet +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_date(breaks = "30 days", labels = date_format("%b"), expand = c(0, 0))


    ggsave(paste0("reopening_scenarios", ".pdf"),
      plot = pplot, path = file.path(expDIR), width = 8, height = 4, device = "pdf"
    )
  }


  #### ---------------------------
  ### Rt Timeline
  #### ---------------------------
  Rtplot <- TRUE
  if (Rtplot) {
    startdate <- as.Date("2020-02-13")
    weekwindow <- 13
    scendat <- read.csv(file.path(simulation_output, exp_name, "sampled_parameters.csv")) %>%
      dplyr::select(scen_num, reopening_multiplier_4)


    #### Aggregate scen nums as well as regions
    plotdat <- read.csv(file.path(simulation_output, exp_name, "estimatedRt/EMS_combined_estimated_Rt.csv")) %>%
      # dplyr::mutate(date = as.Date(startdate + t_start + weekwindow)) %>%
      dplyr::mutate(date = as.Date(startdate + t_start)) %>%
      left_join(scendat, by = "scen_num") %>%
      dplyr::group_by(date, Date, reopening_multiplier_4) %>%
      dplyr::summarize(mean.val = mean(Mean)) %>%
      dplyr::filter(date >= as.Date("2020-06-01") & date <= as.Date("2020-12-31"))
    # dplyr::filter(Date >= as.Date("2020-06-01") & Date <= as.Date("2020-12-31"))

    pplot <- ggplot(data = plotdat) +
      theme_cowplot() +
      geom_line(aes(x = date, y = mean.val, col = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), size = 1.3) +
      geom_line(
        data = subset(plotdat, reopening_multiplier_4 == 0 & date <= as.Date(baselineDate)),
        aes(x = date, y = mean.val, group = reopening_multiplier_4), col = "black", size = 1.3
      ) +
      geom_hline(yintercept = 1) +
      scale_color_brewer(palette = "Dark2") +
      labs(
        y = expr(italic(R[t])),
        subtitle = "", # Estimated change in transmission intensity\n
        title = "",
        x = "",
        col = "reopening"
      ) +
      customTheme +
      theme(legend.position = "none") +
      scale_y_continuous() +
      scale_x_date(date_breaks = "30 days", date_labels = "%b", expand = c(0, 0))


    ggsave(paste0("reopening_scenarios_Rt", ".pdf"),
      plot = pplot, path = file.path(pdfdir), width = 5, height = 3.5, device = "pdf"
    )
  }

  return(deathsIncrease)
}

source("load_paths.R")
exp_name <- "20200731_IL_reopen_counterfactual"
expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)
predDat <- f_getPredDat(exp_name)
capacity <- load_capacity(selected_ems = tolower(unique(predDat$restore_region)))
f_simulationTimelne_counterfactual()


## ================================================
###  Figure 3 - HS scenarios - no contact tracing
## ================================================

if (combineScenarioDats) {
  
  f_getPredDat <- function(expDIR) {
    trajectoriesDat <- read.csv(file.path(expDIR, "trajectoriesDat_trim.csv"))
    unique(trajectoriesDat$reopening_multiplier_4)
    unique(trajectoriesDat$fraction_critical)

    region_names <- paste0("EMS-", c(1:11))

    colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))


    ### per restore region
    paramvars <- c(
      paste0("deaths_", region_names),
      paste0("hosp_cumul_", region_names),
      paste0("hosp_det_", region_names),
      paste0("hospitalized_", region_names),
      paste0("infected_", region_names),
      paste0("deaths_det_", region_names),
      paste0("prevalence_", region_names),
      paste0("critical_", region_names),
      paste0("crit_cumul_", region_names)
    )


    keepvars <- c("time", "startdate", "scen_num", "reopening_multiplier_4", paramvars)


    predDat <- trajectoriesDat %>%
      dplyr::select(keepvars) %>%
      dplyr::mutate(date = as.Date(startdate) + time) %>%
      pivot_longer(cols = -c("time", "date", "startdate", "scen_num", "reopening_multiplier_4"), names_to = "name") %>%
      dplyr::mutate(
        name = gsub("_cumul_", ".cumul_", name),
        name = gsub("_det_", ".det_", name)
      ) %>%
      separate(name, into = c("param", "region"), sep = "_") %>%
      dplyr::mutate(
        param = gsub("[.]", "_", param),
        exp_name = exp_name,
        region = gsub("EMS-", "", region)
      ) %>%
      f_addRestoreRegion() %>%
      dplyr::group_by(date, scen_num, restore_region, param, exp_name, reopening_multiplier_4) %>%
      dplyr::summarize(value = sum(value)) %>%
      dplyr::group_by(date, restore_region, param, exp_name, reopening_multiplier_4) %>%
      dplyr::summarize(
        mean.val = mean(value, na.rm = TRUE),
        sd.val = sd(value, na.rm = TRUE),
        n.val = n(),
      ) %>%
      dplyr::mutate(
        se.val = sd.val / sqrt(n.val),
        lower.ci.val = mean.val - qt(1 - (0.05 / 2), n.val - 1) * se.val,
        upper.ci.val = mean.val + qt(1 - (0.05 / 2), n.val - 1) * se.val
      )


    predDat$restore_region <- str_to_sentence(predDat$restore_region)

    return(predDat)
  }


  f_combineDat <- function(expDIR, scenname, Rt = FALSE) {
    if (Rt == FALSE) df <- f_getPredDat(expDIR) %>% mutate(scenario = scenname)
    if (Rt == TRUE) df <- read.csv(file.path(expDIR, "estimatedRt", "EMS_combined_estimated_Rt.csv")) %>% mutate(scenario = scenname)

    return(df)
  }


  exp_names <- c(
    "20200801_IL_reopen_TD", "20200731_IL_reopen_counterfactual",
    "20200801_IL_reopen_HS40TD", "20200801_IL_reopen_HS40",
    "20200801_IL_reopen_HS80TD", "20200801_IL_reopen_HS80"
  )
  dfList <- list()
  for (exp_name in exp_names) {
    print(exp_name)
    scenname <- gsub("20200801_IL_reopen_", "", exp_name)
    scenname <- gsub("20200731_IL_reopen_", "", exp_name)
    expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)

    df <- f_combineDat(expDIR, scenname, Rt = TRUE)
    dfList[[length(dfList + 1)]] <- df
  }

  source("load_paths.R")
  expDIR <- file.path(simulation_output, "contact_tracing/20200731/20200801_IL_reopen_TD/")
  predDat_TD <- f_getPredDat(expDIR) %>% mutate(scenario = "TDonly")
  RtDat_TD <- read.csv(file.path(expDIR, "estimatedRt", "EMS_combined_estimated_Rt.csv")) %>% mutate(scenario = "TDonly")


  expDIR <- file.path(simulation_output, "contact_tracing/20200731/20200731_IL_reopen_counterfactual/")
  predDat_counterfactual <- f_getPredDat(expDIR) %>% mutate(scenario = "counterfactual")
  RtDat_counterfactual <- read.csv(file.path(expDIR, "estimatedRt", "EMS_combined_estimated_Rt.csv")) %>% mutate(scenario = "counterfactual")


  expDIR <- file.path(simulation_output, "contact_tracing/20200731/20200801_IL_reopen_HS40/")
  predDat_HS40 <- f_getPredDat(expDIR) %>% mutate(scenario = "HS40")
  RtDat_HS40 <- read.csv(file.path(expDIR, "estimatedRt", "EMS_combined_estimated_Rt.csv")) %>% mutate(scenario = "HS40")

  expDIR <- file.path(simulation_output, "contact_tracing/20200731/20200801_IL_reopen_HS40TD/")
  predDat_HS40TD <- f_getPredDat(expDIR) %>% mutate(scenario = "HS40TD")
  RtDat_HS40TD <- read.csv(file.path(expDIR, "estimatedRt", "EMS_combined_estimated_Rt.csv")) %>% mutate(scenario = "HS40TD")

  expDIR <- file.path(simulation_output, "contact_tracing/20200731/20200801_IL_reopen_HS80/")
  predDat_HS80 <- f_getPredDat(expDIR) %>% mutate(scenario = "HS80")
  RtDat_HS80 <- read.csv(file.path(expDIR, "estimatedRt", "EMS_combined_estimated_Rt.csv")) %>% mutate(scenario = "HS80")

  expDIR <- file.path(simulation_output, "contact_tracing/20200731/20200801_IL_reopen_HS80TD/")
  predDat_HS80TD <- f_getPredDat(expDIR) %>% mutate(scenario = "HS80TD")
  RtDat_HS80TD <- read.csv(file.path(expDIR, "estimatedRt", "EMS_combined_estimated_Rt.csv")) %>% mutate(scenario = "HS80TD")


  predDatHS <- rbind(predDat_counterfactual, predDat_TD, predDat_HS40, predDat_HS40TD, predDat_HS80, predDat_HS80TD)
  RtDatHS <- rbind(RtDat_counterfactual, RtDat_TD, RtDat_HS40, RtDat_HS40TD, RtDat_HS80, RtDat_HS80TD)

  table(predDatHS$scenario)
  table(RtDatHS$scenario)

  if (SAVE) save(predDatHS, file = file.path(simulation_output, "contact_tracing/20200731/predDatHS.Rdata"))
  if (SAVE) save(RtDatHS, file = file.path(simulation_output, "contact_tracing/20200731/RtDatHS.Rdata"))
}


load(file.path(simulation_output, "contact_tracing/20200731/predDatHS.Rdata"))
capacity <- load_capacity(selected_ems = tolower(unique(predDatHS$restore_region)))

predDatHS <- predDatHS %>%
  mutate(date = as.character(date)) %>%
  mutate(date = as.Date(date))


customTheme <- f_getCustomTheme()

predDatHS$scenario_fct <- factor(predDatHS$scenario,
  levels = c("counterfactual", "HS40", "HS80", "TDonly", "HS40TD", "HS80TD"),
  labels = c(
    "current trend (comparison)",
    "increase detections of mild symptoms to 40%",
    "faster testing and isolation",
    "increase detections of mild symptoms to 80%",
    "increase detections of mild symptoms to 40%\n& faster testing and isolation",
    "increase detections of mild symptoms to 80%\n& faster testing and isolation"
  )
)


predplotDat <- predDatHS %>%
  mutate(geography_name = tolower(restore_region)) %>%
  left_join(capacity, by = "geography_name") %>%
  filter(param %in% c("critical") &
    date >= as.Date(startdate) & date <= as.Date(stopdate) &
    reopening_multiplier_4 %in% reopen) %>%
  group_by(date, param, scenario, scenario_fct, reopening_multiplier_4) %>%
  summarize(
    icu_available = sum(icu_available),
    mean.val = sum(mean.val)
  )

predplot <- predplotDat %>%
  ggplot() +
  theme_cowplot() +
  # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
  geom_line(aes(x = date, y = mean.val, col = scenario_fct, group = scenario), size = 1.3) +
  geom_hline(aes(yintercept = icu_available), linetype = "dashed", col = "black", size = 0.7) +
  geom_hline(aes(yintercept = 0)) +
  facet_grid(~reopening_multiplier_4, scales = "free") +
  customTheme +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "",
    y = "Cases requiring ICU beds\n per 1000 population",
    color = ""
  ) +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = function(x) x / 1000, expand = c(0, 0)) +
  scale_x_date(breaks = "30 days", labels = date_format("%b"), expand = c(0, 0))




#### For RT
load(file.path(simulation_output, "contact_tracing/20200731/RtDatHS.Rdata"))

RtDatHS <- RtDatHS %>%
  f_addRestoreRegion() %>%
  rename(reopening_multiplier_4 = grpvar) %>%
  group_by(Date, restore_region, scenario, reopening_multiplier_4) %>%
  summarize(mean.val = mean(Mean)) %>%
  mutate(capacity = 1)

RtDatHS$scenario_fct <- factor(RtDatHS$scenario,
  levels = c("counterfactual", "HS40", "HS80", "TDonly", "HS40TD", "HS80TD"),
  labels = c(
    "current trend (comparison)",
    "increase detections of mild symptoms to 40%",
    "faster testing and isolation",
    "increase detections of mild symptoms to 80%",
    "increase detections of mild symptoms to 40%\n& faster testing and isolation",
    "increase detections of mild symptoms to 80%\n& faster testing and isolation"
  )
)

rtplotdat <- RtDatHS %>%
  ungroup() %>%
  mutate(date = as.Date(Date)) %>%
  filter(date >= as.Date(startdate) & date <= as.Date(stopdate) &
    reopening_multiplier_4 %in% reopen) %>%
  group_by(date, scenario, scenario_fct, reopening_multiplier_4) %>%
  summarize(mean.val = mean(mean.val))

rtplot <- rtplotdat %>%
  ggplot() +
  theme_cowplot() +
  # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
  geom_line(aes(x = date, y = mean.val, col = scenario_fct, group = scenario), size = 1.3) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", col = "black", size = 0.7) +
  facet_grid(~reopening_multiplier_4, scales = "free") +
  customTheme +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "",
    y = expr(italic(R[t])),
    color = ""
  ) +
  theme(legend.position = "bottom") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(breaks = "30 days", labels = date_format("%b"), expand = c(0, 0))



predplot <- predplot + theme(legend.position = "none")
rtplot <- rtplot + theme(strip.text.x = element_text(color = "white"))
pplot <- plot_grid(predplot, rtplot, ncol = 1, rel_heights = c(1, 1.3))


ggsave(paste0("HS_scenarios_IL", ".pdf"),
  plot = pplot, path = file.path(pdfdir), width = 12, height = 8, device = "pdf"
)




## ------------------------------------------------
### Relative reduction barchart

### Relative reduction
### Relative reduction barchart
predplotDat <- data.table(predplotDat, key = c("date", "reopening_multiplier_4", "param"))
predplotDat[, mean_percRed := (mean.val[scenario == "counterfactual"] - mean.val) / mean.val[scenario == "counterfactual"], by = c("date", "reopening_multiplier_4", "param")]

pbar1 <- predplotDat %>%
  dplyr::group_by(scenario_fct) %>%
  dplyr::summarize(mean_percRed = mean(mean_percRed)) %>%
  ggplot() +
  theme_cowplot() +
  # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
  geom_bar(aes(x = scenario_fct, y = mean_percRed, fill = scenario_fct), show.legend = FALSE, stat = "identity", size = 1.3) +
  customTheme +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    x = "",
    y = "Relative reduction in cases (%)\n compared to current trend",
    color = ""
  ) +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = function(x) x * 100, expand = c(0, 0)) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggsave(paste0("HS_scenarios_barplot_IL", ".pdf"),
  plot = pbar1, path = file.path(pdfdir), width = 7, height = 4, device = "pdf"
)



pbar2 <- rtplotdat %>%
  dplyr::group_by(date, reopening_multiplier_4, scenario_fct) %>%
  dplyr::summarize(mean.val = mean(mean.val)) %>%
  dplyr::filter(date > as.Date(ct_startdate) & mean.val < 1 & reopening_multiplier_4 %in% reopen) %>%
  dplyr::group_by(reopening_multiplier_4, scenario_fct) %>%
  dplyr::summarize(mindate = min(date)) %>%
  dplyr::mutate(
    datediff = as.Date(mindate) - as.Date(ct_startdate),
    mthdiff = as.numeric(datediff / 30)
  ) %>%
  ggplot() +
  geom_bar(aes(
    x = scenario_fct, y = mthdiff, group = interaction(reopening_multiplier_4, scenario_fct), alpha = reopening_multiplier_4,
    fill = as.factor(scenario_fct)
  ), stat = "identity", position = "dodge", col = "azure3", show.legend = FALSE) +
  labs(x = "", y = expr("Time until " * italic(R[t]) * " is < 1 (month)")) +
  customTheme +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


ggsave(paste0("HS_scenarios_Rt_barplot_IL", ".pdf"),
  plot = pbar2, path = file.path(pdfdir), width = 7, height = 4, device = "pdf"
)

## ---------------------------
#### Explore boxplot
additionalPlotsExplored <- FALSE
if (additionalPlotsExplored) {
  load(file.path(simulation_output, "contact_tracing/20200731/RtDatHS.Rdata"))

  RtDatHS <- RtDatHS %>%
    f_addRestoreRegion() %>%
    rename(reopening_multiplier_4 = grpvar) %>%
    group_by(Date, region, restore_region, scenario, reopening_multiplier_4) %>%
    summarize(mean.val = mean(Mean)) %>%
    mutate(capacity = 1)

  RtDatHS$scenario_fct <- factor(RtDatHS$scenario,
    levels = c("counterfactual", "HS40", "HS80", "TDonly", "HS40TD", "HS80TD"),
    labels = c(
      "current trend (comparison)",
      "increase detections of mild symptoms to 40%",
      "faster testing and isolation",
      "increase detections of mild symptoms to 80%",
      "increase detections of mild symptoms to 40%\n& faster testing and isolation",
      "increase detections of mild symptoms to 80%\n& faster testing and isolation"
    )
  )

  rtplotdat <- RtDatHS %>%
    ungroup() %>%
    mutate(date = as.Date(Date)) %>%
    filter(date >= as.Date(startdate) & date <= as.Date(stopdate) &
      reopening_multiplier_4 %in% reopen) %>%
    group_by(date, region, restore_region, scenario, scenario_fct, reopening_multiplier_4) %>%
    summarize(mean.val = mean(mean.val))

  startRt <- rtplotdat %>%
    filter(date == as.Date("2020-06-15")) %>%
    dplyr::select(mean.val, region, restore_region, scenario, reopening_multiplier_4) %>%
    rename(startRt = mean.val)

  rtplotdat %>%
    left_join(startRt, by = c("scenario", "reopening_multiplier_4")) %>%
    mutate(mean_percRed = (startRt - mean.val) / startRt) %>%
    group_by(scenario_fct) %>%
    summarize(mean_percRed = mean(mean_percRed)) %>%
    ggplot() +
    theme_cowplot() +
    geom_boxplot(aes(x = scenario, y = mean_percRed, fill = scenario_fct), show.legend = FALSE, size = 1.3)


  pplot <- rtplotdat %>%
    ggplot() +
    theme_cowplot() +
    # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_line(aes(x = date, y = mean.val, col = scenario_fct, group = scenario), size = 1.3) +
    geom_hline(aes(yintercept = 1), linetype = "dashed", col = "black", size = 0.7) +
    facet_grid(reopening_multiplier_4 ~ region, scales = "free") +
    customTheme +
    scale_color_brewer(palette = "Dark2") +
    labs(
      x = "",
      y = expr(italic(R[t])),
      color = ""
    ) +
    theme(legend.position = "bottom") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_date(breaks = "30 days", labels = date_format("%b"), expand = c(0, 0))

  ggsave(paste0("HS_scenarios_perRegion", ".png"),
    plot = pplot, path = file.path(simulation_output, "contact_tracing", simdate), width = 31, height = 15, device = "png"
  )



  ##### Per restore region
  pplot <- predDatHS %>%
    mutate(geography_name = tolower(restore_region)) %>%
    left_join(capacity, by = "geography_name") %>%
    filter(param %in% c("critical") &
      date >= as.Date("2020-07-01") & date <= as.Date("2020-12-01")) %>%
    ggplot() +
    theme_cowplot() +
    # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_line(aes(x = date, y = mean.val, col = scenario_fct, group = scenario), size = 1.3) +
    geom_hline(aes(yintercept = critical), linetype = "dashed", col = "black", size = 0.7) +
    geom_hline(aes(yintercept = 0)) +
    facet_grid(restore_region ~ reopening_multiplier_4, scales = "free") +
    scale_y_continuous(labels = function(x) x / 1000, expand = c(0, 0)) +
    scale_color_brewer(palette = "Dark2") +
    customThemeNoFacet +
    labs(
      x = "",
      y = "Cases requiring ICU beds\n per 1000 population",
      color = ""
    ) +
    theme(legend.position = "bottom")

  ggsave(paste0("HS_scenarios_critical_perRestoreRegion", ".pdf"),
    plot = pplot, path = file.path(pdfdir), width = 12, height = 8, device = "pdf"
  )



  f_tempPlot <- function(reopen) {
    ppred <- predDatHS %>%
      mutate(geography_name = tolower(restore_region)) %>%
      left_join(capacity, by = "geography_name") %>%
      filter(reopening_multiplier_4 %in% reopen &
        param %in% c("critical") &
        date >= as.Date("2020-07-01") & date <= as.Date("2020-12-01")) %>%
      ggplot() +
      theme_cowplot() +
      # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
      geom_line(aes(x = date, y = mean.val, col = scenario_fct, group = scenario), size = 1.3) +
      geom_hline(aes(yintercept = critical), linetype = "dashed", col = "black", size = 0.7) +
      facet_wrap(~restore_region, scales = "free_y", nrow = 2) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_color_brewer(palette = "Dark2") +
      customThemeNoFacet +
      labs(
        x = "",
        y = "Cases requiring ICU beds",
        color = "",
        caption = paste0("reopening = ", reopen)
      ) +
      theme(legend.position = "bottom")


    ggsave(paste0("HS_scenarios_reopen", reopen, "_perRestoreRegion", ".png"),
      plot = ppred, path = file.path(simulation_output, "contact_tracing", simdate), width = 8, height = 6, device = "png"
    )
  }
  for (reopen in unique(predDatHS$reopening_multiplier_4)) {
    f_tempPlot(reopen)
  }
  ##### WHen is capacity reached and by how much
  # predDatHS %>% group_by(restore_region, param, scenario, reopening_multiplier_4 , scenario_fct) %>%
  filledAreaPlot <- FALSE
  if (filledAreaPlot) {

    ## Calculate reduction to counterfactual

    predDatHS <- data.table(predDatHS, key = c("date", "restore_region", "param", "reopening_multiplier_4"))
    predDatHS[, mean.valAverted := mean.val - mean.val["scenario" == "counterfactual"], by = c("date", "restore_region", "param", "reopening_multiplier_4")]

    predDatHS <- data.table(predDatHS, key = c("date", "restore_region", "param", "reopening_multiplier_4"))
    predDatHS[, mean.valAverted := mean.val - mean.val[scenario == "counterfactual"], by = c("date", "restore_region", "param", "reopening_multiplier_4")]


    tapply(predDatHS$mean.valAverted, predDatHS$scenario, summary)
    tapply(predDatHS$mean.valAverted, predDatHS$param, summary)



    predDatHS_wide <- predDatHS %>%
      select(date, mean.valAverted, reopening_multiplier_4, scenario, param, restore_region) %>%
      pivot_wider(names_from = "scenario", values_from = "mean.valAverted")

    
    popdat <- load_population() %>% rename(region=geography_name) 
    
    #### FILLED AREA PLOT< ALSO FOR RT
    predDatHS_wide %>%
      filter(param %in% c("critical") & reopening_multiplier_4 == 0 &
        date >= as.Date("2020-07-01") & date <= as.Date("2020-12-01")) %>%
      #left_join(popdat, by="region") %>%
      #mutate() %>%
      ggplot() +
      theme_cowplot() +
      geom_ribbon(aes(x = date, ymin = counterfactual, ymax = HS40), fill = "blue", size = 1, alpha = 0.3) +
      geom_line(aes(x = date, y = counterfactual), col = "blue", size = 1.3) +
      geom_ribbon(aes(x = date, ymin = HS40, ymax = HS40TD), size = 1, alpha = 0.3) +
      geom_line(aes(x = date, y = HS40), size = 1.3) +
      geom_ribbon(aes(x = date, ymin = HS40TD, ymax = HS80), fill = "red", size = 1, alpha = 0.3) +
      geom_line(aes(x = date, y = HS80), size = 1.3, col = "red") +
      geom_ribbon(aes(x = date, ymin = HS80, ymax = HS80TD), fill = "orange", size = 1, alpha = 0.3) +
      geom_line(aes(x = date, y = HS80TD), size = 1.3, col = "orange") +
      # geom_hline(aes(yintercept = 0)) +
      facet_grid(restore_region ~ reopening_multiplier_4, scales = "free") +
      scale_y_continuous(labels = function(x) x / 1000, expand = c(0, 0)) +
      #scale_y_continuous(expand = c(0, 0)) +
      scale_color_brewer(palette = "Dark2") +
      customThemeNoFacet +
      labs(
        x = "",
        y = "Cases requiring ICU beds\n per 1000 population",
        color = ""
      ) +
      theme(legend.position = "bottom")
  }


  #### BAR PLOTS
  barplots <- FALSE
  if (barplots) {
    scenario_cols <- c("0.15" = "#e7298a", "0.1" = "#807dba", "0.05" = "#fc4e2a", "0" = "#41ae76")

    ppredList <- list()
    predDatHS$scenarioLabel <- factor(predDatHS$scenario,
      levels = c("counterfactual", "HS40", "HS80", "TDonly", "HS40TD", "HS80TD"),
      labels = c(
        "counterfactual",
        "increased\ntesting\n(40%)", "increased\ntesting\n(80%)", "faster\ntesting",
        "increased and faster\ntesting\n(40%)", "increased and faster\ntesting\n(80%)"
      )
    )

    for (i in unique(predDatHS$reopening_multiplier_4)) {
      fillcolor <- as.character(scenario_cols[names(scenario_cols) == i])


      ppred <- predDatHS %>%
        dplyr::mutate(geography_name = tolower(restore_region)) %>%
        dplyr::left_join(capacity, by = "geography_name") %>%
        filter(reopening_multiplier_4 == i &
          param %in% c("hospitalized") &
          date >= as.Date("2020-07-01") & date <= as.Date("2020-12-31")) %>%
        dplyr::group_by(date, scenarioLabel) %>%
        dplyr::summarize(
          hospitalized = sum(hospitalized),
          mean.val = sum(mean.val)
        ) %>%
        dplyr::group_by(scenarioLabel) %>%
        dplyr::summarize(
          hospitalized = mean(hospitalized),
          mean.val = mean(mean.val)
        ) %>%
        ggplot() +
        theme_cowplot() +
        geom_bar(aes(x = scenarioLabel, y = mean.val), fill = fillcolor, stat = "identity", size = 1.3) +
        # geom_hline(aes(yintercept=hospitalized), linetype="dashed", col="grey", size=1.7) +
        scale_y_continuous(label = comma, expand = c(0, 0))
      customThemeNoFacet +
        labs(x = "") +
        labs(y = "deaths") +
        theme(legend.position = "right")

      ppredList[[length(ppredList) + 1]] <- ppred
      # ggsave(paste0("reopen_",i, "HS_scenarios_hospitalized", ".pdf"),
      #        plot = ppred, path = file.path(expDIR), width = 6, height = 3, device = "pdf"
      # )
    }

    p1 <- ppredList[[1]]
    p2 <- ppredList[[2]]
    p3 <- ppredList[[3]]
    p4 <- ppredList[[4]]


    pall <- plot_grid(p4, p3, p2, p1, ncol = 1)
    ggsave(paste0("reopen_HS_scenarios_hospitalized", ".pdf"),
      plot = pall, path = file.path(expDIR), width = 6, height = 10, device = "pdf"
    )
  }
  ### Load Rt's
}


## ================================================
###  Plot individual trajectories over time for method plot
## ================================================

methodPlot <- FALSE
if (methodPlot) {
  customTheme <- f_getCustomTheme(fontscl = 3)

  exp_name <- "20200731_IL_reopen_contactTracing"
  source("C:/Users/mrm9534/gitrepos/covid-chicago/Rfiles/ct_analysis/loadData_defineParam.R")

  selected_ems <- c(1:11)
  emsvars_temp <- c("critical_EMS.")
  emsvars <- NULL
  for (ems in selected_ems) {
    emsvars <- c(emsvars, paste0(emsvars_temp, ems))
  }

  groupvars <- c("startdate", "Date", "time", "scen_num", "sample_num", "run_num", "reopening_multiplier_4", "detection_success", "isolation_success", "grpvar")
  (keepvars <- c(groupvars, emsvars))

  ### Aggregate for all IL
  subdat <- trajectoriesDat %>%
    dplyr::select(keepvars) %>%
    pivot_longer(cols = -c(groupvars)) %>%
    dplyr::mutate(name = gsub("All", "EMS.IL", name)) %>%
    dplyr::mutate(name = gsub("[.]", "_", name)) %>%
    separate(name, into = c("outcome", "region"), sep = "_EMS_") %>%
    dplyr::filter(Date >= reopeningdate - 60 & Date <= as.Date("2021-01-15")) %>%
    dplyr::select(-c(time)) %>%
    filter(region == 11) %>%
    dplyr::group_by(startdate, region, Date, reopening_multiplier_4, scen_num, sample_num, run_num, detection_success, isolation_success, grpvar) %>%
    dplyr::summarize(value = sum(value))

  capacity <- load_capacity(unique(subdat$region)) %>%
    dplyr::rename(capacity = critical)

  subdat <- subdat %>%
    merge(capacity, by.x = "region", by.y = "geography_name") %>%
    filter(grpvar == 0.05)

  ##### SMooth, calculate weekly average
  subdatWklAvr_lines <- subdat %>%
    mutate(week = week(Date)) %>%
    group_by(week, scen_num, grpvar, detection_success, isolation_success, capacity) %>%
    summarize(
      value = mean(value),
      Date = max(Date)
    ) %>%
    ungroup() %>%
    group_by(scen_num, grpvar, capacity) %>%
    mutate(peak = max(value)) %>%
    f_valuefct()


  subdatWklAvr <- subdat %>%
    dplyr::filter(Date >= reopeningdate & Date <= as.Date("2021-02-01")) %>%
    mutate(week = week(Date)) %>%
    group_by(week, scen_num, grpvar, detection_success, isolation_success, capacity) %>%
    summarize(
      value = mean(value),
      Date = max(Date)
    ) %>%
    ungroup() %>%
    group_by(scen_num, grpvar, capacity) %>%
    mutate(peak = max(value)) %>%
    f_valuefct()


  #### Generate factor variable with custom cuts
  summary(subdatWklAvr$value)

  table(subdatWklAvr$value_fct, exclude = NULL)
  tapply(subdatWklAvr$value, subdatWklAvr$value_fct, summary)


  maxval <- max(subdatWklAvr$value, na.rm = TRUE)

  l_plot <- ggplot(data = subdatWklAvr_lines) +
    theme_cowplot() +
    geom_line(
      data = subset(subdatWklAvr_lines),
      aes(x = Date, y = value, group = scen_num), col = "azure4", size = 1, alpha = 0.5
    ) +
    # geom_line(data = subset(subdatWklAvr, grpvar == unique(subdat$grpvar)[1] & scen_num %in% selectedScens),
    #          aes(x = Date, y = value, group=scen_num), col = "brown3", size = 1, alpha=0.8)  +
    geom_point(
      data = subset(subdatWklAvr, value == peak),
      aes(x = Date, y = value, group = scen_num, fill = value_fct), size = 4, alpha = 1, shape = 21
    ) +
    # geom_point(data = subset(subdatWklAvr, value == peak & grpvar == unique(subdat$grpvar)[1] & scen_num %in% selectedScens),
    #          aes(x = Date, y = value, group=scen_num), fill = "brown3", size =4, alpha=1, shape=21)  +
    geom_hline(aes(yintercept = capacity)) +
    scale_color_viridis(option = "C", discrete = TRUE) +
    scale_fill_viridis(option = "C", discrete = TRUE) +
    labs(
      title = "",
      subtitle = "", y = "Predicted ICU bed demand\n per 1000 population/n",
      x = ""
    ) +
    customTheme +
    scale_x_date(breaks = "30 days", labels = date_format("%b"), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, maxval + 100), expand = c(0, 0)) +
    theme(legend.position = "none")


  ggsave(paste0("scenarios_timeline_wklAvr.png"),
    plot = l_plot, path = file.path(exp_dir), width = 10, height = 6, device = "png"
  )

  ggsave(paste0("scenarios_timeline_wklAvr.pdf"),
    plot = l_plot, path = file.path(pdfoutdir), width = 7, height = 5.5, device = "pdf"
  )


  #### Generate scatter plot
  peakdat <- subset(subdatWklAvr, value == peak & !is.na(value_fct))

  s_plot <- ggplot(data = peakdat) +
    theme_minimal() +
    geom_point(aes(x = detection_success, y = isolation_success, fill = value_fct), size = 4, alpha = 1, shape = 21) +
    scale_fill_viridis(option = "C", discrete = TRUE) +
    labs(
      title = "",
      subtitle = "",
      x = "Fraction detected",
      y = "Fraction detected that isolate\n",
      fill = "Predicted ICU bed demand\nper 1000 population"
    ) +
    customTheme +
    theme(legend.position = "none")



  ggsave(paste0("scenarios_scatter_wklAvr_scl.png"),
    plot = s_plot, path = file.path(exp_dir), width = 10, height = 6, device = "png"
  )

  ggsave(paste0("scenarios_scatter_wklAvr_scl.pdf"),
    plot = s_plot, path = file.path(pdfoutdir), width = 6, height = 5.5, device = "pdf"
  )


  #### Heatmap - loess
  heatmapPlot <- TRUE
  if (heatmapPlot) {


    #### Do loess regression
    detection_success <- seq(0, 1, 0.001)
    isolation_success <- seq(0, 1, 0.001)
    t_matdat <- expand.grid(detection_success = detection_success, isolation_success = isolation_success)

    m <- loess(value ~ detection_success * isolation_success,
      span = 0.5,
      degree = 2, data = peakdat
    )

    temp_fit_mat <- predict(m, t_matdat)
    dtfit <- melt(temp_fit_mat)
    dtfit$detection_success <- gsub("detection_success=", "", temp_fit$detection_success)
    dtfit$isolation_success <- gsub("isolation_success=", "", temp_fit$isolation_success)

    dtfit$detection_success <- as.numeric(dtfit$detection_success)
    dtfit$isolation_success <- as.numeric(dtfit$isolation_success)
    dtfit <- f_valuefct(dtfit)


    dtfit$value_fct2 <- cut(dtfit$value, 10)


    ### Extract  minimum isolation_success for each detection_success
    thresholdDat <- dtfit %>%
      filter(value <= capacity$capacity) %>%
      group_by(detection_success, grpvar) %>%
      filter(isolation_success == min(isolation_success))

    ### Plot contour-heatmap plot
    p1 <- ggplot(data = subset(dtfit, !is.na(value_fct)), aes(x = detection_success, y = isolation_success)) +
      theme_minimal() +
      geom_tile(aes(fill = value_fct), alpha = 0.8) +
      scale_fill_viridis(option = "C", discrete = TRUE) +
      # scale_fill_viridis(option = "C", discrete = FALSE) +
      labs(
        x = "Fraction detected",
        y = "Fraction detected that isolate\n",
        col = "",
        shape = "",
        linetype = ""
      ) +
      scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), expand = c(0, 0)) +
      scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), expand = c(0, 0)) +
      customThemeNoFacet +
      theme(panel.spacing = unit(1.5, "lines")) +
      theme(legend.position = "none")

    p1_withPoints <- ggplot() +
      geom_line(
        data = subset(thresholdDat, isolation_success != min(isolation_success)),
        aes(x = detection_success, y = isolation_success), size = 1.3
      ) +
      geom_point(data = peakdat, aes(x = detection_success, y = isolation_success, fill = value_fct), size = 3, shape = 21, show.legend = FALSE) +
      scale_fill_viridis(option = "C", discrete = TRUE) +
      # scale_fill_viridis(option = "C", discrete = FALSE) +
      labs(
        x = "Fraction detected",
        y = "Fraction detected that isolate\n",
        col = "",
        shape = "",
        linetype = ""
      ) +
      scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), expand = c(0, 0)) +
      scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), expand = c(0, 0)) +
      customThemeNoFacet +
      theme(panel.spacing = unit(1.5, "lines")) +
      theme(legend.position = "none")

    ggsave("ICUcapacity_heatmap_loess.png",
      plot = p1, path = file.path(pdfoutdir), width = 6, height = 5.5, device = "png"
    )


    ggsave("ICUcapacity_heatmap_loess_layers.pdf",
      plot = p1_withPoints, path = file.path(pdfoutdir), width = 6, height = 5.5, device = "pdf"
    )
  }
}


methodPlot_Rt <- FALSE
if (methodPlot) {
  customTheme <- f_getCustomTheme(fontscl = 3)

  # exp_name <- "20200731_IL_reopen_contactTracing"
  Rtdat <- read.csv(ct_dir, simdate, exp_name, "estimatedRt/EMS_combined_estimated_Rt.csv")


  selected_ems <- c(1:11)
  emsvars_temp <- c("critical_EMS.")
  emsvars <- NULL
  for (ems in selected_ems) {
    emsvars <- c(emsvars, paste0(emsvars_temp, ems))
  }

  ### Aggregate for all IL
  Rtsubdat <- Rtdat %>%
    filter(region == 11) %>%
    mutate(
      Date = as.Date(Date),
      capacity = 1,
      value = Mean
    ) %>%
    filter(grpvar == 0.05) %>%
    filter(Date >= as.Date(reopeningdate - 60) & Date <= as.Date("2020-10-01"))

  ##### SMooth, calculate weekly average
  subdatAvr <- Rtsubdat %>%
    dplyr::filter(Date >= as.Date(reopeningdate) & Date <= as.Date(reopeningdate + 30)) %>%
    dplyr::group_by(scen_num, detection_success, isolation_success, capacity) %>%
    dplyr::summarize(
      value = mean(value),
      Date = mean(Date)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scen_num, capacity) %>%
    mutate(peakavrg = mean(value)) %>%
    f_valuefctRt()

  subdatAvr2 <- Rtsubdat %>%
    dplyr::filter(Date >= as.Date(reopeningdate + 29) & Date <= as.Date(reopeningdate + 30)) %>%
    dplyr::group_by(scen_num, detection_success, isolation_success, capacity) %>%
    dplyr::summarize(
      value = mean(value),
      Date = mean(Date)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scen_num, capacity) %>%
    mutate(peakafter1mth = mean(value)) %>%
    f_valuefctRt()


  #### Generate factor variable with custom cuts
  summary(Rtsubdat$value)
  summary(subdatAvr$value)

  l_plot <- ggplot(data = subdatAvr) +
    theme_cowplot() +
    annotate("rect", xmin = reopeningdate, xmax = reopeningdate + 30, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "azure3") +
    geom_line(
      data = Rtsubdat,
      aes(x = Date, y = value, group = scen_num), col = "azure4", size = 1, alpha = 0.5
    ) +
    geom_point(
      data = subdatAvr,
      aes(x = Date, y = value, group = scen_num, fill = value_fct), shape = 21, size = 4, alpha = 0.3
    ) +
    geom_point(
      data = subdatAvr2,
      aes(x = Date, y = value, group = scen_num, fill = value_fct), shape = 21, size = 4, alpha = 1
    ) +
    geom_hline(aes(yintercept = capacity)) +
    scale_color_viridis(option = "C", discrete = TRUE) +
    scale_fill_viridis(option = "C", discrete = TRUE) +
    labs(
      title = "",
      subtitle = "", y = expr("Estimated" * " " * italic(R[t])),
      x = ""
    ) +
    customTheme +
    scale_x_date(breaks = "1 month", labels = date_format("%b"), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.position = "none")


  ggsave(paste0("scenarios_timeline_Rt.png"),
    plot = l_plot, path = file.path(exp_dir), width = 10, height = 6, device = "png"
  )

  ggsave(paste0("scenarios_timeline_Rt.pdf"),
    plot = l_plot, path = file.path(pdfoutdir), width = 7, height = 5.5, device = "pdf"
  )


  #### Generate scatter plot
  peakdat <- subset(subdatAvr2, value == peakafter1mth & !is.na(value_fct))

  s_plot <- ggplot(data = peakdat) +
    theme_minimal() +
    geom_point(aes(x = detection_success, y = isolation_success, fill = value_fct), size = 4, alpha = 1, shape = 21) +
    scale_fill_viridis(option = "C", discrete = TRUE) +
    labs(
      title = "",
      subtitle = "",
      x = "Fraction detected",
      y = "Fraction detected that isolate\n",
      fill = "Predicted ICU bed demand\nper 1000 population"
    ) +
    customTheme +
    theme(legend.position = "none")



  ggsave(paste0("scenarios_scatter_after1mth_Rt.png"),
    plot = s_plot, path = file.path(exp_dir), width = 10, height = 6, device = "png"
  )

  ggsave(paste0("scenarios_scatter_after1mth_Rt.pdf"),
    plot = s_plot, path = file.path(pdfoutdir), width = 6, height = 5.5, device = "pdf"
  )


  #### Heatmap - loess
  heatmapPlot <- TRUE
  if (heatmapPlot) {


    #### Do loess regression
    detection_success <- seq(0, 1, 0.001)
    isolation_success <- seq(0, 1, 0.001)
    t_matdat <- expand.grid(detection_success = detection_success, isolation_success = isolation_success)

    m <- loess(value ~ detection_success * isolation_success,
      span = 0.5,
      degree = 2, data = peakdat
    )

    temp_fit_mat <- predict(m, t_matdat)
    dtfit <- melt(temp_fit_mat)
    dtfit$detection_success <- gsub("detection_success=", "", temp_fit$detection_success)
    dtfit$isolation_success <- gsub("isolation_success=", "", temp_fit$isolation_success)

    dtfit$detection_success <- as.numeric(dtfit$detection_success)
    dtfit$isolation_success <- as.numeric(dtfit$isolation_success)
    dtfit <- f_valuefctRt(dtfit)
    dtfit$value_fct2 <- cut(dtfit$value, 10)


    ### Extract  minimum isolation_success for each detection_success
    thresholdDat <- dtfit %>%
      filter(value <= 1) %>%
      group_by(detection_success) %>%
      filter(isolation_success == min(isolation_success))

    ### Plot contour-heatmap plot
    p1 <- ggplot(data = subset(dtfit, !is.na(value_fct)), aes(x = detection_success, y = isolation_success)) +
      theme_minimal() +
      geom_tile(aes(fill = value_fct), alpha = 0.8) +
      scale_fill_viridis(option = "C", discrete = TRUE) +
      # scale_fill_viridis(option = "C", discrete = FALSE) +
      labs(
        x = "Fraction detected",
        y = "Fraction detected that isolate\n",
        col = "",
        shape = "",
        linetype = ""
      ) +
      scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), expand = c(0, 0)) +
      scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), expand = c(0, 0)) +
      customThemeNoFacet +
      theme(panel.spacing = unit(1.5, "lines")) +
      theme(legend.position = "none")

    p1_withPoints <- ggplot() +
      geom_line(
        data = subset(thresholdDat, isolation_success != min(isolation_success)),
        aes(x = detection_success, y = isolation_success), size = 1.3
      ) +
      geom_point(data = peakdat, aes(x = detection_success, y = isolation_success, fill = value_fct), size = 3, shape = 21, show.legend = FALSE) +
      scale_fill_viridis(option = "C", discrete = TRUE) +
      # scale_fill_viridis(option = "C", discrete = FALSE) +
      labs(
        x = "Fraction detected",
        y = "Fraction detected that isolate\n",
        col = "",
        shape = "",
        linetype = ""
      ) +
      scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), expand = c(0, 0)) +
      scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), expand = c(0, 0)) +
      customThemeNoFacet +
      theme(panel.spacing = unit(1.5, "lines")) +
      theme(legend.position = "none")

    ggsave("ICUcapacity_heatmap_loess_Rt.png",
      plot = p1, path = file.path(pdfoutdir), width = 6, height = 5.5, device = "png"
    )


    ggsave("ICUcapacity_heatmap_loess_Rt_layers.pdf",
      plot = p1_withPoints, path = file.path(pdfoutdir), width = 6, height = 5.5, device = "pdf"
    )
  }
}

## ================================================
###  HS scenarios with contact tracing
## ================================================
f_getPredDat <- function(expDIR) {
  trajectoriesDat <- read.csv(file.path(expDIR, "trajectoriesDat.csv"))
  unique(trajectoriesDat$reopening_multiplier_4)
  unique(trajectoriesDat$fraction_critical)

  restore_region_names <- c("southern", "central", "northcentral", "northeast")

  colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))


  ### per restore region
  paramvars <- c(
    paste0("deaths_", restore_region_names),
    paste0("hosp_cumul_", restore_region_names),
    paste0("hosp_det_", restore_region_names),
    paste0("hospitalized_", restore_region_names),
    paste0("infected_", restore_region_names),
    paste0("deaths_det_", restore_region_names),
    paste0("prevalence_", restore_region_names),
    paste0("crit_cumul_", restore_region_names)
  )


  keepvars <- c("time", "startdate", "scen_num", "reopening_multiplier_4", "d_AsP_ct1_grp", "d_AsP_ct1", "reduced_inf_of_det_cases_ct1", paramvars)

  trajectoriesDat$d_AsP_ct1_grp <- NA
  trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 <= 0.05] <- "<0.05"
  trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 >= 0.05 & trajectoriesDat$d_AsP_ct1 <= 0.1] <- "<0.1"
  trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 >= 0.1 & trajectoriesDat$d_AsP_ct1 <= 0.2] <- "0.1-0.2"
  trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 >= 0.2 & trajectoriesDat$d_AsP_ct1 <= 0.3] <- "0.2-0.3"
  trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 >= 0.4 & trajectoriesDat$d_AsP_ct1 <= 0.4] <- "0.3-0.4"
  trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 >= 0.5] <- ">0.5"


  predDat <- trajectoriesDat %>%
    dplyr::select(keepvars) %>%
    dplyr::group_by(time, startdate, reopening_multiplier_4, d_AsP_ct1_grp) %>%
    dplyr::filter(reduced_inf_of_det_cases_ct1 == max(reduced_inf_of_det_cases_ct1)) %>%
    dplyr::mutate(date = as.Date(startdate) + time) %>%
    dplyr::filter(date <= as.Date("2020-12-31")) %>%
    pivot_longer(cols = -c("time", "date", "startdate", "reopening_multiplier_4", "d_AsP_ct1_grp"), names_to = "name") %>%
    dplyr::mutate(
      name = gsub("_cumul_", ".cumul_", name),
      name = gsub("_det_", ".det_", name)
    ) %>%
    separate(name, into = c("param", "restore_region"), sep = "_") %>%
    dplyr::mutate(
      param = gsub(".cumul", "_cumul", param),
      param = gsub(".det", "_det", param)
    ) %>%
    dplyr::mutate(
      exp_name = exp_name
    ) %>%
    dplyr::group_by(date, restore_region, param, exp_name, reopening_multiplier_4, d_AsP_ct1_grp) %>%
    dplyr::summarize(
      mean.val = mean(value, na.rm = TRUE),
      sd.val = sd(value, na.rm = TRUE),
      n.val = n(),
    ) %>%
    dplyr::mutate(
      se.val = sd.val / sqrt(n.val),
      lower.ci.val = mean.val - qt(1 - (0.05 / 2), n.val - 1) * se.val,
      upper.ci.val = mean.val + qt(1 - (0.05 / 2), n.val - 1) * se.val
    )


  if (!("restore_region" %in% colnames(predDat))) predDat$restore_region <- str_to_sentence(predDat$restore_region)

  return(predDat)
}

combineDat=FALSE
if(combineDat){
  source("load_paths.R")
  exp_name <- "20200731_IL_reopen_contactTracing"
  expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)
  predDat_CT <- f_getPredDat(expDIR) %>% mutate(scenario = "CTonly")
  
  exp_name <- "20200731_IL_reopen_contactTracingHS40"
  expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)
  predDat_HS40 <- f_getPredDat(expDIR) %>% mutate(scenario = "HS40")
  
  exp_name <- "20200731_IL_reopen_contactTracingHS40TD"
  expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)
  predDat_HS40TD <- f_getPredDat(expDIR) %>% mutate(scenario = "HS40TD")
  
  exp_name <- "20200731_IL_reopen_contactTracingHS80"
  expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)
  predDat_HS80 <- f_getPredDat(expDIR) %>% mutate(scenario = "HS80")
  
  
  exp_name <- "20200731_IL_reopen_contactTracingHS80TD"
  expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)
  predDat_HS80TD <- f_getPredDat(expDIR) %>% mutate(scenario = "HS80TD")
  
  
  predDat <- rbind(predDat_HS40, predDat_HS40TD, predDat_HS80, predDat_HS80TD)
  
  table(predDat$restore_region, predDat$scenario)
  tapply(predDat$date, predDat$scenario, summary)
  
  table(predDat$d_AsP_ct1_grp, predDat$scenario)
  table(predDat$reopening_multiplier_4, predDat$scenario)
  tapply(predDat$mean.val, predDat$scenario, summary)
  
  
  
  save(predDat, file = file.path(simulation_output, "contact_tracing/20200731/", "reopen_contactTracingAll.Rdata"))
  
}

expDIR <- file.path(simulation_output, "contact_tracing/20200731")
load( file.path(simulation_output, "contact_tracing/20200731/", "reopen_contactTracingAll.Rdata"))


f_linePlotIL <- function() {
  ppred <- predDat %>%
    dplyr::mutate(geography_name = tolower(restore_region)) %>%
    dplyr::left_join(capacity, by = "geography_name") %>%
    dplyr::filter(param %in% c("hospitalized") &
      date >= as.Date("2020-07-01") & date <= as.Date("2020-12-01")) %>%
    mutate(
      HSgrp = ifelse(scenario %in% c("HS40", "HS40TD"), "dSym 40%", "dSym 80%"),
      scenario2 = paste0(scenario, " ", d_AsP_ct1_grp),
      scenario3 = gsub("HS40", "", gsub("HS80", "", scenario2))
    ) %>%
    dplyr::group_by(date, reopening_multiplier_4, scenario, d_AsP_ct1_grp, scenario3) %>%
    dplyr::summarize(
      mean.val = sum(mean.val),
      medsurg_available = sum(medsurg_available)
    ) %>%
    ggplot() +
    theme_cowplot() +
    # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_line(aes(
      x = date,
      y = mean.val,
      col = as.factor(d_AsP_ct1_grp),
      group = interaction(d_AsP_ct1_grp, reopening_multiplier_4)
    ), size = 1.3) +
    geom_hline(aes(yintercept = medsurg_available),
      linetype = "dashed", col = "grey", size = 1.7
    ) +
    facet_wrap(reopening_multiplier_4 ~ scenario, scales = "free_y", nrow = 4) +
    scale_y_continuous(label = comma, expand = c(0, 0)) +
    scale_color_brewer(palette = "Dark2") +
    customThemeNoFacet +
    labs(y = "hospitalized", x = "") +
    theme(legend.position = "right")


  ggsave(paste0("HS_contactTracing_scenarios_hospitalized_IL", ".pdf"),
    plot = ppred, path = file.path(expDIR), width = 12, height = 12, device = "pdf"
  )
}


f_linePlotRR <- function() {
  ppredDat1 <- predDat %>%
    dplyr::mutate(geography_name = tolower(restore_region)) %>%
    dplyr::left_join(capacity, by = "geography_name") %>%
    dplyr::filter(param %in% c("hospitalized") &
      date >= as.Date("2020-07-01") & date <= as.Date("2020-12-01")) %>%
    mutate(
      HSgrp = ifelse(scenario %in% c("HS40", "HS40TD"), "dSym 40%", "dSym 80%"),
      scenario2 = paste0(scenario, " ", d_AsP_ct1_grp),
      scenario3 = gsub("HS40", "", gsub("HS80", "", scenario2))
    )

  ppred <- ppredDat1 %>%
    filter(reopening_multiplier_4 == 0) %>%
    ggplot() +
    theme_cowplot() +
    # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_line(aes(
      x = date,
      y = mean.val,
      col = as.factor(scenario2),
      group = interaction(scenario2)
    ), size = 1.3) +
    geom_hline(aes(yintercept = hospitalized),
      linetype = "dashed", col = "grey", size = 1.7
    ) +
    facet_wrap(~restore_region, scales = "free_y", nrow = 4) +
    scale_y_continuous(label = comma, expand = c(0, 0)) +
    # scale_color_brewer(palette="Dark2")+
    customThemeNoFacet +
    labs(y = "hospitalized", x = "") +
    theme(legend.position = "right")


  ggsave(paste0("HS_contactTracing_scenarios_hospitalized_IL", ".pdf"),
    plot = ppred, path = file.path(expDIR), width = 12, height = 12, device = "pdf"
  )
}



if (testplot) {
  ppredDat <- predDat %>%
    dplyr::mutate(geography_name = tolower(restore_region)) %>%
    dplyr::left_join(capacity, by = "geography_name") %>%
    dplyr::filter(
      d_AsP_ct1_grp %in% c("<0.05", ">0.5"),
      param %in% c("hosp_cumul") &
        date >= as.Date("2020-12-30") & date <= as.Date("2020-12-31")
    )

  ppred <- ggplot(data = ppredDat) +
    theme_cowplot() +
    geom_bar(aes(
      x = scenario,
      y = mean.val,
      fill = as.factor(restore_region),
      group = interaction(restore_region)
    ), position = "stack", stat = "identity", size = 1.3) +
    facet_wrap(d_AsP_ct1_grp ~ reopening_multiplier_4, scales = "free_y", nrow = 2) +
    scale_y_continuous(label = comma, expand = c(0, 0)) +
    scale_fill_manual(values = restoreRegion_cols) +
    customThemeNoFacet +
    labs(y = "hospitalized", x = "") +
    theme(legend.position = "right") +
    coord_flip()


  ggsave(paste0("test_plot", ".pdf"),
    plot = ppred, path = file.path(expDIR), width = 12, height = 12, device = "pdf"
  )
}



ppredDat <- predDat %>%
  dplyr::mutate(geography_name = tolower(restore_region)) %>%
  dplyr::left_join(capacity, by = "geography_name") %>%
  dplyr::filter(
    d_AsP_ct1_grp %in% c("<0.05", "<0.1", "0.1-0.2", "0.2-0.3"),
    param %in% c("hosp_cumul") & date >= as.Date("2020-12-30") & date <= as.Date("2020-12-31")
  ) %>%
  mutate(
    HSgrp = ifelse(scenario %in% c("HS40", "HS40TD"), "dSym 40%", "dSym 80%"),
    scenario2 = paste0(scenario, " ", d_AsP_ct1_grp),
    scenario3 = gsub("HS40", "", gsub("HS80", "", scenario2))
  ) %>%
  dplyr::group_by(HSgrp, reopening_multiplier_4, d_AsP_ct1_grp, scenario, scenario2, scenario3) %>%
  dplyr::summarize(
    mean.val = sum(mean.val),
    medsurg_available = sum(medsurg_available)
  )


pall <- ppredDat %>%
  ggplot() +
  theme_cowplot() +
  geom_bar(aes(
    x = reorder(as.factor(scenario), mean.val, desc = FALSE),
    y = mean.val,
    fill = d_AsP_ct1_grp,
    group = scenario3
  ),
  position = position_dodge(width = 0.9), col = "darkgrey", stat = "identity", size = 0.7
  ) +
  facet_wrap(~reopening_multiplier_4, scales = "free") +
  scale_y_continuous(label = comma, expand = c(0, 0)) +
  scale_fill_brewer(palette = "Greens") +
  customThemeNoFacet +
  labs(y = "hospitalized", x = "") +
  theme(legend.position = "right")
ggsave(paste0("CT_scenarios_allReopen", ".pdf"),
  plot = pall, path = file.path(expDIR), width = 12, height = 7, device = "pdf"
)


p0 <- ppredDat %>%
  filter(reopening_multiplier_4 == 0 ) %>%
  ggplot() +
  theme_cowplot() +
  geom_bar(aes(
    x = reorder(as.factor(scenario), mean.val, desc = FALSE),
    y = mean.val,
    fill = d_AsP_ct1_grp,
    group = scenario3
  ),
  position = position_dodge(width = 0.9), col = "darkgrey", stat = "identity", size = 0.7
  ) +
  facet_wrap(~HSgrp, scales = "free_x") +
  scale_y_continuous(label = comma, expand = c(0, 0)) +
  scale_fill_brewer(palette = "Greens") +
  customThemeNoFacet +
  labs(y = "hospitalized", x = "") +
  theme(legend.position = "right")
ggsave(paste0("CT_scenarios_baseline", ".pdf"),
  plot = p0, path = file.path(expDIR), width = 12, height = 7, device = "pdf"
)




##### Over time
overTime <- FALSE
if (overTime) {
  ppredDat <- predDat %>%
    dplyr::mutate(geography_name = tolower(restore_region)) %>%
    dplyr::left_join(capacity, by = "geography_name") %>%
    dplyr::filter(
      d_AsP_ct1_grp %in% c("<0.05", "<0.1", "0.1-0.2", "0.2-0.3"),
      param %in% c("hospitalized") & date <= as.Date("2020-12-31")
    ) %>%
    mutate(
      HSgrp = ifelse(scenario %in% c("HS40", "HS40TD"), "dSym 40%", "dSym 80%"),
      scenario2 = paste0(scenario, " ", d_AsP_ct1_grp),
      scenario3 = gsub("HS40", "", gsub("HS80", "", scenario2))
    ) %>%
    dplyr::group_by(HSgrp, reopening_multiplier_4, d_AsP_ct1_grp, scenario, scenario2, scenario3) %>%
    dplyr::summarize(mean.val = sum(mean.val), medsurg_available = sum(medsurg_available))


  ppredDat %>%
    ggplot() +
    theme_cowplot() +
    geom_line(aes(
      x = date,
      y = mean.val,
      fill = d_AsP_ct1_grp,
      group = scenario3
    ),
    position = position_dodge(width = 0.9), col = "darkgrey", stat = "identity", size = 1.3
    ) +
    facet_wrap(scenario3~reopening_multiplier_4, scales = "free") +
    scale_y_continuous(label = comma, expand = c(0, 0)) +
    scale_fill_brewer(palette = "greens") +
    customThemeNoFacet +
    labs(y = "hospitalized", x = "") +
    theme(legend.position = "right")
  
}




####### 
if(capacityScatterPlot){
  
  ## ================================================
  ###  HS scenarios with contact tracing
  ## ================================================
  f_getPredDat <- function(expDIR) {
    
    #trajectoriesDat <- read.csv(file.path(expDIR, "trajectoriesDat.csv"))
    trajectoriesDat <- read_csv(file.path(expDIR, "trajectoriesDat.csv"), 
                       col_types = cols_only(time = col_guess(), 
                                             startdate = col_guess(),
                                             scen_num = col_guess(),
                                             d_AsP_ct1 = col_guess(),
                                             reduced_inf_of_det_cases_ct1 = col_guess(),
                                             reopening_multiplier_4 = col_guess(),
                                             hosp_cumul_All = col_guess(),
                                             hosp_det_All = col_guess(),
                                             hospitalized_All = col_guess(),
                                             critical_All = col_guess(),
                                             crit_det_All = col_guess(),
                                             crit_cumul_All = col_guess()))


    trajectoriesDat$d_AsP_ct1_grp <- NA
    trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 <= 0.05] <- "<0.05"
    trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 >= 0.05 & trajectoriesDat$d_AsP_ct1 <= 0.1] <- "<0.1"
    trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 >= 0.1 & trajectoriesDat$d_AsP_ct1 <= 0.2] <- "0.1-0.2"
    trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 >= 0.2 & trajectoriesDat$d_AsP_ct1 <= 0.3] <- "0.2-0.3"
    trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 >= 0.4 & trajectoriesDat$d_AsP_ct1 <= 0.4] <- "0.3-0.4"
    trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 >= 0.5] <- ">0.5"
    
    
    predDat <- trajectoriesDat %>%
      dplyr::group_by(time, startdate, reopening_multiplier_4,d_AsP_ct1, d_AsP_ct1_grp,scen_num) %>%
      dplyr::filter(reduced_inf_of_det_cases_ct1 == max(reduced_inf_of_det_cases_ct1)) %>%
      dplyr::mutate(date = as.Date(startdate) + time) %>%
      dplyr::filter(date <= as.Date("2020-12-31")) %>%
      pivot_longer(cols = -c("time", "date", "startdate", "reopening_multiplier_4","reduced_inf_of_det_cases_ct1", "d_AsP_ct1", "d_AsP_ct1_grp","scen_num"), names_to = "name") %>%
      dplyr::mutate(
        name = gsub("_cumul_", ".cumul_", name),
        name = gsub("_det_", ".det_", name)
      ) %>%
      separate(name, into = c("param", "region"), sep = "_") %>%
      dplyr::mutate(
        param = gsub("[.]", "_", param)
      ) %>%
      dplyr::mutate(
        exp_name = exp_name
      ) %>%
      dplyr::group_by(date, region, param, exp_name, reopening_multiplier_4,d_AsP_ct1, d_AsP_ct1_grp) %>%
      dplyr::summarize(
        median.val = median(value, na.rm = TRUE),
        q25		= quantile(value, probs=0.25, na.rm = TRUE),
        q75		= quantile(value, probs=0.75, na.rm = TRUE),
        q2.5		= quantile(value, probs=0.025, na.rm = TRUE),
        q97.5  	= quantile(value, probs=0.975, na.rm = TRUE),
        n.val = n()
      ) 
    
    return(predDat)
  }
  
  combineDat=FALSE
  if(combineDat){
    source("load_paths.R")
    exp_name <- "20200731_IL_reopen_contactTracing"
    expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)
    predDat_CT <- f_getPredDat(expDIR) %>% mutate(scenario = "CTonly")
    
    exp_name <- "20200731_IL_reopen_contactTracingHS40"
    expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)
    predDat_HS40 <- f_getPredDat(expDIR) %>% mutate(scenario = "HS40")
    
    exp_name <- "20200731_IL_reopen_contactTracingHS40TD"
    expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)
    predDat_HS40TD <- f_getPredDat(expDIR) %>% mutate(scenario = "HS40TD")
    
    exp_name <- "20200731_IL_reopen_contactTracingHS80"
    expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)
    predDat_HS80 <- f_getPredDat(expDIR) %>% mutate(scenario = "HS80")
    
    
    exp_name <- "20200731_IL_reopen_contactTracingHS80TD"
    expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)
    predDat_HS80TD <- f_getPredDat(expDIR) %>% mutate(scenario = "HS80TD")
    
    
    predDat <- rbind(predDat_HS40, predDat_HS40TD, predDat_HS80, predDat_HS80TD)
    
    table(predDat$restore_region, predDat$scenario)
    tapply(predDat$date, predDat$scenario, summary)
    
    table(predDat$d_AsP_ct1_grp, predDat$scenario)
    table(predDat$reopening_multiplier_4, predDat$scenario)
    tapply(predDat$mean.val, predDat$scenario, summary)
    
    
    
    save(predDat, file = file.path(simulation_output, "contact_tracing/20200731/", "reopen_contactTracingAll.Rdata"))
    
  }
  
  expDIR <- file.path(simulation_output, "contact_tracing/20200731")
  load( file.path(simulation_output, "contact_tracing/20200731/", "reopen_contactTracingAll.Rdata"))
  
  ###===========================================
  ###  Plot minimum detection of As and P per Rt
  ###===========================================
  
  f_combineRtdat <- function(exp_name ="20200731_IL_reopen_contactTracing"){
    EMS_combined_estimated_Rt  <- read.csv(file.path(simdir, exp_name , "estimatedRt/EMS_combined_estimated_Rt.csv"))
    
    baselineRt <- EMS_combined_estimated_Rt %>% 
      filter(Date ==as.Date("2020-03-01")) %>% 
      dplyr::group_by(region, grpvar) %>% 
      dplyr::summarize(baselineRt=mean(Mean))
    
    currentRt <- EMS_combined_estimated_Rt %>% 
      filter(Date ==as.Date("2020-07-20")) %>% 
      dplyr::group_by(region, grpvar) %>% 
      dplyr::summarize(currentRt=mean(Mean))
    
    
    Rtbelow1 <- EMS_combined_estimated_Rt %>% 
      filter(Date == as.Date("2020-09-01")) %>% 
      filter(Mean < 1.0) %>%
      dplyr::group_by(region, grpvar) %>% 
      dplyr::summarize(Rtbelow1=mean(Mean),
                       detection_success =min(detection_success)) %>%
      left_join(baselineRt, by=c("region","grpvar")) %>%
      left_join(currentRt, by=c("region","grpvar")) %>%
      mutate(exp_name = exp_name)
    
    
    return(Rtbelow1)
  }
  
  

  customTheme <- f_getCustomTheme()
  

  pplot <- predDat %>%
    filter(param =="hospitalized") %>%
    left_join(capacity, by="geography_name") %>%
    filter(mean.val  ,= ) %>%  
    ggplot(data=subset(Rtbelow1))+ 
    geom_point(aes(x=currentRt , y= detection_success, fill=scenario_fct),col="azure4",shape=21, size=3) +
    geom_smooth(aes(x=currentRt , y= detection_success,col=scenario_fct,fill=scenario_fct), method="lm", alpha=0.3) +
    scale_y_continuous(lim=c(0,1), labels = function(x) x * 100 , expand=c(0,0)) +
    labs(x= expr(italic(R[t]) * " before contact tracing start"),
         y="Minimum required detection of \n a - and pre-symptomatic infections (%) ",
         color="Testing improvements for mild symptoms",
         fill="Testing improvements for mild symptoms") +
    scale_color_brewer(palette = "Dark2")+
    scale_fill_brewer(palette = "Dark2") +
    customTheme +
    theme(legend.position = "bottom")+
    guides(fill=guide_legend(ncol=2))+
    guides(color=guide_legend(ncol=2))
  
  
  ggsave(paste0("RTLE1_contactTracing_scatterPlot.pdf"),
         plot = pplot, path = file.path(pdfdir), width = 9.5, height =7,  device = "pdf"
  )
  

}


####------------------------------------------
#### HEATMAP
####------------------------------------------

if(heatmaps){
  
 
f_getHeatmap_IL <- function(exp_name="20200731_IL_reopen_contactTracing",regname="illinois", grpValues=c(0.00, 0.05, 0.1), showlegend=FALSE){
  
  load(file.path(simulation_output, "contact_tracing/20200731/",exp_name,"/heatmap_ICU/Illinois_dtfit.Rdata"))
  # thresholdDat <- read_csv("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/contact_tracing/20200731/20200731_IL_reopen_contactTracing/heatmap_ICU/Illinois_loess_ICUcapacity.csv")
  capacity <- load_capacity('illinois') %>% rename(capacity=critical)  %>% rename(region=geography_name) 
  popdat <- load_population() %>% rename(region=geography_name) 
  
  
  

  dtfit <- dtfit %>%  
    filter(!is.na(value) & grpvar %in% grpValues) %>%
    dplyr::mutate(region = regname) %>%
    left_join(popdat, by="region") %>%
    left_join(capacity, by="region") %>%
    mutate(percCapacity = (value-capacity)/capacity,
           belowCapacity = ifelse(value <= capacity, 1,0)) %>%
    mutate(value=as.numeric(value),
           pop = as.numeric(pop),
           value_pop1000 = (value /pop )*1000,
           capacity_pop1000 = (capacity /pop )*1000) %>%
   f_valuefct_cap("value") 
  
  labs=quantile(dtfit$percCapacity, probs = seq(0.1,1, 0.1), na.rm=FALSE)
  
  dtfit <- dtfit %>%    mutate(value_cut =cut(percCapacity, 10, labels=labs ) )
  

  thresholdDat <- dtfit %>%
    dplyr::filter(value <= capacity ) %>%
    dplyr::group_by(detection_success, grpvar,capacity, pop) %>%
    dplyr::filter(isolation_success == min(isolation_success)) 
    #dplyr::filter(percCapacity == min(percCapacity)) 

  pplot <- ggplot(data = subset(dtfit,  !is.na(value_fct)), aes(x = detection_success, y = isolation_success)) +
    theme_minimal() +
    geom_tile(aes(fill = as.factor(value_cut)), alpha = 0.8) +
    scale_fill_viridis(option = "C", discrete = T, direction=-1) +
    geom_line(    data = subset(thresholdDat, isolation_success != min(isolation_success)),
      aes(x = detection_success, y = isolation_success), size = 1.3
    ) +
    labs(
      x = "detections (%)",
      y = "isolation success (%)",
      col = "",
      fill = "Critical",
      shape = "",
      linetype = ""
    ) +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    facet_wrap(~grpvar, nrow=1) +
    customThemeNoFacet +
    theme(panel.spacing = unit(1.5, "lines")) +
    theme(legend.position = "right")+
      theme(strip.text.x = element_text(size = 12, face = "bold", color="white"),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            plot.margin = margin(l = 1)  )
  
    if(showlegend==FALSE) pplot <- pplot + theme(legend.position = "none")
  
  return(pplot)
}
  
  

legend <- get_legend(f_getHeatmap_IL(exp_name="20200731_IL_reopen_contactTracing", showlegend=TRUE))
p1 <- f_getHeatmap_IL(exp_name="20200731_IL_reopen_contactTracing")
p2 <- f_getHeatmap_IL(exp_name="20200731_IL_reopen_contactTracingHS40")
p3 <- f_getHeatmap_IL(exp_name="20200731_IL_reopen_contactTracingHS80")


p123 <- cowplot::plot_grid(p3 + theme(strip.text.x = element_text(size = 12, face = "bold", color="black"), 
                   p2, 
                   p1  ),
                   nrow = 3,
                   labels = "auto",
                   align = "hv")

ggsave(paste0("IL_heatmap", ".png"),
       plot = p123, path = file.path(pdfdir), width = 8, height = 8, device = "png"
)

  
}

