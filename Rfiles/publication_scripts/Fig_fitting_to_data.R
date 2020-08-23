
## ================================================
###  Figure 1a Load  data and plot over time
## ================================================


pdfdir <- "C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/project_notes/publications/covid_model_IL/pdfs_ais/"

#### Plots edited for publication
library(tidyverse)
library(cowplot)
library(scales)
library(viridis)
library(data.table)
library(stringi)
library(lubridate)

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



f_data_plot_emr <- function(SAVE=TRUE, simulation_output=simulation_output, exp_name=exp_name) {
  
  library(stringi)
  library(lubridate)
  
  emresource <- f_loadData(data_path)
  
  pplot <- emresource %>%
    mutate(Date=as.Date(Date),
           week =week(Date),
           month=month(Date),
           restore_region = str_to_sentence(restore_region)) %>%
    dplyr::group_by(Date, month, week, restore_region) %>%
    dplyr::summarize(
      suspected_and_confirmed_covid_icu = sum(suspected_and_confirmed_covid_icu),
      confirmed_covid_deaths_prev_24h = sum(confirmed_covid_deaths_prev_24h),
      confirmed_covid_icu = sum(confirmed_covid_icu),
      covid_non_icu = sum(covid_non_icu)
    ) %>%
    dplyr::group_by(month, week, restore_region) %>%
    dplyr::summarize(
      Date = max(Date),
      suspected_and_confirmed_covid_icu = mean(suspected_and_confirmed_covid_icu),
      confirmed_covid_deaths_prev_24h = mean(confirmed_covid_deaths_prev_24h),
      confirmed_covid_icu = mean(confirmed_covid_icu),
      covid_non_icu = mean(covid_non_icu)
    ) %>%
    ggplot() +
    theme_cowplot() +
    geom_line(aes(x = Date, y = suspected_and_confirmed_covid_icu, group = restore_region, col = restore_region), size = 1.3) +
    scale_color_manual(values = restoreRegion_cols) +
    scale_y_log10() +
    scale_x_date(lim = c(as.Date("2020-03-01"), as.Date("2020-08-01")))
  
if(SAVE){
  
  ggsave(paste0("emresource_timeline", ".pdf"),
         plot = pplot, path = file.path(simulation_output, exp_name), width = 8, height = 4, device = "pdf"
  )
}
}

f_data_plot_LL <- function(SAVE=TRUE, simulation_output=simulation_output, exp_name=exp_name) {
  
  library(stringi)
  library(lubridate)
  
  emresource <- f_loadData(data_path)
  
  emresource <- emresource %>%
    mutate(Date=as.Date(Date),
           week =week(Date),
           month=month(Date)) %>%
    f_addRestoreRegion() %>%
    mutate(restore_region = str_to_sentence(restore_region)) %>%
    filter(!is.na(restore_region)) %>%
    dplyr::select(Date, month, week, restore_region, region, LL_cases, LL_deaths, LL_admissions)
  
  pplot <- emresource %>%
    dplyr::group_by(Date, month, week, restore_region) %>%
    dplyr::summarize(
      cases = sum(LL_cases),
      deaths = sum(LL_deaths),
      admissions = sum(LL_admissions)
    ) %>%
    dplyr::group_by(month, week, restore_region) %>%
    dplyr::summarize(
      Date = max(Date),
      cases = mean(cases),
      deaths = mean(deaths),
      admissions = mean(admissions)
    ) %>%
    ggplot() +
    theme_cowplot() +
    geom_line(aes(x = Date, y = deaths, group = restore_region, col = restore_region), size = 1.3) +
    scale_color_manual(values = restoreRegion_cols) +
    scale_y_log10() +
    scale_x_date(lim = c(as.Date("2020-03-01"), as.Date("2020-08-01")))
  
  if(SAVE){
  ggsave(paste0("LL_deaths_timeline", ".pdf"),
         plot = pplot, path = file.path(simulation_output, exp_name), width = 8, height = 4, device = "pdf"
  )
  }
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





