###----------------------------------------------------------------------------------------------
# This script takes hospital data from various EMS regions of Illinois
###----------------------------------------------------------------------------------------------
# Input: 
# - simulation trajectoriesDat.csv located in projects\covid_chicago\cms_sim\simulation_output\forFitting\<exp_name>
# - reference data to fit to: 
#   - EMResource data "emresource_by_region.csv" located in data\covid_IDPH\Corona virus reports
#   - Line List data "200928_jg_aggregated_covidregion.csv" located in data\covid_IDPH\Cleaned Data

# Output : 
#  - pre-fit plots to visualise parameter space
#  - post_fit plot shwoing the 5% with minimal negative log likelihood 
#  - csv files located in \<exp_name>\fitting\csv : 
#    - "best_parameter_ranges_ems_11.csv" region specific csv files with 5% of parameter combinations
#    - "range_parameters_emsAll.csv"
#    - "best_parameters_emsAll.csv"
#    - "best_n_pairs_parameters_emsAll.csv"

# Changelog : 
# - This script is an revised version of the initial fitting script.
# - All parts around the NLL fit have been edited and updated:
#    - to be used for different parameter at different periods of the epidemic curve, 
#    - to use the locale simulation model instead of base model and separate EMS trajectories
#    - updated data sources
#    - fit equally to EMR and LL data for 3 instead of 5 outcomes
###----------------------------------------------------------------------------------------------


## set working directory to R.project location
library(here)
library(ggplot2)
library(data.table)
library(tidyverse)
library(cowplot)

theme_set(theme_cowplot())

### Set working directory to the GitHub repository R files
source("load_paths.R")
source("processing_helpers.R")


## --------------------------------
####  Experiment folder settings
## --------------------------------
exp_name <- "20200924_IL__test_initialFit"
exp_name <- "20200925_IL__test_reopening"
exp_name <- "20200925_IL__test_lockdown"
exp_name <- "20200927_IL__test_fitsm7"
exp_name <- "20200928_IL__test2_fitsm7"

simdate <- str_split(exp_name, "_")[[1]][1]
fitstep <- "newparam" # "initial"  # "reopen" #"lockdown"

exp_dir <- file.path(simulation_output, "forFitting", exp_name)
out_dir <- file.path(simulation_output, "forFitting", exp_name, "fitting")


if (!dir.exists(csv_dir)) dir.create(csv_dir)
if (!dir.exists(out_dir)) dir.create(out_dir)
if (!dir.exists(file.path(out_dir, "pre_fit"))) dir.create(file.path(out_dir, "pre_fit"))
if (!dir.exists(file.path(out_dir, "post_fit"))) dir.create(file.path(out_dir, "post_fit"))
if (!dir.exists(file.path(out_dir, "csv"))) dir.create(file.path(out_dir, "csv"))



## --------------------------------
#### Functions
## --------------------------------
load_sim_dat <- function(fittingParam, exp_name, i) {
  outcomeParam <- paste0(c("death_det_cumul", "crit_det", "hosp_det", "hosp_det_cumul", "infected_cumul"), paste0("_EMS-", i))
  KeepCols <- c("time", "startdate", "scen_num", "sample_num", fittingParam, outcomeParam)

  df <- fread(file.path(simulation_output, "forFitting", exp_name, "trajectoriesDat.csv"), select = KeepCols) %>%
    dplyr::mutate(
      region = i,
      startdate = as.Date(startdate),
      date = startdate + time
    ) %>%
    setNames(gsub(paste0("_EMS-", i), "", names(.))) %>%
    dplyr::filter(date <= stop_date & date >= start_date)


  grpVars <- c(fittingParam, "time")

  df <- df %>%
    dplyr::group_by(socialDistance_time7, social_multiplier_7, time, startdate) %>%
    dplyr::summarize(
      death_det_cumul = median(death_det_cumul),
      hosp_det_cumul = median(hosp_det_cumul),
      infected_cumul = median(infected_cumul),
      crit_det = median(crit_det),
      hosp_det = median(hosp_det)
    ) %>%
    dplyr::group_by(socialDistance_time7, social_multiplier_7) %>%
    mutate(scen_num = cur_group_id()) %>%
    as.data.table()

  timevars <- colnames(df)[grep("time", colnames(df))]
  df <- calculate_dates(df) %>% as.data.frame()


  df <- df %>%
    dplyr::group_by(scen_num) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(
      new_detected_deaths = 0,
      new_detected_deaths = death_det_cumul - lag(death_det_cumul),
      new_detected_hospitalized = 0, new_detected_hospitalized = hosp_det_cumul - lag(hosp_det_cumul),
      new_infected = 0, new_infected = infected_cumul - lag(infected_cumul)
    ) %>%
    dplyr::mutate(
      new_detected_deaths = ifelse(time == 0, 0, new_detected_deaths),
      new_detected_hospitalized = ifelse(time == 0, 0, new_detected_hospitalized),
      new_infected = ifelse(time == 0, 0, new_infected)
    ) %>%
    dplyr::mutate(
      new_detected_deaths = ifelse(is.na(new_detected_deaths), 0, new_detected_deaths),
      new_detected_hospitalized = ifelse(is.na(new_detected_hospitalized), 0, new_detected_hospitalized),
      new_infected = ifelse(is.na(new_infected), 0, new_infected),
    ) %>%
    as.data.frame()

  df$date <- as.Date(as.character(df$date))

  return(df)
}

load_ref_dat <- function(i, LLdate = "200928") {
  emresource_ems <- fread(file.path(data_path, "covid_IDPH/Corona virus reports/emresource_by_region.csv")) %>%
    dplyr::filter(covid_region %in% i) %>%
    dplyr::mutate(date = as.Date(date_of_extract)) %>%
    dplyr::filter(date >= start_date & date <= stop_date) %>%
    complete(date = seq.Date(min(date), max(date), by = "day"))

  emresource_ems[is.na(emresource_ems)] <- 0

  LL_ems <- fread(file.path(data_path, "covid_IDPH", "Cleaned Data", paste0(LLdate, "_jg_aggregated_covidregion.csv"))) %>%
    dplyr::filter(covid_region %in% i) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::filter(date >= start_date & date <= stop_date) %>%
    complete(date = seq.Date(min(date), max(date), by = "day"))

  emresource_ems$date <- as.Date(as.character(emresource_ems$date))
  LL_ems$date <- as.Date(as.character(LL_ems$date))


  ref_dat_list <- list(emresource_ems, LL_ems)
  names(ref_dat_list) <- c("emresource_ems", "LL_ems")

  return(ref_dat_list)
}

merge_sim_and_ref_dat <- function(sim_ems, i, LLdate = "200928") {
  emresource_ems <- load_ref_dat(i, LLdate = LLdate)[[1]]
  LL_ems <- load_ref_dat(i, LLdate = LLdate)[[2]]


  emresource_ems$filterVar <- 1
  LL_ems$filterVar <- 1

  sim_ems_emresource <- sim_ems %>%
    left_join(emresource_ems, by = "date") %>%
    filter(!is.na(filterVar))

  sim_ems_LL <- sim_ems %>%
    left_join(LL_ems, by = "date") %>%
    filter(!is.na(filterVar))

  # summary(sim_ems_emresource$date)
  # summary(sim_ems_LL$date)

  dat_list <- list(sim_ems_emresource, sim_ems_LL)
  names(dat_list) <- c("sim_ems_emresource", "sim_ems_LL")

  return(dat_list)
}

calculate_dates <- function(df) {
  timevars <- colnames(df)[grep("", colnames(df))]
  df[, ("startdate") := as.Date(get("startdate"))]
  for (timevar in timevars) {
    timevar_date <- gsub("time", "date", timevar)
    if (!(timevar_date %in% colnames(df))) {
      df[, timevar_date] <- df[, "startdate"] + df[, get(timevar)]
    }
  }
  return(df)
}

pre_fit_plot <- function(fittingVar, logscale = TRUE) {

  # fittingParam
  df <- as.data.frame(sim_ems_emresource)
  df$fittingVar <- df[, fittingVar]

  p1 <- ggplot(data = df) +
    geom_line(aes(x = date, y = crit_det, col = as.factor(fittingVar), group = scen_num)) +
    geom_point(aes(x = date, y = confirmed_covid_icu, group = scen_num)) +
    theme(legend.position = "None") +
    labs(title = "EMSrecourse\nconfirmed_covid_icu", subtitle = "", caption = paste0(fittingVar))

  p2 <- ggplot(data = df) +
    geom_line(aes(x = date, y = new_detected_deaths, col = as.factor(fittingVar), group = scen_num)) +
    geom_point(aes(x = date, y = confirmed_covid_deaths_prev_24h, group = scen_num)) +
    theme(legend.position = "None") +
    labs(title = "EMSrecourse\nconfirmed_covid_deaths_prev_24h", subtitle = "", caption = paste0(fittingVar))

  p3 <- ggplot(data = df) +
    geom_line(aes(x = date, y = hosp_det, col = as.factor(fittingVar), group = scen_num)) +
    geom_point(aes(x = date, y = covid_non_icu, group = scen_num)) +
    theme(legend.position = "None") +
    labs(title = "EMSrecourse\ncovid_non_icu", subtitle = "", caption = paste0(fittingVar))

  if (logscale == TRUE) {
    p1 <- p1 + scale_y_log10()
    p2 <- p2 + scale_y_log10()
    p3 <- p3 + scale_y_log10()
  }
  emr_plot <- plot_grid(p1, p2, p3, nrow = 1)

  df <- as.data.frame(sim_ems_LL)
  df$fittingVar <- df[, fittingVar]

  p1 <- ggplot(data = df) +
    geom_line(aes(x = date, y = new_detected_deaths, col = as.factor(fittingVar), group = scen_num)) +
    geom_point(aes(x = date, y = deaths, group = scen_num)) +
    theme(legend.position = "None") +
    labs(title = "Line List\nnew_detected_deaths", subtitle = "", caption = paste0(fittingVar))


  p2 <- ggplot(data = df) +
    geom_line(aes(x = date, y = new_detected_hospitalized, col = as.factor(fittingVar), group = scen_num)) +
    geom_point(aes(x = date, y = admissions, group = scen_num)) +
    theme(legend.position = "None") +
    labs(title = "Line List\nadmissions", subtitle = "", caption = paste0(fittingVar))

  if (logscale == TRUE) {
    p1 <- p1 + scale_y_log10()
    p2 <- p2 + scale_y_log10()
  }

  ll_plot <- plot_grid(p1, p2, nrow = 1)

  pplot <- plot_grid(emr_plot, ll_plot, nrow = 2)

  return(pplot)
}

f_paramplots <- function(use_values_dat, ems, fittingParam) {
  # use_values_dat=ems1_use_values

  best_param <- use_values_dat[which(use_values_dat$NLL == min(use_values_dat$NLL)), ]


  best_param$param1 <- best_param[, colnames(best_param) == fittingParam[1]]
  best_param$param2 <- best_param[, colnames(best_param) == fittingParam[2]]

  pplot <- ggplot(best_param, aes(x = param1, y = param2)) +
    geom_point() +
    labs(fill = "NLL") +
    ylim(0, 1) +
    labs(title = "", subtitle = ems, x = fittingParam[1], y = fittingParam[2]) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )


  ggsave(paste0(ems, "_best_parameter_plots.png"),
    plot = pplot,
    path = file.path(out_dir, "best_parameter_plots/"), width = 13, height = 6, device = "png"
  )

  return(pplot)
}

f_post_fit_plot <- function(use_values_dat, ems, logscale = TRUE) {
  df <- as.data.frame(sim_ems_emresource)
  df <- subset(df, df$scen_num %in% row.names(use_values_dat))

  use_values_dat$scen_num_fit <- rownames(use_values_dat)
  minNLL <- use_values_dat %>% filter(NLL == min(NLL))
  minNLL <- minNLL[1, ]
  dfMin <- subset(df, df$scen_num %in% minNLL$scen_num_fit)

  p1 <- ggplot(data = df) +
    geom_line(aes(x = date, y = crit_det, group = scen_num), col = "deepskyblue3") +
    geom_line(data = dfMin, aes(x = date, y = crit_det, group = scen_num), col = "red", size = 1.1) +
    geom_point(aes(x = date, y = confirmed_covid_icu, group = scen_num)) +
    theme(legend.position = "None") +
    labs(title = "EMSrecourse\nconfirmed_covid_icu", subtitle = "")

  p2 <- ggplot(data = df) +
    geom_line(aes(x = date, y = new_detected_deaths, group = scen_num), col = "deepskyblue3") +
    geom_line(data = dfMin, aes(x = date, y = new_detected_deaths, group = scen_num), col = "red", size = 1.1) +
    geom_point(aes(x = date, y = confirmed_covid_deaths_prev_24h, group = scen_num)) +
    theme(legend.position = "None") +
    labs(title = "EMSrecourse\nconfirmed_covid_deaths_prev_24h", subtitle = "")

  p3 <- ggplot(data = df) +
    geom_line(aes(x = date, y = hosp_det, group = scen_num), col = "deepskyblue3") +
    geom_line(data = dfMin, aes(x = date, y = hosp_det, group = scen_num), col = "red", size = 1.1) +
    geom_point(aes(x = date, y = covid_non_icu, group = scen_num)) +
    theme(legend.position = "None") +
    labs(title = "EMSrecourse\ncovid_non_icu", subtitle = "")

  if (logscale == TRUE) {
    p1 <- p1 + scale_y_log10()
    p2 <- p2 + scale_y_log10()
    p3 <- p3 + scale_y_log10()
  }

  emr_plot <- plot_grid(p1, p2, p3, nrow = 1)
  rm(p1, p2, p3, df)

  df <- as.data.frame(sim_ems_LL)
  df <- subset(df, df$scen_num %in% row.names(use_values_dat))

  use_values_dat$scen_num_fit <- rownames(use_values_dat)
  minNLL <- use_values_dat %>% filter(NLL == min(NLL))
  minNLL <- minNLL[1, ]
  dfMin <- subset(df, df$scen_num %in% minNLL$scen_num_fit)

  p1 <- ggplot(data = df) +
    geom_line(aes(x = date, y = new_detected_deaths, group = scen_num), col = "deepskyblue3") +
    geom_line(data = dfMin, aes(x = date, y = new_detected_deaths, group = scen_num), col = "red", size = 1.1) +
    geom_point(aes(x = date, y = deaths, group = scen_num)) +
    theme(legend.position = "None") +
    labs(title = "Line List\nnew_detected_deaths", subtitle = "")


  p2 <- ggplot(data = df) +
    geom_line(aes(x = date, y = new_detected_hospitalized, group = scen_num), col = "deepskyblue3") +
    geom_line(data = dfMin, aes(x = date, y = new_detected_hospitalized, group = scen_num), col = "red", size = 1.1) +
    geom_point(aes(x = date, y = admissions, group = scen_num)) +
    theme(legend.position = "None") +
    labs(title = "Line List\nadmissions", subtitle = "")

  if (logscale) {
    p1 <- p1 + scale_y_log10()
    p2 <- p2 + scale_y_log10()
  }

  ll_plot <- plot_grid(p1, p2, nrow = 1)

  pplot <- plot_grid(emr_plot, ll_plot, nrow = 2)

  ggsave(paste0(i, "_post_fit_plot.png"),
    plot = pplot,
    path = file.path(out_dir, "post_fit"), width = 13, height = 10, device = "png"
  )

  return(pplot)
}

f_run_fitting <- function(sim_ems_emresource, sim_ems_LL, scens) {

  # create data.frame to hold likelihood results
  ems_output <- matrix(0, length(scens), length(fittingParam) + 2)
  ems_output <- as.data.frame(ems_output)
  colnames(ems_output) <- c(fittingParam, "NLL", "region")
  ems_output[, "region"] <- i

  # loop over simulation scenarios and record likelihood
  for (j in 1:length(scens)) {
    print(paste0("scenario ", j))
    # pull out all simulation values of given parameter value
    emresource_sub <- sim_ems_emresource[which(sim_ems_emresource$scen_num == scens[j]), ]
    LL_sub <- sim_ems_LL[which(sim_ems_LL$scen_num == scens[j]), ]

    ### EMResource data
    # Likelihood of simulation generating detected critical
    nll1 <- -1 * sum(dpois(emresource_sub$confirmed_covid_icu, emresource_sub$crit_det + 1e-10, log = T))

    # Likelihood of simulations generating admission data
    nll2 <- -1 * sum(dpois(emresource_sub$covid_non_icu, emresource_sub$hosp_det + 1e-10, log = T))

    ### Line list  data
    # Likelihood of simulations creating death data that doesn't come from EMresource.
    nll3 <- -1 * sum(dpois(LL_sub$deaths, LL_sub$new_detected_deaths + 1e-10, log = T), na.rm = TRUE)


    # Sum all Likelihood, weighting emresource data higher
    nll <- nll1 + nll2 + nll3

    # Put Likelihood values and corresponding start date and Ki in output dataframe
    for (z in c(1:length(fittingParam))) {
      ems_output[j, z] <- unique(emresource_sub[, fittingParam[z]])
    }

    ems_output[j, "NLL"] <- nll

    rm(nll, nll1, nll2, nll3, emresource_sub, LL_sub)
  }


  # Create sheet of the 5% most likely parameter combinations
  use_values <- (ems_output[which(ems_output$NLL < quantile(ems_output$NLL, prob = 1 - 95 / 100)), ])

  return(use_values)
}

f_export_sumary_csv <- function(use_values_list, NLL, npairs = 10) {
  do.call(rbind.data.frame, use_values_list) %>%
    dplyr::group_by(region) %>%
    dplyr::filter(NLL == min(NLL)) %>%
    fwrite(file.path(out_dir, "csv", "best_parameters_emsAll.csv"))

  do.call(rbind.data.frame, use_values_list) %>%
    dplyr::group_by(region) %>%
    dplyr::filter(NLL <= median(NLL)) %>%
    dplyr::summarise_at(.vars = fittingParam, .funs = c("min", "max")) %>%
    fwrite(file.path(out_dir, "csv", "range_parameters_emsAll.csv"))

  do.call(rbind.data.frame, use_values_list) %>%
    dplyr::group_by(region) %>%
    dplyr::arrange(NLL) %>%
    slice(1:npairs) %>%
    dplyr::select(-NLL) %>%
    as.data.frame() %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(counter = 1:n()) %>%
    pivot_wider(names_from = "region", values_from = c("socialDistance_time7", "social_multiplier_7")) %>%
    fwrite(file.path(out_dir, "csv", "best_n_pairs_parameters_emsAll.csv"))
}

## ----------------------------------------------



## --------------------------------
#### Settings
## --------------------------------
LLdate <- "200928"
if (fitstep == "initial") {
  fittingParam <- c("time_infection_import", "Ki") ###  initial
  start_date <- as.Date("2020-01-01")
  stop_date <- as.Date("2020-04-01")
}
if (fitstep == "reopen") {
  fittingParam <- c(
    "social_multiplier_5", "social_multiplier_6", "social_multiplier_7",
    "socialDistance_time5", "socialDistance_time6", "socialDistance_time7"
  ) ## reopen all

  start_date <- as.Date("2020-06-01")
  stop_date <- as.Date("2020-10-01")
}
if (fitstep == "lockdown") {
  fittingParam <- c(
    "social_multiplier_3", "social_multiplier_4",
    "socialDistance_time3", "socialDistance_time4"
  ) ## reopen all

  start_date <- as.Date("2020-03-01")
  stop_date <- as.Date("2020-5-20")
}
if (fitstep == "newparam") {
  fittingParam <- c("socialDistance_time7", "social_multiplier_7") ### reopen

  start_date <- as.Date("2020-08-01")
  stop_date <- as.Date("2020-9-30")
}


## --------------------------------
#### Pre-fit plots
## --------------------------------
for (i in c(1:11)) {
  print(paste0("Region ", i))

  sim_ems <- load_sim_dat(fittingParam, exp_name, i)

  str(sim_ems)
  summary(sim_ems$date)
  sapply(sim_ems[fittingParam], summary)
  sapply(sim_ems[fittingParam], unique)

  ### Prepare for merge and merge ref_dat to sim_dat
  sim_ems_emresource <- merge_sim_and_ref_dat(sim_ems, i, LLdate = LLdate)[[1]]
  sim_ems_LL <- merge_sim_and_ref_dat(sim_ems, i, LLdate = LLdate)[[2]]

  for (paramVar in fittingParam) {
    pplot <- pre_fit_plot(fittingVar = paramVar, logscale = TRUE)

    ggsave(paste0(i, "_pre_fit_plot_", paramVar, ".png"),
      plot = pplot,
      path = file.path(out_dir, "pre_fit"), width = 13, height = 10, device = "png"
    )
  }
}


## --------------------------------
### Fitting and post-fit plots
## --------------------------------

## loop begin over EMS regions
use_values_list <- list()
for (i in c(1:11)) {
  print(paste0("Region ", i))

  sim_ems <- load_sim_dat(fittingParam, exp_name, i)

  str(sim_ems)
  summary(sim_ems$date)
  sapply(sim_ems[fittingParam], summary)
  sapply(sim_ems[fittingParam], unique)

  ### Prepare for merge and merge ref_dat to sim_dat
  sim_ems_emresource <- merge_sim_and_ref_dat(sim_ems, i, LLdate = LLdate)[[1]]
  sim_ems_LL <- merge_sim_and_ref_dat(sim_ems, i, LLdate = LLdate)[[2]]

  ## get a list of all scenario numbers run for this EMS
  scens <- unique(sim_ems_emresource$scen_num)

  use_values <- f_run_fitting(sim_ems_emresource, sim_ems_LL, scens)
  use_values_list[[i]] <- use_values

  #### Generate plots and save csv's
  plot_and_save <- T
  if (plot_and_save) {
    pplot <- f_post_fit_plot(use_values_dat = use_values, ems = i, logscale = TRUE)

    # Save best paramter combinations
    fwrite(use_values, file.path(out_dir, "csv", paste0("best_parameter_ranges_ems_", i, ".csv")), quote = FALSE, row.names = FALSE)
  }

  rm(sim_ems_emresource, sim_ems, scens, use_values)
}


## --------------------------------
### Export summary csv files with best parameter for all regions
## --------------------------------
f_export_sumary_csv(use_values_list, npairs = 10)
