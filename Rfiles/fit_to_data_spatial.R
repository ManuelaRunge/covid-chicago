### ----------------------------------------------------------------------------------------------
# This script takes hospital data from various EMS regions of Illinois
### ----------------------------------------------------------------------------------------------
# Input:
# - simulation trajectoriesDat.csv located in projects\covid_chicago\cms_sim\simulation_output\<exp_name>
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
### ----------------------------------------------------------------------------------------------



## set working directory to R.project location
library(ggplot2)
library(data.table)
library(tidyverse)
library(zoo) ## for 7 day rolling average

library(cowplot)
theme_set(theme_cowplot())


runInBatchMode <- F

if (runInBatchMode) {
  cmd_agrs <- commandArgs()
  length(cmd_agrs)
  exp_name <- cmd_agrs[length(cmd_agrs) - 3]
  useSmoothedData <- cmd_agrs[length(cmd_agrs) - 2]
  Location <- cmd_agrs[length(cmd_agrs) - 1]
  workingDir <- cmd_agrs[length(cmd_agrs)]

  if (tolower(useSmoothedData) == "false") useSmoothedData <- FALSE
  if (tolower(useSmoothedData) == "true") useSmoothedData <- TRUE
} else {
  exp_name <- "_forFitting/20201024_IL_mr_20201003_IL_mr_fitkistartsm3_sm4and5"

  Location <- "Local"
  workingDir <- getwd()
  useSmoothedData <- TRUE
  weightDeath <- FALSE
  excludeDeaths <- TRUE
  includeLLadmissions <- FALSE
  includeCLIadmissions <- FALSE
  excludeEMRnonICU <- FALSE
  
  #### TODO fix to automatic selection , needs manual editing when multiplier + time is fitted vs multiplier only or multiple multipliers
  fittingParam_kistartsm3 <- c('Ki','time_infection_import','ki_multiplier_3c')
  fittingParam_Ki <- c('Ki')
  fittingParam_4 <- c( "ki_multiplier_5",  "ki_multiplier_time_5")
  fittingParam_45 <- c( "ki_multiplier_4", "ki_multiplier_5", "ki_multiplier_time_4", "ki_multiplier_time_5")
  fittingParam_67 <- c( "ki_multiplier_6", "ki_multiplier_7", "ki_multiplier_time_6", "ki_multiplier_time_7")
  fittingParam_89 <- c( "ki_multiplier_8", "ki_multiplier_9", "ki_multiplier_time_8", "ki_multiplier_time_9")
  fittingParam_fit5 <- c(
    'Ki_EMS_1',	'Ki_EMS_2', 'Ki_EMS_3', 'Ki_EMS_4'	,'Ki_EMS_5','Ki_EMS_6',	'Ki_EMS_7', 'Ki_EMS_8', 'Ki_EMS_9'	,'Ki_EMS_10', 'Ki_EMS_11',
    'ki_multiplier_4_EMS_1',	'ki_multiplier_4_EMS_2', 'ki_multiplier_4_EMS_3', 'ki_multiplier_4_EMS_4'	,'ki_multiplier_4_EMS_5','ki_multiplier_4_EMS_6',	'ki_multiplier_4_EMS_7', 'ki_multiplier_4_EMS_8', 'ki_multiplier_4_EMS_9'	,'ki_multiplier_4_EMS_10', 'ki_multiplier_4_EMS_11',
    'ki_multiplier_5_EMS_1',	'ki_multiplier_5_EMS_2', 'ki_multiplier_5_EMS_3', 'ki_multiplier_5_EMS_4'	,'ki_multiplier_5_EMS_5','ki_multiplier_5_EMS_6',	'ki_multiplier_5_EMS_7', 'ki_multiplier_5_EMS_8', 'ki_multiplier_5_EMS_9'	,'ki_multiplier_5_EMS_10', 'ki_multiplier_5_EMS_11',
    'ki_multiplier_6_EMS_1',	'ki_multiplier_6_EMS_2', 'ki_multiplier_6_EMS_3', 'ki_multiplier_6_EMS_4'	,'ki_multiplier_6_EMS_5','ki_multiplier_6_EMS_6',	'ki_multiplier_6_EMS_7', 'ki_multiplier_6_EMS_8', 'ki_multiplier_6_EMS_9'	,'ki_multiplier_6_EMS_10', 'ki_multiplier_6_EMS_11',
    'ki_multiplier_7_EMS_1',	'ki_multiplier_7_EMS_2', 'ki_multiplier_7_EMS_3', 'ki_multiplier_7_EMS_4'	,'ki_multiplier_7_EMS_5','ki_multiplier_7_EMS_6',	'ki_multiplier_7_EMS_7', 'ki_multiplier_7_EMS_8', 'ki_multiplier_7_EMS_9'	,'ki_multiplier_7_EMS_10', 'ki_multiplier_7_EMS_11',
    'ki_multiplier_8_EMS_1',	'ki_multiplier_8_EMS_2', 'ki_multiplier_8_EMS_3', 'ki_multiplier_8_EMS_4'	,'ki_multiplier_8_EMS_5','ki_multiplier_8_EMS_6',	'ki_multiplier_8_EMS_7', 'ki_multiplier_8_EMS_8', 'ki_multiplier_8_EMS_9'	,'ki_multiplier_8_EMS_10', 'ki_multiplier_8_EMS_11',
    'ki_multiplier_9_EMS_1',	'ki_multiplier_9_EMS_2', 'ki_multiplier_9_EMS_3', 'ki_multiplier_9_EMS_4'	,'ki_multiplier_9_EMS_5','ki_multiplier_9_EMS_6',	'ki_multiplier_9_EMS_7', 'ki_multiplier_9_EMS_8', 'ki_multiplier_9_EMS_9'	,'ki_multiplier_9_EMS_10', 'ki_multiplier_9_EMS_11')
  
  fittingParam <- fittingParam_Ki
  #fittingParam <-  c(fittingParam_89,fittingParam_67) #c(fittingParam_45,fittingParam_67,fittingParam_89)
  
  start_date <- as.Date("2020-07-01") # as.Date(paste0("2020-", monthnr, "-01"))
  stop_date <- as.Date("2020-08-01") #  start_date + 30
  #start_date <- as.Date("2020-08-01") # as.Date(paste0("2020-", monthnr, "-01"))
  # stop_date <- as.Date("2020-10-01") #  start_date + 30
  
}

## Print out for log
print(exp_name)
print(Location)
print(useSmoothedData)
## --------------------------------
### Set working directory to the GitHub repository R files
## --------------------------------
source("load_paths.R")
source("processing_helpers.R")
setwd(workingDir)


## --------------------------------
### Experiment specifc parameters
## --------------------------------
exp_name_split <- str_split(exp_name, "_")[[1]]
simdate <- exp_name_split[1]
#monthnr <- gsub("fitki", "", exp_name_split[length(exp_name_split)])

smooth_n_days <- 7
exp_dir <- file.path(simulation_output, exp_name)
out_dir <- file.path(simulation_output, exp_name, "fitting")

if (!dir.exists(out_dir)) dir.create(out_dir)
if (!dir.exists(file.path(out_dir, "pre_fit"))) dir.create(file.path(out_dir, "pre_fit"))
if (!dir.exists(file.path(out_dir, "post_fit"))) dir.create(file.path(out_dir, "post_fit"))
if (!dir.exists(file.path(out_dir, "csv"))) dir.create(file.path(out_dir, "csv"))



## --------------------------------
#### Functions
## --------------------------------
## Note: All directories need to be defined before executing the functions via the load_paths.R.

## Data management
load_sim_dat <- function(fittingParam, exp_name, i, start_date, stop_date, fname = "trajectoriesDat.csv",  samplePlot=TRUE) {

  #' Load simulation outputs and do basic data operations
  #'
  #' Load simulation csv file, then calculate the date of the simulation
  #' and is aggregating scenarios per fitting parameter
  #' and calculated incidence measures for fitting
  #' @param fittingParam vector including the names of the parameters to estimate by fitting
  #' @param exp_name name of the simulation experiment
  #' @param i identifier for the region (1 to 11)
  #' @param start_date first date of the time period to fit
  #' @param stop_date  last date of the time period to fit
  #' @param fname name of the simulation output file, default is 'trajectoriesDat.csv', often used is also 'trajectoriesDat_trim.csv'
  
  outcomeParam <- paste0(c("death_det_cumul", "crit_det", "hosp_det", "hosp_det_cumul", "infected_cumul"))
  KeepCols <- c("time", "startdate", "scen_num",  "sample_num", fittingParam, outcomeParam)
  
  if (sum(grep(".Rdata", fname), grep(".RData", fname)) == 1) {
    load(file.path(exp_dir, fname))
    subdat = as.data.frame(subdat)
    df <- na.omit(subdat)
    
    rm(subdat)
  }
  
  if (sum(grep(".csv", fname)) == 1) {
    df <- fread(file.path(simulation_output, exp_name, fname), select = KeepCols)
  }

  df <- df %>%
    dplyr::mutate(
      region = i,
      startdate = as.Date(as.character(startdate)),
      # startdate = as.Date(as.character(startdate), format = "%m/%d/%Y"), #, format = "%m/%d/%Y"
      date = startdate + time
    ) %>%
    setNames(gsub(paste0("_EMS-", i), "", names(.))) %>%
    as.data.frame()
  
  
  ### Prepare for merge and merge ref_dat to sim_dat
  if(samplePlot){
    
    emresource_ems <- fread(file.path(data_path, "covid_IDPH/Corona virus reports/emresource_by_region.csv")) %>%
      dplyr::filter(covid_region %in% i) %>%
      dplyr::mutate(date = as.Date(date_of_extract)) %>%
      complete(date = seq.Date(min(date), max(date), by = "day"))
    
    nsamples=500
    pplot <- ggplot(data=subset(df, scen_num %in% sample(df$scen_num,nsamples, replace = F) )) +
      geom_line(aes(x=date, y=crit_det, group=scen_num), col="deepskyblue3", alpha=0.3)+
      geom_point(data=emresource_ems,aes(x=date, y=confirmed_covid_icu)) +
      labs(title=paste0("region ", i),
           subtitle = ("confirmed_covid_icu, df"), 
           caption=paste0("subset for ", nsamples))+
      facet_wrap(~region)
    
    ggsave(paste0(i, "_samplePlot.png"),
           plot = pplot,
           path = file.path(out_dir, "pre_fit"), width = 13, height = 10, device = "png"
    )
  }
  
  
  aggregateDat=TRUE
  if(aggregateDat){
    
    grpVars <- c(fittingParam, "time", "date")
    
    df <- df %>%
      dplyr::group_by_at(.vars = grpVars) %>%
      dplyr::summarize(
        death_det_cumul = median(death_det_cumul),
        hosp_det_cumul = median(hosp_det_cumul),
        infected_cumul = median(infected_cumul),
        crit_det = median(crit_det),
        hosp_det = median(hosp_det)
      ) %>%
      dplyr::group_by_at(.vars = fittingParam) %>%
      mutate(scen_num = cur_group_id(),
             sample_num = "aggregated_median") %>%
      as.data.table()
  }
  
  grpVars <- c(fittingParam, "scen_num")
  
  df <- df  %>%
    dplyr::filter(date <= as.Date(stop_date) & date >= as.Date(start_date)) %>%
    dplyr::group_by_at(.vars=grpVars) %>%
    dplyr::arrange(date) %>%
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

load_ref_dat <- function(i, start_date, stop_date, smooth_n_days) {

  #' Load reference data  and do basic data operations
  #'
  #' Load reference data, EMResource and Line list data
  #' @param i identifier for the region (1 to 11)
  #' @param start_date first date of the time period to fit
  #' @param stop_date  last date of the time period to fit
  #' @param LL_file_date date of the latest line list data
  #'

  emresource_ems <- fread(file.path(data_path, "covid_IDPH/Corona virus reports/emresource_by_region.csv")) %>%
    dplyr::filter(covid_region %in% i) %>%
    dplyr::mutate(date = as.Date(date_of_extract)) %>%
    dplyr::filter(date >= start_date & date <= stop_date) %>%
    complete(date = seq.Date(min(date), max(date), by = "day"))

  emresource_ems[is.na(emresource_ems)] <- 0

  LLdir <- file.path(data_path, "covid_IDPH", "Cleaned Data")
  LLfiles <- list.files(LLdir)[grep("aggregated_covidregion", list.files(LLdir))]
  LL_file_dates <- as.numeric(gsub("_jg_aggregated_covidregion.csv", "", LLfiles))
  LL_file_date <- max(LL_file_dates)


  LL_ems <- fread(file.path(data_path, "covid_IDPH", "Cleaned Data", paste0(LL_file_date, "_jg_aggregated_covidregion.csv"))) %>%
    dplyr::filter(covid_region %in% i) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::filter(date >= start_date & date <= stop_date) %>%
    complete(date = seq.Date(min(date), max(date), by = "day"))

  county_covidregion <- fread(file.path(data_path, "covid_IDPH/EMS Population/covidregion_population_by_county.csv")) %>% mutate(County = tolower(County))
  CLI_ems <- fread(file.path(data_path, "covid_IDPH/Corona virus reports/CLI_admissions.csv")) %>%
    mutate(County = tolower(region)) %>%
    left_join(county_covidregion, by = "County") %>%
    rename(covid_region = new_restore_region) %>%
    group_by(date, covid_region) %>%
    summarize(inpatient = sum(inpatient)) %>%
    dplyr::filter(covid_region %in% i) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::filter(date >= start_date & date <= stop_date) %>%
    complete(date = seq.Date(min(date), max(date), by = "day"))


  emresource_ems$date <- as.Date(as.character(emresource_ems$date))
  LL_ems$date <- as.Date(as.character(LL_ems$date))
  CLI_ems$date <- as.Date(as.character(CLI_ems$date))


  ### Add 7 day rolling average to test fitting to raw vs smoothed
  emresource_ems <- emresource_ems %>%
    group_by(covid_region) %>%
    arrange(date) %>%
    mutate(
      confirmed_covid_icu_avrg = round(rollmean(confirmed_covid_icu, smooth_n_days, align = "right", fill = 0), 0),
      covid_non_icu_avrg = round(rollmean(covid_non_icu, smooth_n_days, align = "right", fill = 0), 0)
    )

  LL_ems <- LL_ems %>%
    group_by(covid_region) %>%
    arrange(date) %>%
    mutate(
      deaths_avrg = round(rollmean(deaths, smooth_n_days, align = "right", fill = 0), 0),
      admissions_avrg = round(rollmean(admissions, smooth_n_days, align = "right", fill = 0), 0)
    )

  CLI_ems <- CLI_ems %>%
    group_by(covid_region) %>%
    arrange(date) %>%
    mutate(
      inpatient_avrg = round(rollmean(inpatient, smooth_n_days, align = "right", fill = 0), 0)
    )



  ref_dat_list <- list(emresource_ems, LL_ems, CLI_ems)
  names(ref_dat_list) <- c("emresource_ems", "LL_ems", "CLI_ems")

  return(ref_dat_list)
}

merge_sim_and_ref_dat <- function(sim_ems, i, start_date, stop_date, smooth_n_days) {

  #' Load reference data  and do basic data operations
  #'
  #' Load reference data, EMResource and Line list data
  #' @param sim_ems minimum date of  the time period to fit to
  #' @param i identifier for the region (1 to 11)
  #' @param start_date first date of the time period to fit
  #' @param stop_date  last date of the time period to fit
  #' @param LL_file_date date of the lates line list data
  #'

  ref_dfs <- load_ref_dat(i, start_date, stop_date, smooth_n_days)
  emresource_ems <- ref_dfs[[1]]
  LL_ems <- ref_dfs[[2]]
  CLI_ems <- ref_dfs[[3]]
  rm(ref_dfs)


  emresource_ems$filterVar <- 1
  LL_ems$filterVar <- 1
  CLI_ems$filterVar <- 1

  sim_ems_emresource <- sim_ems %>%
    dplyr::left_join(emresource_ems, by = "date") %>%
    dplyr::filter(!is.na(filterVar))

  sim_ems_LL <- sim_ems %>%
    left_join(LL_ems, by = "date") %>%
    filter(!is.na(filterVar))

  sim_ems_CLI <- sim_ems %>%
    left_join(CLI_ems, by = "date") %>%
    filter(!is.na(filterVar))
  # summary(sim_ems_emresource$date)
  # summary(sim_ems_LL$date)

  dat_list <- list(sim_ems_emresource, sim_ems_LL, sim_ems_CLI)
  names(dat_list) <- c("sim_ems_emresource", "sim_ems_LL", "sim_ems_CLI")

  return(dat_list)
}

f_post_fit_plot <- function(use_values_dat, i, logscale = TRUE) {

  #' Generates custom pre-fitting plots to assess whether data lies within simulated parameter space
  #'
  #' @param use_values_dat matrix for one region produced from f_run_fitting that includes the best 5% scenarios
  #' @param i region , previously called ems (1 to)
  #' @param logscale boolean, if TRUE uses  log scale for y axis
  #'
  #'
  df <- as.data.frame(sim_ems_emresource)
  df <- subset(df, df$scen_num %in% unique(use_values_dat$scen_num))

  minNLL <- use_values_dat %>% filter(NLL == min(NLL))
  minNLL <- minNLL[1, ]
  dfMin <- subset(df, df$scen_num == minNLL$scen_num)

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
  df <- subset(df, df$scen_num %in% unique(use_values_dat$scen_num))
  
  minNLL <- use_values_dat %>% filter(NLL == min(NLL))
  minNLL <- minNLL[1, ]
  dfMin <- subset(df, df$scen_num == minNLL$scen_num)

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


  df <- as.data.frame(sim_ems_CLI)
  df <- subset(df, df$scen_num %in% unique(use_values_dat$scen_num))
  

  minNLL <- use_values_dat %>% filter(NLL == min(NLL))
  minNLL <- minNLL[1, ]
  dfMin <- subset(df, df$scen_num == minNLL$scen_num)

  p3 <- ggplot(data = df) +
    geom_line(aes(x = date, y = new_detected_hospitalized, group = scen_num), col = "deepskyblue3") +
    geom_line(data = dfMin, aes(x = date, y = new_detected_hospitalized, group = scen_num), col = "red", size = 1.1) +
    geom_point(aes(x = date, y = inpatient, group = scen_num)) +
    theme(legend.position = "None") +
    labs(title = "IDPH\nCLI_admissions", subtitle = "")

  if (logscale) {
    p3 <- p3 + scale_y_log10()
  }


  ll_plot <- plot_grid(p1, p2, p3, nrow = 1)

  pplot <- plot_grid(emr_plot, ll_plot, nrow = 2)

  ggsave(paste0(i, "_post_fit_plot.png"),
         plot = pplot,
         path = file.path(out_dir, "post_fit"), width = 13, height = 10, device = "png"
  )

  return(pplot)
}

f_run_fitting <- function(i, sim_ems_emresource, sim_ems_LL, sim_ems_CLI, scens, useSmoothedData = TRUE, weightDeath = FALSE,
                          excludeDeaths = FALSE, includeLLadmissions = FALSE, includeCLIadmissions = FALSE, excludeEMRnonICU = FALSE) {

  #' Fitting function
  #'
  #' Calculated negative log likelihoods per scenario and selects the minimum 5%
  #' Optional includes smoothing using 7 day rolling average before fitting
  #' @param sim_ems_emresource combined simulation with EMresource data, generated by merge_sim_and_ref_dat
  #' @param sim_ems_LL combined simulation with Line List data, generated by merge_sim_and_ref_dat
  #' @param scens simulation scenarios (scen_num) that successfully ran per region (only successes are included in the trajectoriesDat.csv)
  #' @param useSmoothedData boolean, if TRUE takes 7 day rolling average for fitting

  # create data.frame to hold likelihood results
  ems_output <- matrix(0, length(scens), length(fittingParam) + 4)
  ems_output <- as.data.frame(ems_output)
  colnames(ems_output) <- c("row_num", "scen_num", "region", "NLL", fittingParam)
  ems_output[, "region"] <- i

  # loop over simulation scenarios and record likelihood
  count <- 0
  for (j in scens) {
    count <- count + 1
    print(paste0("scenario ", j))

    # pull out all simulation values of given parameter value
    emresource_sub <- sim_ems_emresource[which(sim_ems_emresource$scen_num ==j), ]
    LL_sub <- sim_ems_LL[which(sim_ems_LL$scen_num == j), ]
    CLI_sub <- sim_ems_CLI[which(sim_ems_CLI$scen_num == j), ]

    if (useSmoothedData == FALSE) {
      nll1 <- -1 * sum(dpois(emresource_sub$confirmed_covid_icu, emresource_sub$crit_det + 1e-10, log = T), na.rm = TRUE)
      nll2 <- -1 * sum(dpois(emresource_sub$covid_non_icu, emresource_sub$hosp_det + 1e-10, log = T))
      nll3 <- -1 * sum(dpois(LL_sub$deaths, LL_sub$new_detected_deaths + 1e-10, log = T), na.rm = TRUE)
      nll4 <- -1 * sum(dpois(LL_sub$admissions, LL_sub$new_detected_hospitalized + 1e-10, log = T), na.rm = TRUE)
      nll5 <- -1 * sum(dpois(CLI_sub$inpatient, CLI_sub$new_detected_hospitalized + 1e-10, log = T), na.rm = TRUE)
    }

    if (useSmoothedData) {
      nll1 <- -1 * sum(dpois(emresource_sub$confirmed_covid_icu_avrg, emresource_sub$crit_det + 1e-10, log = T), na.rm = TRUE)
      nll2 <- -1 * sum(dpois(emresource_sub$covid_non_icu_avrg, emresource_sub$hosp_det + 1e-10, log = T), na.rm = TRUE)
      nll3 <- -1 * sum(dpois(LL_sub$deaths_avrg, LL_sub$new_detected_deaths + 1e-10, log = T), na.rm = TRUE)
      nll4 <- -1 * sum(dpois(LL_sub$admissions_avrg, LL_sub$new_detected_hospitalized + 1e-10, log = T), na.rm = TRUE)
      nll5 <- -1 * sum(dpois(CLI_sub$inpatient_avrg, CLI_sub$new_detected_hospitalized + 1e-10, log = T), na.rm = TRUE)
    }

    # Sum all Likelihood
    nll <- nll1 + nll2 + nll3
    if (weightDeath) nll <- nll1 + nll2 + (nll3) * (length(emresource_sub$confirmed_covid_icu) / length(LL_sub$cases)) / 2
    if (excludeDeaths) nll <- nll1 + nll2

    if (includeLLadmissions & includeCLIadmissions == F) {
      nll <- nll1 + nll2 + nll3 + nll4
      if (weightDeath) nll <- nll1 + nll2 + (nll3 + nll4) * (length(emresource_sub$confirmed_covid_icu) / length(LL_sub$cases)) / 2
      if (excludeDeaths) nll <- nll1 + nll2 + nll4
    }
    if (includeLLadmissions & includeCLIadmissions) {
      nll <- nll1 + nll2 + nll3 + nll4 + nll5
      if (weightDeath) nll <- nll1 + nll2 + nll5 + (nll3 + nll4) * (length(emresource_sub$confirmed_covid_icu) / length(LL_sub$cases)) / 2
      if (excludeDeaths) nll <- nll1 + nll2 + nll4 + nll5
    }
    if (excludeEMRnonICU) {
      nll <- nll - nll2
    }


    # Put Likelihood values and corresponding start date and Ki in output dataframe
    for (z in c(1:length(fittingParam))) {
      ems_output[count, z + 4] <- unique(emresource_sub[, fittingParam[z]])
    }

    ems_output[count, "NLL"] <- nll
    ems_output[count, "scen_num"] <- j
    ems_output[count, "row_num"] <- count

    rm(j, nll, nll1, nll2, nll3, nll4, nll5, emresource_sub, LL_sub, CLI_sub)
  }


  # Create sheet of the 5% most likely parameter combinations
  use_values <- (ems_output[which(ems_output$NLL < quantile(ems_output$NLL, prob = 1 - 95 / 100)), ])

  return(use_values)
}

f_export_sumary_csv <- function(use_values_list, fittingParam, npairs = 10, writeYamlSnippet = FALSE) {

  #' Export csv files with different format of best parameters
  #'
  #' Output files:
  #' best_parameters_emsAll.csv  - 1 parameter set per region
  #' best_parameter_ranges_combined.csv  - all 5% per region
  #' range_parameters_emsAll.csv - summarized min and max per parameter per region
  #' best_n_pairs_parameters_emsAll.csv - n parameter pairs per region
  #' @param use_values_list list produced from f_run_fitting that includes the best 5% scenarios
  #' @param npairs defines how many parameter pairs to write out per region
  #'
  do.call(rbind.data.frame, use_values_list) %>%
    dplyr::group_by(region) %>%
    dplyr::filter(NLL == min(NLL)) %>%
    fwrite(file.path(out_dir, "csv", "best_parameters_emsAll.csv"))

  do.call(rbind.data.frame, use_values_list) %>%
    fwrite(file.path(out_dir, "csv", "best_parameter_ranges_combined.csv"))

  do.call(rbind.data.frame, use_values_list) %>%
    dplyr::group_by(region) %>%
    dplyr::filter(NLL <= median(NLL)) %>%
    dplyr::summarise_at(.vars = fittingParam, .funs = c("min", "max")) %>%
    fwrite(file.path(out_dir, "csv", "range_parameters_emsAll.csv"))

  do.call(rbind.data.frame, use_values_list) %>%
    dplyr::group_by(region) %>%
    dplyr::arrange(NLL, by_group = TRUE) %>%
    slice(1:npairs) %>%
    dplyr::select(-NLL) %>%
    as.data.frame() %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(counter = 1:n()) %>%
    pivot_wider(names_from = "region", values_from = fittingParam) %>%
    fwrite(file.path(out_dir, "csv", "best_n_pairs_parameters_emsAll.csv"))


  #### Generate yaml snippet
  if (writeYamlSnippet) {
    range_dat <- fread(file.path(out_dir, "csv", "range_parameters_emsAll.csv")) %>% as.data.frame()
    yaml_snippet <- suppressWarnings(readLines(file.path(git_dir, "experiment_configs", "snippets", "templates", "config_weekly_ki_multiplier_fit.txt")))
    yaml_snippet <- gsub("@monthnr@", monthnr, yaml_snippet)
    yaml_snippet <- gsub("social_multiplier_", "ki_multiplier_", yaml_snippet)
    yaml_snippet <- gsub("socialDistance_time", "ki_multiplier_time_", yaml_snippet)
    yaml_snippet <- gsub("__", "_", yaml_snippet)

    if (length(fittingParam) == 1) {
      yaml_snippet <- yaml_snippet[17:length(yaml_snippet)]
      for (i in c(1:11)) {
        yaml_snippet <- gsub(paste0("@ki_multiplier_EMS_", i, "_lwr@"), round(range_dat[i, "min"], 3), yaml_snippet)
        yaml_snippet <- gsub(paste0("@ki_multiplier_EMS_", i, "_upr@"), round(range_dat[i, "max"], 3), yaml_snippet)
      }
    }
    if (length(fittingParam) == 2) {
      for (i in c(1:11)) {
        yaml_snippet <- gsub(paste0("@ki_multiplier_time_EMS_", i, "_lwr@"), round(range_dat[i, paste0(fittingParam[1], "_min")], 0), yaml_snippet)
        yaml_snippet <- gsub(paste0("@ki_multiplier_EMS_", i, "_lwr@"), round(range_dat[i, paste0(fittingParam[2], "_min")], 3), yaml_snippet)
        yaml_snippet <- gsub(paste0("@ki_multiplier_time_EMS_", i, "_upr@"), round(range_dat[i, paste0(fittingParam[1], "_max")], 0), yaml_snippet)
        yaml_snippet <- gsub(paste0("@ki_multiplier_EMS_", i, "_upr@"), round(range_dat[i, paste0(fittingParam[2], "_max")], 3), yaml_snippet)
      }
    }
    writeLines(yaml_snippet, file.path(git_dir, "experiment_configs", "snippets", "config_weekly_ki_multiplier_fit.txt"))
  }
}



## --------------------------------
#### Fitting + post-fit plots + export csv's
## --------------------------------
use_values_list <- list()
for (i in c(1:11)) {
  print(paste0("Start fitting process for region ", i))

  # sim_ems <- load_sim_dat(fittingParam, exp_name, i, start_date, stop_date, fname = paste0("trajectoriesDat_region_",i,".RData"))
  # sim_ems <- load_sim_dat(fittingParam, exp_name, i, start_date, stop_date, fname="trajectoriesDat_trim.csv")
  sim_ems <- load_sim_dat(fittingParam, exp_name, i, start_date, stop_date, fname = "trajectoriesDat.csv")

  str(sim_ems)
  summary(sim_ems$date)
  sapply(sim_ems[fittingParam], summary)
  sapply(sim_ems[fittingParam], unique)

  ### Prepare for merge and merge ref_dat to sim_dat
  sim_ems_emresource <- merge_sim_and_ref_dat(sim_ems, i, start_date, stop_date, smooth_n_days)[[1]]
  sim_ems_LL <- merge_sim_and_ref_dat(sim_ems, i, start_date, stop_date, smooth_n_days)[[2]]
  sim_ems_CLI <- merge_sim_and_ref_dat(sim_ems, i, start_date, stop_date, smooth_n_days)[[3]]

  ## get a list of all scenario numbers run for this EMS
  scens <- sort(unique(sim_ems_emresource$scen_num))

  use_values <- f_run_fitting(i,
    sim_ems_emresource, sim_ems_LL, sim_ems_CLI, scens,
    useSmoothedData = useSmoothedData,
    weightDeath = weightDeath,
    excludeDeaths = excludeDeaths,
    includeLLadmissions = includeLLadmissions,
    includeCLIadmissions = includeCLIadmissions,
    excludeEMRnonICU = excludeEMRnonICU
  )


  use_values_list[[i]] <- use_values

  #### Generate plots and save csv's
  pplot <- f_post_fit_plot(use_values_dat = use_values, i, logscale = TRUE)
  fwrite(use_values, file.path(out_dir, "csv", paste0("best_parameter_ranges_ems_", i, ".csv")), quote = FALSE, row.names = FALSE)

  rm(sim_ems_emresource, sim_ems_LL, sim_ems_CLI, sim_ems, scens, use_values)
}

f_export_sumary_csv(use_values_list, fittingParam, npairs = 10)

sink(file.path(exp_dir, "fitting/selectedIndicators.txt"))
cat("\n")
print(paste0("useSmoothedData  = ", useSmoothedData))
print(paste0("weightDeath  = ", weightDeath))
print(paste0("excludeDeaths  = ", excludeDeaths))
print(paste0("includeLLadmissions  = ", includeLLadmissions))
print(paste0("includeCLIadmissions  = ", includeCLIadmissions))
print(paste0("excludeEMRnonICU  = ", excludeEMRnonICU))
cat("\n")
print(paste0("start_date  = ", start_date))
print(paste0("stop_date  = ", stop_date))
cat("\n")
print(paste0("fittingParam  = ", fittingParam))
sink()
