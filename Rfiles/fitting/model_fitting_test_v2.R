###----------------------------------------------------------------------------------------------
# This script takes hospital data from various EMS regions of Illinois
###----------------------------------------------------------------------------------------------
# Input: 
# - simulation trajectoriesDat.csv located in projects\covid_chicago\cms_sim\simulation_output\_forFitting\<exp_name>
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
library(zoo)

theme_set(theme_cowplot())

### Set working directory to the GitHub repository R files
source("load_paths.R")
source("processing_helpers.R")


## --------------------------------
#### Functions
## --------------------------------


f_get_fittingParam <- function(dat){
  fittingParam <-  colnames(dat)[c(grep("social",tolower(colnames(dat))),grep("ki",tolower(colnames(dat))))]
  fittingParam <- fittingParam[c(grep("multiplier",tolower(fittingParam)),grep("time",tolower(fittingParam)))]
  fittingParam <- fittingParam[!(grepl("EMS",fittingParam))]

  if(step=="all") fittingParam <- c(fittingParam, "Ki_initial","time_infection_import")
  
  fittingParam<- unique(fittingParam)
  return(fittingParam)
}

## Data management
load_sim_dat <- function(fittingParam=NULL, exp_name, i, start_date, stop_date, fname = "trajectoriesDat_trim.csv", aggregateByFittingParam=TRUE) {
  
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
  

  if(sum(grep(".Rdata",fname),grep(".RData",fname))==1){
    
    load(file.path(exp_dir,fname))
    
    if(is.null(fittingParam)){
      cols = colnames(subdat)
      fittingParam <- f_get_fittingParam(subdat)
    }
    
    fittingParam <- unique(fittingParam)
    outcomeParam <- paste0(c("death_det_cumul", "crit_det", "hosp_det", "hosp_det_cumul", "infected_cumul"), paste0("_EMS-", i))
    KeepCols <- c("time", "startdate", "scen_num", "sample_num", fittingParam, outcomeParam)
    
    KeepCols2 <- KeepCols[!(KeepCols %in% c("Ki_initial","time_infection_import"))]
    
    df <-subdat %>% 
      dplyr::select(KeepCols2) %>%
      dplyr::mutate(
        region = i,
        startdate = as.Date(as.character(startdate)),
        # startdate = as.Date(as.character(startdate), format = "%m/%d/%Y"), #, format = "%m/%d/%Y"
        date = startdate + time
      ) %>%
      setNames(gsub(paste0("_EMS-", i), "", names(.))) %>%
      dplyr::filter(date <= as.Date(stop_date) & date >= as.Date(start_date))
    
    sampleDat <- fread(file.path(exp_dir,"sampled_parameters.csv" )) %>% dplyr::select_at(.vars=c("scen_num",fittingParam)) %>% as.data.frame()
    sampleDat <- sampleDat[, c("scen_num",  colnames(sampleDat)[!(colnames(sampleDat) %in% colnames(subdat))])]
    
    df <-  df %>% left_join(sampleDat, by="scen_num")
  }
  
if(sum(grep(".csv",fname))==1){
  
  if(is.null(fittingParam)){
    cols = fread(file.path(exp_dir, fname),nrows=1, header=TRUE) 
    fittingParam <- f_get_fittingParam(cols)
  }
  
  outcomeParam <- paste0(c("death_det_cumul", "crit_det", "hosp_det", "hosp_det_cumul", "infected_cumul"), paste0("_EMS-", i))
  KeepCols <- c("time", "startdate", "scen_num", "sample_num", fittingParam, outcomeParam)

  
  df <- fread(file.path(exp_dir, fname), select = KeepCols) %>%
    dplyr::mutate(
      region = i,
      startdate = as.Date(as.character(startdate)),
      # startdate = as.Date(as.character(startdate), format = "%m/%d/%Y"), #, format = "%m/%d/%Y"
      date = startdate + time
    ) %>%
    setNames(gsub(paste0("_EMS-", i), "", names(.))) %>%
    dplyr::filter(date <= as.Date(stop_date) & date >= as.Date(start_date))
}
  
  
  if(aggregateByFittingParam){
    grpVars <- c(fittingParam, "time", "startdate", "date")
    
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
      mutate(scen_num = cur_group_id()) %>%
      as.data.table()
  }

  
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
  
  emresource_ems$date <- as.Date(as.character(emresource_ems$date))
  LL_ems$date <- as.Date(as.character(LL_ems$date))
  
  
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
  
  
  ref_dat_list <- list(emresource_ems, LL_ems)
  names(ref_dat_list) <- c("emresource_ems", "LL_ems")
  
  return(ref_dat_list)
}

merge_sim_and_ref_dat <- function(sim_ems, i, start_date, stop_date, smooth_n_days=7) {
  
  #' Load reference data  and do basic data operations
  #'
  #' Load reference data, EMResource and Line list data
  #' @param sim_ems minimum date of  the time period to fit to
  #' @param i identifier for the region (1 to 11)
  #' @param start_date first date of the time period to fit
  #' @param stop_date  last date of the time period to fit
  #' @param LL_file_date date of the lates line list data
  #'
  
  emresource_ems <- load_ref_dat(i, start_date, stop_date, smooth_n_days)[[1]]
  LL_ems <- load_ref_dat(i, start_date, stop_date, smooth_n_days)[[2]]
  
  
  emresource_ems$filterVar <- 1
  LL_ems$filterVar <- 1
  
  sim_ems_emresource <- sim_ems %>%
    dplyr::left_join(emresource_ems, by = "date") %>%
    dplyr::filter(!is.na(filterVar))
  
  sim_ems_LL <- sim_ems %>%
    left_join(LL_ems, by = "date") %>%
    filter(!is.na(filterVar))
  
  # summary(sim_ems_emresource$date)
  # summary(sim_ems_LL$date)
  
  dat_list <- list(sim_ems_emresource, sim_ems_LL)
  names(dat_list) <- c("sim_ems_emresource", "sim_ems_LL")
  
  return(dat_list)
}

## Plotting
pre_fit_plot <- function(fittingVar, logscale = TRUE) {
  
  #' Generates custom pre-fitting plots to assess whether data lies within simulated parameter space
  #'
  #' @param fittingVar vector with names of fitting parameters
  #' @param logscale boolean, if TRUE uses  log scale for y axis
  #'
  
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

f_post_fit_plot <- function(use_values_dat, i, logscale = TRUE) {
  
  #' Generates custom pre-fitting plots to assess whether data lies within simulated parameter space
  #'
  #' @param use_values_dat matrix for one region produced from f_run_fitting that includes the best 5% scenarios
  #' @param i region , previously called ems (1 to)
  #' @param logscale boolean, if TRUE uses  log scale for y axis
  #'
  #'
  df <- as.data.frame(sim_ems_emresource)
  df <- subset(df, df$scen_num %in% row.names(use_values_dat))
  
  use_values_dat$scen_num_fit <- rownames(use_values_dat)
  minNLL <- use_values_dat %>% filter(NLL>0) %>% filter(NLL == min(NLL))
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
  minNLL <- use_values_dat %>% filter(NLL>0) %>% filter(NLL == min(NLL))
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


## Fitting + export csv's
f_run_fitting <- function(i, sim_ems_emresource, sim_ems_LL, scens,bestPercent=5, 
                          useSmoothedData = TRUE, weightDeath=FALSE, 
                          excludeDeaths=FALSE,includeLLadmissions=FALSE) {
  
  #' Fitting function
  #'
  #' Calculated negative log likelihoods per scenario and selects the minimum 5%
  #' Optional includes smoothing using 7 day rolling average before fitting
  #' @param sim_ems_emresource combined simulation with EMresource data, generated by merge_sim_and_ref_dat
  #' @param sim_ems_LL combined simulation with Line List data, generated by merge_sim_and_ref_dat
  #' @param scens simulation scenarios (scen_num) that successfully ran per region (only successes are included in the trajectoriesDat.csv)
  #' @param useSmoothedData boolean, if TRUE takes 7 day rolling average for fitting
  
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
    
    if (useSmoothedData == FALSE) {
      nll1 <- -1 * sum(dpois(emresource_sub$confirmed_covid_icu, emresource_sub$crit_det + 1e-10, log = T), na.rm = TRUE)
      nll2 <- -1 * sum(dpois(emresource_sub$covid_non_icu, emresource_sub$hosp_det + 1e-10, log = T))
      nll3 <- -1 * sum(dpois(LL_sub$deaths, LL_sub$new_detected_deaths + 1e-10, log = T), na.rm = TRUE)
      nll4 <- -1 * sum(dpois(LL_sub$admissions, LL_sub$new_detected_hospitalized + 1e-10, log = T), na.rm = TRUE)
    }
    
    if (useSmoothedData) {
      nll1 <- -1 * sum(dpois(emresource_sub$confirmed_covid_icu_avrg, emresource_sub$crit_det + 1e-10, log = T), na.rm = TRUE)
      nll2 <- -1 * sum(dpois(emresource_sub$covid_non_icu_avrg, emresource_sub$hosp_det + 1e-10, log = T), na.rm = TRUE)
      nll3 <- -1 * sum(dpois(LL_sub$deaths_avrg, LL_sub$new_detected_deaths + 1e-10, log = T), na.rm = TRUE)
      nll4 <- -1 * sum(dpois(LL_sub$admissions, LL_sub$new_detected_hospitalized + 1e-10, log = T), na.rm = TRUE)
    }
    
    # Sum all Likelihood, weighting emresource data higher
    nll <- nll1 + nll2 + nll3
    if(weightDeath) nll <- nll1 + nll2 + (nll3) * (length(emresource_sub$confirmed_covid_icu) / length(LL_sub$cases)) / 2
    if(excludeDeaths) nll <- nll1 + nll2
    
    if(includeLLadmissions){
      nll <- nll1 + nll2 + nll3 + nll4
      if(weightDeath) nll <- nll1 + nll2 + (nll3 + nll4) * (length(emresource_sub$confirmed_covid_icu) / length(LL_sub$cases)) / 2
      if(excludeDeaths) nll <- nll1 + nll2 + nll4
    }
    
    # Put Likelihood values and corresponding start date and Ki in output dataframe
    for (z in c(1:length(fittingParam))) {
      ems_output[j, z] <- unique(emresource_sub[, fittingParam[z]])
    }
    
    ems_output[j, "NLL"] <- nll
    ems_output[j, "nll1"] <- nll1
    ems_output[j, "nll2"] <- nll2
    ems_output[j, "nll3"] <- nll3
    ems_output[j, "nll4"] <- nll4
    
    rm(nll, nll1, nll2, nll3,nll4, emresource_sub, LL_sub)
  }
  
  
  # Create sheet of the 5% most likely parameter combinations
  use_values <- (ems_output[which(ems_output$NLL < quantile(ems_output$NLL, prob = 1 - (100-bestPercent) / 100)), ])
  
  use_values$excludeDeaths= "FALSE"
  if(excludeDeaths==TRUE) use_values$excludeDeaths= "TRUE"
  
  use_values$useSmoothedData= "FALSE"
  if(useSmoothedData==TRUE) use_values$useSmoothedData= "TRUE"
  
  use_values$includeLLadmissions= "FALSE"
  if(includeLLadmissions==TRUE) use_values$includeLLadmissions= "TRUE"
  
  use_values$weightDeath= "FALSE"
  if(weightDeath==TRUE) use_values$weightDeath= "TRUE"
  
  
  
  return(use_values)
}

f_export_sumary_csv <- function(use_values_list,fittingParam, npairs = 10) {
  
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
  
  
  # #### Generate yaml snippet 
  # range_dat <- fread(file.path(out_dir, "csv", "range_parameters_emsAll.csv")) %>% as.data.frame()
  # yaml_snippet <- suppressWarnings(readLines(file.path(git_dir,'experiment_configs','snippets','templates','config_weekly_ki_multiplier_fit.txt')))
  # yaml_snippet <- gsub("@monthnr@",monthnr,yaml_snippet )
  # # 
  # # for(i in c(1:11)){
  #   
  #   yaml_snippet <- gsub(paste0("@socialDistance_time_EMS_",i,"_lwr@"), round(range_dat[i ,paste0(fittingParam[1],"_min")] ,0),yaml_snippet )
  #   yaml_snippet <- gsub(paste0("@social_multiplier_EMS_",i,"_lwr@"), round(range_dat[i ,paste0(fittingParam[2],"_min")] ,3) ,yaml_snippet )
  #   yaml_snippet <- gsub(paste0("@socialDistance_time_EMS_",i,"_upr@"), round(range_dat[i ,paste0(fittingParam[1],"_max")] ,0) ,yaml_snippet )
  #   yaml_snippet <- gsub(paste0("@social_multiplier_EMS_",i,"_upr@"), round(range_dat[i ,paste0(fittingParam[2],"_max")] ,3) ,yaml_snippet )
  # }
  # writeLines(yaml_snippet,file.path(git_dir,'experiment_configs','snippets','config_weekly_ki_multiplier_fit.txt'))
  # 
}

## ----------------------------------------------
## Wrapper function to run for all
prefit_plots <- function(start_date, stop_date) {
  for (i in c(1:11)) {
    print(paste0("Region ", i))
    
    sim_ems <- load_sim_dat(fittingParam, exp_name, i, start_date, stop_date, fname = "trajectoriesDat_trim.csv")
    
    str(sim_ems)
    summary(sim_ems$scen_num)
    summary(sim_ems$date)
    sapply(sim_ems[fittingParam], summary)
    sapply(sim_ems[fittingParam], unique)
    
    ### Prepare for merge and merge ref_dat to sim_dat
    sim_ems_emresource <- merge_sim_and_ref_dat(sim_ems, i,start_date, stop_date,smooth_n_days)[[1]]
    sim_ems_LL <- merge_sim_and_ref_dat(sim_ems, i,start_date, stop_date,smooth_n_days)[[2]]
    
    ggplot(data = sim_ems) +
      geom_line(aes(x = date, y = crit_det, group = scen_num)) +
      scale_x_date(lim = c(as.Date("2020-01-01"), as.Date("2020-05-01")))
    
    for (paramVar in fittingParam) {
      pplot <- pre_fit_plot(fittingVar = paramVar, logscale = TRUE)
      
      ggsave(paste0(i, "_pre_fit_plot_", paramVar, ".png"),
             plot = pplot,
             path = file.path(out_dir, "pre_fit"), width = 13, height = 10, device = "png"
      )
    }
  }
}

## ----------------------------------------------


## ----------------------------------------------


## --------------------------------
####  Experiment folder settings
## --------------------------------
#exp_name <- "20200924_IL__test_initialFit"
#exp_name <- "20200925_IL__test_reopening"
#exp_name <- "20200925_IL__test_lockdown"
#exp_name <- "20200927_IL__test_fitsm7"
#exp_name <- "20200928_IL__test2_fitsm7"
#exp_name <- "20200930_IL_mr_fit_initial"
#exp_name <- "20201001_IL_mr_fit_initial"
#exp_name <- "20201001_IL_mr_fit_initial_nosm1"
#exp_name <- "20201001_IL_mr_fit_lockdown_quest"
#exp_name <- "20201002_IL_mr_fitsm4"
#exp_name <- "20201003_IL_mr_fitsm3"
#exp_name <- "20201003_IL_mr_fitkistartsm3"
#exp_name <- "20201006_IL_mr_local_fitkiall"
exp_name <- "20201008_IL_mr_fitkiall"
exp_name <- "20201005_IL_mr_fitsm4"
exp_name <- "20201010_IL_fit_789"
exp_name <- "20201008_IL_mr_multiplier_fitkiall"
#exp_name <-"20201010_IL_run_fitting_567"

fitstep <- "all"#"newparam" # "initial"  # "reopen" #"lockdown"

simdate <- str_split(exp_name, "_")[[1]][1]
exp_dir <- file.path(simulation_output, "_forFitting", exp_name)
out_dir <- file.path(simulation_output, "_forFitting", exp_name, "fitting")

if (!dir.exists(out_dir)) dir.create(out_dir)
if (!dir.exists(file.path(out_dir, "pre_fit"))) dir.create(file.path(out_dir, "pre_fit"))
if (!dir.exists(file.path(out_dir, "post_fit"))) dir.create(file.path(out_dir, "post_fit"))
if (!dir.exists(file.path(out_dir, "csv"))) dir.create(file.path(out_dir, "csv"))



## --------------------------------
#### Settings
## --------------------------------
if (fitstep == "all") {
  #fittingParam <- c("time_infection_import", "Ki") ###  initial
  # fittingParam <- c('Ki', 'social_multiplier_1','socialDistance_time1','time_infection_import')
  fittingParam <- c('Ki_initial','time_infection_import','Ki_multiplier_3','Ki_multiplier_4','Ki_multiplier_5',
                    'Ki_multiplier_6','Ki_multiplier_7','Ki_multiplier_8','Ki_multiplier_9','Ki_multiplier_10')
  
  fittingParam <- c('Ki_initial','time_infection_import',
                    'ki_3','ki_4','ki_5', 'ki_6','ki_7','ki_8','ki_9','ki_10',
                    'ki_time_3','ki_time_4','ki_time_5', 'ki_time_6','ki_time_7','ki_time_8','ki_time_9','ki_time_10')
  
  start_date <- as.Date("2020-01-01")
  stop_date <- as.Date("2020-10-20")
  
}
if (fitstep == "initial") {
  #fittingParam <- c("time_infection_import", "Ki") ###  initial
  # fittingParam <- c('Ki', 'social_multiplier_1','socialDistance_time1','time_infection_import')
  fittingParam <- c('Ki_fit', 'social_multiplier_3','time_infection_import')
   
  start_date <- as.Date("2020-01-01")
  stop_date <- as.Date("2020-04-28")
}
if (fitstep == "reopen") {
  fittingParam <- c(
    "social_multiplier_5", "social_multiplier_6", "social_multiplier_7",
    "socialDistance_time5", "socialDistance_time6", "socialDistance_time7"
  ) 
  fittingParam <- c( "social_multiplier_5","socialDistance_time5"  )
  
 # 2020-06-07,2020-06-14, 2020-06-21, 2020-06-28
  start_date <- as.Date("2020-06-07")
  stop_date <- as.Date("2020-07-07")
}
if (fitstep == "lockdown") {
  fittingParam <- c(
    "social_multiplier_5", 
    "social_multiplier_4",
    "socialDistance_time5", 
    "socialDistance_time4"
  ) 
  
 fittingParam <- c(
   "social_multiplier_4", 
    "socialDistance_time4"
  ) 
 fittingParam <- c(
   "social_multiplier_3", 
   "socialDistance_time3"
 ) 
  
  #start_date <- as.Date("2020-04-01")
  #stop_date <- as.Date("2020-6-01")
  
  start_date <- as.Date("2020-03-01")
  stop_date <- as.Date("2020-4-15")
  
}
if (fitstep == "current") {
  fittingParam <- c("socialDistance_time7", "social_multiplier_7") ### reopen
  
  start_date <- as.Date("2020-08-01")
  stop_date <- as.Date("2020-9-30")
}



## --------------------------------
#### Pre-fit plots
## --------------------------------
runPreFitPlot <- TRUE
if (runPreFitPlot) prefit_plots()

## --------------------------------
### Fitting and post-fit plots
## --------------------------------


start_date <- as.Date("2020-05-01")
stop_date <- as.Date("2020-08-01")


if (!dir.exists(out_dir)) dir.create(out_dir)
if (!dir.exists(file.path(out_dir, "pre_fit"))) dir.create(file.path(out_dir, "pre_fit"))
if (!dir.exists(file.path(out_dir, "post_fit"))) dir.create(file.path(out_dir, "post_fit"))
if (!dir.exists(file.path(out_dir, "csv"))) dir.create(file.path(out_dir, "csv"))

fittingParam <- c(fittingParam, "Ki_initial","time_infection_import")

## loop begin over EMS regions
runFitting=TRUE
if(runFitting){
  use_values_list <- list()
  for (i in c(1:11)) {
    
    print(paste0("Region ", i))
    
    sim_ems <- load_sim_dat(fittingParam, exp_name, i, start_date, stop_date, fname = paste0("trajectoriesDat_region_",i,".RData"),
                            aggregateByFittingParam=FALSE)
    #  sim_ems <- load_sim_dat(fittingParam, exp_name, i, start_date, stop_date, fname = paste0("trajectoriesDat.csv"))
    
    str(sim_ems)
    summary(sim_ems$date)
    sapply(sim_ems[fittingParam], summary)
    sapply(sim_ems[fittingParam], unique)
    
    ### Prepare for merge and merge ref_dat to sim_dat
    smooth_n_days=7
    sim_ems_emresource <- merge_sim_and_ref_dat(sim_ems, i,start_date, stop_date,smooth_n_days)[[1]]
    sim_ems_LL <- merge_sim_and_ref_dat(sim_ems, i,start_date, stop_date,smooth_n_days)[[2]]
    
    ## get a list of all scenario numbers run for this EMS
    scens <- unique(sim_ems_emresource$scen_num)
    
    ### parallelize in batches of 1000 !! 
    use_values <- f_run_fitting(i, sim_ems_emresource, sim_ems_LL, scens,bestPercent=1, useSmoothedData = TRUE,excludeDeaths = FALSE,includeLLadmissions = TRUE)

    use_values_list[[i+1]] <- use_values
    
    #### Generate plots and save csv's
    plot_and_save <- T
    if (plot_and_save) {
      pplot <- f_post_fit_plot(use_values_dat = use_values, i, logscale = TRUE)
      
      # Save best paramter combinations
      fwrite(use_values, file.path(out_dir, "csv", paste0("best_parameter_ranges_ems_", i, ".csv")), quote = FALSE, row.names = FALSE)
    }
    
    rm(sim_ems_emresource, sim_ems, scens, use_values)
  }
  
  ### Export summary csv files with best parameter for all regions
  ## --------------------------------
  f_export_sumary_csv(use_values_list,fittingParam, npairs = 10)
}


