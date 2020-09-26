
# This script takes hospital data from various EMS regions of Illinois, and compares
## them to simulations where we varied the transmission parameter (Ki), the amount
# that Ki was reduced during shelter in place, and the start date of the epidemic
# (10 asymptomatic individuals) for each EMS region

# This script assumes that inside simulation output, there is a file
# called EMS_X, where X is a given ems region.
# It assumes that inside that file is a sheet title trajectoriesDat.csv
# It also assumes that in the "data" folder int he "covid_chicago" folder on box
# There are files titled "emresource_by_region.csv", "200522_jg_admission_date_ems.csv",
# And "200522_jg_deceased_date_ems.csv".
# The "library(here) call sets the working directoty to be wherever this file lives.
# For me it lived inside the covid_chicago file in Box (unsynced). If youres lives in git,
# put in a call to set the working directory to the covid_chicago folder on box.
# Line 233- change working directory to where you want results saved.

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


##----------------------------------------------
## Experiment folder settings
exp_name <- "20200924_IL__test_initialFit"
simdate <- str_split(exp_name, "_")[[1]][1]

exp_dir <- file.path(simulation_output, "forFitting", exp_name)
csv_dir <-  file.path(exp_dir, "_csv")
plot_dir <- file.path(simulation_output, "forFitting", exp_name, "_plots")

if(!dir.exists(csv_dir))dir.create(csv_dir)
if(!dir.exists(plot_dir))dir.create(plot_dir)

out_dir <- file.path(project_path, "parameter_estimates_by_EMS/testing/")
if (!dir.exists(out_dir)) dir.create(out_dir)

##----------------------------------------------


##----------------------------------------------
## Helper functions
load_sim_dat <- function(fittingParam, exp_name,i){
  
   #fittingParam <- c("socialDistance_time7", "social_multiplier_7") ### flexible input
   #fittingParam <- c("time_infection_import", "Ki") ### flexible input
  
  outcomeParam <- paste0(c("death_det_cumul", "crit_det", "hosp_det", "hosp_det_cumul", "infected_cumul"), paste0("_EMS-", i))
  KeepCols <- c("time", "startdate", "scen_num", "sample_num", fittingParam, outcomeParam)
  
  df <- fread(file.path(simulation_output, "forFitting", exp_name, "trajectoriesDat.csv"), select = KeepCols) %>%
    mutate(
      region = i,
      startdate = as.Date(startdate),
      date = startdate + time
    ) %>%
    setNames(gsub(paste0("_EMS-", i), "", names(.))) %>%
    filter(date <= Sys.Date())
  
  timevars <- colnames(df)[grep("time", colnames(df))]
  
  
  df <- calculate_dates(df)
  
  df <- df %>%
    group_by(scen_num) %>%
    arrange(time) %>%
    mutate(
      new_detected_deaths = 0, new_detected_deaths = death_det_cumul - lag(death_det_cumul),
      new_detected_hospitalized = 0, new_detected_hospitalized = hosp_det_cumul - lag(hosp_det_cumul),
      new_infected = 0, new_infected = infected_cumul - lag(infected_cumul)
    ) %>%
    mutate(
      new_detected_deaths = ifelse(time == 0, 0, new_detected_deaths),
      new_detected_hospitalized = ifelse(time == 0, 0, new_detected_hospitalized),
      new_infected = ifelse(time == 0, 0, new_infected)
    )
  
  return(df)
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

pre_fit_plot <- function(fittingVar, logscale=TRUE) {
  
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
  
  if(logscale==TRUE){
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
  
  if(logscale==TRUE){
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
  
  
  best_param$param1 <- best_param[,colnames(best_param)==fittingParam[1]]
  best_param$param2 <- best_param[,colnames(best_param)==fittingParam[2]]
  
  pplot <- ggplot(best_param, aes(x = param1, y = param2)) +
    geom_point() +
    labs(fill = "NLL") +
    ylim(0, 1) +
    labs(title = "", subtitle = ems, x=fittingParam[1], y=fittingParam[2]) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  
  
  ggsave(paste0(ems, "_best_parameter_plots.png"),
         plot = pplot,
         path = file.path(out_dir, "best_parameter_plots/"), width = 13, height = 6, device = "png"
  )
  
  return(pplot)
}

f_post_fit_plot <- function( use_values_dat,ems,fittingParam, logscale=TRUE){
  
  # fittingParam
  df <- as.data.frame(sim_ems_emresource)
  df <- subset(df, df$scen_num %in% row.names(use_values_dat))

  length(unique(df$scen_num))

  p1 <- ggplot(data = df) +
    geom_line(aes(x = date, y = crit_det, group = scen_num), col="deepskyblue3") +
    geom_point(aes(x = date, y = confirmed_covid_icu, group = scen_num)) +
    theme(legend.position = "None") +
    labs(title = "EMSrecourse\nconfirmed_covid_icu", subtitle = "")
  
  p2 <- ggplot(data = df) +
    geom_line(aes(x = date, y = new_detected_deaths, group = scen_num), col="deepskyblue3") +
    geom_point(aes(x = date, y = confirmed_covid_deaths_prev_24h, group = scen_num)) +
    theme(legend.position = "None") +
    labs(title = "EMSrecourse\nconfirmed_covid_deaths_prev_24h", subtitle = "")
  
  p3 <- ggplot(data = df) +
    geom_line(aes(x = date, y = hosp_det, group = scen_num), col="deepskyblue3") +
    geom_point(aes(x = date, y = covid_non_icu, group = scen_num)) +
    theme(legend.position = "None") +
    labs(title = "EMSrecourse\ncovid_non_icu", subtitle = "")
  
  if(logscale==TRUE){
    p1 <- p1 + scale_y_log10()
    p2 <- p2 + scale_y_log10()
    p3 <- p3 + scale_y_log10()
    
  }
  emr_plot <- plot_grid(p1, p2, p3, nrow = 1)
  rm(p1,p2,p3, df)
  
  df <- as.data.frame(sim_ems_LL)
  df <- subset(df, df$scen_num %in% row.names(use_values_dat))
  
  
  p1 <- ggplot(data = df) +
    geom_line(aes(x = date, y = new_detected_deaths, group = scen_num), col="deepskyblue3") +
    geom_point(aes(x = date, y = deaths, group = scen_num)) +
    theme(legend.position = "None") +
    labs(title = "Line List\nnew_detected_deaths", subtitle = "")
  
  
  p2 <- ggplot(data = df) +
    geom_line(aes(x = date, y = new_detected_hospitalized,  group = scen_num), col="deepskyblue3") +
    geom_point(aes(x = date, y = admissions, group = scen_num)) +
    theme(legend.position = "None") +
    labs(title = "Line List\nadmissions", subtitle = "") 
  
  if(logscale){
    p1 <- p1 + scale_y_log10()
    p2 <- p2 + scale_y_log10()
  }
  
  ll_plot <- plot_grid(p1, p2, nrow = 1)
  
  pplot <- plot_grid(emr_plot, ll_plot, nrow = 2)
  
  return(pplot)
  
}
##----------------------------------------------



### Settings
fittingParam <- c("socialDistance_time7", "social_multiplier_7") ### flexible input
fittingParam <- c("time_infection_import", "Ki") ### flexible input

stop_date <- as.Date("2020-04-01")

## loop begin over EMS regions
use_values_list <- list()
for (i in c(1:11)) {
  
  print(paste0("Region ", i))
  
  sim_ems <- load_sim_dat(fittingParam, exp_name,i) 
  sim_ems <- as.data.frame(sim_ems)
  summary(sim_ems$date)
  summary(sim_ems[, fittingParam[1]])
  summary(sim_ems[, fittingParam[2]])
  
  sim_ems <- sim_ems %>% filter(date <= stop_date)
  
  length(unique(sim_ems[, fittingParam[1]]))
  length(unique(sim_ems[, fittingParam[2]]))
  
  emresource_ems <- fread(file.path(data_path, "covid_IDPH/Corona virus reports/emresource_by_region.csv")) %>%
    filter(covid_region %in% i) %>%
    mutate(date = as.Date(date_of_extract)) %>%
    filter(date >=as.Date("2020-01-01") & date <= stop_date) %>%
    complete(date = seq.Date(min(date), max(date), by = "day"))
  
  summary(emresource_ems$date)
  
  emresource_ems[is.na(emresource_ems)] <- 0 ## TODO check
  
  LL_ems <- fread(file.path(data_path, "covid_IDPH", "Cleaned Data", "200921_jg_aggregated_covidregion.csv")) %>%
    filter(covid_region %in% i) %>%
    mutate(date = as.Date(date)) %>%
    filter(date >=as.Date("2020-01-01") & date <= stop_date) %>%
    complete(date = seq.Date(min(date), max(date), by = "day"))
  
  
  summary(emresource_ems$date)
  summary(LL_ems$date)
  
  emresource_ems$filterVar <- 1
  LL_ems$filterVar <- 1
  
  emresource_ems$date <- as.Date(as.character(emresource_ems$date))
  LL_ems$date <- as.Date(as.character(LL_ems$date))
  sim_ems$date <- as.Date(as.character(sim_ems$date))
  
  sim_ems_emresource <- sim_ems %>%
    left_join(emresource_ems, by = "date") %>%
    filter(!is.na(filterVar))
  
  sim_ems_LL <- sim_ems %>%
    left_join(LL_ems, by = "date") %>%
    filter(!is.na(filterVar))
  
  summary(sim_ems_emresource$date)
  summary(sim_ems_LL$date)
  
  createPlot <- T
  if (createPlot) {
    plot1 <- pre_fit_plot(fittingVar = fittingParam[1], logscale=TRUE)
    plot2 <- pre_fit_plot(fittingVar = fittingParam[2], logscale = TRUE)
    
    ggsave(paste0(i, "_pre_fit_plot1.png"),
           plot = plot1,
           path = plot_dir, width = 13, height = 10, device = "png"
    )
    ggsave(paste0(i, "_pre_fit_plot2.png"),
           plot = plot2,
           path = plot_dir, width = 13, height = 10, device = "png"
    )
  }
  
  ## get a list of all scenario numbers run for this EMS
  scens <- unique(sim_ems_emresource$scen_num)
  
  # create data.frame to hold likelihood results
  ems_output <- matrix(0, length(scens), length(fittingParam) + 2)
  ems_output <- as.data.frame(ems_output)
  colnames(ems_output) <- c(fittingParam, "NLL","region")
  ems_output[,"region"] <- i
  
  # loop over simulation scenarios and record likelihood
  for (j in 1:length(scens)) {
    
    print(paste0("scenario ", j))
    # pull out all simulation values of given parameter value
    emresource_sub <- sim_ems_emresource[which(sim_ems_emresource$scen_num == scens[j]), ]
    LL_sub <- sim_ems_LL[which(sim_ems_LL$scen_num == scens[j]), ]
    
    ### EMResource data
    # Likelihood of simulation generating detected critical
    nll1 <- -1 * sum(dpois(emresource_sub$confirmed_covid_icu, emresource_sub$crit_det + 1e-10, log = TRUE))
    
    # Likelihood of simulation generating detected covid deaths
    nll2 <- -1 * sum(dpois(emresource_sub$confirmed_covid_deaths_prev_24h, emresource_sub$new_detected_deaths + 1e-10, log = TRUE))
    
    # Likelihood of simulations generating admission data
    nll3 <- -1 * sum(dpois(emresource_sub$covid_non_icu, emresource_sub$hosp_det + 1e-10, log = TRUE))
    
    ### Line list  data
    # Likelihood of simulations creating death data that doesn't come from EMresource.
    nll4 <- -1 * sum(dpois(LL_sub$deaths, LL_sub$new_detected_deaths + 1e-10, log = TRUE), na.rm = TRUE)
    
    nll5 <- -1 * sum(dpois(LL_sub$admissions, LL_sub$new_detected_hospitalized + 1e-10, log = TRUE), na.rm = TRUE)
    
    # Sum all Likelihood, weighting emresource data higher
    nll <- nll1 + nll2 + nll3 + (nll4 + nll5) * (length(emresource_sub$confirmed_covid_icu) / length(LL_sub$cases)) / 2
    
    # Put Likelihood values and corresponding start date and Ki in output dataframe
    ems_output[j, 1] <- unique(emresource_sub[, fittingParam[1]])
    ems_output[j, 2] <- unique(emresource_sub[, fittingParam[2]])
    if (length(fittingParam) - 1 > 2) ems_output[3, j] <- unique(emresource_sub[, fittingParam[3]])
    ems_output[j, "NLL"] <- nll

    rm(nll,nll1,nll2,nll3,nll4,nll5,emresource_sub, LL_sub)
  }
  
  
  
  # Create sheet of the 5% most likely parameter combinations
  use_values <- (ems_output[which(ems_output$NLL < quantile(ems_output$NLL, prob = 1 - 95 / 100)), ])
  dim(ems_output)
  dim(use_values)
  
  use_values_list[[i]] <- use_values
  
  #### Generate plots and save csv's
  plot_and_save=T
  if(plot_and_save){
    
    #f_paramplots(use_values_dat = ems_output, ems = i, fittingParam)
    pplot <- f_post_fit_plot(use_values_dat=use_values,ems=i,fittingParam, logscale=TRUE)
    
    ggsave(paste0(i, "_post_fit_plot.png"),
           plot = pplot,
           path = plot_dir, width = 13, height = 10, device = "png"
    )
    
    # Save best paramter combinations
    fwrite(use_values, file.path(csv_dir, paste0("best_parameter_ranges_ems_", i, ".csv")), quote=FALSE, row.names=FALSE)
  }
  
  rm(sim_ems_emresource,sim_ems,scens, use_values)
  
}

### Export best parameter fit as csv for all EMS
df_best <- do.call(rbind.data.frame, use_values_list) %>%
  group_by(region) %>%
  filter(NLL == min(NLL)) %>%
  write.csv(file.path(exp_dir, "best_parameters_emsAll.csv"))


