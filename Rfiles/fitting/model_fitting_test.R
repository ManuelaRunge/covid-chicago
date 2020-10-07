
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


### Set working directory to the GitHub repository R files
source("load_paths.R")
source("processing_helpers.R")

out_dir =  file.path(project_path,"parameter_estimates_by_EMS/testing/")
if(!dir.exists(out_dir))dir.create(out_dir)

simdate = "20200525"

## Read in simulations
exp_names <- list.dirs(file.path(simulation_output,"forFitting"), recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("_test1",exp_names)]

simulationslist <- list()
for(exp_name in exp_names){
  simulationslist[[length(simulationslist)+1]] <- fread(file.path(simulation_output,"forFitting", exp_name,"trajectoriesDat.csv" ))
}

### Read in 
dat <- f_loadData(data_path, simdate ='200921')
dat <- na.omit(dat)

## Separate hospital data by EMS, put in list
dat_list <- list()
for(i in c(1:11)){
  dat_list[[length(dat_list)+1]] <- dat[dat$region == i,]
  
}


## loop begin over EMS regions
use_values_list <- list()
for (i in 1:11) {

  # OK, let's prepare the simulation sheet
  sim_ems <- simulationslist[[i]]
  sim_ems <- na.omit(sim_ems)


  # In the simulations, add a colum for deaths each day rather than cumulative deaths
   sim_ems <- sim_ems %>% 
              group_by(scen_num) %>% 
              arrange(time) %>% 
              mutate(death_det_24hr=0, death_det_24hr = death_det_cumul- lag(death_det_cumul)) %>%
              mutate(death_det_24hr = ifelse(time==0, 0,death_det_24hr ))

  # Also add a column for new detected symptomatic each day
   sim_ems <- sim_ems %>% 
     group_by(scen_num) %>% 
     arrange(time) %>% 
     mutate(new_det=0, new_det = symp_mild_det_cumul- lag(symp_mild_det_cumul)) %>%
     mutate(new_det = ifelse(time==0, 0,new_det ))
   
   
  # We need to match simulation dates with real world dates.
  # simulation dates aren't integers, so round them.
  sim_ems$time <- floor(sim_ems$time)
  table(sim_ems$startdate)

  ## get EMS specific hosp_data
  dat_ems <- dat_list[[i]]
  # Fill in dates that are missing with 0's
  dat_ems <- dat_ems %>%  complete(Date = seq.Date(min(dat_ems$Date), max(dat_ems$Date), by = "day"))

  ## get a list of all scenario numbers run for this EMS
  scens <- unique(sim_ems$scen_num)

  # create data.frame to hold likelihood results
  ems_output <- matrix(0, length(scens), 4)
  ems_output <- as.data.frame(ems_output)
  colnames(ems_output) <- c("Start_date", "Ki", "Ki_red", "NLL")
  # ems_output$Start_date <- as.Date(ems_output$Start_date, origin = '2020-01-01')
  ems_output$Start_date <- "" # as.Date(sim_ems$startdate, format="yyyy-mm-")


  # loop over simulation scenarios and record likelihood
  
  for (j in 1:length(scens)) {
    # pull out all simulation values of given Ki value
    sim_ems_subset <- sim_ems[which(sim_ems$scen_num == scens[j]), ]
    
    
    ## TODO check 
    # Set the start date to be that based on when we started disease interventions
     sim_ems_subset$time <- as.Date(sim_ems_subset$time,origin = (as.Date("2020-03-12") - floor(sim_ems_subset$socialDistance_time1[1])))
     #sim_ems_subset$time <- as.Date(sim_ems_subset$startdate)

    # Get earliest and latest dates of the simualtion data and hopital date
    mindate <- max(min(sim_ems_subset$time), min(dat_ems$Date))
    maxdate <- min(max(sim_ems_subset$time), max(dat_ems$Date))

    sim_ems_subset1 <-  sim_ems_subset %>% 
      rename(Date=time) %>% 
      select(Date,scen_num, crit_det , death_det_24hr) %>%
      left_join(dat_ems, by="Date") %>%
      filter(!is.na(region))
    
    # Calculate likelyhood
    # Likelyhood of simulation generating detected critical
    nll1 <- -1 * sum(dpois(sim_ems_subset1$confirmed_covid_icu, sim_ems_subset1$crit_det + 1e-10, log = TRUE))
    
    # Likelyhood of simulation generating detected covid deaths
    nll2 <- -1 * sum(dpois(sim_ems_subset1$confirmed_covid_deaths_prev_24h, sim_ems_subset1$death_det_24hr + 1e-10, log = TRUE))
    
    # Likelyhood of simulations generating admission data
    nll3 <- -1 * sum(dpois(sim_ems_subset1$LL_cases, sim_ems_subset1$crit_det + 1e-10, log = TRUE))
    
    # Likelyhood of simulations creating death data that doesn't come from EMresource.
    nll4 <- -1 * sum(dpois(sim_ems_subset1$LL_cases, sim_ems_subset1$death_det_24hr + 1e-10, log = TRUE))
    
    # Summ all likelyhood, weighting nll1 and nll2 higher
    nll <- nll1 + nll2 + (nll3 + nll4) * (length(sim_ems_subset1$confirmed_covid_icu) / length(sim_ems_subset1$LL_cases)) / 2
    
    

    # Put likelihood values and corresponding start date and Ki in output dataframe
    ems_output$Start_date[j] <- as.character(unique(sim_ems_subset$startdate)) # as.Date((as.Date("2020-03-12") - floor(sim_ems_subset$socialDistance_time1[1])))
    ems_output$Ki[j] <- sim_ems_subset$Ki[1]
    ems_output$Ki_red[j] <- sim_ems_subset$social_multiplier_4[1]
    ems_output$NLL[j] <- nll

  }

  # Create sheet of the 5% most likely parameter combinations
  nam <- paste("ems", i, "_use_values", sep = "")
  assign(nam, ems_output[which(ems_output$NLL < quantile(ems_output$NLL, prob = 1 - 95 / 100)), ])

  ems_output <- ems_output[which(ems_output$NLL < quantile(ems_output$NLL, prob = 1 - 95 / 100)), ]
  ems_output$ems <- i
  use_values_list[[i]] <- ems_output
}


## Set working directory to whereever you want results saved.
# Create figures showing correlations between paramter combinations for
# Each EMS region

library(cowplot)
f_paramplots <- function(use_values_dat, ems) {
  # use_values_dat=ems1_use_values

  best_param <- use_values_dat[which(use_values_dat$NLL == min(use_values_dat$NLL)), ]


  Ki_startdate <- ggplot(use_values_dat, aes(x = Start_date, y = Ki)) +
    geom_point() +
    labs(fill = "NLL") +
    ylim(0, 1) +
    labs(title = "Ki_red_startdate", subtitle = ems) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )

  Ki_red_startdate <- ggplot(use_values_dat, aes(x = Start_date, y = Ki_red)) +
    geom_point() +
    labs(fill = "NLL") +
    labs(title = "Ki_red_startdate", subtitle = ems) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )

  Ki_Ki_red <- ggplot(use_values_dat, aes(x = Ki, y = Ki_red)) +
    geom_point() +
    labs(fill = "NLL") +
    xlim(0, 1) +
    labs(title = "Ki_Ki_red", subtitle = ems, x = "\n\n\nKi")

  pplot <- plot_grid(Ki_startdate, Ki_red_startdate, Ki_Ki_red, nrow = 1)
  ggsave(paste0(ems, "_best_parameter_plots.png"),
    plot = pplot,
    path = file.path(out_dir,"best_parameter_plots/"), width = 13, height = 6, device = "png"
  )

  return(pplot)
}


### Generate plots with points for parameter ranges
for (i in c(1:11)) {
  ems_i <- paste0("EMS_", i)
  nam <- use_values_list[[i]]
  
  f_paramplots(use_values_dat = nam, ems = ems_i)

  # Save best paramter combinations
  write.csv(nam, paste0(out_dir,"best_parameter_sheets/best_parameter_ranges_ems", i, ".csv"))
}


### Export best parameter fit as csv for all EMS 
df_best <- do.call(rbind.data.frame, use_values_list) %>%
  group_by(ems) %>%
  filter(NLL == min(NLL)) %>%
  write.csv(file.path(out_dir,"best_parameters_emsAll2.csv"))


