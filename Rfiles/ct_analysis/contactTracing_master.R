## =====================================================================================
# R script that analyses contact tracing simulations
# Simulations are stored by date under the "contact_tracing" folder in simulation_output
# Contact tracing simulations are specified by having varying detection and isolation parameters for As+P and/or Sym
# To avoid noise of sample parameters, most simulations are run with fixed sample parameters, only varying the contact tracing specific parameters
## =====================================================================================


## Load packages
packages_needed <- c( 'tidyverse', 'cowplot', 'scales', 'readxl', 'viridis', 'stringr', 'broom') 
lapply(packages_needed, require, character.only = TRUE) 

## Load directories and custom objects and functions
source("load_paths.R")
source("processing_helpers.R")
source("ct_analysis/helper_functions_CT.R")


# Define experiment iteration and simdate
ct_dir <- file.path(simulation_output, "contact_tracing")
simdate <- "20200627"

### Limit for ICU beds
labs <- c(
  "EMS_1\n limit: 148", "EMS_2\n limit: 181", "EMS_3\n limit: 103", "EMS_4\n limit: 98", "EMS_5\n limit: 88", "EMS_6\n limit: 109",
  "EMS_7\n limit: 404", "EMS_8\n limit: 255", "EMS_9\n limit: 265", "EMS_10\n limit: 150", "EMS_11\n limit: 785"
)

### Load simulation data
exp_names <- list.dirs(file.path(ct_dir, simdate), recursive = FALSE, full.names = FALSE)

### Define which analysis scripts to run
describeDat <- FALSE
heatmapPerEMS <- FALSE
tresholdsAll <- FALSE
estimateRt <- FALSE 
heatmapRt <- TRUE 
generateMap <- FALSE

### Loop through each EMS or the operational 'super-regions' 
geography <- "EMS"
# geography <- "Region"

## When plotting heatmaps, should the legend show predictions per 100'000 population ? 
scalePop <- TRUE

## RUn analysis scripts for each experiment in exp_names (must have same contact tracing parameters!)
for (exp_name in exp_names) {
  # exp_name <- exp_names[1]
  print(exp_name)

  ### Define experiment specfic directories
  exp_dir <- file.path(ct_dir, simdate, exp_name)
  ems_dir <- file.path(ct_dir, simdate, exp_name, "per_ems")
  if (!dir.exists(ems_dir)) dir.create(ems_dir)

  Rt_dir <- file.path(ct_dir, simdate, exp_name, "estimatedRt")
  if (!dir.exists(Rt_dir)) dir.create(Rt_dir)

  ## Load trajectories Dat
  trajectoriesDat <- read.csv(file.path(exp_dir, "trajectoriesDat.csv"))

  ## Define contact  tracing parameters
  detectionVar <- "d_AsP_ct1" # "d_Sym_ct1"
  isolationVar <- "reduced_inf_of_det_cases_ct1"
  groupVar <- "d_Sym_ct1" # "time_to_detection" # "change_testDelay_Sym_1"

  ## Define label per parameter for plotting
  detectionVar_label <- detectionVar
  isolationVar_label <- isolationVar
  groupVar_label <- groupVar

  if (detectionVar == "d_AsP_ct1") detectionVar_label <- "detection of As and P (%)"
  if (detectionVar == "d_Sym_ct1") detectionVar_label <- "increased detection of Sym (%)"
  if (detectionVar == "d_AsPSym_ct1") detectionVar_label <- "detection of As, P, Sym (%)"
  if (groupVar == "d_Sym_ct1") groupVar_label <- "increased detection of Sym (%)"
  if (groupVar == "change_testDelay_Sym_1") groupVar_label <- "reduced detection delay of Sym (days)"
  if (isolationVar == "reduced_inf_of_det_cases_ct1") isolationVar_label <- "isolation success As, P (%)"
  if (groupVar == "contact_tracing_start_1") trajectoriesDat <- trajectoriesDat %>% mutate(grpvar = as.Date(contact_tracing_start_1 + startdate))

  ### Extract relevant dates from trajectpries dat
  reopeningdate <- unique(as.Date(trajectoriesDat$socialDistanceSTOP_time, origin = trajectoriesDat$startdate))
  interventionstart <- unique(as.Date(trajectoriesDat$contact_tracing_start_1, origin = trajectoriesDat$startdate))
  interventionstop <- unique(as.Date(trajectoriesDat$contact_tracing_stop1, origin = trajectoriesDat$startdate))

  ### Discard time entries before reopening date
  trajectoriesDat <- trajectoriesDat %>%
    mutate(
      startdate = as.Date(startdate),
      Date = as.Date(time + startdate)
    )

  ### Define CT variables
  trajectoriesDat$detection_success <- trajectoriesDat[, colnames(trajectoriesDat) == detectionVar]
  trajectoriesDat$isolation_success <- trajectoriesDat[, colnames(trajectoriesDat) == isolationVar]
  trajectoriesDat$grpvar <- trajectoriesDat[, colnames(trajectoriesDat) == groupVar]
  if (isolationVar == "reduced_inf_of_det_cases_ct1") trajectoriesDat$isolation_success <- 1 - (trajectoriesDat$isolation_success)

  ## Check CT parameters
  unique(trajectoriesDat$detection_success)
  unique(trajectoriesDat$isolation_success)
  unique(trajectoriesDat$grpvar)
  
  ### Run analysis scripts for selected outcome
  selected_outcome <- "critical"
  if (describeDat) source(file.path("ct_analysis/describeTrajectoriesDat.R"))
  if (estimateRt) source(file.path("ct_analysis/get_Rt_from_contactTracingSimulations.R"))
  if (heatmapRt) source(file.path("ct_analysis/ct_estimatedRT.R"))

  ### Subset for identifying thresholds (historical estimates not needed)
  trajectoriesDat <- trajectoriesDat %>%
    filter(time >= as.Date(reopeningdate) - as.Date(max(startdate)) - 30)

  if (heatmapPerEMS) source(file.path("ct_analysis/heatmap_loess_contactTracing.R"))
  
}

### Generate pointrange plots with minimum detection level aggregated for Illinois
### Running either with Rt or critical (and includes plot comparing both )
summary1 <- FALSE
if (summary1){
  selected_outcome <- "Rt" # critical
  compareOutcomes <- FALSE
  generateMap=FALSE
  source(file.path("ct_analysis/combined_exp_summary_plot.R"))
}

