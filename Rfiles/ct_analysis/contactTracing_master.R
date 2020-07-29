## =====================================================================================
# R script that analyses contact tracing simulations
# Simulations are stored by date under the "contact_tracing" folder in simulation_output
# Contact tracing simulations are specified by having varying detection and isolation parameters for As+P and/or Sym
# Simulations are run with fixed sample parameters, only varying the contact tracing specific parameters
## =====================================================================================


## Load packages
packages_needed <- c( 'tidyverse','reshape', 'cowplot', 'scales', 'readxl', 'viridis', 'stringr', 'broom') 
lapply(packages_needed, require, character.only = TRUE) 

## Load directories and custom objects and functions
source("load_paths.R")
source("processing_helpers.R")
source("ct_analysis/helper_functions_CT.R")

simdate <- "20200728"
exp_names <- list.dirs(file.path(ct_dir, simdate), recursive = FALSE, full.names = FALSE)

### Define which analysis scripts to run
describeDat <- FALSE
heatmapCritical <- TRUE

estimateRt <- FALSE 
heatmapRt <- FALSE 

tresholdsAll <- FALSE
generateMap <- FALSE

### Loop through each EMS or the operational 'super-regions' 
geography <- "Region"  # 'EMS'

## When plotting heatmaps, should the legend show predictions per 100'000 population ? 
scalePop <- TRUE

## Run analysis scripts for each experiment in exp_names (must have same contact tracing parameters!)
for (exp_name in exp_names) {
  # exp_name <- exp_names[1]
  print(exp_name)
   
  ## Load trajectories dat and define parameters for analysis
  source('ct_analysis/loadData_defineParam.R')

  ### Run analysis scripts for selected outcome
  LOCAL=TRUE
  if (describeDat) source(file.path("ct_analysis/describeTrajectoriesDat.R"))
  if (heatmapCritical) source(file.path("ct_analysis/heatmap_loess_contactTracing.R"))  
  
  if (estimateRt) source(file.path("ct_analysis/get_Rt_from_contactTracingSimulations.R"))
  if (heatmapRt) source(file.path("ct_analysis/combine_Rt_and_plot.R"))
  if (heatmapRt) source(file.path("ct_analysis/heatmap_loess_contactTracing_Rt.R"))
  
  if (heatmapCritical | heatmapRt) source(file.path("ct_analysis/combine_thresholds_dat.R"))
  
}

### Generate pointrange plots with minimum detection level aggregated for Illinois
### Running either with Rt or critical (and includes plot comparing both )
summary1 <- FALSE
if (summary1){
  selected_outcome <- "Rt" # critical
  compareOutcomes <- FALSE
  generateMap=FALSE
  source(file.path("ct_analysis/combined_exp_summary_plot_IL.R"))
}

