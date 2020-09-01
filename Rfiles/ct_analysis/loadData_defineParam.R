###=========================================================================================
###  Rscript to define detectionVar, isolationVar and groupVar
### for the current experiment to run 
### Opionally does not load the trajectoriesDat if only the parameter labels are needed
###=========================================================================================

  ### Define experiment specfic directories
  exp_dir <- file.path(ct_dir, simdate, exp_name)
  ems_dir <- file.path(ct_dir, simdate, exp_name, "per_ems")
  if (!dir.exists(ems_dir)) dir.create(ems_dir)

  Rt_dir <- file.path(ct_dir, simdate, exp_name, "estimatedRt")
  if (!dir.exists(Rt_dir)) dir.create(Rt_dir)
  
  heatmapRtDir <- file.path(exp_dir, "heatmap_Rt")
  if(!dir.exists(heatmapRtDir))dir.create(heatmapRtDir)
  
  
  heatmapICUDir <- file.path(exp_dir, "heatmap_ICU")
  if (!dir.exists(heatmapICUDir)) dir.create(heatmapICUDir)


  ## Define contact  tracing parameters
  detectionVar <- "d_AsP_ct1" # "d_Sym_ct1"
  isolationVar <- "reduced_inf_of_det_cases_ct1"
  groupVar <- "reopening_multiplier_4" #"d_Sym_ct1" # "time_to_detection" # "change_testDelay_Sym_1"
  
  
  ## Define label per parameter for plotting
  detectionVar_label <- detectionVar
  isolationVar_label <- isolationVar
  groupVar_label <- groupVar
  
  if (detectionVar == "d_AsP_ct1") detectionVar_label <- "detection of As and P (%)"
  if (detectionVar == "d_Sym_ct1") detectionVar_label <- "increased detection of Sym (%)"
  if (detectionVar == "d_AsPSym_ct1") detectionVar_label <- "detection of As, P, Sym (%)"
  if (groupVar == "d_Sym_ct1") groupVar_label <- "increased detection of Sym (%)"
  if (groupVar == "change_testDelay_Sym_1") groupVar_label <- "reduced detection delay of Sym (days)"
  if (groupVar == "reopening_multiplier_4") groupVar_label <- "% relaxation"
  if (isolationVar == "reduced_inf_of_det_cases_ct1") isolationVar_label <- "isolation success As, P (%)"
  #if (groupVar == "contact_tracing_start_1") trajectoriesDat <- trajectoriesDat %>% mutate(grpvar = as.Date(contact_tracing_start_1 + startdate))
  
  
f_loadTrajectories <- function(useTrim=FALSE){
  ## Load trajectories Dat
  fname =  "trajectoriesDat.csv"
  if(useTrim) fname = "trajectoriesDat_trim.csv"

  trajectoriesDat <- read.csv(file.path(exp_dir, fname))

  ### Discard time entries before reopening date
  trajectoriesDat <- trajectoriesDat %>%
    dplyr::mutate(
      startdate = as.Date(startdate),
      Date = as.Date(time + startdate)
    )
  
  ### Define CT variables
  trajectoriesDat$detection_success <- trajectoriesDat[, colnames(trajectoriesDat) == detectionVar]
  trajectoriesDat$isolation_success <- trajectoriesDat[, colnames(trajectoriesDat) == isolationVar]
  trajectoriesDat$grpvar <- trajectoriesDat[, colnames(trajectoriesDat) == groupVar]
  
  trajectoriesDat$expname <- exp_name
  if (isolationVar == "reduced_inf_of_det_cases_ct1") trajectoriesDat$isolation_success <- 1 - (trajectoriesDat$isolation_success)
  
  return(trajectoriesDat)
  
}


if(!exists("loadTrajectores"))loadTrajectores=TRUE
if(!exists("useTrim"))useTrim=TRUE

if(loadTrajectores){
  trajectoriesDat <- f_loadTrajectories(useTrim=useTrim)
  
  ### Extract relevant dates from trajectpries dat
 # reopeningdate <- unique(as.Date(trajectoriesDat$gradual_reopening_time4, origin = trajectoriesDat$startdate))
  #interventionstart <- unique(as.Date(trajectoriesDat$contact_tracing_start_1, origin = trajectoriesDat$startdate))
  #interventionstop <- unique(as.Date(trajectoriesDat$contact_tracing_stop1, origin = trajectoriesDat$startdate))
  
  reopeningdate <- as.Date("2020-08-30")
  interventionstart <- as.Date("2020-09-01")
  interventionstop <- as.Date("2022-09-01")
  enddate <- as.Date("2020-12-31")
  
}

