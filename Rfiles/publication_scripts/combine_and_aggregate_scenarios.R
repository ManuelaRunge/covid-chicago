

## Load packages
packages_needed <- c( 'tidyverse') 
lapply(packages_needed, require, character.only = TRUE) 

## Load directories and custom objects and functions
#setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
source("load_paths.R")
source("processing_helpers.R")



f_getPredDat <- function(simdir, exp_name) {
  
  expDIR <- file.path(simdir, exp_name )
  trajectoriesDat <- read_csv(file.path(expDIR, "trajectoriesDat_trim.csv"))
  unique(trajectoriesDat$reopening_multiplier_4)
  
  region_names <-c("All", paste0("EMS-",c(1:11)))
    

  ### per restore region
  outcomevars <- c(
   # paste0("deaths_", region_names),
    paste0("infected_", region_names),
    paste0("hosp_cumul_", region_names),
    paste0("hospitalized_det_", region_names),
    paste0("hospitalized_", region_names),
    paste0("infected_", region_names),
   # paste0("deaths_det_", region_names),
    #paste0("prevalence_", region_names),
   paste0("crit_det_", region_names),
   paste0("crit_cumul_", region_names)
  )
  
  
  paramvars <- c("reopening_multiplier_4","change_testDelay_Sym_1","change_testDelay_As_1", "d_Sym_ct1", "d_AsP_ct1", "reduced_inf_of_det_cases_ct1")
  keepvars <- c("time", "startdate", "scen_num",  paramvars, outcomevars)

  predDat <- trajectoriesDat %>%
    dplyr::select(keepvars) %>%
    dplyr::group_by(time, startdate, reopening_multiplier_4,reduced_inf_of_det_cases_ct1, d_AsP_ct1,  change_testDelay_As_1,d_Sym_ct1) %>%
    dplyr::mutate(date = as.Date(startdate) + time) %>%
    pivot_longer(cols = -c("date","time", "startdate", "scen_num",paramvars), names_to = "name") %>%
    dplyr::mutate(
      name = gsub("_cumul_", ".cumul_", name),
      name = gsub("_det_", ".det_", name)
    ) %>%
    separate(name, into = c("param", "region"), sep = "_") %>%
    dplyr::mutate(
      param = gsub(".cumul", "_cumul", param),
      param = gsub(".det", "_det", param)
    ) %>%
    dplyr::group_by(date, region, param, reopening_multiplier_4,reduced_inf_of_det_cases_ct1, d_AsP_ct1,  change_testDelay_As_1,d_Sym_ct1) %>%
    dplyr::summarize(
      median.val = median(value, na.rm = TRUE),
      q25		= quantile(value, probs=0.25, na.rm = TRUE),
      q75		= quantile(value, probs=0.75, na.rm = TRUE),
      q2.5		= quantile(value, probs=0.025, na.rm = TRUE),
      q97.5  	= quantile(value, probs=0.975, na.rm = TRUE)) 
  
  predDat$exp_name <- exp_name
  
  return(predDat)
}

f_combineDat <- function(simdir, exp_name, Rt = FALSE) {
  
  if (Rt == FALSE) df <- f_getPredDat(simdir, exp_name) %>%  mutate(scenario = exp_name)
  if (Rt == TRUE) df <- read.csv(file.path(simdir, exp_name, "estimatedRt", "EMS_combined_estimated_Rt.csv")) %>% mutate(scenario = exp_name)
  
  return(df)
}

simdate = "20200827"
simdir <- file.path(simulation_output, "contact_tracing/",simdate)

exp_names <- list.dirs(simdir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[!grepl("reopen_contactTracing",exp_names)]
exp_names <- exp_names[grep("IL_reopen",exp_names)]



RtList <- list()
for (exp_name in exp_names) {
  # exp_name = exp_names[1]
  
  print(exp_name)

 Rtdf <- f_combineDat(simdir, exp_name, Rt = TRUE)
 RtList[[length(RtList) + 1]] <- Rtdf
  
  rm(Rtdf)

}
RtDatHS <- RtList %>% bind_rows() 
save(RtDatHS, file = file.path(simulation_output, "contact_tracing",simdate,"RtDatHS.Rdata"))

predList <- list()
for (exp_name in exp_names) {

  print(exp_name)
  preddf <- f_combineDat(simdir, exp_name, Rt = FALSE)
  predList[[length(predList) + 1]] <- preddf
  rm(preddf)

}

predDatHS <- predList %>% bind_rows() 
save(predDatHS, file = file.path(simulation_output, "contact_tracing",simdate,"predDatHS.Rdata"))





