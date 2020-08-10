

## Load packages
packages_needed <- c( 'tidyverse') 
lapply(packages_needed, require, character.only = TRUE) 

## Load directories and custom objects and functions
setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
source("load_paths.R")
source("processing_helpers.R")



f_getPredDat <- function(simdir, exp_name) {
  
  expDIR <- file.path(simdir, exp_name )
  trajectoriesDat <- read.csv(file.path(expDIR, "trajectoriesDat.csv"))
  unique(trajectoriesDat$reopening_multiplier_4)
  
  region_names <- paste0("EMS-",c(1:11))
    
  colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))
  
  ### per restore region
  outcomevars <- c(
    paste0("deaths_", region_names),
    paste0("hosp_cumul_", region_names),
    paste0("hospitalized_det_", region_names),
    paste0("hospitalized_", region_names),
    paste0("infected_", region_names),
    paste0("deaths_det_", region_names),
    paste0("prevalence_", region_names),
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
      mean.val = mean(value, na.rm = TRUE),
      sd.val = sd(value, na.rm = TRUE),
      n.val = n(),
    ) %>%
    dplyr::mutate(
      se.val = sd.val / sqrt(n.val),
      lower.ci.val = mean.val - qt(1 - (0.05 / 2), n.val - 1) * se.val,
      upper.ci.val = mean.val + qt(1 - (0.05 / 2), n.val - 1) * se.val
    )
  
  predDat$exp_name <- exp_name
  
  return(predDat)
}


f_combineDat <- function(simdir, exp_name, Rt = FALSE) {
  
  if (Rt == FALSE) df <- f_getPredDat(simdir, exp_name) %>%  mutate(scenario = exp_name)
  if (Rt == TRUE) df <- read.csv(file.path(simdir, exp_name, "estimatedRt", "EMS_combined_estimated_Rt.csv")) %>% mutate(scenario = exp_name)
  
  return(df)
}


exp_names <- c(
  "20200801_IL_reopen_TD", "20200731_IL_reopen_counterfactual",
  "20200801_IL_reopen_HS40TD", "20200801_IL_reopen_HS40",
  "20200801_IL_reopen_HS80TD", "20200801_IL_reopen_HS80"
)


RtList <- list()
predList <- list()

for (exp_name in exp_names) {
  # exp_name = exp_names[1]
  
  print(exp_name)

  simdir <- file.path(simulation_output, "contact_tracing/20200731/")
  
  Rtdf <- f_combineDat(simdir, exp_name, Rt = TRUE)
  RtList[[length(RtList) + 1]] <- Rtdf
  
  preddf <- f_combineDat(simdir, exp_name, Rt = FALSE)
  predList[[length(predList) + 1]] <- preddf
  
  rm(Rtdf, preddf )
}


predDat <- predList %>% bind_rows()
RtDat <- RtList %>% bind_rows()


save(predDat, file = file.path(simulation_output, "contact_tracing/20200731/combined_predDat.Rdata"))
save(RtDat, file = file.path(simulation_output, "contact_tracing/20200731/combined_RtDat.Rdata"))






