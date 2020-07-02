## ==================================================
# R script that analysis trajectoriesDat
## ==================================================

require(tidyverse)
require(cowplot)
require(scales)
require(lattice)
require(readxl)
require(viridis)
require(stringr)
require(ggrepel)
require(broom)


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
exp_names <- exp_names[!grepl("20200627_IL_EMS_stopSIP10_isolationvsdetAsP_bydetSym_witsamples", exp_names)]
exp_names <- exp_names[!grepl("20200628_IL_EMS_stopSIP10_isolationvsdetAsP_bydetSym_lowerTDSym_varyCTtime", exp_names)]
exp_names <- exp_names[!grepl("20200628_IL_EMS_isolationvsdetAsP_bydetSym_stopSIP20", exp_names)]
exp_names <- exp_names[!grepl("20200628_IL_EMS_stopSIP20", exp_names)]
exp_names <- exp_names[!grepl("20200627_IL_EMS_stopSIP10_isolationvsdetAsP_bydetSym_lowerTD_stopAll", exp_names)]
exp_names <- exp_names[!grepl("20200627_IL_EMS_stopSIP10_isolationvsdetAsP_bydetSym", exp_names)]

### Run analysis scripts
describeDat <- FALSE
heatmapPerEMS <- FALSE
tresholdsAll <- FALSE
tresholdsAll2 <- FALSE
estimateRt <- FALSE ## run on quest


geography <- "EMS"
#geography <- "Region"
scalePop <- TRUE

for (exp_name in exp_names) {
  # exp_name <- exp_names[1]
  print(exp_name)
  exp_dir <- file.path(ct_dir, simdate, exp_name)
  ems_dir <- file.path(ct_dir, simdate, exp_name, "per_ems")
  if (!dir.exists(ems_dir)) dir.create(ems_dir)

  Rt_dir <- file.path(ct_dir, simdate, exp_name, "estimatedRt")
  if (!dir.exists(Rt_dir)) dir.create(Rt_dir)
  # nexpsfiles <- list.files(file.path(ct_dir, simdate), pattern = "trajectoriesDat.csv", recursive = TRUE, full.names = TRUE)
  # trajectoriesDat <- sapply(nexpsfiles, read.csv, simplify = FALSE) %>%  bind_rows(.id = "id")


  trajectoriesDat <- read.csv(file.path(exp_dir, "trajectoriesDat.csv"))

  detectionVar <- "d_AsP_ct1" # "d_Sym_ct1"
  isolationVar <- "reduced_inf_of_det_cases_ct1"
  groupVar <- "d_Sym_ct1" # "time_to_detection" # "change_testDelay_Sym_1"

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

  table(trajectoriesDat$grpvar)
  summary(trajectoriesDat$detection_success)
  summary(trajectoriesDat$isolation_success)
  summary(trajectoriesDat$Date)

  summary(trajectoriesDat$Date)

  # outcomeList <- c("critical")
  # for (selected_outcome in outcomeList) {
  selected_outcome <- "critical"

  if (describeDat) source(file.path("ct_analysis/describeTrajectoriesDat.R"))
  if (estimateRt) source(file.path("ct_analysis/get_Rt_from_contactTracingSimulations.R"))
  if (estimateRt) source(file.path("ct_analysis/ct_estimatedRT.R"))

  ### Subset
  trajectoriesDat <- trajectoriesDat %>% filter(time >= as.Date(reopeningdate) - as.Date(max(startdate)) - 30)

  if (heatmapPerEMS) source(file.path("ct_analysis/heatmap_loess_contactTracing.R"))  
  
  
  if (tresholdsAll) source(file.path("ct_analysis/ct_regionAll.R"))
  if (tresholdsAll2) source(file.path("ct_analysis/ct_regionAll_v2.R"))
}
