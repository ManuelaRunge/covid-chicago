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
source("helper_functions_CT.R")


# Define experiment iteration and simdate
ct_dir <- file.path(simulation_output, "contact_tracing")

simdate <- "20200615"
exp_dir <- file.path(ct_dir, simdate)

reopeningdate <- as.Date("2020-06-01")

detectionVar <- "d_AsP_ct1" 
#detectionVar <- "d_Sym_ct1"
isolationVar <- "reduced_inf_of_det_cases_ct1"
timeVar <- "time_to_detection" # "change_testDelay_Sym_1"

### Limit for ICU beds
labs <- c("EMS_1\n limit: 148", "EMS_2\n limit: 181", "EMS_3\n limit: 103", "EMS_4\n limit: 98", "EMS_5\n limit: 88", "EMS_6\n limit: 109",
          "EMS_7\n limit: 404", "EMS_8\n limit: 255", "EMS_9\n limit: 265", "EMS_10\n limit: 150", "EMS_11\n limit: 785")


### Load simulation data 
nexpsfiles <- list.files(file.path(ct_dir, simdate), pattern = "trajectoriesDat.csv", recursive = TRUE, full.names = TRUE)  #[1]
trajectoriesDat <- sapply(nexpsfiles, read.csv, simplify = FALSE) %>%
  bind_rows(.id = "id")

### Discard time entries before reopening date
trajectoriesDat <- trajectoriesDat %>% 
                    filter(time >= as.Date(reopeningdate) - as.Date(max(startdate))) %>%
                    mutate(startdate = as.Date(startdate), 
                           Date = as.Date(time + startdate))

### Define CT variables 
trajectoriesDat$detection_success <- trajectoriesDat[, colnames(trajectoriesDat)==detectionVar]
trajectoriesDat$isolation_success <- 1-(trajectoriesDat[, colnames(trajectoriesDat)==isolationVar])
trajectoriesDat$time_to_detection <- trajectoriesDat[, colnames(trajectoriesDat)==timeVar]


### Run analysis scripts
describeDat = FALSE
heatmapPerEMS=FALSE
tresholdsAll=FALSE
estimateRt=FALSE  ## run on quest

if(describeDat)source(file.path("ct_analysis/describeTrajectoriesDat.R"))

geography <- "EMS"
# geography <- "Region"
if(heatmapPerEMS)source(file.path("ct_analysis/heatmap_contactTracing.R"))

if(tresholdsAll)source(file.path("ct_analysis/ct_regionAll.R"))

if(estimateRt)source(file.path("ct_analysis/get_Rt_from_contactTracingSimulations.R"))

if(estimateRt)source(file.path("ct_analysis/estimateRt.R"))

