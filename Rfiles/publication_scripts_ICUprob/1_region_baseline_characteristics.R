
if(!exists("wdir")){
  library(tidyverse)
  library(cowplot)
  library(data.table)
  library(raster)
  
  source("load_paths.R")
  source("setup.R")
  source("processing_helpers.R")
  source("publication_scripts_ICUprob/functions.R")
  
  theme_set(theme_cowplot())
  customTheme <- f_getCustomTheme(fontscl = 0)
  
  SAVE <- FALSE
  analysis_dir <- file.path(simulation_output, "_overflow_simulations")
  civis_dir <- file.path(simulation_output, "_simulations_for_civis")
  outdir <- file.path(file.path(project_path, "/project_notes/publications/covid_model_IL_overflow/outputs"))
  simulation_output <- file.path(simulation_output, "_overflow_simulations")
}


