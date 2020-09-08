## ============================================================
## R script to combine Rt files
## ============================================================

library(tidyverse)

f_loadAndCombine <- function(filedir, filename, identfier) {
  load(file.path(filedir, paste0(identfier[1], filename, ".Rdata")))
  Rt_dat <- Rt_tempdat_All

  for (i in identfier) {
    load(file.path(filedir, paste0(i, filename, ".Rdata")))
    Rt_dat <- rbind(Rt_dat, Rt_tempdat_All)
    rm(Rt_tempdat_All)
  }
  return(Rt_dat)
}


## Load directories and custom objects and functions
if (!exists("Location")) Location <- "LOCAL"
if (Location == "NUCLUSTER") setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
source("load_paths.R")

if (!exists("exp_name")) exp_name <- "20200825_IL_RR_baseline_0"
fname <- "_estimated_Rt"

Rt_dir <- file.path(simulation_output, exp_name, "estimatedRt")

Rt_dat <- f_loadAndCombine(filedir = Rt_dir, filename = fname, identfier = c(1:11))

save(Rt_dat, file = file.path(Rt_dir, paste0("combined_Rt.Rdata")))
write.csv(Rt_dat, file = file.path(Rt_dir, paste0("combined_Rt.csv")), row.names = FALSE)


