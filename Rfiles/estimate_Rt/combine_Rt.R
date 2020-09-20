## ============================================================
## R script to combine Rt files
## ============================================================

library(tidyverse)
library(data.table)

f_loadAndCombine <- function(filedir, filename) {
  rtfiles <- list.files(filedir, pattern = fname, recursive = FALSE)
  rtfiles <- rtfiles[!(grepl("combined", rtfiles))]

  load(file.path(filedir, rtfiles[1]))
  Rt_dat <- Rt_tempdat_All

  for (rtfile in rtfiles) {
    load(file.path(filedir, rtfile))
    Rt_dat <- rbind(Rt_dat, Rt_tempdat_All)
    rm(Rt_tempdat_All)
  }
  return(Rt_dat)
}


## Load directories and custom objects and functions
Location <- "Local"
if (Location == "NUCLUSTER") setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
source("load_paths.R")

exp_name <- "20200917_IL_gradual_reopening"
fname <- "_estimated_Rt.Rdata"

Rt_dir <- file.path(simulation_output, exp_name, "estimatedRt")

Rt_dat <- f_loadAndCombine(filedir = Rt_dir, filename = fname)

save(Rt_dat, file = file.path(Rt_dir, paste0("combined", fname, ".Rdata")))
fwrite(Rt_dat, file = file.path(Rt_dir, paste0("combined", fname, ".csv")), row.names = FALSE)
