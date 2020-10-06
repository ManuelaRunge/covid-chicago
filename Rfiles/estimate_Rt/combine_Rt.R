## ============================================================
## R script to combine Rt files 
## ============================================================

library(tidyverse)
library(data.table)


f_combineRT <- function(exp_name,  fname = "_estimated_Rt"){
  
    Rt_dir <- file.path(simulation_output, exp_name,  "estimatedRt")
    RtFiles <- list.files(Rt_dir)[grep(fname,list.files(Rt_dir))]
    
    Rt_datList <- list()
    for (file in  RtFiles ) {
      load(file.path(filedir, file))
      Rt_datList[[length(Rt_datList)+1]] <-Rt_tempdat_All
      rm(Rt_tempdat_All)
    }
    
    Rt_dat <- Rt_datList %>% bind_rows() 
    colnames(Rt_dat) <-  tolower(gsub("[(R)]","", colnames(Rt_dat)))
    
    
    save(Rt_dat, file = file.path(Rt_dir, paste0("combined",fname, ".Rdata")))
    fwrite(Rt_dat, file = file.path(Rt_dir, paste0("combined",fname, ".csv")), row.names = FALSE)
    
}

## Load directories and custom objects and functions
Location = "LOCAL"
if(Location == "NUCLUSTER") setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
source("load_paths.R")

#simulation_output <- file.path(simulation_output, "forFitting")
exp_names = list.dirs(simulation_output, recursive = FALSE, full.names = FALSE)
exp_name =  '20201003_IL_mr_fitkistartsm3' # exp_names[1]

f_combineRT(exp_name)
