
## Load packages
packages_needed <- c( 'tidyverse') 
lapply(packages_needed, require, character.only = TRUE) 

## Load directories and custom objects and functions
if(!exists("Location"))Location="LOCAL"
if(Location=="NUCLUSTER")setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
source("load_paths.R")
source("processing_helpers.R")

#### Combine critical 
f_combineCSVs <- function(exp_dir,  fname = "ICUcapacity.csv", fnameout="CT_ICU_thresholds.csv"){
  
  thresholdsfiles <- list.files(file.path(exp_dir),fname, recursive = TRUE, full.names = TRUE)
  
  
  if(length(thresholdsfiles)!=0){
    print("file found - processing")

  datlist <- list()
  for(i in c(1:length(thresholdsfiles))){
    temp <-  read.csv(thresholdsfiles[i])
    
    if(dim(temp)[1]<1)next
    
    temp$id <- thresholdsfiles[i]
    
    colnames(temp) <- gsub("regin","region",    colnames(temp))
    temp$region <- as.character(temp$region )
    datlist[[length(datlist)+1]] <- temp 
    
    
  }
  
  lmthresholdsDat <- datlist %>% bind_rows()
  
  if (dim(lmthresholdsDat)[1] <= 1) next
  
  colnames(lmthresholdsDat)[colnames(lmthresholdsDat)=="regin"] <- "region"
  
  lmthresholdsDat$id <- gsub(exp_dir, "", lmthresholdsDat$id)

  write.csv(lmthresholdsDat, file.path(exp_dir, fnameout), row.names = FALSE)
  }
  if(length(thresholdsfiles)==0){
    print("file not found")
  }
  
  rm(thresholdsfiles)
}



simdate <- "20200827"

exp_names <- list.dirs( file.path(simulation_output,'contact_tracing',simdate), recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("reopen_contact",exp_names)]

for (exp_name in exp_names) {
  # exp_name = exp_names[1]

  
  print(exp_name)
  exp_dir <- file.path(simulation_output,'contact_tracing',simdate, exp_name)
  
  f_combineCSVs(exp_dir,  fname = "loess_ICUcapacity.csv", fnameout="CT_ICU_thresholds.csv")
  
  print("CT_RT")
  f_combineCSVs(exp_dir,  fname = "loess_Rt.csv", fnameout="CT_Rt_thresholds.csv")
  

}





