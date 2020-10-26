### R script to combine trajectoriesDat subsets and save them separate per region as Rdata or csv file

library(tidyverse)
library(data.table)

exp_name <- "20201025_IL_mr_local_20201003_IL_mr_fitkistartsm3_sm5and6"
exp_dir <- file.path("/projects/p30781/covidproject/covid-chicago/_temp/",exp_name)
 
f_save_per_grp <- function(filepattern, i,paramVars, outcomeVars_stem, SAVE="csv"){

    outcomeVars <- paste0(outcomeVars_stem , paste0("_EMS-", i))
    KeepCols <- c("time", "startdate", "scen_num", "sample_num", paramVars, outcomeVars)
    
    trajectoriesFiles <- list.files(exp_dir, pattern=filepattern)
    
    subdatList <- list()
    for(tfile in trajectoriesFiles){
      subdatList[[length(subdatList)+1]] <- fread(file.path(exp_dir,tfile), select = KeepCols)
    }
  
    subdat <- subdatList %>% bind_rows()
   
  if(tolower(SAVE)=="csv")fwrite(subdat, file.path(exp_dir, paste0("trajectoriesDat_region_",i,".csv")))
  if(tolower(SAVE)=="rdata")save(subdat,file=file.path(exp_dir, paste0("trajectoriesDat_region_",i,".RData")))
}


for(i in c(1:11)){

  paramVars <- c('ki_multiplier_6' , 'ki_multiplier_7', 'ki_multiplier_time_6', 'ki_multiplier_time_7') 
  outcomeVars_stem <- c("death_det_cumul", "crit_det", "hosp_det", "hosp_det_cumul", "infected_cumul")
  
  f_save_per_grp( filepattern="trim.csv" ,i, paramVars=paramVars, outcomeVars_stem=outcomeVars_stem, ,SAVE="RData")
}