### R script to combine trajectoriesDat subsets and save them separate per region as Rdata or csv file

library(tidyverse)
library(data.table)

exp_name <- "20201110_IL_mr_gradual_reopening_Sep2"
exp_dir <- file.path("/projects/p30781/covidproject/covid-chicago/_temp/",exp_name)
 
f_save_per_grp <- function(filepattern, i,paramVars, outcomeVars_stem, SAVE="csv"){

    outcomeVars <- paste0(outcomeVars_stem , i)
    KeepCols <- c("time", "startdate", "scen_num", "scen_num2", "sample_num", "run_num", paramVars, outcomeVars)
    if(i!="All")KeepCols <- c(KeepCols,paste0('Ki_t',i) )
    
    trajectoriesFiles <- list.files(exp_dir, pattern=filepattern)
    
    subdatList <- list()
    for(tfile in trajectoriesFiles){
      print(tfile)
      subdatList[[length(subdatList)+1]] <- fread(file.path(exp_dir,tfile), select = KeepCols)
    }
  
    subdat <- subdatList %>% bind_rows()
   
  if(tolower(SAVE)=="csv")fwrite(subdat, file.path(exp_dir, paste0("trajectoriesDat_region_",gsub("_EMS-","",i),".csv")))
  if(tolower(SAVE)=="rdata")save(subdat,file=file.path(exp_dir, paste0("trajectoriesDat_region_",gsub("_EMS-","",i),".RData")))
}



for(i in c('All', paste0("_EMS-", c(1:11)))){
  print(paste0("\nStart combining files for region ", i))

  paramVars <- c('reopening_multiplier_4')
  outcomeVars_stem <- c('hosp_det_cumul', 'hosp_cumul',  'crit_cumul',
                        'crit_det_cumul', 'death_det_cumul','deaths', 'crit_det',
                         'critical', 'hosp_det', 'hospitalized', "infected_cumul","infected")
  
  f_save_per_grp( filepattern="trim.csv" ,i, paramVars=paramVars, outcomeVars_stem=outcomeVars_stem, SAVE="RData")
}