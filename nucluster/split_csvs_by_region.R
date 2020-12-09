### R script to combine trajectoriesDat subsets and save them separate per region as Rdata or csv file

library(tidyverse)
library(data.table)

combineCSVs1 <- TRUE
combineCSVs2 <- FALSE

exp_names <- c(
  "20201121_IL_regreopen100perc_3daysdelay_sm4", "20201121_IL_regreopen100perc_3daysdelay_sm8",
  "20201121_IL_regreopen50perc_3daysdelay_sm4", "20201121_IL_regreopen50perc_3daysdelay_sm8", "20201121_IL_regreopen50perc_7daysdelay_sm8"
)


for (exp_name in exp_names) {
  exp_dir <- file.path("/projects/p30781/covidproject/covid-chicago/_temp/", exp_name)

  f_save_per_grp <- function(filepattern, i, paramVars, outcomeVars_stem, SAVE = "csv") {
    outcomeVars <- paste0(outcomeVars_stem, i)
    KeepCols <- c("time", "startdate", "scen_num", "scen_num2", "sample_num", "run_num", paramVars, outcomeVars)
    if (i != "All") KeepCols <- c(KeepCols, paste0("Ki_t", i), paste0("triggertime", i))
    trajectoriesFiles <- list.files(exp_dir, pattern = filepattern)

    subdatList <- list()
    for (tfile in trajectoriesFiles) {
      print(tfile)
      subdatList[[length(subdatList) + 1]] <- fread(file.path(exp_dir, tfile), select = KeepCols)
    }

    subdat <- subdatList %>% bind_rows()

    if (tolower(SAVE) == "csv") fwrite(subdat, file.path(exp_dir, paste0("trajectoriesDat_region_", gsub("_EMS-", "", i), ".csv")))
    if (tolower(SAVE) == "rdata") save(subdat, file = file.path(exp_dir, paste0("trajectoriesDat_region_", gsub("_EMS-", "", i), ".RData")))
  }

  if (combineCSVs1) {
    for (i in c(paste0("_EMS-", c(1:11)))) {
      print(paste0("\nStart combining files for region ", i))

      paramVars <- c("capacity_multiplier", "trigger_delay_days") # c('reopening_multiplier_4') #
      outcomeVars_stem <- c(
        "hosp_det_cumul", "hosp_cumul", "crit_cumul",
        "crit_det_cumul", "death_det_cumul", "deaths", "crit_det",
        "critical", "hosp_det", "hospitalized", "infected_cumul", "infected"
      )


      f_save_per_grp(filepattern = "trim.csv", i, paramVars = paramVars, outcomeVars_stem = outcomeVars_stem, SAVE = "csv")
      # f_save_per_grp( filepattern="trim.csv" ,i, paramVars=paramVars, outcomeVars_stem=outcomeVars_stem, SAVE="RData")
    }
  }


  ### Combine to csv
  if (combineCSVs2) {
    count <- 0
    for (i in c(c(1:11))) {
      count <- count + 1
      # load(file.path(exp_dir,paste0("trajectoriesDat_region_",i,".RData")))
      subdat <- fread(file.path(exp_dir, paste0("trajectoriesDat_region_", i, ".csv")))
      if (count == 1) trajectoriesDat <- subdat
      if (count > 1) trajectoriesDat <- left_join(trajectoriesDat, subdat, by = c("time", "startdate", "scen_num", "sample_num", "capacity_multiplier", "trigger_delay_days"))
      rm(subdat)
      fwrite(trajectoriesDat, file = file.path(exp_dir, "trajectoriesDat.csv"), quote = FALSE)
      # save(trajectoriesDat, file=file.path(exp_dir, "trajectoriesDat.Rdata"))
    }
  }
}
