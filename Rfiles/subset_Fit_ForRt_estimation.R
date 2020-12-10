
library(tidyverse)
library(data.table)


f_combine_csv <- function(csv_dir, fname){
  
  Files <- list.files(csv_dir)[grep(fname,list.files(csv_dir))]
  
  datList <- list()
  for (file in  Files ) {
    temp_csv <- fread(file.path(csv_dir, file))
    temp_csv$scen_num <- as.numeric(row.names(temp_csv))
    datList[[length(datList)+1]] <-temp_csv
    rm(temp_csv)
  }
  dat <- datList %>% bind_rows() 
  fwrite(dat, file = file.path(csv_dir, paste0("best_parameter_ranges_All.csv")), row.names = FALSE)
  return(dat)
}

## Load directories and custom objects and functions
Location = "LOCAL"
if(Location == "NUCLUSTER") setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
source("load_paths.R")

simulation_output <- file.path(simulation_output, "forFitting")
exp_names <- list.dirs(simulation_output, recursive = FALSE, full.names = FALSE)
exp_name <-  '20201003_IL_mr_fitkistartsm3' # exp_names[1]
exp_dir <- file.path(simulation_output, exp_name)  
Rt_dir <- file.path(simulation_output, exp_name,  "estimatedRt")  
dataRtDir <- file.path(project_path, "Plots + Graphs/Rt_plots")


### Fitting results
fitting_dat <- f_combine_csv(csv_dir=file.path(exp_dir, "fitting/csv/") ,fname="best_parameter_ranges_All.csv") %>% mutate(region =as.numeric(region))
fitting_dat <- fitting_dat %>% rename(scen_num_fit =scen_num) %>% select(region, scen_num_fit)

outcomeVars <- paste0( "infected_cumul_EMS-",c(1:11))


subDatForRt <- fread(file.path(exp_dir, "trajectoriesDat.csv"), select = c("time","scen_num","sample_num","run_num","startdate",outcomeVars))

subDatForRt <- subDatForRt %>%
                mutate(date=time+as.Date(startdate))  %>%
                filter(date <= as.Date("2020-10-01")) %>%
                pivot_longer(cols=-c("date","time","scen_num","sample_num","run_num","startdate")) %>%
                separate(name, into=c("outcome","region"), sep="EMS-") %>%
                mutate(region =as.numeric(region)) %>%
                left_join(fitting_dat, by="region") %>%
                group_by(region) %>%
                filter(scen_num %in% scen_num_fit)


