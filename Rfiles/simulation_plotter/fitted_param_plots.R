
library(tidyverse)
library(cowplot)
library(scales)
library(viridis)

source("load_paths.R")
source("processing_helpers.R")


f_plot_param <- function(exp_name, paramname, emsname, timeVarying = TRUE, SAVE = TRUE, 
                         plot_start_date="2020-03-01", plot_end_date="2020-12-30") {

  ### Note use Ki_EMS-  or Ki_EMS.   not Ki_EMS_  (time varying vs input parameter)
  paramvars <- paste0(paramname, emsname, c(1:11))
  keepvars <- c("time", "startdate", paramvars)
  
  trajectoriesDat <- fread(file.path(simulation_output,exp_name, "trajectoriesDat.csv"), select=keepvars)

  ### Wide to long format and calculate date
  ### And aggregate samples
  paramvalues <- trajectoriesDat %>%
    mutate(date = as.Date(startdate) + time) %>%
    pivot_longer(cols = -c("time", "date", "startdate"), names_to = "region") %>%
    mutate(
      region = gsub(emsname, "", gsub(paramname, "", region)),
      region = as.numeric(region),
      exp_name = exp_name,
    ) %>%
    group_by(date, region,exp_name) %>%
    summarize(value = mean(value)) %>%
    filter(date >= Sys.Date() & date <= Sys.Date()+1)

  if (timeVarying == FALSE) {
    pplot <- trajectoriesDat %>%
      mutate(date = as.Date(startdate) + time) %>%
      pivot_longer(cols = -c("time", "date", "startdate"), names_to = "region") %>%
      mutate(
        region = gsub(emsname, "", gsub(paramname, "", region)),
        region = as.numeric(region),
        exp_name = exp_name,
      ) %>%
      group_by(date, region,exp_name) %>%
      summarize(value = mean(value)) %>%
      filter(date >= Sys.Date() & date <= Sys.Date()+1) %>%
      ggplot() +
      theme_cowplot() +
      geom_bar(aes(x = as.factor(region), y = value, group = region), stat = "identity", size = 1.3, position = "dodge") +
      scale_color_viridis(discrete = TRUE) +
      labs(
        y = gsub("_"," ",paramname), #"% relaxation"
        title = "",
        #subtitle = "Estimated relaxation of shelter-in-place polices\n (reopening 21st June)",
        subtitle = "",
        x = "region"
      ) +
      customThemeNoFacet +
      scale_y_continuous(expand = c(0, 0)) 
  }
  if (timeVarying == TRUE) {
    pplot <- trajectoriesDat %>%
      mutate(date = as.Date(startdate) + time) %>%
      pivot_longer(cols = -c("time", "date", "startdate"), names_to = "region") %>%
      mutate(
        region = gsub(emsname, "", gsub(paramname, "", region)),
        region = as.numeric(region),
        exp_name = exp_name,
      ) %>%
      group_by(date, region,exp_name) %>%
      summarize(value = mean(value)) %>%
      filter(date >= as.Date(plot_start_date) & date <= as.Date(plot_end_date)) %>%
      ggplot() +
      theme_cowplot() +
      geom_line(aes(x = date, y = value, col = as.factor(region), group = region), size = 1.3) +
      scale_color_viridis(discrete = TRUE) +
      labs(
        y = gsub("_"," ",paramname),
        subtitle = "", # Estimated change in transmission intensity\n
        title = "",
        x = "",
        col="covid region"
      ) +
      customThemeNoFacet +
      scale_y_continuous(expand = c(0, 0))
  }

  if (SAVE) {
    ggsave(paste0(paramname, ".png"),
      plot = pplot, path = file.path(simulation_output, exp_name), width = 8, height = 5, device = "png"
    )
    ggsave(paste0(paramname, ".pdf"),
           plot = pplot, path = file.path(simulation_output, exp_name), width = 8, height = 5, device = "pdf"
    )
  }

  out <- list(paramvalues, pplot)
  return(out)
}


f_Ki_table(exp_name, paramname = "Ki_t_",emsname="EMS_"){
  
  paramvars <- paste0(paramname, emsname, c(1:11))
  keepvars <- c("time", "startdate", paramvars)
  
  trajectoriesDat <- fread(file.path(simulation_output,exp_name, "trajectoriesDat.csv"), select=keepvars)
  
  kidat <- trajectoriesDat %>%
    mutate(date = as.Date(startdate) + time) %>%
    pivot_longer(cols = -c("time", "date", "startdate"), names_to = "region") %>%
    mutate(
      region = gsub(emsname, "", gsub(paramname, "", region)),
      region = as.numeric(region),
      exp_name = exp_name,
    ) %>%
    group_by(date, region,exp_name) %>%
    summarize(ki_median = median(value),
              ki_lower = quantile(value, probs = 0.025, na.rm = TRUE),
              ki_upper = quantile(value, probs = 0.975, na.rm = TRUE)) %>%
    filter(date >= as.Date(plot_start_date) & date <= as.Date(plot_end_date)) 
  
  initialKi <- kidat %>% 
    filter(date >= as.Date(initialDate) & date <= as.Date(initialDate)+1) %>% 
    select(region, date, ki_median ,ki_lower, ki_upper,date) %>%
    mutate(Ki_type = paste0("initial"))
  
  lowestKi <- kidat %>%  
    group_by(region) %>%
    filter(date <= Sys.Date()) %>% 
    filter(ki_median == min(ki_median)) %>% 
    select(region, date, ki_median ) %>%
    filter(date ==min(date) ) %>%
    mutate(Ki_type = paste0("lowest") )
  
  baselineKi <- kidat %>% 
    filter(date >= as.Date(baselineDate) & date <= as.Date(baselineDate)+1) %>% 
    select(region,date,  ki_median ,ki_lower, ki_upper,date) %>%
    mutate(Ki_type = paste0("baseline"))
  
  Kidat <- rbind(initialKi, lowestKi, baselineKi)
  
  Kidat <- data.table(Kidat, key =c( "region" ))
  Kidat[,Ki_red_lockdown := 1-(ki_median[Ki_type=="lowest"] / ki_median[Ki_type=="initial"]), by = c( "region" ) ]
  Kidat[,Ki_incr_reopen := 1-(ki_median[Ki_type=="baseline"] / ki_median[Ki_type=="lowest"]), by = c( "region" ) ]
  Kidat[,Ki_current_initial := 1-(ki_median[Ki_type=="baseline"] / ki_median[Ki_type=="initial"]), by = c( "region" ) ]
  
  fwrite(Kidat, file.path(project_path,"project_notes", "estimated_baseline_Ki.csv"))
  
  
}


### Load trajectories Dat
exp_name <- "20200915_IL_RR_fitting_0"

#simulation_output <-  file.path(simulation_output, "EMS/20200730_increasedSym/")


f_plot_param(exp_name = exp_name, paramname = "social_multiplier_4_", emsname="EMS_", timeVarying = FALSE)
f_plot_param(exp_name = exp_name, paramname = "social_multiplier_5_", emsname="EMS_", timeVarying = FALSE)
f_plot_param(exp_name = exp_name, paramname = "social_multiplier_6_", emsname="EMS_", timeVarying = FALSE)
f_plot_param(exp_name = exp_name, paramname = "backtonormal_multiplier_1_", emsname="EMS_", timeVarying = FALSE)
f_plot_param(exp_name = exp_name, paramname = "Ki_t_", emsname = "EMS-", timeVarying = TRUE, plot_start_date = "2020-07-01")
f_plot_param(exp_name = exp_name, paramname = "d_Sym_t_", emsname = "EMS-", timeVarying = TRUE)

## Additional parameters depending on the scenario
#f_plot_param(exp_name = exp_name, paramname = "triggertime_", emsname = "EMS-", timeVarying = TRUE)



