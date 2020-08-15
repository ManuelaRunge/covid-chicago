
library(tidyverse)
library(cowplot)
library(scales)
library(viridis)

source("load_paths.R")
source("processing_helpers.R")

plot_first_day ="2020-08-01"
plot_last_day ="2021-04-01"

f_plot_param <- function(exp_name, paramname, emsname, timeVarying = TRUE, SAVE = TRUE,keepScens=FALSE) {
  trajectoriesDat <- read.csv(file.path(simulation_output,exp_name, "trajectoriesDat.csv"))

  ### Note use Ki_EMS-  or Ki_EMS.   not Ki_EMS_  (time varying vs input parameter)
  colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))
  paramvars <- paste0(paramname, emsname, c(1:11))
  keepvars <- c("time", "startdate", paramvars)
  if(keepScens) keepvars <- c(keepvars, "scen_num")

  ### Wide to long format and calculate date
  ### And aggregate samples
  
  paramvalues <- trajectoriesDat %>%
    select(keepvars) %>%
    mutate(date = as.Date(startdate) + time) %>%
    pivot_longer(cols = -c("time", "date", "startdate"), names_to = "region") %>%
    mutate(
      region = gsub(emsname, "", gsub(paramname, "", region)),
      region = as.numeric(region),
      exp_name = exp_name,
    ) %>%
    group_by(date, region,exp_name) %>%
    summarize(value = mean(value)) %>%
    filter(date >= as.Date("2020-08-01") & date <= as.Date("2020-08-02"))

  if (timeVarying == FALSE) {
    pplot <- trajectoriesDat %>%
      select(keepvars) %>%
      mutate(date = as.Date(startdate) + time) %>%
      pivot_longer(cols = -c("time", "date", "startdate"), names_to = "region") %>%
      mutate(
        region = gsub(emsname, "", gsub(paramname, "", region)),
        region = as.numeric(region),
        exp_name = exp_name,
      ) %>%
      group_by(date, region,exp_name) %>%
      summarize(value = mean(value)) %>%
      filter(date >= as.Date("2020-08-01") & date <= as.Date("2020-08-02")) %>%
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
  if (timeVarying == TRUE & keepScens==FALSE ) {
    pplot <- trajectoriesDat %>%
      select(keepvars) %>%
      mutate(date = as.Date(startdate) + time) %>%
      pivot_longer(cols = -c("time", "date", "startdate"), names_to = "region") %>%
      mutate(
        region = gsub(emsname, "", gsub(paramname, "", region)),
        region = as.numeric(region),
        exp_name = exp_name,
      ) %>%
      group_by(date, region,exp_name) %>%
      summarize(value = mean(value)) %>%
      filter(date <= as.Date(plot_last_day)) %>%
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
  
  
  
  if (timeVarying == TRUE & keepScens==TRUE) {
    pplot <- trajectoriesDat %>%
      select(keepvars) %>%
      mutate(date = as.Date(startdate) + time) %>%
      pivot_longer(cols = -c("time", "date", "startdate","scen_num"), names_to = "region") %>%
      mutate(
        region = gsub(emsname, "", gsub(paramname, "", region)),
        region = as.numeric(region),
        exp_name = exp_name,
      ) %>%
      group_by(date, region,exp_name,scen_num) %>%
      summarize(value = mean(value)) %>%
      filter(date <= as.Date(plot_last_day)) %>%
      filter(date >= as.Date(plot_first_day)) %>%
      ggplot() +
      theme_cowplot() +
      geom_line(aes(x = date, y = value, col = as.factor(scen_num), group = scen_num), size = 1) +
      scale_color_viridis(discrete = TRUE) +
      labs(
        y = gsub("_"," ",paramname),
        subtitle = "", # Estimated change in transmission intensity\n
        title = "",
        x = "",
        col="scen_num"
      ) +
      customThemeNoFacet +
      scale_y_continuous(expand = c(0, 0))+
      facet_wrap(~region)
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



### Load trajectories Dat
exp_names <- list.dirs(simulation_output, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("rollback",exp_names)]
#exp_names <- exp_names[!grepl("MR_t",exp_names)]

for(exp_name in exp_names){
  #exp_name = "20200812_IL_MR_hospitalized90_rollback"
  #simulation_output <-  file.path(simulation_output, "EMS/20200730_increasedSym/")
  
  
  f_plot_param(exp_name = exp_name, paramname = "social_multiplier_4_", emsname="EMS_", timeVarying = FALSE)
  f_plot_param(exp_name = exp_name, paramname = "backtonormal_multiplier_1_", emsname="EMS_", timeVarying = FALSE)
  f_plot_param(exp_name = exp_name, paramname = "Ki_t_", emsname = "EMS_", timeVarying = TRUE)
  f_plot_param(exp_name = exp_name, paramname = "d_Sym_t_", emsname = "EMS_", timeVarying = TRUE)
  
  
  
  out <- f_plot_param(exp_name = exp_name, paramname = "Ki_t_", emsname = "EMS_", timeVarying = TRUE, SAVE=F , keepScens=TRUE)
  pplot <- out[[2]]
  pplot <- pplot + theme(legend.position = "none")
  ggsave(paste0( "Ki_triggered_byScenNum.png"),
         plot = pplot, path = file.path(simulation_output, exp_name), width = 8, height = 5, device = "png"
  )

}




