
library(tidyverse)
library(cowplot)
library(scales)
library(viridis)

source("load_paths.R")
source("processing_helpers.R")


f_plot_param <- function(exp_name, paramname, emsname, timeVarying = TRUE, SAVE = TRUE, 
                         plot_start_date="2020-03-01", plot_end_date="2020-11-10", exp_dir=NULL) {

  ### Note use Ki_EMS-  or Ki_EMS.   not Ki_EMS_  (time varying vs input parameter)
  if(is.null(exp_dir))exp_dir=file.path(simulation_output, exp_name)
  paramvars <- paste0(paramname, emsname, c(1:11))
  keepvars <- c("time", "startdate", paramvars)
  
  byParameter=TRUE
  trajectoriesDat <- fread(file.path(exp_dir, "trajectoriesDat.csv"), select=keepvars)

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
    filter(as.character(date)  %in% c(plot_start_date, plot_end_date,as.character(Sys.Date()) ))

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
    
    if (byParameter == FALSE) {
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
      background_grid()+
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
    
    if (byParameter == TRUE) {
      keepvars <- c(keepvars, "reopening_multiplier_4")
      keepvars <- unique(keepvars)
      trajectoriesDat <- fread(file.path(exp_dir, "trajectoriesDat.csv"), select=keepvars)
      
      paramvalues <- trajectoriesDat %>%
        mutate(date = as.Date(startdate) + time) %>%
        pivot_longer(cols = -c("time", "date", "startdate",'reopening_multiplier_4'), names_to = "region") %>%
        mutate(
          region = gsub(emsname, "", gsub(paramname, "", region)),
          region = as.numeric(region),
          exp_name = exp_name,
        ) %>%
        group_by(date, region,exp_name, reopening_multiplier_4) %>%
        summarize(value = mean(value)) %>%
        filter(as.character(date)  %in% c(plot_start_date, plot_end_date,as.character(Sys.Date()) ))
      
      
      pplot <- trajectoriesDat %>%
        mutate(date = as.Date(startdate) + time) %>%
        pivot_longer(cols = -c("time", "date", "startdate",'reopening_multiplier_4'), names_to = "region") %>%
        mutate(
          region = gsub(emsname, "", gsub(paramname, "", region)),
         # region = as.numeric(region),
          exp_name = exp_name,
        ) %>%
        group_by(date, region,exp_name, reopening_multiplier_4) %>%
        summarize(value = mean(value)) %>%
        filter(date >= as.Date(plot_start_date) & date <= as.Date(plot_end_date)) %>%
        ggplot() +
        theme_cowplot() +
        geom_line(aes(x = date, y = value, col = as.factor(reopening_multiplier_4), group = region), size = 1.3) +
        scale_color_viridis(discrete = TRUE) +
        background_grid()+
        labs(
          y = gsub("_"," ",paramname),
          subtitle = "", # Estimated change in transmission intensity\n
          title = "",
          x = "",
          col="covid region"
        ) +
        customThemeNoFacet +
        scale_y_continuous(expand = c(0, 0))+
        facet_wrap(~region, scales="free")
    }
    
  }

  if (SAVE) {
    ggsave(paste0(paramname, "_3.png"),
      plot = pplot, path = file.path(exp_dir), width = 8, height = 5, device = "png"
    )
    ggsave(paste0(paramname, "_3.pdf"),
           plot = pplot, path = file.path(exp_dir), width = 8, height = 5, device = "pdf"
    )
  }

  out <- list(paramvalues, pplot)
  return(out)
}


 



### Load trajectories Dat
exp_name <- "20201110_IL_mr_gradual_reopening_Sep2" #"20201104_IL_mr_local_baseline"

#simulation_output <-  file.path(simulation_output, "EMS/20200730_increasedSym/")


out <-f_plot_param(exp_name = exp_name, paramname = "Ki_t_", 
             emsname = "EMS-", timeVarying = TRUE, plot_start_date = "2020-03-01", 
             exp_dir=file.path(simulation_output,"_overflow_simulations",exp_name))


#f_plot_param(exp_name = exp_name, paramname = "social_multiplier_4_", emsname="EMS_", timeVarying = FALSE)
#f_plot_param(exp_name = exp_name, paramname = "social_multiplier_5_", emsname="EMS_", timeVarying = FALSE)
#f_plot_param(exp_name = exp_name, paramname = "social_multiplier_6_", emsname="EMS_", timeVarying = FALSE)
#f_plot_param(exp_name = exp_name, paramname = "backtonormal_multiplier_1_", emsname="EMS_", timeVarying = FALSE)
#f_plot_param(exp_name = exp_name, paramname = "d_Sym_t_", emsname = "EMS-", timeVarying = TRUE)

## Additional parameters depending on the scenario
#f_plot_param(exp_name = exp_name, paramname = "triggertime_", emsname = "EMS-", timeVarying = TRUE)



paramvalues_wide <- paramvalues %>% ungroup() %>%
  mutate(phase = ifelse(as.character(date) == plot_start_date, "initial", "latest")) %>%
  dplyr::select( -date, -exp_name) %>% unique() %>%as.data.frame() %>%
  pivot_wider(names_from="phase", values_from="value")

