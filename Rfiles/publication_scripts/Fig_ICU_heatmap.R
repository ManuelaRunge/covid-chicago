
#### Plots edited for publication
library(tidyverse)
library(cowplot)
library(scales)
library(viridis)
library(data.table)

theme_set(theme_cowplot())

# setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("ct_analysis/helper_functions_CT.R")

pdfdir <- "C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/project_notes/publications/covid_model_IL/pdfs_ais/"
ct_dir <- file.path(simulation_output, "contact_tracing")

# region_cols <- c()
restoreRegion_cols <- c("Central" = "red2", "Northcentral" = "dodgerblue3", "Northeast" = "chartreuse4", "Southern" = "orchid4")

simdate ="20200731"
startdate <- "2020-06-15"
stopdate <- "2020-12-30"
reopen <- c(0, 0.05, 0.1)
customTheme <- f_getCustomTheme()
ct_startdate <- as.Date("2020-07-30")

reopeningdate = as.Date("2020-07-22")

####------------------------------------------
#### HEATMAP
####------------------------------------------

if(heatmaps){
  
  
  f_getHeatmap_IL <- function(exp_name="20200731_IL_reopen_contactTracing",regname="illinois", grpValues=c(0.00, 0.05, 0.1), showlegend=FALSE){
    
    load(file.path(simulation_output, "contact_tracing/20200731/",exp_name,"/heatmap_ICU/Illinois_dtfit.Rdata"))
    # thresholdDat <- read_csv("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/contact_tracing/20200731/20200731_IL_reopen_contactTracing/heatmap_ICU/Illinois_loess_ICUcapacity.csv")
    capacity <- load_capacity('illinois') %>% rename(capacity=critical)  %>% rename(region=geography_name) 
    popdat <- load_population() %>% rename(region=geography_name) 
    
    
    ## Aggregate resolution for faster processing and lower image resolution? Or run for interval of five rather than having to aggregate later. 
    aggregateDat=FALSE
    if(aggregateDat){
      dtfit <- dtfit %>% 
        mutate(isolation_success_grp = cut(isolation_success, 100),
               detection_success_grp = cut(detection_success, 100)) %>%
        group_by(region,  grpvar, isolation_success_grp, detection_success_grp) %>%
        summarize(value=mean(value)) %>%
        rename(isolation_success=isolation_success_grp,
               detection_success = detection_success_grp)
    }
    
    
    
    dtfit <- dtfit %>%  
      filter(!is.na(value) & grpvar %in% grpValues) %>%
      dplyr::mutate(region = regname) %>%
      left_join(popdat, by="region") %>%
      left_join(capacity, by="region") %>%
      mutate(percCapacity = (value-capacity)/capacity,
             belowCapacity = ifelse(value <= capacity, 1,0)) %>%
      mutate(value=as.numeric(value),
             pop = as.numeric(pop),
             value_pop1000 = (value /pop )*1000,
             capacity_pop1000 = (capacity /pop )*1000) %>%
      f_valuefct_cap("value") 
    
    labs=quantile(dtfit$percCapacity, probs = seq(0.1,1, 0.1), na.rm=FALSE)
    
    dtfit <- dtfit %>%    mutate(value_cut =cut(percCapacity, 10, labels=labs ) )
    
    
    thresholdDat <- dtfit %>%
      dplyr::filter(value <= capacity ) %>%
      dplyr::group_by(detection_success, grpvar,capacity, pop) %>%
      dplyr::filter(isolation_success == min(isolation_success)) 
    #dplyr::filter(percCapacity == min(percCapacity)) 
    
    pplot <- ggplot(data = subset(dtfit,  !is.na(value_fct)), aes(x = detection_success, y = isolation_success)) +
      theme_minimal() +
      geom_tile(aes(fill = as.factor(value_cut)), alpha = 0.8) +
      scale_fill_viridis(option = "C", discrete = T, direction=-1) +
      geom_line(    data = subset(thresholdDat, isolation_success != min(isolation_success)),
                    aes(x = detection_success, y = isolation_success), size = 1.3
      ) +
      labs(
        x = "detections (%)",
        y = "isolation success (%)",
        col = "",
        fill = "Critical",
        shape = "",
        linetype = ""
      ) +
      scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
      scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
      facet_wrap(~grpvar, nrow=1) +
      customThemeNoFacet +
      theme(panel.spacing = unit(1.5, "lines")) +
      theme(legend.position = "right")+
      theme(strip.text.x = element_text(size = 12, face = "bold", color="white"),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            plot.margin = margin(l = 1)  )
    
    if(showlegend==FALSE) pplot <- pplot + theme(legend.position = "none")
    
    return(pplot)
  }
  
  
  
  legend <- get_legend(f_getHeatmap_IL(exp_name="20200731_IL_reopen_contactTracing", showlegend=TRUE))
  p1 <- f_getHeatmap_IL(exp_name="20200731_IL_reopen_contactTracing")
  p2 <- f_getHeatmap_IL(exp_name="20200731_IL_reopen_contactTracingHS40")
  p3 <- f_getHeatmap_IL(exp_name="20200731_IL_reopen_contactTracingHS80")
  
  
  p123 <- cowplot::plot_grid(p3 + theme(strip.text.x = element_text(size = 12, face = "bold", color="black"), 
                                        p2, 
                                        p1  ),
                             nrow = 3,
                             labels = "auto",
                             align = "hv")
  
  ggsave(paste0("IL_heatmap", ".png"),
         plot = p123, path = file.path(pdfdir), width = 8, height = 8, device = "png"
  )
  
  
}


