
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
#source("ct_analysis/helper_functions_CT.R")
source("publication_scripts/helper_functions_CT.R")


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
#### ICU HEATMAP
####------------------------------------------

if(heatmaps){
  
  
  f_getHeatmap_IL <- function(exp_name="20200731_IL_reopen_contactTracing",regname="illinois", grpValues=c(0.00, 0.05, 0.1), showlegend=FALSE){
    
    load(file.path(simulation_output, "contact_tracing/20200731/",exp_name,"/heatmap_ICU/Illinois_dtfit.Rdata"))
    # thresholdDat <- read_csv("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/contact_tracing/20200731/20200731_IL_reopen_contactTracing/heatmap_ICU/Illinois_loess_ICUcapacity.csv")
    capacity <- load_capacity('illinois') %>% rename(capacity=icu_available)  %>% rename(region=geography_name) 
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
      scale_fill_viridis(option = "C", discrete = T, drop=F) +
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
      scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
      scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
      facet_wrap(~grpvar, nrow=1) +
      customTheme +
      theme(panel.spacing = unit(1.5, "lines")) +
      theme(legend.position = "right") +
      theme(plot.margin = margin(l = 1) ,
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    if(showlegend==FALSE) pplot <- pplot + theme(legend.position = "none")
    
    return(pplot)
  }
  
  
  
  legend <- get_legend(f_getHeatmap_IL(exp_name="20200731_IL_reopen_contactTracing", showlegend=TRUE))
  p1 <- f_getHeatmap_IL(exp_name="20200731_IL_reopen_contactTracing")
  p2 <- f_getHeatmap_IL(exp_name="20200731_IL_reopen_contactTracingHS40")
  p3 <- f_getHeatmap_IL(exp_name="20200731_IL_reopen_contactTracingHS80")
  

  p123 <- cowplot::plot_grid(p3, p2, p1,
                             nrow = 3,   align = "hv")
  
  ggsave(paste0("IL_heatmap_3", ".png"),
         plot = p123, path = file.path(pdfdir), width = 8, height = 8, device = "png"
  )
  
  
}

if(heatmapCombined){
  
  dat_ct <- read.csv(file.path(ct_dir, "20200731/20200731_IL_reopen_contactTracing/CT_ICU_thresholds.csv")) %>% mutate(scenario="CT")
  dat_ctHS40 <- read.csv(file.path(ct_dir, "20200731/20200731_IL_reopen_contactTracingHS40/CT_ICU_thresholds.csv")) %>% mutate(scenario="CTHS40")
  dat_ctHS80 <- read.csv(file.path(ct_dir, "20200731/20200731_IL_reopen_contactTracingHS80/CT_ICU_thresholds.csv")) %>% mutate(scenario="CTHS80")
  
  dat <- rbind(dat_ct, dat_ctHS40, dat_ctHS80)
  popdat <- load_population() %>% rename(region =geography_name ) 
  #popdatCentral <- popdat
  
  dat <- dat %>%
    left_join(popdat, by = "region") %>%
    group_by(region, grpvar, scenario) %>%
    mutate(fitmax = max(isolation_success, na.rm = TRUE))
  
  
  
  dat$scenario_fct <- factor(dat$scenario,
                                  levels = c("CT", "CTHS40","CTTDonly",  "CTHS80",  "CTHS40TD", "CTHS80TD"),
                                  labels = c(
                                    "CT + current trend",
                                    "CT + increase detections to 40%",
                                    "CT + faster testing and isolation",
                                    "CT + increase detections to 80%",
                                    "CT + increase detections to 40%\n& faster testing and isolation",
                                    "CT + increase detections to 80%\n& faster testing and isolation"
                                  ))
  
  customTheme <- f_getCustomTheme(fontscl=3)
  grpValues=c(0.00, 0.05, 0.1)
  pplot <-  dat %>%
    group_by(region, scenario_fct,  grpvar) %>%
    filter(grpvar %in% grpValues, isolation_success == fitmax & detection_success == min(detection_success) ) %>%
    filter(region %in% c(1:11) ) %>%
    dplyr::group_by( scenario_fct, region, grpvar) %>%
    dplyr::summarize(mean.val = mean(detection_success)) %>%
    ggplot() + 
    geom_boxplot(aes(x = reorder(as.factor(grpvar), mean.val), y = mean.val, col = scenario_fct, group = interaction(grpvar,scenario_fct)),
                 size = 1,width=0.4, position = position_dodge(0.8),outlier.shape = NA
    ) +
    geom_point(aes(x = reorder(as.factor(grpvar), mean.val), y = mean.val, fill = scenario_fct, group = interaction(region,scenario_fct)),
               size = 2.5, position = position_dodge(0.8), shape=21, col="azure4"
    ) +
    labs(
      title = "",
      subtitle = "",
      x = "\nPartial reopening scenario",
      y = "Minimum detection of a- and presymptomatics\nto prevent exceeding ICU capacities\n",
      col = "",
      fill = "",
      shape = "",
      caption= "Each point shows one covid region"
    ) +
    scale_color_brewer(palette="Dark2", drop=F) +
    scale_fill_brewer(palette="Dark2", drop=F) +
    customTheme +
    scale_y_continuous(lim = c(0, 0.7), breaks = seq(0, 0.7, 0.10), labels = seq(0, 70, 10), expand=c(0,0)) +
    background_grid()+
    theme(legend.position="bottom",
          panel.grid.major.x = element_blank()) 

    ggsave(paste0("IL_CT_thresholds_boxplot", ".pdf"),
           plot = pplot, path = file.path(pdfdir), width = 9, height =7, device = "pdf"
    ) 
    
    
   # customTheme <- f_getCustomTheme(fontscl=0)
    plotdat <- dat %>% 
      filter( region %in% c("Illinois") & grpvar %in% grpValues) %>%
      group_by(grpvar, scenario,scenario_fct, detection_success) %>%
      summarize(isolation_success=min(isolation_success))
    
   pplot <-  ggplot(data=plotdat) +
      theme_minimal() +
     geom_ribbon(data=subset(plotdat, scenario=="CTHS80"), aes(
       x = detection_success,
       xmin = detection_success, xmax = Inf,
       ymin = isolation_success, ymax = Inf, fill = as.factor(scenario_fct), group = scenario_fct
     ), alpha = 0.8) +
     geom_ribbon(data=subset(plotdat, scenario=="CTHS40"), aes(
       x = detection_success,
       xmin = detection_success, xmax = Inf,
       ymin = isolation_success, ymax = Inf,  group = scenario_fct
     ), alpha = 1, fill="white") +
     geom_ribbon(data=subset(plotdat, scenario=="CTHS40"), aes(
       x = detection_success,
       xmin = detection_success, xmax = Inf,
       ymin = isolation_success, ymax = Inf, fill = as.factor(scenario_fct), group = scenario_fct
     ), alpha = 0.8) +
     geom_ribbon(data=subset(plotdat, scenario=="CT"), aes(
       x = detection_success,
       xmin = detection_success, xmax = Inf,
       ymin = isolation_success, ymax = Inf,  group = scenario_fct
     ), alpha = 1, fill="white") +
     geom_ribbon(data=subset(plotdat, scenario=="CT"), aes(
       x = detection_success,
       xmin = detection_success, xmax = Inf,
       ymin = isolation_success, ymax = Inf, fill = as.factor(scenario_fct), group = scenario_fct
     ), alpha = 0.8) +
      geom_line(aes(x = detection_success, y = isolation_success, col = as.factor(scenario_fct), group = scenario_fct), size = 1) +
      #facet_wrap(restore_region~region) +
      facet_wrap(~grpvar) +
      scale_color_brewer(palette= "Dark2",  drop=F) +
      scale_fill_brewer(palette= "Dark2",  drop=F) +
      customTheme +
      scale_x_continuous(lim = c(0, 0.7), breaks = seq(0, 0.7, 0.2), labels = seq(0, 0.7, 0.2) * 100, expand = c(0, 0)) +
      scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
      theme(panel.spacing = unit(1, "lines"))+
      geom_hline(yintercept = c(-Inf, Inf)) +
      geom_vline(xintercept = c(-Inf, Inf))+
     labs(
       title = "",
       subtitle = "",
       x = "Detection success (%)",
       y = "Isolation success (%)",
       col = "",
       fill = "",
       shape = "",
       caption= "all IL"
     ) 
    

    

    ggsave(paste0("IL_CT_thresholds_area", ".pdf"),
           plot = pplot, path = file.path(pdfdir), width = 13, height =4.5, device = "pdf"
    ) 
    
  ### show different regions and ranges in thresholds  
if(boxplotVariations_supplement){
  
  pplot <-  dat %>%
    group_by(region, scenario,  grpvar, pop) %>%
    filter(isolation_success == fitmax & detection_success == min(detection_success) & !is.na(pop)) %>%
    # dplyr::group_by(scenario,  grpvar) %>%
    #dplyr::summarize(mean.val = weighted.mean(detection_success, w=pop)) %>%
    #dplyr::summarize(mean.val = mean(detection_success)) %>%
    dplyr::group_by(scenario, grpvar) %>%
    dplyr::summarize(
      min.val = min(detection_success),
      max.val = max(detection_success),
      mean.val = mean(detection_success)
    ) %>% 
    ggplot() + 
    theme_minimal() +
    geom_pointrange(aes(x = as.factor(grpvar), y = mean.val, ymin = min.val, ymax = max.val, col = scenario, group = scenario),
                    size = 1, position = position_dodge(0.3)
    ) +
    labs(
      title = "",
      subtitle = "",
      x = "\nPartial reopening scenario",
      y = "Minimum detection of a- and presymptomatics\nto prevent exceeding ICU capacities\n",
      col = "",
      shape = ""
    ) +
    #scale_fill_manual(values = c("deepskyblue3", "orange", "mediumvioletred")) +
    scale_color_brewer(palette="Dark2") +
    scale_fill_brewer(palette="Dark2") +
    customThemeNoFacet +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10)) +
    theme(panel.grid.major.x = element_blank())
  
  
  
  
  pplot <-  dat %>%
    group_by(region, scenario,  grpvar, pop) %>%
    filter(grpvar %in% grpValues & isolation_success == fitmax & detection_success == min(detection_success) & !is.na(pop)) %>%
    # dplyr::group_by(scenario,  grpvar) %>%
    #dplyr::summarize(mean.val = weighted.mean(detection_success, w=pop)) %>%
    #dplyr::summarize(mean.val = mean(detection_success)) %>%
    f_addRestoreRegion()%>%
    dplyr::group_by(restore_region,scenario, grpvar) %>%
    dplyr::summarize(
      min.val = min(detection_success),
      max.val = max(detection_success),
      mean.val = mean(detection_success)
    ) %>% 
    ggplot() + 
    theme_minimal() +
    geom_pointrange(aes(x = as.factor(scenario), y = mean.val, ymin = min.val, ymax = max.val, col = restore_region, group = interaction(restore_region, grpvar)),
                    size = 1, position = position_dodge(0.7)
    ) +
    labs(
      title = "",
      subtitle = "",
      x = "\nPartial reopening scenario",
      y = "Minimum detection of a- and presymptomatics\nto prevent exceeding ICU capacities\n",
      col = "",
      shape = ""
    ) +
    #scale_fill_manual(values = c("deepskyblue3", "orange", "mediumvioletred")) +
    scale_color_brewer(palette="Dark2") +
    scale_fill_brewer(palette="Dark2") +
    customThemeNoFacet +
    facet_wrap(~grpvar, strip.position = "bottom", nrow=1) +  
    scale_y_continuous(lim = c(0, 0.7), breaks = seq(0, 0.7, 0.20), labels = seq(0, 70, 20)) +
    theme(panel.grid.major.x = element_blank())
  
  
  grpValues=c(0.00, 0.05, 0.1)
  
  pplot <-  dat %>%
    group_by(region, scenario,  grpvar) %>%
    filter(grpvar %in% grpValues, isolation_success == fitmax & detection_success == min(detection_success) ) %>%
    dplyr::group_by( region, grpvar) %>%
    #dplyr::summarize(mean.val = weighted.mean(detection_success, w=pop)) %>%
    dplyr::summarize(mean.val = mean(detection_success)) %>%
    f_addRestoreRegion() %>%
    filter(!is.na(restore_region)) %>%
    ggplot() + 
    theme_minimal() +
    geom_boxplot(aes(x = reorder(as.factor(grpvar), mean.val), y = mean.val, col = restore_region, group = interaction(restore_region,grpvar)),
                 size = 1, position = position_dodge(0.8)
    ) +
    labs(
      title = "",
      subtitle = "",
      x = "\nPartial reopening scenario",
      y = "Minimum detection of a- and presymptomatics\nto prevent exceeding ICU capacities\n",
      col = "",
      shape = ""
    ) +
    #scale_fill_manual(values = c("deepskyblue3", "orange", "mediumvioletred")) +
    scale_color_brewer(palette="Dark2") +
    scale_fill_brewer(palette="Dark2") +
    #facet_wrap(~grpvar, nrow=3)+
    customThemeNoFacet +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10)) +
    theme(panel.grid.major.x = element_blank()) 
  
  

  pplot <-  dat %>%
    group_by(region, scenario,  grpvar) %>%
    filter(grpvar %in% grpValues, isolation_success == fitmax & detection_success == min(detection_success) ) %>%
    dplyr::group_by(scenario, region, grpvar) %>%
    #dplyr::summarize(mean.val = weighted.mean(detection_success, w=pop)) %>%
    dplyr::summarize(mean.val = mean(detection_success)) %>%
    f_addRestoreRegion() %>%
    filter(!is.na(restore_region)) %>%
    ggplot() + 
    theme_minimal() +
    geom_boxplot(aes(x = reorder(as.factor(grpvar), mean.val), y = mean.val, col = scenario, group = interaction(scenario,grpvar)),
                 size = 1, position = position_dodge(0.8)
    ) +
    labs(
      title = "",
      subtitle = "",
      x = "\nPartial reopening scenario",
      y = "Minimum detection of a- and presymptomatics\nto prevent exceeding ICU capacities\n",
      col = "",
      shape = ""
    ) +
    #scale_fill_manual(values = c("deepskyblue3", "orange", "mediumvioletred")) +
    scale_color_brewer(palette="Dark2") +
    scale_fill_brewer(palette="Dark2") +
    facet_wrap(~restore_region, nrow=3)+
    customThemeNoFacet +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 0.7, 0.20), labels = seq(0, 70, 20)) +
    theme(panel.grid.major.x = element_blank()) 
}
  
if(combinedHeatmap){
  

  
  
  
  pplot <- dat %>% 
    filter( region %in% c(1:11) & grpvar %in% grpValues) %>%
    f_addRestoreRegion()%>%
    group_by(restore_region, grpvar, detection_success) %>%
    summarize(isolation_success=min(isolation_success)) %>%
    ggplot() +
    theme_minimal() +
    geom_hline(yintercept = seq(0, 1, 0.1), alpha = 0.3, col = "lightgrey", size = 0.3) +
    geom_vline(xintercept = seq(0, 1, 0.1), alpha = 0.3, col = "lightgrey", size = 0.3) +
    geom_ribbon(aes(
      x = detection_success,
      xmin = detection_success, xmax = 1,
      ymin = isolation_success, ymax = 1, fill = as.factor(grpvar), group = grpvar
    ), alpha = 0.8) +
    geom_line(aes(x = detection_success, y = isolation_success, col = as.factor(grpvar), group = grpvar), size = 1) +
    #facet_wrap(restore_region~region) +
    facet_wrap(~restore_region) +
    scale_color_viridis(option = "C", discrete = TRUE, drop=F) +
    scale_fill_viridis(option = "C", discrete = TRUE, drop=F) +
    customThemeNoFacet +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
    theme(panel.spacing = unit(1, "lines"))+
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))
  
  
  
  
  dat$scenario_fct <- factor(dat$scenario, levels=c("CTHS80","CTHS40","CT"), labels=c("CTHS80","CTHS40","CT"))
  
  pplot <- dat %>% 
    filter( region =="Illinois" & grpvar %in% grpValues) %>%
    ggplot() +
    theme_minimal() +
    geom_hline(yintercept = seq(0, 1, 0.1), alpha = 0.3, col = "lightgrey", size = 0.3) +
    geom_vline(xintercept = seq(0, 1, 0.1), alpha = 0.3, col = "lightgrey", size = 0.3) +
    geom_ribbon(aes(
      x = detection_success,
      xmin = detection_success, xmax = 1,
      ymin = isolation_success, ymax = 1, fill = scenario_fct, group = scenario_fct
    ), alpha = 1) +
    geom_line(aes(x = detection_success, y = isolation_success, col =scenario_fct, group = scenario_fct), size = 1) +
    #facet_wrap(restore_region~region) +
    facet_wrap(~grpvar) +
    scale_color_viridis(option = "C", discrete = TRUE, begin=0.4, end=0.7) +
    scale_fill_viridis(option = "C", discrete = TRUE, begin=0.4, end=0.7) +
    customThemeNoFacet +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
    theme(panel.spacing = unit(1, "lines"))+
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))
  
  
  
  pplot <- dat %>% 
    filter( !(region %in% c("Illinois",c(1:11))) & grpvar %in% grpValues) %>%
    ggplot() +
    theme_minimal() +
    geom_hline(yintercept = seq(0, 1, 0.1), alpha = 0.3, col = "lightgrey", size = 0.3) +
    geom_vline(xintercept = seq(0, 1, 0.1), alpha = 0.3, col = "lightgrey", size = 0.3) +
    geom_ribbon(aes(
      x = detection_success,
      xmin = detection_success, xmax = 1,
      ymin = isolation_success, ymax = 1, fill = scenario_fct, group = scenario_fct
    ), alpha = 0.8) +
    geom_line(aes(x = detection_success, y = isolation_success, col =scenario_fct, group = scenario_fct), size = 1) +
    #facet_wrap(restore_region~region) +
    facet_wrap(region ~ grpvar, ncol=3) +
    scale_color_viridis(option = "viridis", discrete = TRUE, direction = -1) +
    scale_fill_viridis(option = "viridis", discrete = TRUE, direction = -1) +
    customThemeNoFacet +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
    theme(panel.spacing = unit(1, "lines"))+
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))
  
 
  
}  
  
  
  
}

if(regionComparison){
  
  
}



####------------------------------------------
#### Rt HEATMAP
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



