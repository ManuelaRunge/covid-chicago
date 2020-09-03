### =======================================================
#### Additional plots for contact tracing simulations
### =======================================================

#### Plots edited for publication
library(tidyverse)
library(cowplot)
library(scales)
library(viridis)
library(data.table)

theme_set(theme_cowplot())

Location="LOCAL"
pdfdir <- "C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/project_notes/publications/covid_model_IL/pdfs_ais/"

if(Location=="NUCLUSTER") {
  setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
  pdfdir <- "/home/mrm9534/Box/NU-malaria-team/projects/covid_chicago/project_notes/publications/covid_model_IL/pdfs_ais/"
}

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts/helper_functions_CT.R")


ct_dir <- file.path(simulation_output, "contact_tracing")

# region_cols <- c()
restoreRegion_cols <- c("Central" = "red2", "Northcentral" = "dodgerblue3", "Northeast" = "chartreuse4", "Southern" = "orchid4")

simdate ="20200827"
startdate <- "2020-07-01"
stopdate <- "2020-12-30"

reopen <- c(0, 0.05, 0.1, 0.15, 0.20)
customTheme <- f_getCustomTheme()
ct_startdate <- as.Date("2020-09-01")
reopeningdate = as.Date("2020-08-30")

## ================================================
###  Figure 3 - HS scenarios - no contact tracing
## ================================================

load(file.path(simulation_output, "contact_tracing",simdate,"predDatHS.Rdata"))



capacityDat <- load_new_capacity()

popdat <- load_population() %>% 
  mutate(region =as.character(geography_name) ) %>% 
  filter(region !="illinois") %>%
  f_addRestoreRegion() %>% 
  group_by(restore_region) %>%
  summarize(pop=sum(as.numeric(pop))) %>%
  mutate(geography_name = tolower(restore_region))


#### ---------------------------
capacity_long <- capacityDat %>%
  pivot_longer(cols = -c("geography_name"), names_to = "param", values_to = "capacity") %>%
  filter(param != "vents_available") %>%
  mutate(param = case_when(
    param == "icu_available" ~ "critical",
    param == "medsurg_available" ~ "hospitalized"
  )) 

capacity_long <- capacity_long %>%
  filter(param != "vents_available") %>%
  mutate(param = case_when(
    param == "hospitalized" ~ "hosp_cumul",
    param == "critical" ~ "crit_cumul"
  )) %>%
  rbind(capacity_long) %>%
  group_by(param) %>%
  summarize(capacity=sum(capacity))


load(file.path(simulation_output, "contact_tracing",simdate,"predDatHS.Rdata"))
predDatHS <- predDatHS %>%
  mutate(date = as.character(date)) %>%
  mutate(date = as.Date(date))


customTheme <- f_getCustomTheme()

predDatHS$scenario_fct <- factor(predDatHS$scenario,
                                 levels = c("counterfactual", "HS40", "TDonly","HS80", "HS40TD", "HS80TD"),
                                 labels = c(
                                   "current trend (comparison)",
                                   "increase detections to 40%",
                                   "faster testing and isolation",
                                   "increase detections to 80%",
                                   "increase detections to 40%\n& faster testing and isolation",
                                   "increase detections to 80%\n& faster testing and isolation"
                                 ))


#### Sum per IL 
table(predDatHS$param)

ILcapacity = capacityDat %>% filter(geography_name=="illinois")%>% select(icu_available) %>% as.numeric()
  
predplotDat <- predDatHS %>%
  filter(region=="All" &
          param %in% c("infected",  "hospitalized_det", "crit_det") &
           date >= as.Date(startdate) & date <= as.Date(stopdate) &
           reopening_multiplier_4 %in% reopen) %>%
  dplyr::select(-c('sd.val', 'n.val', 'se.val', 'lower.ci.val', 'upper.ci.val')) %>% 
  pivot_wider(names_from = "param", values_from="mean.val") %>%
  ungroup() %>%
  dplyr::select(date,  scenario, scenario_fct, reopening_multiplier_4,infected, hospitalized_det,crit_det) %>%
  pivot_longer(cols=-c(date,  scenario, scenario_fct, reopening_multiplier_4), names_to="param", values_to="mean.val") %>%
  mutate(pop=sum(popdat$pop),
         icu_available =ILcapacity) %>%
  mutate(icu_available_per1000pop = (icu_available/pop)*1000,
         mean.val_per1000pop = (mean.val/pop)*1000) 


predplot <- predplotDat %>%
  filter(param %in% c("critical")) %>%
  ggplot() +
  theme_cowplot() +
  # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
  geom_line(aes(x = date, y = mean.val_per1000pop, col = scenario_fct, group = scenario), size = 1.3) +
  geom_hline(aes(yintercept = icu_available_per1000pop), linetype = "dashed", col = "black", size = 0.7) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  facet_grid(~reopening_multiplier_4, scales = "free") +
  customTheme +
  scale_color_brewer(palette = "Dark2", drop=F) +
  labs(
    x = "",
    y = "Critical cases per 1000 population",
    color = ""
  ) +
  theme(legend.position = "bottom") +
  scale_y_continuous(expand = c(0, 0), limits=c(0, 1)) +
  scale_x_date(breaks = "30 days", labels = date_format("%b"), expand = c(0, 0)) 




predplot2 <- predplotDat %>%
  ungroup() %>% 
  filter(param %in% c("prevalence")) %>%
  group_by(reopening_multiplier_4,scenario_fct,param) %>%
  filter(mean.val_per1000pop== max(mean.val_per1000pop)) %>%
  ggplot() +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_hline(yintercept = c( 0.5), col="grey") +
  theme_cowplot() +
  # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
  geom_bar(aes(x = scenario_fct, y = mean.val, fill = scenario_fct,
                 group = interaction(param,reopening_multiplier_4, scenario)), position=position_dodge(width=0.5) ,col="azure4",stat="identity", size = 1.3) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  #facet_wrap(~reopening_multiplier_4) + 
  customTheme +
  scale_color_brewer(palette = "Dark2", drop=F) +
  scale_fill_brewer(palette = "Dark2", drop=F) +
  labs(
    x = "",
    y = "Prevalence",
    color = ""
  ) +
  theme(legend.position = "bottom") +
  scale_y_continuous(expand = c(0, 0), limits=c(0,30)) 



if(SAVE){
  ggsave(paste0("HS_scenarios_timeline.pdf"),
         plot = predplot, path = file.path(pdfdir), width = 12, height =5,  device = "pdf"
  )
}



if(SAVE){
  ggsave(paste0("HS_scenarios_prevalence.pdf"),
         plot = predplot2, path = file.path(pdfdir), width = 9.5, height =7,  device = "pdf"
  )
}

#### For RT
load(file.path(simulation_output, "contact_tracing/20200731/RtDatHS.Rdata"))

RtDatHS <- RtDatHS %>%
  f_addRestoreRegion() %>%
  rename(reopening_multiplier_4 = grpvar) %>%
  group_by(Date, restore_region, scenario, reopening_multiplier_4) %>%
  summarize(mean.val = mean(Mean)) %>%
  mutate(capacity = 1)

RtDatHS$scenario_fct <- factor(RtDatHS$scenario,
                               levels = c("counterfactual", "HS40", "HS80", "TDonly", "HS40TD", "HS80TD"),
                               labels = c(
                                 "current trend (comparison)",
                                 "increase detections of mild symptoms to 40%",
                                 "faster testing and isolation",
                                 "increase detections of mild symptoms to 80%",
                                 "increase detections of mild symptoms to 40%\n& faster testing and isolation",
                                 "increase detections of mild symptoms to 80%\n& faster testing and isolation"
                               )
)

rtplotdat <- RtDatHS %>%
  ungroup() %>%
  mutate(date = as.Date(Date)) %>%
  filter(date >= as.Date(startdate) & date <= as.Date(stopdate) &
           reopening_multiplier_4 %in% reopen) %>%
  group_by(date, scenario, scenario_fct, reopening_multiplier_4) %>%
  summarize(mean.val = mean(mean.val))

rtplot <- rtplotdat %>%
  ggplot() +
  theme_cowplot() +
  # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
  geom_line(aes(x = date, y = mean.val, col = scenario_fct, group = scenario), size = 1.3) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", col = "black", size = 0.7) +
  facet_grid(~reopening_multiplier_4, scales = "free") +
  customTheme +
  scale_color_brewer(palette = "Dark2", drop=F) +
  labs(
    x = "",
    y = expr(italic(R[t])),
    color = ""
  ) +
  theme(legend.position = "bottom") +
  scale_y_continuous(expand = c(0, 0), lim=c(0.8, 1.3)) +
  scale_x_date(breaks = "30 days", labels = date_format("%b"), expand = c(0, 0))+
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) 



predplot <- predplot + theme(legend.position = "none")
rtplot <- rtplot + theme(strip.text.x = element_text(color = "white"))
pplot <- plot_grid(predplot, rtplot, ncol = 1, rel_heights = c(1, 1.3), align="v")


ggsave(paste0("HS_scenarios_IL", ".pdf"),
       plot = pplot, path = file.path(pdfdir), width = 12, height = 8, device = "pdf"
)




## ------------------------------------------------
### Relative reduction barchart
## ------------------------------------------------

### Relative reduction
### Relative reduction barchart
predplotDat <- data.table(predplotDat, key = c("date", "reopening_multiplier_4", "param"))
predplotDat[, mean_percRed := (mean.val[scenario == "counterfactual"] - mean.val) / mean.val[scenario == "counterfactual"], by = c("date", "reopening_multiplier_4", "param")]

pbar1 <- predplotDat %>%
  dplyr::group_by(scenario_fct) %>%
  dplyr::summarize(mean_percRed = mean(mean_percRed)) %>%
  ggplot() +
  theme_cowplot() +
  # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
  geom_bar(aes(x = scenario_fct, y = mean_percRed, fill = scenario_fct), show.legend = FALSE, stat = "identity", size = 1.3) +
  customTheme +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    x = "",
    y = "Relative reduction in cases (%)\n compared to current trend",
    color = ""
  ) +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = function(x) x * 100, expand = c(0, 0)) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggsave(paste0("HS_scenarios_barplot_IL", ".pdf"),
       plot = pbar1, path = file.path(pdfdir), width = 7, height = 4, device = "pdf"
)



pbar2 <- rtplotdat %>%
  dplyr::group_by(date, reopening_multiplier_4, scenario_fct) %>%
  dplyr::summarize(mean.val = mean(mean.val)) %>%
  dplyr::filter(date > as.Date(ct_startdate) & mean.val < 1 & reopening_multiplier_4 %in% reopen) %>%
  dplyr::group_by(reopening_multiplier_4, scenario_fct) %>%
  dplyr::summarize(mindate = min(date)) %>%
  dplyr::mutate(
    datediff = as.Date(mindate) - as.Date(ct_startdate),
    mthdiff = as.numeric(datediff / 30)
  ) %>%
  ggplot() +
  geom_bar(aes(
    x = scenario_fct, y = mthdiff, group = interaction(reopening_multiplier_4, scenario_fct), alpha = reopening_multiplier_4,
    fill = as.factor(scenario_fct)
  ), stat = "identity", position = "dodge", col = "azure3", show.legend = FALSE) +
  labs(x = "", y = expr("Time until " * italic(R[t]) * " is < 1 (month)")) +
  customTheme +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


ggsave(paste0("HS_scenarios_Rt_barplot_IL", ".pdf"),
       plot = pbar2, path = file.path(pdfdir), width = 7, height = 4, device = "pdf"
)

## ---------------------------
#### Explore boxplot
additionalPlotsExplored <- FALSE
if (additionalPlotsExplored) {
  load(file.path(simulation_output, "contact_tracing/20200731/RtDatHS.Rdata"))
  
  RtDatHS <- RtDatHS %>%
    f_addRestoreRegion() %>%
    rename(reopening_multiplier_4 = grpvar) %>%
    group_by(Date, region, restore_region, scenario, reopening_multiplier_4) %>%
    summarize(mean.val = mean(Mean)) %>%
    mutate(capacity = 1)
  
  RtDatHS$scenario_fct <- factor(RtDatHS$scenario,
                                 levels = c("counterfactual", "HS40", "HS80", "TDonly", "HS40TD", "HS80TD"),
                                 labels = c(
                                   "current trend (comparison)",
                                   "increase detections of mild symptoms to 40%",
                                   "faster testing and isolation",
                                   "increase detections of mild symptoms to 80%",
                                   "increase detections of mild symptoms to 40%\n& faster testing and isolation",
                                   "increase detections of mild symptoms to 80%\n& faster testing and isolation"
                                 )
  )
  
  rtplotdat <- RtDatHS %>%
    ungroup() %>%
    mutate(date = as.Date(Date)) %>%
    filter(date >= as.Date(startdate) & date <= as.Date(stopdate) &
             reopening_multiplier_4 %in% reopen) %>%
    group_by(date, region, restore_region, scenario, scenario_fct, reopening_multiplier_4) %>%
    summarize(mean.val = mean(mean.val))
  
  startRt <- rtplotdat %>%
    filter(date == as.Date("2020-06-15")) %>%
    dplyr::select(mean.val, region, restore_region, scenario, reopening_multiplier_4) %>%
    rename(startRt = mean.val)
  
  rtplotdat %>%
    left_join(startRt, by = c("scenario", "reopening_multiplier_4")) %>%
    mutate(mean_percRed = (startRt - mean.val) / startRt) %>%
    group_by(scenario_fct) %>%
    summarize(mean_percRed = mean(mean_percRed)) %>%
    ggplot() +
    theme_cowplot() +
    geom_boxplot(aes(x = scenario, y = mean_percRed, fill = scenario_fct), show.legend = FALSE, size = 1.3)
  
  
  pplot <- rtplotdat %>%
    ggplot() +
    theme_cowplot() +
    # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_line(aes(x = date, y = mean.val, col = scenario_fct, group = scenario), size = 1.3) +
    geom_hline(aes(yintercept = 1), linetype = "dashed", col = "black", size = 0.7) +
    facet_grid(reopening_multiplier_4 ~ region, scales = "free") +
    customTheme +
    scale_color_brewer(palette = "Dark2") +
    labs(
      x = "",
      y = expr(italic(R[t])),
      color = ""
    ) +
    theme(legend.position = "bottom") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_date(breaks = "30 days", labels = date_format("%b"), expand = c(0, 0))
  
  ggsave(paste0("HS_scenarios_perRegion", ".png"),
         plot = pplot, path = file.path(simulation_output, "contact_tracing", simdate), width = 31, height = 15, device = "png"
  )
  
  
  
  ##### Per restore region
  pplot <- predDatHS %>%
    mutate(geography_name = tolower(restore_region)) %>%
    left_join(capacity, by = "geography_name") %>%
    filter(param %in% c("critical") &
             date >= as.Date("2020-07-01") & date <= as.Date("2020-12-01")) %>%
    ggplot() +
    theme_cowplot() +
    # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_line(aes(x = date, y = mean.val, col = scenario_fct, group = scenario), size = 1.3) +
    geom_hline(aes(yintercept = critical), linetype = "dashed", col = "black", size = 0.7) +
    geom_hline(aes(yintercept = 0)) +
    facet_grid(restore_region ~ reopening_multiplier_4, scales = "free") +
    scale_y_continuous(labels = function(x) x / 1000, expand = c(0, 0)) +
    scale_color_brewer(palette = "Dark2") +
    customThemeNoFacet +
    labs(
      x = "",
      y = "Cases requiring ICU beds\n per 1000 population",
      color = ""
    ) +
    theme(legend.position = "bottom")
  
  ggsave(paste0("HS_scenarios_critical_perRestoreRegion", ".pdf"),
         plot = pplot, path = file.path(pdfdir), width = 12, height = 8, device = "pdf"
  )
  
  
  
  f_tempPlot <- function(reopen) {
    ppred <- predDatHS %>%
      mutate(geography_name = tolower(restore_region)) %>%
      left_join(capacity, by = "geography_name") %>%
      filter(reopening_multiplier_4 %in% reopen &
               param %in% c("critical") &
               date >= as.Date("2020-07-01") & date <= as.Date("2020-12-01")) %>%
      ggplot() +
      theme_cowplot() +
      # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
      geom_line(aes(x = date, y = mean.val, col = scenario_fct, group = scenario), size = 1.3) +
      geom_hline(aes(yintercept = critical), linetype = "dashed", col = "black", size = 0.7) +
      facet_wrap(~restore_region, scales = "free_y", nrow = 2) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_color_brewer(palette = "Dark2") +
      customThemeNoFacet +
      labs(
        x = "",
        y = "Cases requiring ICU beds",
        color = "",
        caption = paste0("reopening = ", reopen)
      ) +
      theme(legend.position = "bottom")
    
    
    ggsave(paste0("HS_scenarios_reopen", reopen, "_perRestoreRegion", ".png"),
           plot = ppred, path = file.path(simulation_output, "contact_tracing", simdate), width = 8, height = 6, device = "png"
    )
  }
  for (reopen in unique(predDatHS$reopening_multiplier_4)) {
    f_tempPlot(reopen)
  }
  ##### WHen is capacity reached and by how much
  # predDatHS %>% group_by(restore_region, param, scenario, reopening_multiplier_4 , scenario_fct) %>%
  filledAreaPlot <- FALSE
  if (filledAreaPlot) {
    
    ## Calculate reduction to counterfactual
    
    predDatHS <- data.table(predDatHS, key = c("date", "restore_region", "param", "reopening_multiplier_4"))
    predDatHS[, mean.valAverted := mean.val - mean.val["scenario" == "counterfactual"], by = c("date", "restore_region", "param", "reopening_multiplier_4")]
    
    predDatHS <- data.table(predDatHS, key = c("date", "restore_region", "param", "reopening_multiplier_4"))
    predDatHS[, mean.valAverted := mean.val - mean.val[scenario == "counterfactual"], by = c("date", "restore_region", "param", "reopening_multiplier_4")]
    
    
    tapply(predDatHS$mean.valAverted, predDatHS$scenario, summary)
    tapply(predDatHS$mean.valAverted, predDatHS$param, summary)
    
    
    
    predDatHS_wide <- predDatHS %>%
      select(date, mean.valAverted, reopening_multiplier_4, scenario, param, restore_region) %>%
      pivot_wider(names_from = "scenario", values_from = "mean.valAverted")
    
    #### FILLED AREA PLOT< ALSO FOR RT
    predDatHS_wide %>%
      filter(param %in% c("critical") & reopening_multiplier_4 == 0 &
               date >= as.Date("2020-07-01") & date <= as.Date("2020-12-01")) %>%
      ggplot() +
      theme_cowplot() +
      geom_ribbon(aes(x = date, ymin = counterfactual, ymax = HS40), fill = "blue", size = 1, alpha = 0.3) +
      geom_line(aes(x = date, y = counterfactual), col = "blue", size = 1.3) +
      geom_ribbon(aes(x = date, ymin = HS40, ymax = HS40TD), size = 1, alpha = 0.3) +
      geom_line(aes(x = date, y = HS40), size = 1.3) +
      geom_ribbon(aes(x = date, ymin = HS40TD, ymax = HS80), fill = "red", size = 1, alpha = 0.3) +
      geom_line(aes(x = date, y = HS80), size = 1.3, col = "red") +
      geom_ribbon(aes(x = date, ymin = HS80, ymax = HS80TD), fill = "orange", size = 1, alpha = 0.3) +
      geom_line(aes(x = date, y = HS80TD), size = 1.3, col = "orange") +
      # geom_hline(aes(yintercept = 0)) +
      facet_grid(restore_region ~ reopening_multiplier_4, scales = "free") +
      scale_y_continuous(labels = function(x) x / 1000, expand = c(0, 0)) +
      scale_color_brewer(palette = "Dark2") +
      customThemeNoFacet +
      labs(
        x = "",
        y = "Cases requiring ICU beds\n per 1000 population",
        color = ""
      ) +
      theme(legend.position = "bottom")
  }
  
  
  #### BAR PLOTS
  barplots <- FALSE
  if (barplots) {
    scenario_cols <- c("0.15" = "#e7298a", "0.1" = "#807dba", "0.05" = "#fc4e2a", "0" = "#41ae76")
    
    ppredList <- list()
    predDatHS$scenarioLabel <- factor(predDatHS$scenario,
                                      levels = c("counterfactual", "HS40", "HS80", "TDonly", "HS40TD", "HS80TD"),
                                      labels = c(
                                        "counterfactual",
                                        "increased\ntesting\n(40%)", "increased\ntesting\n(80%)", "faster\ntesting",
                                        "increased and faster\ntesting\n(40%)", "increased and faster\ntesting\n(80%)"
                                      )
    )
    
    for (i in unique(predDatHS$reopening_multiplier_4)) {
      fillcolor <- as.character(scenario_cols[names(scenario_cols) == i])
      
      
      ppred <- predDatHS %>%
        dplyr::mutate(geography_name = tolower(restore_region)) %>%
        dplyr::left_join(capacity, by = "geography_name") %>%
        filter(reopening_multiplier_4 == i &
                 param %in% c("hospitalized") &
                 date >= as.Date("2020-07-01") & date <= as.Date("2020-12-31")) %>%
        dplyr::group_by(date, scenarioLabel) %>%
        dplyr::summarize(
          hospitalized = sum(hospitalized),
          mean.val = sum(mean.val)
        ) %>%
        dplyr::group_by(scenarioLabel) %>%
        dplyr::summarize(
          hospitalized = mean(hospitalized),
          mean.val = mean(mean.val)
        ) %>%
        ggplot() +
        theme_cowplot() +
        geom_bar(aes(x = scenarioLabel, y = mean.val), fill = fillcolor, stat = "identity", size = 1.3) +
        # geom_hline(aes(yintercept=hospitalized), linetype="dashed", col="grey", size=1.7) +
        scale_y_continuous(label = comma, expand = c(0, 0))
      customThemeNoFacet +
        labs(x = "") +
        labs(y = "deaths") +
        theme(legend.position = "right")
      
      ppredList[[length(ppredList) + 1]] <- ppred
      # ggsave(paste0("reopen_",i, "HS_scenarios_hospitalized", ".pdf"),
      #        plot = ppred, path = file.path(expDIR), width = 6, height = 3, device = "pdf"
      # )
    }
    
    p1 <- ppredList[[1]]
    p2 <- ppredList[[2]]
    p3 <- ppredList[[3]]
    p4 <- ppredList[[4]]
    
    
    pall <- plot_grid(p4, p3, p2, p1, ncol = 1)
    ggsave(paste0("reopen_HS_scenarios_hospitalized", ".pdf"),
           plot = pall, path = file.path(expDIR), width = 6, height = 10, device = "pdf"
    )
  }
  ### Load Rt's
}
