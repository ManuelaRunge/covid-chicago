###===========================================
###  Plot minimum detection of As and P per Rt
###===========================================

SAVE = F

#### Plots edited for publication
library(tidyverse)
library(cowplot)
library(scales)
library(viridis)
library(readr)
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



f_combineRtdat <- function(exp_name ="20200731_IL_reopen_contactTracing"){
EMS_combined_estimated_Rt  <- read_csv(file.path(simdir, exp_name , "estimatedRt/EMS_combined_estimated_Rt.csv"))

baselineRt <- EMS_combined_estimated_Rt %>% 
              filter(Date ==as.Date("2020-03-01")) %>% 
              dplyr::group_by(region, grpvar) %>% 
              dplyr::summarize(baselineRt=mean(Mean))

currentRt <- EMS_combined_estimated_Rt %>% 
              filter(Date ==as.Date("2020-07-20")) %>% 
              dplyr::group_by(region, grpvar) %>% 
              dplyr::summarize(currentRt=mean(Mean))
  
  
Rtbelow1 <- EMS_combined_estimated_Rt %>% 
  filter(Date == as.Date("2020-09-01")) %>% 
  filter(Mean < 1.0) %>%
  dplyr::group_by(region, grpvar) %>% 
  dplyr::summarize(Rtbelow1=mean(Mean),
                   detection_success =min(detection_success)) %>%
  left_join(baselineRt, by=c("region","grpvar")) %>%
  left_join(currentRt, by=c("region","grpvar")) %>%
  mutate(exp_name = exp_name)


return(Rtbelow1)
}


simdir = file.path("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/contact_tracing/20200731/")
pdfdir = file.path("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/project_notes/publications/covid_model_IL/pdfs_ais/")

exp_names <- list.dirs(simdir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("reopen_contactTracing",exp_names)]
exp_names <- exp_names[exp_names != "20200731_IL_reopen_contactTracing_TD"]



RtDatList <- list()
for(exp_name in exp_names){
  RtDatList[[length(RtDatList)+1]] <- f_combineRtdat(exp_name)  
}

Rtbelow1 <- RtDatList %>% bind_rows()

Rtbelow1$scenario <- gsub("20200731_IL_reopen_contactTracing","", Rtbelow1$exp_name)
Rtbelow1$scenario[Rtbelow1$scenario ==""] <- "counterfactual"


customTheme <- f_getCustomTheme()

Rtbelow1$scenario_fct <- factor(Rtbelow1$scenario,
                                 levels = c("counterfactual", "HS40", "HS80", "TDonly", "HS40TD", "HS80TD"),
                                 labels = c(
                                   "current trend (comparison)",
                                   "increase detections to 40%",
                                   "faster testing and isolation",
                                   "increase detections to 80%",
                                   "increase detections to 40%\n& faster testing and isolation",
                                   "increase detections to 80%\n& faster testing and isolation"
                                 ))
                                 

pplot <- ggplot(data=subset(Rtbelow1))+ 
  geom_point(aes(x=currentRt , y= detection_success, fill=scenario_fct),col="azure4",shape=21, size=3) +
  geom_smooth(aes(x=currentRt , y= detection_success,col=scenario_fct,fill=scenario_fct), method="lm", alpha=0.3) +
  scale_y_continuous(lim=c(0,1), labels = function(x) x * 100 , expand=c(0,0)) +
  labs(x= expr(italic(R[t]) * " before contact tracing start"),
       y="Minimum required detection of \n a - and pre-symptomatic infections (%) ",
       color="Testing improvements for mild symptoms",
       fill="Testing improvements for mild symptoms") +
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2") +
  customTheme +
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(ncol=2))+
  guides(color=guide_legend(ncol=2))


if(SAVE){
  ggsave(paste0("RTLE1_contactTracing_scatterPlot.pdf"),
         plot = pplot, path = file.path(pdfdir), width = 9.5, height =7,  device = "pdf"
  )
}
pplotRt <- pplot
rm(pplot)


###===========================================
###  Plot minimum detection of As and P per Rt FOR CRITICAL
###===========================================


  
  dat_ct <- read.csv(file.path(ct_dir, "20200731/20200731_IL_reopen_contactTracing/CT_ICU_thresholds.csv")) %>% mutate(scenario="counterfactual" , region = as.character(region))
  dat_ctTDonly <- read.csv(file.path(ct_dir, "20200731/20200731_IL_reopen_contactTracing_TD/CT_ICU_thresholds.csv")) %>% mutate(scenario="TDonly", region = as.character(region))
  dat_ctHS40 <- read.csv(file.path(ct_dir, "20200731/20200731_IL_reopen_contactTracingHS40/CT_ICU_thresholds.csv")) %>% mutate(scenario="HS40", region = as.character(region))
  dat_ctHS80 <- read.csv(file.path(ct_dir, "20200731/20200731_IL_reopen_contactTracingHS80/CT_ICU_thresholds.csv")) %>% mutate(scenario="HS80", region = as.character(region))
  dat_ctHS40TD <- read.csv(file.path(ct_dir, "20200731/20200731_IL_reopen_contactTracingHS40TD/CT_ICU_thresholds.csv")) %>% mutate(scenario="HS40TD", region = as.character(region))
  dat_ctHS80TD <- read.csv(file.path(ct_dir, "20200731/20200731_IL_reopen_contactTracingHS80TD/CT_ICU_thresholds.csv")) %>% mutate(scenario="HS80TD", region = as.character(region))
  
  dat <- rbind(dat_ct, dat_ctHS40, dat_ctHS80,dat_ctTDonly,  dat_ctHS40TD, dat_ctHS80TD)
  popdat <- load_population() %>% rename(region =geography_name ) 
  #popdatCentral <- popdat
  
  dat <- dat %>%
    dplyr::left_join(popdat, by = "region") %>%
    dplyr::group_by(region, grpvar, scenario) %>%
    dplyr::mutate(fitmax = max(isolation_success, na.rm = TRUE))
  
  
  dat$region <- as.character(dat$region)
  #### Ideally add Rt from for each of the experiments!!!
  EMS_combined_estimated_Rt  <- read.csv(file.path(ct_dir, simdate, "20200731_IL_reopen_contactTracing" , "estimatedRt/EMS_combined_estimated_Rt.csv"))
  
  baselineRt <- EMS_combined_estimated_Rt %>% 
    filter(Date ==as.Date("2020-03-01")) %>% 
    dplyr::group_by(region, grpvar) %>% 
    dplyr::summarize(baselineRt=mean(Mean)) %>%
    mutate(region = as.character(region))
  
  currentRt <- EMS_combined_estimated_Rt %>% 
    filter(Date ==as.Date("2020-07-20")) %>% 
    dplyr::group_by(region, grpvar) %>% 
    dplyr::summarize(currentRt=mean(Mean))%>%
    mutate(region = as.character(region)) %>%
    left_join(baselineRt,  by=c("region","grpvar"))
    
  
  
  ### combine data
  datWithRt <- dat %>% left_join(currentRt, by=c("region","grpvar")) 
  
  customTheme <- f_getCustomTheme()
  
  datWithRt$scenario_fct <- factor(datWithRt$scenario,
                                  levels = c("counterfactual", "HS40", "TDonly","HS80", "HS40TD", "HS80TD"),
                                  labels = c(
                                    "current trend (comparison)",
                                    "increase detections to 40%",
                                    "faster testing and isolation",
                                    "increase detections to 80%",
                                    "increase detections to 40%\n& faster testing and isolation",
                                    "increase detections to 80%\n& faster testing and isolation"
                                  ))
  
  
  pplot <- datWithRt %>% 
    filter(isolation_success==fitmax &  region %in% as.character(c(1:11)) ) %>% 
    group_by(region, currentRt, scenario, scenario_fct) %>% 
    summarize(detection_success=min(detection_success)) %>% 
    ggplot()+ 
    geom_point(aes(x=currentRt , y= detection_success, fill=scenario_fct),col="azure4",shape=21, size=3) +
    geom_smooth(aes(x=currentRt , y= detection_success,col=scenario_fct,fill=scenario_fct), method="lm", alpha=0.3) +
    scale_y_continuous(lim=c(0,1), labels = function(x) x * 100 , expand=c(0,0)) +
    labs(x= expr(italic(R[t]) * " before contact tracing start"),
         y="Minimum required detection of \n a - and pre-symptomatic infections (%) ",
         color="Testing improvements for mild symptoms",
         fill="Testing improvements for mild symptoms") +
    scale_color_brewer(palette = "Dark2", drop=F)+
    scale_fill_brewer(palette = "Dark2", drop=F) +
    customTheme +
    theme(legend.position = "bottom")+
    guides(fill=guide_legend(ncol=2))+
    guides(color=guide_legend(ncol=2))
  
  
if(SAVE){
  ggsave(paste0("ICUcap_contactTracing_scatterPlot.pdf"),
         plot = pplot, path = file.path(pdfdir), width = 9.5, height =7,  device = "pdf"
  )
}
  
  pplotICU <- pplot
  rm(pplot)


  
  
##### combined plot
  
  subdat1 <- Rtbelow1 %>% 
              filter( !is.na(scenario_fct) & region %in% c(1:11) )  %>% 
    select(grpvar, detection_success, currentRt,  scenario, scenario_fct) %>% 
    mutate(type="Rt", region=as.character(region)) 
  
  table(subdat1$region, subdat1$scenario_fct)
  
  dat <- datWithRt  %>%
    filter(isolation_success==fitmax & !is.na(scenario_fct) & region %in% c(1:11) )  %>% 
    select(grpvar, detection_success, currentRt,  scenario, scenario_fct)  %>% 
    mutate(type="ICU", region=as.character(region))   %>% 
    rbind(subdat1) %>% 
    group_by(region, currentRt, scenario, scenario_fct, type) %>% 
    summarize(detection_success=min(detection_success)) 
  
  pplot <- dat%>% 
    ggplot()+ 
    geom_point(aes(x=currentRt , y= detection_success, fill=scenario_fct),col="azure4",shape=21, size=3) +
    geom_smooth(aes(x=currentRt , y= detection_success,col=scenario_fct,fill=scenario_fct), method="lm", alpha=0.3) +
    scale_y_continuous(lim=c(0,1), labels = function(x) x * 100 , expand=c(0,0)) +
    labs(x= expr(italic(R[t]) * " before contact tracing start"),
         y="Minimum required detection of \n a - and pre-symptomatic infections (%) ",
         color="Testing improvements for mild symptoms",
         fill="Testing improvements for mild symptoms",
         caption="Each point shows a region (n=11) for different reopening scenarios (n=4)") +
    scale_color_brewer(palette = "Dark2", drop=F)+
    scale_fill_brewer(palette = "Dark2", drop=F )+
    customTheme +
    background_grid() + 
    theme(legend.position = "bottom")+
    guides(fill=guide_legend(ncol=2))+
    guides(color=guide_legend(ncol=2)) +
    facet_wrap(~type, scales="free_y")
  
  
  if(SAVE){
    ggsave(paste0("contactTracing_scatterPlot.pdf"),
           plot = pplot, path = file.path(pdfdir), width = 14, height =7,  device = "pdf"
    )
  }
  