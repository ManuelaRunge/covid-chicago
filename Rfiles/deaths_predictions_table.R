
library(tidyverse)
library(cowplot)
library(scales)
library(viridis)
library(data.table)

source("load_paths.R")
source("processing_helpers.R")

theme_set(theme_cowplot())

#### Functions

f_load_data <- function(exp_name){
  
keepVars <- c("date","geography_modeled","scenario_name","deaths_median","deaths_lower","deaths_upper","deaths_det_median","deaths_det_lower","deaths_det_upper")
fname=paste0("nu_",simdate,".csv")
dat <- fread(file.path(simulation_output,exp_name ,fname), select=keepVars) %>% as.data.frame() %>%
  filter(date>=date_today & date<=date_end) %>%
  mutate(region = gsub("covidregion_","",geography_modeled))  %>%
  group_by(region,scenario_name) %>%
  dplyr::select(-date, -geography_modeled) %>%
  summarize_all(.funs="sum") %>%
  as.data.frame()

dat$region <- factor(dat$region, levels=c('illinois',c(1:11)), labels=c('illinois',c(1:11))) 

return(dat)
}

f_plot <- function(dat){
  pplot <- ggplot(data=subset(dat, region!="illinois"))+
    geom_bar(aes(x=region, y=deaths_median), stat="identity", fill="lightgrey",col="darkgrey") +
    geom_bar(aes(x=region, y=deaths_det_median), stat="identity", fill="darkgrey",col="darkgrey") +
    geom_errorbar(aes(x=region, y=deaths_median,ymin=deaths_lower , ymax=deaths_upper ), width=0.5) +
    geom_label(aes(x=region, y=deaths_median, label=round(deaths_median,0)), stat="identity",col="black", vjust=-0.5, fill="white") +
    labs(title="Predicted deaths per covidregion between Nov 11 and Dec 31 in Illinois\nif current transmission conditions continue",
         subtitle="\nUsing NU's COVID-19 transmission and health burden model", x="", 
         y="Number of cumulative deaths",
         caption="Showing median and 95% uncertainity interval\nThe lightgrey fillcolor shows COVID-19 deaths not reported, assuming a detection rate of 80-100%\nErrorbars in 8 and 10 exceed 5000 and were removed")+
    background_grid("y")+
    scale_y_continuous(lim=c(0,5000),breaks=seq(0,5000,500),  labels=comma)+
    customTheme
  
  return(pplot)
}


exp_name <- "20201110_IL_ae_baseline"
#exp_name <- "20201112_IL_rollback"

simdate <- strsplit(exp_name,"_")[[1]][1]
date_today = Sys.Date()
date_end = as.Date("2020-12-31")


dat <- f_load_data(exp_name=exp_name)
filename <- paste0("nu_deathspredictions_",gsub("-","",date_today),"_to_",gsub("-","",date_end))
fwrite(dat, file.path(simulation_output, exp_name, paste0(filename, ".csv")))

pplot <- f_plot(dat)

ggsave(paste0(filename,".png"),
       plot = pplot, path = file.path(simulation_output, exp_name), width = 10, height = 7, device = "png"
)





