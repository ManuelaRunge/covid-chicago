
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



date_today = as.Date("2020-11-12")
date_end = as.Date("2020-12-31")


exp_name <- "20201112_IL_rollback"
simdate <- strsplit(exp_name,"_")[[1]][1]
dat1 <- f_load_data(exp_name=exp_name) %>% mutate(scenario_name="mitigation") %>% select(region, scenario_name, deaths_det_median)
exp_name <- "20201112_IL_600_baseline"
simdate <- strsplit(exp_name,"_")[[1]][1]
dat2 <- f_load_data(exp_name=exp_name) %>% mutate(scenario_name="baseline") %>% select(region, scenario_name, deaths_det_median)
dat <- rbind(dat1, dat2)
dat$region <- factor(dat$region, levels=c('illinois',c(1:11)), labels=c('illinois',c(1:11))) 
dat=subset(dat, region!="illinois")
dat_wide <- dat %>% pivot_wider(names_from=scenario_name, values_from =deaths_det_median) %>%  mutate(diff = mitigation-baseline )
rm(dat)  
  

exp_name <- "20201112_IL_rollback"
simdate <- strsplit(exp_name,"_")[[1]][1]
dat1 <- f_load_data(exp_name=exp_name) %>% mutate(scenario_name="lockdown starting Nov 13")

exp_name <- "20201112_IL_600_baseline"
simdate <- strsplit(exp_name,"_")[[1]][1]
dat2 <- f_load_data(exp_name=exp_name) %>% mutate(scenario_name="current trend continued")


dat <- rbind(dat1, dat2)
dat$region <- factor(dat$region, levels=c('illinois',c(1:11)), labels=c('illinois',c(1:11))) 
dat=subset(dat, region!="illinois")

  
ggplot(data=subset(dat, region!="illinois"))+
  geom_bar(aes(x=region, y=deaths_det_median,fill=scenario_name), stat="identity", col="darkgrey", position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(x=region, y=deaths_det_median,ymin=deaths_det_lower , ymax=deaths_det_upper , group=scenario_name), width=0.5, position = position_dodge(width = 0.7)) +
  scale_y_continuous(labels=comma)+
  scale_fill_manual(values=c("#d2d2d2","#ec008b"))


pplot <- ggplot(data=dat)+
  geom_bar(data=subset(dat, scenario_name=="current trend continued"), aes(x=region, y=deaths_det_median, fill=scenario_name),width=0.8,  stat="identity", col="darkgrey") +
  geom_bar(data=subset(dat, scenario_name=="lockdown starting Nov 13"), aes(x=region, y=deaths_det_median, fill=scenario_name),width=0.8, stat="identity", col="darkgrey") +
  #geom_errorbar(aes(x=region, y=deaths_det_median,ymin=deaths_lower , ymax=deaths_upper ), width=0.5) +
  geom_text(data=dat_wide, aes(x=region, y=baseline/2, label=round(diff,0)), stat="identity",col="deepskyblue4", vjust=-0.9, fill=NA) +
  labs(title="Cumulative deaths averted between Nov 12th and Dec 30th\n", x="Covidregion", 
       y="Number of cumulative deaths\n",
       caption="",fill="Scenario")+
  background_grid("y")+
  scale_y_continuous(lim=c(0,4000),breaks=seq(0,4000,500),  labels=comma)+
  customTheme+
  scale_fill_manual(values=c("#d2d2d2","deepskyblue4"))+
  theme(legend.position = "bottom")

dat %>% group_by(scenario_name) %>% summarize(deaths_det_median=sum(deaths_det_median))
dat %>% group_by(scenario_name) %>% summarize(deaths_det_median=sum(deaths_det_lower))
dat %>% group_by(scenario_name) %>% summarize(deaths_det_median=sum(deaths_det_upper))


ggsave(paste0("cumulative_deaths_nov_dec_comparison.png"),
       plot = pplot, path = file.path(simulation_output, exp_name1,'_plots'), width =8, height = 7, device = "png"
)

ggsave(paste0("cumulative_deaths_nov_dec_comparison.pdf"),
       plot = pplot, path = file.path(simulation_output, exp_name1,'_plots'), width = 8, height = 7, device = "pdf"
)






#### Mitigation strengths

f_initial_and_timevarying_Ki <- function(exp_dir, param = NULL) {
  library(tidyverse)
  library(data.table)
  
  keepVars <- paste0("Ki_EMS_", c(1:11))
  initialKi <- fread(file.path(exp_dir, "sampled_parameters.csv"), select = keepVars) %>%
    unique() %>%
    melt() %>%
    separate(variable, into = c("del", "region"), sep = "Ki_EMS_") %>%
    rename(Ki_initial = value) %>%
    dplyr::select(-del)
  
  if (is.null(param)) param <- c("reopening_multiplier_4", "capacity_multiplier", "trigger_delay_days")
  keepVars <- c("time", "startdate", "scen_num", "sample_num", param, paste0("Ki_t_EMS-", c(1:11)))
  timevaryingKi <- fread(file.path(exp_dir, "trajectoriesDat.csv"), select = keepVars) %>%
    filter(as.numeric(time) < 365) %>%
    unique() %>%
    pivot_longer(cols = -c("time", "startdate", "scen_num", "sample_num", param)) %>%
    separate(name, into = c("del", "region"), sep = "Ki_t_EMS-") %>%
    rename(Ki_t = value) %>%
    mutate(date = as.Date(startdate) + time) %>%
    dplyr::select(-del, -time, -startdate)
  
  
  grpVars <- c("date", param, "region")
  Ki_dat <- timevaryingKi %>%
    left_join(initialKi, by = "region") %>%
    mutate(Ki_rebound = (Ki_t / Ki_initial)) %>%
    ungroup() %>%
    dplyr::group_by_at(.vars = grpVars) %>%
    summarize(
      Ki_t = median(Ki_t),
      Ki_initial = median(Ki_initial),
      Ki_rebound = median(Ki_rebound)
    )
  
  Ki_dat$region <- factor(Ki_dat$region, levels = c(1:11), labels = c(1:11))
  return(Ki_dat)
}


Ki_dat <- f_initial_and_timevarying_Ki(exp_dir=file.path(simulation_output, exp_name2))
Ki_dat <- Ki_dat %>% 
  ungroup() %>% 
  mutate(region=as.character(region),
         Ki_rebound=round(Ki_rebound*100,0))  %>% select(date, region, Ki_rebound)


Ki_dat_current <- Ki_dat %>% filter(date>=as.Date("2020-11-10") & date <as.Date("2020-11-11")) %>% mutate(scenario_name="current trend continued") 
Ki_dat_mitigation <- Ki_dat %>%  filter(date>=as.Date("2020-11-14") & date <as.Date("2020-11-15")) %>% mutate(scenario_name="lockdown starting Nov 13")


Kidat <- rbind(Ki_dat_mitigation,Ki_dat_current)
Kidat$region <- factor(Kidat$region, levels=c('illinois',c(1:11)), labels=c('illinois',c(1:11))) 
Kidat_wide <- Kidat %>% select(-date) %>%pivot_wider(names_from = 'scenario_name', values_from="Ki_rebound") %>%
                mutate(reduction =(1-( `lockdown starting Nov 13` / `current trend continued`))*100)

pplot <- ggplot(data=Kidat)+
  geom_bar(data=subset(Kidat, scenario_name=="current trend continued"), aes(x=region, y=Ki_rebound, fill=scenario_name),width=0.8,  stat="identity", col="darkgrey") +
  geom_bar(data=subset(Kidat, scenario_name=="lockdown starting Nov 13"), aes(x=region, y=Ki_rebound, fill=scenario_name),width=0.8, stat="identity", col="darkgrey") +
  #geom_errorbar(aes(x=region, y=deaths_det_median,ymin=deaths_lower , ymax=deaths_upper ), width=0.5) +
  geom_text(data=Kidat_wide, aes(x=region, y=`current trend continued`/2, label=round(reduction,1)), stat="identity",col="#ec008b", vjust=-3, fill=NA) +
  labs(title="Assumed change in transmission rate\n", x="Covidregion", 
       y="% returned to normal\n",
       caption="(100% = pre-mitigation transmission early March)",fill="Scenario")+
  background_grid("y")+
  scale_y_continuous(lim=c(0,50),breaks=seq(0,50,5),  labels=seq(0,50,5))+
  customTheme+
  scale_fill_manual(values=c("#d2d2d2","#ec008b"))+
  theme(legend.position = "bottom")

ggsave(paste0("cumulative_deaths_nov_dec_comparison.png"),
       plot = pplot, path = file.path(simulation_output, exp_name2,'_plots'), width =8, height = 7, device = "png"
)

ggsave(paste0("changeInTransmission_comparison.pdf"),
       plot = pplot, path = file.path(simulation_output, exp_name2,'_plots'), width = 8, height = 7, device = "pdf"
)

