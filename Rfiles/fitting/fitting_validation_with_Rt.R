

library(tidyverse)
library(data.table)


## Load directories and custom objects and functions
Location = "LOCAL"
if(Location == "NUCLUSTER") setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
source("load_paths.R")
source("processing_helpers.R")

exp_name <-  '20201003_IL_mr_fitkistartsm3' # exp_names[1]

simulation_output <- file.path(simulation_output, "_forFitting")
exp_names <- list.dirs(simulation_output, recursive = FALSE, full.names = FALSE)
exp_dir <- file.path(simulation_output, exp_name)  
Rt_dir <- file.path(simulation_output, exp_name,  "estimatedRt")  
dataRtDir <- file.path(project_path, "Plots + Graphs/Rt_plots")

### Load dataframs
## Rt estimated from data
dataRt <- fread(file.path(dataRtDir, "nu_il_fromdata_estimated_Rt.csv")) %>%
                group_by(covid_region) %>% 
                #mutate(minDate = as.Date(min(date))+15) %>% 
                #filter(date  <=minDate) %>%
                filter(date  <=as.Date("2020-04-01")) %>%
                select(-date) %>%
                summarize_all(.funs="mean") %>%
                rename(region=covid_region,
                       dat_rt_median =rt_median ,
                       dat_rt_lower =rt_lower ,
                       dat_rt_upper=rt_upper)

mean(dataRt$dat_rt_median )

#fread(file.path(dataRtDir, "nu_il_fromdata_estimated_Rt.csv")) %>%
#  group_by(covid_region) %>% 
#  filter(date ==min(date))

### Sampled parameters
sample_dat <- fread(file.path(exp_dir, "sampled_parameters.csv"), 
                    select = c("scen_num","sample_num","startdate","Ki", "time_infection_import","social_multiplier_3")) 

## Rt from simulations
load(file.path(Rt_dir, "combined_estimated_Rt.Rdata"))
Rt_dat <- Rt_dat %>%
  left_join(sample_dat, by="scen_num") %>%
  mutate(date = t_end + as.Date(startdate),
         region =as.numeric(region)) %>% 
  # filter(date  == as.Date("2020-03-01")) %>%
  filter(date <= as.Date("2020-04-01")) %>%
  select( -c(date, t_start, t_end, weekwindow)) %>%
  group_by(region, scen_num, sample_num,startdate) %>% 
  summarize_at(.vars=c('median', 'quantile.0.025', 'quantile.0.05','quantile.0.25','quantile.0.75', 'quantile.0.95', 'quantile.0.975'), .funs="mean") 
 
### Fitting results
fitting_dat <- f_combine_csv(csv_dir=file.path(exp_dir, "fitting/csv/") ,fname="best_parameter_ranges_All.csv") %>% mutate(region =as.numeric(region))
#tapply(as.numeric(fitting_dat$scen_num), fitting_dat$region, summary)

subdat <- Rt_dat %>% 
            inner_join(fitting_dat, by=c("scen_num","region")) %>%
            left_join(dataRt, by=c("region"))

subdat$time_infection_import_grp <- cut(subdat$time_infection_import, breaks = quantile(subdat$time_infection_import))
subdat$social_multiplier_3_grp <- cut(subdat$social_multiplier_3, breaks = quantile(subdat$social_multiplier_3))
subdat$Ki_grp <- cut(subdat$Ki, breaks = unique(quantile(subdat$Ki)))
subdat$Ki_adj <- subdat$Ki * subdat$social_multiplier_3 
subdat$Ki_adj_grp <- cut(subdat$Ki_adj, breaks = unique(quantile(subdat$Ki_adj)))
subdat$NLL_grp <- cut(subdat$NLL, breaks = unique(quantile(subdat$NLL)))

subdat <- subdat %>% dplyr::group_by(region) %>%  dplyr::mutate(minNLL = min(NLL))

ggplot(data=subdat)+   
  geom_rect( aes(xmin=-Inf, xmax=Inf,ymax=dat_rt_upper , ymin=dat_rt_lower),fill="grey", alpha=0.01) + 
  geom_hline( aes(yintercept=dat_rt_median),col="grey") + 
  geom_pointrange( aes(x=Ki, y=median, ymin=quantile.0.025, ymax=quantile.0.975 , col=as.numeric(NLL_grp))) + 
  geom_point(data=subset(subdat, NLL==minNLL), aes(x=Ki, y=median),col="orange", shape=17, size=3)+
  scale_y_log10() +
  facet_wrap(~region)+
  geom_hline(yintercept = 1)+
  labs(y="Rt", x="Ki", col="Nll quartiles (dark=lowest)")

ggplot(data=subdat)+   
  geom_rect( aes(xmin=-Inf, xmax=Inf,ymax=dat_rt_upper , ymin=dat_rt_lower),fill="grey", alpha=0.01) + 
  geom_hline( aes(yintercept=dat_rt_median),col="grey") + 
  geom_pointrange( aes(x=Ki_adj, y=median, ymin=quantile.0.025, ymax=quantile.0.975 , col=as.numeric(NLL_grp))) + 
  geom_point(data=subset(subdat, NLL==minNLL), aes(x=Ki, y=median),col="orange", shape=17, size=3)+
  scale_y_log10() +
  facet_wrap(~region)+
  geom_hline(yintercept = 1)+
  labs(y="Rt", x="Ki*multiplier", col="Nll quartiles (dark=lowest)")


ggplot(data=subset(subdat, !is.na(time_infection_import_grp)),aes(x=Ki, y=median, col=social_multiplier_3_grp))+  geom_point( ) + #geom_smooth() +
  facet_wrap(~time_infection_import_grp, scales="free", nrow=1)

p1 <- ggplot(data=subset(subdat, !is.na(Ki_grp)),aes(x=time_infection_import, y=median, col=Ki_grp))+  geom_point( ) 
p2 <- ggplot(data=subset(subdat, !is.na(Ki_grp)),aes(x=time_infection_import, y=median, col=Ki_adj_grp))+  geom_point( ) 
plot_grid(p1,p2)


ggplot(data=subset(subdat),aes(x=dat_rt_median, y=median))+  geom_point( ) 


subdat <- subdat %>% filter(median >= dat_rt_lower & median <= dat_rt_upper )

ggplot(data=subdat)+   
  geom_rect( aes(xmin=-Inf, xmax=Inf,ymax=dat_rt_upper , ymin=dat_rt_lower),fill="grey", alpha=0.01) + 
  geom_hline( aes(yintercept=dat_rt_median),col="grey") + 
  #geom_pointrange( aes(x=Ki_adj, y=median, ymin=quantile.0.025, ymax=quantile.0.975 , col=as.numeric(NLL_grp))) + 
  geom_point( aes(x=Ki_adj, y=median,  col=as.numeric(NLL_grp))) + 
  geom_point(data=subset(subdat, NLL==minNLL), aes(x=Ki, y=median),col="orange", shape=17, size=3)+
  scale_y_log10() +
  facet_wrap(~region)+
  geom_hline(yintercept = 1)+
  labs(y="Rt", x="Ki*multiplier", col="Nll quartiles (dark=lowest)")



ggplot(data=subdat)+   
  geom_rect( aes(xmin=-Inf, xmax=Inf,ymax=dat_rt_upper , ymin=dat_rt_lower),fill="grey", alpha=0.01) + 
  geom_hline( aes(yintercept=dat_rt_median),col="grey") + 
  #geom_pointrange( aes(x=Ki_adj, y=median, ymin=quantile.0.025, ymax=quantile.0.975 , col=as.numeric(NLL_grp))) + 
  geom_point( aes(x=time_infection_import, y=median,  col=as.numeric(NLL_grp))) + 
  geom_point(data=subset(subdat, NLL==minNLL), aes(x=Ki, y=median),col="orange", shape=17, size=3)+
  scale_y_log10() +
  facet_wrap(~region)+
  geom_hline(yintercept = 1)+
  labs(y="Rt", x="time_infection_import", col="Nll quartiles (dark=lowest)")
