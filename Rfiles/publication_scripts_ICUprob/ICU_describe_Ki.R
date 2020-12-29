### Describe Rt


library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

theme_set(theme_minimal())
simcolor <- "#F6921E"
capacitycolor <- "dodgerblue2"
customTheme <- f_getCustomTheme()


simdate <- "20201212"
sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (!dir.exists(file.path(sim_dir, "ICU_Rt_plots"))) dir.create(file.path(sim_dir, "ICU_Rt_plots"))


exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c(grep("daysdelay", exp_names), grep("counterfactual", exp_names))]
exp_names <- exp_names[!(grepl("_reopen", exp_names))]

dat <- f_combineDat(sim_dir, exp_names, "Ki_dat_All.csv") %>%
        f_get_scenVars()
dat$date <- as.Date(dat$date)
summary(dat$time)

dat <- dat %>% filter(region %in% c(1,4,11))
dat$region <- factor(dat$region, levels = c(1,4,11), labels = paste0('Region ', c(1,4,11)))
table(dat$capacity_multiplier_fct, exclude = NULL)


###------------------------------
## Counterfactual
###------------------------------
dat_counterfactual <- dat %>% filter(rollback=="counterfactual")

ggplot(data=subset(dat_counterfactual, date <= as.Date("2020-12-31"))) +
  geom_line(aes(x=date, y= Ki_t, group=interaction(sample_num, reopen),col=reopen))+
  facet_wrap(~region, ncol=1) +
  geom_hline(yintercept = 1)+
  geom_vline(xintercept = as.Date("2020-09-01"))+
  scale_x_date(date_breaks = "30 days", date_labels = "%b")+
  customTheme


###------------------------------
## Scenarios
###------------------------------
dat_scenarios <- dat %>% filter(rollback!="counterfactual" & delay=="1daysdelay")
ggplot(data=subset(dat_scenarios,  rollback =="pr6" &  
                     date >= as.Date("2020-07-01") &  
                     date <= as.Date("2020-12-31"))) +
  geom_line(aes(x=date, y= Ki_t, group=interaction(sample_num, reopen),col=reopen))+
  facet_grid(reopen~region, scales="free") +
  scale_x_date(date_breaks = "30 days", date_labels = "%b")+
  customTheme 

ggplot(data=subset(dat_scenarios,  
                     date >= as.Date("2020-07-01") &  
                     date <= as.Date("2020-12-31"))) +
  geom_line(aes(x=date, y= Ki_t, group=interaction(sample_num, rollback, reopen),col=rollback))+
  facet_grid(reopen~region, scales="free") +
  scale_x_date(date_breaks = "30 days", date_labels = "%b")+
  customTheme 


dat_scenarios %>% 
  group_by(region, exp_name) %>% 
  arrange(date, Ki_t) %>%
  tail()

uniqueKis <- dat_scenarios %>%  
        select(region, exp_name, reopen, rollback, delay, Ki_t) %>% 
        unique() %>% 
        group_by(region,reopen, rollback, delay) %>% 
        summarize(min_Ki=min(Ki_t), max_Ki = max(Ki_t)) %>%
        mutate(Ki_red =(1-(min_Ki/max_Ki))*100 )

  