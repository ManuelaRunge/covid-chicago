
library(data.table)
library(tidyverse)
filedir <- "C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/_overflow_simulations/20201121/20201121_IL_regreopen50perc_7daysdelay_sm4"

dat <- fread(file.path(filedir,"Ki_dat_region_1.csv" ))
dat$date <- as.Date(dat$date)
length(unique(dat$scen_num))
length(unique(dat$capacity_multiplier))
length(unique(dat$date))

length(unique(dat$scen_num)) * length(unique(dat$capacity_multiplier)) * length(unique(dat$date))

datAggr <- dat %>% group_by(date, capacity_multiplier) %>% summarize(Ki=mean(`Ki_t_EMS-8`)) 
ggplot(data=datAggr)+
  geom_line(aes(x=date, y=Ki, col=capacity_multiplier, group=capacity_multiplier))


ggplot(data=dat)+
  geom_line(aes(x=date, y=`Ki_t_EMS-1`, col=as.factor(capacity_multiplier), group=scen_num))+
  facet_wrap(~capacity_multiplier)



###------------------------------------------------------------
###------------------------------------------------------------
dat <- fread(file.path(filedir,"triggerdate_region_1.csv" ))
dat$trigger_date <- as.Date(dat$trigger_date)
length(unique(dat$scen_num))
length(unique(dat$capacity_multiplier))
length(unique(dat$trigger_date))
dat <- dat %>% group_by(capacity_multiplier) %>% 
              mutate(counter = row_number(capacity_multiplier))

samples <- dat %>% select(capacity_multiplier,sample_num) %>% unique() %>% 
         group_by(capacity_multiplier) %>% 
         add_tally() %>%
         group_by(capacity_multiplier, n) %>% 
         summarize(samples=paste0(sort(sample_num), collapse = " ")) 

length(unique(dat$capacity_multiplier))  
samples2 <- dat %>% 
  select(capacity_multiplier,sample_num) %>% unique() %>% 
  group_by(sample_num) %>% 
  add_tally() %>%
  group_by(sample_num, n) %>% 
  summarize(capacity_multiplier=paste0(sort(capacity_multiplier), collapse = " ")) 

table(samples2$n)



datAggr <- dat %>% group_by( capacity_multiplier,sample_num ) %>% summarize(trigger_date=mean(trigger_date, na.rm=TRUE)) 
ggplot(data=datAggr)+
  geom_point(aes(x=capacity_multiplier, y=as.numeric(trigger_date),col=sample_num ))+
  geom_line(aes(x=capacity_multiplier, y=as.numeric(trigger_date),col=sample_num ))

datAggr <- dat %>% group_by( capacity_multiplier) %>% summarize(trigger_date=mean(trigger_date, na.rm=TRUE)) 
ggplot(data=datAggr)+
  geom_point(aes(x=capacity_multiplier, y=as.numeric(trigger_date)))+
  geom_line(aes(x=capacity_multiplier, y=as.numeric(trigger_date)))

ggplot(data=subset(dat))+
  geom_point(aes(x=capacity_multiplier, y=as.numeric(trigger_date), col=as.factor(capacity_multiplier), group=scen_num))


#### clean - remove incomplete scenarios
dat %>% group_by(capacity_multiplier) %>% select(capacity_multiplier, scen_num) %>% unique()

ggplot(data=subset(dat))+
  geom_line(aes(x=capacity_multiplier, y=as.numeric(trigger_date),group=counter))

ggplot(data=subset(dat))+
  geom_line(aes(x=capacity_multiplier, y=as.numeric(trigger_date),group=sample_num))


ggplot(data=subset(dat))+
  geom_boxplot(aes(x=capacity_multiplier, y=as.numeric(trigger_date), group=capacity_multiplier))

