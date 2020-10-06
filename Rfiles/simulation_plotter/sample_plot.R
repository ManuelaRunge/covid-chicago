
library(tidyverse)
library(cowplot)
library(readr)
library(RColorBrewer)
library(scales)
dat <- read_csv("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/sample_trajectories/extended_SEIR_I/trajectoriesDat_v6.csv")


dat %>% 
  select(time, scen_num, susceptible, exposed, infected, removed)  %>%
  pivot_longer(cols=-c('time', 'scen_num'), names_to ="outcome") %>%
  group_by(time, outcome) %>% 
  summarize(median.val = median(value, na.rm=TRUE)) %>%
  ggplot() + 
  theme_cowplot() +
  geom_line(aes(x=time, y=median.val, col=outcome), size=2) +
  scale_color_brewer(palette = "Dark2")+
  labs(y="population", x="days after infection import", color="")+
  scale_y_continuous(labels=comma, expand=c(0,0))+
  geom_vline(xintercept = c(-Inf , Inf))+
  geom_hline(yintercept = c(-Inf , Inf))
  


dat %>% 
  mutate(hospitalized= hospitalized + critical) %>%
  select(time, scen_num, susceptible, exposed,   asymptomatic,symptomatic, hospitalized,  removed)  %>%
  pivot_longer(cols=-c('time', 'scen_num'), names_to ="outcome") %>%
  group_by(time, outcome) %>% 
  summarize(median.val = median(value, na.rm=TRUE)) %>%
  ggplot() + 
  theme_cowplot() +
  geom_line(aes(x=time, y=median.val, col=outcome), size=2) +
  scale_color_brewer(palette = "Dark2")+
  labs(y="population", x="days after infection import", color="")+
  scale_y_continuous(labels=comma, expand=c(0,0))+
  geom_vline(xintercept = c(-Inf , Inf))+
  geom_hline(yintercept = c(-Inf , Inf))


