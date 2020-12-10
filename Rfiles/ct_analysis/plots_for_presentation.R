
## ==================================================
# R script that analysis trajectoriesDat
## ==================================================

require(tidyverse)
require(cowplot)
require(scales)
require(readxl)
require(viridis)
require(stringr)
require(broom)

library(gapminder)
library(gganimate)
library(gifski)
library(transformr)

source("load_paths.R")
source("processing_helpers.R")


funplot=FALSE
if(funplot){
  ## standard ggplot2
  myPlot <-ggplot(data=dat1) +
    theme_cowplot() +  customThemeNoFacet +
    geom_line(aes(x=Date, y=infected, col=as.factor(d_Sym_ct1)),size=2) +
    geom_hline(yintercept = c(-Inf, Inf)) +   
    geom_vline(xintercept = c(-Inf, Inf)) +
    # Here comes the gganimate specific bits
    #labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
    transition_states(d_AsP_ct1) +
    #transition_reveal()
    #transition_states(Month, wrap = FALSE)
    ease_aes('linear') 
  # shadow_mark()
  
  animate(myPlot, duration = 6,  renderer = gifski_renderer())
  anim_save("output3.gif")
  
}


ct_dir <- file.path(simulation_output, "contact_tracing")


dat0 <- read.csv("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/MR_tests/20200619_TEST_generic_test/trajectoriesDat.csv")


dat1 <- read.csv("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/MR_tests/20200619_EMS_11_state_eventTest2/trajectoriesDat.csv")
dat2 <- read.csv("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/MR_tests/20200619_EMS_11_state_eventTest_counterfactual2/trajectoriesDat.csv")


dat <- dat0 #dat1

dat <- dat %>% mutate(Date = time + as.Date(startdate)) %>% filter(Ki!=0)

ggplot(data=subset(dat, time <100)) +
  theme_cowplot() +  customThemeNoFacet +
  geom_line(aes(x=Date, y=susceptible),size=2) +
  geom_hline(yintercept = c(-Inf, Inf)) +   
  geom_vline(xintercept = c(-Inf, Inf)) +
  facet_wrap(~ Ki, scales="free")



