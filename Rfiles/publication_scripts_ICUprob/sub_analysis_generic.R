

library(tidyverse)
library(cowplot)
library(data.table)
library(raster)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

theme_set(theme_cowplot())

#outdir <- file.path("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/project_notes/publications/covid_model_IL_overflow/out")
outdir <- file.path(simulation_output, "_overflow_simulations")


outcomeVars <-c("infected","crit_det","hosp_det")
outVars <- c("date", "scen_num", "sample_num", "capacity_multiplier","rollback","Ki", outcomeVars)


exp_name <- "20201015_Chicago_mr_testrun_counterfactual"
dat1 <- fread(file.path(outdir,exp_name, "trajectoriesDat.csv" )) %>% 
          mutate(scenario="counterfactual", Ki=round(Ki,4)) %>% dplyr::mutate(date = as.Date(startdate) + time)

exp_name <- "20201014_Chicago_mr_testrun_trigger"
exp_dir <- file.path(outdir,exp_name)
dat2 <- fread(file.path(outdir,exp_name, "trajectoriesDat.csv" )) %>% 
  mutate(scenario="trigger", Ki=round(Ki,4))  %>% dplyr::mutate(date = as.Date(startdate) + time) %>% dplr::select(colnames(dat))


dat <- rbind(dat1, dat2, fill=TRUE) %>% filter(Ki > 0.1) %>% 
      group_by(date, scenario, rollback, capacity_multiplier, Ki) %>% summarize(crit_det=median(crit_det))


p1 <- ggplot(data=subset(dat, Ki > 0.2 & scenario=="trigger" & date <=as.Date("2020-07-01")))+
  geom_line(aes(x=date, y=crit_det, col=as.factor(capacity_multiplier), linetype=as.factor(rollback)), size=1.3)+
  facet_wrap(rollback~Ki, scales="free", ncol=2, labeller = label_both) +
  geom_hline(yintercept = 420)

p2 <-  ggplot(data=subset(dat, Ki > 0.2 & scenario=="trigger" & date <=as.Date("2020-07-01")))+
  geom_line(aes(x=date, y=crit_det, col=as.factor(rollback), linetype=as.factor(rollback)), size=1.3)+
  facet_wrap(capacity_multiplier~Ki, scales="free", ncol=2, labeller = label_both) +
  geom_hline(yintercept = 420)


ggsave(paste0("pplot1.png"),
       plot = p1, path = file.path(exp_dir), width = 8, height = 8, device = "png"
)

ggsave(paste0("pplot2.png"),
       plot = p2, path = file.path(exp_dir), width = 8, height = 8, device = "png"
)
