
library(readr)
sampled_parameters <- read_csv("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/_forFitting/20201108_IL_mr_refit_kistartsm3/sampled_parameters.csv")
sampled_parameters$time_infection_import <- round(sampled_parameters$time_infection_import,0)
fitingPara <- unique(sampled_parameters[,c('Ki','time_infection_import','ki_multiplier_3c')])
dim(fitingPara)
fitingPara$scen_num <- rownames(fitingPara)
fitingPara <- subset(fitingPara, Ki >0.000)
unique(fitingPara$Ki)
write_csv(fitingPara, file.path("C:/Users/mrm9534/gitrepos/covid-chicago/experiment_configs/input_csv/fittingparam.csv"))

