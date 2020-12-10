

library(tidyverse)
library(cowplot)
library(scales)
library(viridis)
require(data.table)

source("load_paths.R")
source("processing_helpers.R")

theme_set(theme_cowplot())

simdate <- "20201022"
exp_name <- "20201022_IL_crit_det_baseline_triggeredrollback"  #"20200917_IL_gradual_reopening"
plot_dir <- file.path(simulation_output, exp_name, "_plots")
if(!dir.exists(plot_dir))dir.create(plot_dir)


region_names <- c("All", paste0("EMS-", c(1:11)))
outcomevars <- c(
  paste0("hosp_det_", region_names),
  paste0("hospitalized_", region_names),
  paste0("crit_det_", region_names),
  paste0("critical_", region_names)
)

region_names <- paste0("EMS-", c(1:11))
outcomevars2 <- c(
  paste0("Ki_t_", region_names)
)

paramvars <- c("capacity_multiplier")
keepvars <- c("time", "startdate", "scen_num", "run_num","sample_num", paramvars, outcomevars, outcomevars2)



trajectoriesDat <- fread(file.path(simulation_output, exp_name, "trajectoriesDat.csv"), select = c(keepvars))

outcomeVars <- c("All", paste0("EMS-", c(1:11)))
outcomeVars <- paste0("crit_det_", outcomeVars)
outVars <- c("date", "scen_num", "sample_num", "capacity_multiplier", outcomeVars)

simdat <- trajectoriesDat %>%
  dplyr::mutate(date = as.Date(startdate) + time) %>%
  dplyr::select(outVars) %>%
  pivot_longer(cols = -c(date, scen_num, sample_num, capacity_multiplier)) %>%
  separate(name, into = c("channel", "det", "geography_name"), sep = "_") %>%
  mutate(
    exp_name = exp_name,
    geography_name = gsub("EMS-", "", geography_name),
    geography_name = gsub("All", "illinois", geography_name)
  ) 
simdat$region <- factor(simdat$geography_name,
                        levels = c("illinois", c(1:11)),
                        labels = c("illinois", c(1:11))
)




  
  simdat <- simdat %>% left_join(load_new_capacity(), by = "geography_name")
  simdat <- simdat %>% left_join(load_population(), by = "geography_name")
  
  simdat <- subset(simdat, date>= as.Date("2020-10-20") & date <= as.Date("2020-12-30"))
  
  
  ggplot(data=subset(simdat, channel=="crit"))+
    geom_line(aes(x=date, y=value, group=scen_num, col=capacity_multiplier))+
    facet_wrap(~region, scales="free")+
    geom_hline(aes(yintercept = icu_available))
  
  propDat_sim <- simdat %>%
    dplyr::filter(channel == "crit" & date > as.Date("2020-10-25") & date <= as.Date("2020-12-31")) %>%
    dplyr::group_by(scen_num, sample_num, geography_name, exp_name, pop, capacity_multiplier, icu_available) %>%
    dplyr::summarize(value = max(value)) %>%
    dplyr::mutate(aboveCapacity = ifelse(value >= icu_available, 1, 0)) %>%
    dplyr::group_by(geography_name, exp_name,pop, capacity_multiplier, icu_available) %>%
    add_tally(name = "nsamples") %>%
    dplyr::group_by(geography_name, exp_name, pop, capacity_multiplier, icu_available, nsamples) %>%
    dplyr::summarize(nabove = sum(aboveCapacity)) %>%
    dplyr::mutate(prob_overflow = nabove / nsamples)
  
  propDat_sim$geography_name <- factor(propDat_sim$geography_name, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))
  
  save(propDat_sim, file = file.path(simulation_output, exp_name, "propDat_sim.Rdata"))
  
  pplot <- ggplot(data = propDat_sim) +
    geom_line(aes(x = capacity_multiplier, y = prob_overflow, group = exp_name), size = 1.1) +
    geom_hline(yintercept = 0.2) +
    scale_color_manual(values = custom_cols) +
    scale_fill_manual(values = custom_cols) +
    scale_y_continuous(lim = c(0, 1)) +
    customTheme +
    background_grid()+
    facet_wrap(~geography_name)
  
