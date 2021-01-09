
library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

TwoCols_seq <- c("#00a79d", "#f7941d")
capacity_col <- "#2a3b90"
customTheme <- f_getCustomTheme()

## -------------------------------
## Run script
## -------------------------------
simdate <- "20201212"
sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (!dir.exists(file.path(sim_dir, "ICU_bar_plots"))) dir.create(file.path(sim_dir, "ICU_bar_plots"))
if (!dir.exists(file.path(sim_dir, "ICU_reduction_plots"))) dir.create(file.path(sim_dir, "ICU_reduction_plots"))


exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c(grep("daysdelay", exp_names), grep("counterfactual", exp_names))]
exp_names <- exp_names[!(grepl("_reopen", exp_names))]

dat <- f_combineDat(sim_dir, exp_names, "peak_exceed_df.csv") %>%
  filter(ems %in% c("EMS-1", "EMS-4", "EMS-11")) %>%
  f_get_scenVars()

dat$region <- factor(dat$ems, levels = c(paste0("EMS-", c(1, 4, 11))), labels = paste0("Region ", c(1, 4, 11)))


rollback_values <- unique(dat$rollback)
delay_values <- unique(dat$delay)

rollback_val <- rollback_values[2]
delay_val <- delay_values[1]

table(dat$capacity_multiplier_fct, exclude = NULL)
dat$capacity_multiplier_fct



ggplot(data=subset(dat, delay=="1daysdelay"))+
  geom_point(aes(x=capacity_multiplier_fct2, y=exceed_diff_1_median, col=reopen_fct ))+
  facet_wrap(reopen~region)


ggplot(data=subset(dat, delay=="1daysdelay"))+
  geom_point(aes(x=exceed_diff_1_median , y=capacity_multiplier_fct2, col=reopen_fct ))+
  geom_line(aes(x=exceed_diff_1_median , y=capacity_multiplier_fct2, col=reopen_fct ))+
  facet_wrap(reopen~region)



