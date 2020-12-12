

library(tidyverse)
library(cowplot)
library(data.table)
library(raster)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")


exp_names <- list.dirs(file.path(simulation_output,'_overflow_simulations'),recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep('20200919_IL_regreopen', exp_names)]
exp_names <- exp_names[!grepl('combined', exp_names)]

datList=list()
for(exp_name in exp_names){
  exp_dir <- file.path(simulation_output,'_overflow_simulations',exp_name)
  if(!file.exists(file.path(exp_dir, "hospitaloverflow.csv")))next
  datList[[length(datList)+1]] <- fread(file.path(exp_dir, "hospitaloverflow.csv")) %>% mutate(exp_name=exp_name)
}
dat <- datList %>% bind_rows() %>% separate(exp_name, into=c("simdate","reg","reopen","delay","rollback"), sep="_") %>% mutate(delay=gsub("daysdelay","",delay))
table(dat$exp_name)

subregions=c('covidregion_1','covidregion_4','covidregion_11')
#selectedCols <- c("#c6dbef", "#6baed6", "#2171b5")
#selectedCols <- c("#fee0d2", "#fb6a4a", "#cb181d")
selectedCols <- c("#2171b5","#cb181d")


ggplot(data = subset(dat, delay == "3" & geography_modeled  %in% subregions)) +
  geom_hline(yintercept = Inf) +
  geom_vline(xintercept = Inf) +
  geom_bar(aes(x = capacity_multiplier, y = number_that_exceed_median, fill = rollback, group = interaction(delay, capacity_multiplier)),
           col = "azure4", position = position_dodge(width = 0.5), stat = "identity"
  ) +
  geom_errorbar(data = subset(dat, delay == "3" & geography_modeled  %in% subregions & rollback ==unique(dat$rollback)[1]),
                aes(x = capacity_multiplier, ymin = number_that_exceed_95CI_lower, ymax = number_that_exceed_95CI_upper, group = interaction(delay, capacity_multiplier)),
                col = "black", position = position_dodge(width = 0.5), width = 0, stat = "identity"
  ) +
  facet_grid(geography_modeled~ reopen, scales="free") +
  labs(
    subtitle = "", y = expression(italic(ICU[predicted_peak] - ICU[capacity])),
    x = "Trigger threshold"
  ) +
  scale_fill_manual(values = selectedCols) +
  scale_color_manual(values = selectedCols) +
  geom_hline(yintercept = 0, linetype = "solid", size = 0.75) +
  theme(panel.grid.major.y = element_line(colour = "grey", size = 0.75))+
  customTheme 



