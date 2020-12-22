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
theme_set(theme_minimal())
## -------------------------------
## Run script
## -------------------------------
capacityDat <- load_new_capacity(filedate = "20200915")
capacityDat <- capacityDat %>%
  filter(geography_name %in% c(1,4,11)) %>%
  rename(avg_resource_available=icu_available )
capacityDat$region <- factor(capacityDat$geography_name, levels = c(1,4,11), labels = paste0("Region ", c(1,4,11)))


simdate <-'20201212'
sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)
if (!dir.exists(file.path(sim_dir, "ICU_trajectories_plots"))) dir.create(file.path(sim_dir, "ICU_trajectories_plots"))
if (!dir.exists(file.path(sim_dir, "ICU_trajectories_plots", "pdf"))) dir.create(file.path(sim_dir, "ICU_trajectories_plots", "pdf"))

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen",exp_names)]
exp_names <- exp_names[c(grep("daysdelay",exp_names),grep("counterfactual",exp_names))]
exp_names <- exp_names[!(grepl("_reopen",exp_names))]

exp_name1 <- "20201212_IL_regreopen100perc_1daysdelay_pr6"
exp_name2 <- "20201212_IL_regreopen50perc_1daysdelay_pr6"

dat1 <- f_load_trajectories(sim_dir, exp_name1, region_nr = 1) %>% mutate(exp_name=exp_name1)
dat2 <- f_load_trajectories(sim_dir, exp_name2, region_nr = 1) %>% mutate(exp_name=exp_name2)


dat <-  rbind(dat1,dat2) %>%
  left_join(capacityDat, by="region") %>%
  filter(date >=as.Date("2020-07-01")) %>% 
  group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
  mutate(peak=max(crit_det)) %>% 
  mutate(above_yn = ifelse(peak > avg_resource_available & date <= as.Date("2020-12-31"), 1,0))

dat$scen_name <- gsub(paste0(simdate, "_IL_regreopen"), "", dat$exp_name)
dat <- dat %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat$rollback[is.na(dat$rollback)] <- "counterfactual"

rollback_val <- unique(dat$rollback)
delay_val <- unique(dat$delay)

table(dat$exp_name,dat$geography_modeled)
length(unique(dat$sample_num))
length(unique(dat$capacity_multiplier))
tapply(dat$nsamples, dat$capacity_multiplier, summary)
tapply(dat$nmultiplier_per_sample, dat$sample_num, summary)


datsub <- dat %>% filter(date==max(date) & reopen=="100perc")
table(datsub$nmultiplier_per_sample,datsub$capacity_multiplier)
dat %>% filter(date==max(date)) %>% group_by(capacity_multiplier)  %>% tally()

#### get filter for before after peak
peakDate <- dat %>%
  group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
  filter(crit_det==peak)  %>%
  mutate(peak_date=date) %>% 
  group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
  filter(peak_date==min(peak_date))  %>% 
  select(exp_name,capacity_multiplier, sample_num, scen_num, peak_date)

dat <- dat %>% 
  left_join(peakDate , by=c('exp_name','capacity_multiplier', 'sample_num', 'scen_num')) %>%
  group_by(exp_name,capacity_multiplier)  %>%
  mutate(after_peak_yn = ifelse( date > peak_date , 1,0))

summary(dat$peak_date)
table(dat$after_peak_yn)


triggerdat <- dat %>% 
  filter(date > as.Date("2020-10-01"))%>% 
  group_by(exp_name,sample_num, avg_resource_available,capacity_multiplier, capacity_multiplier_fct2) %>%
  arrange(Ki_t) %>% 
  summarize(min_Ki = min(Ki_t), max_Ki = max(Ki_t)) %>%
  mutate(mitigation_activated = ifelse(min_Ki < max_Ki, 1, 0))
table(triggerdat$capacity_multiplier_fct2, triggerdat$mitigation_activated)


#### Trajectory plot
### Investigate unique samples
plotdat <- subset(dat,date <=as.Date("2020-12-31") & 
                    peak_date <=as.Date("2020-12-31") & 
                    capacity_multiplier_fct2 %in% c(20, 40, 60, 80,100))

unique(plotdat$sample_num)
table(plotdat$sample_num)
#plotdat <- plotdat %>% 
#  filter(sample_num %in% c(28, 37))
table(plotdat$sample_num, plotdat$capacity_multiplier_fct2)

ggplot(data=subset(plotdat, sample_num==3)) +
  geom_line(aes(x=date, y=crit_det, 
                group=interaction(scen_num), col=as.factor(capacity_multiplier_fct2))) +
  scale_y_continuous(expand=c(0,0), breaks=seq(0,180,20))+
  facet_wrap(~exp_name) +  
  theme(legend.position = "none")+
  customTheme

plotdat <- subset(dat,date >=as.Date("2020-06-01") &
                    date <=as.Date("2020-12-31") & 
                    peak_date <=as.Date("2020-12-31") & 
                    capacity_multiplier_fct2 %in% c(20, 40, 60, 80,100))

trigger_capacity_Dat <- plotdat %>% 
  select(exp_name,capacity_multiplier, sample_num, scen_num, avg_resource_available, crit_det,date) %>%
  mutate(trigger_capacity_val =avg_resource_available*capacity_multiplier) %>%
  filter(crit_det > trigger_capacity_val)  %>% 
  group_by(exp_name,capacity_multiplier,sample_num, scen_num, avg_resource_available, trigger_capacity_val) %>% 
  filter(date==min(date)) %>%
  rename(trigger_capacity_date=date) %>%
  unique()
summary(trigger_capacity_Dat$trigger_capacity_date)
table(trigger_capacity_Dat$capacity_multiplier)
trigger_capacity_Dat

plotdat <- plotdat %>% 
  f_addVar(trigger_capacity_Dat) %>%
  group_by(exp_name,capacity_multiplier)  %>%
  mutate(after_trigger_capacity_yn = ifelse( date > trigger_capacity_date , 1,0))

summary(plotdat$trigger_capacity_date)
table(plotdat$after_trigger_capacity_yn)
