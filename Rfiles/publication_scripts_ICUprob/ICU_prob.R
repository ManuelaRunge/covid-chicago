

library(tidyverse)
library(cowplot)
library(data.table)
library(raster)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

theme_set(theme_minimal())

#simdate <-'20200919'
#simdate <-'20201121'
simdate <-'20201212'
sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)
if(!dir.exists(file.path(sim_dir, "ICU_prob_plots")))dir.create(file.path(sim_dir, "ICU_prob_plots"))

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen",exp_names)]
exp_names <- exp_names[c(grep("daysdelay",exp_names),grep("counterfactual",exp_names))]
exp_names <- exp_names[!(grepl("counterfactual",exp_names))]

dat <- f_combineDat(sim_dir,exp_names, "hospitaloverflow.csv")
table(dat$exp_name)
table(dat$exp_name,dat$geography_modeled)
unique(dat$geography_modeled)

#subregions=
subregions=c(1,4,11) #,c('covidregion_1','covidregion_4','covidregion_11'))
dat <- dat %>% filter(geography_modeled %in% subregions)
table(dat$exp_name,dat$geography_modeled)

dat$scen_name <- gsub(paste0(simdate, "_IL_regreopen"), "", dat$exp_name)
dat <- dat %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat$rollback[is.na(dat$rollback)] <- "counterfactual"

dat$region <- factor(dat$geography_modeled, levels=subregions, labels=paste0("Region ",subregions))
table(dat$geography_modeled)
table(dat$region)
table(dat$rollback)
table(dat$reopen)
table(dat$delay)


customTheme <- f_getCustomTheme()

pplot <- ggplot(data = subset(dat, delay=="7daysdelay")) + theme_minimal()+
  geom_vline(xintercept = c(-Inf, Inf)) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_rect(xmin = 75, xmax = Inf, ymin = -Inf, ymax =Inf, fill = "grey", alpha = 0.01) +
  geom_line(aes(x = capacity_multiplier*100, y = prob*100, linetype = rollback , col = reopen ), size = 1.1) +
  geom_hline(yintercept = 25, col="red",size=0.5) +
  scale_color_manual(values=c("orange2","#35978f"))+
  scale_fill_manual(values=c("orange2","#35978f"))+
  scale_linetype_manual(values=c('solid','dashed')) + 
  scale_y_continuous(lim = c(0, 101), expand=c(0,0)) +
  customTheme +
  background_grid() +
  facet_wrap( ~ region,scales="free" ) +
  theme(panel.spacing = unit(2, "lines") , legend.position = 'None')+
  labs(y='Probability of ICU overflow (%)',x='Trigger threshold (% of available ICU beds)')

ggsave(paste0("ICU_prob.pdf"), plot = pplot, path =  file.path(sim_dir, "ICU_prob_plots"), width = 12, height=4, device = "pdf")

# rollback=="sm4"
pplot2 <-   ggplot(data = subset(dat, rollback=="pr8")) +
  theme_minimal()+
  geom_vline(xintercept = c(-Inf, Inf)) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_rect(xmin = 75, xmax = Inf, ymin = -Inf, ymax =Inf, fill = "grey", alpha = 0.01) +
  geom_line(aes(x = capacity_multiplier*100, y = prob*100, linetype = delay , col = reopen, group=interaction(delay,reopen) ), size = 1.1) +
  geom_hline(yintercept = 25, col="red",size=0.5) +
  scale_color_manual(values=c("orange2","#35978f"))+
  scale_fill_manual(values=c("orange2","#35978f"))+
  scale_linetype_manual(values=c('solid','dashed','dotted')) + 
  scale_y_continuous(lim = c(0, 101), expand=c(0,0)) +
  customTheme +
  background_grid() +
  facet_wrap( ~ region,scales="free" ) +
  theme(panel.spacing = unit(2, "lines") , legend.position = 'None')+
  labs(y='Probability of ICU overflow (%)',x='Trigger threshold (% of available ICU beds)')

ggsave(paste0("ICU_prob_delay.pdf"), plot = pplot2, path =  file.path(sim_dir, "ICU_prob_plots"), width = 12, height=4, device = "pdf")


pplotall <- plot_grid(pplot,pplot2, ncol=1)
ggsave(paste0("ICU_prob_pplotall.pdf"), plot = pplotall, path = file.path(sim_dir, "ICU_prob_plots"), width = 12, height=8, device = "pdf")


#### For text
dat %>% filter(rollback %in%  c("sm4","sm8")) %>% 
  group_by(geography_modeled,rollback) %>% 
  summarize(prob=mean(prob)) %>%
  pivot_wider(names_from="rollback", values_from="prob") %>%
  mutate(diff = (sm4- sm8)*100)

dat %>% filter(delay %in%  c("1daysdelay","7daysdelay")) %>% 
  group_by(geography_modeled,delay) %>% 
  summarize(prob=mean(prob)) %>%
  pivot_wider(names_from="delay", values_from="prob") %>%
  mutate(diff = (`0daysdelay`- `7daysdelay`)*100)

#### TODO Run regression ?

# dat %>% filter(rollback %in%  c("sm4","sm7") & prob >=0.24 & prob <= 0.26) %>% 
#   group_by(geography_modeled,rollback) %>% 
#   summarize(prob=mean(prob)) %>%
#   pivot_wider(names_from="rollback", values_from="prob") %>%
#   mutate(diff = (sm4- sm7)*100)
# 
# dat %>% filter(delay %in%  c("0daysdelay","7daysdelay")  & prob >=0.24 & prob <= 0.26) %>% 
#   group_by(geography_modeled,delay) %>% 
#   summarize(prob=mean(prob)) %>%
#   pivot_wider(names_from="delay", values_from="prob") %>%
#   mutate(diff = (`0daysdelay`- `7daysdelay`)*100)



### TODO how much time in between?
