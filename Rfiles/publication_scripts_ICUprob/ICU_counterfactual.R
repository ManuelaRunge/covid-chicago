


library(tidyverse)
library(cowplot)
library(data.table)


source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")
TwoCols_seq <- c("#00a79d", "#f7941d")
capacity_col <- "#2a3b90"
theme_set(theme_minimal())
customTheme <- f_getCustomTheme()

simdate <-'20201212'
sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)
if(!dir.exists(file.path(sim_dir, "ICU_bar_plots")))dir.create(file.path(sim_dir, "ICU_bar_plots"))

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[(grep("counterfactual",exp_names))]

dat <- f_combineDat(sim_dir,exp_names, "trajectories_aggregated.csv")
Rdat <- f_combineDat(sim_dir,exp_names, "Rt_dat.csv")

#subregions=
subregions=c('covidregion_1','covidregion_4','covidregion_11')
dat <- dat %>% filter(geography_modeled %in% subregions)
dat$region <- factor(dat$geography_modeled, levels=subregions, labels=paste0("Region ",c(1,4,11)))
dat$date <- as.Date(dat$date)
table(dat$exp_name,dat$geography_modeled)

dat$scen_name <- gsub(paste0(simdate, "_IL_regreopen"), "", dat$exp_name)
dat <- dat %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat$rollback[is.na(dat$rollback)] <- "counterfactual"

rollback_val <- unique(dat$rollback)
delay_val <- unique(dat$delay)


capacityDat <- load_new_capacity(filedate = "20200915")
capacityDat <- capacityDat %>%
  filter(geography_name %in% c(1,4,11)) %>%
  rename(avg_resource_available=icu_available )
capacityDat$region <- factor(capacityDat$geography_name, levels = c(1,4,11), labels = paste0("Region ", c(1,4,11)))

TwoCols_seq1 <- c("grey", "grey")
pplot <- ggplot(data=subset(dat, rollback=='counterfactual' & date > as.Date("2020-10-01") &  date < as.Date("2020-12-31"))) +
  #geom_rect()+
  geom_ribbon(aes(x=date, ymin=crit_det_95CI_lower, ymax=crit_det_95CI_upper,fill=reopen), alpha=0.3)+
  geom_ribbon(aes(x=date, ymin=crit_det_50CI_lower, ymax=crit_det_50CI_upper,fill=reopen), alpha=0.4)+
  geom_line(aes(x=date, y=crit_det_median,col=reopen),size=1)+
  scale_fill_manual(values=TwoCols_seq)+
  scale_color_manual(values=TwoCols_seq)+
  facet_wrap(~region, scales = "free", ncol = 1, strip.position="right") +
  scale_x_date(date_breaks = "30 days", date_label="%b")+
  scale_y_continuous(expand=c(0,0))+
  geom_hline(yintercept = 0)+
  geom_hline(data=capacityDat, aes(yintercept=avg_resource_available), 
             linetype="dashed", col=capacity_col,size=1)+
  theme(legend.position = "none")+
  labs(x="",y="Predictd peak in ICU census\nuntil end of December")+
  geom_text(data=capacityDat,
            aes(x=as.Date("2020-10-10"), 
                y=avg_resource_available+(avg_resource_available*0.5), 
                label="ICU capacity"), col=capacity_col)+
  customTheme

pplot

f_save_plot(plot_name=paste0("timeline_counterfactual"), pplot = pplot, 
            plot_dir = file.path(sim_dir, "ICU_timeline_plots"), width = 6, height = 8)

