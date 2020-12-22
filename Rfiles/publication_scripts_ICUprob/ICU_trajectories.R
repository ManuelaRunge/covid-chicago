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


#### Trajectory plot
plotdat <- subset(dat,date >=as.Date("2020-09-01") &
                    date <=as.Date("2020-12-31") & 
                    peak_date <=as.Date("2020-12-31") & 
                    capacity_multiplier_fct2 %in% c(40, 60, 80,100))

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

plotdat$reopen_fct <- factor(plotdat$reopen, 
                             levels=c("100perc","50perc"), 
                             labels=c("High\ntransmission\nincrease","Low\ntransmission\nincrease"))

##---------------------------------
## Prepare A
##---------------------------------

pplot1 <- ggplot(data=plotdat) +
  geom_rect(xmin=-Inf, xmax=Inf, ymin=100, ymax=Inf, fill="white", alpha=1) + 
  geom_rect(xmin=-Inf, xmax=Inf, ymin=100, ymax=Inf, fill="#e5e5e5", alpha=0.07) + 
  geom_line(data=subset(plotdat), 
            aes(x=date, y=(crit_det/avg_resource_available)*100, 
                group=interaction(scen_num)),col="#b2b2b2",size=0.5) +
  geom_line(data=subset(plotdat, after_peak_yn!=1), 
            aes(x=date, y=(crit_det/avg_resource_available)*100, 
                group=interaction(scen_num),
                col=capacity_multiplier_fct2),size=0.5) +
  geom_hline(yintercept = 100,col=capacity_col, linetype="dashed")+
  geom_hline(aes(yintercept = 100 * capacity_multiplier),
             col="#262626", 
             linetype="longdash", size=1)+
  geom_point(aes(x=peak_date, y=(peak/avg_resource_available)*100, 
                 group=interaction(scen_num),
                 fill=capacity_multiplier_fct2),col="black",shape=21) +
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")+
  labs(subtitle="ICU threshold to trigger mitigation (%)",
       x="",
       y="Predicted ICU census\nrelative to capacity (%)", 
       color="Trigger threshold")+
  scale_y_continuous(expand=c(0,0), breaks=seq(0,180,20), labels=seq(0,180,20))+
  geom_hline(yintercept = c(0, Inf)) +  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(plot.subtitle = element_text(hjust=0.5),
        legend.position="none",
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(),
        axis.text.x = element_text(angle=90, vjust=0))+
  facet_grid(reopen_fct~capacity_multiplier_fct,scales="free") + 
  customTheme

pplot1
f_save_plot(plot_name=paste0("timeline_region1_full"), pplot = pplot1, 
            plot_dir = file.path(sim_dir, "ICU_trajectories_plots"), width = 14, height = 7)



##### Simplified
plotdat <- plotdat %>% filter(reopen=="100perc" & capacity_multiplier>0.2)

pplot1 <- ggplot(data=plotdat) +
  geom_rect(xmin=-Inf, xmax=Inf, ymin=100, ymax=Inf, fill="white", alpha=1) + 
  geom_rect(xmin=-Inf, xmax=Inf, ymin=100, ymax=Inf, fill="#e5e5e5", alpha=0.07) + 
  geom_line(data=subset(plotdat), 
            aes(x=date, y=(crit_det/avg_resource_available)*100, 
                group=interaction(scen_num)),col="#b2b2b2",size=0.5) +
  geom_line(data=subset(plotdat, after_peak_yn!=1), 
            aes(x=date, y=(crit_det/avg_resource_available)*100, 
                group=interaction(scen_num),col=capacity_multiplier_fct2),size=0.5) +
  geom_hline(yintercept = 100,col=capacity_col, linetype="dashed")+
  geom_hline(aes(yintercept = 100 * capacity_multiplier, 
                 col=capacity_multiplier_fct2), linetype="longdash", size=1)+
  geom_point(aes(x=peak_date, y=(peak/avg_resource_available)*100, 
                 group=interaction(scen_num),
                 fill=capacity_multiplier_fct2),col="black",shape=21) +
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")+
  labs(subtitle="ICU threshold to trigger mitigation (%)",
       x="",
       y="Predicted ICU census\nrelative to capacity (%)", 
       color="Trigger threshold")+
  scale_y_continuous(expand=c(0,0), breaks=seq(0,180,20), labels=seq(0,180,20), lim=c(0,200))+
  geom_hline(yintercept = c(0, Inf)) + 
  geom_vline(xintercept = c(-Inf, Inf)) +
  #geom_text(x=as.Date("2020-10-01"), y=110, label="ICU capacity", col=capacity_col)+
  #geom_text(x=as.Date("2020-10-01"), y=40, label="Trigger threshold", col="black")+
  theme(plot.subtitle = element_text(hjust=0.5),
        legend.position="none",
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(),
        axis.text.x = element_text(angle=90, vjust=0))+
  facet_wrap(~capacity_multiplier_fct,nrow=1) + 
  customTheme

pplot1


f_save_plot(plot_name=paste0("timeline_region1_100perc"), pplot = pplot1, 
            plot_dir = file.path(sim_dir, "ICU_trajectories_plots"), width = 14, height = 7)


##---------------------------------
## Prepare B with dates
##---------------------------------
datesDat <- plotdat %>% 
            ungroup() %>%
            select(exp_name, scen_num, sample_num, capacity_multiplier_fct2,
                   capacity_multiplier_fct, peak_date ,trigger_capacity_date) %>% 
            unique() %>% 
            filter(!is.na(trigger_capacity_date)) %>%
            mutate(diff_trigger_peak= peak_date - trigger_capacity_date)

datesDatAggr <- datesDat %>% 
  group_by(exp_name, capacity_multiplier_fct2,capacity_multiplier_fct) %>% 
  summarize(peak_date_min=min(peak_date),
            peak_date_max=max(peak_date),
            peak_date_mean=mean(peak_date),
            trigger_capacity_date_min=min(trigger_capacity_date),
            trigger_capacity_date_max=max(trigger_capacity_date),
            trigger_capacity_date_mean=mean(trigger_capacity_date),
            diff_trigger_peak_min=min(diff_trigger_peak),
            diff_trigger_peak_max=max(diff_trigger_peak),
            diff_trigger_peak_mean=mean(diff_trigger_peak))
  
datesDat_long <- datesDat%>% 
  dplyr::select(-c(diff_trigger_peak)) %>% 
  pivot_longer(col=-c(exp_name, capacity_multiplier_fct2,
                      capacity_multiplier_fct, scen_num, sample_num), 
               names_to="date_type") 

datesDatAggr_long <- datesDatAggr %>% 
  dplyr::select(-c(diff_trigger_peak_max,diff_trigger_peak_min,diff_trigger_peak_mean)) %>% 
  pivot_longer(col=-c(exp_name, capacity_multiplier_fct2,capacity_multiplier_fct)) %>%
  mutate(name=gsub("_m","__m",name)) %>%
  separate(name, into=c("date_type","stat"), sep="__") %>%
  pivot_wider(names_from="stat", values_from="value")

datesDatAggr_long$date_type <- factor(datesDatAggr_long$date_type,
                                      levels=c("trigger_capacity_date","peak_date"), 
                                      labels=c("trigger\ndate",'peak\ndate'))

datesDat_long$date_type <- factor(datesDat_long$date_type,
                                      levels=c("trigger_capacity_date","peak_date"), 
                                      labels=c("trigger\ndate",'peak\ndate'))

pplot <- ggplot(data=datesDatAggr_long)+
  geom_jitter(data=datesDat_long,aes(x=date_type, y=value, 
                                     group=date_type, fill=capacity_multiplier_fct2), 
              width=0.08, height=0.01, alpha=0.5,shape=21) +
  geom_pointrange(aes(x=date_type, ymin=min, ymax=max, y=mean, 
                      fill=capacity_multiplier_fct2),shape=21,size=1) +
  coord_flip()+
  facet_wrap(~capacity_multiplier_fct, nrow=1)+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")+
  scale_y_date(date_breaks = "30 days",date_minor_breaks = "5 days",
               lim=c(as.Date("2020-09-01"), 
                     as.Date("2020-12-31")), date_labels = "%d\n%b")+
  customTheme+
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.ticks = element_line())+
  geom_hline(yintercept = c(-Inf, Inf)) + 
  geom_vline(xintercept = c(-Inf, Inf)) 

pplot

f_save_plot(plot_name=paste0("trajectories_dates_100perc_full"), pplot = pplot, 
            plot_dir = file.path(sim_dir, "ICU_trajectories_plots"), width = 10, height = 4)


##---------------------------------------
#### For text
##---------------------------------------
sink( file.path(sim_dir, "ICU_trajectories_plots","for_text.txt"))

datesDat %>% 
  group_by(capacity_multiplier_fct2) %>% 
  summarize(mean(diff_trigger_peak))

datesDatAggr %>% 
  ungroup() %>% 
  select(-exp_name) %>% 
  as.data.frame()

datesDatAggr %>% 
  ungroup() %>% 
  select(-exp_name) %>% 
  as.data.frame() %>% 
  group_by(capacity_multiplier_fct) %>% 
  summarize(range = trigger_capacity_date_max - trigger_capacity_date_min)

datesDatAggr %>% 
  ungroup() %>% 
  select(-exp_name) %>% 
  as.data.frame() %>% 
  group_by(capacity_multiplier_fct) %>% 
  select(capacity_multiplier_fct, trigger_capacity_date_mean) %>%
  mutate(diff= lag(trigger_capacity_date_mean) -trigger_capacity_date_mean)

datesDatAggr %>% 
  ungroup() %>% 
  select(-exp_name) %>%
  as.data.frame() %>% 
  group_by(capacity_multiplier_fct) %>% 
  summarize(range = peak_date_mean - trigger_capacity_date_mean)

sink()

