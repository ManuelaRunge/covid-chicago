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
table(dat[dat$date==max(dat$date),'nmultiplier_per_sample'])

####TO DO Not use
scen_above <- dat %>%
  filter(date >= as.Date("2020-10-01") & date <= as.Date("2020-12-31")) %>%
  group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>%
  mutate(peak=max(crit_det)) %>% 
  filter(
         peak > avg_resource_available) %>% 
  dplyr::select(scen_num) %>% 
  unique()
dat$above_yn2=0
dat$above_yn2[dat$scen_num %in% scen_above$scen_num] <-1

table(dat$above_yn,dat$above_yn2, exclude = NULL)
table(dat$above_yn)
table(dat$above_yn2)

#### get filter for before after peak
peakDate <- dat %>%
  group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
  filter(crit_det==peak)  %>% 
  mutate(peak_date=date) %>% 
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


#dat_full = dat
dat <- dat %>% 
  filter(nmultiplier_per_sample>=10)

length(unique(dat$sample_num))
unique(dat$capacity_multiplier)

#### Trajectory plot
unique(dat$capacity_multiplier_fct2)

### Investigate unique samples
plotdat <- subset(dat,date <=as.Date("2020-12-31") & 
                    peak_date <=as.Date("2020-12-31") & 
                    capacity_multiplier_fct2 %in% c(20, 40, 60, 80,100))

unique(plotdat$sample_num)
table(plotdat$sample_num)
plotdat <- plotdat %>% 
  filter(sample_num %in% c(28, 37))

table(plotdat$sample_num, plotdat$capacity_multiplier_fct2)
ggplot(data=plotdat) +
  geom_line(data=subset(plotdat), 
            aes(x=date, y=(crit_det/avg_resource_available)*100, 
                group=interaction(scen_num), col=as.factor(sample_num))) +
  geom_hline(aes(yintercept = 100 * capacity_multiplier), col=capacity_col, linetype="solid")+
  scale_y_continuous(expand=c(0,0), breaks=seq(0,180,20))+
  geom_hline(yintercept = 0) +
  facet_wrap(exp_name~capacity_multiplier_fct2, nrow=2) +  geom_hline(yintercept = 100)+
  customTheme

plotdat <- subset(dat,date >=as.Date("2020-09-01") &
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

plotdat$reopen_fct <- factor(plotdat$reopen, 
                             levels=c("100perc","50perc"), 
                             labels=c("High\ntransmission\nincrease","Low\ntransmission\nincrease"))

pplot1 <- ggplot(data=plotdat) +
  geom_rect(xmin=-Inf, xmax=Inf, ymin=100, ymax=Inf, fill="white", alpha=1) + 
  geom_rect(xmin=-Inf, xmax=Inf, ymin=100, ymax=Inf, fill="#e5e5e5", alpha=0.07) + 
  #geom_rect(xmin=-Inf, xmax=Inf, ymin=100, ymax=Inf, fill="#fde8e9", alpha=0.2) +
  #geom_line(data=subset(plotdat, after_trigger_capacity_yn==0), 
  #          aes(x=date, y=(crit_det/avg_resource_available)*100, 
  #              group=interaction(scen_num), col=capacity_multiplier_fct2), alpha=0.5) +
  geom_line(data=subset(plotdat), 
            aes(x=date, y=(crit_det/avg_resource_available)*100, 
                group=interaction(scen_num)),col="#b2b2b2",size=0.5) +
  geom_line(data=subset(plotdat, after_peak_yn!=1), 
            aes(x=date, y=(crit_det/avg_resource_available)*100, 
                group=interaction(scen_num),col=capacity_multiplier_fct2),size=0.5) +
  geom_hline(yintercept = 100,col=capacity_col, linetype="dashed")+
  geom_hline(aes(yintercept = 100 * capacity_multiplier, col=capacity_multiplier_fct2), linetype="longdash", size=1)+
  geom_point(aes(x=peak_date, y=(peak/avg_resource_available)*100, 
                 group=interaction(scen_num),fill=capacity_multiplier_fct2),col="black",shape=21) +
  #geom_point(aes(x=trigger_capacity_date, y=(trigger_capacity_val/avg_resource_available)*100, 
  #               group=interaction(scen_num),fill=capacity_multiplier_fct2),col=capacity_col,shape=21,alpha=0.5) +
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
f_save_plot(plot_name=paste0("timeline_region1"), pplot = pplot1, 
            plot_dir = file.path(sim_dir, "ICU_trajectories_plots"), width = 14, height = 7)



##### Simplified
plotdat <- plotdat %>% filter(reopen=="100perc" & capacity_multiplier>0.2)
pplot1 <- ggplot(data=plotdat) +
  geom_rect(xmin=-Inf, xmax=Inf, ymin=100, ymax=Inf, fill="white", alpha=1) + 
  geom_rect(xmin=-Inf, xmax=Inf, ymin=100, ymax=Inf, fill="#e5e5e5", alpha=0.07) + 
  #geom_rect(xmin=-Inf, xmax=Inf, ymin=100, ymax=Inf, fill="#fde8e9", alpha=0.2) +
  #geom_line(data=subset(plotdat, after_trigger_capacity_yn==0), 
  #          aes(x=date, y=(crit_det/avg_resource_available)*100, 
  #              group=interaction(scen_num), col=capacity_multiplier_fct2), alpha=0.5) +
  geom_line(data=subset(plotdat), 
            aes(x=date, y=(crit_det/avg_resource_available)*100, 
                group=interaction(scen_num)),col="#b2b2b2",size=0.5) +
  geom_line(data=subset(plotdat, after_peak_yn!=1), 
            aes(x=date, y=(crit_det/avg_resource_available)*100, 
                group=interaction(scen_num),col=capacity_multiplier_fct2),size=0.5) +
  geom_hline(yintercept = 100,col=capacity_col, linetype="dashed")+
  geom_hline(aes(yintercept = 100 * capacity_multiplier, col=capacity_multiplier_fct2), linetype="longdash", size=1)+
  geom_point(aes(x=peak_date, y=(peak/avg_resource_available)*100, 
                 group=interaction(scen_num),fill=capacity_multiplier_fct2),col="black",shape=21) +
  #geom_point(aes(x=trigger_capacity_date, y=(trigger_capacity_val/avg_resource_available)*100, 
  #               group=interaction(scen_num),fill=capacity_multiplier_fct2),col=capacity_col,shape=21,alpha=0.5) +
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

datesDat <- plotdat %>% ungroup() %>%
            select(exp_name, scen_num, sample_num, capacity_multiplier_fct2,capacity_multiplier_fct, peak_date ,trigger_capacity_date) %>% 
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
  pivot_longer(col=-c(exp_name, capacity_multiplier_fct2,capacity_multiplier_fct, scen_num, sample_num), names_to="date_type") 

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
  geom_jitter(data=datesDat_long,aes(x=date_type, y=value, group=date_type, fill=capacity_multiplier_fct2), 
              width=0.08, height=0.01, alpha=0.5,shape=21) +
  geom_pointrange(aes(x=date_type, ymin=min, ymax=max, y=mean, fill=capacity_multiplier_fct2),shape=21,size=1) +
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

f_save_plot(plot_name=paste0("trajectories_dates_100perc_pall"), pplot = pplot, 
            plot_dir = file.path(sim_dir, "ICU_trajectories_plots"), width = 10, height = 4)



#### For text
datesDat %>% group_by(capacity_multiplier_fct2) %>% summarize(mean(diff_trigger_peak))

datesDatAggr %>% ungroup() %>% select(-exp_name) %>%as.data.frame()

datesDatAggr %>% ungroup() %>% select(-exp_name) %>%as.data.frame() %>% 
  group_by(capacity_multiplier_fct) %>% 
  summarize(range = trigger_capacity_date_max - trigger_capacity_date_min)

datesDatAggr %>% ungroup() %>% select(-exp_name) %>%as.data.frame() %>% 
  group_by(capacity_multiplier_fct) %>% 
  select(capacity_multiplier_fct, trigger_capacity_date_mean) %>%
  mutate(diff= lag(trigger_capacity_date_mean) -trigger_capacity_date_mean)

datesDatAggr %>% ungroup() %>% select(-exp_name) %>%as.data.frame() %>% 
  group_by(capacity_multiplier_fct) %>% 
  summarize(range = peak_date_mean - trigger_capacity_date_mean)


###=======================================
#### Additional plot
###=======================================
pplot1 <- ggplot(data=subset(dat,date <=as.Date("2020-12-31") & capacity_multiplier_fct2 %in% c(20, 40, 60, 80,100))) +
  geom_line(aes(x=date, y=(crit_det/121)*100, 
                  group=interaction(sample_num,capacity_multiplier_fct2),
                  col=capacity_multiplier_fct2))+
  scale_color_viridis_d(option="B", direction=1)+
  geom_hline(aes(yintercept = 100 * capacity_multiplier), col=capacity_col, linetype="solid")+
  labs(x="",
       y="Predicted ICU census\nrelative to capacity (%)", 
       color="Trigger threshold")+
  scale_y_continuous(expand=c(0,0), breaks=seq(0,180,20))+
  geom_hline(yintercept = 0) +
  theme(legend.position="none")+
  customTheme

pplot1
pplot1 + facet_wrap(~capacity_multiplier_fct2)

### divide into trigger pulled yes no (even if for supplement)
## add capacity lines in shaded areas!!!



### same aggregated version
datAggr <- dat %>% 
  filter (date <=as.Date("2020-12-31") & capacity_multiplier_fct2 %in% c(20, 40, 60, 80,100)) %>%
  ungroup()%>%
  dplyr::group_by(date, capacity_multiplier, capacity_multiplier_fct, capacity_multiplier_fct2) %>%
  dplyr::mutate(value=(crit_det/121)*100) %>%
  dplyr::summarize( min.value = min(value, na.rm = TRUE),
                        max.value = max(value, na.rm = TRUE),
                        median.value = median(value, na.rm = TRUE),
                        q25.value = quantile(value, probs = 0.25, na.rm = TRUE),
                        q75.value = quantile(value, probs = 0.75, na.rm = TRUE),
                        q2.5.value = quantile(value, probs = 0.025, na.rm = TRUE),
                        q97.5.value = quantile(value, probs = 0.975, na.rm = TRUE))
  
pplot2 <- ggplot(data=datAggr) +
  # geom_rect(xmin=-Inf, xmax=as.Date("2020-10-01"), ymin=-Inf, ymax=Inf, fill="grey", alpha=0.03) +
  #geom_vline(xintercept = as.Date("2020-10-01"))+
  #geom_rect(xmin=-Inf, xmax=Inf, ymin=121*0.8, ymax=121, fill="grey", alpha=0.03) +
  geom_ribbon(aes(x=date, ymin=min.value, ymax=max.value, fill=capacity_multiplier_fct2), alpha=0.1) +
  geom_ribbon(aes(x=date, ymin=q2.5.value, ymax=q97.5.value, fill=capacity_multiplier_fct2), alpha=0.2) +
  geom_ribbon(aes(x=date, ymin=q25.value, ymax=q75.value, fill=capacity_multiplier_fct2),alpha=0.4) +
  geom_line(aes(x=date, y=median.value, 
                group=interaction(capacity_multiplier_fct2),
                col=capacity_multiplier_fct2))+
  scale_color_viridis_d(option="B", direction=1)+
  scale_fill_viridis_d(option="B", direction=1)+
  geom_hline(yintercept = 100, col=capacity_col, linetype="solid")+
  geom_hline(yintercept = 100*0.8, col=capacity_col, linetype="dashed")+
  geom_hline(yintercept = 100*0.6, col=capacity_col, linetype="dashed")+
  geom_hline(yintercept = 100*0.4, col=capacity_col, linetype="dashed")+
  geom_hline(yintercept = 100*0.2, col=capacity_col, linetype="dashed")+
  labs(x="",
       y="Predicted ICU census\nrelative to capacity (%)", 
       color="Trigger threshold",
      fill="Trigger threshold")+
  scale_y_continuous(expand=c(0,0), breaks=seq(0,180,20))+
  geom_hline(yintercept = 0) +
  theme(legend.position="none")+
  customTheme

pplot2


pplot <- plot_grid(pplot1, pplot2, nrow=1)
pplot


