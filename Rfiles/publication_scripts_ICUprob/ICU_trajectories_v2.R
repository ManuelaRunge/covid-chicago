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

for(reg_nr in c(1,4,11)){
selected_region <- paste0("covidregion_", reg_nr)
dat1 <- f_load_trajectories(sim_dir, exp_name1, region_nr =reg_nr) %>% mutate(exp_name=exp_name1)
dat2 <- f_load_trajectories(sim_dir, exp_name2, region_nr = reg_nr) %>% mutate(exp_name=exp_name2)


dat <-  rbind(dat1,dat2) %>%
  left_join(capacityDat, by="region") %>%
  filter(date >=as.Date("2020-09-01") &
           date <=as.Date("2020-12-31")) %>% 
  group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
  mutate(peak=max(crit_det)) %>% 
  mutate(above_yn = ifelse(peak > avg_resource_available, 1,0)) %>%
  f_get_scenVars()

rm(dat1, dat2)

rollback_val <- unique(dat$rollback)
delay_val <- unique(dat$delay)

table(dat$exp_name,dat$geography_modeled)
length(unique(dat$sample_num))
length(unique(dat$capacity_multiplier))
tapply(dat$nsamples, dat$capacity_multiplier, summary)
tapply(dat$nmultiplier_per_sample, dat$sample_num, summary)


dat %>% filter(date==max(date)) %>% group_by(capacity_multiplier)  %>% tally()

#### get filter for before after peak
peakDate <- dat %>%
  group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
  filter(crit_det==peak)  %>%
  mutate(peak_date=date) %>% 
  group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
  filter(peak_date==min(peak_date))  %>% 
  filter(peak_date<= as.Date("2020-12-31"))  %>% 
  filter(peak_date >= as.Date("2020-10-01"))  %>% 
  select(exp_name,capacity_multiplier, sample_num, scen_num, peak_date) %>%
  unique()


exceedDate <- dat %>%
  group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
  filter(crit_det>=avg_resource_available)  %>%
  mutate(exceed_date_from=min(date),
         exceed_date_to=max(date),
         exceed_diff= exceed_date_to- exceed_date_from) %>% 
  filter(exceed_date_to<= as.Date("2020-12-31"))  %>% 
  filter(exceed_date_from >= as.Date("2020-10-01"))  %>% 
  group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
  select(exp_name,capacity_multiplier, sample_num, scen_num, exceed_date_from, exceed_date_to, exceed_diff) %>%
  unique()


plotdat <- dat %>% 
  #filter(capacity_multiplier %in% c(0.4, 0.6, 0.8, 1)) %>%
  left_join(peakDate , by=c('exp_name','capacity_multiplier', 'sample_num', 'scen_num')) %>%
  left_join(exceedDate , by=c('exp_name','capacity_multiplier', 'sample_num', 'scen_num')) %>%
  group_by(exp_name,capacity_multiplier, sample_num, scen_num)  %>%
  mutate(after_peak_yn = ifelse( date > peak_date , 1,0),
         after_exceed_yn = ifelse( date > exceed_date_from , 1,0))

summary(plotdat$peak_date)
table(plotdat$after_peak_yn)


#### Trajectory plot
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
                             labels=c("High\ntransmission\nincrease",
                                      "Low\ntransmission\nincrease"))
##---------------------------------
## Prepare A
##---------------------------------

summary(plotdat$peak_date)
summary(plotdat$exceed_date_from)
plotdat <- plotdat %>% filter(!is.na(peak_date))

nsubsample <- plotdat %>% 
  filter(crit_det >= trigger_capacity_val) %>%
  group_by(region, exp_name, capacity_multiplier) %>%
  sample_n(50, replace=FALSE) %>%
  mutate(nsamples_sub=n_distinct(sample_num),
         scen_num_sel = scen_num ) %>%
  dplyr::select(region, exp_name, capacity_multiplier, scen_num_sel, nsamples_sub) %>%
  unique()

plotdat_sub <- plotdat %>% 
  select(-sample_num) %>% unique() %>%
  left_join(nsubsample, by=c('region', 'exp_name', 'capacity_multiplier')) %>%
  filter(scen_num %in% scen_num_sel) 

dim(plotdat_sub)

plotdat_sub2 <- plotdat_sub %>% 
  filter(capacity_multiplier %in% c(0.4, 0.6, 0.8, 1)) %>%
  dplyr::select(date,region,rollback_fct, reopen, capacity_multiplier, capacity_multiplier_fct, exp_name,scen_num,
                after_peak_yn,after_exceed_yn, crit_det, exceed_date_to, 
                exceed_date_from,exceed_diff, avg_resource_available,peak_date, peak,
                trigger_capacity_date, trigger_capacity_val,above_yn,nsamples_sub) %>% unique()
dim(plotdat_sub2)

table(plotdat_sub2$nsamples_sub)
table(plotdat_sub2$capacity_multiplier, plotdat_sub2$nsamples_sub)
annotation_dat <- unique(plotdat_sub2[,c("region","capacity_multiplier_fct","capacity_multiplier")])
  
pplot_top <- ggplot(data=plotdat_sub2) +
  geom_rect(xmin=-Inf, xmax=Inf, ymin=100, ymax=Inf, fill="white", alpha=1) + 
  geom_rect(xmin=-Inf, xmax=Inf, ymin=100, ymax=Inf, fill="#e5e5e5", alpha=0.07) + 
  geom_line(aes(x=date, y=(crit_det/avg_resource_available)*100, 
                group=interaction(scen_num, reopen)),col="#b2b2b2",size=0.5) +
  geom_line(data=subset(plotdat_sub2, after_peak_yn!=1), 
            aes(x=date, y=(crit_det/avg_resource_available)*100, 
                group=interaction(scen_num, reopen),
                col=reopen), 
            size=0.5) +
  geom_hline(yintercept = 100, 
             col=reopen, 
             linetype="dashed")+
  geom_hline(data=annotation_dat, 
             aes(yintercept = 100 * capacity_multiplier),
             col="black", 
             linetype="longdash", 
             size=1)+
  geom_point(aes(x=peak_date, y=(peak/avg_resource_available)*100, 
                 group=interaction(scen_num, reopen),
                 fill=reopen),col="black",shape=21) +
  scale_color_manual(values=TwoCols_seq)+
  scale_fill_manual(values=TwoCols_seq)+
  labs(title="",
       subtitle="ICU threshold to trigger mitigation (%)",
       x="",
       y="Predicted ICU census\nrelative to capacity (%)", 
       color="Trigger threshold")+
  scale_y_continuous(expand=c(0,0), 
                     breaks=seq(0,180,20), 
                     labels=seq(0,180,20), 
                     lim=c(0,220))+
  geom_hline(yintercept = c(0, Inf)) + 
  geom_vline(xintercept = c(-Inf, Inf)) +
  geom_text(x=as.Date("2020-10-01"), y=110, label="ICU capacity", col=capacity_col)+
  geom_text(data=annotation_dat, 
            aes(x=as.Date("2020-10-01"), y=(80*capacity_multiplier)),
            label="Trigger threshold", col="black")+
  theme(plot.subtitle = element_text(hjust=0.5),
        legend.position="none",
        panel.spacing = unit(1, "lines"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(),
        axis.text.x = element_text(angle=90, vjust=0))+
  facet_wrap(~capacity_multiplier_fct,nrow=1) + 
  customTheme

pplot_top

##---------------------------------
## Prepare B with dates
##---------------------------------
datesDat <- plotdat_sub2 %>% 
  ungroup() %>%
  dplyr::select(exp_name,region,reopen, scen_num,  capacity_multiplier, capacity_multiplier_fct, 
         exceed_date_from,exceed_date_to,  peak_date ,trigger_capacity_date) %>% 
  unique() %>% 
  filter(!is.na(trigger_capacity_date)) %>%
  mutate(diff_trigger_peak= peak_date - trigger_capacity_date,
         diff_trigger_exceed= exceed_date_from - trigger_capacity_date,
         diff_exceed_peak= peak_date - exceed_date_from,
         diff_peak_exceed= exceed_date_to - peak_date)%>%
  unique()

datesDatAggr <- datesDat %>% 
  group_by(exp_name, region, reopen, capacity_multiplier,capacity_multiplier_fct) %>% 
  summarize(peak_date_min=min(peak_date, na.rm=TRUE),
            peak_date_max=max(peak_date, na.rm=TRUE),
            peak_date_mean=mean(peak_date, na.rm=TRUE),
            exceed_date_from_min=min(exceed_date_from, na.rm=TRUE),
            exceed_date_from_max=max(exceed_date_from, na.rm=TRUE),
            exceed_date_from_mean=mean(exceed_date_from, na.rm=TRUE),
            exceed_date_to_min=min(exceed_date_to, na.rm=TRUE),
            exceed_date_to_max=max(exceed_date_to, na.rm=TRUE),
            exceed_date_to_mean=mean(exceed_date_to, na.rm=TRUE),
            trigger_capacity_date_min=min(trigger_capacity_date, na.rm=TRUE),
            trigger_capacity_date_max=max(trigger_capacity_date, na.rm=TRUE),
            trigger_capacity_date_mean=mean(trigger_capacity_date, na.rm=TRUE),
            diff_trigger_peak_min=min(diff_trigger_peak, na.rm=TRUE),
            diff_trigger_peak_max=max(diff_trigger_peak, na.rm=TRUE),
            diff_trigger_peak_mean=mean(diff_trigger_peak, na.rm=TRUE),
            diff_trigger_exceed_min=min(diff_trigger_exceed, na.rm=TRUE),
            diff_trigger_exceed_max=max(diff_trigger_exceed, na.rm=TRUE),
            diff_trigger_exceed_mean=mean(diff_trigger_exceed, na.rm=TRUE),
            diff_exceed_peak_min=min(diff_exceed_peak, na.rm=TRUE),
            diff_exceed_peak_max=max(diff_exceed_peak, na.rm=TRUE),
            diff_exceed_peak_mean=mean(diff_exceed_peak, na.rm=TRUE),
            diff_peak_exceed_min=min(diff_peak_exceed, na.rm=TRUE),
            diff_peak_exceed_max=max(diff_peak_exceed, na.rm=TRUE),
            diff_peak_exceed_mean=mean(diff_peak_exceed, na.rm=TRUE))

datesDat_long <- datesDat %>% 
  dplyr::select(-c(diff_trigger_peak, diff_trigger_exceed, diff_exceed_peak, diff_peak_exceed)) %>% 
  pivot_longer(col=-c(exp_name,region, reopen,  capacity_multiplier,
                      capacity_multiplier_fct, scen_num), 
               names_to="date_type") 

datesDatAggr_long <- datesDatAggr %>% 
  dplyr::select(-c(diff_trigger_peak_max,diff_trigger_peak_min,diff_trigger_peak_mean,
                   diff_trigger_exceed_max,diff_trigger_exceed_min,diff_trigger_exceed_mean,
                   diff_exceed_peak_max,diff_exceed_peak_min,diff_exceed_peak_mean,
                   diff_peak_exceed_max,diff_peak_exceed_min,diff_peak_exceed_mean)) %>% 
  pivot_longer(col=-c(exp_name,region, reopen, capacity_multiplier,capacity_multiplier_fct)) %>%
  mutate(name=gsub("_m","__m",name)) %>%
  separate(name, into=c("date_type","stat"), sep="__") %>%
  pivot_wider(names_from="stat", values_from="value")

unique(sort(datesDatAggr_long$date_type))
unique(sort(datesDat_long$date_type))

lev <- c("trigger_capacity_date", "exceed_date_from","peak_date") 
lab <- c("Trigger\ndate","ICU capacity\ndate", 'Peak\ndate')

datesDatAggr_long$date_type <- factor(datesDatAggr_long$date_type,
                                      levels=lev,
                                      labels=lab)

datesDat_long$date_type <- factor(datesDat_long$date_type,
                                  levels=lev,
                                  labels=lab)

datesDat_long <- datesDat_long %>%
                        group_by(region, capacity_multiplier, exp_name, date_type) %>%
                        add_tally()

tapply(datesDat_long$n, datesDat_long$date_type, summary)

datesDatAggr_long$reopen  <- factor(datesDatAggr_long$reopen, levels=c("100perc","50perc"), labels=c("High","Low"))
datesDat_long$reopen <- factor(datesDat_long$reopen, levels=c("100perc","50perc"), labels=c("High","Low"))

pplot_bottom <- ggplot(data=subset(datesDatAggr_long, !is.na(date_type)))+
  geom_jitter(data=subset(datesDat_long, !is.na(date_type)),
              aes(x=date_type, y=value,  fill=reopen), 
              width=0.08, height=0.01, alpha=0.5,shape=21) +
  geom_pointrange(aes(x=date_type, ymin=min, ymax=max, y=mean, 
                      fill=reopen),shape=21,size=1) +
  coord_flip()+
  facet_grid(reopen~capacity_multiplier_fct)+
  scale_color_manual(values=TwoCols_seq)+
  scale_fill_manual(values=TwoCols_seq)+
  scale_y_date(date_breaks = "30 days",date_minor_breaks = "5 days",
               lim=c(as.Date("2020-09-01"), 
                     as.Date("2020-12-31")), date_labels = "%b %d")+
  labs(x="", y="")+
  customTheme+
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color="grey",size=0.75),
        panel.grid.minor.x = element_line(color="lightgrey",size=0.5),
        axis.ticks.x = element_line(),
        axis.text.x = element_text(angle=90, vjust=0.5))+
  geom_hline(yintercept = c(-Inf, Inf)) + 
  geom_vline(xintercept = c(-Inf, Inf)) 

pplot_bottom


pplot <- plot_grid(pplot_top, pplot_bottom, ncol=1, rel_heights = c(1, 0.85), labels=c("A","B"))

f_save_plot(plot_name=paste0("trajectories_plot_combined_", reopen_val,"_", selected_region), pplot = pplot, 
            plot_dir = file.path(sim_dir, "ICU_trajectories_v2_plots"), width = 12, height = 8)
rm(pplot,pplot_top, pplot_bottom)


##### Duration plot and save dataframe


#### test, calculate probability for all regardless of reopen

plotdat_sub_prob <- plotdat_sub %>%
                    filter(date==max(date)) %>%
                    group_by(region,  capacity_multiplier) %>%
                    add_tally(name="n_all") %>%
                    select(region,  capacity_multiplier, crit_det, avg_resource_available, reopen, n_all, above_yn ) %>%
                    group_by(region,  capacity_multiplier) %>%
                    summarize(n_all=mean(n_all),
                              n_above=sum(above_yn)) %>%
                    mutate(prob=n_above/n_all )
plotdat_sub_prob

plotdat_sub_prob2 <- plotdat_sub %>%
  filter(date==max(date)) %>%
  group_by(region, reopen, capacity_multiplier) %>%
  add_tally(name="n_all") %>%
  select(region,  capacity_multiplier, crit_det, avg_resource_available, reopen, n_all, above_yn ) %>%
  group_by(region,  reopen, capacity_multiplier) %>%
  summarize(n_all=mean(n_all),
            n_above=sum(above_yn)) %>%
  mutate(prob=n_above/n_all )
plotdat_sub_prob2


ggplot(data=plotdat_sub_prob) +
  geom_line(aes(x=capacity_multiplier, y=prob),size=1)+
  geom_line(data=plotdat_sub_prob2, aes(x=capacity_multiplier, y=prob,col=reopen),size=1)+
  geom_point(aes(x=capacity_multiplier, y=prob),size=2, shape=21)+
  geom_point(data=plotdat_sub_prob2, aes(x=capacity_multiplier, y=prob,fill=reopen),size=2, shape=21)+
  scale_x_continuous(breaks=seq(0,1,0.1))+
  scale_color_manual(values=TwoCols_seq)+
  scale_fill_manual(values=TwoCols_seq)+
  customTheme

### test predicted peak
plotdat_sub_bar <- plotdat_sub %>%
  group_by(scen_num, capacity_multiplier, exp_name) %>%
  filter(crit_det==max(crit_det)) 

plotdat_sub_bar_Aggr <- plotdat_sub_bar %>%
  group_by(capacity_multiplier, exp_name,reopen,avg_resource_available) %>%
  summarize(            min.value = min(crit_det, na.rm = TRUE),
                        max.value = max(crit_det, na.rm = TRUE),
                        median.value = median(crit_det, na.rm = TRUE),
                        q25.value = quantile(crit_det, probs = 0.25, na.rm = TRUE),
                        q75.value = quantile(crit_det, probs = 0.75, na.rm = TRUE),
                        q2.5.value = quantile(crit_det, probs = 0.025, na.rm = TRUE),
                        q97.5.value = quantile(crit_det, probs = 0.975, na.rm = TRUE))

ggplot(data=plotdat_sub_bar_Aggr) +
  #geom_ribbon(aes(x=capacity_multiplier, y = median.value, ymin= min.value, ymax = max.value, 
   #               fill=reopen, group=1), alpha=0.3) +
  geom_errorbar(aes(x=capacity_multiplier, y = median.value, ymin= min.value, ymax = max.value, 
                 col=reopen, group=1), alpha=0.5,width=0) +
  geom_line(aes(x=capacity_multiplier, y = median.value, col=reopen),size=1)+
  geom_point(aes(x=capacity_multiplier, y = median.value, fill=reopen),shape=21)+
  geom_hline(aes(yintercept=avg_resource_available))+
  facet_wrap(~reopen)+
  scale_color_manual(values=TwoCols_seq)+
  scale_fill_manual(values=TwoCols_seq)+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(breaks=seq(0,1,0.1))+
  customTheme




}

