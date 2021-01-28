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

#exp_names <- exp_names[(grep("100perc",exp_names))]
#exp_names <- exp_names[!(grepl("counterfactual",exp_names))]
exp_names <- exp_names[!(grepl("7daysdelay",exp_names))]

peakDat_List <- list()
exceedDat_List <- list()
exceed70Dat_List <- list()
triggerDat_List <- list()

for(exp_name in exp_names){
  print(exp_name)
  
  for(region_nr in c(1)){
    
    temp_dat <- f_load_trajectories(sim_dir, exp_name, region_nr = region_nr) %>% 
      filter(date>=as.Date("2020-10-01")) %>% 
      dplyr::select(c(date, scen_num, sample_num, 
                      capacity_multiplier, region, 
                      nsamples, nmultiplier_per_sample, 
                      crit_det)) %>%
      mutate(exp_name=exp_name) 
    
    temp_dat <-  temp_dat %>%
      left_join(capacityDat, by="region") %>%
      group_by(region, exp_name,capacity_multiplier, sample_num, scen_num) %>% 
      mutate(peak=max(crit_det)) %>% 
      mutate(above_yn = ifelse(peak > avg_resource_available & date <= as.Date("2020-12-31"), 1,0))
    
    peakDat <- temp_dat %>% 
      group_by(region, exp_name,capacity_multiplier, sample_num, scen_num) %>% 
      filter(crit_det==peak)  %>%
      mutate(peak_date=date) %>% 
      group_by(region, exp_name,capacity_multiplier, sample_num, scen_num) %>% 
      filter(peak_date==min(peak_date))  %>% 
      dplyr::select(region, exp_name,capacity_multiplier, sample_num, scen_num, peak_date)
    
    
    exceedDat <- temp_dat %>% 
      group_by(region, exp_name,capacity_multiplier, sample_num, scen_num) %>% 
      filter(crit_det >= avg_resource_available)  %>%
      mutate(exceed_date_from=min(date),
             exceed_date_to=max(date),
             diff_exceed_date = as.numeric(exceed_date_to - exceed_date_from)) %>% 
      dplyr::select(region, exp_name,capacity_multiplier, sample_num, scen_num, exceed_date_from , exceed_date_to, diff_exceed_date)%>% 
      unique()
    
    exceed70Dat <- temp_dat %>% 
      group_by(region, exp_name,capacity_multiplier, sample_num, scen_num) %>% 
      filter(crit_det >= avg_resource_available * 0.7)  %>%
      mutate(exceed70_date_from=min(date),
             exceed70_date_to=max(date),
             diff_exceed70_date = as.numeric(exceed70_date_to - exceed70_date_from)) %>% 
      dplyr::select(region, exp_name,capacity_multiplier, sample_num, scen_num, exceed70_date_from ,exceed70_date_to, diff_exceed70_date) %>% 
      unique()
    
    trigger_capacity_Dat <- temp_dat %>% 
      filter(date >=as.Date("2020-10-01") & date <=as.Date("2020-12-31") ) %>%
      dplyr::select(region, exp_name,capacity_multiplier, sample_num, scen_num, avg_resource_available, crit_det,date) %>%
      mutate(trigger_capacity_val =avg_resource_available*capacity_multiplier) %>%
      filter(crit_det > trigger_capacity_val)  %>% 
      group_by(region, exp_name,capacity_multiplier,sample_num, scen_num, avg_resource_available, trigger_capacity_val) %>% 
      filter(date==min(date)) %>%
      rename(trigger_capacity_date=date) %>%
      unique()
    
    peakDat_List[[length(peakDat_List)+1]] <- peakDat
    exceedDat_List[[length(exceedDat_List)+1]] <- exceedDat
    exceed70Dat_List[[length(exceed70Dat_List)+1]] <- exceed70Dat
    triggerDat_List[[length(triggerDat_List)+1]] <- trigger_capacity_Dat
    
    rm(temp_dat)
    
  }
}

peakDat <- peakDat_List %>% bind_rows()
trigger_capacity_Dat <- triggerDat_List %>% bind_rows()
exceed70Dat <- exceed70Dat_List %>% bind_rows()
exceedDat <- exceedDat_List %>% bind_rows()

###--------------------------------
## exceed
###--------------------------------

plotdat_1 <- exceedDat  %>% 
  group_by(exp_name,region, capacity_multiplier) %>% 
  add_tally() %>%
  f_get_scenVars %>%
  filter(delay!="7daysdelay")

tapply(plotdat_1$exceed_date_from, plotdat_1$capacity_multiplier, summary)
tapply(plotdat_1$exceed_date_to, plotdat_1$capacity_multiplier, summary)

###-------------------------------------
plotdat_1_sub <- plotdat_1 %>% 
  ungroup() %>%
  dplyr::select(region,scen_num,rollback, reopen,capacity_multiplier,exceed_date_from, exceed_date_to) %>% 
  pivot_longer(cols=-c(region,scen_num,reopen,rollback,capacity_multiplier))

pplot <- ggplot(data=plotdat_1_sub) +
  geom_violin(aes(x=capacity_multiplier, y=value, fill=name, group=interaction(rollback,name,reopen,capacity_multiplier )))+
  #geom_boxplot(aes(x=capacity_multiplier, y=value, fill=name, group=interaction(region,name,reopen,capacity_multiplier )))+
  geom_point( aes(x=capacity_multiplier, y=value, fill=name, 
                  group=interaction(region,name,reopen,capacity_multiplier )),shape=21,alpha=0.3)+
  geom_hline(yintercept = as.Date("2020-02-01"))+
  facet_grid(rollback~reopen)+
  coord_flip()+
  customTheme
pplot

f_save_plot(plot_name=paste0("trajectories_exceed_duration_draft_region_1"), pplot = pplot, 
            plot_dir = file.path(sim_dir, "ICU_trajectories_plots"), width = 14, height = 8)
rm(pplot)
###-------------------------------------

plotdat_1_sub <- plotdat_1 %>% 
  filter() %>%
  ungroup() %>%
  dplyr::select(region,scen_num,rollback, reopen,capacity_multiplier,diff_exceed_date) %>% 
  pivot_longer(cols=-c(region,scen_num,rollback,reopen,capacity_multiplier))

pplot <- ggplot(data=plotdat_1_sub) +
  geom_violin(aes(x=rollback, y=value, fill=name, group=interaction(rollback,name,reopen,capacity_multiplier )))+
  #geom_boxplot(aes(x=capacity_multiplier, y=value, fill=name, group=interaction(region,name,reopen,capacity_multiplier )))+
  geom_point( aes(x=rollback, y=value, fill=name, 
                  group=interaction(region,name,reopen,capacity_multiplier )),shape=21,alpha=0.3)+
  facet_grid(capacity_multiplier~reopen)+
  coord_flip()+
  customTheme
pplot


f_save_plot(plot_name=paste0("trajectories_exceed_duration_draft2_region_1"), pplot = pplot, 
            plot_dir = file.path(sim_dir, "ICU_trajectories_plots"), width = 14, height = 12)
rm(pplot)
###-------------------------------------



tapply(plotdat_1$n, plotdat_1$capacity_multiplier, summary)
plotdat_1$capacity_multiplier_fct3 <- NA
plotdat_1$capacity_multiplier_fct3[plotdat_1$capacity_multiplier<=0.4] <- "<40"
plotdat_1$capacity_multiplier_fct3[plotdat_1$capacity_multiplier<=0.8] <- "<80"
plotdat_1$capacity_multiplier_fct3[plotdat_1$capacity_multiplier>0.8] <- ">80"

summary(plotdat_1$n)
plotdat_1 %>% filter(n<=5)

plotdatAggr_1 <- plotdat_1 %>% 
  group_by(exp_name, region, capacity_multiplier)  %>% 
  #mutate(diff_exceed_date = ifelse(n <10, 0, diff_exceed_date)) %>%
  sample_n(100, replace=TRUE) %>%
  group_by(exp_name, region, capacity_multiplier)  %>% 
  summarize(nsamples=n_distinct(sample_num),
            diff_exceed_date_lower =  quantile(diff_exceed_date, probs = 0.25, na.rm = TRUE) ,
            diff_exceed_date_mean = mean(diff_exceed_date, na.rm = TRUE),
            diff_exceed_date_upper =  quantile(diff_exceed_date, probs = 0.75, na.rm = TRUE))%>%
  f_get_scenVars

summary(plotdatAggr_1$nsamples)


plotdat_1 <- subset(plotdat_1,( sample_num %in% sall) | ( rollback =="counterfactual"))


pplot_by_capacity <- 
  ggplot(data=subset(plotdat_1, region=="Region 1" & rollback %in% c("pr4")))+
  geom_jitter(aes(x=as.factor(capacity_multiplier), 
                  y=diff_exceed_date, 
                  fill=reopen,
                  col=reopen),
              alpha=0.6, shape=21,width=0.1)  +
  scale_shape_manual(values=c(1,21,21))+
  #scale_fill_brewer(palette = "Dark2")+
  scale_color_manual(values=TwoCols_seq)+
  scale_fill_manual(values=TwoCols_seq)+
  customTheme+
  #facet_wrap(~reopen, ncol=2)+
  scale_y_continuous(lim=c(0,120), breaks=seq(0,120,20), minor_breaks = seq(0,120,10), expand=c(0,0))+
  theme(panel.grid.major.x = element_blank())+
  labs(y="Days above ICU capacity", x="Mitigation effectiveness")
pplot_by_capacity


pplot_by_capacity <- 
  ggplot(data=subset(plotdat_1, region=="Region 1" & 
                       capacity_multiplier %in% c(0, 0.2, 0.4, 0.6, 0.8, 1) &
                       diff_exceed_date>0 & rollback %in% c("pr4")))+
  geom_histogram(aes(x=diff_exceed_date, 
                     fill=reopen,
                     col=reopen), bins = 60)  +
  scale_color_manual(values=TwoCols_seq)+
  scale_fill_manual(values=TwoCols_seq)+
  customTheme+
  facet_wrap(~capacity_multiplier, nrow=1)+
  theme(panel.grid.major.x = element_blank())+
  labs(y="Frquency exceeding capacity", x="Trigger threshold")
pplot_by_capacity



pplot_with_trajectories_1 <- 
  ggplot(data=subset(plotdat_1, region=="Region 1"))+
  geom_jitter(aes(x=as.factor(rollback), 
                  y=diff_exceed_date, 
                  fill=reopen,
                  col=reopen),
              alpha=0.6, shape=21,width=0.1)  +
  geom_pointrange(data=subset(plotdatAggr_1, region=="Region 1"), 
                  aes(x=as.factor(rollback),
                      y=diff_exceed_date_mean, 
                      ymin=diff_exceed_date_lower, 
                      ymax=diff_exceed_date_upper,
                      fill=reopen), shape=21,size=0.7)  +
  facet_wrap(~reopen, ncol=2)+
  scale_shape_manual(values=c(1,21,21))+
  #scale_fill_brewer(palette = "Dark2")+
  scale_color_manual(values=TwoCols_seq)+
  scale_fill_manual(values=TwoCols_seq)+
  customTheme+
  scale_y_continuous(lim=c(0,120), breaks=seq(0,120,20), minor_breaks = seq(0,120,10), expand=c(0,0))+
  theme(panel.grid.major.x = element_blank())+
  labs(y="Days above ICU capacity", x="Mitigation effectiveness")

pplot_with_trajectories_1

###--------------------------------
##  diff_trigger_peak
###--------------------------------

plotdat_2 <- peakDat %>% 
  f_addVar(trigger_capacity_Dat) %>%
  group_by(exp_name, region, scen_num) %>%
  mutate(diff_trigger_peak = as.numeric(peak_date - trigger_capacity_date )) %>%
  f_get_scenVars


plotdatAggr_2 <- plotdat_2 %>% 
  group_by(exp_name, region)  %>% 
  #mutate(diff_trigger_peak = ifelse(n <10, 0, diff_trigger_peak)) %>%
  summarize(diff_trigger_peak_lower =  quantile(diff_trigger_peak, probs = 0.25, na.rm = TRUE) ,
            diff_trigger_peak_mean = mean(diff_trigger_peak, na.rm = TRUE),
            diff_trigger_peak_upper =  quantile(diff_trigger_peak, probs = 0.75, na.rm = TRUE))%>%
  f_get_scenVars

sall <- c( 10  ,11 , 12  ,14 , 17,  18 , 28 , 37 , 43 , 45  ,52 , 55  ,56  ,60 , 79  ,83 , 85 , 95 ,103, 109 ,110 ,123, 126,
           127 ,135 ,137, 138, 139, 140, 142, 143, 152, 157, 169, 171, 174 ,178 ,180, 194,
           221 ,223 ,224, 228, 229, 231, 233, 237, 238 ,240, 259, 260 ,277 ,294 ,309 ,314, 319, 323 ,325,328 ,329 ,330, 
           334, 335, 336 ,351, 360, 365, 373, 374, 387 ,388, 389, 390, 392, 398)

plotdat_2 <- subset(plotdat_2,( sample_num %in% sall) | ( rollback =="counterfactual"))

pplot_with_trajectories_2 <- 
  ggplot(data=subset(plotdat_2, region =="Region 1"))+
  geom_jitter(aes(x=as.factor(rollback), 
                  y=diff_trigger_peak, 
                  fill=reopen,
                  col=reopen),
              alpha=0.6,shape=21, width=0.1)  +
  geom_pointrange(data=subset(plotdatAggr_2, region =="Region 1") , 
                  aes(x=as.factor(rollback),
                      y=diff_trigger_peak_mean, 
                      ymin=diff_trigger_peak_lower, 
                      ymax=diff_trigger_peak_upper,
                      fill=reopen), shape=21,size=0.7)  +
  facet_wrap(~reopen, ncol=2)+
  scale_shape_manual(values=c(1,21,21))+
  #scale_fill_brewer(palette = "Dark2")+
  scale_color_manual(values=TwoCols_seq)+
  scale_fill_manual(values=TwoCols_seq)+
  customTheme+
  scale_y_continuous(lim=c(0,150), breaks=seq(0,150,20), minor_breaks = seq(0,150,10), expand=c(0,0))+
  theme(panel.grid.major.x = element_blank())+
  labs(y="Days after trigger\nuntil peak in  ICU census", x="Mitigation effectiveness")

pplot_with_trajectories_2

###--------------------------------
##  trigger_to_exceed70_to
###--------------------------------

plotdat_3 <- exceed70Dat %>% 
  f_addVar(trigger_capacity_Dat) %>%
  group_by(exp_name,region,  scen_num) %>%
  mutate(trigger_to_exceed70_to = as.numeric(exceed70_date_to - trigger_capacity_date) ) %>%
  group_by(exp_name,region, capacity_multiplier) %>%
  add_tally()%>%
  f_get_scenVars

plotdatAggr_3 <- plotdat_3 %>% 
  group_by(exp_name, region )  %>% 
  #mutate(diff_trigger_peak = ifelse(n <10, 0, diff_trigger_peak)) %>%
  summarize(trigger_to_exceed70_to_lower =  quantile(trigger_to_exceed70_to, probs = 0.25, na.rm = TRUE) ,
            trigger_to_exceed70_to_mean = mean(trigger_to_exceed70_to, na.rm = TRUE),
            trigger_to_exceed70_to_upper =  quantile(trigger_to_exceed70_to, probs = 0.75, na.rm = TRUE))%>%
  f_get_scenVars

sall <- c( 10  ,11 , 12  ,14 , 17,  18 , 28 , 37 , 43 , 45  ,52 , 55  ,56  ,60 , 79  ,83 , 85 , 95 ,103, 109 ,110 ,123, 126,
           127 ,135 ,137, 138, 139, 140, 142, 143, 152, 157, 169, 171, 174 ,178 ,180, 194,
           221 ,223 ,224, 228, 229, 231, 233, 237, 238 ,240, 259, 260 ,277 ,294 ,309 ,314, 319, 323 ,325,328 ,329 ,330, 
           334, 335, 336 ,351, 360, 365, 373, 374, 387 ,388, 389, 390, 392, 398)

plotdat_3 <- subset(plotdat_3,( sample_num %in% sall) | ( rollback =="counterfactual"))

pplot_with_trajectories_3 <- 
  ggplot(data=subset(plotdat_3, region=="Region 1"))+
  geom_jitter(aes(x=as.factor(rollback), 
                  y=trigger_to_exceed70_to, 
                  fill=reopen,
                  col=reopen),
              alpha=0.6,shape=21, width=0.1)  +
  geom_pointrange(data=subset(plotdatAggr_3, region=="Region 1"), 
                  aes(x=as.factor(rollback),
                      y=trigger_to_exceed70_to_mean, 
                      ymin=trigger_to_exceed70_to_lower, 
                      ymax=trigger_to_exceed70_to_upper,
                      fill=reopen), shape=21,size=0.7)  +
  facet_wrap(~reopen, ncol=2)+
  scale_shape_manual(values=c(1,21,21))+
  #scale_fill_brewer(palette = "Dark2")+
  scale_color_manual(values=TwoCols_seq)+
  scale_fill_manual(values=TwoCols_seq)+
  customTheme+
  scale_y_continuous(lim=c(0,180), breaks=seq(0,180,20), minor_breaks = seq(0,180,10), expand=c(0,0))+
  theme(panel.grid.major.x = element_blank())+
  labs(y="Days after trigger\nuntil ICU census decreased to 70%", x="Mitigation effectiveness")

pplot_with_trajectories_3


f_save_plot(plot_name=paste0("pplot_with_trajectories_1_reg1"), pplot = pplot_with_trajectories_1, 
            plot_dir = file.path(sim_dir, "ICU_duration_plots"), width = 12, height = 8)

f_save_plot(plot_name=paste0("pplot_with_trajectories_2_reg1"), pplot = pplot_with_trajectories_2, 
            plot_dir = file.path(sim_dir, "ICU_duration_plots"), width = 12, height = 8)

f_save_plot(plot_name=paste0("pplot_with_trajectories_3_reg1"), pplot = pplot_with_trajectories_3, 
            plot_dir = file.path(sim_dir, "ICU_duration_plots"), width = 12, height = 8)



pplot <- plot_grid(pplot_with_trajectories_1, pplot_with_trajectories_3, ncol=1)


rm(pplot_with_trajectories_1 , pplot_with_trajectories_2, pplot_with_trajectories_3)


##--------------------------------
##### Aggregated only
##--------------------------------

plotdat_1$capacity_multiplier_grp <- NA
plotdat_1$capacity_multiplier_grp[plotdat_1$capacity_multiplier <= 0.8] <- "<= 80%"
plotdat_1$capacity_multiplier_grp[plotdat_1$capacity_multiplier > 0.8] <- "> 80%"

plotdat_3$capacity_multiplier_grp <- NA
plotdat_3$capacity_multiplier_grp[plotdat_3$capacity_multiplier <= 0.8] <- "<= 80%"
plotdat_3$capacity_multiplier_grp[plotdat_3$capacity_multiplier > 0.8] <- "> 80%"


plotdatAggr_1 <- plotdat_1 %>% 
  group_by(exp_name,region, capacity_multiplier)  %>% 
  summarize(diff_exceed_date_lower =  quantile(diff_exceed_date, probs = 0.25, na.rm = TRUE) ,
            diff_exceed_date_mean = mean(diff_exceed_date, na.rm = TRUE),
            diff_exceed_date_upper =  quantile(diff_exceed_date, probs = 0.75, na.rm = TRUE),
            diff_exceed_date_ci_lower =  quantile(diff_exceed_date, probs = 0.025, na.rm = TRUE),
            diff_exceed_date_ci_upper =  quantile(diff_exceed_date, probs = 0.975, na.rm = TRUE))%>%
  f_get_scenVars

plotdatAggr_3 <- plotdat_3 %>% 
  group_by(exp_name,region, capacity_multiplier )  %>% 
  summarize(trigger_to_exceed70_to_lower =  quantile(trigger_to_exceed70_to, probs = 0.25, na.rm = TRUE) ,
            trigger_to_exceed70_to_mean = mean(trigger_to_exceed70_to, na.rm = TRUE),
            trigger_to_exceed70_to_upper =  quantile(trigger_to_exceed70_to, probs = 0.75, na.rm = TRUE),
            trigger_to_exceed70_to_ci_lower =  quantile(trigger_to_exceed70_to, probs = 0.025, na.rm = TRUE),
            trigger_to_exceed70_to_ci_upper =  quantile(trigger_to_exceed70_to, probs = 0.975, na.rm = TRUE)
  )%>%
  f_get_scenVars


pplot_top <- 
  ggplot(data=subset(plotdat_1, region =="Region 1"))+
  #geom_violin(data=subset(plotdat_1, region =="Region 1") , 
  #                aes(x=as.factor(rollback),
  #                    y=diff_exceed_date,
  #                    fill=reopen),size=0.7)  +
  geom_pointrange(data=subset(plotdatAggr_1, region =="Region 1") , 
                  aes(x=as.factor(capacity_multiplier),
                      y=diff_exceed_date_mean, 
                      ymin=diff_exceed_date_ci_lower, 
                      ymax=diff_exceed_date_ci_upper,
                      fill=reopen, group=reopen), position = position_dodge(width=0.3), shape=21,size=0.7)  +
  scale_shape_manual(values=c(1,21,21))+
  #scale_fill_brewer(palette = "Dark2")+
  scale_color_manual(values=TwoCols_seq)+
  scale_fill_manual(values=TwoCols_seq)+
  customTheme+
  scale_y_continuous(lim=c(0,180), breaks=seq(0,180,20), minor_breaks = seq(0,180,10), expand=c(0,0))+
  theme(panel.grid.major.x = element_blank())+
  labs(y="Days above ICU capacity", x="Mitigation effectiveness")+
  facet_wrap(~rollback)

pplot_top


pplot_bottom <- 
  ggplot(data=subset(plotdat_3, region =="Region 1"))+
  geom_pointrange(data=subset(plotdatAggr_3, region =="Region 1") , 
                  aes(x=as.factor(rollback),
                      y=trigger_to_exceed70_to_mean, 
                      ymin=trigger_to_exceed70_to_ci_lower, 
                      ymax=trigger_to_exceed70_to_ci_upper,
                      fill=reopen, group=capacity_multiplier_grp), position = position_dodge(width=0.3), shape=21,size=0.7)  +
  scale_shape_manual(values=c(1,21,21))+
  #scale_fill_brewer(palette = "Dark2")+
  scale_color_manual(values=TwoCols_seq)+
  scale_fill_manual(values=TwoCols_seq)+
  customTheme+
  scale_y_continuous(lim=c(0,180), breaks=seq(0,180,20), minor_breaks = seq(0,180,10), expand=c(0,0))+
  theme(panel.grid.major.x = element_blank())+
  labs(y="Days after trigger\nuntil ICU census decreased to 70%", x="Mitigation effectiveness")+
  facet_wrap(~reopen)

pplot_bottom

pplot <- plot_grid(pplot_top, pplot_bottom, ncol=1, labels=c("A","B"))


f_save_plot(plot_name=paste0("trajectories_duration_by_capacity_grp"), pplot = pplot, 
            plot_dir = file.path(sim_dir, "ICU_duration_plots"), width = 12, height = 10)


