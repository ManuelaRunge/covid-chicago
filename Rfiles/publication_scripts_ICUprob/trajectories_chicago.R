### Describe Rt
library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

theme_set(theme_minimal())
simcolor <- "#F6921E"
capacitycolor <- "dodgerblue2"
customTheme <- f_getCustomTheme()


simdate <- "20201212"
sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (!dir.exists(file.path(sim_dir, "ICU_Rt_plots"))) dir.create(file.path(sim_dir, "ICU_Rt_plots"))


exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c( grep("counterfactual", exp_names))]


dat1 <- fread(file.path(sim_dir, exp_names[1],"trajectoriesDat_region_11.csv")) %>% select(time,startdate,scen_num, `Ki_t_EMS-11`,`crit_det_EMS-11`)%>% mutate(reopen="100perc")
dat2 <- fread(file.path(sim_dir, exp_names[2],"trajectoriesDat_region_11.csv")) %>% select(time,startdate,scen_num, `Ki_t_EMS-11`,`crit_det_EMS-11`)%>% mutate(reopen="50perc")

dat <- rbind(dat1,dat2) %>% mutate(date = as.Date(startdate)+time) %>% filter(date>=as.Date("2020-09-01"))
rm(dat1, dat2)
colnames(dat) <- gsub("_EMS-11","",colnames(dat) )

peakDat <- dat %>% group_by(reopen, scen_num) %>% filter(crit_det==max(crit_det))
capacityDat <- load_new_capacity(11, filedate="20200915")

pplot <- ggplot(data=subset(dat,date <= as.Date("2020-12-31") )) + 
  geom_line(aes(x=date, y=crit_det, group=interaction(scen_num, reopen), col=reopen),alpha=0.5) +
  geom_hline(yintercept = 516, linetype="dashed",col="#be1e2d")+
  scale_color_manual(values=c("#5c5859","#fdbf11")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  customTheme+
  labs(x="",y="ICU occupancy",color="")+
  theme(legend.position = "none")+
  theme_minimal()

f_save_plot(
  plot_name = paste0("counterfactual_reg11"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_chicago_plots"), width =8, height = 4
)


################################
### Scenarios
################################

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c( !grepl("counterfactual", exp_names))]
exp_names <- exp_names[c( !grepl("50perc", exp_names))]
exp_names <- exp_names[c( !grepl("_reopen", exp_names))]
exp_names <- exp_names[c( !grepl("_7daysdelay", exp_names))]

dat <- fread(file.path(sim_dir, exp_names[3],"trajectoriesDat_region_11.csv")) %>% 
  select(time,startdate,scen_num, capacity_multiplier,`Ki_t_EMS-11`,`crit_det_EMS-11`)%>% 
  mutate(exp_name=exp_names[3])
dat <- dat %>% 
  mutate(date = as.Date(startdate)+time) %>% 
  filter(date>=as.Date("2020-09-01"))

colnames(dat) <- gsub("_EMS-11","",colnames(dat) )

peakDat <- dat %>% group_by( scen_num,capacity_multiplier) %>% filter(crit_det==max(crit_det))
capacityDat <- load_new_capacity(11, filedate="20200915")

datAggr <- dat %>% group_by(date, capacity_multiplier) %>% summarize(crit_det=median(crit_det))

pplot <- ggplot(data=subset(dat,date <= as.Date("2020-12-31") & capacity_multiplier %in% c(0,0.2,0.4, 0.6, 0.8,1) )) + 
  geom_line(aes(x=date, y=crit_det, group=interaction(scen_num, capacity_multiplier), 
                col=as.factor(capacity_multiplier)),alpha=0.08) +
  geom_line(data=subset(datAggr,date <= as.Date("2020-12-31") & capacity_multiplier %in% c(0,0.2,0.4, 0.6, 0.8,1)  ),
            aes(x=date, y=crit_det, group=interaction(capacity_multiplier),
                col=as.factor(capacity_multiplier)),alpha=1,size=1.2) +
  geom_hline(yintercept = 516, linetype="dashed",col="#be1e2d") +
  scale_color_manual(values=c("#fa9fb5","#f768a1","#dd3497","#e7298a","#7a0177","#49006a")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  customTheme+
  labs(x="",y="ICU occupancy",color="")+
  theme(legend.position = "none")+
  theme_minimal()


f_save_plot(
  plot_name = paste0("scenarios_reg11"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_chicago_plots"), width =6, height = 3
)


peakDatAggr <-  datAggr %>% group_by( capacity_multiplier) %>% filter(crit_det==max(crit_det))
  
  
pplot <- ggplot(data=subset(dat,date <= as.Date("2020-12-31") & capacity_multiplier %in% c(0,0.2,0.4, 0.6, 0.8,1) )) + 
  geom_line(data=subset(datAggr,date <= as.Date("2020-12-31") & capacity_multiplier %in% c(0,0.2,0.4, 0.6, 0.8,1)  ),
            aes(x=date, y=crit_det, group=interaction(capacity_multiplier),
                col=as.factor(capacity_multiplier)),alpha=1,size=1.2) +
  geom_point(data=subset(peakDatAggr,date <= as.Date("2020-12-31") & capacity_multiplier %in% c(0,0.2,0.4, 0.6, 0.8,1)  ),
            aes(x=date, y=crit_det, group=interaction(capacity_multiplier),
                col=as.factor(capacity_multiplier)),alpha=1,size=4) +
  geom_hline(yintercept = 516, linetype="dashed",col="#be1e2d") +
  scale_color_manual(values=c("#fa9fb5","#f768a1","#dd3497","#e7298a","#7a0177","#49006a")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  customTheme+
  labs(x="",y="ICU occupancy",color="")+
  theme(legend.position = "none")+
  theme_minimal()


f_save_plot(
  plot_name = paste0("scenarios_peak_reg11"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_chicago_plots"),  width =10, height =6
)


peakDat$peak_date <- peakDat$date
dat <- dat %>%left_join(peakDat[,c("scen_num","peak_date")])

pplot <- ggplot(data=subset(dat,date <= as.Date("2020-12-31")& date <=peak_date & capacity_multiplier %in% c(0,0.2,0.4, 0.6, 0.8,1) )) + 
  geom_line(aes(x=date, y=crit_det, group=interaction(scen_num, capacity_multiplier), 
                col=as.factor(capacity_multiplier)),alpha=0.08) +
  #geom_line(aes(x=date, y=crit_det, group=interaction(scen_num, capacity_multiplier), 
  #              col=as.factor(capacity_multiplier)),alpha=0.08) +
  geom_point(data=subset(peakDat,date >= as.Date("2020-09-30") & date <= as.Date("2020-12-31") & capacity_multiplier %in% c(0,0.2,0.4, 0.6, 0.8,1)  ),
             aes(x=date, y=crit_det, group=interaction(capacity_multiplier),
                 fill=as.factor(capacity_multiplier)),size=2.5,shape=21,alpha=0.9) +
  geom_hline(yintercept = 516, linetype="dashed",col="#be1e2d") +
  scale_color_manual(values=c("#fa9fb5","#f768a1","#dd3497","#e7298a","#7a0177","#49006a")) +
  scale_fill_manual(values=c("#fa9fb5","#f768a1","#dd3497","#e7298a","#7a0177","#49006a")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  customTheme+
  labs(x="",y="ICU occupancy",color="")+
  theme(legend.position = "none")+
  theme_minimal()


f_save_plot(
  plot_name = paste0("scenarios_prob_reg11"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_chicago_plots"), width =10, height = 6
)




##############################
## Scenario with counterfactual
##############################


exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c( !grepl("50perc", exp_names))]
exp_names <- exp_names[c( !grepl("_reopen", exp_names))]
exp_names <- exp_names[c( !grepl("_7daysdelay", exp_names))]


dat1 <- fread(file.path(sim_dir, exp_names[3],"trajectoriesDat_region_11.csv")) %>% select(time,startdate,scen_num,capacity_multiplier, `Ki_t_EMS-11`,`crit_det_EMS-11`)%>% mutate(exp_name=exp_names[3],scen="pr60")
dat2 <- fread(file.path(sim_dir, exp_names[5],"trajectoriesDat_region_11.csv")) %>% select(time,startdate,scen_num,capacity_multiplier, `Ki_t_EMS-11`,`crit_det_EMS-11`)%>% mutate(exp_name=exp_names[5],scen="counterfactual")

dat <- rbind(dat1,dat2) %>% mutate(date = as.Date(startdate)+time) %>% filter(date>=as.Date("2020-09-01"))
rm(dat1, dat2)
colnames(dat) <- gsub("_EMS-11","",colnames(dat) )

peakDat <- dat %>% group_by(scen,capacity_multiplier, scen_num) %>% filter(crit_det==max(crit_det))
capacityDat <- load_new_capacity(11, filedate="20200915")


datAggr <- dat %>% group_by(date, scen,capacity_multiplier) %>% summarize(crit_det=median(crit_det))

pplot <- ggplot() + 
  geom_line(data=subset(dat,scen =="counterfactual" & date <= as.Date("2020-12-31")  ),
            aes(x=date, y=crit_det, group=interaction(scen_num, capacity_multiplier)),alpha=0.2,col="#5c5859") +
  geom_line(data=subset(datAggr, scen =="counterfactual" & date <= as.Date("2020-12-31")),
            aes(x=date, y=crit_det, group=interaction(capacity_multiplier)),alpha=1,size=1.2,col="#5c5859") +
  geom_line(data=subset(dat,scen !="counterfactual" & date <= as.Date("2020-12-31") & capacity_multiplier %in% c(0,0.2,0.4, 0.6, 0.8,1) ),
            aes(x=date, y=crit_det, group=interaction(scen_num, capacity_multiplier), 
                col=as.factor(capacity_multiplier)),alpha=0.08) +
  geom_line(data=subset(datAggr, scen !="counterfactual" & date <= as.Date("2020-12-31") & capacity_multiplier %in% c(0,0.2,0.4, 0.6, 0.8,1)  ),
            aes(x=date, y=crit_det, group=interaction(capacity_multiplier),
                col=as.factor(capacity_multiplier)),alpha=1,size=1.2) +
  geom_hline(yintercept = 516, linetype="dashed",col="#be1e2d") +
  scale_color_manual(values=c("#fa9fb5","#f768a1","#dd3497","#e7298a","#7a0177","#49006a")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  customTheme+
  labs(x="",y="ICU occupancy",color="")+
  theme(legend.position = "none")+
  theme_minimal()
pplot



f_save_plot(
  plot_name = paste0("scenarios_with_counterfactual_reg11"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_chicago_plots"), width =6, height = 3
)

datAggr  %>%ungroup() %>% filter(date <=as.Date("2020-12-31")) %>%
  group_by(scen, capacity_multiplier )%>%
  filter(date ==max(date))


######## Trajectories by mitigation
peakDat$peak_date <- peakDat$date
dat <- dat %>%left_join(peakDat[,c("scen_num","peak_date")])
dat_trigger <- dat %>% filter(scen!="counterfactual" & capacity_multiplier %in% c(0.4, 0.6, 0.8,1)) %>%
  select(scen,capacity_multiplier) %>% unique() %>% mutate(capacity=516)

pplot <- ggplot() + 
  geom_line(data=subset(dat,scen !="counterfactual" & date >=peak_date & date <= as.Date("2020-12-31") & capacity_multiplier %in% c(0.4, 0.6, 0.8,1) ),
            aes(x=date, y=crit_det, group=interaction(scen_num, capacity_multiplier)),col="grey",alpha=0.2) +
  geom_line(data=subset(dat,scen !="counterfactual" & date <=peak_date & date <= as.Date("2020-12-31") & capacity_multiplier %in% c(0.4, 0.6, 0.8,1) ),
            aes(x=date, y=crit_det, group=interaction(scen_num, capacity_multiplier), 
                col=as.factor(capacity_multiplier)),alpha=0.3) +
  geom_point(data=subset(peakDat,scen !="counterfactual" & date <= as.Date("2020-12-31") & 
                           capacity_multiplier %in% c(0.4, 0.6, 0.8,1) ),
            aes(x=date, y=crit_det, group=interaction(scen_num, capacity_multiplier), 
                fill=as.factor(capacity_multiplier)),alpha=0.8,shape=21) +
  #geom_line(data=subset(datAggr, scen !="counterfactual" & date <= as.Date("2020-12-31") & capacity_multiplier %in% c(0.4, 0.6, 0.8,1)  ),
  #          aes(x=date, y=crit_det, group=interaction(capacity_multiplier),
  #              col=as.factor(capacity_multiplier)),alpha=1,size=1.2) +
  geom_hline(data=dat_trigger,aes(yintercept = capacity*capacity_multiplier), linetype="longdash",col="black",size=1.3) +
  geom_hline(yintercept = 516, linetype="dashed",col="#be1e2d",size=1.3) +
  scale_color_manual(values=c("#dd3497","#e7298a","#7a0177","#49006a")) +
  scale_fill_manual(values=c("#dd3497","#e7298a","#7a0177","#49006a")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  customTheme+
  labs(x="",y="ICU occupancy",color="")+
  theme(legend.position = "none")+
  theme_minimal()+
  facet_wrap(~capacity_multiplier ,nrow=1)
pplot

f_save_plot(
  plot_name = paste0("scenarios_facets_reg11"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_chicago_plots"), width =10, height =4
)




peakDat2 <- dat %>% filter(scen!="counterfactual"  & date <= as.Date("2020-12-31") & capacity_multiplier %in% c(0.4, 0.6, 0.8,1)) %>%
  group_by(scen,capacity_multiplier, scen_num) %>% filter(crit_det==max(crit_det)) %>% 
  unique() %>% 
  mutate(date_type="peak_date") 
triggerDat2 <-  dat %>% filter(scen!="counterfactual"  & date <= as.Date("2020-12-31")& capacity_multiplier %in% c(0.4, 0.6, 0.8,1)) %>% 
  ungroup() %>% select(-peak_date) %>% unique() %>%
                      group_by(scen,capacity_multiplier, scen_num) %>% 
                      filter(crit_det>=516*capacity_multiplier) %>% 
                      filter(date==min(date)) %>% mutate(date_type="trigger_date") 

peakDat2 <- peakDat2[,c("scen_num",'capacity_multiplier',"date_type",'date')]
triggerDat2 <- triggerDat2[,c("scen_num",'capacity_multiplier',"date_type",'date')]

dateDat <- rbind(peakDat2,triggerDat2)
dateDat$date <- as.Date(dateDat$date)
dateDatAggr <- dateDat %>% 
  group_by(date_type,capacity_multiplier)%>% 
  summarise(date_min=min(date), date_max=max(date), date_median = median(date))

dateDat$date_type <- factor(dateDat$date_type, levels=c("trigger_date","peak_date"), labels=c("trigger_date","peak_date"))
dateDatAggr$date_type <- factor(dateDatAggr$date_type, levels=c("trigger_date","peak_date"), labels=c("trigger_date","peak_date"))

dateDatAggr %>% pivot_wider(names_from = "date_type", values_from=c("date_min","date_median","date_max")) %>%
            group_by(capacity_multiplier ) %>%
            mutate(date_median_diff = date_median_peak_date  -date_median_trigger_date   ) %>%
            select(capacity_multiplier,date_median_diff)

pplot <- ggplot(data=dateDat) +
  geom_jitter(aes(y=date,x=date_type, fill=as.factor(capacity_multiplier) ), width =  0.08, height =0,alpha=0.3, shape=21)+
  geom_pointrange(data=dateDatAggr,aes(y=date_median, ymin=date_min, ymax=date_max,x=date_type, fill=as.factor(capacity_multiplier) ),size=1,shape=21)+
  theme_minimal()+
  scale_y_date(date_breaks = "1 month", date_labels = "%b", lim=c(as.Date("2020-09-01"), as.Date("2020-12-31"))) +
  facet_wrap(~capacity_multiplier ,nrow=1)+
  scale_color_manual(values=c("#dd3497","#e7298a","#7a0177","#49006a")) +
  scale_fill_manual(values=c("#dd3497","#e7298a","#7a0177","#49006a")) +
  coord_flip()

f_save_plot(
  plot_name = paste0("scenarios_facets_dates_reg11"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_chicago_plots"), width =10, height =4
)


##########################################
####### Mitigation strengths 100 perc
##########################################

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c( !grepl("50perc", exp_names))]
exp_names <- exp_names[c( !grepl("_reopen", exp_names))]
exp_names <- exp_names[c( !grepl("_7daysdelay", exp_names))]


dat1 <- fread(file.path(sim_dir, exp_names[1],"trajectoriesDat_region_11.csv")) %>% select(time,startdate,scen_num,capacity_multiplier, `Ki_t_EMS-11`,`crit_det_EMS-11`)%>% mutate(exp_name=exp_names[1],scen="pr2")
dat2 <- fread(file.path(sim_dir, exp_names[2],"trajectoriesDat_region_11.csv")) %>% select(time,startdate,scen_num,capacity_multiplier, `Ki_t_EMS-11`,`crit_det_EMS-11`)%>% mutate(exp_name=exp_names[2],scen="pr4")
dat3 <- fread(file.path(sim_dir, exp_names[3],"trajectoriesDat_region_11.csv")) %>% select(time,startdate,scen_num,capacity_multiplier, `Ki_t_EMS-11`,`crit_det_EMS-11`)%>% mutate(exp_name=exp_names[3],scen="pr6")
dat4 <- fread(file.path(sim_dir, exp_names[4],"trajectoriesDat_region_11.csv")) %>% select(time,startdate,scen_num,capacity_multiplier, `Ki_t_EMS-11`,`crit_det_EMS-11`)%>% mutate(exp_name=exp_names[4],scen="pr8")
dat5 <- fread(file.path(sim_dir, exp_names[5],"trajectoriesDat_region_11.csv")) %>% select(time,startdate,scen_num,capacity_multiplier, `Ki_t_EMS-11`,`crit_det_EMS-11`)%>% mutate(exp_name=exp_names[5],scen="counterfactual")

dat <- rbind(dat1,dat2,dat3, dat4, dat5) %>%
  mutate(date = as.Date(startdate)+time) %>%
  filter(date>=as.Date("2020-09-01") &
           date <= as.Date("2020-12-31"))


rm(dat1,dat2,dat3, dat4, dat5)
colnames(dat) <- gsub("_EMS-11","",colnames(dat) )
capacityDat <- load_new_capacity(11, filedate="20200915")


dat <- dat %>% group_by(capacity_multiplier, exp_name, scen, scen_num) %>% filter(crit_det >= 516 *capacity_multiplier )


dat <- dat %>% f_get_scenVars()

dat_sub <- dat %>% 
  filter(scen!="counterfactual") %>% 
  group_by(capacity_multiplier, exp_name, scen, scen_num,rollback_fct) %>% 
  filter(crit_det ==max(crit_det))


dat_subAggr <- dat_sub %>% group_by(capacity_multiplier, exp_name, scen,rollback_fct) %>% 
  summarize(            min.value = min(crit_det, na.rm = TRUE),
                        max.value = max(crit_det, na.rm = TRUE),
                        median.value = median(crit_det, na.rm = TRUE),
                        q25.value = quantile(crit_det, probs = 0.25, na.rm = TRUE),
                        q75.value = quantile(crit_det, probs = 0.75, na.rm = TRUE),
                        q2.5.value = quantile(crit_det, probs = 0.025, na.rm = TRUE),
                        q97.5.value = quantile(crit_det, probs = 0.975, na.rm = TRUE))

dat_sub_counterfactual <- dat %>% filter(scen=="counterfactual") %>% 
  group_by(capacity_multiplier, exp_name, scen, scen_num,rollback_fct) %>% 
  filter(crit_det ==max(crit_det))

dat_subAggr_counterfactual <- dat_sub_counterfactual %>%
  group_by(capacity_multiplier, exp_name, scen,rollback_fct) %>% 
  summarize(            min.value = min(crit_det, na.rm = TRUE),
                        max.value = max(crit_det, na.rm = TRUE),
                        median.value = median(crit_det, na.rm = TRUE),
                        q25.value = quantile(crit_det, probs = 0.25, na.rm = TRUE),
                        q75.value = quantile(crit_det, probs = 0.75, na.rm = TRUE),
                        q2.5.value = quantile(crit_det, probs = 0.025, na.rm = TRUE),
                        q97.5.value = quantile(crit_det, probs = 0.975, na.rm = TRUE))


dat_sub$capacity_multiplier_fct <- factor(dat_sub$capacity_multiplier, levels=rev(seq(0,1,0.1)),labels=rev(seq(0,1,0.1)))
dat_subAggr$capacity_multiplier_fct <- factor(dat_subAggr$capacity_multiplier, levels=rev(seq(0,1,0.1)),labels=rev(seq(0,1,0.1)))

pplot <- ggplot(data =dat_sub)+
  #geom_point(aes(x=capacity_multiplier_fct, y=crit_det, group=exp_name,alpha=rollback_fct),col="grey", fill="grey", shape=21)+
  #geom_ribbon(data=dat_subAggr, aes(x=capacity_multiplier_fct, y=median.value,  ymin=q2.5.value,  ymax=q97.5.value, group=exp_name), fill="grey",alpha=0.1)+
  #geom_ribbon(data=dat_subAggr, aes(x=capacity_multiplier_fct, y=median.value,  ymin=q25.value,  ymax=q2.5.value, group=exp_name), fill="#5c5859",alpha=0.2)+
  geom_line(data=dat_subAggr, aes(x=capacity_multiplier_fct, y=median.value, group=exp_name,alpha=rollback_fct), col="#5c5859",size=1.5)+
  scale_alpha_manual(values=(c(1,0.75,0.5, 0.25)))+
  geom_hline(yintercept = 516, col="red", linetype="dashed",size=1)+
  customTheme


f_save_plot(
  plot_name = paste0("reduction_by_mitigation_100perc_reg11"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_chicago_plots"), width =10, height =4
)


################################################################

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c( !grepl("100perc", exp_names))]
exp_names <- exp_names[c( !grepl("_reopen", exp_names))]
exp_names <- exp_names[c( !grepl("_7daysdelay", exp_names))]


dat1 <- fread(file.path(sim_dir, exp_names[1],"trajectoriesDat_region_11.csv")) %>% select(time,startdate,scen_num,capacity_multiplier, `Ki_t_EMS-11`,`crit_det_EMS-11`)%>% mutate(exp_name=exp_names[1],scen="pr2")
dat2 <- fread(file.path(sim_dir, exp_names[2],"trajectoriesDat_region_11.csv")) %>% select(time,startdate,scen_num,capacity_multiplier, `Ki_t_EMS-11`,`crit_det_EMS-11`)%>% mutate(exp_name=exp_names[2],scen="pr4")
dat3 <- fread(file.path(sim_dir, exp_names[3],"trajectoriesDat_region_11.csv")) %>% select(time,startdate,scen_num,capacity_multiplier, `Ki_t_EMS-11`,`crit_det_EMS-11`)%>% mutate(exp_name=exp_names[3],scen="pr6")
dat4 <- fread(file.path(sim_dir, exp_names[4],"trajectoriesDat_region_11.csv")) %>% select(time,startdate,scen_num,capacity_multiplier, `Ki_t_EMS-11`,`crit_det_EMS-11`)%>% mutate(exp_name=exp_names[4],scen="pr8")
dat5 <- fread(file.path(sim_dir, exp_names[5],"trajectoriesDat_region_11.csv")) %>% select(time,startdate,scen_num,capacity_multiplier, `Ki_t_EMS-11`,`crit_det_EMS-11`)%>% mutate(exp_name=exp_names[5],scen="counterfactual")

dat <- rbind(dat1,dat2,dat3, dat4, dat5) %>%
  mutate(date = as.Date(startdate)+time) %>%
  filter(date>=as.Date("2020-09-01") &
           date <= as.Date("2020-12-31"))


rm(dat1,dat2,dat3, dat4, dat5)
colnames(dat) <- gsub("_EMS-11","",colnames(dat) )
capacityDat <- load_new_capacity(11, filedate="20200915")


dat <- dat %>% group_by(capacity_multiplier, exp_name, scen, scen_num) %>% filter(crit_det >= 516 *capacity_multiplier )


dat <- dat %>% f_get_scenVars()

dat_sub <- dat %>% 
  filter(scen!="counterfactual") %>% 
  group_by(capacity_multiplier, exp_name, scen, scen_num,rollback_fct) %>% 
  filter(crit_det ==max(crit_det))


dat_subAggr <- dat_sub %>% group_by(capacity_multiplier, exp_name, scen,rollback_fct) %>% 
  summarize(            min.value = min(crit_det, na.rm = TRUE),
                        max.value = max(crit_det, na.rm = TRUE),
                        median.value = median(crit_det, na.rm = TRUE),
                        q25.value = quantile(crit_det, probs = 0.25, na.rm = TRUE),
                        q75.value = quantile(crit_det, probs = 0.75, na.rm = TRUE),
                        q2.5.value = quantile(crit_det, probs = 0.025, na.rm = TRUE),
                        q97.5.value = quantile(crit_det, probs = 0.975, na.rm = TRUE))

dat_sub_counterfactual <- dat %>% filter(scen=="counterfactual") %>% 
  group_by(capacity_multiplier, exp_name, scen, scen_num,rollback_fct) %>% 
  filter(crit_det ==max(crit_det))

dat_subAggr_counterfactual <- dat_sub_counterfactual %>%
  group_by(capacity_multiplier, exp_name, scen,rollback_fct) %>% 
  summarize(            min.value = min(crit_det, na.rm = TRUE),
                        max.value = max(crit_det, na.rm = TRUE),
                        median.value = median(crit_det, na.rm = TRUE),
                        q25.value = quantile(crit_det, probs = 0.25, na.rm = TRUE),
                        q75.value = quantile(crit_det, probs = 0.75, na.rm = TRUE),
                        q2.5.value = quantile(crit_det, probs = 0.025, na.rm = TRUE),
                        q97.5.value = quantile(crit_det, probs = 0.975, na.rm = TRUE))


dat_sub$capacity_multiplier_fct <- factor(dat_sub$capacity_multiplier, levels=rev(seq(0,1,0.1)),labels=rev(seq(0,1,0.1)*100))
dat_subAggr$capacity_multiplier_fct <- factor(dat_subAggr$capacity_multiplier, levels=rev(seq(0,1,0.1)),labels=rev(seq(0,1,0.1)*100))

pplot <- ggplot(data =dat_sub)+
  #geom_point(aes(x=capacity_multiplier_fct, y=crit_det, group=exp_name,alpha=rollback_fct),col="grey", fill="grey", shape=21)+
  #geom_ribbon(data=dat_subAggr, aes(x=capacity_multiplier_fct, y=median.value,  ymin=q2.5.value,  ymax=q97.5.value, group=exp_name), fill="grey",alpha=0.1)+
  #geom_ribbon(data=dat_subAggr, aes(x=capacity_multiplier_fct, y=median.value,  ymin=q25.value,  ymax=q2.5.value, group=exp_name), fill="#5c5859",alpha=0.2)+
  geom_line(data=dat_subAggr, aes(x=capacity_multiplier_fct, y=median.value, group=exp_name,alpha=rollback_fct), col="#fbbf16",size=1.5)+
  scale_alpha_manual(values=(c(1,0.75,0.5, 0.25)))+
  scale_y_continuous(lim=c(0,1000))+
  geom_hline(yintercept = 516, col="red", linetype="dashed",size=1)+
  customTheme


pplot 


f_save_plot(
  plot_name = paste0("reduction_by_mitigation_50perc_reg11"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_chicago_plots"), width =10, height =4
)

