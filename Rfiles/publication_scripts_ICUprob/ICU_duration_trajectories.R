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

exp_names <- exp_names[(grep("100perc",exp_names))]
exp_names <- exp_names[!(grepl("counterfactual",exp_names))]
exp_names <- exp_names[!(grepl("7daysdelay",exp_names))]

peakDat_List <- list()
exceedDat_List <- list()
exceed70Dat_List <- list()
triggerDat_List <- list()

for(exp_name in exp_names){
  print(exp_name)
  
  temp_dat_list <- list()
  for(reg in c(1,4,11)) {
    tdat <- f_load_trajectories(sim_dir, exp_name, region_nr = reg) %>% 
                                    filter(date>=as.Date("2020-10-01")) %>% 
                                    dplyr::select(c(date, scen_num, sample_num, 
                                                    capacity_multiplier, region, 
                                                    nsamples, nmultiplier_per_sample, 
                                                    crit_det)) %>%
                                    mutate(exp_name=exp_name) 
    
    
    tdat <-  tdat %>%
      left_join(capacityDat, by="region") %>%
      group_by(exp_name,region, capacity_multiplier, sample_num, scen_num) %>% 
      mutate(peak=max(crit_det)) %>% 
      mutate(above_yn = ifelse(peak > avg_resource_available & date <= as.Date("2020-12-31"), 1,0))
    
    
    exceedDat <- tdat %>% 
      filter(date>=as.Date("2020-10-01")) %>%
      group_by(exp_name,region, capacity_multiplier, sample_num, scen_num) %>% 
      filter(crit_det >= avg_resource_available)  %>%
      mutate(exceed_date_from=min(date),
             exceed_date_to=max(date),
             diff_exceed_date = as.numeric(exceed_date_to - exceed_date_from)) %>% 
      select(exp_name,region, capacity_multiplier, sample_num, scen_num, exceed_date_from , exceed_date_to, diff_exceed_date)%>% 
      unique()
    
    
    exceedDatAggr <- exceedDat  %>% 
      group_by(exp_name,region, capacity_multiplier) %>% 
      add_tally() %>%
      f_get_scenVars %>%
      filter(delay!="7daysdelay") %>% 
      group_by(exp_name, region,capacity_multiplier)  %>% 
      mutate(diff_exceed_date = ifelse(n <10, 0, diff_exceed_date)) %>%
      summarize(nsamples=sum(n),
                diff_exceed_date_lower =  quantile(diff_exceed_date, probs = 0.25, na.rm = TRUE) ,
                diff_exceed_date_mean = mean(diff_exceed_date, na.rm = TRUE),
                diff_exceed_date_upper =  quantile(diff_exceed_date, probs = 0.75, na.rm = TRUE))%>%
      f_get_scenVars
    
    
    temp_dat_list[[length(temp_dat_list)+1]] <- exceedDatAggr
  }
  
  outdat <- temp_dat_list %>% bind_rows()
  exceedDat_List[[length(exceedDat_List)+1]] <- outdat
  rm(temp_dat_list, exceedDatAggr)
  
}


###--------------------------------
## exceed
###--------------------------------
plotdatAggr <- exceedDat_List %>% bind_rows()
save(plotdatAggr,file=file.path(sim_dir, "plotdatAggr.Rdata"))

pplot <- ggplot(data=plotdatAggr)+
  geom_ribbon(data=subset(plotdatAggr),
              aes(x=capacity_multiplier,
                  ymin=diff_exceed_date_lower ,
                  ymax=diff_exceed_date_upper ,
                  fill=rollback, group=rollback), alpha=0.3)  + 
  geom_line(data=subset(plotdatAggr), 
            aes(x=capacity_multiplier,
                y=diff_exceed_date_mean ,
                col=rollback, group=rollback), size=1)  +
  facet_wrap(region~reopen)+
  #scale_fill_manual(values = TwoCols_seq) +
  #scale_color_manual(values = TwoCols_seq) +
  scale_y_continuous(lim=c(0, 120))+
  customTheme+
  theme() + 
  labs( y = "Days above capacity",
        x = "Trigger threshold",
        caption="mean and IQR")


pplot <- ggplot(data=plotdatAggr)+
  geom_errorbar(data=subset(plotdatAggr),
                  aes(x=capacity_multiplier,
                      y=diff_exceed_date_mean ,
                      ymin=diff_exceed_date_lower ,
                      ymax=diff_exceed_date_upper , group=rollback,col=reopen), width=0)  + 
  geom_point(data=subset(plotdatAggr),
              aes(x=capacity_multiplier,
                  y=diff_exceed_date_mean ,
                  fill=reopen, group=rollback, alpha=nsamples),shape=21)  + 
  facet_wrap(region~reopen)+
  scale_fill_manual(values = TwoCols_seq) +
  scale_color_manual(values = TwoCols_seq) +
  scale_y_continuous(lim=c(0, 120))+
  customTheme+
  theme() + 
  labs( y = "Days above capacity",
        x = "Trigger threshold",
        caption="mean and IQR")

pplot 


pplot <- ggplot(data=plotdatAggr)+
  geom_line(data=subset(plotdatAggr),
             aes(x=capacity_multiplier,
                 y=diff_exceed_date_mean ,
                 fill=reopen, group=rollback, alpha=nsamples),shape=21)  + 
  facet_wrap(region~reopen)+
  scale_fill_manual(values = TwoCols_seq) +
  scale_color_manual(values = TwoCols_seq) +
  scale_y_continuous(lim=c(0, 120))+
  customTheme+
  theme() + 
  labs( y = "Days above capacity",
        x = "Trigger threshold",
        caption="mean and IQR")

pplot 
###--------------------------------
##  diff_trigger_peak
###--------------------------------

plotdat <- peakDat %>% 
  f_addVar(trigger_capacity_Dat) %>%
  group_by(exp_name, scen_num) %>%
  mutate(diff_trigger_peak = as.numeric(peak_date - trigger_capacity_date )) %>%
  f_get_scenVars

ggplot(data=plotdat)+
  # geom_jitter(aes(x=as.factor(capacity_multiplier), y=diff_trigger_peak, fill=as.factor(exp_name)),
  #            width=0.1,shape=21,alpha=0.5)  +
  geom_boxplot(aes(x=as.factor(capacity_multiplier), y=diff_trigger_peak, fill=as.factor(rollback)), width=0.3,shape=21,alpha=0.5)  +
  facet_wrap(~reopen, ncol=2)

###--------------------------------
##  trigger_to_exceed70_to
###--------------------------------
plotdat <- exceed70Dat %>% 
  f_addVar(trigger_capacity_Dat) %>%
  group_by(exp_name, scen_num) %>%
  mutate(trigger_to_exceed70_to = as.numeric(exceed70_date_to - trigger_capacity_date) ) %>%
  group_by(exp_name, capacity_multiplier) %>% add_tally()%>%
  f_get_scenVars


ggplot(data=plotdat)+
  geom_jitter(aes(x=as.factor(capacity_multiplier), 
                  y=trigger_to_exceed70_to,
                  fill=as.factor(exp_name)),
              width=0.1,shape=21,alpha=0.3)  +
  geom_boxplot(data=subset(plotdat, n>5), 
               aes(x=as.factor(capacity_multiplier),
                   y=trigger_to_exceed70_to, fill=as.factor(exp_name)),
               width=0.3,shape=21)  +
  facet_wrap(~exp_name, ncol=1)

