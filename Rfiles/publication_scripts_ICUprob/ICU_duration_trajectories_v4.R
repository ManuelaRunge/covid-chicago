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
  
  temp_dat <- f_load_trajectories(sim_dir, exp_name, region_nr = 11) %>% 
    filter(date>=as.Date("2020-10-01")) %>% 
    dplyr::select(c(date, scen_num, sample_num, 
                    capacity_multiplier, region, 
                    nsamples, nmultiplier_per_sample, 
                    crit_det)) %>%
    mutate(exp_name=exp_name) 
  
  temp_dat <-  temp_dat %>%
    left_join(capacityDat, by="region") %>%
    group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
    mutate(peak=max(crit_det)) %>% 
    mutate(above_yn = ifelse(peak > avg_resource_available & date <= as.Date("2020-12-31"), 1,0))
  
  peakDat <- temp_dat %>% 
    group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
    filter(crit_det==peak)  %>%
    mutate(peak_date=date) %>% 
    group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
    filter(peak_date==min(peak_date))  %>% 
    select(exp_name,capacity_multiplier, sample_num, scen_num, peak_date)
  
  
  exceedDat <- temp_dat %>% 
    group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
    filter(crit_det >= avg_resource_available)  %>%
    mutate(exceed_date_from=min(date),
           exceed_date_to=max(date),
           diff_exceed_date = as.numeric(exceed_date_to - exceed_date_from)) %>% 
    select(exp_name,capacity_multiplier, sample_num, scen_num, exceed_date_from , exceed_date_to, diff_exceed_date)%>% 
    unique()
  
  exceed70Dat <- temp_dat %>% 
    group_by(exp_name,capacity_multiplier, sample_num, scen_num) %>% 
    filter(crit_det >= avg_resource_available * 0.7)  %>%
    mutate(exceed70_date_from=min(date),
           exceed70_date_to=max(date),
           diff_exceed70_date = as.numeric(exceed70_date_to - exceed70_date_from)) %>% 
    select(exp_name,capacity_multiplier, sample_num, scen_num, exceed70_date_from ,exceed70_date_to, diff_exceed70_date) %>% 
    unique()
  
  trigger_capacity_Dat <- temp_dat %>% 
    filter(date >=as.Date("2020-10-01") & date <=as.Date("2020-12-31") ) %>%
    select(exp_name,capacity_multiplier, sample_num, scen_num, avg_resource_available, crit_det,date) %>%
    mutate(trigger_capacity_val =avg_resource_available*capacity_multiplier) %>%
    filter(crit_det > trigger_capacity_val)  %>% 
    group_by(exp_name,capacity_multiplier,sample_num, scen_num, avg_resource_available, trigger_capacity_val) %>% 
    filter(date==min(date)) %>%
    rename(trigger_capacity_date=date) %>%
    unique()
  
  peakDat_List[[length(peakDat_List)+1]] <- peakDat
  exceedDat_List[[length(exceedDat_List)+1]] <- exceedDat
  exceed70Dat_List[[length(exceed70Dat_List)+1]] <- exceed70Dat
  triggerDat_List[[length(triggerDat_List)+1]] <- trigger_capacity_Dat
  
  rm(temp_dat, peakDat, exceedDat, exceed70Dat, trigger_capacity_Dat)
  
}


trigger_capacity_Dat <- triggerDat_List %>% bind_rows()
exceed70Dat <- exceed70Dat_List %>% bind_rows()
exceedDat <- exceedDat_List %>% bind_rows()


trigger_capacity_Dat_reg11 <- trigger_capacity_Dat_reg11 %>% mutate(region ="Region 11")
exceed70Dat_reg11 <- exceed70Dat_reg11 %>% mutate(region ="Region 11")
exceedDat_reg11 <- exceedDat_reg11 %>% mutate(region ="Region 11")

trigger_capacity_Dat_reg4 <- trigger_capacity_Dat_reg4 %>% mutate(region ="Region 4")
exceed70Dat_reg4 <- exceed70Dat_reg4 %>% mutate(region ="Region 4")
exceedDat_reg4 <- exceedDat_reg4 %>% mutate(region ="Region 4")

trigger_capacity_Dat_reg1 <- trigger_capacity_Dat_reg1 %>% mutate(region ="Region 1")
exceed70Dat_reg1 <- exceed70Dat_reg1 %>% mutate(region ="Region 1")
exceedDat_reg1 <- exceedDat_reg1 %>% mutate(region ="Region 1")

trigger_capacity_Dat <- rbind(trigger_capacity_Dat_reg11, trigger_capacity_Dat_reg4, trigger_capacity_Dat_reg1)
exceed70Dat <- rbind(exceed70Dat_reg11, exceed70Dat_reg4, exceed70Dat_reg1)
exceedDat <- rbind(exceedDat_reg11, exceedDat_reg4, exceedDat_reg1)

###--------------------------------
## exceed
###--------------------------------

plotdat_1 <- exceedDat  %>% 
  group_by(exp_name,region, capacity_multiplier) %>% 
  add_tally() %>%
  f_get_scenVars %>%
  filter(delay!="7daysdelay")

tapply(plotdat_1$n, plotdat_1$capacity_multiplier, summary)
plotdat_1$capacity_multiplier_fct3 <- NA
plotdat_1$capacity_multiplier_fct3[plotdat_1$capacity_multiplier<=0.4] <- "<40"
plotdat_1$capacity_multiplier_fct3[plotdat_1$capacity_multiplier<=0.8] <- "<80"
plotdat_1$capacity_multiplier_fct3[plotdat_1$capacity_multiplier>0.8] <- ">80"

sall <- c( 10  ,11 , 12  ,14 , 17,  18 , 28 , 37 , 43 , 45  ,52 , 55  ,56  ,60 , 79  ,83 , 85 , 95 ,103, 109 ,110 ,123, 126,
           127 ,135 ,137, 138, 139, 140, 142, 143, 152, 157, 169, 171, 174 ,178 ,180, 194,
           221 ,223 ,224, 228, 229, 231, 233, 237, 238 ,240, 259, 260 ,277 ,294 ,309 ,314, 319, 323 ,325,328 ,329 ,330, 
           334, 335, 336 ,351, 360, 365, 373, 374, 387 ,388, 389, 390, 392, 398)

plotdat_1 <- subset(plotdat_1,( sample_num %in% sall) | ( rollback =="counterfactual"))


plotdatAggr_1 <- plotdat_1 %>% 
  group_by(exp_name, region)  %>% 
  mutate(diff_exceed_date = ifelse(n <10, 0, diff_exceed_date)) %>%
  summarize(nsamples=sum(n),
            diff_exceed_date_lower =  quantile(diff_exceed_date, probs = 0.25, na.rm = TRUE) ,
            diff_exceed_date_mean = mean(diff_exceed_date, na.rm = TRUE),
            diff_exceed_date_upper =  quantile(diff_exceed_date, probs = 0.75, na.rm = TRUE))%>%
  f_get_scenVars


###--------------------------------
##  trigger_to_exceed70_to
###--------------------------------

plotdat_3 <- exceed70Dat %>% 
  f_addVar(trigger_capacity_Dat) %>%
  group_by(exp_name, region, scen_num) %>%
  mutate(trigger_to_exceed70_to = as.numeric(exceed70_date_to - trigger_capacity_date) ) %>%
  group_by(exp_name, capacity_multiplier) %>% add_tally()%>%
  f_get_scenVars


##--------------------------------
##### Aggregated only
##--------------------------------

plotdatAggr_1 <- plotdat_1 %>% 
  group_by(exp_name, region, capacity_multiplier)  %>% 
  summarize(trigger_to_exceed70_to_lower =  quantile(trigger_to_exceed70_to, probs = 0.25, na.rm = TRUE) ,
            trigger_to_exceed70_to_mean = mean(trigger_to_exceed70_to, na.rm = TRUE),
            trigger_to_exceed70_to_upper =  quantile(trigger_to_exceed70_to, probs = 0.75, na.rm = TRUE))%>%
  f_get_scenVars

plotdatAggr_3 <- plotdat_3 %>% 
  group_by(exp_name, region, )  %>% 
  summarize(trigger_to_exceed70_to_lower =  quantile(trigger_to_exceed70_to, probs = 0.25, na.rm = TRUE) ,
            trigger_to_exceed70_to_mean = mean(trigger_to_exceed70_to, na.rm = TRUE),
            trigger_to_exceed70_to_upper =  quantile(trigger_to_exceed70_to, probs = 0.75, na.rm = TRUE),
            trigger_to_exceed70_to_ci_lower =  quantile(trigger_to_exceed70_to, probs = 0.025, na.rm = TRUE),
            trigger_to_exceed70_to_ci_upper =  quantile(trigger_to_exceed70_to, probs = 0.975, na.rm = TRUE)
  )%>%
  f_get_scenVars


pplot_top <- 
  ggplot(data=plotdat_3)+
  geom_pointrange(data=plotdatAggr_3, 
                  aes(x=as.factor(rollback),
                      y=trigger_to_exceed70_to_mean, 
                      ymin=trigger_to_exceed70_to_ci_lower, 
                      ymax=trigger_to_exceed70_to_ci_upper,
                      fill=region,
                      group=region),position = position_dodge(width=0.5), shape=21,size=0.7)  +
  scale_shape_manual(values=c(1,21,21))+
  #scale_fill_brewer(palette = "Dark2")+
  #scale_color_manual(values=TwoCols_seq)+
  #scale_fill_manual(values=TwoCols_seq)+
  customTheme+
  scale_y_continuous(lim=c(0,180), breaks=seq(0,180,20), minor_breaks = seq(0,180,10), expand=c(0,0))+
  theme(panel.grid.major.x = element_blank())+
  labs(y="Days after trigger\nuntil ICU census decreased to 70%", x="Mitigation effectiveness")+
  facet_wrap(~reopen, nrow=1)

pplot_top
