#### --------------------------------------
#### Single simulation analysis
#### --------------------------------------

library(tidyverse)
library(cowplot)
library(data.table)
library(raster)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

theme_set(theme_cowplot())

outdir <- file.path("C:/Users/mrm9534/Box/MR_archive/testfigures2")
# simulation_output <- file.path(simulation_output, "_overflow_simulations")


#### --------------------------------------
#### Region characteristics
#### --------------------------------------

dat <- f_region_characteristics()
summary(dat$icubeds_per10th)



#### --------------------------------------
####  Baseline - predicted ICU
#### --------------------------------------

exp_name <- "20201006_IL_baseline_oldsm8"
exp_dir <- file.path(simulation_output,"_simulations_for_civis", exp_name)
simdat <- f_load_single_exp(exp_dir)
picu <- f_icu_timeline(dat = simdat, selected_channel = "crit_det")


#### --------------------------------------
####  Baseline -  How often in the past did 20 happen?
#### --------------------------------------

exp_name <- "20201006_IL_baseline_oldsm8"
exp_dir <- file.path(simulation_output,"_simulations_for_civis", exp_name)
simdat <- f_load_single_exp(exp_dir)
simdat$date <- as.Date(simdat$date)
simdat <- simdat %>% filter(date <= Sys.Date())
(ndates <- length(unique(simdat$date)))

ICU_threshold_past <- list()
for(capacity_threshold in seq(0.2,1,0.2)){
ICU_threshold_past[[length(ICU_threshold_past)+1]] <- simdat %>% 
                      group_by(geography_name) %>% 
                      filter(channel=="crit_det") %>% 
                      mutate(capacity_multiplier=capacity_threshold) %>% 
                      filter(median.value >= icu_available*capacity_multiplier) %>%
                      add_tally() 
}
ICU_threshold_past <- ICU_threshold_past %>% bind_rows()
table(ICU_threshold_past$capacity_multiplier, ICU_threshold_past$geography_name)

ICU_threshold_past %>%  dplyr::select(geography_name, capacity_multiplier, date) %>% arrange(geography_name, capacity_multiplier, date)

pplot <- ggplot(data=ICU_threshold_past)+
  geom_point(aes(x=date, y=capacity_multiplier)) +
  facet_wrap(~geography_name)
f_save_plot(pplot=pplot,plot_name = "ICUcapacity_thresholds_in_the_past",plot_dir = file.path(outdir))



#### --------------------------------------
####  Counterfactual - varying reopening
#### --------------------------------------
exp_name <- "20200919_IL_gradual_reopening_sm7"
exp_dir <- file.path(simulation_output, "_overflow_simulations", exp_name)
simdat <- f_load_single_exp(exp_dir = exp_dir, mainVars = c("date", "scen_num", "sample_num", "reopening_multiplier_4"))
unique(simdat$geography_name)
unique(simdat$reopening_multiplier_4)
picu <- f_icu_timeline(dat = simdat, subregions = c("1"), selected_channel = "crit_det", facetVar = "reopening_multiplier_4")

### Timeline plot
for (i in c("illinois", c(1:11))) {
  if (!dir.exists(file.path(outdir, "gradual_reopening"))) dir.create(file.path(outdir, "gradual_reopening"))
  picu <- f_icu_timeline(dat = simdat, subregions = c(i), selected_channel = "crit_det", facetVar = "reopening_multiplier_4")
  ggsave(paste0("timeline_", selected_channel, "_region_", i, ".png"),
    plot = picu, path = file.path(outdir, "gradual_reopening"), width = 14, height = 8, device = "png"
  )
}

##### Peak in ICU
ICUcumul_out <- f_describe_ICU_cumul(facetVar = "reopening_multiplier_4", subfolder = "gradual_reopening")

##### More ICU beds needed at peak
ICUpeak_out <- f_describe_ICU_peak(facetVar = "reopening_multiplier_4", subfolder = "gradual_reopening")
ICUpeak_out[[2]] %>% as.data.frame()


#### --------------------------------------
####  Triggered reopening - predicted ICU
#### --------------------------------------

exp_names_sm4 <- c(
  "20200919_IL_regreopen50perc_0daysdelay_sm4", "20200919_IL_regreopen100perc_0daysdelay_sm4",
  "20200919_IL_regreopen50perc_3daysdelay_sm4", "20200919_IL_regreopen100perc_3daysdelay_sm4",
  "20200919_IL_regreopen50perc_7daysdelay_sm4", "20200919_IL_regreopen100perc_7daysdelay_sm4"
)

exp_names_sm7 <- c(
  "20200919_IL_regreopen50perc_0daysdelay_sm7", "20200919_IL_regreopen100perc_0daysdelay_sm7",
  "20200919_IL_regreopen50perc_3daysdelay_sm7", "20200919_IL_regreopen100perc_3daysdelay_sm7",
  "20200919_IL_regreopen50perc_7daysdelay_sm7", "20200919_IL_regreopen100perc_7daysdelay_sm7"
)

exp_names <- exp_names_sm7

for (exp_name in exp_names) {
  exp_name_sub <- gsub("20200919_IL_", "", exp_name)
  exp_dir <- file.path(simulation_output, "_overflow_simulations", exp_name)
  simdat <- f_load_single_exp(exp_dir)
  out <- f_describe_peak_and_cumul(dat = simdat, subfolder = exp_name_sub)
  rm(out, simdat)
}

##### Combined plot
exp_name <- "20200919_IL_regreopen_combined"
exp_name_sub <- exp_name
exp_dir <- file.path(simulation_output, "_overflow_simulations", exp_name)
list_csvs <- list.files(file.path(simulation_output, "_overflow_simulations"), pattern = "trajectoriesDat_sub_long.csv", recursive = TRUE)

subregions <- c("11")
out1 <- f_stacked_barplot(dflist = list_csvs, subregions = subregions, rollback = "sm4", reopen = "50perc", exp_name_sub)
out2 <- f_stacked_barplot(dflist = list_csvs, subregions = subregions, rollback = "sm4", reopen = "100perc", exp_name_sub)
out3 <- f_stacked_barplot(dflist = list_csvs, subregions = subregions, rollback = "sm7", reopen = "50perc", exp_name_sub)
out4 <- f_stacked_barplot(dflist = list_csvs, subregions = subregions, rollback = "sm7", reopen = "100perc", exp_name_sub)

pplot <- plot_grid(f_remove_legend(out1[[2]]), f_remove_legend(out3[[2]]), f_remove_legend(out2[[2]]), f_remove_legend(out4[[2]]))

f_save_plot(pplot = pplot, plot_name = "barplot", plot_dir = file.path(outdir, exp_name_sub), width = 14, height = 10)
f_save_plot(pplot = pplot, plot_name = "barplot_v2", plot_dir = file.path(outdir, exp_name_sub), width = 8, height = 6)

f_save_plot(pplot = out1[[2]], plot_name = "legend1", plot_dir = file.path(outdir, exp_name_sub), width = 8, height = 6)
f_save_plot(pplot = out4[[2]], plot_name = "legend2", plot_dir = file.path(outdir, exp_name_sub), width = 8, height = 6)

###### For text
unique(tab_peak_50$capacity_multiplier)
tab_peak_50 %>%
  filter(capacity_multiplier > 0.8 & capacity_multiplier < 1) %>%
  as.data.frame() %>%
  arrange(geography_name)
tab_peak_50 %>%
  filter(capacity_multiplier > 0.4 & capacity_multiplier < 0.5) %>%
  as.data.frame()

tab_peak_50 %>%
  group_by(geography_name) %>%
  filter(q97.5.aboveICU_ratio < 1) %>%
  filter(capacity_multiplier == max(capacity_multiplier)) %>%
  mutate(capacity_multiplier = round(capacity_multiplier, 1)) %>%
  arrange(capacity_multiplier) %>%
  as.data.frame()

tab_peak_50 %>%
  filter(geography_name == 5) %>%
  as.data.frame()

# rbind(tab_cumul_100, tab_cumul_50) %>% fwrite(file.path(outdir,"icu_cumul.csv" ), quote=FALSE)
# rbind(tab_peak_100, tab_peak_50) %>% fwrite(file.path(outdir,"icu_peak.csv" ), quote=FALSE)


#### --------------------------------------
####  Triggered reopening - Probability
#### --------------------------------------

exp_names_sm4 <- c(
  "20200919_IL_regreopen50perc_0daysdelay_sm4", "20200919_IL_regreopen100perc_0daysdelay_sm4",
  "20200919_IL_regreopen50perc_3daysdelay_sm4", "20200919_IL_regreopen100perc_3daysdelay_sm4",
  "20200919_IL_regreopen50perc_7daysdelay_sm4", "20200919_IL_regreopen100perc_7daysdelay_sm4"
)

exp_names_sm7 <- c(
  "20200919_IL_regreopen50perc_0daysdelay_sm7", "20200919_IL_regreopen100perc_0daysdelay_sm7",
  "20200919_IL_regreopen50perc_3daysdelay_sm7", "20200919_IL_regreopen100perc_3daysdelay_sm7",
  "20200919_IL_regreopen50perc_7daysdelay_sm7", "20200919_IL_regreopen100perc_7daysdelay_sm7"
)

exp_names <- c(exp_names_sm4, exp_names_sm7)

for (exp_name in exp_names) {
  exp_name_sub <- gsub("20200919_IL_", "", exp_name)
  exp_dir <- file.path(simulation_output, "_overflow_simulations", exp_name)
  simdat <- f_get_probabilities(exp_dir)
  rm(simdat)
}

## Analyze combines probabilities
exp_name <- "20200919_IL_regreopen_combined"
exp_name_sub <- exp_name
exp_dir <- file.path(simulation_output, "_overflow_simulations", exp_name)

## Combine probability files
exp_dirs <- list.files(file.path(simulation_output, "_overflow_simulations"), pattern = "propDat_sim.Rdata", recursive = T, full.names = T)
propDat_sim <- f_combine_Rdata(exp_dirs)


## Create custom grouping variable for delay and reopen
propDat_sim$grpVar <- paste0(gsub("none","0days",gsub(" ","",propDat_sim$delay)),"-", propDat_sim$reopen)

delay_reopen = c("7days-100perc","3days-100perc","0days-100perc",
                  "7days-50perc","3days-50perc","0days-50perc")

propDat_sim$grpVar <- factor(propDat_sim$grpVar, levels=delay_reopen, labels=delay_reopen)

fwrite(propDat_sim, file.path(exp_dir, "propDat_sim_combined.csv"), quote = FALSE)


### Differences across regions
f_custom_prob_plot2(
  dat = subset(propDat_sim), exp_dir = exp_dir,
  plot_name = "ICUoverflow_proball_compare_regions"
)
f_custom_prob_plot2(
  dat = subset(propDat_sim), exp_dir = exp_dir, subregions = c(1,4,11),
  plot_name = "ICUoverflow_proball_compare_regions1-4-11", width=12, height=8
)

### Probability plots
f_custom_prob_plot(
  dat = subset(propDat_sim), exp_dir = exp_dir,
  plot_name = "ICUoverflow_proball_perCovidRegion", reopenFacet = FALSE
)

f_custom_prob_plot(
  dat = subset(propDat_sim, geography_name %in% c("1", "4", "11")), exp_dir = exp_dir,
  plot_name = "ICUoverflow_proball_perCovidRegion_sub1", width = 15, height = 6, reopenFacet = FALSE
)


f_custom_prob_plot(
  dat = subset(propDat_sim, geography_name %in% c("1", "4", "11")), exp_dir = exp_dir,
  plot_name = "ICUoverflow_proball_perCovidRegion_sub2", width = 15, height = 9
)



#### COmpare with characteristics
propDat_sim <- fread(file.path(exp_dir, "propDat_sim_combined.csv"))
propDat_sim <- propDat_sim %>% dplyr::select(-pop,-icu_available) %>% left_join(f_region_characteristics(), by=c("geography_name"))

ICU_threshold_future <- list()
for(riskTolerance in seq(0.1,1,0.1)){
  
  ICU_threshold_future[[length(ICU_threshold_future)+1]] <- propDat_sim %>%
    filter( prob_overflow<=riskTolerance) %>%
    group_by(geography_name, exp_name) %>%
    mutate(risk_tolerance=riskTolerance)%>%
    filter(prob_overflow==max(prob_overflow)) %>%
    filter(capacity_multiplier==max(capacity_multiplier))
}
ICU_threshold_future <- ICU_threshold_future %>% bind_rows()

ICU_threshold_future$grpVar <- paste0(gsub("none","0days",gsub(" ","",ICU_threshold_future$delay)),"-", ICU_threshold_future$reopen)

delay_reopen = c("7days-100perc","3days-100perc","0days-100perc",
                 "7days-50perc","3days-50perc","0days-50perc")

ICU_threshold_future$grpVar <- factor(ICU_threshold_future$grpVar, levels=delay_reopen, labels=delay_reopen)

custom_cols <- c("0days-50perc"="#c6dbef", "3days-50perc"="#6baed6","7days-50perc"="#2171b5", 
                 "0days-100perc"="#fee0d2","3days-100perc"="#fb6a4a", "7days-100perc"="#cb181d")

ICU_threshold_future$reopen_label <- NA
ICU_threshold_future$reopen_label[ICU_threshold_future$reopen=="100perc"] <- "Fast rebound"
ICU_threshold_future$reopen_label[ICU_threshold_future$reopen=="50perc"] <- "Moderate rebound"

table(ICU_threshold_future$risk_tolerance, ICU_threshold_future$geography_name)
unique(ICU_threshold_future$risk_tolerance)

ggplot(data=subset(ICU_threshold_future, risk_tolerance<0.9)) +
  geom_jitter(aes(x=icubeds_per10th, y=capacity_multiplier, col=as.factor(risk_tolerance), group=geography_name),size=3)+
  facet_wrap(~exp_name)


pplot <- ggplot(data=subset(ICU_threshold_future, risk_tolerance<0.9)) +
  geom_smooth(aes(x=risk_tolerance, y=capacity_multiplier,group=exp_name, col=grpVar, linetype=rollback),size=1, se=F)+
  scale_color_manual(values = custom_cols) +
  scale_fill_manual(values = custom_cols) +
  facet_wrap(~reopen_label) + 
  customThemeNoFacet+
  background_grid()+
  labs(x="Probability of ICU overflow (risk tolerance)", 
       y="Trigger threshold\n(% of ICU COVID availability)",
       color="delay")


f_save_plot(pplot=pplot,plot_name = "risk-tolerance_capacity_overall",plot_dir = file.path(outdir), width =8, height=3)



##### Export for map in QGIS
# exp="20200919_IL_regreopen100perc_0daysdelay_sm4"
# subdat <- propDat_sim %>% 
#           filter(exp_name==exp, prob_overflow<=0.5) %>% 
#           group_by(geography_name) %>% 
#           filter(prob_overflow==max(prob_overflow)) %>% 
#           filter(capacity_multiplier==max(capacity_multiplier))
# 
# fwrite(subdat, file.path(exp_dir,"regreopen100perc_0daysdelay_sm4_riskTolerance_5.csv"),quote=FALSE)
# 


