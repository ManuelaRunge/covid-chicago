####--------------------------------------
#### Single simulation analysis
####--------------------------------------

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
#simulation_output <- file.path(simulation_output, "overflow_simulations")


####--------------------------------------
#### Region characteristics
####--------------------------------------

dat <- f_region_characteristics()
summary(dat$icubeds_per10th)



####--------------------------------------
####  Baseline - predicted ICU 
####--------------------------------------

exp_name ="20201006_IL_baseline_oldsm8"
exp_dir <- file.path(simulation_output, exp_name)
simdat <- f_load_single_exp(exp_dir)
picu <- f_icu_timeline(dat=simdat, selected_channel="crit_det")


####--------------------------------------
####  Counterfactual - varying reopening
####--------------------------------------
exp_name ="20200919_IL_gradual_reopening_sm7"
exp_dir <- file.path(simulation_output, 'overflow_simulations',exp_name)
simdat <- f_load_single_exp(exp_dir=exp_dir, mainVars= c("date", "scen_num", "sample_num", "reopening_multiplier_4"))
unique(simdat$geography_name)
unique(simdat$reopening_multiplier_4)
picu <- f_icu_timeline(dat=simdat, subregions=c("1"), selected_channel="crit_det", facetVar="reopening_multiplier_4")

### Timeline plot 
for(i in c("illinois", c(1:11))){
  if(!dir.exists(file.path(outdir,"gradual_reopening")))dir.create(file.path(outdir,"gradual_reopening"))
  picu <- f_icu_timeline(dat=simdat, subregions=c(i), selected_channel="crit_det", facetVar="reopening_multiplier_4")
  ggsave(paste0("timeline_",selected_channel,"_region_",i,".png"),
         plot = picu, path = file.path(outdir,"gradual_reopening"), width = 14, height = 8, device = "png"
  )
}

##### Peak in ICU
ICUcumul_out <- f_describe_ICU_cumul( facetVar = "reopening_multiplier_4", subfolder="gradual_reopening")

##### More ICU beds needed at peak
ICUpeak_out <- f_describe_ICU_peak( facetVar = "reopening_multiplier_4", subfolder="gradual_reopening")
ICUpeak_out[[2]]  %>% as.data.frame()


####--------------------------------------
####  Triggered reopening - predicted ICU
####--------------------------------------

exp_names_sm4 <- c("20200919_IL_regreopen50perc_0daysdelay_sm4", "20200919_IL_regreopen100perc_0daysdelay_sm4",
               "20200919_IL_regreopen50perc_3daysdelay_sm4", "20200919_IL_regreopen100perc_3daysdelay_sm4",
               "20200919_IL_regreopen50perc_7daysdelay_sm4", "20200919_IL_regreopen100perc_7daysdelay_sm4")

exp_names_sm7 <- c("20200919_IL_regreopen50perc_0daysdelay_sm7", "20200919_IL_regreopen100perc_0daysdelay_sm7",
               "20200919_IL_regreopen50perc_3daysdelay_sm7", "20200919_IL_regreopen100perc_3daysdelay_sm7",
               "20200919_IL_regreopen50perc_7daysdelay_sm7", "20200919_IL_regreopen100perc_7daysdelay_sm7")

exp_names = exp_names_sm7

for(exp_name in exp_names){
  exp_name_sub <- gsub("20200919_IL_","",exp_name)
  exp_dir <- file.path(simulation_output, 'overflow_simulations',exp_name)
  simdat <- f_load_single_exp(exp_dir)
  out <- f_describe_peak_and_cumul(dat=simdat,subfolder=exp_name_sub)
  rm(out, simdat)
  
}

##### Combined plot
exp_name = "20200919_IL_regreopen_combined"
exp_name_sub = exp_name
exp_dir <- file.path(simulation_output, 'overflow_simulations', exp_name)
list_csvs <- list.files(file.path(simulation_output, 'overflow_simulations'), pattern="trajectoriesDat_sub_long.csv", recursive = TRUE)

subregions <- c("11")
out1 <- f_stacked_barplot(dflist=list_csvs, subregions = subregions, rollback="sm4",reopen="50perc" ,exp_name_sub)
out2 <- f_stacked_barplot(dflist=list_csvs, subregions = subregions, rollback="sm4",reopen="100perc" ,exp_name_sub)
out3 <- f_stacked_barplot(dflist=list_csvs, subregions = subregions, rollback="sm7",reopen="50perc" ,exp_name_sub)
out4 <- f_stacked_barplot(dflist=list_csvs, subregions = subregions, rollback="sm7",reopen="100perc" ,exp_name_sub)

pplot <- plot_grid(f_remove_legend(out1[[2]]),f_remove_legend(out3[[2]]),f_remove_legend(out2[[2]]),f_remove_legend(out4[[2]]))

f_save_plot(pplot=pplot,plot_name = "barplot",plot_dir = file.path(outdir,exp_name_sub), width = 14, height=10)
f_save_plot(pplot=pplot,plot_name = "barplot_v2",plot_dir = file.path(outdir,exp_name_sub), width = 8, height=6)

f_save_plot(pplot=out1[[2]],plot_name = "legend1",plot_dir = file.path(outdir,exp_name_sub), width = 8, height=6)
f_save_plot(pplot=out4[[2]],plot_name = "legend2",plot_dir = file.path(outdir,exp_name_sub), width = 8, height=6)

###### For text
unique(tab_peak_50$capacity_multiplier)
tab_peak_50 %>% filter(capacity_multiplier>0.8 & capacity_multiplier <1) %>% as.data.frame() %>% arrange(geography_name )
tab_peak_50 %>% filter(capacity_multiplier>0.4 & capacity_multiplier <0.5) %>% as.data.frame()

tab_peak_50 %>% group_by(geography_name ) %>% 
          filter(q97.5.aboveICU_ratio<1) %>% 
          filter(capacity_multiplier==max(capacity_multiplier)) %>% 
          mutate(capacity_multiplier=round(capacity_multiplier,1)) %>% 
          arrange(capacity_multiplier )  %>% 
          as.data.frame()

tab_peak_50 %>% filter(geography_name==5) %>% as.data.frame()

#rbind(tab_cumul_100, tab_cumul_50) %>% fwrite(file.path(outdir,"icu_cumul.csv" ), quote=FALSE)
#rbind(tab_peak_100, tab_peak_50) %>% fwrite(file.path(outdir,"icu_peak.csv" ), quote=FALSE)


####--------------------------------------
####  Triggered reopening - Probability
####--------------------------------------

#### probabilities..
if (probPlot) {
  
  
  exp_names_sm4 <- c("20200919_IL_regreopen50perc_0daysdelay_sm4", "20200919_IL_regreopen100perc_0daysdelay_sm4",
                     "20200919_IL_regreopen50perc_3daysdelay_sm4", "20200919_IL_regreopen100perc_3daysdelay_sm4",
                     "20200919_IL_regreopen50perc_7daysdelay_sm4", "20200919_IL_regreopen100perc_7daysdelay_sm4")
  
  exp_names_sm7 <- c("20200919_IL_regreopen50perc_0daysdelay_sm7", "20200919_IL_regreopen100perc_0daysdelay_sm7",
                     "20200919_IL_regreopen50perc_3daysdelay_sm7", "20200919_IL_regreopen100perc_3daysdelay_sm7",
                     "20200919_IL_regreopen50perc_7daysdelay_sm7", "20200919_IL_regreopen100perc_7daysdelay_sm7")
  
  exp_names = exp_names_sm4
  
  for(exp_name in exp_names){
    exp_name_sub <- gsub("20200919_IL_","",exp_name)
    exp_dir <- file.path(simulation_output, 'overflow_simulations',exp_name)
    simdat <- f_get_probabilities(exp_dir)
    rm(simdat)
    
  }
  
  propDat_simList <- list()
  for(exp_name in c(exp_names_sm4,exp_names_sm7)){
    exp_dir <- file.path(simulation_output, 'overflow_simulations',exp_name)
    load(file.path(exp_dir, "propDat_sim.Rdata"))
    propDat_simList[[length(propDat_simList)+1]] <- propDat_sim
    rm(propDat_sim)
  }
  propDat_sim <- propDat_simList %>% bind_rows()
  rm(propDat_simList)
 
  exp_name = "20200919_IL_regreopen_combined"
  exp_name_sub = exp_name
  exp_dir <- file.path(simulation_output, 'overflow_simulations', exp_name)
  
  pplot <- ggplot(data = subset(propDat_sim)) +
    geom_line(aes(x = capacity_multiplier, y = prob_overflow, group = exp_name, col = reopen, linetype = rollback, alpha = delay), size = 1.1) +
    # geom_point(aes(x=capacity_multiplier, y=prob_overflow, group=exp_name, fill=reopen),col="white",shape=21, size=2) +
    geom_hline(yintercept = 0.2) +
    scale_alpha_manual(values = c(1, 0.6, 0.3)) +
    scale_color_manual(values = c("deepskyblue3", "orange")) +
    scale_fill_manual(values = c("deepskyblue3", "orange")) +
    facet_wrap(~geography_name, scales = "free") +
    customThemeNoFacet +
    background_grid()
  
  f_save_plot(pplot=pplot,plot_name = "ICUoverflow_proball_perCovidRegion",plot_dir = file.path(exp_dir), width = 14, height=8)
  

  propDat_sim$grpVar <- paste0(gsub("none","0days",gsub(" ","",propDat_sim$delay)),"-", propDat_sim$reopen)
  unique(  propDat_sim$grpVar )
  propDat_sim$grpVar <- factor(propDat_sim$grpVar,
                               levels=c("7days-100perc","3days-100perc","0days-100perc",
                                        "7days-50perc","3days-50perc","0days-50perc"),
                               labels=c("7days-100perc","3days-100perc","0days-100perc",
                                        "7days-50perc","3days-50perc","0days-50perc"))
  
  
  custom_cols <- c("0days-50perc"="#c6dbef", "3days-50perc"="#6baed6","7days-50perc"="#2171b5", 
                   "0days-100perc"="#fee0d2","3days-100perc"="#fb6a4a", "7days-100perc"="#cb181d")
  
  
  #### Subset 
  pplot <- ggplot(data = subset(propDat_sim, geography_name %in% c("1","4","11")) ) +
    geom_line(aes(x = capacity_multiplier, y = prob_overflow, group = exp_name, col = grpVar, linetype=rollback), size = 1.1) +
    # geom_point(aes(x=capacity_multiplier, y=prob_overflow, group=exp_name, fill=reopen),col="white",shape=21, size=2) +
    geom_hline(yintercept = 0.2) +
    scale_alpha_manual(values = c(1, 0.6, 0.3)) +
    scale_color_manual(values = custom_cols) +
    scale_fill_manual(values = custom_cols) +
    facet_wrap(~geography_name, scales = "free") +
    customThemeNoFacet +
    background_grid()
  
  f_save_plot(pplot=pplot,plot_name = "ICUoverflow_proball_perCovidRegion_sub1",plot_dir = file.path(exp_dir), width = 15, height = 6)

  
  pplot <- ggplot(data = subset(propDat_sim, geography_name %in% c("1","4","11")) ) +
    geom_line(aes(x = capacity_multiplier, y = prob_overflow, group = exp_name, col = grpVar, linetype=rollback), size = 1.1) +
    # geom_point(aes(x=capacity_multiplier, y=prob_overflow, group=exp_name, fill=reopen),col="white",shape=21, size=2) +
    geom_hline(yintercept = 0.2) +
    scale_alpha_manual(values = c(1, 0.6, 0.3)) +
    scale_color_manual(values = custom_cols) +
    scale_fill_manual(values = custom_cols) +
    facet_wrap(reopen~geography_name, scales = "free") +
    scale_y_continuous(lim=c(0,1))+
    customThemeNoFacet +
    background_grid()
  
  f_save_plot(pplot=pplot,plot_name = "ICUoverflow_proball_perCovidRegion_sub2",plot_dir = file.path(exp_dir), width = 15, height = 9)
  
 
  
  
}




