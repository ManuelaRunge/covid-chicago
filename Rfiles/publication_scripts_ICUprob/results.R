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
customTheme <- f_getCustomTheme(fontscl = 0)

SAVE <- FALSE
outdir <- file.path(file.path(project_path, "/project_notes/publications/covid_model_IL_overflow/out"))
# simulation_output <- file.path(simulation_output, "_overflow_simulations")


#### --------------------------------------
#### Experiment names and locations
#### --------------------------------------
exp_name_baseline <- "20201006_IL_baseline_oldsm8"
exp_dir_baseline <- file.path(simulation_output,'_simulations_for_civis',  exp_name_baseline)

exp_name_counterfactual <- "20200919_IL_regreopen_counterfactual"
exp_dir_counterfactual<- file.path(simulation_output, "_overflow_simulations", exp_name_counterfactual)

exp_name_reopen <- "20200919_IL_gradual_reopening_sm7" #  "20200917_IL_gradual_reopening"
exp_dir_reopen <- file.path(simulation_output, "_overflow_simulations", exp_name_reopen)




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

exp_name_regreopen_combined <- "20200919_IL_regreopen_combined"


#### --------------------------------------
#### 1- Region characteristics -> region_characteristics.csv
#### --------------------------------------
chunk1=F
if (chunk1) {
  dat <- f_region_characteristics()
  summary(dat$icubeds_per10th)
  print(dat)
  fwrite(dat, file.path(outdir,"tables", "region_characteristics.csv"), quote=FALSE)
  
  
  ## load refdat
  
  l
}

#### --------------------------------------
####  2- Baseline - get characteristics
#### --------------------------------------

### Extract baseline transmission information and add to region characteristics
### Use civis results format


chunk2 <- F
if (chunk2) {
  #### --------------------------------------
  ####  Baseline - predicted ICU in the past
  #### --------------------------------------

  exp_name <- exp_name_baseline
  exp_dir <- exp_dir_baseline
  simdat <- f_load_single_exp(exp_dir,paramvars = c("capacity_multiplier"))
  pplot <- f_icu_timeline(dat = simdat, subregions = unique(simdat$geography_name), selected_channel = "crit_det") +
    facet_wrap(~geography_name, scales="free")

  #### --------------------------------------
  ####  Baseline -  How often in the past did overflow happen?
  #### --------------------------------------

  exp_name <- exp_name_baseline
  exp_dir <- exp_dir_baseline
  simdat <- f_load_single_exp(exp_dir)
  simdat$date <- as.Date(simdat$date)
  simdat <- simdat %>% filter(date <= Sys.Date())
  (ndates <- length(unique(simdat$date)))
  
  ### Current ICU
  simdat %>% filter(channel=="crit_det" & geography_name %in% c(1,4,11) & 
                      date ==as.Date("2020-09-01"))  %>%
    group_by(geography_name) %>%
    View()

  ICU_threshold_past <- list()
  for (capacity_threshold in seq(0.2, 1, 0.2)) {
    ICU_threshold_past[[length(ICU_threshold_past) + 1]] <- simdat %>%
      group_by(geography_name) %>%
      filter(channel == "crit_det") %>%
      mutate(capacity_multiplier = capacity_threshold) %>%
      filter(median.value >= icu_available * capacity_multiplier) %>%
      add_tally()
  }
  ICU_threshold_past <- ICU_threshold_past %>% bind_rows()
  table(ICU_threshold_past$capacity_multiplier, ICU_threshold_past$geography_name)

  ICU_threshold_past %>%
    dplyr::select(geography_name, capacity_multiplier, date) %>%
    arrange(geography_name, capacity_multiplier, date)

  pplot <- ggplot(data = ICU_threshold_past) +
    geom_point(aes(x = date, y = capacity_multiplier)) +
    facet_wrap(~geography_name)
  f_save_plot(pplot = pplot, plot_name = "ICUcapacity_thresholds_in_the_past", plot_dir = file.path(outdir))
}

#### --------------------------------------
####  Counterfactual - varying reopening - 2_counterfactual_description.R
#### --------------------------------------
chunk3 <- F
if (chunk3) {
  exp_name <- exp_name_reopen
  exp_dir <- exp_dir_reopen
  simdat <- f_load_single_exp(exp_dir = exp_dir, paramvars = c("reopening_multiplier_4"))
  unique(simdat$geography_name)
  unique(simdat$reopening_multiplier_4)
  pplot <- f_icu_timeline(dat = simdat, subregions = c("1"), selected_channel = "crit_det", facetVar = "reopening_multiplier_4")
  pplot <- f_icu_timeline(dat = simdat, subregions = c("4"), selected_channel = "crit_det", facetVar = "reopening_multiplier_4")
  pplot <- f_icu_timeline(dat = simdat, subregions = c("11"), selected_channel = "crit_det", facetVar = "reopening_multiplier_4")
  
  ## Rebound values
  rebound <- f_get_rebound_values(dat = simdat)

  ### Timeline plot
  for (i in unique(simdat$geography_name)) {
    selected_channel = "crit_det"
    pplot <- f_icu_timeline(dat = simdat, subregions = c(i), selected_channel = selected_channel, facetVar = "reopening_multiplier_4")
    ggsave(paste0("timeline_", selected_channel, "_region_", i, ".png"),
      plot = pplot, path = file.path(exp_dir, "_plots"), width = 14, height = 8, device = "png"
    )
  }

  ##### Peak in ICU
  ICUcumul_out <- f_describe_ICU_cumul(facetVar = "reopening_multiplier_4")

  ##### More ICU beds needed at peak
  ICUpeak_out <- f_describe_ICU_peak(facetVar = "reopening_multiplier_4")
  ICUpeak_out[[2]] %>% as.data.frame()
}

#### --------------------------------------
####  Triggered reopening - predicted ICU
#### --------------------------------------
chunk4 <- F
if (chunk4) {
  exp_names <- exp_names_sm7

  for (exp_name in exp_names) {
    exp_name_sub <- gsub("20200919_IL_", "", exp_name)
    exp_dir <- file.path(simulation_output, "_overflow_simulations", exp_name)
    simdat <- f_load_single_exp(exp_dir)
    out <- f_describe_peak_and_cumul(dat = simdat, subfolder = exp_name_sub)
    rm(out, simdat)
  }

  ##### Combined plot
  exp_name <- exp_name_regreopen_combined
  exp_name_sub <- exp_name
  exp_dir <- file.path(simulation_output, "_overflow_simulations", exp_name)
  list_csvs <- list.files(file.path(simulation_output, "_overflow_simulations"), pattern = "trajectoriesDat_sub_long.csv", recursive = TRUE)
  
  simdate="20200919"
  list_csvs <- list_csvs[grep(simdate, list_csvs)]
  
  for (subregion in c(1:11)) {
    print(subregion)
    # subregion <- c("11")
    out1 <- f_stacked_barplot(dflist = list_csvs, subregions = subregion, rollback = "sm4", reopen = "50perc", exp_name_sub)
    out2 <- f_stacked_barplot(dflist = list_csvs, subregions = subregion, rollback = "sm4", reopen = "100perc", exp_name_sub)
    out3 <- f_stacked_barplot(dflist = list_csvs, subregions = subregion, rollback = "sm7", reopen = "50perc", exp_name_sub)
    out4 <- f_stacked_barplot(dflist = list_csvs, subregions = subregion, rollback = "sm7", reopen = "100perc", exp_name_sub)

    pplot <- plot_grid(f_remove_legend(out1[[2]]), f_remove_legend(out3[[2]]), f_remove_legend(out2[[2]]), f_remove_legend(out4[[2]]))
    if(SAVE)f_save_plot(pplot = pplot, plot_name = paste0("barplot_reg_", subregion), plot_dir = file.path(outdir), width = 8, height = 7)
    
    rm(out3, out4)
    out3 <- f_stacked_barplot(dflist = list_csvs, subregions = subregion, rollback = "sm7", reopen = "50perc", exp_name_sub, stackLike = T)
    out4 <- f_stacked_barplot(dflist = list_csvs, subregions = subregion, rollback = "sm7", reopen = "100perc", exp_name_sub, stackLike = T)

    pplot2 <- plot_grid(f_remove_legend(out4[[2]]), f_remove_legend(out3[[2]]), ncol = 1)
    if(SAVE)f_save_plot(pplot = pplot2, plot_name = paste0("barplot2b_reg_", subregion), plot_dir = file.path(outdir), width = 7, height = 10)

    rm(pplot, subregion)
  }

  #### Different barplot with errorbars
  for (subregion in c(1:11)) {
    print(subregion)
    # subregion <- c("11")
    rm(out3, out4)
    out3 <- f_stacked_barplot_errorbars(dflist = list_csvs, subregions = subregion, rollback = "sm7", reopen = "50perc", exp_name_sub, stackLike = T)
    out4 <- f_stacked_barplot_errorbars(dflist = list_csvs, subregions = subregion, rollback = "sm7", reopen = "100perc", exp_name_sub, stackLike = T)

    pplot2 <- plot_grid(f_remove_legend(out4[[2]]), f_remove_legend(out3[[2]]), ncol = 1)
    f_save_plot(pplot = pplot2, plot_name = paste0("barplot2b_erorbars_delay0_reg_", subregion), plot_dir = file.path(outdir), width = 7, height = 10)

    rm(pplot2, subregion)
  }


  # f_save_plot(pplot = out1[[2]], plot_name = "barplot_legend1", plot_dir = file.path(outdir, exp_name_sub), width = 8, height = 6)
  # f_save_plot(pplot = out4[[2]], plot_name = "barplot_legend2", plot_dir = file.path(outdir, exp_name_sub), width = 8, height = 6)

  
  ###### For text
  exp_name <- exp_names_sm7[1]
  
  exp_dir <- file.path(simulation_output, "_overflow_simulations", exp_name)
  simdat <- f_load_single_exp(exp_dir)
  tab_peak_50 <- f_get_peak_numbers()
  tab_peak_50 %>% filter(geography_name %in% c(1,4,11) & capacity_multiplier >0.77 )
  
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

  
  
  ###### For text
  exp_name <- exp_names_sm4[2]
  exp_dir <- file.path(simulation_output, "_overflow_simulations", exp_name)
  simdat <- f_load_single_exp(exp_dir)
  tab_peak_100 <- f_get_peak_numbers()
  tab_peak_100 %>% filter(geography_name %in% c(1,4,11) & capacity_multiplier >0.77 ) 
  
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
}


#### --------------------------------------
####  Triggered reopening - Probability
#### --------------------------------------
#### --------------------------------------
chunk5 <- F
if (chunk5) {
  exp_names <- c(exp_names_sm4, exp_names_sm7)

  for (exp_name in exp_names) {
    simdate <- strsplit(exp_name, "_")[[1]][1]
    exp_name_sub <- gsub(paste0(simdate, "_IL_"), "", exp_name)
    exp_dir <- file.path(simulation_output, "_overflow_simulations", exp_name)
    simdat <- f_get_probabilities(exp_dir)
    rm(simdat)
  }

  ## Analyze combines probabilities
  exp_name <- exp_name_regreopen_combined
  exp_name_sub <- exp_name
  exp_dir <- file.path(simulation_output, "_overflow_simulations", exp_name)

  ## Combine probability files
  exp_dirs <- list.files(file.path(simulation_output, "_overflow_simulations"), pattern = "propDat_sim.Rdata", recursive = T, full.names = T)
  propDat_sim <- f_combine_Rdata(exp_dirs)


  ## Create custom grouping variable for delay and reopen
  propDat_sim$grpVar <- paste0(gsub("none", "0days", gsub(" ", "", propDat_sim$delay)), "-", propDat_sim$reopen)

  delay_reopen <- c(
    "7days-100perc", "3days-100perc", "0days-100perc",
    "7days-50perc", "3days-50perc", "0days-50perc"
  )

  propDat_sim$grpVar <- factor(propDat_sim$grpVar, levels = delay_reopen, labels = delay_reopen)

  if (SAVE) fwrite(propDat_sim, file.path(exp_dir, "propDat_sim_combined.csv"), quote = FALSE)
  LOAD <- F
  if (LOAD) {
    exp_name <- exp_name_regreopen_combined
    exp_name_sub <- exp_name
    exp_dir <- file.path(simulation_output, "_overflow_simulations", exp_name)
    propDat_sim <- fread(file.path(exp_dir, "propDat_sim_combined.csv"))
  }


  ### Differences across regions
  f_custom_prob_plot2(
    dat = subset(propDat_sim), exp_dir = exp_dir,
    plot_name = "ICUoverflow_proball_compare_regions",
    SAVE = SAVE
  )
  f_custom_prob_plot2(
    dat = subset(propDat_sim), exp_dir = exp_dir, subregions = c(1, 4, 11),
    plot_name = "ICUoverflow_proball_compare_regions1-4-11", width = 12, height = 8,
    SAVE = SAVE
  )

  ### Probability plots
  propDat_sim$geography_name <- factor(propDat_sim$geography_name, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))
  f_custom_prob_plot(
    dat = subset(propDat_sim), exp_dir = exp_dir,
    plot_name = "ICUoverflow_proball_perCovidRegion", reopenFacet = FALSE,
    SAVE = SAVE
  )

  f_custom_prob_plot(
    dat = subset(propDat_sim, geography_name %in% c("1", "4", "11")), exp_dir = exp_dir,
    plot_name = "ICUoverflow_proball_perCovidRegion_sub1", width = 15, height = 6, reopenFacet = FALSE,
    SAVE = SAVE
  )

  f_custom_prob_plot(
    dat = subset(propDat_sim, geography_name %in% c("1", "4", "11") & delay %in% c("none")), exp_dir = exp_dir,
    plot_name = "ICUoverflow_proball_perCovidRegion_sub1b", width = 15, height = 6, reopenFacet = FALSE,
    SAVE = SAVE
  )

  f_custom_prob_plot(
    dat = subset(propDat_sim, geography_name %in% c("1", "4", "11")), exp_dir = exp_dir,
    plot_name = "ICUoverflow_proball_perCovidRegion_sub2", width = 15, height = 9,
    SAVE = SAVE
  )
}


#### --------------------------------------
####  Triggered reopening - Probability - Compare with characteristics
#### --------------------------------------
chunk6 <- F
if (chunk6) {
  exp_name <- exp_name_regreopen_combined
  exp_name_sub <- exp_name
  exp_dir <- file.path(simulation_output, "_overflow_simulations", exp_name)
  #### Compare with characteristics
  propDat_sim <- fread(file.path(exp_dir, "propDat_sim_combined.csv"))
  propDat_sim <- propDat_sim %>%
    dplyr::select(-pop, -icu_available) %>%
    left_join(f_region_characteristics(), by = c("geography_name"))


  ICU_threshold_future <- f_generate_prob_treshold_dat(propDat_sim)
  table(ICU_threshold_future$risk_tolerance, ICU_threshold_future$geography_name)
  unique(ICU_threshold_future$risk_tolerance)


  ### By ICU capacity per pop
  ggplot(data = subset(ICU_threshold_future, risk_tolerance < 0.9)) +
    geom_jitter(aes(x = icubeds_per10th, y = capacity_multiplier, col = as.factor(risk_tolerance), group = geography_name), size = 3) +
    facet_wrap(~exp_name)

  ### By rebound
  table(ICU_threshold_future$reopen)
  rebound <- rebound %>%
    rename(`100perc` = fast_rebound, `50perc` = moderate_rebound) %>%
    pivot_longer(cols = -"geography_name", names_to = "reopen", values_to = "reopen_value")
  ICU_threshold_future <- ICU_threshold_future %>% left_join(rebound, by = c("geography_name", "reopen"))

  ggplot(data = subset(ICU_threshold_future, risk_tolerance < 0.9)) +
    geom_jitter(aes(x = risk_tolerance, y = capacity_multiplier, col = as.factor(reopen_value), group = geography_name), size = 2) +
    geom_smooth(aes(x = risk_tolerance, y = capacity_multiplier, col = as.factor(reopen_value), group = reopen_value), size = 1, se = FALSE, method = "lm") +
    facet_wrap(~exp_name)
}


#### --------------------------------------
####  Triggered reopening - Probability - risk tolerance plot
#### --------------------------------------
chunk7 <- F
if (chunk7) {
  pplot <- f_ICU_tolerance_plot(dat = subset(ICU_threshold_future), byRollback = T)
  pplot2 <- f_ICU_tolerance_plot(dat = subset(ICU_threshold_future), byRollback = F)
  f_save_plot(pplot = pplot, plot_name = "risk-tolerance_capacity_delay", plot_dir = file.path(outdir), width = 8, height = 3)
  f_save_plot(pplot = pplot2, plot_name = "risk-tolerance_capacity_rollback", plot_dir = file.path(outdir), width = 8, height = 3)


  ### For supplement and description, plots per region
  for (reg in unique(ICU_threshold_future$geography_name)) {
    f_ICU_tolerance_plot(dat = subset(ICU_threshold_future, rollback == "sm7"), reg = reg)

    f_save_plot(pplot = pplot, plot_name = paste0("risk-tolerance_capacity_overall_reg_", reg), plot_dir = file.path(outdir), width = 8, height = 3)
    rm(pplot)
  }
}

#### --------------------------------------
####  Triggered reopening - Compare recommendation vs generic
#### --------------------------------------
## Already covered in stacked barplot like figure
chunk8 <- F
if (chunk8) {
  compare_recommended_vs_generic <- FALSE
  if (compare_recommended_vs_generic) {
    exp_name <- exp_name_regreopen_combined
    exp_name_sub <- exp_name
    exp_dir <- file.path(simulation_output, "_overflow_simulations", exp_name)
    list_csvs <- list.files(file.path(simulation_output, "_overflow_simulations"), pattern = "trajectoriesDat_sub_long.csv", recursive = T, full.names = T)

    propDat_sim <- fread(file.path(exp_dir, "propDat_sim_combined.csv"))

    simdat <- f_combine_csv_from_list(list_csvs) %>%
      dplyr::filter(channel == "crit_det") %>%
      dplyr::filter(exp_name != "20200919_IL_gradual_reopening_sm7")

    simdat_compare <- f_generate_generic_recommended_dat()

    ggplot(data = subset(simdat_compare, exp_name == "20200919_IL_regreopen100perc_0daysdelay_sm7")) +
      geom_line(aes(x = date, y = median.value, col = risk_tolerance)) +
      facet_wrap(~geography_name, scales = "free") +
      customTheme

    simdat_comparePeak <- simdat_compare %>%
      group_by(geography_name, risk_tolerance, exp_name) %>%
      filter(median.value == max(median.value)) %>%
      filter(date == min(date))

    ggplot(data = subset(simdat_comparePeak, geography_name != "illinois")) +
      geom_boxplot(aes(x = geography_name, y = median.value, col = risk_tolerance, group = interaction(risk_tolerance, geography_name))) +
      customTheme
  }

  exportForQgis <- F
  if (exportForQgis) {
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
  }
}


## --------------------------------
### Rt comparison
## --------------------------------
chunk9 <- F
if (chunk9) {
  exploreRt_baseline_ICUprob <- FALSE
  if (exploreRt_baseline_ICUprob) {
    ## Get Rt
    ## Get rebound and combine
    ## Get probabilities and combine
    ## Get starting value and combine


    ### Load Rt files
    exp_name <- "20200917_IL_gradual_reopening" #exp_name_reopen
    Rtdat <- fread(file.path(simulation_output, "_overflow_simulations", exp_name, "estimatedRt", "combined_estimated_Rt.csv"))

    ### + 120 as trimmed trajectories was used
    Rtdat <- Rtdat %>%
      rename(
        rt_median = `Median(R)`,
        rt_lower = `Quantile.0.025(R)`,
        rt_upper = `Quantile.0.975(R)`
      ) %>%
      mutate(
        date = as.Date("2020-02-13") + t_end + 120,
        geography_name = gsub("EMS-", "", region),
        geography_name = gsub("All", "illinois", geography_name)
      ) %>%
      filter(date >= as.Date("2020-09-01") & date <= as.Date("2020-12-01")) %>%
      as.data.frame()

    rebound <- f_get_rebound_values()
    Rtdat <- Rtdat %>% left_join(rebound, by = "geography_name")

    Rtdat_fast <- Rtdat %>%
      filter(reopening_multiplier_4 == fast_rebound) %>%
      mutate(rebound = "fast") %>%
      dplyr::select(-fast_rebound)
    Rtdat_moderate <- Rtdat %>%
      filter(reopening_multiplier_4 == moderate_rebound) %>%
      mutate(rebound = "moderate") %>%
      dplyr::select(colnames(Rtdat_fast))
    Rtdat <- rbind(Rtdat_fast, Rtdat_moderate)
    
    f_rt_timeline(dat = Rtdat, subregions = c(1,4, 11), selected_channel = "median_rt", facetVar = "geography_name")

    Rtdat_peak <- Rtdat %>%
      group_by(geography_name, rebound) %>%
      filter(rt_median == max(rt_median)) %>%
      filter(date == min(date)) %>%
      rename(rt_median_peak = rt_median) %>%
      dplyr::select(geography_name, rebound, rt_median_peak)
    Rtdat_baseline <- Rtdat %>%
      filter(date > as.Date("2020-09-30") & date <= as.Date("2020-10-01")) %>%
      rename(rt_median_base = rt_median) %>%
      dplyr::select(geography_name, rebound, rt_median_base)
    Rtdat_compare <- Rtdat_peak %>%
      left_join(Rtdat_baseline, by = c("geography_name", "rebound")) %>%
      mutate(rt_ratio = rt_median_peak / rt_median_base, rt_diff = rt_median_peak - rt_median_base) %>%
      mutate(reopen = ifelse(rebound == "fast", "100perc", "50perc"))


    ### Get baseline ICU
    exp_name <- exp_name_regreopen_combined
    exp_name_sub <- exp_name
    exp_dir <- file.path(simulation_output, "_overflow_simulations", exp_name)
    list_csvs <- list.files(file.path(simulation_output, "_overflow_simulations"), pattern = "trajectoriesDat_sub_long.csv", recursive = T, full.names = T)
    simdat <- f_combine_csv_from_list(list_csvs) %>%
      dplyr::filter(channel == "crit_det") %>%
      dplyr::filter(exp_name != "20200919_IL_gradual_reopening_sm7") %>%
      dplyr::filter(date > as.Date("2020-09-30") & date <= as.Date("2020-10-01")) %>%
      dplyr::select(geography_name, date, exp_name, median.value, pop, icu_available) %>%
      mutate(
        icu_ratio = median.value / icu_available,
        icu_diff = median.value - icu_available,
        icu_available_per10th = (icu_available / pop) * 10000,
        median.value_per10th = (median.value / pop) * 10000,
        icu_diff_per10th = median.value_per10th - icu_available_per10th
      )

    summary(as.Date(simdat$date))
    summary(simdat$icu_ratio)
    summary(simdat$icu_diff)
    summary(simdat$icu_diff_per10th)

    ### add overflow probabilities
    propDat_sim <- fread(file.path(exp_dir = file.path(simulation_output, "_overflow_simulations", "20200919_IL_regreopen_combined"), "propDat_sim_combined.csv"))
    propDat_sim <- propDat_sim %>% left_join(Rtdat_compare, by = c("geography_name", "reopen"))
    propDat_sim <- propDat_sim %>% left_join(simdat, by = c("geography_name", "exp_name"))


    ## Explorative plots
    ggplot(data = subset(propDat_sim, geography_name %in% c(1:11) & capacity_multiplier == unique(propDat_sim$capacity_multiplier)[6])) +
      geom_point(aes(x = rt_ratio, y = prob_overflow, group = exp_name))
    ggplot(data = subset(propDat_sim, prob_overflow >= 0.45 & prob_overflow <= 0.5)) +
      geom_jitter(aes(x = rt_ratio, y = capacity_multiplier, col = reopen))
    ggplot(data = subset(propDat_sim, prob_overflow >= 0.45 & prob_overflow <= 0.5)) +
      geom_jitter(aes(x = rt_ratio, y = capacity_multiplier, col = delay))
    ggplot(data = subset(propDat_sim, prob_overflow >= 0.45 & prob_overflow <= 0.5)) +
      geom_jitter(aes(x = rt_ratio, y = capacity_multiplier, col = rollback))


    ggplot(data = subset(propDat_sim, geography_name %in% c(1:11) & capacity_multiplier == unique(propDat_sim$capacity_multiplier)[6])) +
      geom_point(aes(x = icu_diff_per10th, y = prob_overflow, group = exp_name))
    ggplot(data = subset(propDat_sim, prob_overflow >= 0.45 & prob_overflow <= 0.5)) +
      geom_jitter(aes(x = icu_diff_per10th, y = capacity_multiplier, col = reopen))
    ggplot(data = subset(propDat_sim, prob_overflow >= 0.45 & prob_overflow <= 0.5)) +
      geom_jitter(aes(x = icu_diff_per10th, y = capacity_multiplier, col = delay))
    ggplot(data = subset(propDat_sim, prob_overflow >= 0.45 & prob_overflow <= 0.5)) +
      geom_jitter(aes(x = icu_diff_per10th, y = capacity_multiplier, col = rollback))


    ggplot(data = subset(propDat_sim, geography_name %in% c(1:11) & capacity_multiplier == unique(propDat_sim$capacity_multiplier)[6])) +
      geom_point(aes(x = icu_available_per10th, y = prob_overflow, group = exp_name))
    ggplot(data = subset(propDat_sim, prob_overflow >= 0.45 & prob_overflow <= 0.5)) +
      geom_jitter(aes(x = icu_available_per10th, y = capacity_multiplier, col = reopen))
    ggplot(data = subset(propDat_sim, prob_overflow >= 0.45 & prob_overflow <= 0.5)) +
      geom_jitter(aes(x = icu_available_per10th, y = capacity_multiplier, col = delay))
    ggplot(data = subset(propDat_sim, prob_overflow >= 0.45 & prob_overflow <= 0.5)) +
      geom_jitter(aes(x = icu_available_per10th, y = capacity_multiplier, col = rollback))


    ggplot(data = subset(propDat_sim, geography_name %in% c(1:11) & capacity_multiplier == unique(propDat_sim$capacity_multiplier)[6])) +
      geom_point(aes(x = icu_diff_per10th, y = rt_ratio, group = exp_name, col = prob_overflow))
  }
}
