#### ===========================================
### Explorative code for capacity overflow manuscript
#### ===========================================

library(tidyverse)
library(cowplot)
library(data.table)
library(raster)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")

theme_set(theme_cowplot())

capacity_multiplier2_cols <- c("#d0d1e6", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0")
delay_cols <- c("#d0d1e6", "#7fcdbb", "#1d91c0")
rollback_cols <- c("#d0d1e6", "#1d91c0")


outdir <- file.path("C:/Users/mrm9534/Box/MR_archive/testfigures2")
simulation_output <- file.path(simulation_output, "overflow_simulations")

popDat <- load_population()
capacityDat <- load_new_capacity()

dat <- left_join(popDat, capacityDat, by = c("geography_name"))
dat$icu_available <- as.numeric(dat$icu_available)
dat$pop <- as.numeric(dat$pop)
dat$popDens <- (dat$pop / 10000)
dat$icubeds_per10th <- (dat$icu_available / dat$pop) * 10000
dat$medsurgbeds_per10th <- (dat$medsurg_available / dat$pop) * 10000

summary(dat$icubeds_per10th)

fwrite(dat, file = file.path(outdir, "region_characteristics.csv"), quote = FALSE, row.names = FALSE)

### Load simulation data
load_sim_data <- function(exp_names) {
  
  simdatList <- list()
  
  for (exp_name in exp_names) {
    trajectoriesDat <- fread(file = file.path(simulation_output, exp_name, "trajectoriesDat_trim.csv"))
    summary(trajectoriesDat$scen_num)
    summary(trajectoriesDat$sample_num)
    
    outcomeVars <- c("All", paste0("EMS-", c(1:11)))
    outcomeVars <- paste0("crit_det_", outcomeVars)
    outVars <- c("date", "scen_num", "sample_num", "capacity_multiplier", outcomeVars)
    
    simdat <- trajectoriesDat %>%
      dplyr::mutate(date = as.Date(startdate) + time) %>%
      dplyr::select(outVars) %>%
      pivot_longer(cols = -c(date, scen_num, sample_num, capacity_multiplier)) %>%
      separate(name, into = c("channel", "det", "geography_name"), sep = "_") %>%
      mutate(
        exp_name = exp_name,
        geography_name = gsub("EMS-", "", geography_name),
        geography_name = gsub("All", "illinois", geography_name),
        exp_label = gsub("20200919_IL_regreopen", "", exp_name)
      ) %>%
      separate(exp_label, into = c("reopen", "delay", "rollback"), sep = "_") %>%
      left_join(dat, by = "geography_name")
    
    simdatList[[length(simdatList) + 1]] <- simdat
  }
  
  

  
  simdat <- simdatList %>% bind_rows()
  rm(simdatList)
  
  
  simdat$rollback <- factor(simdat$rollback,
                            levels = c("sm7", "sm4"),
                            labels = c("sm7", "sm4")
  )
  
  simdat$reopen <- factor(simdat$reopen,
                          levels = rev(c("100perc", "50perc")),
                          labels = rev(c("100perc", "50perc"))
  )
  
  simdat$delay <- factor(simdat$delay,
                         levels = rev(c("0daysdelay", "3daysdelay", "7daysdelay")),
                         labels = rev(c("none", "3 days", "7 days"))
  )
  
  
  simdat$capacity_multiplier2 <- factor(simdat$capacity_multiplier,
                                        levels = rev(c("0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1")),
                                        labels = rev(c("0.4", "0.5", "0.6", "0.7", " 0.8", "0.9", "1"))
  )
  
  simdat$region <- factor(simdat$geography_name,
                          levels = c("illinois", c(1:11)),
                          labels = c("illinois", c(1:11))
  )
  
  
  return(simdat)
}

### Load shp
get_popDensity <- function(SAVE = FALSE) {
  shp_path <- file.path(data_path, "covid_IDPH", "shapefiles")
  
  # county_populations.R
  pop_per_county <- fread(file.path(data_path, "covid_IDPH", "EMS Population", "covidregion_population_by_county.csv")) %>%
    rename(county = County)
  
  
  counties_shp <- shapefile(file.path(shp_path, "covid_regions/counties.shp"))
  crs(counties_shp)
  counties_shp$area_sqkm <- area(counties_shp) / 1000000
  
  counties_shp <- merge(counties_shp, pop_per_county, by = "county")
  counties_dat <- as.data.frame(counties_shp) %>%
    mutate(popDensity = pop / area_sqkm)
  
  covidRegion_dat <- counties_dat %>%
    group_by(new_restor) %>%
    summarize(pop = sum(pop), area_sqkm = sum(area_sqkm)) %>%
    mutate(popDensity = pop / area_sqkm)
  
  
  if (SAVE) {
    fwrite(counties_dat, file = file.path(data_path, "covid_IDPH", "EMS Population", "counties_popDensity_R.csv"), quote = FALSE, row.names = FALSE)
    fwrite(covidRegion_dat, file = file.path(data_path, "covid_IDPH", "EMS Population", "covidRegion_popDensity_R.csv"), quote = FALSE, row.names = FALSE)
  }
}

get_baselineRt <- function() {
  simdat <- fread(file.path(project_path, "NU_civis_outputs/20200929/csv/nu_20200929.csv"))
  
  initialDate <- as.Date("2020-03-01")
  baselineDate <- Sys.Date()
  
  initialRt <- simdat %>%
    filter(date == initialDate) %>%
    select(geography_modeled, date, rt_median, rt_lower, rt_upper) %>%
    mutate(
      geography_modeled = gsub("covidregion_", "", geography_modeled),
      note = paste0("extracted from nu_20200929.csv"),
      rt_type = paste0("initial")
    )
  
  lowestRt <- simdat %>%
    group_by(geography_modeled) %>%
    filter(date <= Sys.Date()) %>%
    filter(rt_median == min(rt_median)) %>%
    select(geography_modeled, date, rt_median, rt_lower, rt_upper, date) %>%
    mutate(
      geography_modeled = gsub("covidregion_", "", geography_modeled),
      note = paste0("extracted from nu_20200929.csv"),
      rt_type = paste0("lowest ")
    )
  
  baselineRt <- simdat %>%
    filter(date == baselineDate) %>%
    select(geography_modeled, date, rt_median, rt_lower, rt_upper) %>%
    mutate(
      geography_modeled = gsub("covidregion_", "", geography_modeled),
      note = paste0("extracted from nu_20200929.csv"),
      rt_type = paste0("baseline ")
    )
  
  
  Rtdat <- rbind(initialRt, lowestRt, baselineRt)
  fwrite(Rtdat, file.path(project_path, "project_notes", "estimated_baseline_Rt.csv"))
}

### Figure 3 ?
trajectoriesPlot <- FALSE
if (trajectoriesPlot) {
  
  exp_combined <- "20200919_IL_regreopen_combined"
  fname <- "combined_dataframe.Rdata"
  #dir.create(file.path(simulation_output, exp_combined))
  
  if (!file.exists(file.path(simulation_output, exp_combined, fname))) {
    exp_names <- list.dirs(simulation_output, recursive = FALSE, full.names = FALSE)
    exp_names <- exp_names[grep("20200919_IL_regreopen", exp_names)]
    exp_names <- exp_names[!(exp_names =="20200919_IL_regreopen100perc_0daysdelay_sm4")]
    
    simdat <- load_sim_data(exp_names = exp_names)
    save(simdat,file=file.path(simulation_output, exp_combined, fname) )
  } else {
    load(file.path(simulation_output, exp_combined, fname))
  }
  
  table(simdat$exp_name)
  table(simdat$capacity_multiplier)
  table(simdat$capacity_multiplier2)
  table(simdat$reopen)
  table(simdat$rollback)
  table(simdat$delay)
  table(simdat$region)
  
  trajectoriesPlot_2 <- function() {
    
    p1 <- ggplot(data = subset(simdat, geography_name == "illinois" & capacity_multiplier >= 0.8 & capacity_multiplier <= 1& delay == "none")) +
      geom_line(aes(x = date, y = value, col = as.factor(rollback), group = interaction(scen_num, rollback)), alpha = 1) +
      geom_hline(aes(yintercept = icu_available)) +
      facet_wrap(~reopen, scales = "free", ncol = 1) +
      labs(color = "", x = "", y = "predicted ICU census") +
      theme(legend.position = "none") +
      scale_color_manual(values = rollback_cols) +
      customThemeNoFacet
    
    
    p2 <- ggplot(data = subset(simdat, geography_name == "illinois"  & capacity_multiplier >= 0.8 & capacity_multiplier <= 1& rollback == "sm4")) +
      geom_line(aes(x = date, y = value, col = as.factor(delay), group = interaction(scen_num, delay)), alpha = 1) +
      geom_hline(aes(yintercept = icu_available)) +
      facet_wrap(~reopen, scales = "free", ncol = 1) +
      labs(color = "", x = "", y = "predicted ICU census") +
      theme(legend.position = "none") +
      scale_color_manual(values = delay_cols) +
      customThemeNoFacet
    
    p3 <- ggplot(data = subset(simdat, geography_name == "illinois" & !is.na(capacity_multiplier) & delay == "none" & rollback == "sm4")) +
      geom_line(aes(x = date, y = value, col = as.factor(capacity_multiplier), group = interaction(scen_num, capacity_multiplier2)), alpha = 1) +
      geom_hline(aes(yintercept = icu_available)) +
      facet_wrap(~reopen, scales = "free", ncol = 1) +
      labs(color = "", x = "", y = "predicted ICU census") +
      theme(legend.position = "none") +
    #  scale_color_manual(values = capacity_multiplier2_cols) +
      customThemeNoFacet
    
    pplot <- plot_grid(p1, p2, p3, nrow = 1)
    pplot
    
    
    ggsave(paste0("trajectories", ".png"),
           plot = pplot, path = file.path(outdir), width = 10, height = 6, device = "png"
    )
    ggsave(paste0("trajectories", ".pdf"),
           plot = pplot, path = file.path(outdir), width = 10, height = 6, device = "pdf"
    )
    
    
    #### Per region
    p1 <- ggplot(data = subset(simdat, geography_name %in% c("1", "7", "11") & capacity_multiplier == 0.8 & delay == "none" & reopen == "100perc")) +
      geom_line(aes(x = date, y = value, col = as.factor(rollback), group = interaction(scen_num, rollback)), alpha = 1) +
      geom_hline(aes(yintercept = icu_available)) +
      facet_wrap(~region, scales = "free", nrow = 1) +
      labs(color = "", x = "", y = "predicted ICU census") +
      theme(legend.position = "none") +
      scale_color_manual(values = rollback_cols) +
      customThemeNoFacet
    
    
    p2 <- ggplot(data = subset(simdat, geography_name %in% c("1", "7", "11") & capacity_multiplier == 0.8 & rollback == "sm4" & reopen == "100perc")) +
      geom_line(aes(x = date, y = value, col = as.factor(delay), group = interaction(scen_num, delay)), alpha = 1) +
      geom_hline(aes(yintercept = icu_available)) +
      facet_wrap(~region, scales = "free", nrow = 1) +
      labs(color = "", x = "", y = "predicted ICU census") +
      theme(legend.position = "none") +
      scale_color_manual(values = delay_cols) +
      customThemeNoFacet
    
    
    
    p3 <- ggplot(data = subset(simdat, geography_name %in% c("1", "7", "11") & !is.na(capacity_multiplier2) & delay == "none" & rollback == "sm4" & reopen == "100perc")) +
      geom_line(aes(x = date, y = value, col = as.factor(capacity_multiplier2), group = interaction(scen_num, capacity_multiplier2)), alpha = 1) +
      geom_hline(aes(yintercept = icu_available)) +
      facet_wrap(~region, scales = "free", nrow = 1) +
      labs(color = "", x = "", y = "predicted ICU census") +
      theme(legend.position = "none") +
      scale_color_manual(values = capacity_multiplier2_cols) +
      customThemeNoFacet
    
    pplot <- plot_grid(p1, p2, p3, ncol = 1)
    pplot
    
    
    ggsave(paste0("trajectories_perGrp", ".png"),
           plot = pplot, path = file.path(outdir), width = 7, height = 6, device = "png"
    )
    ggsave(paste0("trajectories_perGrp", ".pdf"),
           plot = pplot, path = file.path(outdir), width = 7, height = 6, device = "pdf"
    )
  }
  
  trajectoriesPlot_3 <- function() {
    simdatAggr <- simdat %>%
      filter(geography_name == "1" & date >= as.Date("2020-09-01") & date <= as.Date("2020-12-31")) %>%
      filter(capacity_multiplier == 0.8 & delay == "none") %>%
      group_by(date, rollback, reopen) %>%
      summarize(
        min.value = min(value, na.rm = TRUE),
        max.value = max(value, na.rm = TRUE),
        median.value = median(value, na.rm = TRUE),
        q25.value = quantile(value, probs = 0.25, na.rm = TRUE),
        q75.value = quantile(value, probs = 0.75, na.rm = TRUE),
        q2.5.value = quantile(value, probs = 0.025, na.rm = TRUE),
        q97.5.value = quantile(value, probs = 0.975, na.rm = TRUE),
        icu_available = mean(icu_available)
      )
    
    simdatAggr1 <- subset(simdatAggr, rollback == "sm6")
    simdatAggr2 <- subset(simdatAggr, rollback == "sm4")
    
    
    p1 <- ggplot(data = simdatAggr) +
      geom_ribbon(aes(x = date, ymin = 0, ymax = max.value, fill = as.factor(rollback)), alpha = 1) +
      # geom_ribbon(data = simdatAggr1,aes(x = date, ymin=0, ymax = max.value, fill = as.factor(rollback)), alpha = 1) +
      # geom_line(data = simdatAggr1,aes(x = date, y = q97.5.value, col = as.factor(rollback)), alpha = 1) +
      # geom_line(data = simdatAggr1,aes(x = date, y = q75.value, col = as.factor(rollback)), alpha = 1) +
      # geom_line(data = simdatAggr1,aes(x = date, y = median.value, col = as.factor(rollback)), alpha = 1) +
      # geom_ribbon(data = simdatAggr2,aes(x = date, ymin=0, ymax = max.value, fill = as.factor(rollback)), alpha = 1) +
      # geom_line(data = simdatAggr2,aes(x = date, y = q97.5.value, col = as.factor(rollback)), alpha = 1) +
      # geom_line(data = simdatAggr2,aes(x = date, y = q75.value, col = as.factor(rollback)), alpha = 1) +
      # geom_line(data = simdatAggr2,aes(x = date, y = median.value, col = as.factor(rollback)), alpha = 1) +
      geom_hline(aes(yintercept = icu_available)) +
      facet_wrap(~reopen, scales = "free", ncol = 1) +
      labs(fill = "", x = "", y = "predicted ICU census") +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = rollback_cols) +
      # scale_color_manual(values = c("azure4", "azure4" )) +
      customThemeNoFacet
    
    
    simdatAggr <- simdat %>%
      filter(geography_name == "11" & date >= as.Date("2020-09-01") & date <= as.Date("2020-12-31")) %>%
      filter(capacity_multiplier == 0.8 & rollback == "sm4") %>%
      group_by(date, delay, reopen) %>%
      summarize(
        min.value = min(value, na.rm = TRUE),
        max.value = max(value, na.rm = TRUE),
        median.value = median(value, na.rm = TRUE),
        q25.value = quantile(value, probs = 0.25, na.rm = TRUE),
        q75.value = quantile(value, probs = 0.75, na.rm = TRUE),
        q2.5.value = quantile(value, probs = 0.025, na.rm = TRUE),
        q97.5.value = quantile(value, probs = 0.975, na.rm = TRUE),
        icu_available = mean(icu_available)
      )
    
    
    p2 <- ggplot(data = simdatAggr) +
      geom_ribbon(aes(x = date, ymin = 0, ymax = max.value, fill = as.factor(delay)), alpha = 1) +
      geom_hline(aes(yintercept = icu_available)) +
      facet_wrap(~reopen, scales = "free", ncol = 1) +
      labs(fill = "", x = "", y = "predicted ICU census") +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = delay_cols) +
      customThemeNoFacet
    
    
    simdatAggr <- simdat %>%
      filter(geography_name == "11" & date >= as.Date("2020-09-01") & date <= as.Date("2020-12-31")) %>%
      filter(delay == "none" & rollback == "sm4") %>%
      group_by(date, capacity_multiplier, reopen) %>%
      summarize(
        min.value = min(value, na.rm = TRUE),
        max.value = max(value, na.rm = TRUE),
        median.value = median(value, na.rm = TRUE),
        q25.value = quantile(value, probs = 0.25, na.rm = TRUE),
        q75.value = quantile(value, probs = 0.75, na.rm = TRUE),
        q2.5.value = quantile(value, probs = 0.025, na.rm = TRUE),
        q97.5.value = quantile(value, probs = 0.975, na.rm = TRUE),
        icu_available = mean(icu_available)
      )
    
    simdatAggr$capacity_multiplier2 <- factor(simdatAggr$capacity_multiplier,
                                              levels = rev(c("0.4", "0.6", "0.8", "1")),
                                              labels = rev(c("0.4", "0.6", " 0.8", "1"))
    )
    
    
    
    p3 <- ggplot(data = subset(simdatAggr, !is.na(capacity_multiplier2))) +
      geom_ribbon(aes(x = date, ymin = 0, ymax = max.value, fill = as.factor(capacity_multiplier2)), alpha = 1) +
      geom_hline(aes(yintercept = icu_available)) +
      facet_wrap(~reopen, scales = "free", ncol = 1) +
      labs(fill = "", x = "", y = "predicted ICU census") +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = capacity_multiplier2_cols) +
      customThemeNoFacet
    
    
    
    
    pplot <- plot_grid(p1, p2, p3, nrow = 1)
    pplot
    
    
    ggsave(paste0("trajectories", ".png"),
           plot = pplot, path = file.path(outdir), width = 10, height = 6, device = "png"
    )
    ggsave(paste0("trajectories", ".pdf"),
           plot = pplot, path = file.path(outdir), width = 10, height = 6, device = "pdf"
    )
  }
}


#### Addition to method figure ? Not needed
compareToBaseline <- FALSE
if (compareToBaseline) {
  loadBaseline <- TRUE
  if (loadBaseline) {
    exp_baseline <- "20200826_IL_baseline"
    baselineDat <- fread(file.path(simulation_output, exp_baseline, "trajectoriesDat.csv"))
    
    ### Note use Ki_EMS-  or Ki_EMS.   not Ki_EMS_  (time varying vs input parameter)
    # colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))
    region_names <- c("All", paste0("EMS-", c(1:11)))
    outcomevars <- c(
      paste0("hospitalized_det_", region_names),
      paste0("hospitalized_", region_names),
      paste0("crit_det_", region_names),
      paste0("critical_", region_names)
    )
    
    
    region_names <- paste0("EMS-", c(1:11))
    outcomevars2 <- c(
      paste0("Ki_t_", region_names)
    )
    
    
    paramvars <- c("reopening_multiplier_4")
    keepvars <- c("time", "startdate", "scen_num", "sample_num", paramvars, outcomevars, outcomevars2)
    
    
    baselineDat <- baselineDat %>%
      select(keepvars) %>%
      filter(time > 120) %>%
      mutate(date = as.Date(startdate) + time) %>%
      pivot_longer(cols = -c("time", "date", "startdate", "reopening_multiplier_4"), names_to = "region") %>%
      separate(region, into = c("outcome", "region"), sep = "_EMS-") %>%
      mutate(
        region = as.numeric(region),
        exp_name = exp_name,
      ) %>%
      group_by(date, region, exp_name, reopening_multiplier_4, outcome) %>%
      summarize(
        median.value = median(value, na.rm = TRUE),
        q2.5.value = quantile(value, probs = 0.025, na.rm = TRUE),
        q97.5.value = quantile(value, probs = 0.975, na.rm = TRUE)
      )
  }
  
  simdate <- "20200826"
  exp_name <- "20200826_IL_RR_gradual_reopening_0"
  
  
  trajectoriesDat <- read_csv(file.path(simulation_output, exp_name, "trajectoriesDat.csv"))
  
  ### Note use Ki_EMS-  or Ki_EMS.   not Ki_EMS_  (time varying vs input parameter)
  # colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))
  region_names <- c("All", paste0("EMS-", c(1:11)))
  outcomevars <- c(
    paste0("hospitalized_det_", region_names),
    paste0("hospitalized_", region_names),
    paste0("crit_det_", region_names),
    paste0("critical_", region_names)
  )
  
  
  region_names <- paste0("EMS-", c(1:11))
  outcomevars2 <- c(
    paste0("Ki_t_", region_names)
  )
  
  
  paramvars <- c("reopening_multiplier_4")
  keepvars <- c("time", "startdate", "scen_num", "sample_num", paramvars, outcomevars, outcomevars2)
  
  
  paramvalues <- trajectoriesDat %>%
    select(keepvars) %>%
    filter(time > 120) %>%
    mutate(date = as.Date(startdate) + time) %>%
    pivot_longer(cols = -c("time", "date", "startdate", "reopening_multiplier_4"), names_to = "region") %>%
    separate(region, into = c("outcome", "region"), sep = "_EMS-") %>%
    mutate(
      region = as.numeric(region),
      exp_name = exp_name,
    ) %>%
    group_by(date, region, exp_name, reopening_multiplier_4, outcome) %>%
    summarize(
      median.value = median(value, na.rm = TRUE),
      q2.5.value = quantile(value, probs = 0.025, na.rm = TRUE),
      q97.5.value = quantile(value, probs = 0.975, na.rm = TRUE)
    )
  
  
  capacityDat <- load_new_capacity() %>% mutate(region = as.character(geography_name))
  
  
  paramvalues$region <- factor(paramvalues$region, levels = c(1:11), labels = c(1:11))
  capacityDat$region <- factor(capacityDat$region, levels = c(1:11), labels = c(1:11))
  paramvalues$reopening <- round(paramvalues$reopening_multiplier_4, 2) * 100
  
  dat <- paramvalues
  
  
  table(baselineDat$region)
  table(dat$region)
  
  baselineDat$region <- as.numeric(baselineDat$region)
  dat$region <- as.numeric(dat$region)
  capacityDat$region <- as.numeric(capacityDat$region)
  
  combinedDat <- rbind(baselineDat, dat)
  combinedDat <- combinedDat %>% filter(date <= as.Date("2020-12-31"))
  combinedDat <- left_join(combinedDat, capacityDat, by = "region")
  
  belowCapacityDat <- combinedDat %>%
    filter(date >= as.Date("2020-08-19") & date <= as.Date("2020-12-30") & outcome == "crit_det") %>%
    group_by(region, reopening_multiplier_4, outcome) %>%
    filter(median.value == max(median.value)) %>%
    mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0)) %>%
    select(region, outcome, reopening_multiplier_4, belowCapacity)
  
  combinedDat <- left_join(combinedDat, belowCapacityDat, by = c("region", "reopening_multiplier_4", "outcome"))
  
  pplot1 <- ggplot(data = subset(combinedDat, region %in% c(1, 7, 11) & outcome %in% c("crit_det"))) +
    geom_ribbon(aes(x = date, ymin = q2.5.value, ymax = q97.5.value, fill = as.factor(belowCapacity), group = as.factor(reopening_multiplier_4)), alpha = 0.2) +
    geom_line(aes(x = date, y = median.value, col = as.factor(belowCapacity), group = as.factor(reopening_multiplier_4)), size = 1) +
    geom_line(
      data = subset(combinedDat, region %in% c(1, 7, 11) & outcome %in% c("crit_det") & reopening_multiplier_4 == 0),
      aes(x = date, y = median.value, group = as.factor(reopening_multiplier_4)), col = "black", size = 1.2
    ) +
    facet_wrap(~region, scales = "free") +
    scale_color_manual(values = c("indianred", "deepskyblue3")) +
    geom_hline(aes(yintercept = icu_available), color = "darkgrey", linetype = "dashed", size = 1) +
    labs(x = "", y = "ICU census", color = "ICU overflow", fill = "ICU overflow") +
    customThemeNoFacet
  
  
  pplot2 <- dat %>%
    filter(region %in% c(1, 7, 11) & date >= as.Date("2020-08-19") & outcome == "crit_det") %>%
    group_by(region, reopening, outcome) %>%
    filter(median.value == max(median.value)) %>%
    left_join(capacityDat) %>%
    mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0)) %>%
    ggplot() +
    geom_errorbar(aes(x = as.factor(reopening), ymin = q2.5.value, ymax = q97.5.value, col = as.factor(belowCapacity)), width = 0.3) +
    geom_point(aes(x = as.factor(reopening), y = median.value, fill = as.factor(belowCapacity)), shape = 21, show.legend = F, size = 2) +
    facet_wrap(~region, scales = "free") +
    scale_fill_manual(values = c("indianred", "deepskyblue3")) +
    geom_hline(aes(yintercept = icu_available)) +
    labs(x = "Reopening multiplier (%)", y = "Peak in ICU census", color = "ICU overflow", fill = "ICU overflow") +
    customThemeNoFacet
  
  pplot <- plot_grid(pplot1, pplot2, ncol = 1)
  
  ggsave(paste0("ICU_reopening_perGrp.png"),
         plot = pplot, path = file.path(outdir), width = 10, height = 8, device = "png"
  )
  ggsave(paste0("ICU_reopening_perGrp.pdf"),
         plot = pplot, path = file.path(outdir), width = 10, height = 8, device = "pdf"
  )
}


#### Load probabilities..
if (probPlot) {
  
  propDat_sim <- simdat %>%
    dplyr::filter(channel == "critical" & date > as.Date("2020-09-19") & date <= as.Date("2020-12-31")) %>%
    dplyr::group_by(scen_num, sample_num, geography_name, exp_name, reopen, delay, rollback, pop, capacity_multiplier, icu_available) %>%
    dplyr::summarize(value = max(value)) %>%
    dplyr::mutate(aboveCapacity = ifelse(value >= icu_available, 1, 0)) %>%
    dplyr::group_by(geography_name, exp_name, reopen, delay, rollback, pop, capacity_multiplier, icu_available) %>%
    add_tally(name = "nsamples") %>%
    dplyr::group_by(geography_name, exp_name, reopen, delay, rollback, pop, capacity_multiplier, icu_available, nsamples) %>%
    dplyr::summarize(nabove = sum(aboveCapacity)) %>%
    dplyr::mutate(prob_overflow = nabove / nsamples)
  
  propDat_sim$geography_name <- factor(propDat_sim$geography_name, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))
  
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
  
  ggsave(paste0("ICUoverflow_proball_perCovidRegion.png"),
         plot = pplot, path = file.path(simulation_output, exp_name), width = 14, height = 8, device = "png"
  )
  ggsave(paste0("ICUoverflow_proball_perCovidRegion.pdf"),
         plot = pplot, path = file.path(simulation_output, exp_name), width = 14, height = 8, device = "pdf"
  )
  
  rm(pplot)
  
  
  ##### Separate
  p1 <- ggplot(data = subset(propDat_sim, geography_name %in% c(1, 4, 11) & rollback %in% c("sm4"))) +
    geom_line(aes(x = capacity_multiplier, y = prob_overflow, group = exp_name, col = delay, linetype = reopen), size = 1.1) +
    # geom_point(aes(x=capacity_multiplier, y=prob_overflow, group=exp_name, fill=reopen),col="white",shape=21, size=2) +
    geom_hline(yintercept = 0.2) +
    scale_alpha_manual(values = c(1, 0.6, 0.3)) +
    scale_color_manual(values = delay_cols) +
    scale_fill_manual(values = delay_cols) +
    facet_wrap(~geography_name, scales = "free") +
    customThemeNoFacet +
    background_grid()
  
  p2 <- ggplot(data = subset(propDat_sim, geography_name %in% c(1, 4, 11) & delay %in% c("none"))) +
    geom_line(aes(x = capacity_multiplier, y = prob_overflow, group = exp_name, col = rollback, linetype = reopen), size = 1.1) +
    geom_hline(yintercept = 0.2) +
    scale_alpha_manual(values = c(1, 0.6, 0.3)) +
    scale_color_manual(values = rollback_cols) +
    scale_fill_manual(values = rollback_cols) +
    facet_wrap(~geography_name, scales = "free") +
    customThemeNoFacet +
    background_grid()
  
  
  p12 <- plot_grid(p1, p2, ncol = 1)
  
  
  ggsave(paste0("ICUoverflow_probability.png"),
         plot = p12, path = file.path(simulation_output, exp_name), width = 14, height = 8, device = "png"
  )
  ggsave(paste0("ICUoverflow_probability.pdf"),
         plot = p12, path = file.path(simulation_output, exp_name), width = 14, height = 8, device = "pdf"
  )
  
  
  ggsave(paste0("ICUoverflow_probability.png"),
         plot = p12, path = file.path(outdir), width = 14, height = 8, device = "png"
  )
  ggsave(paste0("ICUoverflow_probability.pdf"),
         plot = p12, path = file.path(outdir), width = 14, height = 8, device = "pdf"
  )
}




#### Figure 7 compare region specific and generic trigger
lastFigure <- FALSE
if (lastFigure) {
  
  exp_combined <- "20200831_IL_regreopen_combined"
  fname <- "combined_dataframe.Rdata"
  
  if (!file.exists(file.path(simulation_output, exp_combined, fname))) {
    exp_names <- list.dirs(simulation_output, recursive = FALSE, full.names = FALSE)
    exp_names <- exp_names[grep("20200831_IL_regreopen", exp_names)]
    simdat <- load_sim_data(exp_names = exp_names)
  } else {
    load(file.path(simulation_output, exp_combined, fname))
  }
  
  reg_capacities <- propDat_sim %>%
    dplyr::group_by(geography_name, icu_available, exp_name, reopen, delay, rollback) %>%
    dplyr::filter(prob_overflow <= 0.4) %>%
    dplyr::filter(capacity_multiplier == max(capacity_multiplier)) %>%
    dplyr::rename(capacity_recom = capacity_multiplier) %>%
    dplyr::select(geography_name, icu_available, exp_name, reopen, delay, rollback, capacity_recom)
  
  pplot <- ggplot(data = reg_capacities) +
    geom_point(aes(x = exp_name, y = capacity_recom, group = exp_name, col = reopen, shape = delay)) +
    facet_wrap(~geography_name, scales = "free") +
    theme(axis.text.y = element_blank()) +
    coord_flip()
  
  
  ####
  simdat_generic <- simdat %>%
    dplyr::filter(capacity_multiplier == 0.8) %>%
    dplyr::mutate(scenario = "generic") %>%
    dplyr::select(scen_num, date, geography_name, icu_available, exp_name, reopen, delay, rollback, scenario, value)
  
  simdat_specific <- simdat %>%
    left_join(reg_capacities, by = c("geography_name", "icu_available", "exp_name", "reopen", "delay", "rollback")) %>%
    dplyr::filter(capacity_multiplier == capacity_recom) %>%
    dplyr::mutate(scenario = "specific") %>%
    dplyr::select(scen_num, date, geography_name, icu_available, exp_name, reopen, delay, rollback, scenario, value)
  
  simdat_compare <- rbind(simdat_generic, simdat_specific) %>%
    dplyr::filter(geography_name != "illinois") %>%
    dplyr::group_by(date, geography_name, icu_available, exp_name, reopen, delay, rollback, scenario) %>%
    dplyr::summarize(
      median.value = median(value, na.rm = TRUE),
      q2.5.value = quantile(value, probs = 0.025, na.rm = TRUE),
      q97.5.value = quantile(value, probs = 0.975, na.rm = TRUE)
    )
  
  simdat_compare_IL <- rbind(simdat_generic, simdat_specific) %>%
    dplyr::filter(geography_name != "illinois") %>%
    dplyr::group_by(scen_num, date, exp_name, reopen, delay, rollback, scenario) %>%
    dplyr::summarize(value = sum(value)) %>%
    dplyr::group_by(date, exp_name, reopen, delay, rollback, scenario) %>%
    dplyr::summarize(
      median.value = median(value, na.rm = TRUE),
      q2.5.value = quantile(value, probs = 0.025, na.rm = TRUE),
      q97.5.value = quantile(value, probs = 0.975, na.rm = TRUE)
    ) %>%
    dplyr::mutate(geography_name = "illinois")
  
  
  simdat_compare$geography_name <- factor(simdat_compare$geography_name, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))
  
  pplot <- ggplot(data = subset(simdat_compare, exp_name == "20200831_IL_regreopen100perc_0daysdelay_sm4")) +
    geom_ribbon(aes(x = date, ymin = q2.5.value, ymax = q97.5.value, fill = as.factor(scenario)), alpha = 0.2) +
    geom_line(aes(x = date, y = median.value, col = as.factor(scenario)), size = 1) +
    facet_wrap(~geography_name, scales = "free") +
    scale_color_manual(values = c("indianred", "deepskyblue3")) +
    geom_hline(aes(yintercept = icu_available), color = "darkgrey", linetype = "dashed", size = 1) +
    labs(x = "", y = "ICU census", color = "ICU overflow", fill = "ICU overflow") +
    customThemeNoFacet
  
  
  ggsave(paste0("ICU_trajectories_comparison_perCovidRegion.png"),
         plot = pplot, path = file.path(simulation_output, exp_name), width = 14, height = 8, device = "png"
  )
  ggsave(paste0("ICU_trajectories_comparison_perCovidRegion.pdf"),
         plot = pplot, path = file.path(simulation_output, exp_name), width = 14, height = 8, device = "pdf"
  )
  rm(pplot)
  
  
  ##########################################
  
  simdat_generic <- simdat %>%
    dplyr::filter(capacity_multiplier == 0.8) %>%
    dplyr::mutate(
      scenario = "generic",
      capacity_recom = 0.8,
      risk_tolerance = "generic"
    ) %>%
    dplyr::select(scen_num, date, geography_name, icu_available, exp_name, reopen, delay, rollback, scenario, value, capacity_recom, risk_tolerance)
  
  
  reg_capacities_02 <- propDat_sim %>%
    dplyr::group_by(geography_name, icu_available, exp_name, reopen, delay, rollback) %>%
    dplyr::filter(prob_overflow <= 0.2) %>%
    dplyr::filter(capacity_multiplier == max(capacity_multiplier)) %>%
    dplyr::rename(capacity_recom = capacity_multiplier) %>%
    dplyr::mutate(risk_tolerance = "reg_capacities_02") %>%
    dplyr::select(geography_name, icu_available, exp_name, reopen, delay, rollback, capacity_recom, risk_tolerance)
  
  
  reg_capacities_04 <- propDat_sim %>%
    dplyr::group_by(geography_name, icu_available, exp_name, reopen, delay, rollback) %>%
    dplyr::filter(prob_overflow <= 0.4) %>%
    dplyr::filter(capacity_multiplier == max(capacity_multiplier)) %>%
    dplyr::rename(capacity_recom = capacity_multiplier) %>%
    dplyr::mutate(risk_tolerance = "reg_capacities_04") %>%
    dplyr::select(geography_name, icu_available, exp_name, reopen, delay, rollback, capacity_recom, risk_tolerance)
  
  
  reg_capacities_06 <- propDat_sim %>%
    dplyr::group_by(geography_name, icu_available, exp_name, reopen, delay, rollback) %>%
    dplyr::filter(prob_overflow <= 0.6) %>%
    dplyr::filter(capacity_multiplier == max(capacity_multiplier)) %>%
    dplyr::rename(capacity_recom = capacity_multiplier) %>%
    dplyr::mutate(risk_tolerance = "reg_capacities_06") %>%
    dplyr::select(geography_name, icu_available, exp_name, reopen, delay, rollback, capacity_recom, risk_tolerance)
  
  reg_capacities <- rbind(reg_capacities_02, reg_capacities_04, reg_capacities_06)
  
  simdat_specific <- simdat %>%
    dplyr::left_join(reg_capacities, by = c("geography_name", "icu_available", "exp_name", "reopen", "delay", "rollback")) %>%
    dplyr::filter(capacity_multiplier == capacity_recom) %>%
    dplyr::mutate(scenario = "specific") %>%
    dplyr::select(scen_num, date, geography_name, icu_available, exp_name, reopen, delay, rollback, scenario, value, capacity_recom, risk_tolerance)
  
  simdat_compare <- rbind(simdat_generic, simdat_specific) %>%
    dplyr::filter(geography_name != "illinois") %>%
    dplyr::group_by(date, geography_name, icu_available, exp_name, reopen, delay, rollback, scenario, capacity_recom, risk_tolerance) %>%
    dplyr::summarize(
      median.value = median(value, na.rm = TRUE),
      q2.5.value = quantile(value, probs = 0.025, na.rm = TRUE),
      q97.5.value = quantile(value, probs = 0.975, na.rm = TRUE)
    )
  
  
  
  ##########################################
  
  ##########################################
  
  pplot <- ggplot(data = subset(simdat_compare, risk_tolerance %in% c("reg_capacities_04", "generic") & 
                                  geography_name %in% c("1", "4", "11") & exp_name == "20200831_IL_regreopen100perc_0daysdelay_sm4")) +
    geom_ribbon(aes(x = date, ymin = q2.5.value, ymax = q97.5.value, fill = as.factor(scenario)), alpha = 0.2) +
    geom_line(aes(x = date, y = median.value, col = as.factor(scenario), linetype = risk_tolerance), size = 1) +
    facet_wrap(~geography_name, scales = "free") +
    scale_color_manual(values = c("indianred", "deepskyblue3")) +
    geom_hline(aes(yintercept = icu_available), color = "darkgrey", linetype = "dashed", size = 1) +
    labs(x = "", y = "ICU census", color = "ICU overflow", fill = "ICU overflow") +
    customThemeNoFacet
  
  
  ggsave(paste0("ICU_trajectories_reg_capacities_04.png"),
         plot = pplot, path = file.path(outdir), width = 14, height = 8, device = "png"
  )
  ggsave(paste0("ICU_trajectories_reg_capacities_04.pdf"),
         plot = pplot, path = file.path(outdir), width = 14, height = 8, device = "pdf"
  )
  rm(pplot)
  
  
  
  #### Note, improve by looking at cumulative, not only peal
  
  simdat_compare_Peak <- simdat_compare %>%
    dplyr::filter(geography_name %in% c("1", "7", "11")) %>%
    dplyr::group_by(geography_name, exp_name, scenario, rollback, delay, reopen, capacity_recom, risk_tolerance) %>%
    dplyr::filter(median.value == max(median.value)) %>%
    dplyr::filter(date == min(date))
  
  simdat_compare_Peak1 <- subset(simdat_compare_Peak, scenario != "generic")
  simdat_compare_Peak2 <- subset(simdat_compare_Peak, scenario == "generic") %>%
    dplyr::group_by() %>%
    dplyr::select(-risk_tolerance, -capacity_recom, -scenario) %>%
    dplyr::rename(
      base_median.value = median.value,
      base_q2.5.value = q2.5.value,
      base_q97.5.value = q97.5.value
    )
  
  mergevars <- colnames(simdat_compare_Peak1)[colnames(simdat_compare_Peak1) %in% colnames(simdat_compare_Peak2)]
  simdat_compare_Peak <- left_join(simdat_compare_Peak1, simdat_compare_Peak2, by = mergevars)
  
  ggplot(data = simdat_compare_Peak) +
    geom_bar(aes(x = as.factor(risk_tolerance), y = base_median.value), fill = "red", stat = "identity", position = "dodge") +
    geom_bar(aes(x = as.factor(risk_tolerance), y = median.value), stat = "identity", position = "dodge") +
    facet_wrap(~geography_name)
  
  ggplot(data = simdat_compare_Peak) +
    geom_bar(aes(x = as.factor(exp_name), y = base_median.value), fill = "red", stat = "identity", position = "dodge") +
    geom_bar(aes(x = as.factor(exp_name), y = median.value, fill = risk_tolerance), stat = "identity", position = "dodge") +
    facet_wrap(~geography_name, ncol = 1)
  
  ggplot(data = simdat_compare_Peak) +
    geom_bar(aes(x = as.factor(geography_name), y = base_median.value), fill = "red", stat = "identity", position = "dodge") +
    geom_bar(aes(x = as.factor(geography_name), y = median.value, fill = risk_tolerance), stat = "identity", position = "dodge") +
    facet_wrap(~delay, ncol = 1)
  
  
  ggplot(data = subset(simdat_compare_Peak)) +
    geom_pointrange(aes(x = as.factor(risk_tolerance), y = base_median.value, ymin = base_median.value, ymax = base_median.value)) +
    facet_wrap(~geography_name)
  
  ggplot(data = simdat_compare_Peak) +
    geom_boxplot(aes(x = as.factor(risk_tolerance), y = base_median.value), fill = "red", position = "dodge") +
    geom_boxplot(aes(x = as.factor(risk_tolerance), y = median.value, fill = risk_tolerance), position = "dodge") +
    facet_wrap(~geography_name, ncol = 1)
  
  
  
  ################## CUMULATIVE
  
  simdat_compare1 <- subset(simdat_compare, scenario != "generic")
  
  simdat_compare2 <- subset(simdat_compare, scenario == "generic") %>%
    dplyr::group_by() %>%
    dplyr::select( -risk_tolerance, -capacity_recom, -scenario) %>%
    dplyr::rename(
      base_median.value = median.value,
      base_q2.5.value = q2.5.value,
      base_q97.5.value = q97.5.value
    )
  
  mergevars <- colnames(simdat_compare1)[colnames(simdat_compare1) %in% colnames(simdat_compare2)]
  
  simdat_compare_cum <- left_join(simdat_compare1, simdat_compare2, by = mergevars) %>%
    dplyr::filter(geography_name %in% c("1", "4", "11")) %>%
    dplyr::group_by(geography_name, exp_name, scenario, rollback, delay, reopen, capacity_recom, risk_tolerance) %>%
    dplyr::mutate(
      median.value_cum = cumsum(median.value) - median.value,
      q2.5.value_cum = cumsum(q2.5.value) - q2.5.value,
      q97.5.value_cum = cumsum(q97.5.value) - q97.5.value,
      base_median.value_cum = cumsum(base_median.value) - base_median.value,
      base_q2.5.value_cum = cumsum(base_q2.5.value) - base_q2.5.value,
      base_q97.5.value_cum = cumsum(base_q97.5.value) - base_q97.5.value
    ) %>%
    dplyr::filter(date >= as.Date("2020-12-30") & date <= as.Date("2020-12-31"))
  
  simdat_compare_cum$label <- paste0(simdat_compare_cum$delay, "\n", simdat_compare_cum$rollback)
  simdat_compare_cum$median.value_diff <- simdat_compare_cum$base_median.value_cum - simdat_compare_cum$median.value_cum
  simdat_compare_cum$q2.5.value_diff <- simdat_compare_cum$base_q2.5.value_cum - simdat_compare_cum$q2.5.value_cum
  simdat_compare_cum$q97.5.value_diff <- simdat_compare_cum$base_q97.5.value_cum - simdat_compare_cum$q97.5.value_cum
  
  
  ggplot(data = simdat_compare_cum) +
    geom_pointrange(aes(
      x = interaction(reopen, label), y = median.value_cum,
      ymin = q2.5.value_cum, ymax = q97.5.value_cum, shape = risk_tolerance, col = risk_tolerance
    )) +
    geom_pointrange(aes(
      x = interaction(reopen, label), y = base_median.value_cum,
      ymin = base_q2.5.value_cum, ymax = base_q97.5.value_cum, group = risk_tolerance
    ), col = "red") +
    facet_wrap( ~ geography_name, scales = "free_x", strip.position="top") +
    coord_flip()
  
  
  
  ggplot(data = subset(simdat_compare_cum, delay=="none")) +
    # geom_hline(aes(yintercept = base_median.value_cum))+
    geom_point(aes(
      x = interaction(reopen, label), y = base_median.value_cum, group = risk_tolerance
    ), col = "red", position = position_dodge(width = 0.3)) +
    geom_pointrange(aes(
      x = interaction(reopen, label), y = median.value_cum,
      ymin = q2.5.value_cum, ymax = q97.5.value_cum, shape = risk_tolerance, col = risk_tolerance
    ), position = position_dodge(width = 0.3)) +
    #geom_pointrange(aes(
    #   x = interaction(reopen, label), y = base_median.value_cum,
    #   ymin = base_q2.5.value_cum, ymax = base_q97.5.value_cum, group = risk_tolerance
    # ), col = "red", position = position_dodge(width = 0.3)) +
    facet_wrap( ~ geography_name, scales = "free_x", strip.position="top") +
    coord_flip()
  
  
  
  ggplot(data = subset(simdat_compare_cum, delay=="none")) +
    geom_bar(aes(
      x = interaction(reopen, label), y = base_median.value_cum, group = risk_tolerance
    ), stat="identity",  fill = "red", position = position_dodge(width = 0.9)) +
    geom_bar(aes(
      x = interaction(reopen, label), y = median.value_cum, fill = risk_tolerance
    ), position = position_dodge(width = 0.9), stat="identity") +
    facet_wrap( ~ geography_name, scales = "free_x", strip.position="top") 
  
  
  
  ggplot(data = simdat_compare_cum) +
    #geom_boxplot(aes(
    #  x = risk_tolerance, y = median.value_cum,shape = risk_tolerance, col = risk_tolerance
    # )) +
    geom_boxplot(aes(
      x = risk_tolerance, y = base_median.value_cum, group = risk_tolerance
    ), col = "red") +
    facet_wrap( ~ geography_name, scales = "free_x", strip.position="top") 
  
  pplot <- ggplot(data = simdat_compare_cum) +
    geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0, fill = "lightgrey", alpha = 0.2) +
    geom_pointrange(aes(
      x = reorder(label, median.value_diff), y = median.value_diff,
      ymin = q2.5.value_diff, ymax = q97.5.value_diff, shape = risk_tolerance, col = risk_tolerance
    )) +
    facet_wrap(reopen ~ geography_name, scales = "free_y") +
    customThemeNoFacet +
    labs(x = "", y = "Cumulative ICU cases averted\ncompared to generic trigger of 80%\n") +
    scale_color_brewer(palette = "Dark2")
  
  
  ggsave(paste0("cumICU_averted_allScenarios.png"),
         plot = pplot, path = file.path(outdir), width = 14, height = 8, device = "png"
  )
  ggsave(paste0("cumICU_averted_allScenarios.pdf"),
         plot = pplot, path = file.path(outdir), width = 14, height = 8, device = "pdf"
  )
  rm(pplot)
}
