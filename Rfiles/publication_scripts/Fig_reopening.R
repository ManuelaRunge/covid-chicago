## ================================================
### Figure 2 SImulation:  Baseline and reopening
## ================================================
f_getPredDat <- function(exp_name) {
  
  if (!(file.exists(file.path(simulation_output, exp_name, "predDate.csv")))) {
    
    trajectoriesDat <- read.csv(file.path(expDIR, "trajectoriesDat.csv"))
    unique(trajectoriesDat$reopening_multiplier_4)
    
    
    colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))
    
    
    ### per restore region
    if ("deaths_southern" %in% colnames(trajectoriesDat)) region_names <- c("southern", "central", "northcentral", "northeast")
    if (!("deaths_southern" %in% colnames(trajectoriesDat))) region_names <- paste0("EMS-", c(1:11))
    trajectoriesDat$Ki_t_southern <- (trajectoriesDat$Ki_t_EMS_4 + trajectoriesDat$Ki_t_EMS_5) / 2
    trajectoriesDat$Ki_t_central <- (trajectoriesDat$Ki_t_EMS_3 + trajectoriesDat$Ki_t_EMS_6) / 2
    trajectoriesDat$Ki_t_northcentral <- (trajectoriesDat$Ki_t_EMS_7 + trajectoriesDat$Ki_t_EMS_8 + trajectoriesDat$Ki_t_EMS_9 + trajectoriesDat$Ki_t_EMS_10 + trajectoriesDat$Ki_t_EMS_11) / 5
    trajectoriesDat$Ki_t_northeast <- (trajectoriesDat$Ki_t_EMS_4 + trajectoriesDat$Ki_t_EMS_5) / 2
    
    
    paramvars <- c(
      paste0("Ki_t_", region_names),
      paste0("deaths_", region_names),
      paste0("hosp_cumul_", region_names),
      paste0("hospitalized_det_", region_names),
      paste0("hospitalized_", region_names),
      paste0("infected_", region_names),
      paste0("deaths_det_", region_names),
      paste0("prevalence_", region_names),
      paste0("crit_cumul_", region_names),
      paste0("critical_", region_names)
    )
    
    
    keepvars <- c("time", "startdate", "scen_num", "reopening_multiplier_4", paramvars)
    
    
    predDat <- trajectoriesDat %>%
      dplyr::select(keepvars) %>%
      dplyr::mutate(date = as.Date(startdate) + time) %>%
      pivot_longer(cols = -c("time", "date", "startdate", "scen_num", "reopening_multiplier_4"), names_to = "name") %>%
      dplyr::mutate(
        name = gsub("_cumul_", ".cumul_", name),
        name = gsub("_det_", ".det_", name),
        name = gsub("Ki_t_", "Ki.t_", name)
      ) %>%
      separate(name, into = c("param", "restore_region"), sep = "_") %>%
      dplyr::mutate(
        param = gsub("[.]", "_", param)
      ) %>%
      dplyr::mutate(exp_name = exp_name)
    
    table(predDat$param)
    
    predDat <- predDat %>%
      dplyr::group_by(date, restore_region, param, exp_name, reopening_multiplier_4) %>%
      dplyr::summarize(
        median.val = mean(value, na.rm = TRUE),
        mean.val = mean(value, na.rm = TRUE),
        mean.val = mean(value, na.rm = TRUE),
        q25		= quantile(tempvar, probs=0.25, na.rm = TRUE),
        q75		= quantile(tempvar, probs=0.75, na.rm = TRUE),
        q2.5		= quantile(tempvar, probs=0.025, na.rm = TRUE),
        q97.5  	= quantile(tempvar, probs=0.975, na.rm = TRUE),
        n.val = n())
    
    predDat$restore_region <- str_to_sentence(predDat$restore_region)
    # write.csv(predDat, file.path(simulation_output, exp_name, "predDat.csv"), row.names = FALSE)
    save(predDat, file = file.path(simulation_output, exp_name, "predDat.Rdata"))
  }
  
  
  if ((file.exists(file.path(expDIR, "predDate.csv")))) predDat <- load(file.path(expDIR, "predDat.Rdata"))
  
  # table( predDat$restore_region,  predDat$param)
  return(predDat)
}

#### SHow reopen

f_simulationTimelne_counterfactual <- function(df = predDat, expDIR, baselineDate = "2020-07-21") {
  
  customTheme <- f_getCustomTheme()
  
  df <- data.table(df, key = c("date", "restore_region", "param", "exp_name"))
  df[, additionalPred_mean.val := mean.val - mean.val[reopening_multiplier_4 == 0], by = c("date", "restore_region", "param", "exp_name")]
  df[, additionalPredperc_mean.val := (mean.val - mean.val[reopening_multiplier_4 == 0]) / mean.val[reopening_multiplier_4 == 0], by = c("date", "restore_region", "param", "exp_name")]
  
  
  df %>%
    dplyr::filter(date >= as.Date("2020-12-30") & date <= as.Date("2020-12-31")) %>%
    dplyr::group_by(restore_region, reopening_multiplier_4, param) %>%
    dplyr::summarize(additionalPredperc_mean.val = mean(additionalPredperc_mean.val)) %>%
    pivot_wider(names_from = "param", values_from = "additionalPredperc_mean.val")
  
  
  #### ---------------------------
  ###  Hospitalizations by restore region timeline
  #### ---------------------------
  
  plotdat <- df %>%
    dplyr::mutate(geography_name = tolower(restore_region)) %>%
    left_join(capacity, by = "geography_name") %>%
    dplyr::filter(param %in% c("critical") & date >= as.Date("2020-03-01") & date <= as.Date("2020-12-30"))
  
  
  pplot <- ggplot(data = plotdat) +
    # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_line(aes(x = date, y = mean.val, col = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), size = 1.3) +
    geom_line(data = subset(plotdat, date <= as.Date(baselineDate)), aes(x = date, y = mean.val, group = reopening_multiplier_4), col = "black", size = 1.3) +
    geom_hline(aes(yintercept = icu_available), linetype = "dashed", col = "black", size = 1) +
    scale_y_continuous(labels = function(x) x / 1000, expand = c(0, 0)) + # label = comma
    scale_x_date(date_breaks = "60 days", date_labels = "%b", expand = c(0, 0)) +
    # scale_color_brewer(palette = "Dark2") +
    scale_color_manual(values=c("#fecc5c","#fd8d3c","#f03b20","#bd0026")) +
    customTheme +
    labs(x = "") +
    theme(legend.position = "None") +
    labs(y = "Cases requiring ICU beds\nper 1000 population") +
    facet_wrap(~restore_region, scales = "free_y", ncol = 1)
  
  ggsave(paste0("reopening_scenarios_restoreRegions", ".pdf"),
         plot = pplot, path = file.path(expDIR), width = 5, height = 7.5, device = "pdf"
  )
  ggsave(paste0("reopening_scenarios_restoreRegions", ".pdf"),
         plot = pplot, path = file.path(pdfdir), width = 5, height = 7.5, device = "pdf"
  )
  
  
  
  rm(pplot, plotdat)
  
  
  #### ---------------------------
  ###  Different outcome channels by timeline
  #### ---------------------------
  capacity_long <- capacity %>% pivot_longer(cols = -c("geography_name"), names_to = "param", values_to = "capacity") %>%
    filter(param != "vents_available") %>%
    mutate(param = case_when(
      param == "icu_available" ~ "critical",
      param == "medsurg_available" ~ "hospitalized"
    )) 
  
  capacity_long <- capacity_long %>%
    filter(param != "vents_available") %>%
    mutate(param = case_when(
      param == "hospitalized" ~ "hosp_cumul",
      param == "critical" ~ "crit_cumul"
    )) %>%
    rbind(capacity_long) %>%
    group_by(param) %>%
    summarize(capacity=sum(capacity))
  
  
  popdat <- load_population() %>% 
    mutate(region =as.character(geography_name) ) %>% 
    filter(region !="illinois") %>%
    f_addRestoreRegion() %>% 
    group_by(restore_region) %>%
    summarize(pop=sum(as.numeric(pop))) %>%
    mutate(geography_name = tolower(restore_region)) %>%
    select(-restore_region)
  
  
  
  plotdat <- df %>% as.data.frame() %>%
    dplyr::mutate(geography_name = tolower(restore_region)) %>%
    dplyr::filter(param %in% c("infected", "hospitalized", "critical", "prevalence")) %>%
    dplyr::left_join(popdat, by = c("geography_name")) %>%
    dplyr::filter(date >= as.Date("2020-03-01") & date <= as.Date("2020-12-31")) %>%
    dplyr::group_by(date, reopening_multiplier_4, param) %>%
    select(-c('sd.val', 'n.val', 'se.val', 'lower.ci.val', 'upper.ci.val', 'additionalPred_mean.val', 'additionalPredperc_mean.val')) %>% 
    pivot_wider(names_from = "param", values_from="mean.val") %>%
    dplyr::left_join(capacity, by = c("geography_name")) %>%
    dplyr::summarize(
      icu_available = sum(icu_available),
      medsurg_available = sum(medsurg_available),
      infected = sum(infected),
      hospitalized = sum(hospitalized),
      critical = sum(critical),
      prevalence = mean(prevalence)*100
    ) %>%
    pivot_longer(cols=-c(date,reopening_multiplier_4, icu_available, medsurg_available), names_to="param", values_to="mean.val") %>%
    dplyr::left_join(capacity_long, by = c( "param")) 
    
  
  plotdat$param <- factor(plotdat$param,
                          levels = c("infected","prevalence", "hospitalized", "critical"),
                          labels = c("COVID-19 infections","Prevalence" , "Cases requiring hospitalization", "Cases requiring intensive care")
  )
  
  customTheme <- f_getCustomTheme()
  
  pplot <- ggplot(data = plotdat) +
    # geom_errorbar(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_line(aes(x = date, y = mean.val, col = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), size = 1.3) +
    geom_line(data = subset(plotdat, date <= as.Date(baselineDate)), aes(x = date, y = mean.val, group = reopening_multiplier_4), col = "black", size = 1.3) +
    geom_hline(aes(yintercept = capacity), linetype = "dashed", col = "black", size = 1) +
    geom_hline(yintercept = c(-Inf)) +
    # geom_vline(xintercept =c( -Inf, Inf)) +
    scale_y_continuous(label = comma, expand = c(0, 0)) +
    scale_x_date(date_breaks = "30 days", date_labels = "%b", expand = c(0, 0)) +
    # scale_color_brewer(palette = "Dark2") +
    scale_color_manual(values=c("#fecc5c","#fd8d3c","#f03b20","#bd0026")) +
    customTheme +
    labs(x = "") +
    theme(legend.position = "None") +
    labs(y ="") +
    facet_wrap(~param, scales = "free_y", ncol = 2)
  
  print(pplot)
  
  ggsave(paste0("reopening_scenarios_channels_v2", ".pdf"),
         plot = pplot, path = file.path(expDIR), width = 14, height = 7, device = "pdf"
  )
  ggsave(paste0("reopening_scenarios_channels_v2", ".pdf"),
         plot = pplot, path = file.path(pdfdir), width = 14, height = 7, device = "pdf"
  )
  rm(pplot, plotdat)
  
  
  #### ---------------------------
  ### Increase in critical cases
  #### ---------------------------
  
  pplot <- df %>%
    mutate(geography_name = tolower(restore_region)) %>%
    left_join(capacity, by = "geography_name") %>%
    filter(param %in% c("crit_cumul") & date >= as.Date("2020-12-30") & date <= as.Date("2020-12-31")) %>%
    ggplot() +
    theme_cowplot() +
    # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_bar(aes(
      x = reopening_multiplier_4, y = mean.val,
      fill = as.factor(reopening_multiplier_4),
      group = reopening_multiplier_4
    ), stat = "identity", size = 1.3) +
    facet_wrap(~restore_region, scales = "free_y", nrow = 2) +
    scale_y_continuous(labels = function(x) x / 1000, expand = c(0, 0)) +
    scale_fill_brewer(palette = "Dark2") +
    customTheme +
    labs(x = "") +
    theme(legend.position = "None") +
    labs(y = "Cumulative ICU bed demand by Dec 2020\n per 1000 population")
  
  
  ggsave(paste0("reopening_scenarios_critcumulDec2020", ".pdf"),
         plot = pplot, path = file.path(expDIR), width = 8, height = 6, device = "pdf"
  )
  
  
  #### ILLINOIS
  
  criticalIncrease <- df %>%
    dplyr::mutate(geography_name = tolower(restore_region)) %>%
    dplyr::left_join(capacity, by = "geography_name") %>%
    dplyr::filter(param %in% c("crit_cumul") & date >= as.Date("2020-12-30") & date <= as.Date("2020-12-31")) %>%
    dplyr::group_by(reopening_multiplier_4) %>%
    dplyr::summarize(percIncr = paste0(round(mean(additionalPredperc_mean.val) * 100, 0), " %"))
  
  ppred <- df %>%
    dplyr::mutate(geography_name = tolower(restore_region)) %>%
    # dplyr::left_join(capacity, by = "geography_name") %>%
    filter(param %in% c("crit_cumul") & date >= as.Date("2020-12-30") & date <= as.Date("2020-12-31")) %>%
    dplyr::group_by(reopening_multiplier_4) %>%
    dplyr::summarize(mean.val = sum(mean.val)) %>%
    dplyr::left_join(subset(criticalIncrease, reopening_multiplier_4 != 0), by = "reopening_multiplier_4") %>%
    ggplot() +
    theme_cowplot() +
    # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_bar(aes(
      x = reopening_multiplier_4, y = mean.val,
      fill = as.factor(reopening_multiplier_4),
      group = reopening_multiplier_4
    ), stat = "identity", size = 1.3) +
    geom_text(aes(
      x = reopening_multiplier_4, y = mean.val, label = percIncr,
      col = as.factor(reopening_multiplier_4)
    ), size = 5, vjust = -1) +
    scale_y_continuous(labels = function(x) x / 1000, expand = c(0, 0), lim = c(0, 200000)) +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") +
    customTheme +
    labs(x = "") +
    theme(
      legend.position = "None",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(y = "Cumulative ICU bed demand\n per 1000 population")
  
  
  ggsave(paste0("reopening_scenarios_IL_critcumulDec2020", ".pdf"),
         plot = ppred, path = file.path(pdfdir), width = 5, height = 2.5, device = "pdf"
  )
  
  
  ### PER RESTORE REGION
  ppred <- df %>%
    mutate(geography_name = tolower(restore_region)) %>%
    left_join(capacity, by = "geography_name") %>%
    filter(param %in% c("crit_cumul") & date >= as.Date("2020-12-30") & date <= as.Date("2020-12-31")) %>%
    ggplot() +
    theme_cowplot() +
    # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_bar(aes(
      x = reopening_multiplier_4,
      y = additionalPredperc_mean.val,
      fill = as.factor(reopening_multiplier_4),
      group = reopening_multiplier_4
    ), stat = "identity", size = 1.3) +
    facet_wrap(~restore_region, scales = "free_y", nrow = 2) +
    scale_y_continuous(label = comma, expand = c(0, 0)) +
    scale_fill_brewer(palette = "Dark2") +
    customThemeNoFacet +
    labs(x = "") +
    theme(legend.position = "None") +
    labs(y = "Cumulative ICU bed demand by Dec 2020\n per 1000 population")
  
  
  ggsave(paste0("reopening_scenarios_criticalDec2020_perc", ".pdf"),
         plot = ppred, path = file.path(expDIR), width = 8, height = 6, device = "pdf"
  )
  
  
  #### ---------------------------
  ### Transmission Rate Timeline
  #### ---------------------------
  KIplot <- TRUE
  if (KIplot) {
    ### Note use Ki_EMS-  or Ki_EMS.   not Ki_EMS_  (time varying vs input parameter)
    plotdat <- df %>%
      dplyr::filter(param == "Ki_t") %>%
      dplyr::group_by(date, reopening_multiplier_4, param) %>%
      dplyr::summarize(mean.val = mean(mean.val)) %>%
      filter(date >= as.Date("2020-03-01") & date <= as.Date("2020-09-31"))
    
    pplot <- ggplot(data = plotdat) +
      theme_cowplot() +
      geom_line(aes(x = date, y = mean.val, col = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), size = 1.3) +
      geom_line(
        data = subset(plotdat, reopening_multiplier_4 == 0 & date <= as.Date(baselineDate)),
        aes(x = date, y = mean.val, group = reopening_multiplier_4), col = "black", size = 1.3
      ) +
      # scale_color_viridis(discrete = TRUE) +
      scale_color_brewer(palette = "Dark2") +
      labs(
        y = gsub("_", " ", "Transmission rate"),
        subtitle = "", # Estimated change in transmission intensity\n
        title = "",
        x = "",
        col = "reopening"
      ) +
      customThemeNoFacet +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_date(breaks = "30 days", labels = date_format("%b"), expand = c(0, 0))
    
    
    ggsave(paste0("reopening_scenarios", ".pdf"),
           plot = pplot, path = file.path(expDIR), width = 8, height = 4, device = "pdf"
    )
  }
  
  
  #### ---------------------------
  ### Rt Timeline
  #### ---------------------------
  Rtplot <- TRUE
  if (Rtplot) {
    startdate <- as.Date("2020-02-13")
    weekwindow <- 13
    scendat <- read.csv(file.path(simulation_output, exp_name, "sampled_parameters.csv")) %>%
      dplyr::select(scen_num, reopening_multiplier_4)
    
    
    #### Aggregate scen nums as well as regions
    plotdat <- read.csv(file.path(simulation_output, exp_name, "estimatedRt/EMS_combined_estimated_Rt.csv")) %>%
      # dplyr::mutate(date = as.Date(startdate + t_start + weekwindow)) %>%
      dplyr::mutate(date = as.Date(startdate + t_start)) %>%
      left_join(scendat, by = "scen_num") %>%
      dplyr::group_by(date, Date, reopening_multiplier_4) %>%
      dplyr::summarize(mean.val = mean(Mean)) %>%
      dplyr::filter(date >= as.Date("2020-06-01") & date <= as.Date("2020-12-31"))
    # dplyr::filter(Date >= as.Date("2020-06-01") & Date <= as.Date("2020-12-31"))
    
    pplot <- ggplot(data = plotdat) +
      theme_cowplot() +
      geom_line(aes(x = date, y = mean.val, col = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), size = 1.3) +
      geom_line(
        data = subset(plotdat, reopening_multiplier_4 == 0 & date <= as.Date(baselineDate)),
        aes(x = date, y = mean.val, group = reopening_multiplier_4), col = "black", size = 1.3
      ) +
      geom_hline(yintercept = 1) +
      scale_color_brewer(palette = "Dark2") +
      labs(
        y = expr(italic(R[t])),
        subtitle = "", # Estimated change in transmission intensity\n
        title = "",
        x = "",
        col = "reopening"
      ) +
      customTheme +
      theme(legend.position = "none") +
      scale_y_continuous() +
      scale_x_date(date_breaks = "30 days", date_labels = "%b", expand = c(0, 0))
    
    
    ggsave(paste0("reopening_scenarios_Rt", ".pdf"),
           plot = pplot, path = file.path(pdfdir), width = 5, height = 3.5, device = "pdf"
    )
  }
  
  return(deathsIncrease)
}

source("load_paths.R")
exp_name <- "20200731_IL_reopen_counterfactual"
expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)

predDat <- f_getPredDat(exp_name)
capacity <- load_capacity(selected_ems = tolower(unique(predDat$restore_region)))
f_simulationTimelne_counterfactual()




