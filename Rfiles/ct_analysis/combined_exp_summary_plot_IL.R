## ============================================================
## Combine simulated experiments and make summary plot
## Script currently specific for evaluating varying test delay options for 3 different reopening levels

## Test delay
#- "no reduction in test delay"
#- "reduced test delay As, Sym"
#- reduced test delay Sym only"

## Reopening
# 10%, 20% and 30%
## ============================================================


###================================================================
### DEFINE FUNCTIONS
###================================================================

f_compareOutcomes <= function(compareOutcomes) {
  
  dat1 <- read.csv(file.path(sim_dir, paste0("Rt", "_aggregatedMinThresholds.csv")))
  dat2 <- read.csv(file.path(sim_dir, paste0("critical", "_aggregatedMinThresholds.csv")))
  
  dat1$outcome <- "Rt"
  dat2$outcome <- "critical"
  dat <- rbind(dat1, dat2)
  
  dat$sim <- factor(dat$sim,
                    levels = c("no reduction in test delay", "reduced test delay Sym only", "reduced test delay As, Sym"),
                    labels = c("no reduction in test delay", "reduced test delay Sym only", "reduced test delay As, Sym")
  )
  
  
  pplot <- ggplot(data = dat) +
    theme_minimal() +
    geom_pointrange(aes(x = reopening, y = mean.val, ymin = min.val, ymax = max.val, shape = outcome, col = sim, group = sim),
                    size = 1, position = position_dodge(0.5)
    ) +
    labs(
      title = "",
      subtitle = "",
      x = "\nPartial reopening scenario",
      y = "Minimum detection of a- and presymptomatics\nto prevent exceeding ICU capacities\n",
      col = "",
      shape = ""
    ) +
    scale_color_manual(values = c("deepskyblue3", "orange", "mediumvioletred")) +
    customThemeNoFacet +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10), expand = c(0, 0)) +
    theme(panel.grid.major.x = element_blank())
  
  
  ggsave(paste0("IL_thresholds_loess.png"),
         plot = pplot, path = file.path(sim_dir), width = 8, height = 5, device = "png"
  )
  ggsave(paste0("IL_thresholds_loess.pdf"),
         plot = pplot, path = file.path(sim_dir), width = 10, height = 5, device = "pdf"
  )
}

f_summaryPlot <- function(SAVECSV=TRUE){
  #### Extract minimum detection per region and aggregate
  ### add population for population weighting
  require(spatstat)
  
  pop <- c(736370, 1124941, 619366, 698886, 417674, 788985, 1814891, 1673408, 1970275, 1052839, 2716921)
  popdat <- cbind(region = c(1:11), pop) %>% as.data.frame()
  
  tbl <- tbl %>%
    left_join(popdat, by = "region") %>%
    group_by(region, grpvar, scenario) %>%
    mutate(fitmax = max(isolation_success, na.rm = TRUE))
  
  
  pplot <-  tbl %>%
    group_by(region, scenario,  grpvar) %>%
    filter(isolation_success == fitmax & detection_success == min(detection_success)) %>%
    dplyr::group_by(scenario,  grpvar) %>%
    dplyr::summarize(mean.val = weighted.mean(detection_success, pop)) %>%
    dplyr::group_by(scenario, grpvar) %>%
    dplyr::summarize(
      min.val = min(mean.val),
      max.val = max(mean.val),
      mean.val = mean(mean.val)
    ) %>% 
    ggplot() + 
    theme_minimal() +
    geom_pointrange(aes(x = as.factor(grpvar), y = mean.val, ymin = min.val, ymax = max.val, col = scenario, group = scenario),
                    size = 1, position = position_dodge(0.3)
    ) +
    labs(
      title = "",
      subtitle = "",
      x = "\nPartial reopening scenario",
      y = "Minimum detection of a- and presymptomatics\nto prevent exceeding ICU capacities\n",
      col = "",
      shape = ""
    ) +
    #scale_fill_manual(values = c("deepskyblue3", "orange", "mediumvioletred")) +
    scale_color_brewer(palette="Dark2") +
    scale_fill_brewer(palette="Dark2") +
    customThemeNoFacet +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10)) +
    theme(panel.grid.major.x = element_blank())
  
  
  ggsave(paste0(selected_outcome, "_IL_thresholds_summary_loess.png"),
         plot = pplot, path = file.path(sim_dir), width = 10, height = 6, device = "png"
  )
  ggsave(paste0(selected_outcome, "_IL_thresholds_summary_loess.pdf"),
         plot = pplot, path = file.path(sim_dir), width = 10, height = 6, device = "pdf"
  )
  
  
  if(SAVECSV){
    ### Comparison plot
    savefilename <- paste0(selected_outcome, "_regionMinThresholds.csv")
    write.csv(tbl_sub, file.path(sim_dir, savefilename), row.names = FALSE)
    
    
    savefilename <- paste0(selected_outcome, "_aggregatedMinThresholds.csv")
    write.csv(tblAggrIL, file.path(sim_dir, savefilename), row.names = FALSE)
    
  }
  
  
  pplot <-  tbl %>%
    dplyr::group_by(region,restore_region, scenario,  grpvar) %>%
    filter(isolation_success == fitmax & detection_success == min(detection_success)) %>%
    dplyr::group_by(scenario,restore_region,  grpvar) %>%
    dplyr::summarize(mean.val = weighted.mean(detection_success, pop)) %>%
    dplyr::group_by(restore_region,scenario, grpvar) %>%
    dplyr::summarize(
      min.val = min(mean.val),
      max.val = max(mean.val),
      mean.val = mean(mean.val)
    ) %>% 
    ggplot() + 
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 0.36) +
    theme_minimal() +
    geom_pointrange(aes(x = as.factor(grpvar), y = mean.val, ymin = min.val, ymax = max.val, col = scenario, group = scenario),
                    size = 1, position = position_dodge(0.3)
    ) +
    labs(
      title = "",
      subtitle = "",
      x = "\nPartial reopening scenario",
      y = "Minimum detection of a- and presymptomatics\nto prevent exceeding ICU capacities\n",
      col = "",
      shape = ""
    ) +
    facet_wrap(~restore_region) + 
    #scale_fill_manual(values = c("deepskyblue3", "orange", "mediumvioletred")) +
    scale_color_brewer(palette="Dark2") +
    scale_fill_brewer(palette="Dark2") +
    customThemeNoFacet +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10)) +
    theme(panel.grid.major.x = element_blank())
  
  
  ggsave(paste0(selected_outcome, "_RR_thresholds_summary_loess.png"),
         plot = pplot, path = file.path(sim_dir), width = 10, height = 6, device = "png"
  )
  ggsave(paste0(selected_outcome, "_RR_thresholds_summary_loess.pdf"),
         plot = pplot, path = file.path(sim_dir), width = 10, height = 6, device = "pdf"
  )
  
  
  
  pplot <-  tbl %>%
    dplyr::group_by(region,restore_region, scenario,  grpvar) %>%
    filter(isolation_success == fitmax & detection_success == min(detection_success)) %>%
    dplyr::group_by(scenario,region, restore_region,  grpvar) %>%
    dplyr::summarize(mean.val = weighted.mean(detection_success, pop)) %>%
    dplyr::group_by(region, restore_region,scenario, grpvar) %>%
    dplyr::summarize(
      min.val = min(mean.val),
      max.val = max(mean.val),
      mean.val = mean(mean.val)
    ) %>% 
    ggplot() + 
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 0.36) +
    theme_minimal() +
    geom_pointrange(aes(x = as.factor(grpvar), y = mean.val, ymin = min.val, ymax = max.val, col = scenario, group = scenario),
                    size = 1, position = position_dodge(0.3)
    ) +
    labs(
      title = "",
      subtitle = "",
      x = "\nPartial reopening scenario",
      y = "Minimum detection of a- and presymptomatics\nto prevent exceeding ICU capacities\n",
      col = "",
      shape = ""
    ) +
    facet_wrap(~region) + 
    #scale_fill_manual(values = c("deepskyblue3", "orange", "mediumvioletred")) +
    scale_color_brewer(palette="Dark2") +
    scale_fill_brewer(palette="Dark2") +
    customThemeNoFacet +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10)) +
    theme(panel.grid.major.x = element_blank())
  
  
  ggsave(paste0(selected_outcome, "_covidregion_thresholds_summary_loess.png"),
         plot = pplot, path = file.path(sim_dir), width = 10, height = 6, device = "png"
  )
  ggsave(paste0(selected_outcome, "_covidregion_thresholds_summary_loess.pdf"),
         plot = pplot, path = file.path(sim_dir), width = 10, height = 6, device = "pdf"
  )
  
  
  
}

f_customPlot <- function(df=tbl, selected_outcome="critical", scen=unique(tbl$scenario)[1]) {
  
  pplot <- df %>% 
    filter(scenario==scen & region %in% c(1:11)) %>%
    ggplot() +
    theme_minimal() +
    geom_hline(yintercept = seq(0, 1, 0.1), alpha = 0.3, col = "lightgrey", size = 0.3) +
    geom_vline(xintercept = seq(0, 1, 0.1), alpha = 0.3, col = "lightgrey", size = 0.3) +
    geom_ribbon(aes(
      x = detection_success,
      xmin = detection_success, xmax = 1,
      ymin = isolation_success, ymax = 1, fill = as.factor(grpvar), group = grpvar
    ), alpha = 0.8) +
    geom_line(aes(x = detection_success, y = isolation_success, col = as.factor(grpvar), group = grpvar), size = 1) +
    #facet_wrap(restore_region~region) +
    facet_wrap(restore_region~region) +
    scale_color_viridis(option = "viridis", discrete = TRUE) +
    scale_fill_viridis(option = "viridis", discrete = TRUE) +
    customThemeNoFacet +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
    labs(
      x = detectionVar_label,
      y = isolationVar_label,
      fill = groupVar_label,
      col = groupVar_label
    ) +
    theme(panel.spacing = unit(1, "lines"))+
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))
  
  
  
  ggsave(paste0(selected_outcome, "_IL_thresholds_perEMS_loess_",scen,".png"),
         plot = pplot, path = file.path(sim_dir), width = 12, height =9, device = "png"
  )
  ggsave(paste0(selected_outcome, "_IL_thresholds_perEMS_loess_",scen,".pdf"),
         plot = pplot, path = file.path(sim_dir), width = 12, height = 9, device = "pdf"
  )
  
  
  #scen =c("CT","CTHS40TD","CTHS80TD")
  pplot <- df %>% 
    filter(scenario %in% scen & !(region %in% c(1:11)) &  !(region %in% "Illinois")) %>%
    ggplot() +
    theme_minimal() +
    geom_hline(yintercept = seq(0, 1, 0.1), alpha = 0.3, col = "lightgrey", size = 0.3) +
    geom_vline(xintercept = seq(0, 1, 0.1), alpha = 0.3, col = "lightgrey", size = 0.3) +
    geom_ribbon(aes(
      x = detection_success,
      xmin = detection_success, xmax = 1,
      ymin = isolation_success, ymax = 1, fill = as.factor(grpvar), group = grpvar
    ), alpha = 0.8) +
    geom_line(aes(x = detection_success, y = isolation_success, col = as.factor(grpvar), group = grpvar), size = 1) +
    #facet_wrap(restore_region~region) +
    facet_wrap(scenario~region, ncol=4) +
    scale_color_viridis(option = "viridis", discrete = TRUE) +
    scale_fill_viridis(option = "viridis", discrete = TRUE) +
    customThemeNoFacet +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
    labs(
      x = detectionVar_label,
      y = isolationVar_label,
      fill = groupVar_label,
      col = groupVar_label
    ) +
    theme(panel.spacing = unit(1, "lines"))+
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))
  
  
  
  ggsave(paste0(selected_outcome, "_RR_thresholds_scen_loess_",scen,".png"),
         plot = pplot, path = file.path(sim_dir), width = 16, height =12, device = "png"
  )
  ggsave(paste0(selected_outcome, "_RR_thresholds_scen_loess_",scen,".pdf"),
         plot = pplot, path = file.path(sim_dir), width = 16, height = 12, device = "pdf"
  )
  
  
  
  
}

f_generateMap <- function(df=tbl, selected_outcome="critical", scen=unique(tbl$scenario)[1]){
  library(raster)
  library(ggthemes)
  library(rgdal)
  
  shp <- readOGR(file.path(data_path, "covid_IDPH/shapefiles/covid_regions/covid_regions_editedForR.shp"))

  #Convert
  shp_df <- tidy(shp, region="new_restor")
  shp_df$region <- as.numeric(shp_df$id)
  
  df <- df %>%
    group_by(region, grpvar, scenario) %>%
    mutate(fitmax = max(isolation_success, na.rm = TRUE)) %>%
    filter(scenario == scen) %>%
    dplyr::group_by(region, scenario,  grpvar) %>%
    filter(isolation_success == fitmax) %>%
    filter(detection_success == min(detection_success))
  

  shp_df <- left_join(shp_df, df, by = "region")
  
  
  pmap <- ggplot(data = shp_df) +
    geom_polygon(aes(x = long, y = lat, group = region), fill = "lightgrey", color = "black") +
    geom_polygon(aes(x = long, y = lat, fill = detection_success, group = region), color = "black") +
    scale_fill_gradient2(low = "#f7fcfd", high = "#542788") +
    labs(fill = "Minimum detection coverage") +
    facet_wrap(~grpvar, nrow=1) +
    customThemeNoFacet +
    theme_map() +
  theme(legend.position = "right")
  
  
  if (!dir.exists(file.path(sim_dir, "maps"))) dir.create(file.path(sim_dir, "maps"))
  
  ggsave(paste0(selected_outcome, "_IL_thresholds_perEMS_loess_",scen,".png"),
         plot = pmap, path = file.path(sim_dir, "maps"), width = 16, height = 6, device = "png"
  )
  ggsave(paste0(selected_outcome, "_IL_thresholds_perEMS_loess_",scen,".pdf"),
         plot = pmap, path = file.path(sim_dir, "maps"), width = 16, height =6, device = "pdf"
  )
  
}


###================================================================
### RUN SCRIPT 
###================================================================

selected_outcome <- "critical" # Rt"
expnamePrefix = "20200731_IL_reopen_"

### Load simulation data
exp_names <- list.dirs(file.path(ct_dir, simdate), recursive = FALSE, full.names = FALSE)
sim_dir <- file.path(ct_dir, simdate)

if (selected_outcome == "critical") csvfilename <- "thresholds_loess.csv"
if (selected_outcome == "Rt") csvfilename <- "Rt_thresholds_loess.csv"

csvfiles <- list.files(file.path(sim_dir), pattern=csvfilename, recursive = TRUE, full.names = TRUE)

tbl <- sapply(csvfiles, read.csv, simplify=FALSE) %>% 
  data.table::rbindlist( idcol="id2") %>%
  f_addRestoreRegion() %>%
  as.data.frame()


tbl$scenario <- gsub(sim_dir, "", tbl$id2)
tbl$scenario <- gsub(paste0("/",csvfilename), "", tbl$scenario)
tbl$scenario <- gsub(paste0("/",expnamePrefix),  "", tbl$scenario)
tbl$scenario <- gsub(paste0("/","20200801_IL_reopen_"),  "", tbl$scenario)
tbl$scenario <- gsub("contactTracing",  "CT", tbl$scenario)
tbl$scenario <- gsub("[/]",  "", tbl$scenario)

table(tbl$scenario)
table(tbl$scenario,tbl$region)

for(i in  c(1:length(unique(tbl$scenario)))){
  f_customPlot(selected_outcome="critical", scen=unique(tbl$scenario)[i])
  f_generateMap(selected_outcome="critical", scen=unique(tbl$scenario)[i])
  
}

f_summaryPlot()

if(file.exists(file.path(sim_dir, paste0("Rt", "_aggregatedMinThresholds.csv")))){
  f_compareOutcomes()
}


