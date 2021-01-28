## -----------------------------------------
### Rscript to combine and analyze trigger analyses
## -----------------------------------------

library(tidyverse)
library(cowplot)
library(scales)
library(viridis)

source("load_paths.R")
source("processing_helpers.R")


simdate <- "20200828"
simulation_iteration <- paste0(simdate, "_state_events")
# simulation_output <- file.path(simulation_output, "EMS", simulation_iteration)
simulation_output <- file.path(simulation_output)

# outdir <- file.path(project_path, "Plots + Graphs/simulated_scenarios", simulation_iteration)
outdir <- file.path(project_path, "Plots + Graphs/simulated_scenarios", paste0(simdate, "_state_events"))
if(!dir.exists(outdir))dir.create(outdir)

plot_first_day <- "2020-08-01"
plot_last_day <- "2021-01-01"

cols <- rev(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99"))
theme_set(theme_cowplot())


## ------------------------------
## Define functions
## ------------------------------

#### Load data
f_loadDat <- function(exp_name) {
  
  # capacitiesDat <- load_capacity() %>% mutate(region = ifelse(geography_name == "illinois", "All", geography_name))
  capacitiesDat <- load_new_capacity() %>% mutate(region = ifelse(geography_name == "illinois", "All", geography_name))
  
  
  # trajectories_fname="trajectoriesDat.csv"
  trajectories_fname <- "trajectoriesDat_trim.csv"
  trajectoriesDat <- read_csv(file.path(simulation_output, exp_name, trajectories_fname))
  
  outvars <- colnames(trajectoriesDat)[c(grep("_EMS-", colnames(trajectoriesDat)), grep("_All", colnames(trajectoriesDat)))]
  outvars <- outvars[c(grep("Ki_t", outvars), grep("crit", outvars), grep("hosp", outvars))]
  
  outvars <- c(outvars,colnames(trajectoriesDat)[c(grep("triggertime_EMS_", colnames(trajectoriesDat)), grep("reopening_multiplier_4_EMS_", colnames(trajectoriesDat)))])
  #outvars <- outvars[c(grep("triggertime", outvars), grep("reopening_multiplier_4", outvars))]
  
  keepvars <- c("time", "startdate", "scen_num", "capacity_multiplier",  outvars)
  
  
  dat <- trajectoriesDat %>%
    dplyr::select(keepvars) %>%
    dplyr::mutate(date = as.Date(startdate) + time) %>%
    filter(date > as.Date("2020-08-17") & date <= as.Date("2020-12-31")) %>%
    pivot_longer(cols = -c("time", "date", "startdate", "scen_num", "capacity_multiplier"), names_to = "region") %>%
    dplyr::mutate(
      region = gsub("_All", "_EMS-All", region),
      region = gsub("_EMS_", "_EMS-", region)
    ) %>%
    separate(region, into = c("outcome", "region"), sep = "_EMS-") %>%
    mutate(
      exp_name = exp_name,
    ) %>%
    pivot_wider(names_from = "outcome", values_from = "value") %>%
    left_join(capacitiesDat, by = "region")
  
  
  dat <- dat %>%
   # dplyr::select(time, region, date, scen_num, critical, crit_det, hospitalized, hosp_det, capacity_multiplier, exp_name) %>%
    left_join(capacitiesDat, by = "region")
  
  dat$region <- factor(dat$region, levels = c("All", c(1:11)), labels = c("illinois", c(1:11)))
  
  return(dat)
}

f_processDat <- function(df) {
  
  
  colnames(df)[colnames(df) == "icu_available.y"] <- "icu_available"
  colnames(df)[colnames(df) == "medsurg_available.y"] <- "medsurg_available"
  
  dfAggr_crit <- df %>%
    dplyr::group_by(region, capacity_multiplier, scen_num, exp_nr, rollback, reopening_multiplier_4) %>%
    filter(crit_det == max(crit_det)) %>%
    dplyr::group_by(region, exp_nr, exp_name,  rollback, reopening_multiplier_4, capacity_multiplier, icu_available, medsurg_available) %>%
    dplyr::summarize(
      median.crit_det = median(crit_det, na.rm = TRUE),
      q2.5.crit_det = quantile(crit_det, probs = 0.025, na.rm = TRUE),
      q97.5.crit_det = quantile(crit_det, probs = 0.975, na.rm = TRUE)
    ) %>%
    dplyr::group_by(region, exp_nr, exp_name, rollback, reopening_multiplier_4, icu_available, medsurg_available) %>%
    mutate(
      crit_det_BelowCapacity = ifelse(max(median.crit_det) < icu_available, 1, 0)
    )
  

  minCapacity_crit <- dfAggr_crit %>%
    group_by(region, rollback, reopening_multiplier_4, capacity_multiplier) %>%
    filter(crit_det_BelowCapacity == 0 & median.crit_det < icu_available) %>%
    group_by(region, rollback, reopening_multiplier_4, capacity_multiplier) %>%
    summarize(minCapacity_det = max(capacity_multiplier))
  
  df_crit <- dfAggr_crit %>% left_join(minCapacity_crit, by = c('region', 'rollback', 'reopening_multiplier_4', 'capacity_multiplier'))
  
  
  ### Hospitalized
  dfAggr_hosp <- df %>%
    dplyr::group_by(region, capacity_multiplier, scen_num, exp_nr, rollback, reopening_multiplier_4) %>%
    filter(hosp_det == max(hosp_det)) %>%
    dplyr::group_by(region, exp_nr, exp_name, rollback, reopening_multiplier_4, capacity_multiplier, icu_available, medsurg_available) %>%
    dplyr::summarize(
      median.hosp_det = median(hosp_det, na.rm = TRUE),
      q2.5.hosp_det = quantile(hosp_det, probs = 0.025, na.rm = TRUE),
      q97.5.hosp_det = quantile(hosp_det, probs = 0.975, na.rm = TRUE)
    ) %>%
    dplyr::group_by(region, exp_nr, rollback, reopening_multiplier_4, exp_name, icu_available, medsurg_available) %>%
    mutate(
      hosp_det_BelowCapacity = ifelse(max(median.hosp_det) < medsurg_available, 1, 0)
    )
  
  
  minCapacity_hosp <- dfAggr_hosp %>%
    group_by(region, rollback, reopening_multiplier_4, capacity_multiplier) %>%
    filter(hosp_det_BelowCapacity == 0 & median.hosp_det < medsurg_available) %>%
    group_by(region, rollback, reopening_multiplier_4, capacity_multiplier) %>%
    summarize(minCapacity_det = max(capacity_multiplier)) 
  
  df_hosp <- dfAggr_hosp %>% left_join(minCapacity_hosp,  by = c('region', 'rollback', 'reopening_multiplier_4', 'capacity_multiplier'))
  
  
  out <- list(df_crit, df_hosp)
  return(out)
}


f_processDat_minDate <- function(df) {
  
  
  colnames(df)[colnames(df) == "icu_available.y"] <- "icu_available"
  colnames(df)[colnames(df) == "medsurg_available.y"] <- "medsurg_available"
  
  dfAggr_crit_date <- df %>%
    dplyr::group_by(region,  capacity_multiplier, scen_num, exp_nr, rollback) %>%
    filter(crit_det >icu_available) %>%
    dplyr::group_by(region, exp_nr, exp_name,  rollback, capacity_multiplier) %>%
    dplyr::summarize(
      min.date = min(date, na.rm = TRUE)
    ) 
  
  out <-dfAggr_crit_date
  return(out)
}


f_thresholdScatterPlot <- function(channel = "critical", savePDF = FALSE, expLabel = expLabel) {
  

  if (channel == "critical") {
    df_crit <- f_processDat(df.exp)[[1]]

    
    pplot <- df_crit %>%
      filter(region %in% c(1:11)) %>%
      ggplot() +
      geom_hline(aes(yintercept = icu_available), linetype = "dashed") +
      facet_wrap(~region, scales = "free") +
      scale_color_viridis_d(option = "C") +
      scale_fill_viridis_d(option = "C") +
      theme(legend.position = "bottom") +
      customThemeNoFacet +
      geom_hline(yintercept = c(-Inf, Inf)) +
      geom_vline(xintercept = c(-Inf, Inf)) +
      scale_x_discrete(expand = c(0, 0)) +
      labs(
        color = "scenario", fill = "scenario",
        y = "Predicted ICU census",
        x = "% of available ICU beds at which 'trigger' is pulled"
      ) +
      geom_line(aes(
        x = as.factor(capacity_multiplier), y = median.crit_det,
        col = as.factor(rollback), group = exp_name
      ),
      size = 1, position = position_dodge(width = 0.4)
      ) +
      geom_point(aes(
        x = as.factor(capacity_multiplier), y = median.crit_det,
        fill = as.factor(rollback), group = exp_name
      ),
      size = 2, shape = 21, position = position_dodge(width = 0.4)
      )
    
    
    fname="_impact_on_ICU"
  #  ggsave(paste0("capacity_multiplier_", "_", fname, ".png"),
  #         plot = pplot, path = outdir, width = 12, height = 8, device = "png"
  #  )
    
    if (savePDF) {
      ggsave(paste0("capacity_multiplier_", "_", fname, ".pdf"),
             plot = pplot, path = outdir, width = 12, height = 8, device = "pdf"
      )
    }
  }
  
  
  
  if (channel == "hospitalized") {
    df_hosp <- f_processDat(df.exp)[[2]]
    

    
    pplot <- df_hosp %>%
      filter(region %in% c(1:11)) %>%
      ggplot() +
      geom_hline(aes(yintercept = medsurg_available), linetype = "dashed") +
      facet_wrap(~region, scales = "free") +
      scale_color_viridis_d(option = "C") +
      scale_fill_viridis_d(option = "C") +
      theme(legend.position = "bottom") +
      customThemeNoFacet +
      geom_hline(yintercept = c(-Inf, Inf)) +
      geom_vline(xintercept = c(-Inf, Inf)) +
      scale_x_discrete(expand = c(0, 0)) +
      labs(
        color = "scenario", fill = "scenario",
        y = "Predicted non-ICU  census",
        x = "% of available non-ICU  beds at which 'trigger' is pulled"
      ) +
      geom_line(aes(
        x = as.factor(capacity_multiplier), y = median.hosp_det,
        col = as.factor(rollback), group = exp_name
      ),
      size = 1, position = position_dodge(width = 0.4)
      ) +
      geom_point(aes(
        x = as.factor(capacity_multiplier), y = median.hosp_det,
        fill = as.factor(rollback), group = exp_name
      ),
      size = 2, shape = 21, position = position_dodge(width = 0.4)
      )
    
    
    fname="_impact_on_nonICU"
  #  ggsave(paste0("capacity_multiplier_", "_", fname, ".png"),
  #         plot = pplot, path = outdir, width = 12, height = 8, device = "png"
  #  )
    
    if (savePDF) {
      ggsave(paste0("capacity_multiplier_", "_", fname, ".pdf"),
             plot = pplot, path = outdir, width = 12, height = 8, device = "pdf"
      )
    }
  }
  
  return(pplot)
}


## ----------------------
### Generate plots
## ----------------------

exp_names <- list.dirs(simulation_output, recursive = FALSE, full.names = FALSE)
exp_names_crit <- exp_names[grep("regionreopening_ICU", exp_names)]
exp_names <- c(exp_names_crit)

datList <- list()
for (exp_name in exp_names) {
  
  # exp_name <- exp_names[2]
  print(exp_name)
  exp_dir <- file.path(file.path(simulation_output, exp_name))
  
  if (sum(grep("ICU", exp_name)) == 1) expLabel <- "critical"
  if (sum(grep("nonICU", exp_name)) == 1) expLabel <- "hospitalized"
  
  df.exp <- f_loadDat(exp_name)
  datList[[length(datList) + 1]] <- df.exp
  
  rm(exp_name)
}


df.exp <- datList %>% bind_rows(.id = "exp_nr")
df.exp$expLabel <- gsub(paste0(simdate, "_IL"), "", df.exp$exp_name)
df.exp$expLabel <- gsub(paste0("20200827", "_IL"), "", df.exp$expLabel)
df.exp$expLabel <- gsub(paste0("_regionreopening_ICU_"), "", df.exp$expLabel)
df.exp$expLabel <- gsub("perc_TriggeredRollback", "%", df.exp$expLabel)
unique(df.exp$expLabel)

df.exp$rollback <- factor(df.exp$expLabel,
                          levels = c("triggeredrollback", "triggeredrollbackdelay"),
                          labels = c("immediate", "delayed")
)

f_thresholdScatterPlot(channel = "critical", savePDF = FALSE)


##### Sow date of overflow
df_date <- f_processDat_minDate(df.exp)

summary(df_date$min.date)


pplot <- ggplot(data = df_date) +
  geom_point(aes(x = min.date, y =capacity_multiplier, fill = as.factor(rollback), group = as.factor(rollback)), 
             stat="identity", position="dodge", shape=21, show.legend = F, size = 3) +
  labs(
    x = "Date",
    y = "Capacity multiplier (%)",
    title = ""
  ) +
  facet_wrap(~region)+
  scale_color_manual(values=rev(c("orange","deepskyblue3")))+
  scale_fill_manual(values=rev(c("orange","deepskyblue3")))+
  background_grid() +
  customThemeNoFacet +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf))





ggsave(paste0("date_of_overflow_perCovidRegion_.png"),
       plot = pplot, path = file.path(simulation_output, exp_name), width = 12, height = 8, device = "png"
)



df.exp %>%
filter(region %in% c(1)) %>%
ggplot() + 
  geom_line(aes(x=date, y=crit_det, col=as.factor(capacity_multiplier), group=interaction(rollback, scen_num))) +
  facet_wrap(~region, scales="free") +
  scale_color_viridis_d(option="C")


df.exp$rollback <- factor(df.exp$expLabel,
                          levels = c("triggeredrollback", "triggeredrollbackdelay"),
                          labels = c("immediate action", "delayed action (14 days)")
)

reopenLabel <- unique(df.exp[,c("region", "reopening_multiplier_4")])

reopenLabel$reopenLabel <- paste0("Covid region ",as.character(reopenLabel$region)," (worst case ", reopenLabel$reopening_multiplier_4*100, "% reopening)")
reopenLabel$reopenLabel <- gsub("Covid region illinois (worst case NA% reopening)","Illinois", reopenLabel$reopenLabel )
reopenLabel <- reopenLabel %>% select(-reopening_multiplier_4)

df.exp <- left_join(df.exp, reopenLabel, by=c("region"))
unique(df.exp$reopenLabel)
df.exp$reopenLabel[df.exp$region=="illinois"] <- "Illinois"
unique(df.exp$reopenLabel)

for(reg in c("illinois", c(1:11))){
  
  
  plotdat <- subset(df.exp, region %in% reg)
  titlelab = unique(df.exp$reopenLabel)
  
  pplot <- plotdat %>%
    group_by(capacity_multiplier, rollback, region, date,icu_available.y) %>%
    summarize(
      median.crit_det = median(crit_det, na.rm = TRUE),
      q2.5.crit_det = quantile(crit_det, probs = 0.025, na.rm = TRUE),
      q97.5.crit_det = quantile(crit_det, probs = 0.975, na.rm = TRUE)
    ) %>%
    ggplot() + 
    geom_hline(aes(yintercept=icu_available.y), linetype="dashed") +
    geom_ribbon(aes(x=date, ymin=q2.5.crit_det,ymax=q97.5.crit_det,  fill=as.factor(capacity_multiplier), 
                    group=interaction(capacity_multiplier)), alpha=0.3) +
    geom_line(aes(x=date, y=median.crit_det, col=as.factor(capacity_multiplier), group=interaction(capacity_multiplier))) +
    facet_wrap(~rollback) +
    scale_color_viridis_d(option="C")+
    scale_fill_viridis_d(option="C")+
    customThemeNoFacet +
    labs(title=titlelab,subtitle="",
         y="ICU census", x="", color="trigger at % of available ICU beds filled", fill="trigger at % of available ICU beds filled")
  
  
 ggsave(paste0("ICU_timeline_",reg,".png"),
        plot = pplot, path = file.path(outdir), width = 12, height = 6, device = "png"
  )
  
  ggsave(paste0("ICU_timeline_",reg,".pdf"),
         plot = pplot, path = file.path(outdir), width = 12, height = 6, device = "pdf"
  )
}


