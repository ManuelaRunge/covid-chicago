###=============================================
### Plot ICU over time per region
###=============================================

library(tidyverse)
library(cowplot)
library(data.table)

theme_set(theme_minimal())

runInBatchMode <- FALSE

if (runInBatchMode) {
  cmd_agrs <- commandArgs()
  length(cmd_agrs)
  exp_name <- cmd_agrs[length(cmd_agrs) - 2]
  Location <- cmd_agrs[length(cmd_agrs) - 1]
  workingDir <- cmd_agrs[length(cmd_agrs)]

  exp_names <- exp_name
  simdate <- strsplit(exp_name, split = "_IL_")[[1]][1]
  sim_dir <- "/projects/p30781/covidproject/covid-chicago/_temp"

  setwd(workingDir)
  source("load_paths.R")
  source("setup.R")
  source("processing_helpers.R")
  source("publication_scripts_ICUprob/functions.R")
} else {
  Location <- "Local"
  workingDir <- getwd()
  source("load_paths.R")
  source("setup.R")
  source("processing_helpers.R")
  source("publication_scripts_ICUprob/functions.R")

  sim_dir <- "/projects/p30781/covidproject/covid-chicago/_temp"
  if (Location == "Local") {
    #simdate <-'20200919'
    #simdate <-'20201121'
    simdate <-'20201212'
    sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)
    if(!dir.exists(file.path(sim_dir, "ICU_bar_plots")))dir.create(file.path(sim_dir, "ICU_bar_plots"))
    
    
    exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
    exp_names <- exp_names[grep("IL_regreopen",exp_names)]
    exp_names <- exp_names[c(grep("daysdelay",exp_names),grep("counterfactual",exp_names))]
  }
}


#exp_names <- exp_names[grep("counterfactual", exp_names)]
if (length(exp_names) == 1) {
  sim_dir <- file.path(sim_dir, exp_names[1])
  dat <- fread(file.path(sim_dir, "trajectories_aggregated.csv"))
  dat$exp_name <- exp_names[1]
} else {
  dat <- f_combineDat(sim_dir, exp_names, "trajectories_aggregated.csv")
}

dat$date <- as.Date(dat$date)
tapply(dat$date , dat$exp_name , summary)

### Data processing before plotting

dat <- dat %>%
  # filter(date  >= as.Date("2020-07-01") & date <= as.Date("2021-01-01")) %>%
  filter(date >= as.Date("2020-07-01")) %>%
  mutate(geography_modeled = gsub("EMS-", "covidregion_", ems)) %>%
  filter(ems != "All") %>%
  dplyr::select(-ems)

regions <- paste0("covidregion_", c(1,4,11))

dat <- dat %>% filter(geography_modeled %in% regions)
dat$scen_name <- gsub(paste0(simdate, "_IL_regreopen"), "", dat$exp_name)

dat <- dat %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat$rollback[is.na(dat$rollback)] <- "counterfactual"
dat$region <- factor(dat$geography_modeled, levels = regions, labels = gsub("covidregion_", "Region ", regions))

dat$capacity_multiplier_fct <- round(dat$capacity_multiplier * 100, 0)
fct_labels <- sort(unique(dat$capacity_multiplier_fct))
dat$capacity_multiplier_fct[dat$rollback == "counterfactual"] <- "counterfactual"
dat$capacity_multiplier_fct <- factor(dat$capacity_multiplier_fct,
  levels = c(fct_labels, "counterfactual"),
  labels = c(fct_labels, "counterfactual")
)

### check
str(dat)
table(dat$region)
table(dat$capacity_multiplier, dat$capacity_multiplier_fct)
table(dat$region, dat$reopen)
table(dat$region, dat$delay)
table(dat$region, dat$rollback)

### load capacity
capacityDat <- load_new_capacity(filedate = "20200915") %>% 
  mutate(geography_modeled = paste0("covidregion_", geography_name))

dat <- f_addVar(dat, capacityDat)

### prepare
dat_counterfactual <- dat %>%
  filter(rollback == "counterfactual") %>%
  dplyr::select(-rollback, -delay, -capacity_multiplier_fct)

dat_scen <- dat %>% filter(rollback != "counterfactual" & delay == unique(dat$delay)[1])

####====================================
### PLOTS - basic descriptive
####====================================
customTheme <- f_getCustomTheme()
subregions <-  regions #c("covidregion_1", "covidregion_4", "covidregion_11")

if (length(unique(dat$rollback)) > 1) {
  for (reg in subregions) {
    icu_available <- dat_scen %>%
      filter(geography_modeled == reg) %>%
      dplyr::select(icu_available) %>%
      unique() %>%
      as.numeric()

    pplot <-
      ggplot(data = subset(dat_scen, geography_modeled == reg)) +
      # geom_rect(xmin=-Inf, xmax=as.Date("2020-10-01"), ymin=-Inf, ymax-Inf, alpha=0.01, fill="lightgrey") +
      geom_line(
        data = subset(dat_counterfactual, geography_modeled == reg),
        aes(x = date, y = crit_det_median)
      ) +
      geom_ribbon(
        data = subset(dat_counterfactual, geography_modeled == reg),
        aes(x = date, ymin = crit_det_50CI_lower, ymax = crit_det_50CI_upper), alpha = 0.3
      ) +
      geom_ribbon(
        data = subset(dat_scen, geography_modeled == reg),
        aes(x = date, ymin = crit_det_50CI_lower, ymax = crit_det_50CI_upper, fill = capacity_multiplier_fct, group = capacity_multiplier_fct), alpha = 0.3
      ) +
      geom_line(
        data = subset(dat_scen, geography_modeled == reg),
        aes(x = date, y = crit_det_median, col = capacity_multiplier_fct, group = capacity_multiplier_fct)
      ) +
      geom_hline(aes(yintercept = icu_available), linetype = "dashed", col = "dodgerblue3") +
      # geom_vline(xintercept=as.Date("2020-10-01"))+
      facet_wrap(reopen ~ rollback, scales = "free") +
      scale_x_date(date_breaks = "30 days", date_labels = "%b") +
      labs(
        title = reg,
        subtitle = paste0("\nicu_available: ", icu_available),
        x = "",
        y = "predicted ICU census",
        col = "ICU trigger threshold",
        fill = "ICU trigger threshold",
        caption = "median and 50% IQR"
      ) +
      customTheme

    if (!dir.exists(file.path(sim_dir, "ICU_timeline_plots"))) dir.create(file.path(sim_dir, "ICU_timeline_plots"))
    ggsave(paste0("ICU_timelineplot_", reg, ".pdf"),
      plot = pplot,
      path = file.path(sim_dir, "ICU_timeline_plots"), width = 12, height = 9, device = "pdf"
    )
    rm(pplot, reg)
  }
}

for (reg in subregions) {
  icu_available <- dat_counterfactual %>%
    filter(geography_modeled == reg) %>%
    dplyr::select(icu_available) %>%
    unique() %>%
    as.numeric()

  pplot <-
    ggplot(data = subset(dat_counterfactual, geography_modeled == reg)) +
    # geom_rect(xmin=-Inf, xmax=as.Date("2020-10-01"), ymin=-Inf, ymax=Inf, alpha=0.08, fill="lightgrey") +
    geom_vline(xintercept = as.Date("2020-10-01"), col = "lightgrey") +
    geom_line(
      data = subset(dat_counterfactual, geography_modeled == reg),
      aes(x = date, y = crit_det_median)
    ) +
    geom_ribbon(
      data = subset(dat_counterfactual, geography_modeled == reg),
      aes(x = date, ymin = crit_det_50CI_lower, ymax = crit_det_50CI_upper), alpha = 0.4
    ) +
    geom_ribbon(
      data = subset(dat_counterfactual, geography_modeled == reg),
      aes(x = date, ymin = crit_det_95CI_lower, ymax = crit_det_95CI_upper), alpha = 0.3
    ) +
    geom_hline(aes(yintercept = icu_available), linetype = "dashed", col = "dodgerblue3") +
    facet_wrap(~reopen, scales = "free") +
    scale_x_date(date_breaks = "30 days", date_labels = "%b") +
    labs(
      title = reg,
      subtitle = paste0("\nicu_available: ", icu_available),
      x = "",
      y = "predicted ICU census",
      col = "ICU trigger threshold",
      fill = "ICU trigger threshold",
      caption = "median and 50%, 95% IQR"
    ) +
    customTheme

  if (!dir.exists(file.path(sim_dir, "ICU_timeline_plots"))) dir.create(file.path(sim_dir, "ICU_timeline_plots"))
  ggsave(paste0("ICU_timelineplot_counterfactual_", reg, ".pdf"),
    plot = pplot,
    path = file.path(sim_dir, "ICU_timeline_plots"), width = 12, height = 9, device = "pdf"
  )
  ggsave(paste0("ICU_timelineplot_counterfactual_", reg, ".png"),
    plot = pplot,
    path = file.path(sim_dir, "ICU_timeline_plots"), width = 12, height = 9, device = "png"
  )
  rm(pplot, reg)
}

####====================================
## Recommended versus selected
####====================================
subregions <-  c("covidregion_1", "covidregion_4", "covidregion_11")


unique(dat_scen$capacity_multiplier_fct)

dat_scen_sub <- dat_scen %>% filter(geography_modeled %in% subregions) %>%
  mutate(trigger_recommended= ifelse(geography_modeled=="covidregion_1", "44","67"  ),
         trigger_recommended= ifelse(geography_modeled=="covidregion_4", "89", trigger_recommended)) %>% 
  filter(capacity_multiplier_fct == trigger_recommended | capacity_multiplier_fct == "78") 

dat_counterfactual_sub <- dat_counterfactual %>% filter(geography_modeled %in% subregions)

plotdat <- dat_scen_sub%>% filter(rollback=="sm4" & delay=="0daysdelay")
pplot <-
  ggplot(data = plotdat) +
  #geom_rect(xmin=-Inf, xmax=as.Date("2020-10-01"), ymin=-Inf, ymax-Inf, alpha=0.01, fill="lightgrey") +
 # geom_line(
 #   data = dat_counterfactual_sub,
 #   aes(x = date, y = crit_det_median/icu_available)
 # ) +
 # geom_ribbon(
 #   data = dat_counterfactual_sub,
 #   aes(x = date, ymin = crit_det_50CI_lower/icu_available, ymax = crit_det_50CI_upper/icu_available), 
  #  alpha = 0.3
 # ) +
  geom_ribbon(
    data =plotdat,
    aes(x = date, ymin = crit_det_50CI_lower/icu_available, ymax = crit_det_50CI_upper/icu_available, 
        fill = capacity_multiplier_fct, 
        group = capacity_multiplier_fct), alpha = 0.4
  ) +
  geom_ribbon(
    data =plotdat,
    aes(x = date, ymin = crit_det_95CI_lower/icu_available, ymax = crit_det_95CI_upper/icu_available, 
        fill = capacity_multiplier_fct, 
        group = capacity_multiplier_fct), alpha = 0.2
  ) +
  geom_line(
    data = plotdat,
    aes(x = date, y = crit_det_median/icu_available, 
        col = capacity_multiplier_fct, 
        group = capacity_multiplier_fct),size=1.3
  ) +
  geom_hline(aes(yintercept = icu_available/icu_available), 
             linetype = "dashed", col = "dodgerblue3") +
  # geom_vline(xintercept=as.Date("2020-10-01"))+
  facet_wrap(reopen ~ geography_modeled, scales = "free") +
  scale_x_date(date_breaks = "30 days", date_labels = "%b", lim=c(as.Date("2020-09-01"), as.Date("2021-01-15"))) +
  scale_y_continuous(lim=c(0, 2), expand=c(0,0))+
  labs(
    x = "",
    y = "predicted ICU census of available ICUs (%)",
    col = "ICU trigger threshold",
    fill = "ICU trigger threshold",
    caption = "median and 50% IQR"
  ) +
  customTheme

pplot

