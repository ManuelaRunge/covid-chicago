## ==================================================
# R script that analysis trajectoriesDat
## ==================================================
library(tidyverse)
library(scales)
library(data.table)

f_runDescriptivePlots <- function(perRestoreRegion = TRUE) {
  selected_ems <- c(1:11)
  emsvars_temp <- c("critical_EMS.")
  # selected_ems <-c("northeast","northcentral","southern","central")
  # emsvars_temp <- c("critical_")

  emsvars <- NULL
  for (ems in selected_ems) {
    emsvars <- c(emsvars, paste0(emsvars_temp, ems))
  }


  groupvars <- c("startdate", "Date", "time", "scen_num", "sample_num", "run_num", "reopening_multiplier_4", "detection_success", "isolation_success", "grpvar")
  (keepvars <- c(groupvars, emsvars))

  subdat <- trajectoriesDat %>%
    dplyr::select(keepvars) %>%
    pivot_longer(cols = -c(groupvars)) %>%
    dplyr::mutate(name = gsub("All", "EMS.IL", name)) %>%
    dplyr::mutate(name = gsub("[.]", "_", name)) %>%
    separate(name, into = c("outcome", "region"), sep = "_EMS_") %>%
    dplyr::filter(Date >= reopeningdate) %>%
    dplyr::select(-c(time)) %>%
    f_addRestoreRegion()

  adminlevel <- "covidregion"
  if (perRestoreRegion) {
    adminlevel <- "restoreregion"
    subdat <- subdat %>%
      dplyr::group_by(startdate, Date, restore_region, reopening_multiplier_4, scen_num, sample_num, run_num, detection_success, isolation_success, grpvar) %>%
      dplyr::summarize(value = sum(value)) %>%
      mutate(region = tolower(restore_region))
  }


  capacity <- load_capacity(unique(subdat$region)) %>% dplyr::rename(capacity = critical)
  subdat <- merge(subdat, capacity, by.x = "region", by.y = "geography_name")

  trajectoriesDatAggr <- f_aggrDat(subdat, c("Date", "region", "reopening_multiplier_4", "capacity"), "value")


  subdatAggr <- subdat %>%
    dplyr::group_by(region, scen_num, reopening_multiplier_4) %>%
    dplyr::mutate(valueMax = max(value, na.rm = TRUE)) %>%
    filter(valueMax <= capacity) %>%
    f_aggrDat(c("Date", "region", "reopening_multiplier_4", "capacity"), "value")


  ### Limit for ICU beds
  if (length(unique(subdat$region)) == 11) subdat$region <- factor(subdat$region, levels = c(1:11), labels = c(1:11))
  for (i in unique(subdat$reopening_multiplier_4)) {
    tdat <- subset(trajectoriesDatAggr, reopening_multiplier_4 == i)
    tdatSub <- subset(subdatAggr, reopening_multiplier_4 == i)

    l_plot2 <- ggplot(data = tdat) +
      theme_minimal() +
      geom_ribbon(aes(x = Date, ymin = min.val, ymax = max.val), fill = "deepskyblue3", alpha = 0.2) +
      geom_ribbon(aes(x = Date, ymin = q2.5, ymax = q97.5), fill = "deepskyblue3", alpha = 0.35) +
      geom_ribbon(aes(x = Date, ymin = q25, ymax = q75), fill = "deepskyblue3", alpha = 0.5) +
      geom_ribbon(aes(x = Date, ymin = lower.ci.val, ymax = upper.ci.val), fill = "deepskyblue3", alpha = 0.7) +
      geom_line(aes(x = Date, y = mean.val), col = "deepskyblue4", size = 1.3) +
      geom_ribbon(data = tdatSub, aes(x = Date, ymin = min.val, ymax = max.val), fill = "brown3", alpha = 0.2) +
      geom_ribbon(data = tdatSub, aes(x = Date, ymin = q2.5, ymax = q97.5), fill = "brown3", alpha = 0.35) +
      geom_ribbon(data = tdatSub, aes(x = Date, ymin = q25, ymax = q75), fill = "brown3", alpha = 0.5) +
      geom_line(data = tdatSub, aes(x = Date, y = mean.val), col = "brown3", size = 1.3) +
      geom_hline(aes(yintercept = capacity)) +
      scale_color_viridis(discrete = TRUE) +
      labs(title = paste0("reopen ", i, " %"), subtitle = "", y = "Predicted ICU bed demand") +
      customThemeNoFacet +
      scale_x_date(breaks = "1 month", labels = date_format("%b")) +
      facet_wrap(~region, scales = "free")



    ggsave(paste0("reopen_", i, "_", adminlevel, "_capacity_timeline.png"),
      plot = l_plot2, path = file.path(exp_dir), width = 10, height = 6, device = "png"
    )
    ggsave(paste0("reopen_", i, "_", adminlevel, "_capacity_timeline.pdf"),
      plot = l_plot2, path = file.path(exp_dir), width = 10, height = 6, device = "pdf"
    )
  }


  pplot <- ggplot(data = subset(trajectoriesDat, Date >= as.Date("2020-12-30") & Date <= as.Date("2020-12-31")), aes(x = grpvar, y = crit_cumul_All, group = grpvar)) +
    theme_minimal() +
    geom_violin(fill = "deepskyblue3", col = "deepskyblue3") +
    stat_summary(fun = median, geom = "point", size = 2, color = "black") +
    geom_hline(yintercept = sum(capacity$capacity)) +
    scale_y_continuous(expand = c(0, 0), labels = scales::comma) +
    labs(y = "Daily critical cases", x = "") +
    customThemeNoFacet +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

  ggsave(paste0(adminlevel, "_boxplot_by_grp_cumulEnd2020.png"),
    plot = pplot, path = file.path(exp_dir), width = 10, height = 6, device = "png"
  )
  ggsave(paste0(adminlevel, "_boxplot_by_grp_cumulEnd2020.pdf"),
    plot = pplot, path = file.path(exp_dir), width = 10, height = 6, device = "pdf"
  )


  #####  visualize detection variables
  p1 <- ggplot(data = trajectoriesDat) +
    theme_bw() +
    geom_line(data = subset(trajectoriesDat, Date >= "2020-06-14"), aes(
      x = Date, y = d_As_t, col = (round(detection_success, 1)),
      group = interaction(detection_success, scen_num)
    ), size = 1.3, show.legend = FALSE) +
    geom_line(data = subset(trajectoriesDat, Date <= "2020-06-15"), aes(
      x = Date, y = d_As_t,
      group = interaction(detection_success, scen_num)
    ), size = 1, col = "grey", show.legend = FALSE) +
    scale_color_viridis(discrete = FALSE) +
    labs(title = detectionVar_label, subtitle = "") +
    customThemeNoFacet

  p2 <- ggplot(data = trajectoriesDat) +
    theme_bw() +
    geom_line(data = subset(trajectoriesDat, Date >= "2020-06-14"), aes(
      x = Date, y = d_Sym_t, col = as.factor(round(grpvar, 1)),
      group = interaction(grpvar, detection_success, scen_num)
    ), size = 1.3, show.legend = FALSE) +
    geom_line(data = subset(trajectoriesDat, Date <= "2020-06-15"), aes(
      x = Date, y = d_Sym_t,
      group = interaction(grpvar, detection_success, scen_num)
    ), size = 0.7, col = "grey", show.legend = FALSE) +
    scale_color_viridis(discrete = TRUE) +
    labs(title = groupVar_label, subtitle = "") +
    customThemeNoFacet

  pplot <- plot_grid(p1, p2, nrow = 1)

  ggsave(paste0(adminlevel, "_detection_timeline.png"),
    plot = pplot, path = file.path(exp_dir), width = 14, height = 6, device = "png"
  )
  ggsave(paste0(adminlevel, "_detection_timeline.pdf"),
    plot = pplot, path = file.path(exp_dir), width = 14, height = 6, device = "pdf"
  )

  ##### timeline of outcomes
  pplot <- ggplot(data = subset(trajectoriesDat, isolation_success > 0.9)) +
    theme_bw() +
    geom_line(aes(
      x = Date, y = critical_All, col = as.factor(round(detection_success, 1)),
      group = interaction(detection_success, scen_num)
    ), size = 2, show.legend = TRUE) +
    facet_grid(backtonormal_multiplier ~ grpvar, labeller = labeller(.rows = label_both, .cols = label_both)) +
    scale_color_viridis(discrete = TRUE) +
    geom_hline(yintercept = capacitysum$capacity) +
    labs(color = "detection_success") +
    customThemeNoFacet

  ggsave(paste0(adminlevel, "_sample_timeline.png"),
    plot = pplot, path = file.path(exp_dir), width = 16, height = 6, device = "png"
  )
  ggsave(paste0(adminlevel, "_sample_timeline.pdf"),
    plot = pplot, path = file.path(exp_dir), width = 16, height = 6, device = "pdf"
  )
}






#### Run either local (for loop) or on NUCLUSTER

if (!exists("LOCAL")) runinBatchMode <- TRUE
if (exists("LOCAL")) runinBatchMode <- FALSE

if (runinBatchMode) {
  # cmd_agrs <- commandArgs()
  # length(cmd_agrs)

  # simdate <- cmd_agrs[length(cmd_agrs)]

  # task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
  # print(task_id)
  # print(simdate)
  simdate <- "20200731"

  ## Load packages
  packages_needed <- c("tidyverse", "reshape", "cowplot", "scales", "readxl", "viridis", "stringr", "broom")
  lapply(packages_needed, require, character.only = TRUE)

  ## Load directories and custom objects and functions
  setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
  source("load_paths.R")
  source("processing_helpers.R")
  source("ct_analysis/helper_functions_CT.R")


  exp_names <- list.dirs(file.path(ct_dir, simdate), recursive = FALSE, full.names = FALSE)
  # if(task_id> length(exp_names)) stop()

  # exp_name <- exp_names[task_id]
  for (exp_name in exp_names) {
    # exp_name = exp_names[2]
    source("ct_analysis/loadData_defineParam.R")

    ## Run analysis
    f_runDescriptivePlots(perRestoreRegion = TRUE)
    f_runDescriptivePlots(perRestoreRegion = FALSE)
  }
}


if (runinBatchMode == FALSE) {
  f_runDescriptivePlots(perRestoreRegion = TRUE)
  f_runDescriptivePlots(perRestoreRegion = FALSE)
}
