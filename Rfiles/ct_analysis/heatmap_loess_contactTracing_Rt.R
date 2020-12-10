## --------------------------------------------
## Generate heatmap and supplementary plots per EMS or aggregated region
### Using loess regression per EMS and grpvar
## --------------------------------------------


f_runHeatmapAnalysis_Rt <- function(ems, geography = "EMS", startdate = "2020-08-20", enddate = "2020-08-21") {

  # ems <- emsregions[1]
  if (geography == "Region") {
    selected_ems <- as.numeric(regions[[ems]])
  } else {
    selected_ems <- ems
  }

  dat <- read.csv(file.path(Rt_dir, "EMS_combined_estimated_Rt.csv")) %>%
    mutate(Date = as.Date(Date)) %>%
    filter(region == ems &
      Date >= as.Date(startdate) & Date < as.Date(enddate)) %>%
    dplyr::group_by(region, Date, t_start, scen_num, t_end, isolation_success, detection_success, grpvar) %>%
    dplyr::summarize(average_median_Rt = mean(Mean)) %>%
    dplyr::mutate(Rt_fct = ifelse(average_median_Rt < 1, "<1", ">=1"), capacity = 1)

  dat$region_label <- factor(dat$region, levels = c(1:11), labels = paste0("covid region ", c(1:11), "\n"))

  if (length(selected_ems) > 1) {
    dat <- dat %>%
      group_by(Date, t_start, scen_num, t_end, isolation_success, detection_success, grpvar) %>%
      dplyr::summarize(average_median_Rt = mean(average_median_Rt)) %>%
      dplyr::mutate(Rt_fct = ifelse(average_median_Rt < 1, "<1", ">=1"), capacity = 1)
  }


  dat <- f_valuefct(dat)

  fitlist <- list()

  # if( min(dat$average_median_Rt)>1)next

  for (grp in unique(dat$grpvar)) {
    # grp <- unique(dat$grpvar)[1]
    #### Do loess regression
    detection_success <- seq(0, 1, 0.001)
    isolation_success <- seq(0, 1, 0.001)
    t_matdat <- expand.grid(detection_success = detection_success, isolation_success = isolation_success)

    m <- loess(average_median_Rt ~ detection_success * isolation_success,
      span = 0.5,
      degree = 2, data = subset(dat, grpvar == grp)
    )

    temp.fit_mat <- predict(m, t_matdat)
    temp.fit <- melt(temp.fit_mat)
    temp.fit$detection_success <- gsub("detection_success=", "", temp.fit$detection_success)
    temp.fit$isolation_success <- gsub("isolation_success=", "", temp.fit$isolation_success)


    showPlot <- FALSE
    if (showPlot) {
      library(plotly)
      fig <- plot_ly(
        x = temp_fit$detection_success,
        y = temp_fit$isolation_success,
        z = temp_fit$value,
        type = "contour"
      )
      print(fig)
    }

    temp.fit <- f_valuefct(temp.fit)

    temp.fit$detection_success <- as.numeric(temp.fit$detection_success)
    temp.fit$isolation_success <- as.numeric(temp.fit$isolation_success)

    temp.fit$grpvar <- grp
    fitlist[[length(fitlist) + 1]] <- temp.fit

    rm(temp.fit_mat, temp.fit)
  }

  dtfit <- bind_rows(fitlist)
  rm(fitlist)

  thresholdDat <- dtfit %>%
    dplyr::filter(value <= 1) %>%
    dplyr::group_by(detection_success, grpvar) %>%
    dplyr::filter(isolation_success == min(isolation_success)) %>%
    dplyr::mutate(region = ems)


  save(dtfit, file = file.path(heatmapRtDir, paste0(ems, "_dtfit_Rt.Rdata")))
  if (file.exists(file.path(heatmapRtDir, paste0(ems, "_dtfit_Rt.Rdata")))) print("Rdata saved")


  write.csv(thresholdDat, file = file.path(heatmapRtDir, paste0(ems, "_loess_Rt.csv")), row.names = FALSE)

  p1 <- ggplot(data = subset(dtfit, !is.na(value_fct)), aes(x = detection_success, y = isolation_success)) +
    theme_minimal() +
    geom_tile(aes(fill = value_fct), alpha = 0.8) +
    geom_line(data = subset(thresholdDat, isolation_success != min(isolation_success)), aes(x = detection_success, y = isolation_success), size = 1.3) +
    scale_fill_viridis(option = "C", discrete = TRUE) +
    labs(
      x = "detections (%)",
      y = "isolation success (%)",
      col = "",
      fill = expression(italic(R[t])),
      shape = "",
      linetype = ""
    ) +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    facet_wrap(~grpvar) +
    customThemeNoFacet

  p2 <- p1 + geom_point(data = dat, aes(x = detection_success, y = isolation_success, fill = value_fct), shape = 21, size = 3, show.legend = FALSE)

  plotname1 <- paste0("EMS", "_", ems, "_Rt_heatmap_loess")

  ggsave(paste0(plotname1, ".png"),
    plot = p2, path = file.path(heatmapRtDir), width = 12, height = 5, dpi = 300, device = "png"
  )

  # ggsave(paste0(plotname1, ".pdf"),
  #       plot = p1, path = file.path(heatmapRtDir), width = 12, height = 5, dpi = 300, device = "pdf"
  # )
}



#### Run either local (for loop) or on NUCLUSTER

if (!exists("LOCAL")) {
  runinBatchMode <- TRUE
} else {
  runinBatchMode <- FALSE
}


if (runinBatchMode) {
  cmd_agrs <- commandArgs()
  length(cmd_agrs)

  ems <- cmd_agrs[length(cmd_agrs)]
  task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
  print(task_id)
  print(ems)
  ems <- task_id
  # ems=1


  ## Load packages
  packages_needed <- c("tidyverse", "reshape", "cowplot", "scales", "readxl", "viridis", "stringr", "broom")
  lapply(packages_needed, require, character.only = TRUE)

  ## Load directories and custom objects and functions
  setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
  source("load_paths.R")
  source("processing_helpers.R")
  source("ct_analysis/helper_functions_CT.R")


  simdate <- "20200902"
  exp_names <- list.dirs(file.path(ct_dir, simdate), recursive = FALSE, full.names = FALSE)
  exp_names <- exp_names[grep("reopen_contact", exp_names)]
  # exp_names <- exp_names[!(exp_names == "20200731_IL_reopen_contactTracing_TD")]
  # exp_names = "20200731_IL_reopen_contactTracing_TD"

  for (exp_name in exp_names) {

    ## Run analysis
    source("ct_analysis/loadData_defineParam.R")

    heatmapRtDir <- file.path(exp_dir, "heatmap_Rt")
    if (!dir.exists(heatmapRtDir)) dir.create(heatmapRtDir)
    if (!file.exists(file.path(Rt_dir, "EMS_combined_estimated_Rt.csv"))) next
    f_runHeatmapAnalysis_Rt(ems, startdate = "2020-10-20", enddate = "2020-08-21")
  }
} else {
  #geography="EMS"
  if (geography == "EMS") emsregions <- c(1:11)
  if (geography == "Region") emsregions <- names(regions)
  # emsregions <- "Illinois"

  for (ems in emsregions) {
    f_runHeatmapAnalysis_Rt(ems)
  }
  
  
}


