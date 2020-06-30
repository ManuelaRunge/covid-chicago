## --------------------------------------------
## Generate heatmap and supplementary plots per EMS or aggregated region
## --------------------------------------------

if (geography == "EMS") emsregions <- c(1:11)
if (geography == "Region") emsregions <- names(regions)


for (ems in emsregions) {
  # ems <- emsregions[1]
  if (geography == "Region") {
    selected_ems <- as.numeric(regions[[ems]])
  } else {
    selected_ems <- ems
  }

  capacity <- load_capacity( selected_ems)
  capacity$deaths <- 0


  tempdat <- getdata(trajectoriesDat, selected_ems)
  tempdat$region <- ems
 # write.csv(tempdat, file.path(ems_dir, paste0(geography, "_", ems, "_dat.csv")))

  ### Thresholds from predictions
  thresholdDat <- list()
  ### Thresholds from linear model
  h_thresholdDat <- list()

  # outcomeList <- c("critical", "hospitalized", "deaths", "ventilators")
  # Focus on ICU beds
  outcomeList <- c("critical")

  for (selected_outcome in outcomeList) {
    # selected_outcome = "critical"

    capacityline <- as.numeric(capacity[colnames(capacity) == selected_outcome])

    if (selected_outcome == "ventilators") {
      plotdat <- tempdat %>%
        filter(outcome == "crit_det") %>%
        mutate(
          value = value * 0.660,
          outcome = selected_outcome
        ) %>%
        mutate(region = ems) %>%
        as.data.frame()
    } else {
      plotdat <- tempdat %>%
        filter(outcome == selected_outcome) %>%
        mutate(region = ems) %>%
        as.data.frame()
    }


    peakTimes <- plotdat %>%
      filter(Date > min(Date)+10) %>%
      group_by(N, Ki, region, isolation_success, detection_success, grpvar, scen_num, sample_num, run_num) %>%
      filter(value == max(value)) %>%
      rename(Date_peak = Date) %>%
      select(N, Ki, region, Date_peak, outcome, isolation_success, detection_success, grpvar, scen_num, sample_num, run_num)
    summary(peakTimes$Date_peak)
    
    ## Add peak date to plotdat
    plotdat <- plotdat %>%
      left_join(peakTimes, by = c("N", "Ki", "region", "outcome", "scen_num", "sample_num", "run_num", "isolation_success", "detection_success", "grpvar"))

    showScatter=FALSE
    if(showScatter){
    scatterplot <- plotdat %>%
      filter(Date == Date_peak) %>%
      mutate(capacity = capacityline,
            belowCapacity = ifelse(value <= capacity, "yes", "no") )  %>%
      ggplot() + 
      theme_minimal() +
      geom_point(aes(x=detection_success, y=isolation_success,fill=value, group=scen_num),size=3,shape=21) +
      scale_fill_viridis(option = "C", discrete = FALSE, direction = -1) +
     customThemeNoFacet 
     
     ggsave(paste0(ems, "_method_scatterplot.pdf"),
            plot = scatterplot, path = file.path(ems_dir), width = 5, height =4, device = "pdf"
     )
    }
    ## Filter dataset to get threshold values
    threshold_param <- plotdat %>%
      filter(Date == Date_peak) %>%
      mutate(capacity = capacityline) %>%
      filter(outcome == selected_outcome) %>%
      filter(value <= capacity) %>%
      group_by(region, detection_success, grpvar, scen_num, sample_num, run_num) %>%
      filter(isolation_success == min(isolation_success)) %>%
      dplyr::select(region, N, Ki, Date_peak, scen_num, sample_num, run_num, detection_success, isolation_success, grpvar, outcome, value, capacity)


    thresholdDat[[selected_outcome]] <- threshold_param

    # Aggregate dataset for plot
    plotdatAggr <- plotdat %>%
      group_by(Date, region, outcome, isolation_success, grpvar, detection_success) %>%
      summarize(
        Date_peak = mean(Date_peak),
        value.mean = mean(value, na.rm = T),
        value.min = min(value, na.rm = T),
        value.max = max(value, na.rm = T)
      )

    p_plot <- ggplot(data = subset(plotdatAggr, Date == plotdatAggr$Date_peak)) +
      theme_cowplot() +
      geom_pointrange(aes(
        x = detection_success, y = value.mean,
        ymin = value.min, ymax = value.max, col = isolation_success, shape = as.factor(grpvar)
      ),
      size = 0.7
      ) +
      labs(
        title = paste0(geography, " ", unique(plotdat$region)),
        subtitle = paste0(
          "Time point: peak of epidemic",
          "\nContact tracing start: ", reopeningdate
        ),
        shape = groupVar_label,
        x = detectionVar_label,
        y = selected_outcome
      ) +
      scale_color_viridis(option = "D", discrete = FALSE, direction = -1) +
      geom_hline(yintercept = capacityline, linetype = "dashed")

    ggsave(paste0(geography, "_", ems, "_", selected_outcome, "_scatter.png"),
      plot = p_plot, path = file.path(ems_dir), width = 8, height = 6, dpi = 300, device = "png"
    )


    ### Line plot filtered with threshold value and heatmap
    if (dim(threshold_param)[1] == 0) next
    temp <- threshold_param %>%
      mutate(threshold = 1) %>%
      dplyr::select(-c(outcome, value, capacity))

    testdat <- plotdat %>%
      left_join(temp, by = c("region", "Date_peak", "scen_num", "sample_num", "run_num", "grpvar", "detection_success", "isolation_success")) %>%
      dplyr::filter(threshold == 1)
    # group_by(grpvar) %>%
    # filter(detection_success <= 0.556)
    testdatAggr <- testdat %>%
      group_by(region, Date, detection_success, isolation_success, grpvar) %>%
      summarize(value.min = min(value), value.max = max(value))

    labelDat <- testdat %>%
      filter(Date == max(Date)) %>%
      group_by(region, Date, isolation_success, detection_success, grpvar) %>%
      summarize(value = mean(value), detection_success = round(min(detection_success), 2))


    heatmap_out <- f_heatmap(df = subset(plotdat, Date == plotdat$Date_peak), selected_outcome,groupVar=groupVar, scalePop = scalePop)

    h_plot <- heatmap_out[[1]]
    h_threshold <- heatmap_out[[2]]
    h_plot_new <- heatmap_out[[3]]

    plotname1 = paste0(geography, "_", ems, "_", selected_outcome, "_heatmap")
    plotname2 = paste0(geography, "_", ems, "_", selected_outcome, "_heatmap.v2")
    if(scalePop==TRUE) plotname1 = paste0(plotname1, "_scl")
    if(scalePop==TRUE) plotname2 = paste0(plotname2, "_scl")
    
    ggsave(paste0(plotname1, ".pdf"),
      plot = h_plot, path = file.path(ems_dir), width = 16, height = 6, dpi = 300, device = "pdf"
    )
    ggsave(paste0(plotname2, ".pdf"),
      plot = h_plot_new, path = file.path(ems_dir), width = 6, height = 6, dpi = 300, device = "pdf"
    )
    ggsave(paste0(plotname1, ".png"),
           plot = h_plot, path = file.path(ems_dir), width = 16, height = 6, dpi = 300, device = "png"
    )
    ggsave(paste0(plotname2, ".png"),
           plot = h_plot_new, path = file.path(ems_dir), width = 6, height = 6, dpi = 300, device = "png"
    )
    rm(h_plot)
    if (dim(h_threshold)[1] != 0) {
      h_threshold <- h_threshold %>%
        mutate(
          outcome = selected_outcome,
          region = ems
        ) %>%
        rename(
          detection_success = x,
          isolation_success = ythreshold
        )

      h_thresholdDat[[length(h_thresholdDat) + 1]] <- h_threshold
    }
  }

  thresholdDat <- do.call(rbind.data.frame, thresholdDat)
  write.csv(thresholdDat, file.path(ems_dir, paste0(geography, "_", ems, "_thresholds.csv")))
  
  h_thresholdDat <- do.call(rbind.data.frame, h_thresholdDat)
  write.csv(h_thresholdDat, file.path(ems_dir, paste0(geography, "_", ems, "_lm_thresholds.csv")))
  #
}
