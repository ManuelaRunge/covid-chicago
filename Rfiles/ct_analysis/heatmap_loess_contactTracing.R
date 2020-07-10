## --------------------------------------------
## Generate heatmap and supplementary plots per EMS or aggregated region
### Using loess regression per EMS and grpvar
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

  capacity <- load_capacity(selected_ems)
  capacity$deaths <- 0

  tempdat <- getdata(trajectoriesDat, selected_ems)
  tempdat$region <- ems

  capacityline <- as.numeric(capacity[colnames(capacity) == selected_outcome])

  ## Genrate outcome variable (renamed from selected outcome)
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

  ### Identify peak at least 10 days after reopening
  peakTimes <- plotdat %>%
    filter(Date > min(Date) + 10) %>%
    dplyr::group_by(N, Ki, region, isolation_success, detection_success, grpvar, scen_num, sample_num, run_num) %>%
    dplyr::filter(value == max(value)) %>%
    dplyr::rename("Date_peak" = Date) %>%
    dplyr::select(N, Ki, region, Date_peak, outcome, isolation_success, detection_success, grpvar, scen_num, sample_num, run_num)

  summary(peakTimes$Date_peak)

  ## Add peak date to plotdat
  plotdat <- plotdat %>%
    left_join(peakTimes, by = c("N", "Ki", "region", "outcome", "scen_num", "sample_num", "run_num", "isolation_success", "detection_success", "grpvar"))

  ### Scatter plot  of outcome variable per CT parameter at peak
  showScatter <- TRUE
  if (showScatter) {
    scatterplot <- plotdat %>%
      filter(Date == Date_peak) %>%
      mutate(
        capacity = capacityline,
        belowCapacity = ifelse(value <= capacity, "yes", "no")
      ) %>%
      ggplot() +
      theme_minimal() +
      geom_point(aes(x = detection_success, y = isolation_success, fill = value, group = scen_num), size = 3, shape = 21) +
      # geom_jitter(aes(x = detection_success, y = isolation_success, fill = value, group = scen_num), size = 3, shape = 21, width = 0.025, height = 0.025) +
      scale_fill_viridis(option = "C", discrete = FALSE, direction = -1) +
      customThemeNoFacet

    ggsave(paste0(ems, "_scatterplot.pdf"),
      plot = scatterplot, path = file.path(ems_dir), width = 5, height = 4, device = "pdf"
    )
  }
  # plotdat %>% filter(Date == plotdat$Date_peak) %>% write.csv(file.path(ems_dir,paste0(ems, "_scatterplot_dat.csv")), row.names = FALSE)

  dat <- plotdat %>% filter(Date == plotdat$Date_peak)
  capacity <- capacityline

  fitlist <- list()
  for (grp in unique(dat$grpvar)) {
    # grp  <- unique(dat$grpvar)[1]
    #### Do loess regression
    detection_success <- seq(0, 1, 0.001)
    isolation_success <- seq(0, 1, 0.001)
    t_matdat <- expand.grid(detection_success = detection_success, isolation_success = isolation_success)

    m <- loess(value ~ detection_success * isolation_success,
      span = 0.8,
      degree = 2, data = subset(dat, grpvar == grp)
    )

    temp_fit_mat <- predict(m, t_matdat)
    temp_fit <- melt(temp_fit_mat)
    temp_fit$detection_success <- gsub("detection_success=", "", temp_fit$detection_success)
    temp_fit$isolation_success <- gsub("isolation_success=", "", temp_fit$isolation_success)


    # library(plotly)
    # fig <- plot_ly(
    #   x = temp_fit$detection_success,
    #   y = temp_fit$isolation_success,
    #   z =  temp_fit$value, ,
    #   type = "contour"
    # )

    temp_fit$value_fct <- NA
    temp_fit$value_fct[temp_fit$value >= 400] <- ">400"
    temp_fit$value_fct[temp_fit$value < 400] <- "<400"
    temp_fit$value_fct[temp_fit$value < 300] <- "<300"
    temp_fit$value_fct[temp_fit$value < 200] <- "<200"
    temp_fit$value_fct[temp_fit$value < 100] <- "<100"
    temp_fit$value_fct[temp_fit$value < 50] <- "<50"

    table(temp_fit$value_fct, exclude = NULL)

    temp_fit$value_fct <- factor(temp_fit$value_fct,
      levels = c(">400", "<400", "<300", "<200", "<100", "<50"),
      labels = c(">400", "<400", "<300", "<200", "<100", "<50")
    )

    temp_fit$detection_success <- as.numeric(temp_fit$detection_success)
    temp_fit$isolation_success <- as.numeric(temp_fit$isolation_success)

    temp_fit$grpvar <- grp
    fitlist[[length(fitlist) + 1]] <- temp_fit

    rm(temp_fit_mat, temp_fit)
  }


  dtfit <- bind_rows(fitlist)
  rm(fitlist)


  dat$value_fct <- NA
  dat$value_fct[dat$value >= 400] <- ">400"
  dat$value_fct[dat$value < 400] <- "<400"
  dat$value_fct[dat$value < 300] <- "<300"
  dat$value_fct[dat$value < 200] <- "<200"
  dat$value_fct[dat$value < 100] <- "<100"
  dat$value_fct[dat$value < 50] <- "<50"

  table(dat$value_fct, exclude = NULL)

  dat$value_fct <- factor(dat$value_fct,
    levels = c(">400", "<400", "<300", "<200", "<100", "<50"),
    labels = c(">400", "<400", "<300", "<200", "<100", "<50")
  )

  ### Extract  minimum isolation_success for each detection_success
  thresholdDat <- dtfit %>%
    filter(value <= capacity) %>%
    group_by(detection_success, grpvar) %>%
    filter(isolation_success == min(isolation_success)) %>%
    mutate(region = ems)

  ### Plot contour-heatmap plot
  p1 <- ggplot(data = subset(dtfit, !is.na(value_fct)), aes(x = detection_success, y = isolation_success)) +
    theme_minimal() +
    geom_tile(aes(fill = value_fct), alpha = 0.8) +
    # geom_point(data=subset(dat, grpvar==0.17),  aes(x = detection_success, y = isolation_success, fill = value_fct, group = scen_num), size = 3, shape = 21, show.legend = FALSE) +
    geom_line(data = subset(thresholdDat, isolation_success != min(isolation_success)), aes(x = detection_success, y = isolation_success), size = 1.3) +
    scale_fill_viridis(option = "C", discrete = TRUE) +
    labs(
      x = "detections (%)",
      y = "isolation success (%)",
      col = "",
      fill = "Critical",
      shape = "",
      linetype = ""
    ) +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    facet_wrap(~grpvar) +
    customThemeNoFacet +
    theme(panel.spacing = unit(1.5, "lines"))

  p2 <- p1 + geom_point(data = dat, aes(x = detection_success, y = isolation_success, fill = value_fct), shape = 21, size = 3, show.legend = FALSE)

  plotname1 <- paste0("EMS", "_", ems, "_critical_heatmap_loess")

  ggsave(paste0(plotname1, ".png"),
    plot = p2, path = file.path(ems_dir), width = 12, height = 5, device = "png"
  )

  ggsave(paste0(plotname1, ".pdf"),
    plot = p1, path = file.path(ems_dir), width = 12, height = 5, device = "pdf"
  )

  write.csv(thresholdDat, file = file.path(ems_dir, paste0(ems, "_loess_thresholds.csv")), row.names = FALSE)
}



### Combine all EMS thresholds into one file
thresholdsfiles <- list.files(file.path(ems_dir), pattern = "loess_thresholds", recursive = TRUE, full.names = TRUE)
#lmthresholdsDat <- sapply(thresholdsfiles, read.csv, simplify = FALSE) %>%
#  bind_rows(.id = "id")
datlist <- list()
for(i in c(1:length(thresholdsfiles))){
 temp <-  read.csv(thresholdsfiles[i])
 if(dim(temp)[1]<1)next
 temp$id <- thresholdsfiles[i]
 datlist[[length(datlist)+1]] <- temp 

}
lmthresholdsDat <- datlist %>% bind_rows()
if (dim(lmthresholdsDat)[1] <= 1) next
colnames(lmthresholdsDat)[colnames(lmthresholdsDat)=="regin"] <- "region"
lmthresholdsDat$id <- gsub(ems_dir, "", lmthresholdsDat$id)
lmthresholdsDat$region <- gsub("_loess_thresholds.csv", "", lmthresholdsDat$id)
lmthresholdsDat$region <- gsub("[/]", "", lmthresholdsDat$region)

write.csv(lmthresholdsDat, file.path(exp_dir, "thresholds_loess.csv"), row.names = FALSE)
