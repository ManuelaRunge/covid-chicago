## ==================================================
# R script that analysis trajectoriesDat
## ==================================================

require(tidyverse)
require(cowplot)
require(scales)
require(lattice)
require(readxl)
require(viridis)
require(stringr)
require(ggrepel)
require(broom)

source("load_paths.R")
source("processing_helpers.R")


ct_dir <- file.path(simulation_output, "contact_tracing")

# Define experiment iteration and simdate
simdate <- "20200609_AsPSym"

nexps <- list.files(file.path(ct_dir, simdate))
exp_name <- simdate
exp_dir <- file.path(ct_dir, simdate)


## --------------------------------------------
## Define functions
## --------------------------------------------

reopeningdate <- as.Date("2020-06-01")
evaluation_window <- c(reopeningdate, reopeningdate + 60)

detectionVar <- "d_Sym_ct1"  # "d_As_ct1"
isolationVar <- "reduced_inf_of_det_cases_ct1"
  

# getdata
getdata <- function(selected_ems) {
  #'  getdata is per default using the "trajectoriesDat" dataset to perfom specific data editing
  #'  Outcome variables of interest are selected for one or more EMS regions as specified in selected_ems
  #'  If multiple integers are included in the selected_ems, the data will be aggregated
  #'  The data is filtered by date to include only dates within the defined evaluation window
  #'  Factor variables per contact tracing parameter are generated to
  #'  optionally facilitate plotting or custom tables for data exploration
  #' @param selected_ems  vector of integers representing one or multiple EMS areas


  # emsvars <- colnames(trajectoriesDat)[grep("[.]",colnames(trajectoriesDat))]
  emsvars_temp <- c(
    "N_EMS_", "Ki_EMS_", "susceptible_EMS.", "exposed_EMS.", "asymptomatic_EMS.", "symptomatic_mild_EMS.", "symptomatic_severe_EMS.",
    "hospitalized_EMS.", "critical_EMS.", "deaths_EMS.", "recovered_EMS.", "asymp_cumul_EMS.",
    "asymp_det_cumul_EMS.", "symp_mild_cumul_EMS.", "symp_severe_cumul_EMS.", "hosp_cumul_EMS.", "hosp_det_cumul_EMS.",
    "crit_cumul_EMS.", "crit_det_cumul_EMS.", "crit_det_EMS.", "death_det_cumul_EMS.", "infected_EMS.",
    "infected_cumul_EMS.", "symp_mild_det_cumul_EMS.", "symp_severe_det_cumul_EMS.", "detected_EMS.", "detected_cumul_EMS.",
    "presymptomatic_EMS."
  )

  emsvars <- NULL
  for (ems in selected_ems) {
    emsvars <- c(emsvars, paste0(emsvars_temp, ems))
  }

  groupvars <- c("time", "startdate", "scen_num", "sample_num", "run_num", "time_to_detection", "detection_success", "isolation_success")
  (keepvars <- c(groupvars, emsvars))

  subdat <- trajectoriesDat %>% dplyr::select(keepvars)

  subdat <- subdat %>%
    pivot_longer(cols = -c(groupvars)) %>%
    mutate(
      startdate = as.Date(startdate),
      Date = as.Date(time + startdate),
      name = gsub("All", "EMS.IL", name)
    ) %>%
    dplyr::mutate(name = gsub("[.]", "_", name)) %>%
    separate(name, into = c("outcome", "region"), sep = "_EMS_") %>%
    dplyr::filter(Date >= evaluation_window[1]) %>%
    select(-c(time))

  subdat1 <- subdat %>%
    filter(outcome %in% c("N", "Ki")) %>%
    pivot_wider(names_from = outcome, values_from = value)
  subdat <- subdat %>%
    filter(!(outcome %in% c("N", "Ki"))) %>%
    left_join(subdat1, by = c(
      "startdate", "scen_num", "sample_num", "run_num", "time_to_detection",
      "detection_success",  "region", "Date", "isolation_success"
    ))

  if (length(selected_ems > 1)) {
    groupvars <- c(groupvars[groupvars != "time"], "outcome", "Date", "isolation_success")

    subdat <- subdat %>%
      dplyr::group_by_at(.vars = groupvars) %>%
      dplyr::summarize(
        value = sum(value),
        N = sum(N),
        Ki = mean(Ki)
      )
  }


  return(subdat)
}

# f_heatmap
f_heatmap <- function(df, selected_outcome, scalePop = F) {
  #'  f_heatmap  performs a linear regression between contact tracing parameter and draws a heatmap using the model predicted values
  #' The function returns a list of the heatmap plot, and the thresholds used to draw the lines, as well as the filtered lm prediction dataset
  #' @param df  dataset
  #' @param selected_outcome  name of the outcome variable
  #' @param valuetype  default "absolute", another option would be "growthRate"  - currently disabled


  dfLM <- df %>%
    rename(
      x = detection_success,
      y = isolation_success,
      z = value
    ) %>%
    group_by(N, time_to_detection, outcome) %>%
    do(fitlm = lm(z ~ y + x, data = .))

  (dfLMCoef <- tidy(dfLM, fitlm))
  # augment(dfLM, fitlm)
  # glance(dfLM, fitlm)

  ## Parameter combinations that did run
  sink(file.path(exp_dir, paste0(geography, "_", ems, "_", selected_outcome, "_linear_models.txt")))
  cat("\ntidy(dfLM, fitlm)")
  print(tidy(dfLM, fitlm))
  cat("\naugment(dfLM, fitlm)")
  print(augment(dfLM, fitlm))
  cat("\nglance(dfLM, fitlm)")
  print(glance(dfLM, fitlm))
  sink()

  ### Generate prediction dataset
  xnew <- seq(0, 1, 0.01)
  ynew <- seq(0, 1, 0.01)

  matdat_list <- list()
  for (testDelay in unique(dfLM$time_to_detection)) {
    lmmodel <- dfLM %>% filter(time_to_detection == testDelay)

    t_matdat <- expand.grid(x = xnew, y = ynew)
    t_matdat <- as.data.frame(cbind(t_matdat, predict(lmmodel$fitlm[[1]], newdata = t_matdat, interval = "confidence")))
    t_matdat$time_to_detection <- testDelay
    t_matdat$N <- unique(dfLM$N)

    matdat_list[[length(matdat_list) + 1]] <- t_matdat
  }

  matdat <- matdat_list %>% bind_rows()
  # table(matdat$time_to_detection)

  ### Load capacityDat
  ems <- unique(df$region)

  if (ems %in% names(regions)) ems <- as.numeric(regions[[ems]])

  if (!selected_outcome %in% names(capacity)) threshold <- round(summary(matdat$z)["1st Qu."], 0)
  if (selected_outcome %in% names(capacity)) threshold <- round(as.numeric(capacity[selected_outcome]), 0)


  tdat <- matdat %>%
    pivot_longer(cols = -c(x, y, time_to_detection), names_to = "statistic") %>%
    filter(value <= threshold) %>%
    dplyr::group_by(time_to_detection, x, statistic) %>%
    dplyr::summarize(ythreshold = min(y))

  matdat <- left_join(matdat, tdat, by = c("time_to_detection", "x"))

  flabel <- paste0("Predicted ", selected_outcome)

  matdat$time_to_detection <- round(matdat$time_to_detection, 0)
  df$time_to_detection <- round(df$time_to_detection, 0)

  if (scalePop) {
    matdat$fit <- matdat$fit / matdat$N * 100000
    matdat$lwr <- matdat$lwr / matdat$N * 100000
    matdat$upr <- matdat$upr / matdat$N * 100000

    df$value <- df$value / df$N * 100000

    flabel <- paste0(flabel, "\n per 100'000")
  }
  # https://stackoverflow.com/questions/14290364/heatmap-with-values-ggplot2
  matdatp <- ggplot(data = matdat, aes(x = x, y = y)) +
    geom_tile(aes(fill = fit), alpha = 0.9) +
    geom_smooth(
      data = subset(matdat, statistic == "fit"),
      aes(x = x, y = ythreshold, group = statistic), method = "lm", col = "black", size = 0.75
    ) +
    geom_smooth(
      data = subset(matdat, statistic != "fit"),
      aes(x = x, y = ythreshold, group = statistic), method = "lm", col = "black", size = 0.75, linetype = "dashed"
    ) +
    geom_jitter(data = df, aes(x = detection_success, y = isolation_success, fill = value), shape = 21, size = 2.5) +
    # scale_fill_gradient2(low = "olivedrab3",mid='#f1a340',  high = "mediumvioletred", midpoint= 400) +
    scale_fill_viridis(option = "C", discrete = FALSE, direction = -1) +
    labs(
      title = paste0(geography, " ", ems, "\n"),
      x = "detections (P, As, Sym) (%)",
      y = "isolation success (%)",
      fill = flabel,
      col = "",
      shape = "",
      linetype = "",
      subtitle = paste0("Capacity limit: ", threshold)
    ) +
    customThemeNoFacet +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    facet_wrap(~time_to_detection, labeller = labeller(time_to_detection = label_both)) +
    theme(panel.spacing = unit(2, "lines"))


  tdat_wide <- tdat %>% pivot_wider(names_from = "statistic", values_from = "ythreshold")

  tdat_wide %>%
    dplyr::filter(!is.na(fit)) %>%
    group_by(time_to_detection, x) %>%
    summarize(
      xmin = min(x), xmax = max(x),
      lwrmin = min(lwr), lwrmax = max(lwr)
    )


  xpol <- c(tdat_wide$x, rev(tdat_wide$x))
  ypol <- c(tdat_wide$lwr, rev(tdat_wide$upr))
  time_to_detection <- c(tdat_wide$time_to_detection, rev(tdat_wide$time_to_detection))


  datpol <- as.data.frame(cbind(xpol, ypol, time_to_detection))

  matdatp2 <- ggplot(data = tdat_wide, aes(x = x, y = y)) +
    # geom_smooth(data=subset(matdat, statistic!="fit"),
    #            aes(x = x, y = ythreshold,col=as.factor(time_to_detection), group=interaction(time_to_detection, statistic)),
    #            method="lm", size=0.75, linetype="dashed", show.legend = FALSE) +
    theme_cowplot() +
    # geom_ribbon(data=subset(tdat_wide),   aes(x = x, y = fit, ymin = lwr, ymax = upr, fill= as.factor(time_to_detection),
    #                                           group=time_to_detection),alpha=0.5) +
    geom_polygon(data = datpol, aes(x = xpol, y = ypol, fill = as.factor(time_to_detection)), alpha = 0.5) +
    geom_smooth(
      data = subset(tdat_wide),
      aes(x = x, y = fit, col = as.factor(time_to_detection), group = time_to_detection),
      method = "lm", size = 1.3, show.legend = FALSE
    ) +
    # geom_jitter(data=df, aes(x=detection_success , y = isolation_success,fill=value),shape=21, size = 2.5) +
    # scale_fill_gradient2(low = "olivedrab3",mid='#f1a340',  high = "mediumvioletred", midpoint= 400) +
    scale_fill_viridis(option = "C", discrete = TRUE, direction = -1) +
    scale_color_viridis(option = "C", discrete = TRUE, direction = -1) +
    labs(
      title = paste0(geography, " ", ems, "\n"),
      x = "detections (P, As, Sym) (%)",
      y = "isolation success (%)",
      fill = "Test delay",
      col = "",
      shape = "",
      linetype = "",
      subtitle = paste0("Capacity limit: ", threshold)
    ) +
    customThemeNoFacet +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    theme(panel.spacing = unit(2, "lines"))


  out_list <- list(matdatp, tdat, matdatp2)

  return(out_list)
}

# region_to_fct
region_to_fct <- function(dat, geography) {
  #' Helper function to convert region character variable to factor variable
  #' @param dat  dataset
  #' @param geography  level of analysis EMS or Region (aggregated or not)


  if (geography == "EMS") {
    levels <- c(1:11)
    labels <- paste0("EMS ", levels)
  }

  if (geography == "Region") {
    levels <- c(names(regions)[5], names(regions)[-5])
    labels <- stringr::str_to_title(levels)
  }

  dat$region <- factor(dat$region,
    levels = levels,
    labels = labels
  )
  return(dat)
}

### Load and subset files to save memory
nexpsfiles <- list.files(file.path(ct_dir, simdate), pattern = "trajectoriesDat.csv", recursive = TRUE, full.names = TRUE)
trajectoriesDat <- sapply(nexpsfiles, read.csv, simplify = FALSE) %>%
  bind_rows(.id = "id")

### Discard time entries before reopening date
trajectoriesDat <- subset(trajectoriesDat, time >= as.Date(reopeningdate) - as.Date(max(trajectoriesDat$startdate)))

trajectoriesDat$detection_success <- trajectoriesDat[, colnames(trajectoriesDat)==detectionVar]
trajectoriesDat$isolation_success <- 1-(trajectoriesDat[, colnames(trajectoriesDat)==isolationVar])


## Parameter combinations that did run
sink(file.path(exp_dir, "parameter_combinations.txt"))
cat("\ntable(trajectoriesDat$isolation_success, trajectoriesDat$time_to_detection)")
table(trajectoriesDat$isolation_success, trajectoriesDat$time_to_detection)
cat("\ntable(trajectoriesDat$detection_success, trajectoriesDat$time_to_detection)")
table(trajectoriesDat$detection_success, trajectoriesDat$time_to_detection)
sink()


pplot <- ggplot(data=trajectoriesDat)+ theme_bw() + 
  geom_point(aes(x=detection_success, y=isolation_success, col=as.factor(round(time_to_detection,0))),size=2)
ggsave(paste0("sample_points.png"),
       plot = pplot, path = file.path(ct_dir, simdate), width = 8, height = 6,  device = "png"
)
## --------------------------------------------
## Generate heatmap and supplementary plots per EMS or aggregated region
## --------------------------------------------
geography <- "EMS"
# geography <- "Region"
if (geography == "EMS") emsregions <- c(1:11)
if (geography == "Region") emsregions <- names(regions)

for (ems in emsregions) {
  # ems <- emsregions[1]
  ems_dir <- file.path(exp_dir, paste0(geography, "_", ems))
  if (!dir.exists(ems_dir)) dir.create(ems_dir)

  if (geography == "Region") {
    selected_ems <- as.numeric(regions[[ems]])
  } else {
    selected_ems <- ems
  }

  capacity <- load_capacity(selected_ems)
  capacity$deaths <- 0


  tempdat <- getdata(selected_ems)
  tempdat$region <- ems
  write.csv(tempdat, file.path(ems_dir, paste0(geography, "_", ems, "_dat.csv")))

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

      threshold_param <- plotdat %>%
        filter(Date == max(Date)) %>%
        mutate(
          capacity = capacityline
        ) %>%
        filter(value <= capacity) %>%
        group_by(detection_success, time_to_detection, scen_num, sample_num, run_num) %>%
        filter(isolation_success == min(isolation_success)) %>%
        dplyr::select(region, detection_success, isolation_success, time_to_detection, outcome, scen_num, sample_num, run_num, value, capacity)
    } else {
      plotdat <- tempdat %>%
        filter(outcome == selected_outcome) %>%
        mutate(region = ems) %>%
        as.data.frame()

      peakTimes <- plotdat %>%
        group_by(N, Ki, region, isolation_success, detection_success, time_to_detection, scen_num, sample_num, run_num) %>%
        filter(value == max(value)) %>%
        rename(Date_peak = Date) %>%
        select(N, Ki, region, Date_peak, outcome, isolation_success, detection_success, time_to_detection, scen_num, sample_num, run_num)

      ## Add peak date to plotdat
      plotdat <- plotdat %>%
        left_join(peakTimes, by = c("N", "Ki", "region", "outcome", "scen_num", "sample_num", "run_num", "isolation_success", "detection_success", "time_to_detection"))

      ## Filter dataset to get threshold values
      threshold_param <- plotdat %>%
        filter(Date == Date_peak) %>%
        mutate(capacity = capacityline) %>%
        filter(outcome == selected_outcome) %>%
        filter(value <= capacity) %>%
        group_by(region, detection_success, time_to_detection, scen_num, sample_num, run_num) %>%
        filter(isolation_success == min(isolation_success)) %>%
        dplyr::select(region, N, Ki, Date_peak, scen_num, sample_num, run_num, detection_success, isolation_success, time_to_detection, outcome, value, capacity)
    }

    thresholdDat[[selected_outcome]] <- threshold_param


    l_plot <- ggplot(data = subset(plotdat)) + theme_cowplot() +
      # geom_vline(xintercept=as.Date("2020-08-02"), linetype="dashed")+
      geom_line(aes(x = Date, y = value,
                      group = interaction(scen_num)), size = 1.3, col = "deepskyblue4") +
      facet_wrap(~round(time_to_detection,0), labeller = labeller(time_to_detection = label_both)) +
      labs(
        title = paste0(geography, " ", unique(plotdat$region)),
        caption = "each lines represents an unique combinations of isolation success and detection rate",
        subtitle = "test delay (days)", y = selected_outcome
      )

    ggsave(paste0(geography, "_", ems, "_", selected_outcome, "_line.png"),
           plot = l_plot, path = file.path(ems_dir), width = 8, height = 6, dpi = 300, device = "png"
    )



        # Aggregate dataset for plot
        plotdatAggr = plotdat %>%
          group_by(Date, region, outcome, isolation_success, time_to_detection, detection_success)  %>%
          summarize(Date_peak=mean(Date_peak),
                    value.mean = mean(value, na.rm=T),
                    value.min = min(value, na.rm=T),
                    value.max = max(value, na.rm=T))

        p_plot <- ggplot(data = subset(plotdatAggr, Date ==  plotdatAggr$Date_peak)) +
          theme_cowplot() +
          geom_pointrange(aes(x = detection_success, y = value.mean,
                              ymin = value.min ,  ymax = value.max,  col = isolation_success, shape = as.factor(time_to_detection)),
                         size =0.7) +
           labs(
            title = paste0(geography, " ", unique(plotdat$region)),
            subtitle = paste0("Time point: ",  evaluation_window[2],
                              "\nContact tracing start: ", evaluation_window[1]),
            shape = "test delay (days)",
            x = "detection rate (P, As, Sym)",
            y = selected_outcome
          ) +
          scale_color_viridis(option = "D", discrete = FALSE, direction = -1) +
          geom_hline(yintercept = capacityline, linetype = "dashed")

        ggsave(paste0(geography, "_", ems, "_", selected_outcome, "_scatter.png"),
               plot = p_plot, path = file.path(ems_dir), width = 8, height = 6, dpi = 300, device = "png"
        )


        ### Line plot filtered with threshold value and heatmap
        if(dim(threshold_param)[1]==0) next
        temp <- threshold_param %>%
          mutate(threshold=1) %>%
          dplyr::select(-c(outcome, value, capacity ))

        testdat <- plotdat %>%
          left_join(temp, by=c("region","Date_peak", "scen_num", "sample_num", "run_num", "time_to_detection", "detection_success","isolation_success")) %>%
          dplyr::filter(threshold == 1 )
         # group_by(time_to_detection) %>%
          #filter(detection_success <= 0.556)
        testdatAggr   <-  testdat %>%
          group_by(region,Date,detection_success, isolation_success, time_to_detection) %>%
          summarize(value.min  = min(value), value.max = max(value))

        labelDat = testdat %>%  filter(Date==max(Date)) %>%
          group_by(region,Date,isolation_success,detection_success,  time_to_detection) %>%
          summarize(value = mean(value), detection_success  = round(min(detection_success),2))

        # ## use smooth lines ?
        # l_plot2 <- ggplot(data= testdat ) + theme_cowplot() +
        #    # geom_ribbon(data=testdatAggr , aes(x = Date, ymin = value.min, ymax = value.max,
        #    #                 fill=as.factor(round(isolation_success,2)),
        #    #                 group=interaction(isolation_success, detection_success)), size = 1,alpha=0.7) +
        #   # geom_point(data = subset(testdat),
        #   #            aes(x = Date, y = value,
        #   #                  group=interaction(isolation_success, detection_success)), col="grey", shape=21, size = 1,alpha=0.7) +
        #   geom_smooth(aes(x = Date, y = value,
        #                 col=as.factor(round(isolation_success,2)),
        #                 fill=as.factor(round(isolation_success,2)),
        #                 group=interaction(isolation_success, detection_success)), size = 1.3) +
        #   geom_label_repel(data =labelDat, aes(x = Date, y = value,label=detection_success,
        #                   col=as.factor(round(isolation_success,2)),
        #                   group=interaction(isolation_success, detection_success)),
        #                   size = 3,nudge_x=3,nudge_y=10, show.legend = FALSE) +
        #   geom_hline(yintercept = capacityline, linetype = "dashed") +
        #   labs(color="isolation success",
        #        fill="isolation success",
        #        linetype="test delay",
        #     title = paste0(geography, " ", unique(plotdat$region)),
        #     subtitle = paste0("Minimum detection rate ", round( unique(testdat$detection_success),2)),
        #     caption="line labels = detection rate",
        #     y = selected_outcome
        #   )+
        #   customThemeNoFacet+
        #   theme(axis.text.x  = element_text(size = 12, angle=60, hjust=1, vjust=1)) +
        #   scale_color_viridis(option = "D", discrete = TRUE, direction = -1)+
        #   scale_fill_viridis(option = "D", discrete = TRUE, direction = -1) +
        #   facet_wrap(~round(time_to_detection,0), labeller = labeller(time_to_detection = label_both))
        # 
        # 
        # ggsave(paste0(geography, "_", ems, "_", selected_outcome, "_line_belowCapacity.png"),
        #        plot = l_plot2, path = file.path(ems_dir), width = 8, height = 5, dpi = 300, device = "png"
        # )

    heatmap_out <- f_heatmap(df = subset(plotdat, Date == plotdat$Date_peak), selected_outcome, scalePop = TRUE)

    h_plot <- heatmap_out[[1]]
    h_threshold <- heatmap_out[[2]]
    h_plot_new <- heatmap_out[[3]]

    # ggsave(paste0(geography, "_", ems, "_", selected_outcome, "_heatmap.png"),
    #   plot = h_plot, path = file.path(ems_dir), width = 16, height = 6, dpi = 300, device = "png"
    # )
    ggsave(paste0(geography, "_", ems, "_", selected_outcome, "_heatmap_scl.pdf"),
      plot = h_plot, path = file.path(ems_dir), width = 16, height = 6, dpi = 300, device = "pdf"
    )
    ggsave(paste0(geography, "_", ems, "_", selected_outcome, "_heatmap_scl_v2.pdf"),
      plot = h_plot_new, path = file.path(ems_dir), width = 6, height = 6, dpi = 300, device = "pdf"
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

  # thresholdDat <- do.call(rbind.data.frame, thresholdDat)
  # write.csv(thresholdDat, file.path(ems_dir, paste0(geography, "_", ems, "_thresholds.csv")))
  # h_thresholdDat <- do.call(rbind.data.frame, h_thresholdDat)
  # write.csv(h_thresholdDat, file.path(ems_dir, paste0(geography, "_", ems, "_lm_thresholds.csv")))
  #
}

## --------------------------------------------
#### Summary plot of thresholds per region
## --------------------------------------------
thresholdplot <- F
if (thresholdplot) {

  ### Load thresholds from lm model
  thresholdsfiles <- list.files(file.path(exp_dir), pattern = "thresholds", recursive = TRUE, full.names = TRUE)
  thresholdsfiles <- thresholdsfiles[grep(geography, thresholdsfiles)]
  thresholdsfiles <- thresholdsfiles[grep("lm", thresholdsfiles)]

  lmthresholdsDat <- sapply(thresholdsfiles, read.csv, simplify = FALSE) %>%
    bind_rows(.id = "id") %>%
    dplyr::select(-X) %>%
    dplyr::mutate(
      region = as.character(region),
      outcome = as.character(outcome)
    ) %>%
    dplyr::rename(detection_success = x, isolation_success = ythreshold)

  lmthresholdsDat <- region_to_fct(lmthresholdsDat, geography)

  ### Load thresholds from predictions
  thresholdsfiles <- list.files(file.path(exp_dir), pattern = "thresholds", recursive = TRUE, full.names = TRUE)
  thresholdsfiles <- thresholdsfiles[grep(geography, thresholdsfiles)]
  thresholdsfiles <- thresholdsfiles[!grepl("lm", thresholdsfiles)]

  thresholdsDat <- sapply(thresholdsfiles, read.csv, simplify = FALSE) %>%
    bind_rows(.id = "id") %>%
    dplyr::select(-X) %>%
    mutate(
      region = as.character(region),
      outcome = as.character(outcome),
      outcome = factor(outcome,
        levels = c("hospitalized", "critical", "ventilators", "deaths"),
        labels = c("hospitalized", "critical", "ventilators", "deaths")
      ),
      capacityLabel = paste0("Capacity: ", capacity)
    )

  thresholdsDat <- region_to_fct(thresholdsDat, geography)

  table(thresholdsDat$region)
  table(thresholdsDat$region, thresholdsDat$detection_success)

  capacityText <- thresholdsDat %>%
    select(region, outcome, capacityLabel) %>%
    unique()

  if (geography == "Region") {
    pplot <- ggplot(data = subset(thresholdsDat, outcome != "deaths")) +
      theme_bw() +
      geom_jitter(aes(x = detection_success, y = isolation_success, col = as.factor(time_to_detection)), size = 3) +
      geom_text(data = capacityText, aes(x = 0.18, y = 1, label = capacityLabel), col = "black") +
      geom_smooth(aes(x = detection_success, y = isolation_success), se = FALSE, method = "lm", col = "darkgrey", size = 1) +
      geom_smooth(data = subset(lmthresholdsDat, outcome != "deaths"), aes(x = detection_success, y = isolation_success), se = FALSE, method = "lm", col = "black", size = 1) +
      facet_grid(outcome ~ region) +
      labs(
        color = "test delay",
        x = "detection rate",
        y = "isolation success",
        caption = "\n dots from prediction dataset\ngrey line: thresholds from prediction dataset\nblack line: thresholds from linear model"
      ) +
      scale_color_brewer(palette = "Dark2") +
      customThemeNoFacet +
      theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      ) +
      scale_x_continuous(labels = seq(0, 1, 0.2), breaks = seq(0, 1, 0.2)) +
      scale_y_continuous(labels = seq(0, 1, 0.2), breaks = seq(0, 1, 0.2))


    ggsave(paste0(geography, "_capacity_thresholds.png"),
      plot = pplot, path = file.path(exp_dir), width = 16, height = 9, dpi = 300, device = "png"
    )
  }
  if (geography == "EMS") {
    for (ems in unique(thresholdsDat$region)) {
      pplot <- ggplot(data = subset(thresholdsDat, region %in% ems & outcome != "deaths")) +
        theme_bw() +
        geom_jitter(aes(x = detection_success, y = isolation_success, col = as.factor(time_to_detection)), size = 3) +
        geom_text(data = subset(capacityText, region %in% ems), aes(x = 0.18, y = 1, label = capacityLabel), col = "black") +
        geom_smooth(aes(x = detection_success, y = isolation_success), se = FALSE, method = "lm", col = "darkgrey", size = 1) +
        geom_smooth(data = subset(lmthresholdsDat, region %in% ems & outcome != "deaths"), aes(x = detection_success, y = isolation_success), se = FALSE, method = "lm", col = "black", size = 1) +
        facet_grid(outcome ~ region) +
        labs(
          color = "test delay",
          x = "detection rate",
          y = "isolation success",
          caption = "\n dots from prediction dataset\ngrey line: thresholds from prediction dataset\nblack line: thresholds from linear model"
        ) +
        scale_color_brewer(palette = "Dark2") +
        customThemeNoFacet +
        theme(
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()
        ) +
        scale_x_continuous(labels = seq(0, 1, 0.2), breaks = seq(0, 1, 0.2)) +
        scale_y_continuous(labels = seq(0, 1, 0.2), breaks = seq(0, 1, 0.2))


      ggsave(paste0(ems, "_capacity_thresholds.png"),
        plot = pplot, path = file.path(exp_dir), width = 6, height = 10, dpi = 300, device = "png"
      )
    }
  }

  thresholdsDat$method <- "simulations"
  lmthresholdsDat$method <- "linear_model"

  thresholdsDat %>%
    dplyr::select(colnames(lmthresholdsDat)) %>%
    rbind(lmthresholdsDat) %>%
    dplyr::select(-id) %>%
    pivot_wider(names_from = method, values_from = isolation_success) %>%
    write.csv(file.path(exp_dir, paste0(geography, "_Thresholds.csv")))
}

### Visualize time and size of peak

## presentation-wise, I really like the heatmaps you’ve made. I’d like to see all 3
## (3 testDelay values) side by side for each EMS, and maybe all EMS together (so an 11x3 big plot).
## If you do this, the 3 testdelay plots for each EMS should definitely be on the same color scale.
## across EMS, maybe better to let colorscale change, OR do it all per 100,000 population and standardize the scale.


customThemeNoFacet <- theme(
  strip.text.x = element_text(size = 12, face = "bold"),
  strip.text.y = element_text(size = 12, face = "bold"),
  strip.background = element_blank(),
  plot.title = element_text(size = 16, vjust = -1, hjust = 0),
  plot.subtitle = element_text(size = 12),
  plot.caption = element_text(size = 8),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 12),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 12)
)


allEMSatOnce <- FALSE
if (allEMSatOnce) {
  # read.csv(tempdat, file.path(ems_dir, paste0(geography, "_", ems, "_dat.csv")))

  selected_ems <- c(1:11)
  emsvars_temp <- c(
    "susceptible_EMS.", "exposed_EMS.", "asymptomatic_EMS.", "symptomatic_mild_EMS.", "symptomatic_severe_EMS.",
    "hospitalized_EMS.", "critical_EMS.", "deaths_EMS.", "recovered_EMS.", "asymp_cumul_EMS.",
    "asymp_det_cumul_EMS.", "symp_mild_cumul_EMS.", "symp_severe_cumul_EMS.", "hosp_cumul_EMS.", "hosp_det_cumul_EMS.",
    "crit_cumul_EMS.", "crit_det_cumul_EMS.", "crit_det_EMS.", "death_det_cumul_EMS.", "infected_EMS.",
    "infected_cumul_EMS.", "symp_mild_det_cumul_EMS.", "symp_severe_det_cumul_EMS.", "detected_EMS.", "detected_cumul_EMS.",
    "presymptomatic_EMS."
  )

  emsvars <- NULL
  for (ems in selected_ems) {
    emsvars <- c(emsvars, paste0(emsvars_temp, ems))
  }

  groupvars <- c("time", "startdate", "scen_num", "sample_num", "run_num", "time_to_detection", "detection_success", "isolation_success")
  (keepvars <- c(groupvars, emsvars))

  subdat <- trajectoriesDat %>% dplyr::select(keepvars)
  
  subdat <- filter(subdat, time_to_detection > 1)
 # rm(trajectoriesDat)

  subdat <- subdat %>%
    pivot_longer(cols = -c(groupvars)) %>%
    mutate(
      startdate = as.Date(startdate),
      Date = as.Date(time + startdate),
      name = gsub("All", "EMS.IL", name)
    ) %>%
    separate(name, into = c("outcome", "region"), sep = "_EMS[.]") %>%
    dplyr::filter(Date >= evaluation_window[1]) %>%
    select(-c(time))

  selected_outcome <- "critical"
  capacity <- load_capacity("all")
  capacity$region <- as.character(rownames(capacity))
  capacity <- capacity %>%
    rename(capacity = selected_outcome) %>%
    select(region, capacity)


  plotdat <- subdat %>%
    left_join(capacity, by = "region") %>%
    filter(outcome == selected_outcome) %>%
    as.data.frame()

  peakTimes <- plotdat %>%
    group_by(region, capacity, isolation_success, detection_success, time_to_detection, scen_num, sample_num, run_num) %>%
    filter(value == max(value)) %>%
    rename(Date_peak = Date) %>%
    select(region, capacity, Date_peak, outcome, isolation_success, detection_success, time_to_detection, scen_num, sample_num, run_num)



  ## Add peak date to plotdat
  plotdat <- plotdat %>%
    left_join(peakTimes, by = c("region", "capacity", "outcome", "scen_num", "sample_num", "run_num", "isolation_success", "detection_success", "time_to_detection"))

  # rm(peakTimes)


    df <- subset(plotdat, Date == plotdat$Date_peak)
  # rm(plotdat)

  dfLM <- df %>%
    rename(
      x = detection_success,
      y = isolation_success,
      z = value
    ) %>%
    group_by(region, time_to_detection, outcome, capacity) %>%
    do(fitlm = lm(z ~ y + x, data = .))

  (dfLMCoef <- tidy(dfLM, fitlm))
  # augment(dfLM, fitlm)
  # glance(dfLM, fitlm)

  ## Parameter combinations that did run
  sink(file.path(exp_dir, paste0("perEMS_", selected_outcome, "_linear_models.txt")))
  cat("\ntidy(dfLM, fitlm)")
  print(tidy(dfLM, fitlm))
  cat("\naugment(dfLM, fitlm)")
  print(augment(dfLM, fitlm))
  cat("\nglance(dfLM, fitlm)")
  print((d <- glance(dfLM, fitlm)))
  print(summary(d$r.squared))
  print(tapply(d$r.squared, d$region, summary))
  sink()




  ### Generate prediction dataset
  xnew <- seq(0, 1, 0.01)
  ynew <- seq(0, 1, 0.01)

  matdat_list <- list()
  for (region_i in unique(dfLM$region)) {
    for (testDelay in unique(dfLM$time_to_detection)) {
      lmmodel <- dfLM %>% filter(time_to_detection == testDelay & region == region_i)

      t_matdat <- expand.grid(x = xnew, y = ynew)
      t_matdat <- as.data.frame(cbind(t_matdat, predict(lmmodel$fitlm[[1]], newdata = t_matdat, interval = "confidence")))
      t_matdat$time_to_detection <- testDelay
      t_matdat$region <- region_i
      t_matdat$capacity <- lmmodel$capacity

      matdat_list[[length(matdat_list) + 1]] <- t_matdat
    }
  }

  matdat <- matdat_list %>% bind_rows()
  # table(matdat$time_to_detection)

  tdat <- matdat %>%
    pivot_longer(cols = -c(region, capacity, x, y, time_to_detection), names_to = "statistic") %>%
    filter(value <= capacity) %>%
    dplyr::group_by(region, time_to_detection, x, statistic) %>%
    dplyr::summarize(ythreshold = min(y))

  matdat <- left_join(matdat, tdat, by = c("time_to_detection", "x"))

  flabel <- paste0("Predicted ", selected_outcome)

  matdat$time_to_detection <- round(matdat$time_to_detection, 0)
  df$time_to_detection <- round(df$time_to_detection, 0)




  tdat_wide <- tdat %>% pivot_wider(names_from = "statistic", values_from = "ythreshold")

  tdat_wide %>%
    dplyr::filter(!is.na(fit)) %>%
    group_by(region, time_to_detection, x) %>%
    summarize(
      xmin = min(x), xmax = max(x),
      lwrmin = min(lwr), lwrmax = max(lwr)
    )


  xpol <- c(tdat_wide$x, rev(tdat_wide$x))
  ypol <- c(tdat_wide$lwr, rev(tdat_wide$upr))
  time_to_detection <- c(tdat_wide$time_to_detection, rev(tdat_wide$time_to_detection))
  region <- c(tdat_wide$region, rev(tdat_wide$region))


  datpol <- as.data.frame(cbind(xpol, ypol, time_to_detection, region))

  datpol$xpol <- as.numeric(datpol$xpol)
  datpol$ypol <- as.numeric(datpol$ypol)
  datpol$time_to_detection <- as.numeric(datpol$time_to_detection)

  datpol$region_label <- factor(datpol$region, levels = c(1:11), labels = paste0("EMS_", c(1:11), "\n"))
  tdat_wide$region_label <- factor(tdat_wide$region, levels = c(1:11), labels = paste0("EMS_", c(1:11), "\n"))

  datpol$region_label <- factor(datpol$region, levels = c(1:11), labels = paste0("EMS_", c(1:11), "\n"))
  tdat_wide$region_label <- factor(tdat_wide$region, levels = c(1:11), labels = paste0("EMS_", c(1:11), "\n"))


  datpol$time_to_detection <- round(datpol$time_to_detection, 0)
  tdat_wide$time_to_detection <- round(tdat_wide$time_to_detection, 0)

  hlineDat <- tdat_wide %>%
    group_by(region) %>%
    filter(!is.na(lwr) & x == min(x))
  vlineDat <- tdat_wide %>%
    group_by(region) %>%
    filter(!is.na(lwr) & lwr == min(lwr))

  regLabel <- df %>%
    select(region, capacity) %>%
    unique() %>%
    mutate(regLabel = paste0("EMS_", region, "\n limit: ", capacity))



  labs <- c("EMS_1\n limit: 148", "EMS_2\n limit: 181", "EMS_3\n limit: 103", "EMS_4\n limit: 98", "EMS_5\n limit: 88", "EMS_6\n limit: 109",
            "EMS_7\n limit: 404", "EMS_8\n limit: 255", "EMS_9\n limit: 265", "EMS_10\n limit: 150", "EMS_11\n limit: 785")

  datpol$region_label2 <- factor(datpol$region, levels = c(1:11), labels = labs)
  tdat_wide$region_label2 <- factor(tdat_wide$region, levels = c(1:11), labels = labs)
  hlineDat$region_label2 <- factor(hlineDat$region, levels = c(1:11), labels = labs)
  vlineDat$region_label2 <- factor(vlineDat$region, levels = c(1:11), labels = labs)

  matdatp2 <- ggplot(data = tdat_wide, aes(x = x, y = y)) +
    theme_bw() +
    geom_ribbon(data=tdat_wide, aes(x = x, y = fit,  ymin = lwr, ymax = upr, fill = as.factor(time_to_detection), group = time_to_detection)) +
    #geom_polygon(data = datpol, aes(x = xpol, y = ypol, fill = as.factor(time_to_detection)), alpha = 0.5) +
    geom_smooth(
      data = subset(tdat_wide),
      aes(x = x, y = fit, col = as.factor(time_to_detection), group = time_to_detection),
      method = "lm", size = 1.3, show.legend = FALSE
    ) +
    scale_fill_viridis(option = "C", discrete = TRUE, direction = -1) +
    scale_color_viridis(option = "C", discrete = TRUE, direction = -1) +
    customThemeNoFacet +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    theme(panel.spacing = unit(2, "lines")) +
    facet_wrap(~region_label2, scales = "free") +
    labs(
      color = "time to detection",
      subtitle = "",
      fill = "time to detection",
      x = "detections (P, As) (%)",
      y = "isolation success (%)"
    ) +
   # geom_vline(data = hlineDat, aes(xintercept = x), col = "grey") +
   # geom_hline(data = vlineDat, aes(yintercept = lwr), col = "grey") +
    theme(legend.position = "right")


  ggsave(paste0("all_capacity_thresholds.png"),
    plot = matdatp2, path = file.path(exp_dir), width = 15, height = 10, dpi = 300, device = "png"
  )
  ggsave(paste0("all_capacity_thresholds.pdf"),
    plot = matdatp2, path = file.path(exp_dir), width = 15, height = 10, dpi = 300, device = "pdf"
  )


 

  plotdat$region <- factor(plotdat$region, levels = c(1:11), labels = c(1:11))

  plotdatAggr1 <- plotdat %>%
    group_by(region, Date, scen_num) %>%
    summarize(value = mean(value))

  plotdatAggr <- plotdat %>%
    group_by(region, Date) %>%
    summarize(value = mean(value))


  l_plot2 <- ggplot() +
    theme_cowplot() +
    geom_line(data = plotdatAggr1, aes(
      x = Date, y = value,
      group = interaction(scen_num, region)
    ), size = 1, col = "grey", alpha = 0.3) +
    geom_line(data = plotdatAggr, aes(
      x = Date, y = value,
      col = as.factor(region),
      fill = as.factor(region),
      group = region
    ), size = 2) +
    labs(
      color = "EMS",
      fill = "EMS",
      y = "Required ICU beds", x = ""
    ) +
    customThemeNoFacet +
    theme(axis.text.x = element_text(size = 12, angle = 60, hjust = 1, vjust = 1)) +
    scale_color_viridis(option = "D", discrete = TRUE, direction = -1) +
    scale_fill_viridis(option = "D", discrete = TRUE, direction = -1) +
    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B")


  ggsave(paste0(selected_outcome, "_peaks.png"),
    plot = l_plot2, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
  )
}
