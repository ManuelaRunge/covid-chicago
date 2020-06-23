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
simdate <- "20200622"
#simdate <- "20200611_AsP0"


nexps <- list.files(file.path(ct_dir, simdate))
exp_names <- list.dirs(file.path(ct_dir, simdate), full.names = FALSE)[-1]
exp_name  <-exp_names[1]

exp_dir <- file.path(ct_dir, simdate)

## --------------------------------------------
## Define functions
## --------------------------------------------

reopeningdate <- as.Date("2020-06-01")
evaluation_window <- c(reopeningdate, reopeningdate + 60)

detectionVar <- "d_AsP_ct1"  # "d_As_ct1"
isolationVar <- "reduced_inf_of_det_cases_ct1"
timeVar <- "change_testDelay_Sym_1"

nexpsfiles <- list.files(file.path(ct_dir, simdate), pattern = "trajectoriesDat.csv", recursive = TRUE, full.names = TRUE)
trajectoriesDat <- sapply(nexpsfiles, read.csv, simplify = FALSE) %>%
  bind_rows(.id = "id")

### Discard time entries before reopening date
trajectoriesDat <- subset(trajectoriesDat, time >= as.Date(reopeningdate) - as.Date(max(trajectoriesDat$startdate)))

trajectoriesDat$detection_success <- trajectoriesDat[, colnames(trajectoriesDat)==detectionVar]
trajectoriesDat$isolation_success <- 1-(trajectoriesDat[, colnames(trajectoriesDat)==isolationVar])
trajectoriesDat$time_to_detection <- trajectoriesDat[, colnames(trajectoriesDat)==timeVar]


## Parameter combinations that did run
sink(file.path(exp_dir, "parameter_combinations.txt"))
cat("\ntable(trajectoriesDat$isolation_success, trajectoriesDat$time_to_detection)")
table(trajectoriesDat$isolation_success, trajectoriesDat$time_to_detection)
cat("\ntable(trajectoriesDat$detection_success, trajectoriesDat$time_to_detection)")
table(trajectoriesDat$detection_success, trajectoriesDat$time_to_detection)
sink()


pplot <- ggplot(data=trajectoriesDat)+ theme_bw() + 
  geom_point(aes(x=detection_success, y=isolation_success, col=as.factor(round(time_to_detection,0))),size=2, show.legend = FALSE) +
  facet_grid(time_to_detection~ backtonormal_multiplier, labeller = labeller(.rows = label_both, .cols = label_both)) +
  customThemeNoFacet
ggsave(paste0("sample_points.png"),
       plot = pplot, path = file.path(ct_dir, simdate), width = 8, height = 6,  device = "png"
)


allEMSatOnce <- FALSE
if (allEMSatOnce) {
  # read.csv(tempdat, file.path(ems_dir, paste0(geography, "_", ems, "_dat.csv")))
  
  selected_ems <- c(1:11)
  emsvars_temp <- c("critical_EMS."
  )
  
  emsvars <- NULL
  for (ems in selected_ems) {
    emsvars <- c(emsvars, paste0(emsvars_temp, ems))
  }
  
  groupvars <- c("time", "startdate", "scen_num", "sample_num", "run_num", "time_to_detection", "detection_success", "isolation_success")
  (keepvars <- c(groupvars, emsvars))
  
  subdat <- trajectoriesDat %>% dplyr::select(keepvars)
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
  
  
  regLabel <- df %>%
    select(region, capacity) %>%
    unique() %>%
    mutate(regLabel = paste0("EMS_", region, "\n limit: ", capacity))
  
  
  labs <- c("EMS_1\n limit: 148", "EMS_2\n limit: 181", "EMS_3\n limit: 103", "EMS_4\n limit: 98", "EMS_5\n limit: 88", "EMS_6\n limit: 109",
            "EMS_7\n limit: 404", "EMS_8\n limit: 255", "EMS_9\n limit: 265", "EMS_10\n limit: 150", "EMS_11\n limit: 785")
  
  datpol$region_label2 <- factor(datpol$region, levels = c(1:11), labels = labs)
  tdat_wide$region_label2 <- factor(tdat_wide$region, levels = c(1:11), labels = labs)
  
  
  matdatp2 <- ggplot(data = tdat_wide, aes(x = x, y = y)) +
    theme_cowplot() +
    geom_polygon(data = datpol, aes(x = xpol, y = ypol, fill = as.factor(time_to_detection)), alpha = 0.5) +
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
    theme(legend.position = "right")
  
  
  ggsave(paste0("all_capacity_thresholds_2.png"),
         plot = matdatp2, path = file.path(exp_dir), width = 15, height = 10, dpi = 300, device = "png"
  )
  ggsave(paste0("all_capacity_thresholds_2.pdf"),
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

# do(fitlm = loess(z ~ y + x, data = ., degree = 2, span = 0.7))
# do(fitlm = lm(z ~ poly(y + x,3), data = .))
# data.fit <- expand.grid(list(x = seq(0, 1, 0.01), y = seq(0, 1, 0.01)))
# z <- predict(elevation.loess, newdata = data.fit)
# data.fit$value <- as.numeric(z)
# 
# elevation.loess = subset(dfLM2, region=="6")$fitlm[[1]]
# 
# data.fit2 <- expand.grid(list(x = seq(0, 1, 0.01), y = seq(0, 1, 0.01)))
# z <- predict(elevation.loess, newdata = data.fit2)
# data.fit2$value <- as.numeric(z)
# 
# ggplot(data.fit, aes(x, y)) +
#   geom_tile(aes(fill=value)) +
#   geom_line(data=data.fit2, aes(x, value)) 




allEMSatOnce2 <- TRUE
if (allEMSatOnce2) {
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
  
  #  subdat <- filter(subdat, time_to_detection > 1)
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
  ggplot(data=subset(plotdat )) + theme_cowplot() +
    geom_line(aes(x=Date, y =value, group=scen_num, col=detection_success)) + 
    facet_wrap(~region, scales="free", nrow=2)+
    customThemeNoFacet+
    geom_hline(data=plotdat, aes(yintercept = capacity), linetype="dashed" ) +
    scale_color_viridis(discrete = FALSE )+
    labs(color="isolation_success", caption = "d_Sym_ct1 =1")
  
  
  plotdat$detection_success_fct = NA
  plotdat$detection_success_fct[plotdat$detection_success <0.1] = "<10"  
  plotdat$detection_success_fct[plotdat$detection_success >0.1 & plotdat$detection_success <0.2 ] = "<20"  
  plotdat$detection_success_fct[plotdat$detection_success >0.2 & plotdat$detection_success <0.3 ] = "<30"  
  plotdat$detection_success_fct[plotdat$detection_success >0.3 & plotdat$detection_success <0.4 ] = "<40"  
  plotdat$detection_success_fct[plotdat$detection_success >0.4 & plotdat$detection_success <0.5 ] = "<50"  
  plotdat$detection_success_fct[plotdat$detection_success >0.5 & plotdat$detection_success <0.6 ] = "<60"  
  plotdat$detection_success_fct[plotdat$detection_success >0.6 & plotdat$detection_success <0.7 ] = "<70"  
  plotdat$detection_success_fct[plotdat$detection_success >0.7 & plotdat$detection_success <0.8 ] = "<80"  
  plotdat$detection_success_fct[plotdat$detection_success >0.8 & plotdat$detection_success <0.9 ] = "<90" 
  plotdat$detection_success_fct[plotdat$detection_success >0.9 ] = ">90" 
  table(plotdat$detection_success_fct, exclude = NULL)
  
  ggplot(data=subset(plotdat, detection_success_fct == "<30" & isolation_success >= 0.7)) + 
    theme_cowplot() +
    geom_line(aes(x=Date, y =value, group=scen_num, col=detection_success)) + 
    facet_wrap(~region, scales="free", nrow=2)+
    customThemeNoFacet+
    geom_hline(data=plotdat, aes(yintercept = capacity), linetype="dashed" ) +
    scale_color_viridis(discrete = FALSE )+
    labs(color="isolation_success", caption = "d_Sym_ct1 =1")
  
  
  df <- subset(plotdat, Date == plotdat$Date_peak)
  # rm(plotdat)
  
  
  ## Filter dataset to get threshold values
  threshold_param <- df %>%
    group_by(region,outcome, detection_success,isolation_success, time_to_detection,  capacity) %>%
    summarize(value=mean(value))%>%
    filter(value <= capacity) %>%
    group_by(region,outcome, detection_success, time_to_detection,  capacity) %>%
    filter(isolation_success == min(isolation_success)) 
  
  
  threshold_param2 <- df %>%
    group_by(region,outcome, time_to_detection, detection_success,isolation_success, capacity) %>%
    summarize(value=mean(value))%>%
    filter(value <= capacity) %>%
    group_by(region,outcome,time_to_detection, detection_success) %>%
    filter(isolation_success == min(isolation_success)) 
  
  
  threshold_param2$detection_success_fct <- NA
  threshold_param2$detection_success_fct[threshold_param2$detection_success < 0.9] <-  0.9
  threshold_param2$detection_success_fct[threshold_param2$detection_success < 0.8] <-  0.8
  threshold_param2$detection_success_fct[threshold_param2$detection_success < 0.7] <-  0.7
  threshold_param2$detection_success_fct[threshold_param2$detection_success < 0.6] <-  0.6
  threshold_param2$detection_success_fct[threshold_param2$detection_success < 0.5] <-  0.5
  threshold_param2$detection_success_fct[threshold_param2$detection_success < 0.4] <-  0.4
  threshold_param2$detection_success_fct[threshold_param2$detection_success < 0.4] <-  0.4
  threshold_param2$detection_success_fct[threshold_param2$detection_success < 0.3] <-  0.3
  threshold_param2$detection_success_fct[threshold_param2$detection_success < 0.2] <-  0.2
  threshold_param2$detection_success_fct[threshold_param2$detection_success < 0.1] <-  0.1
  
  threshold_param2 <- threshold_param2 %>%
    group_by(region,outcome,time_to_detection, detection_success_fct) %>%
    dplyr::summarize(isolation_success = min(isolation_success)) %>%
    #dplyr::select(!is.na(detection_success_fct)) %>%
    dplyr::arrange(region,time_to_detection,  detection_success_fct)
  
  # 
  # for(reg in unique(threshold_param2$region))
  # for(i in temp$isolation_success)
  
  
  ggplot(data=threshold_param2) + 
    geom_line(aes(x= detection_success_fct, y= isolation_success),col="blue") +
    facet_grid(time_to_detection~region)
  
  geom_smooth(aes(x= detection_success_fct, y= isolation_success2),size=1.7,col="black",se=FALSE,method="lm", formula=y ~ poly(x, 2, raw=TRUE)) +
    geom_smooth(aes(x= detection_success_fct, y= isolation_success2),size=1.7,col="blue",se=FALSE) +
    facet_grid(time_to_detection~region)
  
  
  threshold_param3 <- df %>%
    group_by(region,outcome,  detection_success,isolation_success, capacity) %>%
    summarize(value=mean(value))%>%
    filter(value <= capacity) %>%
    group_by(region,outcome, detection_success) %>%
    filter(isolation_success == min(isolation_success)) 
  
  threshold_param3$detection_success_fct <- NA
  threshold_param3$detection_success_fct[threshold_param3$detection_success < 0.9] <-  0.9
  threshold_param3$detection_success_fct[threshold_param3$detection_success < 0.8] <-  0.8
  threshold_param3$detection_success_fct[threshold_param3$detection_success < 0.7] <-  0.7
  threshold_param3$detection_success_fct[threshold_param3$detection_success < 0.6] <-  0.6
  threshold_param3$detection_success_fct[threshold_param3$detection_success < 0.5] <-  0.5
  threshold_param3$detection_success_fct[threshold_param3$detection_success < 0.4] <-  0.4
  threshold_param3$detection_success_fct[threshold_param3$detection_success < 0.4] <-  0.4
  threshold_param3$detection_success_fct[threshold_param3$detection_success < 0.3] <-  0.3
  threshold_param3$detection_success_fct[threshold_param3$detection_success < 0.2] <-  0.2
  threshold_param3$detection_success_fct[threshold_param3$detection_success < 0.1] <-  0.1
  
  threshold_param3 <- threshold_param3 %>%
    dplyr::group_by(region,outcome, detection_success_fct) %>%
    dplyr::summarize(isolation_success = min(isolation_success)) #%>%
  #dplyr::select(!is.na(detection_success_fct)) %>%
  # dplyr::arrange(region, detection_success_fct) %>%
  # mutate(isolation_success2 = ifelse(isolation_success < lag(isolation_success), isolation_success, lag(isolation_success)))
  
  ggplot(data=threshold_param3) + 
    geom_line(aes(x= detection_success_fct, y= isolation_success)) +
    geom_smooth(aes(x= detection_success_fct, y= isolation_success),size=1.7,col="black",se=FALSE,method="lm", formula=y ~ poly(x, 2, raw=TRUE)) +
    facet_wrap(~region)
  
  # formula=lm(y ~ poly(x, 3, raw=TRUE)
  labs <- c("EMS_1\n limit: 148", "EMS_2\n limit: 181", "EMS_3\n limit: 103", "EMS_4\n limit: 98", "EMS_5\n limit: 88", "EMS_6\n limit: 109",
            "EMS_7\n limit: 404", "EMS_8\n limit: 255", "EMS_9\n limit: 265", "EMS_10\n limit: 150", "EMS_11\n limit: 785")
  
  df$region_label2 <- factor(df$region, levels = c(1:11), labels = labs)
  threshold_param2$region_label2 <- factor(threshold_param2$region, levels = c(1:11), labels = labs)
  threshold_param3$region_label2 <- factor(threshold_param3$region, levels = c(1:11), labels = labs)  
  
  
  testplot1 <- ggplot(data=df) + theme_classic() +
    geom_point(data=subset(df, value!=capacity),aes(x= detection_success, y= isolation_success, group=time_to_detection),shape=21,fill="white",size=2) +
    geom_point(data=subset(df, value<=capacity),aes(x= detection_success, y= isolation_success, fill=as.factor(time_to_detection)),shape=21,size=2) + 
    scale_fill_viridis(option = "C", discrete = TRUE, direction = -1) +
    geom_smooth(data=threshold_param2 ,aes(x= detection_success_fct, y= isolation_success, col=as.factor(time_to_detection),
                                           fill=as.factor(time_to_detection)), se=FALSE,method="lm", formula=y ~ poly(x, 2, raw=TRUE)) +
    scale_color_viridis(option = "C", discrete = TRUE, direction = -1) +
    customThemeNoFacet +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    theme(panel.spacing = unit(2, "lines")) +
    labs(
      color = "time to detection",
      subtitle = "",
      fill = "time to detection",
      x = "detections (P, As) (%)",
      y = "isolation success (%)"
    ) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    geom_hline(yintercept =c(-Inf, Inf)) +
    theme(legend.position = "right")+
    facet_wrap(~ region_label2)
  
  ggsave(paste0(selected_outcome, "_testplot1.png"),
         plot = testplot1, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
  )  
  
  
  
  testplot <- ggplot(data=df) + theme_classic() +
    geom_point(data=subset(df, value!=capacity),aes(x= detection_success, y= isolation_success, group=time_to_detection),shape=21,fill="white",size=2) + 
    geom_point(data=subset(df, value<=capacity),aes(x= detection_success, y= isolation_success, group=time_to_detection),shape=21,fill="black",size=2) + 
    #  geom_smooth(data=threshold_param2 ,aes(x= detection_success_fct, y= isolation_success, col=as.factor(time_to_detection),
    #                                       fill=as.factor(time_to_detection)), method="lm", formula=y ~ poly(x, 2, raw=TRUE)) +
    # geom_smooth(data=threshold_param2 ,aes(x= detection_success_fct, y= isolation_success),size=1.7,col="blue",se=FALSE) +
    #geom_smooth(data=threshold_param3, aes(x= detection_success_fct, y= isolation_success),size=1.3,col="black",se=FALSE, method="lm", formula=y ~ poly(x, 2, raw=TRUE)) +
    scale_fill_viridis(option = "C", discrete = TRUE, direction = -1) +
    scale_color_viridis(option = "C", discrete = TRUE, direction = -1) +
    customThemeNoFacet +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    theme(panel.spacing = unit(2, "lines")) +
    labs(
      color = "time to detection",
      subtitle = "",
      fill = "time to detection",
      x = "detections (P, As) (%)",
      y = "isolation success (%)"
    ) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    geom_hline(yintercept =c(-Inf, Inf)) +
    theme(legend.position = "right")+
    facet_wrap(~ region_label2)
  
  ggsave(paste0(selected_outcome, "_testplot.png"),
         plot = testplot, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
  )  
  
  
  
  
  testplot1lm <- ggplot(data=df) + theme_classic() +
    geom_point(data=subset(df, value!=capacity),aes(x= detection_success, y= isolation_success, group=time_to_detection),shape=21,fill="white",size=2) +
    geom_point(data=subset(df, value<=capacity),aes(x= detection_success, y= isolation_success, fill=as.factor(time_to_detection)),shape=21,size=2) + 
    scale_fill_viridis(option = "C", discrete = TRUE, direction = -1) +
    geom_smooth(data=threshold_param2 ,aes(x= detection_success_fct, y= isolation_success, col=as.factor(time_to_detection),
                                           fill=as.factor(time_to_detection)), se=FALSE,method="lm") +
    scale_color_viridis(option = "C", discrete = TRUE, direction = -1) +
    customThemeNoFacet +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    theme(panel.spacing = unit(2, "lines")) +
    labs(
      color = "time to detection",
      subtitle = "",
      fill = "time to detection",
      x = "detections (P, As) (%)",
      y = "isolation success (%)"
    ) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    geom_hline(yintercept =c(-Inf, Inf)) +
    theme(legend.position = "right")+
    facet_wrap(~ region_label2)
  
  ggsave(paste0(selected_outcome, "_testplot1lm.png"),
         plot = testplot1lm, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
  )  
  
  
  testplot1loess <- ggplot(data=df) + theme_classic() +
    geom_point(data=subset(df, value!=capacity),aes(x= detection_success, y= isolation_success, group=time_to_detection),shape=21,fill="white",size=2) +
    geom_point(data=subset(df, value<=capacity),aes(x= detection_success, y= isolation_success, fill=as.factor(time_to_detection)),shape=21,size=2) + 
    scale_fill_viridis(option = "C", discrete = TRUE, direction = -1) +
    geom_smooth(data=threshold_param2 ,aes(x= detection_success_fct, y= isolation_success, col=as.factor(time_to_detection),
                                           fill=as.factor(time_to_detection)),method = 'loess' , se=FALSE) +
    scale_color_viridis(option = "C", discrete = TRUE, direction = -1) +
    customThemeNoFacet +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    theme(panel.spacing = unit(2, "lines")) +
    labs(
      color = "time to detection",
      subtitle = "",
      fill = "time to detection",
      x = "detections (P, As) (%)",
      y = "isolation success (%)"
    ) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    geom_hline(yintercept =c(-Inf, Inf)) +
    theme(legend.position = "right")+
    facet_wrap(~ region_label2)
  
  ggsave(paste0(selected_outcome, "_testplot1loess.png"),
         plot = testplot1loess, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
  )  
  
  
  
  
}


