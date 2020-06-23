


# emsvars_temp <- c(
#   "susceptible_EMS.", "exposed_EMS.", "asymptomatic_EMS.", "symptomatic_mild_EMS.", "symptomatic_severe_EMS.",
#   "hospitalized_EMS.", "critical_EMS.", "deaths_EMS.", "recovered_EMS.", "asymp_cumul_EMS.",
#   "asymp_det_cumul_EMS.", "symp_mild_cumul_EMS.", "symp_severe_cumul_EMS.", "hosp_cumul_EMS.", "hosp_det_cumul_EMS.",
#   "crit_cumul_EMS.", "crit_det_cumul_EMS.", "crit_det_EMS.", "death_det_cumul_EMS.", "infected_EMS.",
#   "infected_cumul_EMS.", "symp_mild_det_cumul_EMS.", "symp_severe_det_cumul_EMS.", "detected_EMS.", "detected_cumul_EMS.",
#   "presymptomatic_EMS."
# )


# getdata
getdata <- function(dat, selected_ems) {
  #'  getdata is per default using the "trajectoriesDat" dataset to perfom specific data editing
  #'  Outcome variables of interest are selected for one or more EMS regions as specified in selected_ems
  #'  If multiple integers are included in the selected_ems, the data will be aggregated
  #'  The data is filtered by date to include only dates within the defined evaluation window
  #'  Factor variables per contact tracing parameter are generated to
  #'  optionally facilitate plotting or custom tables for data exploration
  #' @param dat  simulation dataset in wide format
  #' @param selected_ems  vector of integers representing one or multiple EMS areas


  # emsvars <- colnames(trajectoriesDat)[grep("[.]",colnames(trajectoriesDat))]
  emsvars_temp <- c("critical_EMS.","N_EMS_","Ki_EMS_" )

  emsvars <- NULL
  for (ems in selected_ems) {
    emsvars <- c(emsvars, paste0(emsvars_temp, ems))
  }

  groupvars <- c("startdate","Date","time", "backtonormal_multiplier", "scen_num", "sample_num", "run_num", "backtonormal_multiplier", "detection_success", "isolation_success", "time_to_detection"  )
  (keepvars <- c(groupvars, emsvars))

  subdat <- dat %>% dplyr::select(keepvars)

  subdat <- subdat %>%
    pivot_longer(cols = -c(groupvars)) %>%
    mutate( name = gsub("All", "EMS.IL", name)
    ) %>%
    dplyr::mutate(name = gsub("[.]", "_", name)) %>%
    separate(name, into = c("outcome", "region"), sep = "_EMS_") %>%
    dplyr::filter(Date >= reopeningdate) %>%
    select(-c(time))
  
  
  subdat1 <- subdat %>%
    filter(outcome %in% c("N", "Ki")) %>%
    pivot_wider(names_from = outcome, values_from = value)
  
  
  subdat <- subdat %>%
    filter(!(outcome %in% c("N", "Ki"))) %>%
    left_join(subdat1, by = c("startdate","Date","scen_num", "sample_num", "run_num" , "backtonormal_multiplier",  "detection_success", "isolation_success", "time_to_detection" ))

  if (length(selected_ems) > 1) {
    groupvars <- c(groupvars[groupvars != "time"], "outcome", "Date", "scen_num")

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
    group_by(N, region, time_to_detection, outcome) %>%
    do(fitlm = lm(z ~ poly(y + x, 3), data = .))
  # do(fitlm = lm(z ~ y + x, data = .))

  (dfLMCoef <- tidy(dfLM, fitlm))
  # augment(dfLM, fitlm)
  # glance(dfLM, fitlm)

  ## Parameter combinations that did run
  sink(file.path(exp_dir, paste0(geography, "_", ems, "_", selected_outcome, "_linear_models.txt")))
  cat("\ntidy(dfLM, fitlm)")
  print(tidy(dfLM, fitlm))
  # cat("\naugment(dfLM, fitlm)")
  # print(augment(dfLM, fitlm))
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
      aes(x = x, y = ythreshold, group = statistic), method = "lm", formula = y ~ poly(x, 2, raw = TRUE), col = "black", size = 0.75
    ) +
    geom_smooth(
      data = subset(matdat, statistic != "fit"),
      aes(x = x, y = ythreshold, group = statistic), method = "lm", formula = y ~ poly(x, 2, raw = TRUE), col = "black", size = 0.75, linetype = "dashed"
    ) +
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
  
  if(("fit" %in% colnames(tdat_wide))){
  
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
      method = "lm", formula = y ~ poly(x, 2, raw = TRUE), size = 1.3, show.legend = FALSE
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
  }else{
    matdatp2 = NULL
  }

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

### Select consecutive minimum values across all samples
minval <- function(x) {
  newval <- c()
  for (i in c(1:length(x))) {
    if (i == 1) temp <- x[i]

    if (i > 1) {
      if (x[i] <= temp) {
        temp <- x[i]
      }
      if (x[i] > temp) {
        temp <- temp
      }
    }

    newval[i] <- temp
  }

  return(newval)
}
