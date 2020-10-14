
#### --------------------------------------
####  Functions and setup
#### --------------------------------------


capacity_multiplier2_cols <- c("#d0d1e6", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0")
delay_cols <- c("#d0d1e6", "#7fcdbb", "#1d91c0")
rollback_cols <- c("#d0d1e6", "#1d91c0")


f_getCustomTheme <- function(fontscl = 0) {
  customTheme <- theme(
    strip.text.x = element_text(size = 12 + fontscl, face = "bold"),
    strip.text.y = element_text(size = 12 + fontscl, face = "bold"),
    strip.background = element_blank(),
    plot.title = element_text(size = 16 + fontscl, vjust = -1, hjust = 0),
    plot.subtitle = element_text(size = 14 + fontscl),
    plot.caption = element_text(size = 10 + fontscl),
    legend.title = element_text(size = 12 + fontscl),
    legend.text = element_text(size = 12 + fontscl),
    axis.text.x = element_text(size = 11 + fontscl),
    axis.text.y = element_text(size = 11 + fontscl),
    axis.title.x = element_text(size = 12 + fontscl),
    axis.title.y = element_text(size = 12 + fontscl),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.75)
  )
  return(customTheme)
}


#### --------------------------------------
#### Region characteristics
#### --------------------------------------

f_region_characteristics <- function(SAVE = FALSE) {
  if (SAVE == FALSE & file.exists(file.path(outdir, "region_characteristics.csv"))) {
    dat <- fread(file.path(outdir, "region_characteristics.csv"))
    print(paste0("File loaded from ", outdir, "region_characteristics.csv"))
  }

  if (!(file.exists(file.path(outdir, "region_characteristics.csv")))) {
    popDat <- load_population()
    capacityDat <- load_new_capacity()

    dat <- left_join(popDat, capacityDat, by = c("geography_name"))
    dat$icu_available <- as.numeric(dat$icu_available)
    dat$pop <- as.numeric(dat$pop)
    dat$popDens <- (dat$pop / 10000)
    dat$icubeds_per10th <- (dat$icu_available / dat$pop) * 10000
    dat$medsurgbeds_per10th <- (dat$medsurg_available / dat$pop) * 10000
  }


  if (SAVE) {
    fwrite(dat, file = file.path(outdir, "region_characteristics.csv"), quote = FALSE, row.names = FALSE)
    print(paste0("File written to ", outdir, "region_characteristics.csv"))
  }

  return(dat)
}


f_region_Rt_values <- function() {
  civis_dir <- file.path(project_path, "NU_civis_outputs")
  simdate <- list.dirs(civis_dir, recursive = F, full.names = F)
  simdate <- simdate[length(simdate)]
  # simdate = "20200916"

  dat <- fread(file.path(civis_dir, simdate, "csv", paste0("nu_", simdate, ".csv")))

  initial_Rt <- dat %>%
    filter(date <= as.Date("2020-03-12")) %>%
    group_by(geography_modeled) %>%
    summarize(
      rt_median_initial = mean(rt_median),
      rt_lower_initial = mean(rt_lower),
      rt_upper_initial = mean(rt_upper)
    )


  baselineRt <- dat %>%
    filter(date >= as.Date("2020-09-15") & date <= as.Date("2020-09-16")) %>%
    group_by(geography_modeled) %>%
    summarize(
      rt_median_base = mean(rt_median),
      rt_lower_base = mean(rt_lower),
      rt_upper_base = mean(rt_upper)
    )

  #### Get peak from reopening! or trigger!

  peak_Rt <- dat %>%
    group_by(geography_modeled) %>%
    filter(date >= as.Date("2020-10-01")) %>%
    filter(rt_median == max(rt_median, na.rm = TRUE)) %>%
    filter(date == min(date)) %>%
    summarize(
      rt_median_peak = mean(rt_median),
      rt_lower_peak = mean(rt_lower),
      rt_upper_peak = mean(rt_upper)
    )


  Rtdat <- initial_Rt %>%
    left_join(baselineRt, by = c("geography_modeled")) %>%
    left_join(peak_Rt, by = c("geography_modeled")) %>%
    mutate(
      base_peak_rto = ((rt_median_peak - rt_median_base) / rt_median_base) * 100,
      initial_base_rto = rt_median_initial / rt_median_base
    )
}

#### --------------------------------------
#### General helper functions
#### --------------------------------------

f_combine_Rdata <- function(Rdata_files) {
  library(miceadds)
  library(data.table)

  datList <- list()
  for (Rdata_file in Rdata_files) {
    load.Rdata(filename = file.path(Rdata_file), objname = "tempdat")
    datList[[length(datList) + 1]] <- tempdat
    rm(tempdat)
  }
  dat <- data.table::rbindlist(datList)
  return(dat)
}

f_combine_csv_from_list <- function(csv_file_dirs) {
  library(tidyverse)
  library(purrr)

  tbl_fread <-
    file.path(csv_file_dirs) %>%
    map_df(~ fread(.))

  return(tbl_fread)
}

f_remove_legend <- function(p) {
  p <- p + theme(legend.position = "none")
  return(p)
}

f_save_plot <- function(pplot, plot_name, plot_dir, width = 14, height = 8) {
  ggsave(paste0(plot_name, ".png"), plot = pplot, path = plot_dir, width = width, height = height, device = "png")
  ggsave(paste0(plot_name, ".pdf"), plot = pplot, path = plot_dir, width = width, height = height, device = "pdf")
}


load_sim_data <- function(exp_dir) {
  trajectoriesDat <- fread(file = file.path(exp_dir, "trajectoriesDat_trim.csv"))

  outcomeVars <- c("All", paste0("EMS-", c(1:11)))
  outcomeVars <- paste0("crit_det_", outcomeVars)
  outVars <- c("date", "scen_num", "sample_num", "capacity_multiplier", outcomeVars)

  simdat <- trajectoriesDat %>%
    dplyr::mutate(date = as.Date(startdate) + time) %>%
    dplyr::select(outVars) %>%
    pivot_longer(cols = -c(date, scen_num, sample_num, capacity_multiplier)) %>%
    separate(name, into = c("channel", "det", "geography_name"), sep = "_") %>%
    mutate(
      exp_name = exp_name,
      geography_name = gsub("EMS-", "", geography_name),
      geography_name = gsub("All", "illinois", geography_name),
      exp_label = gsub("20200919_IL_regreopen", "", exp_name)
    ) %>%
    separate(exp_label, into = c("reopen", "delay", "rollback"), sep = "_")

  simdat$rollback <- factor(simdat$rollback,
    levels = c("sm7", "sm4"),
    labels = c("sm7", "sm4")
  )

  simdat$reopen <- factor(simdat$reopen,
    levels = rev(c("100perc", "50perc")),
    labels = rev(c("100perc", "50perc"))
  )

  simdat$delay <- factor(simdat$delay,
    levels = rev(c("0daysdelay", "3daysdelay", "7daysdelay")),
    labels = rev(c("none", "3 days", "7 days"))
  )



  simdat$region <- factor(simdat$geography_name,
    levels = c("illinois", c(1:11)),
    labels = c("illinois", c(1:11))
  )


  return(simdat)
}

## Load simulation data
f_load_single_exp <- function(exp_dir, mainVars = NULL, summarize = TRUE, maxDate = as.Date("2020-12-31")) {
  if (!(file.exists(file.path(exp_dir, "trajectoriesDat_sub_long.csv")))) {
    fname <- "trajectoriesDat_trim.csv"
    if (!(file.exists(file.path(exp_dir, fname)))) fname <- "trajectoriesDat.csv"

    dat <- fread(file = file.path(exp_dir, fname))
    popDat <- f_region_characteristics()


    colnames(dat) <- gsub("_All", "_EMS-illinois", colnames(dat))
    emsVars <- c("EMS-illinois", paste0("EMS-", c(1:11)))

    outcomeVars <- c(
      paste0("infected_cumul_", emsVars), paste0("death_det_cumul_", emsVars),
      paste0("crit_det_", emsVars), paste0("crit_det_cumul_", emsVars)
    )

    if (is.null(mainVars)) mainVars <- c("date", "scen_num", "sample_num", "capacity_multiplier")
    outVars <- c(mainVars, outcomeVars)

    tic()
    dat <- dat %>%
      dplyr::mutate(date = as.Date(startdate) + time) %>%
      dplyr::select(outVars) %>%
      pivot_longer(cols = -c(mainVars)) %>% ##  4.8 sec elapsed
      separate(name, into = c("channel", "geography_name"), sep = "_EMS-") %>% ### 27.72 sec
      dplyr::mutate(exp_name = exp_name) %>%
      left_join(popDat, by = "geography_name")
    toc()

    grpVars <- c("date", "geography_name", "channel", "exp_name", mainVars[length(mainVars)])

    if (summarize) {
      tic()
      dat <- dat %>%
        dplyr::group_by_at(.vars = grpVars) %>%
        dplyr::summarize(
          min.value = min(value, na.rm = TRUE),
          max.value = max(value, na.rm = TRUE),
          median.value = median(value, na.rm = TRUE),
          q25.value = quantile(value, probs = 0.25, na.rm = TRUE),
          q75.value = quantile(value, probs = 0.75, na.rm = TRUE),
          q2.5.value = quantile(value, probs = 0.025, na.rm = TRUE),
          q97.5.value = quantile(value, probs = 0.975, na.rm = TRUE)
        ) %>%
        left_join(popDat, by = "geography_name")
      toc() ### 8 min !!!
    }


    dat$date <- as.Date(dat$date)
    dat <- subset(dat, date <= maxDate)

    fwrite(dat, file = file.path(exp_dir, "trajectoriesDat_sub_long.csv"), quote = FALSE)
  } else {
    dat <- fread(file = file.path(exp_dir, "trajectoriesDat_sub_long.csv"))
  }


  dat$geography_name <- factor(dat$geography_name, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))

  return(dat)
}


#### ICU timeline
f_icu_timeline <- function(dat, subregions = NULL, selected_channel = "crit_det", facetVar = "geography_name") {
  if (!exists("customTheme")) customTheme <- f_getCustomTheme()
  # unique(dat$channel)
  if (is.null(subregions)) subregions <- unique(dat$geography_name)

  subdat <- subset(dat, geography_name %in% subregions & channel == selected_channel)
  subdat$date <- as.Date(subdat$date)
  subdat[, "facetVar"] <- subdat[, which(colnames(subdat) == facetVar)]

  pplot <- ggplot(data = subdat) +
    background_grid() +
    geom_ribbon(aes(x = date, ymin = q2.5.value, ymax = q97.5.value), alpha = 0.3) +
    geom_ribbon(aes(x = date, ymin = q25.value, ymax = q75.value), alpha = 0.3) +
    geom_line(aes(x = date, y = median.value)) +
    geom_hline(aes(yintercept = icu_available), col = "red", linetype = "dashed") +
    scale_x_date(date_breaks = "60 days", date_labels = "%b") +
    customTheme +
    geom_hline(yintercept = c(Inf)) +
    geom_vline(xintercept = c(Inf)) +
    facet_wrap(~facetVar, scales = "free") +
    labs(title = paste0("subregions: ", subregions), subtitle = "", x = "", y = selected_channel)

  return(pplot)
}

f_get_cumul_numbers <- function(dat = simdat, subregions = NULL, selected_channel = "crit_det_cumul", facetVar = "capacity_multiplier") {

  # unique(dat$channel)
  if (is.null(subregions)) subregions <- unique(dat$geography_name)

  keepVars <- c(facetVar, "geography_name", "icu_available", "median.value", "q2.5.value", "q97.5.value", "channel", "exp_name")

  subdat <- dat %>%
    ungroup() %>%
    dplyr::filter(geography_name %in% subregions & channel == selected_channel & date >= as.Date("2020-12-30") & date <= as.Date("2020-12-31")) %>%
    dplyr::select_at(.vars = keepVars)

  return(subdat)
}

f_get_peak_numbers <- function(dat = simdat, subregions = NULL, selected_channel = "crit_det", facetVar = "capacity_multiplier") {

  # unique(dat$channel)
  if (is.null(subregions)) subregions <- unique(dat$geography_name)

  grpVars <- c(facetVar, "geography_name", "exp_name")
  keepVars <- c(
    facetVar, "geography_name", "exp_name", "icu_available", "median.value", "q2.5.value", "q97.5.value",
    "median.aboveICU", "q2.5.aboveICU", "q97.5.aboveICU",
    "median.aboveICU_ratio", "q2.5.aboveICU_ratio", "q97.5.aboveICU_ratio",
    "channel", "exp_name"
  )


  subdat <- dat %>%
    dplyr::group_by_at(.vars = grpVars) %>%
    dplyr::filter(geography_name %in% subregions & channel == selected_channel) %>%
    dplyr::filter(median.value == max(median.value)) %>%
    dplyr::filter(date == min(date)) %>%
    dplyr::mutate(
      median.aboveICU = median.value - icu_available,
      q2.5.aboveICU = q2.5.value - icu_available,
      q97.5.aboveICU = q97.5.value - icu_available,
      median.aboveICU_ratio = median.value / icu_available,
      q2.5.aboveICU_ratio = q2.5.value / icu_available,
      q97.5.aboveICU_ratio = q97.5.value / icu_available
    ) %>%
    ungroup() %>%
    dplyr::select_at(.vars = keepVars)

  return(subdat)
}




#### --------------------------------------
####  Baseline - predicted ICU
#### --------------------------------------



#### --------------------------------------
#### Counterfactual - rebound scenarios
#### --------------------------------------

f_get_rebound_values <- function(dat = simdat) {
  moderate_rebound <- dat %>%
    dplyr::group_by(geography_name, reopening_multiplier_4) %>%
    filter(channel == "crit_det") %>%
    filter(median.value == max(median.value)) %>%
    dplyr::mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0)) %>%
    dplyr::group_by(geography_name) %>%
    filter(belowCapacity == 1) %>%
    filter(reopening_multiplier_4 == max(reopening_multiplier_4)) %>%
    filter(date == min(date)) %>%
    dplyr::select(geography_name, reopening_multiplier_4) %>%
    rename(moderate_rebound = reopening_multiplier_4)

  dat_out <- dat %>%
    dplyr::group_by(geography_name, reopening_multiplier_4) %>%
    filter(channel == "crit_det") %>%
    filter(q2.5.value == max(q2.5.value)) %>%
    dplyr::mutate(belowCapacity = ifelse(q2.5.value <= icu_available, 1, 0)) %>%
    dplyr::group_by(geography_name) %>%
    filter(belowCapacity == 1) %>%
    filter(reopening_multiplier_4 == max(reopening_multiplier_4)) %>%
    filter(date == min(date)) %>%
    dplyr::select(geography_name, reopening_multiplier_4) %>%
    rename(fast_rebound = reopening_multiplier_4) %>%
    left_join(moderate_rebound, by = "geography_name")

  return(dat_out)
}

### using f_get_cumul_numbers
f_describe_ICU_cumul <- function(subfolder, SAVE = TRUE, facetVar = "reopening_multiplier_4") {
  rebound <- f_get_rebound_values()

  df1 <- f_get_cumul_numbers(dat = simdat, selected_channel = "crit_det_cumul", facetVar = facetVar) %>%
    left_join(rebound, by = "geography_name") %>%
    filter(reopening_multiplier_4 == fast_rebound) %>%
    dplyr::select(-moderate_rebound, -fast_rebound) %>%
    mutate(rebound = "fast_rebound")

  df2 <- f_get_cumul_numbers(dat = simdat, selected_channel = "crit_det_cumul", facetVar = facetVar) %>%
    left_join(rebound, by = "geography_name") %>%
    filter(reopening_multiplier_4 == moderate_rebound) %>%
    dplyr::select(-fast_rebound, -moderate_rebound) %>%
    mutate(rebound = "moderate_rebound")


  df12 <- rbind(df1, df2)
  pplot <- ggplot(data = subset(df12, geography_name != "illinois")) +
    geom_bar(aes(x = geography_name, y = median.value, fill = rebound), position = "dodge", stat = "identity")

  df12tab <- df12 %>%
    arrange(rebound, median.value) %>%
    mutate(diff = icu_available / median.value) %>%
    dplyr::select(-exp_name, -channel) %>%
    as.data.frame()

  if (SAVE) {
    if (!dir.exists(file.path(outdir, subfolder))) dir.create(file.path(outdir, subfolder))
    ggsave(paste0("describe_ICU_cumul_rebound.png"),
      plot = pplot, path = file.path(outdir, subfolder), width = 14, height = 8, device = "png"
    )

    fwrite(df12tab, file.path(outdir, subfolder, "describe_ICU_cumul_rebound.csv"))
  }


  out <- list(df12, df12tab, pplot)
}

### using f_get_peak_numbers
f_describe_ICU_peak <- function(subfolder, SAVE = TRUE, facetVar = "reopening_multiplier_4") {
  rebound <- f_get_rebound_values()

  df1 <- f_get_peak_numbers(dat = simdat, selected_channel = "crit_det", facetVar = facetVar) %>%
    left_join(rebound, by = "geography_name") %>%
    filter(reopening_multiplier_4 == fast_rebound) %>%
    dplyr::select(-moderate_rebound, -fast_rebound) %>%
    mutate(rebound = "fast_rebound")

  df2 <- f_get_peak_numbers(dat = simdat, selected_channel = "crit_det", facetVar = facetVar) %>%
    left_join(rebound, by = "geography_name") %>%
    filter(reopening_multiplier_4 == moderate_rebound) %>%
    dplyr::select(-fast_rebound, -moderate_rebound) %>%
    mutate(rebound = "moderate_rebound")


  df12 <- rbind(df1, df2)
  pplot <- ggplot(data = subset(df12, geography_name != "illinois")) +
    geom_bar(aes(x = geography_name, y = median.value, fill = rebound), position = "dodge", stat = "identity")

  df12tab <- df12 %>%
    arrange(rebound, median.value) %>%
    dplyr::select(-exp_name, -channel) %>%
    arrange(rebound, median.aboveICU_ratio) %>%
    dplyr::select(rebound, geography_name, everything()) %>%
    as.data.frame()

  if (SAVE) {
    if (!dir.exists(file.path(outdir, subfolder))) dir.create(file.path(outdir, subfolder))
    ggsave(paste0("describe_ICU_peak_rebound.png"),
      plot = pplot, path = file.path(outdir, subfolder), width = 14, height = 8, device = "png"
    )

    fwrite(df12tab, file.path(outdir, subfolder, "describe_ICU_peak_rebound.csv"))
  }

  out <- list(df12, df12tab, pplot)
}



#### --------------------------------------
#### Trigger simulations
#### --------------------------------------


f_describe_peak_and_cumul <- function(dat = simdat, subfolder, SAVE = TRUE) {
  tab_cumul_100 <- f_get_cumul_numbers(dat = dat, selected_channel = "crit_det_cumul")
  #### Get peak  numbers
  tab_peak_100 <- f_get_peak_numbers(dat = dat, selected_channel = "crit_det")

  pplot_peak <- ggplot(data = tab_peak_100) +
    geom_bar(aes(x = capacity_multiplier, y = median.aboveICU, fill = exp_name), position = "stack", stat = "identity") +
    facet_wrap(~geography_name, scales = "free")

  pplot_cumul <- ggplot(data = tab_cumul_100) +
    geom_bar(aes(x = capacity_multiplier, y = median.value, fill = exp_name), position = "stack", stat = "identity") +
    facet_wrap(~geography_name, scales = "free")


  if (SAVE) {
    if (!dir.exists(file.path(outdir, subfolder))) dir.create(file.path(outdir, subfolder))

    f_save_plot(pplot = pplot_cumul, plot_name = "pplot_cumul", plot_dir = file.path(outdir, subfolder))
    f_save_plot(pplot = pplot_peak, plot_name = "pplot_peak", plot_dir = file.path(outdir, subfolder))

    fwrite(tab_cumul_100, file.path(outdir, subfolder, "tab_cumul_100.csv"))
    fwrite(tab_peak_100, file.path(outdir, subfolder, "tab_peak_100.csv"))
  }



  out <- list(tab_cumul_100, tab_peak_100)
  names(out) <- c("tab_cumul_100", "tab_peak_100")
  return(out)
}


f_stacked_barplot <- function(dflist = list_csvs, subregions = NULL, rollback = "sm4", reopen = "50perc", exp_name_sub, stackLike = FALSE) {
  
  library(dplyr)
  library(plyr)
  
  if (!exists("customTheme")) customTheme <- f_getCustomTheme()
  # customTheme <- f_getCustomTheme(fontscl = -3)
  
  dflist <- dflist[grep("regreopen", dflist)]

  tbl_fread <-
    file.path(simulation_output, "_overflow_simulations", dflist) %>%
    map_df(~ fread(.)) %>%
    filter(geography_name %in% subregions)
  
  out <- f_describe_peak_and_cumul(dat = tbl_fread, subfolder = exp_name_sub, SAVE = FALSE)
  tab_peak <- out[[2]]

  tab_peak <- tab_peak %>%
    dplyr::mutate(exp_name_split = exp_name) %>%
    separate(exp_name_split, into = c("simdate", "locale", "reopen", "delay", "rollback"), sep = "_") %>%
    dplyr::mutate(
      reopen = gsub("regreopen", "", reopen),
      delay = gsub("daysdelay", " days", delay)
    )

  y_lu <-  round_any(min(tab_peak$median.aboveICU), 10, f = ceiling)   
  y_up <-  round_any(max(tab_peak$median.aboveICU), 10, f = ceiling)  
  if(y_up <0)y_up=0
  y_step <- round_any(max(abs(y_lu),abs(y_up))/3, 10, f = ceiling)
  labels_and_breaks = seq(y_lu, y_up, y_step)
  
  tab_peak <- tab_peak[tab_peak$reopen==reopen,]
  tab_peak <- tab_peak[tab_peak$rollback==rollback,]

  if (reopen == "50perc") selectedCols <- c("#c6dbef", "#6baed6", "#2171b5")
  if (reopen == "100perc") selectedCols <- c("#fee0d2", "#fb6a4a", "#cb181d")


  suppressWarnings(capacity_threshold <- tab_peak %>%
    dplyr::filter(median.aboveICU >= 0) %>%
    dplyr::filter(capacity_multiplier == min(capacity_multiplier)) %>%
    dplyr::select(capacity_multiplier) %>%
    unique() %>%
    as.numeric())

  if (is.na(capacity_threshold)) capacity_threshold <- 1

  tab_peak$capacity_multiplier_fct <- as.factor(round(tab_peak$capacity_multiplier * 100, 0))
  # tab_peak$delay_rev <- factor(tab_peak$delay, levels=rev(c("0 days","3 days","7 days")), labels=rev(c("0 days","3 days","7 days")))
  
  pplot_peak <- ggplot(data = tab_peak) +
    geom_hline(yintercept = Inf) +
    geom_vline(xintercept = Inf) +
    geom_bar(data = subset(tab_peak), aes(x = capacity_multiplier_fct, y = median.aboveICU, fill = delay, group = interaction(delay, capacity_multiplier)), 
             col = "azure4", position = position_dodge(width = 0.5), stat = "identity") +
    # geom_bar(data = subset(tab_peak, capacity_multiplier < capacity_threshold), aes(x = as.factor(capacity_multiplier), y = median.aboveICU, fill = delay, group=interaction(delay,capacity_multiplier)), col = "azure4", position = position_dodge(width = 0.5), stat = "identity") +
    # geom_bar(data = subset(tab_peak, capacity_multiplier > capacity_threshold), aes(x = as.factor(capacity_multiplier), y = median.aboveICU, fill = delay_rev, group=interaction(delay,capacity_multiplier)), col = "azure4", position = position_dodge(width = 0.5), stat = "identity") +
    scale_y_continuous(lim = c(y_lu, y_up), labels = labels_and_breaks, breaks = labels_and_breaks) +
    labs(
      subtitle = "", y = expression(italic(ICU[predicted_peak] - ICU[capacity])),
      # caption="Difference between predicted\npeak ICU cases and ICU availability",
      x = "Trigger threshold"
    ) +
    facet_wrap(~ rollback + reopen) +
    customTheme +
    scale_fill_manual(values = selectedCols) +
    scale_color_manual(values = selectedCols) +
    geom_hline(yintercept = 0, linetype = "solid", size = 0.75) +
    theme(panel.grid.major.y = element_line(colour = "grey", size = 0.75))


  if (stackLike) {
    pplot_peak <- ggplot(data = tab_peak) +
      geom_hline(yintercept = Inf) +
      geom_vline(xintercept = Inf) +
      geom_bar(data = subset(tab_peak, delay == "0 days"), aes(x = capacity_multiplier, y = median.aboveICU, fill = delay), col = "azure4", position = position_dodge(width = 0.01), stat = "identity") +
      geom_bar(data = subset(tab_peak, delay == "3 days"), aes(x = capacity_multiplier, y = median.aboveICU, fill = delay), col = "azure4", position = position_dodge(width = 0.01), stat = "identity") +
      geom_bar(data = subset(tab_peak, delay == "7 days"), aes(x = capacity_multiplier, y = median.aboveICU, fill = delay), col = "azure4", position = position_dodge(width = 0.01), stat = "identity") +
      geom_bar(data = subset(tab_peak, delay == "7 days" & capacity_multiplier > capacity_threshold), aes(x = capacity_multiplier, y = median.aboveICU, fill = delay), col = "azure4", position = position_dodge(width = 0.01), stat = "identity") +
      geom_bar(data = subset(tab_peak, delay == "3 days" & capacity_multiplier > capacity_threshold), aes(x = capacity_multiplier, y = median.aboveICU, fill = delay), col = "azure4", position = position_dodge(width = 0.01), stat = "identity") +
      geom_bar(data = subset(tab_peak, delay == "0 days" & capacity_multiplier > capacity_threshold), aes(x = capacity_multiplier, y = median.aboveICU, fill = delay), col = "azure4", position = position_dodge(width = 0.01), stat = "identity") +
      facet_wrap(~ rollback + reopen) +
      scale_y_continuous(lim = c(-400, 500), labels = seq(-400, 500, 150), breaks = seq(-400, 500, 150)) +
      labs(
        subtitle = "", y = expression(italic(ICU[predicted_peak] - ICU[capacity])),
        # caption="Difference between predicted\npeak ICU cases and ICU availability",
        x = "Trigger threshold"
      ) +
      customTheme +
      scale_fill_manual(values = selectedCols) +
      scale_color_manual(values = selectedCols) +
      geom_hline(yintercept = 0, linetype = "solid", size = 0.75) +
      theme(panel.grid.major.y = element_line(colour = "grey", size = 0.75))
  }

  # panel.grid.minor.y = element_line(colour="grey", size=0.5)
  # y=expression(Delta*italic(ICU[capacity])),
  out <- list(tab_peak, pplot_peak)
  return(out)
}


f_get_probabilities <- function(exp_dir, SAVE = TRUE) {
  simdat <- load_sim_data(exp_dir)
  simdat <- simdat %>% left_join(load_new_capacity(), by = "geography_name")
  simdat <- simdat %>% left_join(load_population(), by = "geography_name")

  propDat_sim <- simdat %>%
    dplyr::filter(channel == "crit" & date > as.Date("2020-09-19") & date <= as.Date("2020-12-31")) %>%
    dplyr::group_by(scen_num, sample_num, geography_name, exp_name, reopen, delay, rollback, pop, capacity_multiplier, icu_available) %>%
    dplyr::summarize(value = max(value)) %>%
    dplyr::mutate(aboveCapacity = ifelse(value >= icu_available, 1, 0)) %>%
    dplyr::group_by(geography_name, exp_name, reopen, delay, rollback, pop, capacity_multiplier, icu_available) %>%
    add_tally(name = "nsamples") %>%
    dplyr::group_by(geography_name, exp_name, reopen, delay, rollback, pop, capacity_multiplier, icu_available, nsamples) %>%
    dplyr::summarize(nabove = sum(aboveCapacity)) %>%
    dplyr::mutate(prob_overflow = nabove / nsamples)

  propDat_sim$geography_name <- factor(propDat_sim$geography_name, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))

  if (SAVE) save(propDat_sim, file = file.path(exp_dir, "propDat_sim.Rdata"))

  return(propDat_sim)
}


f_custom_prob_plot <- function(dat, plot_name, exp_dir, SAVE = TRUE, showPoints = FALSE, width = 14, height = 8, reopenFacet = TRUE) {
  dat$grpVar <- paste0(gsub("none", "0days", gsub(" ", "", dat$delay)), "-", dat$reopen)

  dat$grpVar <- factor(dat$grpVar,
    levels = c(
      "7days-100perc", "3days-100perc", "0days-100perc",
      "7days-50perc", "3days-50perc", "0days-50perc"
    ),
    labels = c(
      "7days-100perc", "3days-100perc", "0days-100perc",
      "7days-50perc", "3days-50perc", "0days-50perc"
    )
  )

  custom_cols <- c(
    "0days-50perc" = "#c6dbef", "3days-50perc" = "#6baed6", "7days-50perc" = "#2171b5",
    "0days-100perc" = "#fee0d2", "3days-100perc" = "#fb6a4a", "7days-100perc" = "#cb181d"
  )


  pplot <- ggplot(data = dat) +
    geom_line(aes(x = capacity_multiplier, y = prob_overflow, group = exp_name, col = grpVar, linetype = rollback), size = 1.1) +
    geom_hline(yintercept = 0.2) +
    scale_color_manual(values = custom_cols) +
    scale_fill_manual(values = custom_cols) +
    scale_y_continuous(lim = c(0, 1)) +
    customTheme +
    background_grid()


  if (reopenFacet) pplot <- pplot + facet_wrap(reopen ~ geography_name, scales = "free")
  if (reopenFacet == FALSE) pplot <- pplot + facet_wrap(~geography_name, scales = "free")
  if (showPoints) pplot <- pplot + geom_point(aes(x = capacity_multiplier, y = prob_overflow, group = exp_name, fill = reopen), col = "white", shape = 21, size = 2)

  f_save_plot(pplot = pplot, plot_name = plot_name, plot_dir = file.path(exp_dir), width = width, height = height)

  return(pplot)
}


f_custom_prob_plot2 <- function(dat, subregions = NULL, exp_dir, plot_name, width = 15, height = 10) {
  if (!exists("customTheme")) customTheme <- f_getCustomTheme()
  if (is.null(subregions)) subregions <- unique(dat$geography_name)

  pplot <- ggplot(data = subset(dat, geography_name %in% subregions)) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_rect(ymin = -Inf, ymax = Inf, xmin = 0.8, xmax = Inf, fill = "grey", alpha = 0.01) +
    geom_rect(ymin = 0.5, ymax = Inf, xmin = -Inf, xmax = 0.8, fill = "grey", alpha = 0.01) +
    geom_line(aes(x = capacity_multiplier, y = prob_overflow, group = geography_name, col = geography_name), size = 1.1) +
    # geom_hline(yintercept = 0.2) +
    geom_vline(xintercept = 0.8, linetype = "dashed") +
    scale_color_viridis_d() +
    scale_y_continuous(lim = c(0, 1)) +
    customTheme +
    background_grid() +
    facet_grid(reopen + rollback ~ delay) +
    theme(panel.spacing = unit(2, "lines"))

  f_save_plot(pplot = pplot, plot_name = plot_name, plot_dir = file.path(exp_dir), width = width, height = height)
}


f_generate_prob_treshold_dat <- function(propDat_sim = propDat_sim) {
  dat <- list()
  for (riskTolerance in seq(0.1, 1, 0.1)) {
    dat[[length(dat) + 1]] <- propDat_sim %>%
      filter(prob_overflow <= riskTolerance) %>%
      group_by(geography_name, exp_name) %>%
      mutate(risk_tolerance = riskTolerance) %>%
      filter(prob_overflow == max(prob_overflow)) %>%
      filter(capacity_multiplier == max(capacity_multiplier))
  }
  dat <- dat %>% bind_rows()

  dat$grpVar <- paste0(gsub("none", "0days", gsub(" ", "", dat$delay)), "-", dat$reopen)

  delay_reopen <- c(
    "7days-100perc", "3days-100perc", "0days-100perc",
    "7days-50perc", "3days-50perc", "0days-50perc"
  )

  dat$grpVar <- factor(dat$grpVar, levels = delay_reopen, labels = delay_reopen)

  dat$reopen_label <- NA
  dat$reopen_label[dat$reopen == "100perc"] <- "Fast rebound"
  dat$reopen_label[dat$reopen == "50perc"] <- "Moderate rebound"

  return(dat)
}

f_generate_generic_recommended_dat <- function() {
  simdat_generic <- simdat %>%
    dplyr::filter(capacity_multiplier >= 0.7 & capacity_multiplier <= 0.8) %>%
    dplyr::mutate(
      scenario = "generic",
      capacity_recom = capacity_multiplier,
      risk_tolerance = "generic"
    ) %>%
    dplyr::select(
      date, geography_name, icu_available, exp_name, capacity_recom, risk_tolerance,
      min.value, max.value, median.value, q25.value, q75.value, q2.5.value, q97.5.value
    )

  reg_capacities <- propDat_sim %>%
    dplyr::filter(exp_name != "20200919_IL_gradual_reopening_sm7") %>%
    dplyr::group_by(geography_name, icu_available, exp_name, reopen, delay, rollback) %>%
    dplyr::filter(prob_overflow <= 0.5) %>%
    dplyr::filter(capacity_multiplier == max(capacity_multiplier)) %>%
    dplyr::rename(capacity_recom = capacity_multiplier) %>%
    dplyr::mutate(risk_tolerance = "reg_capacities_05") %>%
    dplyr::select(geography_name, icu_available, exp_name, reopen, delay, rollback, capacity_recom, risk_tolerance)

  simdat_specific <- simdat %>%
    dplyr::left_join(reg_capacities, by = c("geography_name", "icu_available", "exp_name")) %>%
    dplyr::filter(capacity_multiplier == capacity_recom) %>%
    dplyr::mutate(scenario = "specific") %>%
    dplyr::select(
      date, geography_name, icu_available, exp_name, capacity_recom, risk_tolerance,
      min.value, max.value, median.value, q25.value, q75.value, q2.5.value, q97.5.value
    )


  simdat_compare <- rbind(simdat_generic, simdat_specific) %>% mutate(date = as.Date(date))

  # table(simdat_compare$geography_name, simdat_compare$risk_tolerance)

  return(simdat_compare)
}
