
#### --------------------------------------
####  Functions and setup
#### --------------------------------------


capacity_multiplier2_cols <- c("#d0d1e6", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0")
delay_cols <- c("#d0d1e6", "#7fcdbb", "#1d91c0")
rollback_cols <- c("#d0d1e6", "#1d91c0")



#### --------------------------------------
#### Region characteristics
#### --------------------------------------

f_region_characteristics <- function(SAVE = FALSE) {
  popDat <- load_population()
  capacityDat <- load_new_capacity()
  
  dat <- left_join(popDat, capacityDat, by = c("geography_name"))
  dat$icu_available <- as.numeric(dat$icu_available)
  dat$pop <- as.numeric(dat$pop)
  dat$popDens <- (dat$pop / 10000)
  dat$icubeds_per10th <- (dat$icu_available / dat$pop) * 10000
  dat$medsurgbeds_per10th <- (dat$medsurg_available / dat$pop) * 10000
  
  
  return(dat)
  
  if (SAVE) fwrite(dat, file = file.path(outdir, "region_characteristics.csv"), quote = FALSE, row.names = FALSE)
}


#### --------------------------------------
#### General helper functions
#### --------------------------------------

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
    
    
    dat <- dat %>%
      dplyr::mutate(date = as.Date(startdate) + time) %>%
      dplyr::select(outVars) %>%
      pivot_longer(cols = -c(mainVars)) %>%
      separate(name, into = c("channel", "geography_name"), sep = "_EMS-") %>%
      dplyr::mutate(
        exp_name = exp_name
      ) %>%
      left_join(popDat, by = "geography_name")
    
    
    grpVars <- c("date", "geography_name", "channel", "exp_name", mainVars[length(mainVars)])
    
    if (summarize) {
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
    }
    
    
    dat$date <- as.Date(dat$date)
    dat <- subset(dat, date <= maxDate)
    
    fwrite(dat, file = file.path(exp_dir, "trajectoriesDat_sub_long.csv"), quote = FALSE)
  } else {
    dat <- fread(file = file.path(exp_dir, "trajectoriesDat_sub_long.csv"))
  }
  
  return(dat)
}


#### ICU timeline
f_icu_timeline <- function(dat, subregions = NULL, selected_channel = "crit_det", facetVar = "geography_name") {
  
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
    customThemeNoFacet +
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

f_get_peak_numbers <- function(dat = simdat, subregions = NULL, selected_channel = "crit_det", facetVar = "geography_name") {
  
  # unique(dat$channel)
  if (is.null(subregions)) subregions <- unique(dat$geography_name)
  
  grpVars <- c(facetVar, "geography_name")
  keepVars <- c(facetVar, 'geography_name', 'icu_available', 'median.value', 'q2.5.value', 'q97.5.value',
                'median.aboveICU', 'q2.5.aboveICU', 'q97.5.aboveICU',
                'median.aboveICU_ratio', 'q2.5.aboveICU_ratio', 'q97.5.aboveICU_ratio',
                'channel', 'exp_name')
  
  
  subdat <- dat %>%
    dplyr::group_by_at(.vars=grpVars) %>%
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
f_describe_ICU_cumul <- function(SAVE=TRUE) {
  
  rebound <- f_get_rebound_values()

  df1 <- f_get_cumul_numbers(dat = simdat, selected_channel = "crit_det_cumul", facetVar = "reopening_multiplier_4") %>%
    left_join(rebound, by = "geography_name") %>%
    filter(reopening_multiplier_4 == fast_rebound) %>%
    dplyr::select(-moderate_rebound, -fast_rebound) %>%
    mutate(rebound = "fast_rebound")

  df2 <- f_get_cumul_numbers(dat = simdat, selected_channel = "crit_det_cumul", facetVar = "reopening_multiplier_4") %>%
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
  
  if(SAVE){
    ggsave(paste0("describe_ICU_cumul_rebound_counterfactual.png"),
           plot = pplot, path = file.path(outdir,"gradual_reopening"), width = 14, height = 8, device = "png"
    )
    
    fwrite(df12tab,  file.path(outdir,"gradual_reopening","describe_ICU_cumul_rebound_counterfactual.csv"))
  }
  

  out <- list(df12, df12tab, pplot)
}

### using f_get_peak_numbers
f_describe_ICU_peak <- function(SAVE=TRUE) {
  
  rebound <- f_get_rebound_values()
  
  df1 <- f_get_peak_numbers(dat = simdat, selected_channel = "crit_det", facetVar = "reopening_multiplier_4") %>%
    left_join(rebound, by = "geography_name") %>%
    filter(reopening_multiplier_4 == fast_rebound) %>%
    dplyr::select(-moderate_rebound, -fast_rebound) %>%
    mutate(rebound = "fast_rebound")
  
  df2 <- f_get_peak_numbers(dat = simdat, selected_channel = "crit_det", facetVar = "reopening_multiplier_4") %>%
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
    arrange(rebound,median.aboveICU_ratio ) %>%
    dplyr::select(rebound, geography_name, everything()) %>%
    as.data.frame()
  
if(SAVE){
  ggsave(paste0("describe_ICU_peak_rebound_counterfactual.png"),
         plot = pplot, path = file.path(outdir,"gradual_reopening"), width = 14, height = 8, device = "png"
  )
  
  fwrite(df12tab,  file.path(outdir,"gradual_reopening","describe_ICU_peak_rebound_counterfactual.csv"))
}
  
  out <- list(df12, df12tab, pplot)
}

