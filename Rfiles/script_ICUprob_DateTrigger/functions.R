
#### --------------------------------------
####  Functions and setup
#### --------------------------------------

custom_date_breaks <- c(
  as.Date("2020-08-01"), as.Date("2020-09-01"),
  as.Date("2020-10-01"), as.Date("2020-11-01"),
  as.Date("2020-12-01"), as.Date("2021-01-01"),
  as.Date("2021-02-01"), as.Date("2021-03-01")
)

custom_date_breaks_JanOct <- c(
  as.Date("2020-01-01"), as.Date("2020-02-01"), as.Date("2020-03-01"),
  as.Date("2020-04-01"), as.Date("2020-05-01"), as.Date("2020-06-01"),
  as.Date("2020-07-01"), as.Date("2020-08-01"), as.Date("2020-09-01"),
  as.Date("2020-10-01")
)

capacity_multiplier2_cols <- c("#d0d1e6", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0")
delay_cols <- c("#d0d1e6", "#7fcdbb", "#1d91c0")
rollback_cols <- c("#d0d1e6", "#1d91c0")
custom_cols <- c(
  "0days-50perc" = "#c6dbef", "3days-50perc" = "#6baed6", "7days-50perc" = "#2171b5",
  "0days-100perc" = "#fee0d2", "3days-100perc" = "#fb6a4a", "7days-100perc" = "#cb181d"
)
custom_cols_delay_50perc <- c(
  "0 days" = "#c6dbef", "3 days" = "#6baed6", "7 days" = "#2171b5"
)
custom_cols_delay_100perc <- c(
  "0 days" = "#fee0d2", "3 days" = "#fb6a4a", "7 days" = "#cb181d"
)
custom_cols_reopen <- c(
  "100perc" = "#cb181d", "50perc" = "#2171b5"
)

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
    axis.ticks = element_line() ,
    #axis.line.x = element_line(),
   # axis.line.y = element_line(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )
  return(customTheme)
}

#### --------------------------------------
#### Region characteristics
#### --------------------------------------

f_initial_and_timevarying_Ki <- function(exp_dir, param = NULL) {
  library(tidyverse)
  library(data.table)

  keepVars <- paste0("Ki_EMS_", c(1:11))
  initialKi <- fread(file.path(exp_dir, "sampled_parameters.csv"), select = keepVars) %>%
    unique() %>%
    melt() %>%
    separate(variable, into = c("del", "region"), sep = "Ki_EMS_") %>%
    rename(Ki_initial = value) %>%
    dplyr::select(-del)

  if (is.null(param)) param <- c("reopening_multiplier_4", "capacity_multiplier", "trigger_delay_days")
  keepVars <- c("time", "startdate", "scen_num", "sample_num", param, paste0("Ki_t_EMS-", c(1:11)))
  timevaryingKi <- fread(file.path(exp_dir, "trajectoriesDat.csv"), select = keepVars) %>%
    filter(as.numeric(time) < 365) %>%
    unique() %>%
    pivot_longer(cols = -c("time", "startdate", "scen_num", "sample_num", param)) %>%
    separate(name, into = c("del", "region"), sep = "Ki_t_EMS-") %>%
    rename(Ki_t = value) %>%
    mutate(date = as.Date(startdate) + time) %>%
    dplyr::select(-del, -time, -startdate)


  grpVars <- c("date", param, "region")
  Ki_dat <- timevaryingKi %>%
    left_join(initialKi, by = "region") %>%
    mutate(Ki_rebound = (Ki_t / Ki_initial)) %>%
    ungroup() %>%
    dplyr::group_by_at(.vars = grpVars) %>%
    summarize(
      Ki_t = median(Ki_t),
      Ki_initial = median(Ki_initial),
      Ki_rebound = median(Ki_rebound)
    )

  Ki_dat$region <- factor(Ki_dat$region, levels = c(1:11), labels = c(1:11))
  return(Ki_dat)
}


f_region_characteristics <- function(SAVE = FALSE) {
  if (SAVE == FALSE & file.exists(file.path(outdir, "region_characteristics.csv"))) {
    dat <- fread(file.path(outdir, "region_characteristics.csv"))
    print(paste0("File loaded from ", outdir, "region_characteristics.csv"))
  }

  if (!(file.exists(file.path(outdir, "region_characteristics.csv")))) {
    popDat <- load_population()
    capacityDat <- load_new_capacity( filedate = '20200915')

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

f_mergevars <- function(datX,datY){
  mergevars <- colnames(datX)[colnames(datX) %in% colnames(datY)]
  return(mergevars)
}


f_addVar <- function(datX,datY, allX=TRUE){
  
  nrowB <- dim(datX)[1]
  mergevars <- f_mergevars(datX, datY)
  
  out <- dplyr::left_join(datX, datY, by = mergevars, all.x = TRUE)
  
  if(dim(out)[1] !=  nrowB)warning("Number of rows do not match")
  message(paste0("Message: x nrow= ", dim(out)[1]," and y nrow=", dim(datX)[1], "\n Number of variables added: ",dim(out)[2]- dim(datX)[2] ))
  
  
  return(out)
  
}

f_merge_Rdata <- function(exp_dir, paramvars = NULL) {
  region_names <- c(paste0("EMS-", c(1:11)))
  outcomevars <- c(
    paste0("hosp_det_", region_names),
    paste0("hospitalized_", region_names),
    paste0("crit_det_", region_names),
    paste0("critical_", region_names),
    paste0("deaths_", region_names)
  )

  region_names <- paste0("EMS-", c(1:11))
  outcomevars2 <- c(
    paste0("Ki_t_", region_names)
  )

  if (is.null(paramvars)) paramvars <- c("reopening_multiplier_4")
  keepvars <- c("time", "startdate", "scen_num", paramvars, outcomevars)

  if (!(file.exists(file.path(exp_dir, "trajectoriesDat.csv")))) {
    combineFromR <- TRUE
    if (combineFromR) {
      count <- 0
      for (i in c(1:11)) {
        count <- count + 1
        load(file.path(exp_dir, paste0("trajectoriesDat_region_", i, ".RData")))
        if (count == 1) trajectoriesDat <- subdat
        if (count > 1) trajectoriesDat <- left_join(trajectoriesDat, subdat, by = c("time", "startdate", "scen_num", "sample_num", paramvars))
        rm(subdat)
        fwrite(trajectoriesDat, file = file.path(exp_dir, "trajectoriesDat.csv"), quote = FALSE)
      }
    }
  }
  trajectoriesDat <- fread(file.path(exp_dir, "trajectoriesDat.csv"), select = c(keepvars))
}

f_combine_Rdata <- function(Rdata_files) {
  library(miceadds)
  library(data.table)

  datList <- list()
  for (Rdata_file in Rdata_files) {
    load.Rdata(filename = file.path(Rdata_file), objname = "tempdat")
    tempdat$filesource <- Rdata_file
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


f_combineDat <- function(sim_dir,exp_names,csvname){
  
  datList <- list()
  for (exp_name in exp_names) {
    exp_dir <- file.path(sim_dir, exp_name)
    if (!file.exists(file.path(exp_dir, csvname))) next
    dat <- fread(file.path(exp_dir, csvname)) %>% mutate(exp_name = exp_name)
    if("geography_modeled" %in% colnames(dat)){
      dat <- dat %>%mutate(geography_modeled=as.character(geography_modeled)) } 
    datList[[length(datList) + 1]] <- dat
      
  }
  
  if(length(datList)==0)warning("List is empty")
  dat <- datList %>% bind_rows()
  
  return(dat)
}

f_remove_legend <- function(p) {
  p <- p + theme(legend.position = "none")
  return(p)
}

f_save_plot <- function(pplot, plot_name, plot_dir, width = 14, height = 8) {
  ggsave(paste0(plot_name, ".png"), plot = pplot, path = plot_dir, 
         width = width, height = height, device = "png")
  if(!dir.exists(file.path(plot_dir,"pdf"))){dir.create(file.path(plot_dir, "pdf"))}
  ggsave(paste0(plot_name, ".pdf"), plot = pplot, path = file.path(plot_dir, "pdf"),
         width = width, height = height, device = "pdf",useDingbats=FALSE)
}


f_simdat <- function(dat, subregions=c(1, 4, 11), grpVars=NULL,stopdate=as.Date("2020-08-01")){
  
  if(is.null(grpVars))grpVars <- c("date", "ems", "scenario_name", "geography_modeled")
  
  dat <- dat %>%
    filter(geography_modeled %in% c("covidregion_1", "covidregion_4", "covidregion_11")) %>%
    pivot_longer(cols = -grpVars) %>%
    mutate(
      name = gsub("_m", "__m", name),
      name = gsub("_9", "__9", name),
      name = gsub("_5", "__5", name)
    ) %>%
    separate(name, into = c("channel", "stat"), sep = "__") %>%
    filter(channel %in% c("hosp_det", "crit_det", "new_detected_deaths", "new_deaths")) %>%
    pivot_wider(names_from = "channel", values_from = "value") %>%
    rename(
      covid_non_icu = hosp_det,
      confirmed_covid_icu = crit_det,
      death_det = new_detected_deaths,
      deaths = new_deaths
    ) %>%
    pivot_longer(cols = -c(grpVars, "stat")) %>%
    mutate(source = "sim") %>%
    pivot_wider(names_from = "stat", values_from = "value") %>%
    dplyr::rename(
      median.val = median,
      q25.val = `50CI_lower`,
      q75.val = `50CI_upper`,
      q2.5.val = `95CI_lower`,
      q97.5.val = `95CI_upper`
    ) %>%
    mutate(Date = as.Date(date))
  dat$region <- factor(dat$ems, levels = paste0("EMS-",subregions), labels = paste0("Region ", subregions))
  
  dat <- subset(
    dat,
    Date > as.Date("2020-03-01") &
      Date <= as.Date("2020-12-31") &
      name %in% c("confirmed_covid_icu", "covid_non_icu", "deaths") 
  )
  
  if("reopening_multiplier_4" %in% colnames(dat)){
    dat <- subset(
      dat,
      Date <= as.Date("2020-12-31") &
        name %in% c("confirmed_covid_icu", "covid_non_icu", "deaths") &
        region %in% c("Region 1", "Region 4", "Region 11") &
        Date > as.Date("2020-06-01") &
        reopening_multiplier_4 < 0.2&
        !( reopening_multiplier_4 %in%  unique(dat$reopening_multiplier_4)[c(2,3,4,5)])
    )
    #unique(simdatSub_reopen$reopening_multiplier_4)
  }
  
  return(dat)
}

f_load_ref_data <- function(subregions=c(1, 4, 11),startdate=as.Date("2020-03-01"), stopdate=as.Date("2020-08-01")){
  LLdat <- f_loadData(data_path) %>%
    mutate(
      Date = as.Date(Date),
      week = week(Date),
      month = month(Date)
    ) %>%
    dplyr::select(Date, region, LL_admissions, LL_deaths) %>%
    dplyr::rename(
      deaths = LL_deaths,
      covid_non_icu = LL_admissions
    ) %>%
    pivot_longer(cols = -c("Date", "region")) %>%
    mutate(source = "LL")
  
  emresource <- f_loadData(data_path) %>%
    mutate(
      Date = as.Date(Date),
      week = week(Date),
      month = month(Date)
    ) %>%
    dplyr::rename(deaths = confirmed_covid_deaths_prev_24h) %>%
    dplyr::select(Date, region, confirmed_covid_icu, covid_non_icu, deaths) %>%
    pivot_longer(cols = -c("Date", "region")) %>%
    dplyr::mutate(source = "EMResource") %>%
    rbind(LLdat)
  
  pplot7dAvr <- emresource %>%
    dplyr::group_by(Date, name, source, region) %>%
    dplyr::summarize(
      value = sum(value, na.rm = TRUE),
    ) %>%
    dplyr::group_by(name, source, region) %>%
    arrange(name, Date, source) %>%
    mutate(value7 = zoo::rollmean(value, k = 7, fill = NA)) %>%
    ungroup()
  
  pplot7dAvrSub <- subset(
    pplot7dAvr,
    Date > startdate &
      Date <= stopdate &
      name %in% c("confirmed_covid_icu", "covid_non_icu", "deaths") &
      region %in% subregions
  ) %>%
    filter((name == "confirmed_covid_icu" & source == "EMResource") |
             (name == "covid_non_icu" & source == "EMResource") |
             (name == "deaths" & source == "LL"))
  
  pplot7dAvrSub$region <- factor(pplot7dAvrSub$region, levels =subregions, labels = paste0("Region ", subregions))
  return(pplot7dAvrSub)
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
f_load_trajectories <- function(sim_dir, exp_name, region_nr){
  
  fname <- paste0("trajectoriesDat_region_",region_nr,".csv")
  gsubname <- paste0("_EMS-",region_nr)
  
  dat <- fread(file.path(sim_dir,exp_name,fname)) %>% 
          rename_with( ~ gsub(gsubname, "", .x)) %>%
          mutate(startdate=as.Date(startdate),
                 date=as.Date(startdate+time)) %>%
          dplyr::select(-time, -startdate) %>%
          dplyr::group_by(capacity_multiplier, date) %>%
          add_tally() %>%
          dplyr::group_by(sample_num, date) %>%
          dplyr::rename(nsamples=n) %>%
          add_tally() %>%
          rename(nmultiplier_per_sample=n)
  

  dat$capacity_multiplier_fct <- round(dat$capacity_multiplier * 100, 0)
  fct_labels <-  sort(unique(dat$capacity_multiplier_fct))
  dat$capacity_multiplier_fct <- factor(dat$capacity_multiplier_fct,
                                        levels = c(fct_labels,"counterfactual"),
                                        labels = c(fct_labels,"counterfactual")
  )
  dat$capacity_multiplier_fct2 <- factor(dat$capacity_multiplier_fct,
                                         levels = rev(c(fct_labels,"counterfactual")),
                                         labels = rev(c(fct_labels,"counter\nfactual"))
  )

  dat$geography_modeled <- region_nr
  dat$region <- factor(dat$geography_modeled, levels=c(1,4,11), labels=paste0("Region ",c(1,4,11)))
  return(dat)
}

f_get_scenVars <- function(dat){
  dat$scen_name <- gsub(paste0(simdate, "_IL_regreopen"), "", dat$exp_name)
  dat <- dat %>% separate(scen_name, into = c("reopen", "Dates", "rollback"), sep = "_")
  dat$rollback[is.na(dat$rollback)] <- "counterfactual"
  
  dat$reopen_fct <- factor(dat$reopen, 
                           levels=c("100perc","50perc"),
                           labels=c("High\ntransmission\nincrease",
                                    "Low\ntransmission\nincrese"))
  
  dat$reopen_fct2 <- factor(dat$reopen, 
                            levels=c("100perc","50perc"),
                            labels=c("High","Low"))
  
  dat$rollback_fct <- factor(dat$rollback, 
                             levels=c("pr8","pr6", "pr4","pr2"),
                             labels=rev(seq(20,80,20)))
  
  return(dat)
}






#-------------------------------
## Functions for Fig 7
#-------------------------------

f_calculate_prob <- function( dfDat) {
  
  dat_prob <- dfDat %>%
    group_by(exp_name, region, time_of_trigger, scen_num) %>%
    filter(crit_det == max(crit_det)) %>%
    filter(date == min(date)) %>%
    mutate(n_above = ifelse(crit_det >= avg_resource_available, 1, 0)) %>%
    add_tally(name = "n_all") %>%
    group_by(exp_name, region, time_of_trigger) %>%
    summarize(
      n_all = sum(n_all),
      n_above = sum(above_yn)
    ) %>%
    mutate(prob = n_above / n_all) %>%
    f_get_scenVars() %>%
    ungroup()
  
  return(dat_prob)
}


f_plot_prob <- function() {
  
  dat_prob$date_of_trigger  =as.numeric( as.character(dat_prob$time_of_trigger)) + as.Date("2020-01-01")
  
  pplot <- ggplot(data = dat_prob) +
    geom_line(aes(
      x = date_of_trigger, y = prob * 100,
      col = reopen,
      alpha = rollback_fct,
      group = interaction(reopen, rollback_fct)
    ), size = 1) +
    facet_wrap(reopen ~ region, scales = "free") +
    scale_y_continuous(
      lim = c(0, 102), expand = c(0, 0),
      breaks = seq(0, 100, 20),
      minor_breaks = seq(0, 100, 10)
    ) +
    scale_color_manual(values = c(TwoCols_seq)) +
    scale_fill_manual(values = c(TwoCols_seq)) +
    scale_alpha_manual(values = c(1, 0.75, 0.5, 0.2, 0.1)) +
    customTheme +
    theme(
      panel.spacing = unit(2, "lines"),
      # legend.position = "None",
      panel.grid.major = element_line(),
      panel.grid.minor = element_line(size = 0.75)
    ) +
    labs(
      y = "Probability of ICU overflow (%)",
      x = "Trigger date (2 weekly interval)",
      color = "Transmission\nincrease", fill = "Transmission\nincrease",
      alpha = "Mitigation strengths"
    )+
    scale_x_date(breaks = c(as.Date("2020-10-01"), as.Date("2020-10-08"), as.Date("2020-10-15"), 
                            as.Date("2020-10-22"), as.Date("2020-10-29"), as.Date("2020-11-05"),
                            as.Date("2020-11-12"), 
                            as.Date("2020-11-19"), as.Date("2020-11-26"), as.Date("2020-12-03"), 
                            as.Date("2020-12-10"), as.Date("2020-12-17"), as.Date("2020-12-24")),
                 date_labels = "%d\n%b")
  
  return(pplot)
}



f_timeline_simple <- function(){
  
  if(sum(grep("50perc", exp_name))>0) subtitle = "Low transmission increase\n"
  if(sum(grep("100perc", exp_name))>0) subtitle = "High transmission increase\n"
  every2ndweek <- sort(unique(dat$time_of_trigger))[seq(1,length(unique(dat$time_of_trigger)),2)]
  
  datAggr <- dat %>%
    group_by(exp_name, region,date, time_of_trigger,avg_resource_available) %>%
    filter(time_of_trigger %in% every2ndweek) %>%
    summarize(crit_det_mean=mean(crit_det),
              crit_det_50lower = quantile(crit_det, probs = 0.25, na.rm = TRUE),
              crit_det_50upper = quantile(crit_det, probs = 0.25, na.rm = TRUE),
              crit_det_95lower = quantile(crit_det, probs = 0.025, na.rm = TRUE),
              crit_det_95upper = quantile(crit_det, probs = 0.975, na.rm = TRUE),
              trigger_threshold_mean=mean(trigger_threshold),
              trigger_threshold_50lower = quantile(trigger_threshold, probs = 0.25, na.rm = TRUE),
              trigger_threshold_50upper = quantile(trigger_threshold, probs = 0.25, na.rm = TRUE),
              trigger_threshold_95lower = quantile(trigger_threshold, probs = 0.025, na.rm = TRUE),
              trigger_threshold_95upper = quantile(trigger_threshold, probs = 0.975, na.rm = TRUE))
  
  datAggr$date_of_trigger <- as.Date("2020-01-01") +   datAggr$time_of_trigger 
  datAggr_hline <- datAggr %>% 
                  group_by(region, exp_name,time_of_trigger,date_of_trigger,avg_resource_available) %>% 
                  filter(date >= as.Date("2020-10-01")) %>%
                  filter(as.character(date)==as.character(date_of_trigger)) %>%
                  dplyr::select(region,date, exp_name,time_of_trigger, date_of_trigger,avg_resource_available,crit_det_mean) %>%
                  mutate(trigger_threshold = crit_det_mean/avg_resource_available )
  
  
  datAggr_hline$label <- paste0( datAggr_hline$date_of_trigger , "\n (mean= ", round(datAggr_hline$trigger_threshold*100,0),  "%)" )
  datAggr <- f_addVar(datAggr, datAggr_hline[,c("trigger_threshold","time_of_trigger","label")])
  
  
  p0 <- ggplot(data=subset(datAggr))  + 
       geom_ribbon(aes(x=date, ymin=crit_det_95lower,  ymax=crit_det_95upper, fill=time_of_trigger, group=time_of_trigger), alpha=0.2)+
      geom_ribbon(aes(x=date, ymin=crit_det_50lower,  ymax=crit_det_50upper, fill=time_of_trigger, group=time_of_trigger), alpha=0.4)+
      geom_line(aes(x=date, y=crit_det_mean, col=time_of_trigger, group=time_of_trigger))+
    geom_hline(aes(yintercept=avg_resource_available), col=capacity_col) +
      geom_hline(data=datAggr_hline, aes(yintercept=crit_det_mean,col=time_of_trigger)) +
    scale_fill_viridis_c(option="C", direction=-1, begin =0, end=0.8)+
    scale_color_viridis_c(option="C", direction=-1, begin =0,  end=0.8) +
    scale_y_continuous(expand=c(0,0)) +
       facet_wrap( ~ exp_name)+
    theme(legend.position = "none")+
    labs(x="", y="ICU occupancy")
  
  p1 <- ggplot(data=subset(datAggr,time_of_trigger==max(time_of_trigger) & date >="2020-10-02"))  + 
    geom_ribbon(aes(x=date, ymin=crit_det_95lower,  ymax=crit_det_95upper,  group=time_of_trigger), alpha=0.2)+
    geom_ribbon(aes(x=date, ymin=crit_det_50lower,  ymax=crit_det_50upper,  group=time_of_trigger), alpha=0.4)+
    geom_line(aes(x=date, y=crit_det_mean, group=time_of_trigger))+
    geom_hline(aes(yintercept=avg_resource_available), col=capacity_col) +
    geom_hline(data=datAggr_hline, aes(yintercept=crit_det_mean,col=time_of_trigger)) +
    geom_text(data=datAggr_hline, aes(x=as.Date("2020-10-05"), y=avg_resource_available+25),col=capacity_col, label="ICU capacity") +
    scale_fill_viridis_c(option="C", direction=-1, begin =0, end=0.8)+
    scale_color_viridis_c(option="C", direction=-1, begin =0,  end=0.8) +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_date(breaks = custom_date_breaks, date_labels = "%d\n%b") +
    theme(legend.position = "none") +
    customTheme+
    labs(title=paste0("Region ", reg_nr),subtitle=subtitle, x="", y="ICU occupancy")
  
  
  p2 <- ggplot(data=subset(datAggr, date >="2020-10-02"))  + 
    geom_ribbon(aes(x=date, ymin=crit_det_95lower,  ymax=crit_det_95upper, fill=time_of_trigger,  group=time_of_trigger), alpha=0.2)+
    geom_ribbon(aes(x=date, ymin=crit_det_50lower,  ymax=crit_det_50upper, fill=time_of_trigger,  group=time_of_trigger), alpha=0.4)+
    geom_line(aes(x=date, y=crit_det_mean,  col=time_of_trigger, group=time_of_trigger))+
    geom_hline(aes(yintercept=avg_resource_available), col=capacity_col) +
    geom_vline(data=datAggr_hline, aes(xintercept=date_of_trigger), linetype='dashed') + 
    geom_hline(data=datAggr_hline, aes(yintercept=crit_det_mean,col=time_of_trigger)) +
    scale_fill_viridis_c(option="C", direction=-1, begin =0, end=0.8)+
    scale_color_viridis_c(option="C", direction=-1, begin =0,  end=0.8) +
    scale_x_date(breaks = custom_date_breaks, date_labels = "%d\n%b") +
    scale_y_continuous(expand=c(0,0)) +
    facet_wrap( ~ label, nrow=1)+
    theme(legend.position = "none",
          panel.spacing = unit(1.5, "lines"),)+
    customTheme +
    labs(subtitle="Date mitigation implented and corresponding ICU occupancy (%)\n", x="", y="ICU occupancy")
  
  
  pplot <- plot_grid(p1, p2, nrow=2, rel_heights =  c(1,0.7))
  
  if(!dir.exists( file.path(sim_dir, exp_name, "ICU_timeline_plots")))dir.create( file.path(sim_dir, exp_name, "ICU_timeline_plots"))
  f_save_plot(
    plot_name = paste0("ICU_prob_trajectories_", reg_nr), pplot = pplot,
    plot_dir = file.path(sim_dir, exp_name, "ICU_timeline_plots"), width = 12, height = 8
  )
  
  
  
}

