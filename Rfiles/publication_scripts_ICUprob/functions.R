
#### --------------------------------------
####  Functions and setup
#### --------------------------------------


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
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
    #panel.border = element_rect(colour = "black", fill = NA, size = 0.75)
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
    datList[[length(datList) + 1]] <- fread(file.path(exp_dir, csvname)) %>% mutate(exp_name = exp_name,geography_modeled=as.character(geography_modeled)) 
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
  
  if("reopening_multiplier_4" %in% colnames(simdat_reopen)){
    dat <- subset(
      dat,
      Date <= as.Date("2020-12-31") &
        name %in% c("confirmed_covid_icu", "covid_non_icu", "deaths") &
        region %in% c("Region 1", "Region 4", "Region 11") &
        Date > as.Date("2020-06-01") &
        reopening_multiplier_4 < 0.2&
        !( reopening_multiplier_4 %in%  unique(simdat_reopen$reopening_multiplier_4)[c(2,3,4,5)])
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
          filter(date >=as.Date("2020-09-01")) %>%
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

f_load_single_exp <- function(exp_dir, paramvars = NULL, summarize = TRUE, maxDate = as.Date("2020-12-31")) {
  
  if (!(file.exists(file.path(exp_dir, "trajectoriesDat_sub_long.csv")))) {
    fname <- "trajectoriesDat_trim.csv"
    if (!(file.exists(file.path(exp_dir, fname)))) fname <- "trajectoriesDat.csv"

    if (is.null(paramvars)) paramvars <- c("capacity_multiplier", "trigger_delay_days")
    mainVars <- c("time", "startdate", "scen_num", "sample_num", paramvars)
    popDat <- f_region_characteristics()

    datList <- list()
    for (i in c(1:11)) {
      emsVars <- c(paste0("EMS-", i))
      outcomeVars <- c(
        paste0("infected_cumul_", emsVars), paste0("death_det_cumul_", emsVars),
        paste0("crit_det_", emsVars), paste0("crit_det_cumul_", emsVars)
      )

      outVars <- c(mainVars, outcomeVars)
      grpVars <- c("date", "geography_name", "channel", "exp_name", paramvars)

      dat <- fread(file = file.path(exp_dir, fname), select = outVars)
      dat <- dat %>%
        pivot_longer(cols = -c(mainVars)) %>% ##  4.8 sec elapsed
        separate(name, into = c("channel", "geography_name"), sep = "_EMS-") %>% ### 27.72 sec
        dplyr::mutate(exp_name = exp_name) %>%
        left_join(popDat, by = "geography_name") %>%
        dplyr::mutate(date = as.Date(startdate) + time) %>%
        dplyr::select(-startdate, -time)

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
      datList[[length(datList) + 1]] <- dat
      rm(dat)
    }

    dat <- datList %>%  bind_rows()
    fwrite(dat,file = file.path(exp_dir, "trajectoriesDat_sub_long.csv"), quote = FALSE)
    
  } else {
    dat <- fread(file = file.path(exp_dir, "trajectoriesDat_sub_long.csv"))
  }
  
  dat$geography_name <- factor(dat$geography_name, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))

  return(dat)
}


get_fit <- function(df=dat, outputVar ="median.value"){
  
  
  df <- as.data.frame(df)
  df$outputVar <- df[, colnames(df)==outputVar]
  xnew <- seq(0, max(df$reopening_multiplier_4), 0.1)
  
  matdat_list <- list()
  for (region_i in unique(df$region)) {
    print(region_i)
    
    tempdat <- df %>% 
      filter(region == region_i) %>%
      filter(date >= as.Date(simtoday) & date <= as.Date(stopdate) & outcome == "crit_det") %>%
      group_by(region,reopening_multiplier_4) %>%
      filter(outputVar == max(outputVar)) %>%
      filter(outputVar >0) %>%
      mutate(x =reopening_multiplier_4 ,
             y =log(outputVar) )  
    
    # plot(tempdat$y, tempdat$x)
    dfModel <- lm(y~ poly(x,3), data=tempdat )
    
    t_matdat <- expand.grid(x = xnew)
    
    t_matdat <- as.data.frame(cbind(t_matdat, predict(dfModel, newdata = t_matdat, interval = "confidence")))
    t_matdat$region <- region_i
    t_matdat$icu_available <- capacityDat[capacityDat$region==region_i,"icu_available"]
    t_matdat[,outputVar] = exp(t_matdat$fit)
    
    matdat_list[[length(matdat_list) + 1]] <- t_matdat
    
    rm(tempdat, t_matdat)
  }
  
  fitDat <- matdat_list %>% bind_rows()
  return(fitDat)
  
}

#### ICU timeline
f_icu_timeline <- function(dat, subregions = NULL, selected_channel = "crit_det", facetVar = "geography_name") {
  if (!exists("customTheme")) customTheme <- f_getCustomTheme()
  # unique(dat$channel)
  if (is.null(subregions)) subregions <- unique(dat$geography_name)

  dat <- as.data.frame(dat)
  subdat <- subset(dat, geography_name %in% subregions & channel == selected_channel)
  subdat$date <- as.Date(subdat$date)
  subdat[, "facetVar"] <- subdat[, which(colnames(subdat) == facetVar)]

  pplot <- ggplot(data = subdat) +
    background_grid() +
    geom_hline(aes(yintercept = icu_available), col = "red", linetype = "dashed") +
    scale_x_date(date_breaks = "60 days", date_labels = "%b") +
    customTheme +
    theme(legend.position = "none") +
    geom_hline(yintercept = c(Inf)) +
    geom_vline(xintercept = c(Inf)) +
    facet_wrap(~facetVar, scales = "free") +
    labs(title = paste0("subregions: ", subregions), subtitle = "", x = "", y = selected_channel)

  if (!("reopening_multiplier_4" %in% colnames(subdat)) | facetVar == "reopening_multiplier_4") {
    pplot <- pplot + geom_ribbon(aes(x = date, ymin = q2.5.value, ymax = q97.5.value), alpha = 0.3) +
      geom_ribbon(aes(x = date, ymin = q25.value, ymax = q75.value), alpha = 0.3) +
      geom_line(aes(x = date, y = median.value))
  }
  if ("reopening_multiplier_4" %in% colnames(subdat) & facetVar != "reopening_multiplier_4") {
    pplot <- pplot + geom_ribbon(aes(x = date, ymin = q2.5.value, ymax = q97.5.value, fill = as.factor(reopening_multiplier_4)), alpha = 0.3) +
      geom_ribbon(aes(x = date, ymin = q25.value, ymax = q75.value, fill = as.factor(reopening_multiplier_4)), alpha = 0.3) +
      geom_line(aes(x = date, y = median.value, col = as.factor(reopening_multiplier_4)))
  }

  return(pplot)
}

#### Rt timeline
f_rt_timeline <- function(dat, subregions = NULL, selected_channel = "crit_det", facetVar = "geography_name") {
  if (!exists("customTheme")) customTheme <- f_getCustomTheme()
  # unique(dat$channel)
  if (is.null(subregions)) subregions <- unique(dat$geography_name)


  dat <- dat %>% filter(geography_name %in% subregions)
  dat$date <- as.Date(dat$date)
  dat[, "selected_channel"] <- dat[, which(colnames(dat) == selected_channel)]
  dat[, "facetVar"] <- dat[, which(colnames(dat) == facetVar)]

  pplot <- ggplot(data = dat) +
    background_grid() +
    geom_ribbon(aes(x = date, ymin = rt_lower, ymax = rt_upper, fill = as.factor(rebound)), alpha = 0.3) +
    # geom_ribbon(aes(x = date, ymin = q25.value, ymax = q75.value), alpha = 0.3) +
    geom_line(aes(x = date, y = rt_median, col = as.factor(rebound))) +
    geom_hline(aes(yintercept = 1), col = "red", linetype = "dashed") +
    scale_x_date(date_breaks = "60 days", date_labels = "%b") +
    customTheme +
    geom_hline(yintercept = c(Inf)) +
    geom_vline(xintercept = c(Inf)) +
    facet_wrap(~facetVar, scales = "free") +
    labs(title = paste0("subregions: ", subregions), subtitle = "", x = "", y = selected_channel) +
    theme(legend.position = "none")

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

f_get_rebound_values <- function(dat) {

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
    dplyr::rename(moderate_rebound = reopening_multiplier_4)

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
    dplyr::rename(fast_rebound = reopening_multiplier_4) %>%
    left_join(moderate_rebound, by = "geography_name")

  return(dat_out)
}

### using f_get_cumul_numbers
f_describe_ICU_cumul <- function(reboundDat=NULL, SAVE = TRUE, facetVar = "reopening_multiplier_4") {
  
  if(is.null(reboundDat))reboundDat <- f_get_rebound_values(dat = simdat)

  df1 <- f_get_cumul_numbers(dat = simdat, selected_channel = "crit_det_cumul", facetVar = facetVar) %>%
    left_join(reboundDat, by = "geography_name") %>%
    filter(reopening_multiplier_4 == fast_rebound) %>%
    dplyr::select(-moderate_rebound, -fast_rebound) %>%
    mutate(rebound = "fast_rebound")

  df2 <- f_get_cumul_numbers(dat = simdat, selected_channel = "crit_det_cumul", facetVar = facetVar) %>%
    left_join(reboundDat, by = "geography_name") %>%
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
    if (!dir.exists(file.path(exp_dir, "_plots"))) dir.create(file.path(exp_dir, "_plots"))
    if (!dir.exists(file.path(exp_dir, "_csv"))) dir.create(file.path(exp_dir, "_csv"))
    ggsave(paste0("describe_ICU_cumul_rebound.png"),
      plot = pplot, path = file.path(exp_dir, "_plots"), width = 14, height = 8, device = "png"
    )

    fwrite(df12tab, file.path(exp_dir, "_csv", "describe_ICU_cumul_rebound.csv"))
  }


  out <- list(df12, df12tab, pplot)
}

### using f_get_peak_numbers
f_describe_ICU_peak <- function(reboundDat=NULL, SAVE = TRUE, facetVar = "reopening_multiplier_4") {
  
  if(is.null(reboundDat))reboundDat <- f_get_rebound_values(dat = simdat)

  df1 <- f_get_peak_numbers(dat = simdat, selected_channel = "crit_det", facetVar = facetVar) %>%
    left_join(reboundDat, by = "geography_name") %>%
    filter(reopening_multiplier_4 == fast_rebound) %>%
    dplyr::select(-moderate_rebound, -fast_rebound) %>%
    mutate(rebound = "fast_rebound")

  df2 <- f_get_peak_numbers(dat = simdat, selected_channel = "crit_det", facetVar = facetVar) %>%
    left_join(reboundDat, by = "geography_name") %>%
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
    if (!dir.exists(file.path(exp_dir, "_plots"))) dir.create(file.path(exp_dir, "_plots"))
    if (!dir.exists(file.path(exp_dir, "_csv"))) dir.create(file.path(exp_dir, "_csv"))
    ggsave(paste0("describe_ICU_peak_rebound.png"),
      plot = pplot, path = file.path(exp_dir, "_plots"), width = 14, height = 8, device = "png"
    )

    fwrite(df12tab, file.path(exp_dir, "_csv", "describe_ICU_peak_rebound.csv"))
  }

  out <- list(df12, df12tab, pplot)
}



#### --------------------------------------
#### Trigger simulations
#### --------------------------------------


f_describe_peak_and_cumul <- function(dat = simdat, subfolder, SAVE = SAVE) {
  
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




f_scatterplot_errorbars <- function(dflist = list_csvs,
                                    subregions = NULL, 
                                    rollback = "sm4", 
                                    reopen = "50perc", 
                                    exp_name_sub, 
                                    stackLike = FALSE) {
  library(dplyr)
  library(plyr)
  
  if (!exists("customTheme")) customTheme <- f_getCustomTheme()
  # customTheme <- f_getCustomTheme(fontscl = -3)
  
  dflist <- dflist[grep("regreopen", dflist)]
  #dflist <- dflist[grep(rollback, dflist)]
  #dflist <- dflist[grep(reopen, dflist)]
  
  tbl_fread = list()
  for(file in dflist){
    tbl_fread[[length(tbl_fread)+1]] <- fread(file.path(simulation_output, "_overflow_simulations", file)) %>%
      mutate(geography_name=as.character(geography_name)) %>%
      filter(geography_name %in% c(1,4,11))
  }
  tbl_fread <- tbl_fread %>% bind_rows()
  
  
  out <- f_describe_peak_and_cumul(dat = tbl_fread, subfolder = exp_name_sub, SAVE = FALSE)
  tab_peak <- out[[2]]
  
  tab_peak <- tab_peak %>%
    dplyr::mutate(exp_name_split = exp_name) %>%
    separate(exp_name_split, into = c("simdate", "locale", "reopen", "delay", "rollback"), sep = "_") %>%
    dplyr::mutate(
      reopen = gsub("regreopen", "", reopen),
      delay = gsub("daysdelay", " days", delay)
    )
  
  
  if (reopen == "50perc") selectedCols <- c("#c6dbef", "#6baed6", "#2171b5")
  if (reopen == "100perc") selectedCols <- c("#fee0d2", "#fb6a4a", "#cb181d")
  
  
  suppressWarnings(capacity_threshold <- tab_peak %>%
                     dplyr::filter(median.aboveICU >= 0) %>%
                     dplyr::filter(capacity_multiplier == min(capacity_multiplier)) %>%
                     dplyr::select(capacity_multiplier) %>%
                     unique() %>%
                     as.numeric())
  
  if (is.na(capacity_threshold)) capacity_threshold <- 1
  
  tab_peak$capacity_multiplier_fct <- factor(tab_peak$capacity_multiplier, levels= rev(unique(sort(tab_peak$capacity_multiplier))),
                                             labels = round(rev(unique(sort(tab_peak$capacity_multiplier)))* 100, 0))
  tab_peak$region <- factor(  tab_peak$geography_name , levels=c(1:11), labels=c(1:11))
  
  pplot_peak <- ggplot(data = subset(tab_peak, delay == "3 days")) +
    geom_hline(yintercept = Inf) +
    geom_vline(xintercept = Inf) +
    geom_hline(yintercept = 0, linetype = "solid", size = 0.5) +
    geom_errorbar(aes(x = capacity_multiplier_fct, ymin = q2.5.aboveICU, ymax = q97.5.aboveICU,col = region, 
                      group = interaction(region,delay, capacity_multiplier)), width = 0 ) +
    geom_line(aes(x = capacity_multiplier_fct, y = median.aboveICU, col = region, group = interaction(delay,geography_name))) +
    geom_point(aes(x = capacity_multiplier_fct, y = median.aboveICU, fill = region, group = interaction(delay, capacity_multiplier)),shape=21,size=2) +
    
    labs(
      subtitle = "", y = expression(italic(ICU[predicted_peak] - ICU[capacity])),
      # caption="Difference between predicted\npeak ICU cases and ICU availability",
      x = "Trigger threshold"
    ) +
    facet_wrap(~ rollback + reopen, scales="free_x") +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") +
    theme(panel.grid.major.y = element_line(colour = "grey", size = 0.5))+
    customTheme
  
  out <- list(tab_peak, pplot_peak)
  return(out)
}





f_stacked_barplot_errorbars <- function(dflist = list_csvs,
                                        subregions = NULL, 
                                        rollback = "sm4", 
                                        reopen = "50perc", 
                                        exp_name_sub, 
                                        stackLike = FALSE) {
  library(dplyr)
  library(plyr)

  if (!exists("customTheme")) customTheme <- f_getCustomTheme()
  # customTheme <- f_getCustomTheme(fontscl = -3)

  dflist <- dflist[grep("regreopen", dflist)]
  dflist <- dflist[grep(rollback, dflist)]
  dflist <- dflist[grep(reopen, dflist)]

  tbl_fread = list()
  for(file in dflist){
    tbl_fread[[length(tbl_fread)+1]] <- fread(file.path(simulation_output, "_overflow_simulations", file)) %>%
      mutate(geography_name=as.character(geography_name)) %>%
      filter(geography_name %in% subregions)
  }
  tbl_fread <- tbl_fread %>% bind_rows()
  

  out <- f_describe_peak_and_cumul(dat = tbl_fread, subfolder = exp_name_sub, SAVE = FALSE)
  tab_peak <- out[[2]]

  tab_peak <- tab_peak %>%
    dplyr::mutate(exp_name_split = exp_name) %>%
    separate(exp_name_split, into = c("simdate", "locale", "reopen", "delay", "rollback"), sep = "_") %>%
    dplyr::mutate(
      reopen = gsub("regreopen", "", reopen),
      delay = gsub("daysdelay", " days", delay)
    )


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

  pplot_peak <- ggplot(data = subset(tab_peak, delay == "3 days")) +
    geom_hline(yintercept = Inf) +
    geom_vline(xintercept = Inf) +
    geom_bar(aes(x = capacity_multiplier_fct, y = median.aboveICU, fill = delay, group = interaction(delay, capacity_multiplier)),
      col = "azure4", position = position_dodge(width = 0.5), stat = "identity"
    ) +
    geom_errorbar(aes(x = capacity_multiplier_fct, ymin = q2.5.aboveICU, ymax = q97.5.aboveICU, group = interaction(delay, capacity_multiplier)),
      col = "black", position = position_dodge(width = 0.5), width = 0.3, stat = "identity"
    ) +
    labs(
      subtitle = "", y = expression(italic(ICU[predicted_peak] - ICU[capacity])),
      # caption="Difference between predicted\npeak ICU cases and ICU availability",
      x = "Trigger threshold"
    ) +
    facet_wrap(~ rollback + reopen) +
    customTheme +
    scale_fill_manual(values = selectedCols[3]) +
    scale_color_manual(values = selectedCols[3]) +
    geom_hline(yintercept = 0, linetype = "solid", size = 0.75) +
    theme(panel.grid.major.y = element_line(colour = "grey", size = 0.75))

  # panel.grid.minor.y = element_line(colour="grey", size=0.5)
  # y=expression(Delta*italic(ICU[capacity])),
  out <- list(tab_peak, pplot_peak)
  return(out)
}




f_stacked_barplot <- function(dflist = list_csvs, 
                              subregions = NULL, 
                              rollback = "sm4", 
                              reopen = "50perc",
                              exp_name_sub, 
                              stackLike = FALSE) {
  library(dplyr)
  library(plyr)

  if (!exists("customTheme")) customTheme <- f_getCustomTheme()

  dflist <- dflist[grep("regreopen", dflist)]
  dflist <- dflist[grep(rollback, dflist)]
  dflist <- dflist[grep(reopen, dflist)]
  
  tbl_fread = list()
  for(file in dflist){
    tbl_fread[[length(tbl_fread)+1]] <- fread(file.path(simulation_output, "_overflow_simulations", file)) %>%
      mutate(geography_name=as.character(geography_name)) %>%
      filter(geography_name %in% subregions)
  }
  tbl_fread <- tbl_fread %>% bind_rows()

  out <- f_describe_peak_and_cumul(dat = tbl_fread, subfolder = exp_name_sub, SAVE = FALSE)
  tab_peak <- out[[2]]

  tab_peak <- tab_peak %>%
    dplyr::mutate(exp_name_split = exp_name) %>%
    separate(exp_name_split, into = c("simdate", "locale", "reopen", "delay", "rollback"), sep = "_") %>%
    dplyr::mutate(
      reopen = gsub("regreopen", "", reopen),
      delay = gsub("daysdelay", " days", delay)
    ) 


  y_up <- round_any(max(abs(c(min(tab_peak$median.aboveICU),max(tab_peak$median.aboveICU)))), 10, f = ceiling)
  y_lu <- -1* y_up
  if (y_up < 0) y_up <- 0
  y_step <- round_any(max(abs(y_lu), abs(y_up)) / 3, 10, f = ceiling)
  labels_and_breaks <- unique(sort(c(-1*c( seq(0, y_up, y_step),y_up ),c( seq(0, y_up, y_step),y_up ))))


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
  tab_peak <- tab_peak %>% filter(!is.na(rollback) & !is.na(reopen))
  
  pplot_peak <- ggplot(data = tab_peak) +
    geom_hline(yintercept = Inf) +
    geom_vline(xintercept = Inf) +
    geom_bar(
      data = subset(tab_peak), aes(x = capacity_multiplier_fct, y = median.aboveICU, fill = delay, group = interaction(delay, capacity_multiplier)),
      col = "azure4", position = position_dodge(width = 0.5), stat = "identity"
    ) +
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
      #scale_y_continuous(lim = c(-400, 500), labels = seq(-400, 500, 150), breaks = seq(-400, 500, 150)) +
      scale_y_continuous(lim = c(y_lu, y_up), labels = labels_and_breaks, breaks = labels_and_breaks) +
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


f_ICU_tolerance_plot <- function(dat = ICU_threshold_future, reg = NULL, riskTolerance = 0.9, byRollback = TRUE) {
  if (is.null(reg)) reg <- unique(dat$geography_name)[!(unique(dat$geography_name) %in% "illinois")]

  if (byRollback == TRUE) {
    pplot <- ggplot(data = subset(dat, geography_name %in% c(reg) & risk_tolerance < riskTolerance)) +
      geom_smooth(aes(x = risk_tolerance, y = capacity_multiplier, col = grpVar, fill = grpVar), size = 1, se = T) +
      scale_color_manual(values = custom_cols) +
      scale_fill_manual(values = custom_cols) +
      facet_wrap(~reopen_label) +
      customTheme +
      background_grid() +
      labs(
        x = "Probability of ICU overflow (risk tolerance)",
        y = "Trigger threshold\n(% of ICU COVID availability)",
        color = "delay",
        fill = "delay"
      )
  }

  if (byRollback == FALSE) {
    pplot <- ggplot(data = subset(dat, geography_name %in% c(reg) & risk_tolerance < riskTolerance)) +
      geom_smooth(aes(x = risk_tolerance, y = capacity_multiplier, col = reopen, fill = reopen, linetype = rollback), size = 1, se = T) +
      scale_color_manual(values = custom_cols_reopen) +
      scale_fill_manual(values = custom_cols_reopen) +
      facet_wrap(~reopen_label) +
      customTheme +
      background_grid() +
      labs(
        x = "Probability of ICU overflow (risk tolerance)",
        y = "Trigger threshold\n(% of ICU COVID availability)",
        linetype = "rollback"
      ) +
      guides(colour = FALSE, fill = FALSE)
  }


  return(pplot)
}


f_get_probabilities <- function(exp_dir, SAVE = TRUE) {
  simdat <- load_sim_data(exp_dir)
  simdat <- simdat %>% left_join(load_new_capacity(filedate = '20200915'), by = "geography_name")
  simdat <- simdat %>% left_join(load_population(filedate = '20200915'), by = "geography_name")

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

  if (SAVE) f_save_plot(pplot = pplot, plot_name = plot_name, plot_dir = file.path(exp_dir), width = width, height = height)

  return(pplot)
}


f_custom_prob_plot2 <- function(dat, subregions = NULL, exp_dir, plot_name, width = 15, height = 10, SAVE = TRUE) {
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

  if (SAVE) f_save_plot(pplot = pplot, plot_name = plot_name, plot_dir = file.path(exp_dir), width = width, height = height)
  return(pplot)
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
