### =============================================================================
###  Comparing forward prediction to data for varying simulation iterations
### =============================================================================

## Load package
require(tidyverse)
require(cowplot)
require(scales)
require(lubridate)

Location <- "LOCAL"
source("load_paths.R")
source("processing_helpers.R")

theme_set(theme_minimal())


customThemeNoFacet <- theme(
  strip.text.x = element_text(size = 20, face = "bold"),
  strip.text.y= element_text(size = 20, face = "bold"),
  strip.background = element_blank(),
  plot.title = element_text(size = 22, vjust = -1, hjust = 0),
  plot.subtitle = element_text(size = 18),
  plot.caption = element_text(size = 16),
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 20),
  axis.title.x = element_text(size =18),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 18),
  axis.text.y = element_text(size = 16)
)

### ----------------------------------------
## Define functions

### Load data
f_loadData <- function() {
  emresource <- read.csv("C:/Users/mrm9534/Box/NU-malaria-team/data/covid_IDPH/Corona virus reports/emresource_by_region.csv") %>%
    dplyr::mutate(
      date_of_extract = as.Date(date_of_extract),
      suspected_and_confirmed_covid_icu = suspected_covid_icu + confirmed_covid_icu
    ) %>%
    dplyr::rename(
      Date = date_of_extract,
      region = covid_region,
    ) %>%
    f_addRestoreRegion() %>%
    mutate(restore_region = tolower(restore_region)) %>%
    filter(!is.na(restore_region)) %>%
    dplyr::select(
      Date, restore_region, region, suspected_and_confirmed_covid_icu,
      confirmed_covid_deaths_prev_24h, confirmed_covid_icu, covid_non_icu
    )


  ref_df <- read.csv(file.path("C:/Users/mrm9534/Box/NU-malaria-team/data/covid_IDPH/Cleaned Data", "200811_jg_aggregated_covidregion.csv"))

  ref_df <- ref_df %>%
    dplyr::rename(
      Date = date,
      region = covid_region,
      LL_deaths = deaths,
      LL_cases = cases,
      LL_admissions = admissions
    ) %>%
    f_addRestoreRegion() %>%
    mutate(restore_region = tolower(restore_region))


  ref_df$Date <- as.Date(ref_df$Date)
  emresource$Date <- as.Date(emresource$Date)


  out <- left_join(emresource, ref_df, by = c("Date", "restore_region", "region"))

  return(out)
}

### Load simulations

f_combineSimulations2 <- function(NUCivisDir = file.path(project_path, "NU_civis_outputs"), SAVE = FALSE) {
  simdates <- list.dirs(NUCivisDir, recursive = FALSE, full.names = FALSE)
  simdates <- as.numeric(simdates)
  # simdates <- simdates[simdates > 20200603 & !is.na(simdates)]
  simdates <- simdates[!is.na(simdates)]
  
  scenario <- "baseline"
  
  
  datList <- list()
  for (simdate in simdates) {
    
    fname <- list.files(file.path(NUCivisDir,simdate,"trajectories"), recursive = TRUE, pattern="baseline", full.names = TRUE)
    fname <- fname[!grepl("trimmed_",fname)]
    fname <- fname[!grepl("_old_",fname)]
    if (is_empty(fname)) next
    
    df <- read.csv(fname) %>% mutate(simdate = simdate)
    datList[[length(datList) + 1]] <- df
  }
  
  
  dat <- datList %>%
    bind_rows() %>%
    as.data.frame()

  dat$Date <-  dat$time +  as.Date("2020-02-13")
  summary(dat$simdate)
  summary(dat$Date)
  
  
  dat$simdate <- as.Date(as.character(dat$simdate), format = "%Y%m%d")
  
  
  if (SAVE) write.csv(dat, "combinedTrajectoriesDat_AprilAug2020.csv", row.names = FALSE)
  return(dat)
}



f_combineSimulations <- function(NUCivisDir = file.path(project_path, "NU_civis_outputs"), SAVE = FALSE) {
  simdates <- list.dirs(NUCivisDir, recursive = FALSE, full.names = FALSE)
  simdates <- as.numeric(simdates)
  # simdates <- simdates[simdates > 20200603 & !is.na(simdates)]
  simdates <- simdates[!is.na(simdates)]

  scenario <- "baseline"


  datList <- list()
  for (simdate in simdates) {
    fname <- file.path(NUCivisDir, paste0(simdate, "/csv/nu_il_", scenario, "_", simdate, ".csv"))
    if (!file.exists(fname)) {
      fname <- file.path(NUCivisDir, paste0(simdate, "/csv/nu_ems_", scenario, "_", simdate, ".csv"))
    }
    if (!file.exists(fname)) {
      fname <- file.path(NUCivisDir, paste0(simdate, "/csv/nu_illinois_", scenario, "_", simdate, ".csv"))
    }
    if (!file.exists(fname)) next
    df <- read.csv(fname) %>% mutate(simdate = simdate)
    if (unique(df$simdate %in% c("20200419", "20200420"))) df$geography_modeled <- "illinois"
    colnames(df)[colnames(df) == "date"] <- "Date"
    colnames(df)[colnames(df) == "ems"] <- "geography_modeled"
    df$geography_modeled <- gsub("ems", "", df$geography_modeled)
    df$geography_modeled <- gsub("covidregion_", "", df$geography_modeled)


    datList[[length(datList) + 1]] <- df
  }


  dat <- datList %>%
    bind_rows() %>%
    as.data.frame()
  dat$Date <- as.Date(dat$Date, "%Y-%m-%d")
  summary(dat$simdate)
  summary(dat$Date)


  geographyLevels <- c("illinois", "northcentral", "northeast", "central", "southern", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
  dat$geography_modeled <- factor(dat$geography_modeled, labels = geographyLevels, levels = geographyLevels)

  dat$simdate <- as.Date(as.character(dat$simdate), format = "%Y%m%d")


  if (SAVE) write.csv(dat, "combinedIterations_AprilAug2020.csv", row.names = FALSE)
  return(dat)
}


f_defineVariablesPerChannel <- function(df, channel = "ICU") {
  df <- as.data.frame(df)
  if (channel == "ICU") {
    df$ylabel <- "ICU census"
    df$simvar_lo <- df$Lower.error.bound.of.number.of.ICU.beds.occupied
    df$simvar_up <- df$Upper.error.bound.of.number.of.ICU.beds.occupied
    df$simvar <- df$Number.of.ICU.beds.occupied
    df$datvar <- df$confirmed_covid_icu
    df$datvarLL <- NA
    df$capacity <- df$icu_available
  }

  if (channel == "deaths") {
    df$ylabel <- "deaths"
    df$simvar_lo <- df$Lower.error.bound.of.covid.19.deaths
    df$simvar_up <- df$Upper.error.bound.of.covid.19.deaths
    df$simvar <- df$Number.of.covid.19.deaths
    df$datvar <- df$confirmed_covid_deaths_prev_24h
    df$datvarLL <- df$LL_deaths
    df$capacity <- NA
  }
  if (channel == "hospitalizations") {
    df$ylabel <- "non-ICU inpatient census"
    df$simvar_lo <- df$Lower.error.bound.of.number.of.hospital.beds.occupied
    df$simvar_up <- df$Upper.error.bound.of.number.of.hospital.beds.occupied
    df$simvar <- df$Number.of.hospital.beds.occupied
    df$datvar <- df$covid_non_icu
    df$datvarLL <- df$LL_admissions
    df$capacity <- df$medsurg_available
  }

  return(df)
}


### Timeline plots per geography and outcome
f_timelinePlot <- function(df, reg = "illinois", channel = "ICU", logscale = TRUE, 
                           showLegend = TRUE, addData = TRUE, showCapacity=FALSE,
                           datasource = "EMResource", SAVE = TRUE,
                           SAVEPDF=FALSE, savedir = getwd()) {
 # if (!(reg %in% c("illinois", "northcentral", "northeast", "central", "southern"))) ptitle <- paste0("Covid region ", reg)
#if (reg == "illinois") ptitle <- "Illinois"
 # if (reg %in% c("northcentral", "northeast", "central", "southern")) ptitle <- paste0(str_to_title(reg), " restore region")

  df <- subset(df, geography_modeled %in% reg & Date <= as.Date("2020-08-31"))


  df <- f_defineVariablesPerChannel(df, channel = channel)
  ylabel <- unique(df$ylabel)

  pplot <- ggplot(data = df) +
    geom_ribbon(aes(
      x = Date,
      ymin = simvar_lo,
      ymax = simvar_up,
      fill = as.factor(simdate),
      group = simdate
    ), alpha = 0.3) +
    geom_line(aes(x = Date, y = simvar, col = as.factor(simdate)), size = 1.3) +
    labs(
      #title = ptitle,
      subtitle = "",
      caption = paste0("Data source: ", datasource, " data (points)"),
      y = paste0("Total " , ylabel),
      x="",
      color = "simdate",
      fill = "simdate"
    ) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    scale_x_date(date_breaks = "30 days", date_labels = "%d\n%b") +
    theme_cowplot() +
    background_grid()  +
    facet_wrap(~geography_modeled, scales="free_y") +
    customThemeNoFacet +
    theme(legend.position = "none") +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))
  
  if(showCapacity){
    if(channel !="deaths") pplot <- pplot+ geom_hline(aes(yintercept = capacity), linetype="dashed", size=1, color="darkred")
  }

  if (addData) {
    if (datasource == "EMResource") pplot <- pplot + geom_point(data = df, aes(x = Date, y = datvar), size = 3)
    if (datasource == "Linelist") pplot <- pplot + geom_point(data = df, aes(x = Date, y = datvarLL), size = 3)
  }


  if (logscale==TRUE) {
    pplot <- pplot + scale_y_log10(labels = comma) + labs(y = paste0(ylabel, "\n(logscale)"))
    channel <- paste0(channel, "_logscale")
  }
  if(logscale==FALSE) {
    pplot <- pplot + scale_y_continuous(labels = comma) + labs(y = paste0(ylabel))
    channel <- paste0(channel, "_logscale")
  }

  if (showLegend) pplot <- pplot + theme(legend.position = "right")

  #fname <- paste0("EMScovid-", reg)
  #if (reg == "illinois") fname <- "IL"
  #if (reg %in% c("northcentral", "northeast", "central", "southern")) fname <- paste0("RestoreRegion-", reg)

  if (SAVE) ggsave(paste0(region, "_30day_timeline_", channel, "_",datasource, ".png"), plot = pplot, path=savedir,width = 12, height = 9, device = "png")
  if (SAVEPDF) ggsave(paste0(region, "_30day_timeline_", channel, "_",datasource, ".pdf"), plot = pplot, path=savedir,width = 12, height = 9, device = "pdf")
  
  return(pplot)
}


### 30day timeline plot
f_30dayTimeline <- function(df = combinedDat, reg, channel = "ICU", SAVE = TRUE, logscale = FALSE) {
  df <- f_defineVariablesPerChannel(df, channel = channel)

  if (!(reg %in% c("illinois", "northcentral", "northeast", "central", "southern"))) ptitle <- paste0("Covid region ", reg)
  if (reg == "illinois") ptitle <- "Illinois"
  if (reg %in% c("northcentral", "northeast", "central", "southern")) ptitle <- str_to_title(reg)

  pplot <- ggplot(data = subset(df, geography_modeled %in% reg)) +
    theme_minimal() +
    geom_ribbon(aes(
      x = Date, ymin = simvar_lo,
      ymax = simvar_up
    ), fill = "deepskyblue3", alpha = 0.3) +
    geom_line(aes(x = Date, y = simvar), col = "deepskyblue3", size = 1.3) +
    geom_point(aes(x = Date, y = datvar), col = "black", size = 2) +
    geom_smooth(aes(x = Date, y = datvar), col = "black", se = FALSE) +
    facet_wrap(~simdate, scales = "free") +
    customThemeNoFacet +
    labs(
      title = reg,
      subtitle = "\n Panels show date of simulation and 30 day forward projection compared to data",
      y = channel,
      x = ""
    ) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))

  if (!(all(is.na(df$datvarLL)))) {
    pplot <- pplot + geom_point(aes(x = Date, y = datvarLL), fill = "grey", size = 2, shape = 21) +
      geom_smooth(aes(x = Date, y = datvarLL), col = "darkgrey", se = FALSE)
  }


  if (logscale == FALSE) pplot <- pplot + scale_y_continuous(labels = comma)
  if (logscale == TRUE) {
    pplot <- pplot + scale_y_log10(labels = comma)
    channel <- paste0(channel, "_logscale")
  }


  fname <- paste0("EMScovid-", reg)
  if (reg == "illinois") fname <- "IL"
  if (reg %in% c("northcentral", "northeast", "central", "southern")) fname <- paste0("RestoreRegion-", reg)

  width <- 14
  height <- 12


  if (SAVE) ggsave(paste0(fname, "_30day_simvsdata_", channel, ".png"), plot = pplot, width = width, height = height, device = "png")

  return(pplot)
}


################
processData <- T
if (processData) {
  coviddat <- f_loadData()

  coviddatIL <- coviddat %>%
    dplyr::group_by(Date) %>%
    dplyr::select(-restore_region, -region) %>%
    dplyr::summarize_all(.funs = "sum") %>%
    mutate(geography_modeled = "illinois")

  coviddatRR <- coviddat %>%
    dplyr::group_by(Date, restore_region) %>%
    dplyr::select(-region) %>%
    dplyr::summarize_all(.funs = "sum") %>%
    rename(geography_modeled = restore_region)

  coviddat <- coviddat %>%
    dplyr::select(-restore_region) %>%
    rename(geography_modeled = region) %>%
    rbind(coviddatIL, coviddatRR)

  geographyLevels <- c("illinois", "northcentral", "northeast", "central", "southern", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
  coviddat$geography_modeled <- factor(coviddat$geography_modeled, labels = geographyLevels, levels = geographyLevels)
  
  
  dat <- f_combineSimulations() %>%
    group_by(geography_modeled, simdate) %>%
    mutate(postsimdays = as.numeric(Date - simdate))
  
  
  
  mergevars <- colnames(dat)[colnames(dat) %in% colnames(coviddat)]
  combinedDat <- dat %>%
    dplyr::group_by(geography_modeled, simdate) %>%
    dplyr::filter(simdate < "2020-08-05" & postsimdays >= 1 & postsimdays <= 30) %>%
    dplyr::left_join(coviddat, by = mergevars, all.x = TRUE) %>%
    dplyr::mutate(
      diff_emr_ICU = abs(Number.of.ICU.beds.occupied - confirmed_covid_icu),
      diff_emr_deaths = abs(Number.of.detected.covid.19.deaths - confirmed_covid_deaths_prev_24h),
      diff_ll_deaths = abs(Number.of.detected.covid.19.deaths - LL_deaths),
      
      ratio_emr_ICU = (Number.of.ICU.beds.occupied / confirmed_covid_icu),
      ratio_emr_deaths = (Number.of.detected.covid.19.deaths / confirmed_covid_deaths_prev_24h),
      ratio_ll_deaths = (Number.of.detected.covid.19.deaths / LL_deaths),
      
      reldiff_emr_ICU = (Number.of.ICU.beds.occupied - confirmed_covid_icu) / confirmed_covid_icu
    ) %>%
    dplyr::group_by(simdate, postsimdays) %>%
    dplyr::mutate(withinUncertainityRange = ifelse(confirmed_covid_icu >= Lower.error.bound.of.number.of.ICU.beds.occupied &
                                                     confirmed_covid_icu <= Upper.error.bound.of.number.of.ICU.beds.occupied, 1, 0))
  
  
  
  capacities <- load_capacity() %>% rename(geography_modeled = geography_name)
  combinedDat <- combinedDat %>% left_join(capacities, by="geography_modeled")
  
  
  geographyLevels <- c("illinois", "northcentral", "northeast", "central", "southern", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
  combinedDat$geography_modeled <- factor(combinedDat$geography_modeled, labels = geographyLevels, levels = geographyLevels)
  
  
  
}


#for (region in unique(combinedDat$geography_modeled)) {
for (region in c("IL","restore_region", "covid_region")) {
    

  if (!(region %in% c("IL", "restore_region"))) geography <- "covid_regions"
  if (region %in% c("IL", "restore_region")) geography <-"restore_regions_and_IL"
    
  if(region=="IL")regs="illinois"
  if(region=="restore_region")regs=c("northcentral", "northeast", "central", "southern")
  if(region=="covid_region")regs=as.character(c(1:11))
    
  tempdir <- file.path(project_path, "Plots + Graphs/simulated_scenarios/iteration_comparison")
  logscale=F
  SAVE=T
  
  if (!dir.exists(tempdir)) dir.create(tempdir)
  f_timelinePlot(combinedDat, reg = regs, channel = "ICU", logscale = logscale, showLegend = F, addData = T, datasource = "EMResource", SAVE=SAVE, savedir = tempdir)
  f_timelinePlot(combinedDat, reg = regs, channel = "hospitalizations", logscale = logscale, showLegend = F, addData = T, datasource = "EMResource",SAVE=SAVE,  savedir = tempdir)
  f_timelinePlot(combinedDat, reg = regs, channel = "deaths", logscale = logscale, showLegend = F, addData = T, datasource = "EMResource",SAVE=SAVE,  savedir = tempdir)

  f_timelinePlot(combinedDat, reg = regs, channel = "hospitalizations", logscale = logscale, showLegend = F, addData = T, datasource = "Linelist",SAVE=SAVE,  savedir = tempdir)
  f_timelinePlot(combinedDat, reg = regs, channel = "deaths", logscale = logscale, showLegend = F, addData = T, datasource = "Linelist",SAVE=SAVE,  savedir = tempdir)

  }


combinedDat %>% group_by(simdate, geography_modeled) %>% 
  mutate(ICUcap = ifelse(Upper.error.bound.of.number.of.ICU.beds.occupied > icu_available,1,0),
         ICUcap = ifelse(Upper.error.bound.of.number.of.hospital.beds.occupied > medsurg_available,1,0))



explorativePlots=FALSE
if(explorativePlots){
  
  
  ########  Compare 30 days
  outtbl <- combinedDat %>%
    filter(simdate > as.Date("2020-05-01") & simdate < as.Date("2020-07-15")) %>%
    dplyr::group_by(geography_modeled, simdate) %>%
    dplyr::summarize(
      confirmed_covid_icu = mean(confirmed_covid_icu, na.rm = TRUE),
      Lower.error.bound.of.number.of.ICU.beds.occupied = mean(Lower.error.bound.of.number.of.ICU.beds.occupied, na.rm = TRUE),
      Upper.error.bound.of.number.of.ICU.beds.occupied = mean(Upper.error.bound.of.number.of.ICU.beds.occupied, na.rm = TRUE)
    ) %>%
    dplyr::mutate(withinUncertainityRange = ifelse(confirmed_covid_icu >= Lower.error.bound.of.number.of.ICU.beds.occupied &
                                                     confirmed_covid_icu <= Upper.error.bound.of.number.of.ICU.beds.occupied, "within uncertainity range", "outside uncertainity range"))
  
  outtbl %>%
    dplyr::group_by(geography_modeled, withinUncertainityRange) %>%
    tally() %>%
    pivot_wider(names_from = "withinUncertainityRange", values_from = "n") %>%
    # mutate(perc = `within uncertainity range`/10 ) %>%
    write.csv()
  
  corrdat <- combinedDat %>%
    filter(simdate > as.Date("2020-05-01") & simdate < as.Date("2020-07-15")) %>%
    dplyr::group_by(geography_modeled, simdate) %>%
    dplyr::summarize(PearsonCor = cor(Number.of.hospital.beds.occupied, covid_non_icu)) %>%
    pivot_wider(names_from = "simdate", values_from = "PearsonCor") %>% 
    write.csv("correlations_nonICUcensus_EMR.csv")
  
  
  corrdat <- combinedDat %>%
    filter(simdate > as.Date("2020-05-01") & simdate < as.Date("2020-07-15")) %>%
    dplyr::group_by(geography_modeled, simdate) %>%
    dplyr::summarize(PearsonCor = cor(Number.of.ICU.beds.occupied, confirmed_covid_icu)) %>%
    pivot_wider(names_from = "simdate", values_from = "PearsonCor") %>% 
    write.csv("correlations_ICUcensus_EMR.csv")
  
  
pcorICU <- combinedDat %>%     filter(!(geography_modeled %in% c( "northcentral", "northeast", "central", "southern"))) %>%
    filter(simdate > as.Date("2020-05-01") & simdate < as.Date("2020-07-15")) %>%
    dplyr::group_by(geography_modeled, simdate) %>%
    dplyr::summarize(PearsonCorICU = cor(Number.of.ICU.beds.occupied, confirmed_covid_icu)) %>%
  ggplot() + 
    geom_boxplot(aes(x=reorder(geography_modeled, abs(PearsonCorICU)), y=abs(PearsonCorICU)))+
  labs(x="", y="Correlation in ICU census")+
  theme_cowplot() 
  
  
pcorHosp <-  combinedDat %>%     filter(!(geography_modeled %in% c( "northcentral", "northeast", "central", "southern"))) %>%
    filter(simdate > as.Date("2020-05-01") & simdate < as.Date("2020-07-15")) %>%
    dplyr::group_by(geography_modeled, simdate) %>%
    dplyr::summarize(PearsonCorHosp = cor(Number.of.hospital.beds.occupied, covid_non_icu)) %>%
    ggplot() + 
    geom_boxplot(aes(x=reorder(geography_modeled, abs(PearsonCorHosp)), y=abs(PearsonCorHosp)))+
  labs(x="", y="Correlation in non-ICU census")+
  theme_cowplot() 
  
 

pcorHospsimdate <-  combinedDat %>%     filter(!(geography_modeled %in% c( "northcentral", "northeast", "central", "southern"))) %>%
  filter(simdate > as.Date("2020-05-01") & simdate < as.Date("2020-07-15")) %>%
  dplyr::group_by(geography_modeled, simdate) %>%
  dplyr::summarize(PearsonCorHosp = cor(Number.of.hospital.beds.occupied, covid_non_icu)) %>%
  ggplot() + 
  geom_boxplot(aes(x=as.factor(simdate) , y=abs(PearsonCorHosp), group=simdate))+
  labs(x="Simulation date", y="Correlation in non-ICU census")+
  theme_cowplot() 



pcorICUsimdate <- combinedDat %>%     filter(!(geography_modeled %in% c( "northcentral", "northeast", "central", "southern"))) %>%
  filter(simdate > as.Date("2020-05-01") & simdate < as.Date("2020-07-15")) %>%
  dplyr::group_by(geography_modeled, simdate) %>%
  dplyr::summarize(PearsonCorICU = cor(Number.of.ICU.beds.occupied, confirmed_covid_icu)) %>%
  ggplot() + 
  geom_boxplot(aes(x=as.factor(simdate) ,  y=abs(PearsonCorICU)))+
  #geom_point(aes(x=as.factor(simdate) ,  y=abs(PearsonCorICU), col=geography_modeled))+
  labs(x="Simulation date", y="Correlation in ICU census") +
  theme_cowplot() 



  
  corrdat2 <- combinedDat %>%
    filter(simdate > as.Date("2020-05-01") & simdate < as.Date("2020-07-15")) %>%
    dplyr::group_by(geography_modeled, simdate) %>%
    dplyr::summarize(PearsonCor = cor(Number.of.ICU.beds.occupied, confirmed_covid_icu)) %>%
    mutate(highcor = ifelse(abs(PearsonCor) > 0.8, "correlation_GT_0.8", "correlation_LT_0.8"))
  
  corrdat2 %>%
    dplyr::group_by(geography_modeled, highcor) %>%
    tally() %>%
    pivot_wider(names_from = "highcor", values_from = "n") %>%
    mutate(perc = correlation_GT_0.8 / 10)
  
  
  tapply(abs(corrdat2$PearsonCor), corrdat2$geography_modeled, summary)
  
  mergevars <- colnames(dat)[colnames(dat) %in% colnames(coviddat)]
  past30daysDat <- dat %>%
    group_by(geography_modeled, simdate) %>%
    filter(simdate < "2020-08-05" & postsimdays >= -30 & postsimdays <= 1) %>%
    left_join(coviddat, by = mergevars, all.x = TRUE) %>%
    mutate(
      diff_emr_ICU = abs(Number.of.ICU.beds.occupied - confirmed_covid_icu),
      diff_emr_deaths = abs(Number.of.detected.covid.19.deaths - confirmed_covid_deaths_prev_24h),
      diff_ll_deaths = abs(Number.of.detected.covid.19.deaths - LL_deaths),
      
      ratio_emr_ICU = (Number.of.ICU.beds.occupied / confirmed_covid_icu),
      ratio_emr_deaths = (Number.of.detected.covid.19.deaths / confirmed_covid_deaths_prev_24h),
      ratio_ll_deaths = (Number.of.detected.covid.19.deaths / LL_deaths),
      
      reldiff_emr_ICU = (Number.of.ICU.beds.occupied - confirmed_covid_icu) / confirmed_covid_icu
    )
  
  
  for (reg in unique(combinedDat$geography_modeled)) {
    f_30dayTimeline(df = combinedDat, reg, channel = "ICU")
    f_30dayTimeline(df = combinedDat, reg, channel = "deaths")
    f_30dayTimeline(df = combinedDat, reg, channel = "hospitalizations")
    
    f_30dayTimeline(df = combinedDat, reg, channel = "ICU", logscale = T)
    f_30dayTimeline(df = combinedDat, reg, channel = "deaths", logscale = T)
    f_30dayTimeline(df = combinedDat, reg, channel = "hospitalizations", logscale = T)
  }
  
  ###  to see the 30 day versions too, and where we didn't anticipate changes in transmission!
  combinedDat <- subset(combinedDat, simdate <= as.Date("2020-07-15"))
  
  combinedDat %>%
    filter(geography_modeled == "illinois") %>%
    ggplot() +
    geom_boxplot(aes(x = as.factor(simdate), y = diff_emr_ICU, group = simdate)) +
    facet_wrap(~geography_modeled, scales = "free_y") +
    labs(
      y = "simulation - data",
      x = "date of simulation"
    ) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))
  
  combinedDat %>%
    filter(geography_modeled == "illinois") %>%
    ggplot() +
    geom_boxplot(aes(x = as.factor(simdate), y = ratio_emr_ICU, group = simdate)) +
    facet_wrap(~geography_modeled, scales = "free_y") +
    geom_hline(yintercept = 1) +
    labs(
      y = "simulation / data",
      x = "date of simulation"
    ) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))
  
  combinedDat %>%
    filter(!(geography_modeled %in% c("illinois", "northcentral", "northeast", "central", "southern"))) %>%
    ggplot() +
    geom_boxplot(aes(x = as.factor(simdate), y = ratio_emr_ICU, group = simdate)) +
    geom_hline(yintercept = 1) +
    facet_wrap(~geography_modeled, scales = "free_y") +
    labs(
      y = "simulation - data",
      x = "date of simulation"
    ) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))
  
  
  
  ### Keep past 30 days
  next30daysDat <- combinedDat
  past30daysDat
  past14daysDat <- past30daysDat %>%
    filter(postsimdays >= -14) %>%
    mutate(pweek = ifelse(postsimdays <= -7, "past14Days", "past7days")) %>%
    group_by(geography_modeled, simdate, pweek) %>%
    summarize(
      icu_sim = mean(Number.of.ICU.beds.occupied),
      icu_dat = mean(confirmed_covid_icu)
    ) %>%
    pivot_wider(names_from = "pweek", values_from = c("icu_sim", "icu_dat")) %>%
    mutate(
      icu_last2wks_sim_ratio = icu_sim_past14Days / icu_sim_past7days,
      icu_last2wks_dat_ratio = icu_dat_past14Days / icu_dat_past7days
    )
  
  tapply(past14daysDat$icu_last2wks_sim_ratio, past14daysDat$simdate, summary)
  tapply(past14daysDat$icu_last2wks_dat_ratio, past14daysDat$simdate, summary)
  
  ggplot(data = past14daysDat) +
    geom_point(aes(x = icu_last2wks_sim_ratio, y = icu_last2wks_dat_ratio))
  
  
  pIL <- combinedDat %>%
    # filter(!(geography_modeled %in% c("illinois", "northcentral", "northeast", "central", "southern" ))) %>%
    # filter((geography_modeled %in% c("illinois", "northcentral", "northeast", "central", "southern" ))) %>%
    # filter((geography_modeled %in% c("illinois" ))) %>%
    left_join(past14daysDat, by = c("geography_modeled", "simdate")) %>%
    ggplot() +
    # geom_boxplot(aes(x=icu_last2wks_sim_ratio , y=ratio_emr_ICU, col=as.factor(simdate),group=interaction(icu_last2wks_sim_ratio,simdate ) )) +
    annotate("rect", xmin = -Inf, xmax = 1, ymin = -Inf, ymax = Inf, fill = "azure3", alpha = 0.3) +
    geom_point(aes(x = icu_last2wks_sim_ratio, y = ratio_emr_ICU, col = as.factor(simdate), alpha = postsimdays, group = simdate), size = 2) +
    scale_alpha_continuous(range = c(1, 0.1)) +
    geom_hline(yintercept = 1) +
    labs(
      y = "ratio simulation vs data",
      x = "ratio past 4 weeks (simulation)",
      col = "simulation date",
      alpha = "days forward predicted"
    ) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    scale_color_viridis_d() +
    annotate("text", x = 0.8, y = 35, label = "No increase the week\nbefore prediction", col = "azure4", size = 5) +
    annotate("text", x = 1.4, y = 35, label = "Increase the week\nbefore prediction", col = "black", size = 5)
  
  if (SAVE) ggsave(paste0("allAreas_post30Days_vs_past2wks_ICUemr.png"), plot = pIL, width = 10, height = 6, device = "png")
  
  
  
  pEMS <- combinedDat %>%
    filter(!(geography_modeled %in% c("illinois", "northcentral", "northeast", "central", "southern"))) %>%
    # filter((geography_modeled %in% c("illinois", "northcentral", "northeast", "central", "southern" ))) %>%
    # filter((geography_modeled %in% c("illinois" ))) %>%
    left_join(past14daysDat, by = c("geography_modeled", "simdate")) %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = 1, ymin = -Inf, ymax = Inf, fill = "azure3", alpha = 0.3) +
    # geom_boxplot(aes(x=icu_last2wks_sim_ratio , y=ratio_emr_ICU, col=as.factor(simdate),group=interaction(icu_last2wks_sim_ratio,simdate ) )) +
    geom_point(aes(x = icu_last2wks_sim_ratio, y = ratio_emr_ICU, col = as.factor(simdate), alpha = postsimdays, group = simdate), size = 2) +
    scale_alpha_continuous(range = c(1, 0.1)) +
    geom_hline(yintercept = 1) +
    labs(
      y = "ratio simulation vs data",
      x = "ratio past 4 weeks (simulation)",
      col = "simulation date"
    ) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    scale_color_viridis_d() +
    facet_wrap(~geography_modeled)
  # annotate("text", x = 0.8, y = 35, label = "No increase the week\nbefore prediction", col="azure4",size=5)+
  # annotate("text", x = 1.4, y = 35, label = "Increase the week\nbefore prediction", col="black",size=5)
  
  if (SAVE) ggsave(paste0("EMScovid_post30Days_vs_past2wks_ICUemr.png"), plot = pEMS, width = 10, height = 6, device = "png")
  
  
  
  ##### Look at trends
  if (correlationPlots) {
    pcorr <- combinedDat %>%
      ggplot() +
      # geom_errorbar(aes(x=confirmed_covid_icu, ymin=Lower.error.bound.of.number.of.ICU.beds.occupied, ymax=Upper.error.bound.of.number.of.ICU.beds.occupied , col=simdate))
      geom_point(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied, col = as.factor(simdate))) +
      geom_smooth(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied, group = simdate, col = as.factor(simdate)), method = "lm", se = FALSE, size = 0.7, alpha = 0.5) +
      geom_smooth(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied), method = "lm", se = FALSE, col = "black", size = 1.1) +
      scale_color_viridis_d() +
      labs(
        y = "Number.of.ICU.beds.occupied (simulation)",
        x = "confirmed_covid_icu (EMResource data)",
        col = "simulation date"
      ) +
      geom_hline(yintercept = c(-Inf, Inf)) +
      geom_vline(xintercept = c(-Inf, Inf))
    
    if (SAVE) ggsave(paste0("allAreas_post30Days_correlation_ICUemr.png"), plot = pcorr, width = 10, height = 6, device = "png")
    
    
    pcorr <- combinedDat %>%
      ggplot() +
      # geom_errorbar(aes(x=confirmed_covid_icu, ymin=Lower.error.bound.of.number.of.ICU.beds.occupied, ymax=Upper.error.bound.of.number.of.ICU.beds.occupied , col=simdate))
      geom_point(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied, alpha = postsimdays, group = geography_modeled, col = as.factor(geography_modeled))) +
      geom_smooth(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied, group = simdate, col = as.factor(geography_modeled)), method = "lm", se = FALSE, size = 0.7, alpha = 0.5, show.legend = F) +
      geom_smooth(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied), method = "lm", se = FALSE, col = "black", size = 1.1, show.legend = F) +
      scale_alpha_continuous(range = c(1, 0.1)) +
      scale_color_viridis_d() +
      labs(
        y = "Number.of.ICU.beds.occupied (simulation)",
        x = "confirmed_covid_icu (EMResource data)",
        col = "simulation date",
        alpha = "days forward predicted"
      ) +
      geom_hline(yintercept = c(-Inf, Inf)) +
      geom_vline(xintercept = c(-Inf, Inf)) +
      facet_wrap(~simdate, scales = "free")
    if (SAVE) ggsave(paste0("allAreas_post30Days_correlation_bygeography_ICUemr.png"), plot = pcorr, width = 10, height = 6, device = "png")
    
    
    
    pcorr <- combinedDat %>%
      ggplot() +
      # geom_errorbar(aes(x=confirmed_covid_icu, ymin=Lower.error.bound.of.number.of.ICU.beds.occupied, ymax=Upper.error.bound.of.number.of.ICU.beds.occupied , col=simdate))
      geom_point(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied, alpha = postsimdays, group = simdate, col = as.factor(simdate))) +
      geom_smooth(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied, group = simdate, col = as.factor(simdate)), method = "lm", se = FALSE, size = 0.7, alpha = 0.5, show.legend = F) +
      # geom_smooth(aes(x=confirmed_covid_icu, y=Number.of.ICU.beds.occupied ), method="lm",se=FALSE, col="black", size=1.1,show.legend = F) +
      scale_alpha_continuous(range = c(1, 0.1)) +
      scale_color_viridis_d() +
      labs(
        y = "Number.of.ICU.beds.occupied (simulation)",
        x = "confirmed_covid_icu (EMResource data)",
        col = "simulation date",
        alpha = "days forward predicted"
      ) +
      geom_hline(yintercept = c(-Inf, Inf)) +
      geom_vline(xintercept = c(-Inf, Inf)) +
      facet_wrap(~geography_modeled, scales = "free")
    if (SAVE) ggsave(paste0("allAreas_post30Days_correlation_bygeography2_ICUemr.png"), plot = pcorr, width = 10, height = 6, device = "png")
    
    
    
    
    
    pcorr <- combinedDat %>%
      ggplot() +
      # geom_errorbar(aes(x=confirmed_covid_icu, ymin=Lower.error.bound.of.number.of.ICU.beds.occupied, ymax=Upper.error.bound.of.number.of.ICU.beds.occupied , col=simdate))
      geom_point(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied, alpha = postsimdays, col = as.factor(geography_modeled))) +
      geom_smooth(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied, group = simdate, col = as.factor(geography_modeled)), method = "lm", se = FALSE, size = 0.7, alpha = 0.5) +
      geom_smooth(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied), method = "lm", se = FALSE, col = "black", size = 1.1) +
      scale_alpha_continuous(range = c(1, 0.1)) +
      scale_color_viridis_d() +
      labs(
        y = "Number.of.ICU.beds.occupied (simulation)",
        x = "confirmed_covid_icu (EMResource data)",
        col = "simulation date",
        alpha = "days forward predicted"
      ) +
      geom_hline(yintercept = c(-Inf, Inf)) +
      geom_vline(xintercept = c(-Inf, Inf)) +
      facet_wrap(~simdate, scales = "free")
    
    if (SAVE) ggsave(paste0("allAreas_post30Days_correlatipn_bygeography_ICUemr.png"), plot = pcorr, width = 10, height = 6, device = "png")
    
    
    combinedDat %>%
      filter(!(geography_modeled %in% c("illinois", "northcentral", "northeast", "central", "southern"))) %>%
      ggplot() +
      # geom_errorbar(aes(x=confirmed_covid_icu, ymin=Lower.error.bound.of.number.of.ICU.beds.occupied, ymax=Upper.error.bound.of.number.of.ICU.beds.occupied , col=simdate))
      geom_point(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied, col = as.factor(simdate))) +
      geom_smooth(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied), method = "lm", se = FALSE, col = "black", size = 1.1) +
      scale_color_viridis_d() +
      facet_wrap(~geography_modeled, scales = "free")
    
    
    combinedDat %>%
      filter((geography_modeled %in% c("northcentral", "northeast", "central", "southern"))) %>%
      ggplot() +
      # geom_errorbar(aes(x=confirmed_covid_icu, ymin=Lower.error.bound.of.number.of.ICU.beds.occupied, ymax=Upper.error.bound.of.number.of.ICU.beds.occupied , col=simdate))
      geom_point(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied, col = as.factor(simdate))) +
      geom_smooth(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied), method = "lm", se = FALSE, col = "black", size = 1.1) +
      scale_color_viridis_d() +
      facet_wrap(~geography_modeled, scales = "free")
  }
  
  
  #
  # combinedDat %>%
  #   filter((geography_modeled %in% c( "northcentral", "northeast", "central", "southern" ))) %>%
  #   ggplot() +
  #   #geom_errorbar(aes(x=confirmed_covid_icu, ymin=Lower.error.bound.of.number.of.ICU.beds.occupied, ymax=Upper.error.bound.of.number.of.ICU.beds.occupied , col=simdate))
  #   geom_point(aes(x=confirmed_covid_icu, y=Number.of.ICU.beds.occupied , col=as.factor(geography_modeled))) +
  #   geom_smooth(aes(x=confirmed_covid_icu, y=Number.of.ICU.beds.occupied ), method="lm",se=FALSE, col="black", size=1.1) +
  #   scale_color_viridis_d() +
  #   facet_wrap(~simdate, scales="free")
  
  table(combinedDat$withinUncertainityRange)
  
  
  
  pIL <- combinedDat %>%
    filter(!is.na(withinUncertainityRange)) %>%
    # select(postsimdays, geography_modeled) %>%
    # summarize)
    # filter(!(geography_modeled %in% c("illinois", "northcentral", "northeast", "central", "southern" ))) %>%
    # filter((geography_modeled %in% c("illinois", "northcentral", "northeast", "central", "southern" ))) %>%
    # filter((geography_modeled %in% c("illinois" ))) %>%
    left_join(past14daysDat, by = c("geography_modeled", "simdate")) %>%
    ggplot() +
    # geom_boxplot(aes(x=icu_last2wks_sim_ratio , y=ratio_emr_ICU, col=as.factor(simdate),group=interaction(icu_last2wks_sim_ratio,simdate ) )) +
    # annotate("rect", xmin=-Inf, xmax=1, ymin=-Inf, ymax=Inf, fill="azure3", alpha=0.3) +
    geom_point(aes(x = as.factor(postsimdays), y = simdate, col = as.factor(withinUncertainityRange), alpha = postsimdays, group = simdate), size = 2) +
    scale_alpha_continuous(range = c(1, 0.1)) +
    facet_wrap(~geography_modeled) +
    labs(
      y = "simulation date",
      x = "days forward predicted",
      col = "within uncertainity range",
      alpha = "days forward predicted"
    ) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))
  
  if (SAVE) ggsave(paste0("allAreas_post30Days_draft_daysForwardPredicted_ICUemr.png"), plot = pIL, width = 10, height = 6, device = "png")
  
  
  
  
  combinedDat %>%
    left_join(past14daysDat, by = c("geography_modeled", "simdate")) %>%
    filter((geography_modeled %in% c("northcentral", "northeast", "central", "southern"))) %>%
    ggplot() +
    # geom_errorbar(aes(x=confirmed_covid_icu, ymin=Lower.error.bound.of.number.of.ICU.beds.occupied, ymax=Upper.error.bound.of.number.of.ICU.beds.occupied , col=simdate))
    geom_point(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied, col = as.factor(simdate))) +
    geom_smooth(aes(x = confirmed_covid_icu, y = Number.of.ICU.beds.occupied), method = "lm", se = FALSE, col = "black", size = 1.1) +
    scale_color_viridis_d() +
    facet_wrap(~geography_modeled, scales = "free")
  
  
  
  
  combinedDat
  
  
}
### Plot lies within uncertainity bounds


### shows same trend
if(extrapolateFromData){
  simdates <- unique(combinedDat[,c("simdate")]) %>% mutate(Date=simdate)
  
  mergevars <- colnames(dat)[colnames(dat) %in% colnames(coviddat)]
  past14daysDat <- dat %>%
    dplyr::group_by(geography_modeled, simdate) %>%
    dplyr::filter(simdate < "2020-08-05" & postsimdays >= -14 & postsimdays <= 0) %>%
    dplyr::left_join(coviddat, by = mergevars, all.x = TRUE) %>%
    select(Date, geography_modeled, postsimdays, confirmed_covid_icu, covid_non_icu, LL_admissions, LL_cases) %>%
    mutate(days = postsimdays+14) 
    

    dfHour = past14daysDat %>% as.data.frame() %>% 
      group_by(simdate, geography_modeled) %>% 
      do(fitHour = lm(confirmed_covid_icu ~ days, data = .))
    
    predListAll <- list() 
for(reg in unique(dfHour$geography_modeled)){
  
  predList <- list()
  ildat <- subset(dfHour, geography_modeled==reg)
  for(d in unique(ildat$simdate) ){
    
    print(d)
    
    subdat <- subset(ildat, simdate==d)
    model <- subdat$fitHour[[1]]
    
    xpred <- c(14:44)
    ypred <- predict(model,  data.frame(days=xpred))
    
    
    pred <- as.data.frame( cbind(xpred,ypred ))
    pred$simdate <- subdat$simdate
    
    predList[[length(predList)+1]] <- pred
  }
  
  
  
  predDat <- predList %>% bind_rows()     %>% mutate(postsimdays = xpred-14)
  predDat$geography_modeled <- reg
  
  predListAll[[length(predListAll)+1]] <- predDat
  
}
    
    
    predDat <- predListAll %>% bind_rows() 
    
    ggplot(data=predDat) + 
      geom_point(aes(x=xpred, y = ypred, col=as.factor(simdate)))
    
    
    
    
    mergevars <- colnames(dat)[colnames(dat) %in% colnames(coviddat)]
    testdat <- dat %>%
      dplyr::group_by(geography_modeled, simdate) %>%
      dplyr::filter(simdate < "2020-08-05" & postsimdays >= 0 & postsimdays <= 30) %>%
      dplyr::left_join(coviddat, by = mergevars, all.x = TRUE) %>%
      left_join(predDat, by=c("postsimdays","geography_modeled", "simdate"))
    
    
    simdateDat <-  testdat %>% group_by(simdate,geography_modeled) %>% filter(Date==simdate)  
    
    
    savedir <- file.path(project_path, "Plots + Graphs/simulated_scenarios/iteration_comparison")
    
    
    
    testdat <- testdat %>% mutate(ypred=ifelse(ypred<0,0, ypred))
    
    pplot <-  ggplot(data=subset(testdat, geography_modeled =="illinois")) +
      geom_point(aes(x=Date, y = confirmed_covid_icu), col="black")+
      geom_smooth(aes(x=Date, y = confirmed_covid_icu), col="black",se=FALSE)+
      geom_line(aes(x=Date, y = Number.of.ICU.beds.occupied, col=as.factor(simdate), group=simdate)) +
      geom_line(aes(x=Date, y = ypred, col=as.factor(simdate), group=simdate), linetype="dashed") +
      geom_point(data=subset(simdateDat, geography_modeled =="illinois"), aes(x=simdate, y = Number.of.ICU.beds.occupied,fill=as.factor(simdate)), shape=23,size=2)+
      geom_point(data=subset(simdateDat, geography_modeled =="illinois"), aes(x=simdate, y = ypred,fill=as.factor(simdate)), shape=23,size=2)+
      scale_color_viridis_d()+
      scale_fill_viridis_d()+
      scale_y_continuous(labels=comma) + 
      labs( x="",caption ="Solid line 30 day forward prediction fro, transmission model\nDashed line 30 day linear extrapolation from 2 weeks prior to projection date" ) +
      theme_cowplot() + background_grid() +
      theme(legend.position = "none") +
      customThemeNoFacet +
      geom_hline(yintercept = c(-Inf, Inf)) +
      geom_vline(xintercept = c(-Inf, Inf))
    
    
    if (SAVE) ggsave(paste0("Illinois_pred_vs_datlinearForward.png"), plot = pplot, path=savedir,width = 12, height = 8, device = "png")
    
    
    pplot <-  ggplot(data=subset(testdat, geography_modeled =="illinois")) +
      geom_point(aes(x=Date, y = confirmed_covid_icu), col="black")+
      geom_smooth(aes(x=Date, y = confirmed_covid_icu), col="black",se=FALSE)+
      geom_line(aes(x=Date, y = Number.of.ICU.beds.occupied, col=as.factor(simdate), group=simdate)) +
      geom_line(aes(x=Date, y = ypred, col=as.factor(simdate), group=simdate), linetype="dashed") +
      geom_point(data=subset(simdateDat, geography_modeled =="illinois"), aes(x=simdate, y = Number.of.ICU.beds.occupied,fill=as.factor(simdate)), shape=23,size=2)+
      geom_point(data=subset(simdateDat, geography_modeled =="illinois"), aes(x=simdate, y = ypred,fill=as.factor(simdate)), shape=23,size=2)+
      scale_color_viridis_d()+
      scale_fill_viridis_d()+
      labs( x="",caption ="Solid line 30 day forward prediction fro, transmission model\nDashed line 30 day linear extrapolation from 2 weeks prior to projection date" ) +
      theme_cowplot() + background_grid() +
      theme(legend.position = "none") +
      scale_y_log10(labels=comma)
      customThemeNoFacet +
      geom_hline(yintercept = c(-Inf, Inf)) +
      geom_vline(xintercept = c(-Inf, Inf))
    
    
    if (SAVE) ggsave(paste0("Illinois_pred_vs_datlinearForward_log.png"), plot = pplot, path=savedir,width = 12, height = 8, device = "png")
    
    
    
   pplot <-   ggplot(data=subset(testdat, geography_modeled =="illinois")) +
     # geom_point(aes(x=Date, y = confirmed_covid_icu), col="black")+
     # geom_smooth(aes(x=Date, y = confirmed_covid_icu), col="black",se=FALSE)+
      geom_line(aes(x=Date, y = Number.of.ICU.beds.occupied, col=as.factor(simdate), group=simdate),size=1.1) +
      geom_line(aes(x=Date, y = ypred, col=as.factor(simdate), group=simdate), linetype="dashed",size=1.1) +
      geom_point(data=subset(simdateDat, geography_modeled =="illinois"), aes(x=simdate, y = Number.of.ICU.beds.occupied,fill=as.factor(simdate)), shape=23,size=2)+
      geom_point(data=subset(simdateDat, geography_modeled =="illinois"), aes(x=simdate, y = ypred,fill=as.factor(simdate)), shape=23,size=2)+
      scale_color_viridis_d()+
      scale_fill_viridis_d() +
     scale_y_continuous(labels=comma) + 
      facet_wrap(~simdate, scales="free")+
     labs( x="", caption ="Solid line 30 day forward prediction fro, transmission model\nDashed line 30 day linear extrapolation from 2 weeks prior to projection date" ) +
      theme_cowplot() + background_grid() +
     theme(legend.position = "none") +
     customThemeNoFacet +
     geom_hline(yintercept = c(-Inf, Inf)) +
     geom_vline(xintercept = c(-Inf, Inf))
   
   if (SAVE) ggsave(paste0("Illinois_pred_vs_datlinearForward_v2.png"), plot = pplot, path=savedir,width = 18, height = 12, device = "png")
   
   
    
   pplot <-   ggplot(data=subset(testdat, geography_modeled =="illinois")) +
     # geom_point(aes(x=Date, y = confirmed_covid_icu), col="black")+
     # geom_smooth(aes(x=Date, y = confirmed_covid_icu), col="black",se=FALSE)+
     geom_line(aes(x=Date, y = Number.of.ICU.beds.occupied, col=as.factor(simdate), group=simdate),size=1.1) +
     geom_line(aes(x=Date, y = ypred, col=as.factor(simdate), group=simdate), linetype="dashed",size=1.1) +
     geom_point(data=subset(simdateDat, geography_modeled =="illinois"), aes(x=simdate, y = Number.of.ICU.beds.occupied,fill=as.factor(simdate)), shape=23,size=2)+
     geom_point(data=subset(simdateDat, geography_modeled =="illinois"), aes(x=simdate, y = ypred,fill=as.factor(simdate)), shape=23,size=2)+
     scale_color_viridis_d()+
     scale_fill_viridis_d() +
     facet_wrap(~simdate, scales="free")+
     labs(x="",caption ="Solid line 30 day forward prediction fro, transmission model\nDashed line 30 day linear extrapolation from 2 weeks prior to projection date" ) +
     scale_y_log10(labels=comma) +
     theme_cowplot() + background_grid() +
     theme(legend.position = "none") +
     customThemeNoFacet +
     geom_hline(yintercept = c(-Inf, Inf)) +
     geom_vline(xintercept = c(-Inf, Inf)) 
     if (SAVE) ggsave(paste0("Illinois_pred_vs_datlinearForward_v2_log.png"), plot = pplot, path=savedir,width = 18, height = 12, device = "png")
   
     
    
    ggplot(data=subset(testdat)) +
     # geom_point(aes(x=Date, y = confirmed_covid_icu), col="black")+
     # geom_smooth(aes(x=Date, y = confirmed_covid_icu), col="black",se=FALSE)+
      geom_line(aes(x=Date, y = Number.of.ICU.beds.occupied, col=as.factor(simdate), group=simdate)) +
      geom_line(aes(x=Date, y = ypred, col=as.factor(simdate), group=simdate), linetype="dashed") +
      geom_point(data=subset(simdateDat), aes(x=simdate, y = Number.of.ICU.beds.occupied,fill=as.factor(simdate)), shape=23,size=2)+
      geom_point(data=subset(simdateDat), aes(x=simdate, y = ypred,fill=as.factor(simdate)), shape=23,size=2)+
      scale_color_viridis_d()+
      scale_fill_viridis_d()+
      theme(legend.position = "none") +
      facet_wrap(~geography_modeled, scales="free")+
      geom_hline(yintercept = c(-Inf, Inf)) +
      geom_vline(xintercept = c(-Inf, Inf))
    
    
    
}