
### =============================================================================
###  Comparing forward prediction to data for varying simulation iterations
### =============================================================================

## Load package
require(tidyverse)
require(cowplot)
require(scales)
require(lubridate)


source("load_paths.R")
source("processing_helpers.R")

theme_set(theme_cowplot())


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

f_defineVariablesPerChannel <- function(df, channel = "ICU") {
  df <- as.data.frame(df)
  if (channel == "ICU") {
    df$ylabel <- "ICU census"
    df$simvar <- df$critical_det
    df$datvar <- df$confirmed_covid_icu
    df$datvarLL <- NA
    df$capacity <- df$icu_available
  }
  
  if (channel == "deaths") {
    df$ylabel <- "deaths"
    df$simvar <- df$deaths_det
    df$datvar <- df$confirmed_covid_deaths_prev_24h
    df$datvarLL <- df$LL_deaths
    df$capacity <- NA
  }
  if (channel == "hospitalizations") {
    df$ylabel <- "non-ICU inpatient census"
    df$simvar <- df$hospitalized_det
    df$datvar <- df$covid_non_icu
    df$datvarLL <- df$LL_admissions
    df$capacity <- df$medsurg_available
  }
  
  return(df)
}


### Load simulations
f_loadTrajectories <- function(fname, simdate, outcomes=NULL) {

  
  df <- read_csv(fname) 
  
    if(is.null(outcomes))outcomes <- c("hospitalized","critical", "deaths",'crit_cumul','hosp_cumul','hosp_det_cumul','crit_det_cumul','death_det_cumul')
    emsvars <- unique (grep(paste(outcomes,collapse="|"),  colnames(df), value=TRUE))
    emsvars  <- emsvars[!grepl("time_to_",emsvars)]
    emsvars  <- emsvars[!grepl("fraction_",emsvars)]
    keepVars <- c("time","scen_num",'sample_num','run_num', emsvars)
    
        df <- df %>%
      dplyr::select(keepVars) %>%
       mutate(startdate = as.Date("2020-02-15"),
        Date = startdate + time ,
         simdate = as.Date(as.character(simdate), format = "%Y%m%d")) %>%
         filter(Date <= simdate+30)
    
   colnames(df) <- gsub("regionAll","__All", colnames(df))
   colnames(df) <- gsub("All","__All", colnames(df))
   colnames(df) <- gsub("_EMS","__EMS", colnames(df))
   colnames(df) <- gsub("___","__", colnames(df))
   colnames(df) <- gsub("____","__", colnames(df))
   
   df <- df %>% as.data.frame() %>% 
         pivot_longer(cols=-c('time', 'scen_num','sample_num','run_num', 'startdate','Date', 'simdate')) %>%
          separate(name , into=c("channel","region"), sep="__") %>%
          pivot_wider(names_from="channel", values_from ="value") %>%
           mutate(postsimdays = as.numeric(Date - simdate))
   

   
   # geographyLevels <- c("All", paste0("EMS-", c(1:11)))
   # geographyLabels<- c("illinois",  as.character(c(1:11)))
   # df$geography_modeled <- factor(df$region, 
   #                                labels = geographyLevels, 
   #                                levels = geographyLabels)
   # 
   
  return(df)
}

NUCivisDir = file.path(project_path, "NU_civis_outputs")
simdates <- list.dirs(NUCivisDir, recursive = FALSE, full.names = FALSE)
simdates <- as.numeric(simdates)
# simdates <- simdates[simdates > 20200603 & !is.na(simdates)]
simdates <- simdates[!is.na(simdates)]


datList <- list()
for (simdate in simdates[-1]) {
  
  fname <- list.files(file.path(NUCivisDir,simdate,"trajectories"), recursive = TRUE, pattern='baseline', full.names = TRUE)
  fname <- fname[!grepl("trimmed_",fname)]
  fname <- fname[!grepl("_old_",fname)]
  
  if (is_empty(fname)) next
   df  <- f_loadTrajectories(fname, simdate)
  
  datList[[length(datList) + 1]] <- df
    
}
dat1 <- datList[c(2:8)] %>%   bind_rows() %>%  as.data.frame()
dat2 <- datList[c(9:13)] %>%   bind_rows() %>%  as.data.frame()
dat <- dat2



### Timeline plots per geography and outcome
f_timelinePlot <- function(df, reg = c(1), 
                           channel = "ICU", 
                           logscale = TRUE, 
                           showLegend = TRUE, addData = TRUE, 
                           showCapacity=FALSE,
                           datasource = "EMResource", 
                           SAVE = TRUE,
                           SAVEPDF=FALSE, savedir = getwd()) {

  #dfAggr <- df %>% f_addRestoreRegion()  %>% filter(restore_region==reg)
  
  capacities <- load_capacity() %>% rename(region = geography_name) %>% filter(region %in% c(1:11)) %>% mutate(region=as.numeric(region))
  df <- df %>% left_join(capacities, by="region")
  
  df <- f_defineVariablesPerChannel(df, channel = channel)
  ylabel <- unique(df$ylabel)
  
  pplot <- 
    df  %>% group_by(simdate, region) %>%
    filter(postsimdays>=0 & postsimdays<=30) %>%
    f_aggrDat( groupVars= c('Date','time','simdate', 'region'),valueVar=simvar) %>%
    ggplot() +
    geom_ribbon(aes(
      x = Date,
      ymin = q2.5,
      ymax = q97.5,
      fill = as.factor(simdate),
      group = simdate
    ), alpha = 0.3) +
    geom_line(aes(
      x = Date, y=median.val,
      col = as.factor(simdate),
      group = simdate
    ), alpha = 1, size=1.1) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    scale_x_date(date_breaks = "30 days", date_labels = "%d\n%b") +
    theme_cowplot() +
    background_grid()  +
    facet_wrap(~region, scales="free_y") +
    customThemeNoFacet +
    theme(legend.position = "none") +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf)) 
  
  
  if(showCapacity){
    if(channel !="deaths") pplot <- pplot+ geom_hline(data=df, aes(yintercept = capacity), linetype="dashed", size=1, color="darkred")
  }
  
  if (addData) {
    coviddat <- f_loadData(data_path) %>% mutate(Date=as.Date(as.character(Date))) %>% as.data.frame()
    if (datasource == "EMResource") pplot <- pplot + geom_point(data = coviddat, aes(x = Date, y = datvar), size = 3)
    if (datasource == "Linelist") pplot <- pplot + geom_point(data = coviddat, aes(x = Date, y = datvarLL), size = 3)
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


