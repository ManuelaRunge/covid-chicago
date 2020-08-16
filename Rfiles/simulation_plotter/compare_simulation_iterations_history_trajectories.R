
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
  
  fname <- list.files(file.path(NUCivisDir,simdate,"trajectories"), recursive = TRUE, pattern=scenario, full.names = TRUE)
  fname <- fname[!grepl("trimmed_",fname)]
  fname <- fname[!grepl("_old_",fname)]
  
  if (is_empty(fname)) next
   df  <- f_loadTrajectories(fname, simdate)
  
  datList[[length(datList) + 1]] <- df
    
}
dat <- datList %>%   bind_rows() %>%  as.data.frame()
#dat <- f_loadData(data_path)



df %>% group_by(simdate, region) %>%
  filter(postsimdays<=30) %>%
  f_aggrDat( groupVars= c('Date','time','simdate', 'region'),valueVar="critical") %>% 
  
  ggplot() +
  geom_ribbon(aes(
    x = Date,
    ymin = q2.5,
    ymax = q97.5,
    fill = as.factor(simdate),
    group = simdate
  ), alpha = 0.3) +
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


