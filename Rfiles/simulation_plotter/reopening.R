
library(tidyverse)
library(cowplot)
library(scales)
library(viridis)
require(readr)
source("load_paths.R")
source("processing_helpers.R")

theme_set(theme_cowplot())


simdate = "20200826"
exp_name <- "20200826_IL_RR_gradual_reopening_0"


trajectoriesDat <- read_csv(file.path(simulation_output, exp_name, "trajectoriesDat.csv"))

### Note use Ki_EMS-  or Ki_EMS.   not Ki_EMS_  (time varying vs input parameter)
# colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))
region_names =c("All", paste0("EMS-",c(1:11)))
outcomevars <- c(
  paste0("hospitalized_det_", region_names),
  paste0("hospitalized_", region_names),
  paste0("crit_det_", region_names),
  paste0("critical_", region_names)
)


region_names =paste0("EMS-",c(1:11))
outcomevars2 <- c(
  paste0("Ki_t_", region_names)
)


paramvars <- c("reopening_multiplier_4")
keepvars <- c("time", "startdate", "scen_num","sample_num",  paramvars, outcomevars,outcomevars2)


paramvalues <- trajectoriesDat %>%
  select(keepvars) %>%
  filter(time>120) %>%
  mutate(date = as.Date(startdate) + time) %>%
  pivot_longer(cols = -c("time", "date", "startdate", "reopening_multiplier_4"), names_to = "region") %>%
  separate(region, into = c("outcome", "region"), sep = "_EMS-") %>%
  mutate(
    region = as.numeric(region),
    exp_name = exp_name,
  ) %>%
  group_by(date, region, exp_name, reopening_multiplier_4, outcome) %>%
  summarize(
    median.value = median(value, na.rm = TRUE),
    q2.5.value = quantile(value, probs = 0.025, na.rm = TRUE),
    q97.5.value = quantile(value, probs = 0.975, na.rm = TRUE)
  )


capacityDat <- load_new_capacity() %>% mutate(region = as.character(geography_name))


paramvalues$region <- factor(paramvalues$region, levels = c(1:11), labels = c(1:11))
capacityDat$region <- factor(capacityDat$region, levels = c(1:11), labels = c(1:11))
paramvalues$reopening <- round(paramvalues$reopening_multiplier_4, 2) * 100

dat=paramvalues
save(dat, file=file.path(simulation_output, exp_name, "aggregatedDat_forR.Rdata"))

library(RColorBrewer)
getPalette <- colorRampPalette(brewer.pal(9, "PuBuGn"))(12)


pplot <- dat %>%
  filter(date >= as.Date("2020-08-19") & outcome == "hospitalized_det") %>%
  group_by(region, reopening, outcome) %>%
  filter(median.value == max(median.value)) %>%
  left_join(capacityDat) %>%
  mutate(belowCapacity = ifelse(median.value <= medsurg_available, 1, 0)) %>%
  ggplot() +
  geom_errorbar(aes(x = as.factor(reopening), ymin = q2.5.value, ymax = q97.5.value, col=as.factor(belowCapacity)), width = 0.3) +
  geom_point(aes(x = as.factor(reopening), y = median.value, fill = as.factor(belowCapacity)), shape = 21, show.legend = F, size = 2) +
  facet_wrap(~region, scales = "free") +
  scale_fill_manual(values = c("indianred", "deepskyblue3")) +
  geom_hline(aes(yintercept = medsurg_available)) +
  labs(x = "Reopening multiplier (%)", y = "Peak in non-ICU census until Jan 2020") +
  customThemeNoFacet

ggsave(paste0("nonICU_reopening_perCovidRegion.png"),
       plot = pplot, path = file.path(simulation_output, exp_name), width = 14, height = 8, device = "png"
)
ggsave(paste0("nonICU_reopening_perCovidRegion.pdf"),
       plot = pplot, path = file.path(simulation_output, exp_name), width = 14, height = 8, device = "pdf"
)
rm(pplot)


pplot <- dat %>%
  filter(date >= as.Date("2020-08-19") & outcome == "crit_det") %>%
  group_by(region, reopening, outcome) %>%
  filter(median.value == max(median.value)) %>%
  left_join(capacityDat) %>%
  mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0)) %>%
  ggplot() +
  geom_errorbar(aes(x = as.factor(reopening), ymin = q2.5.value, ymax = q97.5.value, col=as.factor(belowCapacity)), width = 0.3) +
  geom_point(aes(x = as.factor(reopening), y = median.value, fill = as.factor(belowCapacity)), shape = 21, show.legend = F, size = 2) +
  facet_wrap(~region, scales = "free") +
  scale_fill_manual(values = c("indianred", "deepskyblue3")) +
  geom_hline(aes(yintercept = icu_available)) +
  labs(x = "Reopening multiplier (%)", y = "Peak in ICU census until Jan 2020") +
  customThemeNoFacet

ggsave(paste0("ICU_reopening_perCovidRegion.png"),
       plot = pplot, path = file.path(simulation_output, exp_name), width = 14, height = 8, device = "png"
)
ggsave(paste0("ICU_reopening_perCovidRegion.pdf"),
       plot = pplot, path = file.path(simulation_output, exp_name), width = 14, height = 8, device = "pdf"
)
rm(pplot)


pplot <- dat %>%
  filter(region%in% c(4)) %>%
  filter(reopening <=10) %>%
  filter(date >= as.Date("2020-08-19") & date <= as.Date("2020-12-01") & outcome == "crit_det") %>%
  group_by(region, reopening, outcome) %>%
  filter(median.value == max(median.value)) %>%
  group_by(region, reopening, outcome) %>%
  filter(date == min(date)) %>%
  left_join(capacityDat) %>%
  mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0)) %>%
  ggplot() +
  geom_errorbar(aes(x = as.factor(reopening), ymin = q2.5.value, ymax = q97.5.value, col=as.factor(belowCapacity)), width = 0.2) +
  geom_point(aes(x = as.factor(reopening), y = median.value, fill = as.factor(belowCapacity)), shape = 21, show.legend = F, size = 3) +
  scale_fill_manual(values = c("deepskyblue3", "deepskyblue3")) +
  geom_hline(aes(yintercept = icu_available), linetype="dashed") +
  labs(x = "Additional relaxation of COVID-19 restrictions (%)",
       y = "Predicted peak in ICU census until end of 2020") +
  customThemeNoFacet+
  background_grid()+
  geom_hline(yintercept = c(-Inf, Inf)) + geom_vline(xintercept = c(-Inf, Inf))+
  scale_y_continuous(breaks=seq(0,300,50), labels=seq(0,300,50))

ggsave(paste0("ICU_reopening_region4.png"),
       plot = pplot, path = file.path(simulation_output, exp_name), width = 9, height = 4, device = "png"
)
ggsave(paste0("ICU_reopening_region4.pdf"),
       plot = pplot, path = file.path(simulation_output, exp_name), width = 9, height = 4, device = "pdf"
)




combinedPlot=F
if(combinedPlot){
  
  pdat1 <- dat %>%
    filter(date >= as.Date("2020-08-19") & outcome == "hospitalized_det") %>%
    group_by(region, reopening_multiplier_4, outcome) %>%
    filter(median.value == max(median.value)) %>%
    left_join(capacityDat) %>%
    mutate(belowCapacity = ifelse(median.value <= medsurg_available, 1, 0)) %>%
    filter(median.value <= medsurg_available & belowCapacity == 1) %>%
    filter(reopening_multiplier_4 == max(reopening_multiplier_4)) %>%
    filter(date == min(date))
  
  
  pdat2 <- dat %>%
    filter(date >= as.Date("2020-08-19") & outcome == "crit_det") %>%
    group_by(region, reopening_multiplier_4, outcome) %>%
    filter(median.value == max(median.value)) %>%
    left_join(capacityDat) %>%
    mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0)) %>%
    filter(median.value <= icu_available & belowCapacity == 1) %>%
    filter(reopening_multiplier_4 == max(reopening_multiplier_4)) %>%
    filter(date == min(date)) %>%
    group_by(region)
  
  datRib <- rbind(pdat1, pdat2)  %>% group_by(region, outcome) %>% 
    summarize(maxReopen=max(reopening_multiplier_4)) 
  

  
  pplot <- ggplot(data = datRib) +
    #geom_errorbar(aes(x = as.factor(reopening), ymin = q2.5.value, ymax = q97.5.value, group = outcome), width = 0.3) +
    geom_bar(aes(x = as.factor(region), y = maxReopen, fill = as.factor(outcome), group = as.factor(outcome)), 
             stat="identity", position="dodge", width=0.8,show.legend = F, size = 2) +
    labs(
      x = "Reopening multiplier (%)",
      y = "Peak until Jan 2020",
      title = "Reopening tolerance for ICU (blue) and non-ICU (orange) per covid region\n"
    ) +
    scale_color_manual(values=rev(c("orange","deepskyblue3")))+
    scale_fill_manual(values=rev(c("orange","deepskyblue3")))+
    background_grid() +
    customThemeNoFacet +
    theme(panel.grid.major.y = element_blank()) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))
  
  
  
  
  
  ggsave(paste0("reopening_perCovidRegion.png"),
         plot = pplot, path = file.path(simulation_output, exp_name), width = 12, height = 8, device = "png"
  )
  
  
  
  
  
  
  ########################################
  
  
  ### GEt minimum date
  pdat1 <- dat %>%
    filter(date <= as.Date("2020-12-30") ) %>%
    filter(date >= as.Date("2020-08-19") & outcome == "hospitalized_det") %>%
    group_by(region, reopening_multiplier_4, outcome) %>%
    # filter(median.value == max(median.value)) %>%
    left_join(capacityDat) %>%
    filter(median.value >= medsurg_available ) %>%
    filter(reopening_multiplier_4 == min(reopening_multiplier_4)) %>%
    filter(date == min(date))
  
  
  pdat2 <- dat %>%
    filter(date <= as.Date("2020-12-30") ) %>%
    filter(date >= as.Date("2020-08-19") & outcome == "crit_det") %>%
    group_by(region, reopening_multiplier_4, outcome) %>%
    #filter(median.value == max(median.value)) %>%
    left_join(capacityDat) %>%
    filter(median.value >= icu_available) %>%
    filter(reopening_multiplier_4 == min(reopening_multiplier_4)) %>%
    filter(date == min(date))
  
  
  datRib <- rbind(pdat1, pdat2) 
  
  summary(dat$date)
  summary(datRib$date)
  summary(pdat1$date)
  summary(pdat2$date)
  
  
  pplot <- ggplot(data = datRib) +
    # geom_errorbar(aes(x = as.factor(reopening), ymin = q2.5.value, ymax = q97.5.value, group = outcome), width = 0.3) +
    geom_point(aes(x = date, y =reopening_multiplier_4, fill = as.factor(outcome), group = as.factor(outcome)), 
               stat="identity", position="dodge", shape=21, width=0.8,show.legend = F, size = 3) +
    labs(
      x = "Date",
      y = "Reopening multiplier (%)",
      title = "Median date of capacity exceedance for ICU (blue) and non-ICU (orange) per covid region\n"
    ) +
    facet_wrap(~region)+
    scale_color_manual(values=rev(c("orange","deepskyblue3")))+
    scale_fill_manual(values=rev(c("orange","deepskyblue3")))+
    background_grid() +
    customThemeNoFacet +
    # theme(panel.grid.major.y = element_blank()) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))
  
  
  
  
  
  ggsave(paste0("date_of_overflow_perCovidRegion_.png"),
         plot = pplot, path = file.path(simulation_output, exp_name), width = 12, height = 8, device = "png"
  )
  
}



if(compareToBaseline){
  
  
  if(loadBaseline){
    exp_baseline = "20200826_IL_baseline"
    baselineDat <- read_csv(file.path(simulation_output, exp_baseline, "trajectoriesDat.csv"))
    
    ### Note use Ki_EMS-  or Ki_EMS.   not Ki_EMS_  (time varying vs input parameter)
    # colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))
    region_names =c("All", paste0("EMS-",c(1:11)))
    outcomevars <- c(
      paste0("hospitalized_det_", region_names),
      paste0("hospitalized_", region_names),
      paste0("crit_det_", region_names),
      paste0("critical_", region_names)
    )
    
    
    region_names =paste0("EMS-",c(1:11))
    outcomevars2 <- c(
      paste0("Ki_t_", region_names)
    )
    
    
    paramvars <- c("reopening_multiplier_4")
    keepvars <- c("time", "startdate", "scen_num","sample_num",  paramvars, outcomevars,outcomevars2)
    
    
    baselineDat <- baselineDat %>%
      select(keepvars) %>%
      filter(time>120) %>%
      mutate(date = as.Date(startdate) + time) %>%
      pivot_longer(cols = -c("time", "date", "startdate", "reopening_multiplier_4"), names_to = "region") %>%
      separate(region, into = c("outcome", "region"), sep = "_EMS-") %>%
      mutate(
        region = as.numeric(region),
        exp_name = exp_name,
      ) %>%
      group_by(date, region, exp_name, reopening_multiplier_4, outcome) %>%
      summarize(
        median.value = median(value, na.rm = TRUE),
        q2.5.value = quantile(value, probs = 0.025, na.rm = TRUE),
        q97.5.value = quantile(value, probs = 0.975, na.rm = TRUE)
      )
    
  }
  
  simdate = "20200826"
  exp_name <- "20200826_IL_RR_gradual_reopening_0"
  
  
  trajectoriesDat <- read_csv(file.path(simulation_output, exp_name, "trajectoriesDat.csv"))
  
  ### Note use Ki_EMS-  or Ki_EMS.   not Ki_EMS_  (time varying vs input parameter)
  # colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))
  region_names =c("All", paste0("EMS-",c(1:11)))
  outcomevars <- c(
    paste0("hospitalized_det_", region_names),
    paste0("hospitalized_", region_names),
    paste0("crit_det_", region_names),
    paste0("critical_", region_names)
  )
  
  
  region_names =paste0("EMS-",c(1:11))
  outcomevars2 <- c(
    paste0("Ki_t_", region_names)
  )
  
  
  paramvars <- c("reopening_multiplier_4")
  keepvars <- c("time", "startdate", "scen_num","sample_num",  paramvars, outcomevars,outcomevars2)
  
  
  paramvalues <- trajectoriesDat %>%
    select(keepvars) %>%
    filter(time>120) %>%
    mutate(date = as.Date(startdate) + time) %>%
    pivot_longer(cols = -c("time", "date", "startdate", "reopening_multiplier_4"), names_to = "region") %>%
    separate(region, into = c("outcome", "region"), sep = "_EMS-") %>%
    mutate(
      region = as.numeric(region),
      exp_name = exp_name,
    ) %>%
    group_by(date, region, exp_name, reopening_multiplier_4, outcome) %>%
    summarize(
      median.value = median(value, na.rm = TRUE),
      q2.5.value = quantile(value, probs = 0.025, na.rm = TRUE),
      q97.5.value = quantile(value, probs = 0.975, na.rm = TRUE)
    )
  
  
  capacityDat <- load_new_capacity() %>% mutate(region = as.character(geography_name))
  
  
  paramvalues$region <- factor(paramvalues$region, levels = c(1:11), labels = c(1:11))
  capacityDat$region <- factor(capacityDat$region, levels = c(1:11), labels = c(1:11))
  paramvalues$reopening <- round(paramvalues$reopening_multiplier_4, 2) * 100
  
  dat=paramvalues
  
  
table(baselineDat$region)
table(dat$region)
  
baselineDat$region <- as.numeric(baselineDat$region)
dat$region <- as.numeric(dat$region)
capacityDat$region <- as.numeric(capacityDat$region)

combinedDat <- rbind(  baselineDat, dat)
combinedDat <- combinedDat %>% filter(date <= as.Date("2020-12-31"))
combinedDat <- left_join(combinedDat, capacityDat,  by="region")  



belowCapacityDat_50 <-  combinedDat %>%
  filter(date >= as.Date("2020-08-19") & date <= as.Date("2020-12-30") & outcome == "crit_det") %>%
  group_by(region, reopening_multiplier_4, outcome) %>%
  filter(median.value == max(median.value)) %>%
  mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0)) %>%
  select(region, outcome, reopening_multiplier_4, belowCapacity) %>%
  unique() %>%
  filter(reopening_multiplier_4 == max(reopening_multiplier_4))%>%
  mutate(reopen="50perc")

belowCapacityDat_100 <-  combinedDat %>%
  filter(date >= as.Date("2020-08-19") & date <= as.Date("2020-12-30") & outcome == "crit_det") %>%
  group_by(region, reopening_multiplier_4, outcome) %>%
  filter(q2.5.value == max(q2.5.value)) %>%
  mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0)) %>%
  select(region, outcome, reopening_multiplier_4, belowCapacity) %>%
  unique() %>%
  filter(reopening_multiplier_4 == max(reopening_multiplier_4))%>%
  mutate(reopen="100perc")

#belowCapacityDat <- rbind(belowCapacityDat_50,belowCapacityDat_100 )



belowCapacityDat <-  combinedDat %>%
  filter(date >= as.Date("2020-08-19") & date <= as.Date("2020-12-30") & outcome == "crit_det") %>%
  group_by(region, reopening_multiplier_4, outcome) %>%
  filter(median.value == max(median.value)) %>%
  mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0)) %>%
  select(region, outcome, reopening_multiplier_4, belowCapacity) %>%
  unique()

#combinedDat <- combinedDat %>% select(-belowCapacity.x,belowCapacity.y)
combinedDat <- left_join(combinedDat, belowCapacityDat,  by=c("region","reopening_multiplier_4",'outcome'))  


pplot1 <- ggplot(data=subset(combinedDat, region %in% c(1,7,11) & outcome %in% c("crit_det") )) +
  geom_ribbon(aes(x=date, ymin=q2.5.value, ymax=q97.5.value, fill=as.factor( belowCapacity),group=as.factor(reopening_multiplier_4)),alpha=0.2)+
  geom_line(aes(x=date, y=median.value, col=as.factor( belowCapacity),group=as.factor(reopening_multiplier_4)),size=1)+
  geom_line(data=subset(combinedDat, region %in% c(1,7,11) & outcome %in% c("crit_det") & reopening_multiplier_4==0 ),
            aes(x=date, y=median.value, group=as.factor(reopening_multiplier_4)),col="black", size=1.2)+
  facet_wrap(~region, scales="free")+
  scale_color_manual(values = c("indianred", "deepskyblue3")) +
  geom_hline(aes(yintercept=icu_available), color="darkgrey", linetype="dashed",size=1)+
  labs(x="", y="ICU census", color="ICU overflow",fill="ICU overflow")+
  customThemeNoFacet
 
 
pplot2 <- combinedDat %>%
  filter(region %in% c(1,7,11) & date >= as.Date("2020-08-19") & outcome == "crit_det") %>%
  group_by(region, reopening, outcome) %>%
  filter(median.value == max(median.value)) %>%
  left_join(capacityDat) %>%
  mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0)) %>%
  ggplot() +
  geom_errorbar(aes(x = as.factor(reopening), ymin = q2.5.value, ymax = q97.5.value, col=as.factor(belowCapacity)), width = 0.3) +
  geom_point(aes(x = as.factor(reopening), y = median.value, fill = as.factor(belowCapacity)), shape = 21, show.legend = F, size = 2) +
  facet_wrap(~region, scales = "free") +
  scale_fill_manual(values = c("indianred", "deepskyblue3")) +
  geom_hline(aes(yintercept = icu_available)) +
  labs(x="Reopening multiplier (%)", y="Peak in ICU census", color="ICU overflow",fill="ICU overflow")+
  customThemeNoFacet

pplot <- plot_grid(pplot1, pplot2, ncol=1)

ggsave(paste0("ICU_reopening_perGrp.png"),
       plot = pplot, path = file.path(outdir), width = 10, height = 6, device = "png"
)
ggsave(paste0("ICU_reopening_perGrp.pdf"),
       plot = pplot, path = file.path(outdir), width = 10, height = 6, device = "pdf"
)

}

