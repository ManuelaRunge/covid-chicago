library(tidyverse)
library(cowplot)
library(scales)
library(viridis)
require(readr)
source("load_paths.R")
source("processing_helpers.R")

theme_set(theme_cowplot())


simdate <- "20200826"
exp_name <- "20200826_IL_gradual_reopening"
stopdate <- as.Date("2020-12-30")

trajectoriesDat <- read_csv(file.path(simulation_output, exp_name, "trajectoriesDat_trim.csv"))

### Note use Ki_EMS-  or Ki_EMS.   not Ki_EMS_  (time varying vs input parameter)
# colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))
paramname <- "critical_det_"
emsname <- "EMS-"
paramvars <- paste0(paramname, emsname, c(1:11))
keepvars <- c("time", "startdate", "reopening_multiplier_4", paramvars)
paramname <- "hospitalized_det_"
paramvars <- paste0(paramname, emsname, c(1:11))
keepvars <- c(keepvars, paramvars)


paramvalues <- trajectoriesDat %>%
  select(keepvars) %>%
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


capacityDat <- load_new_capacity() %>% 
                  mutate(region = as.character(geography_name))


paramvalues$region <- factor(paramvalues$region, levels = c(1:11), labels = c(1:11))
capacityDat$region <- factor(capacityDat$region, levels = c(1:11), labels = c(1:11))
paramvalues$reopening <- round(paramvalues$reopening_multiplier_4, 2) * 100


library(RColorBrewer)
getPalette <- colorRampPalette(brewer.pal(9, "PuBuGn"))(12)


pplot <- paramvalues %>%
  filter(date >= as.Date(Sys.Date()) & outcome == "hospitalized_det") %>%
  group_by(region, reopening, outcome) %>%
  filter(median.value == max(median.value)) %>%
  left_join(capacityDat) %>%
  mutate(belowCapacity = ifelse(median.value <= medsurg_available, 1, 0)) %>%
  ggplot() +
  geom_errorbar(aes(x = as.factor(reopening), ymin = q2.5.value, ymax = q97.5.value), width = 0.3) +
  geom_point(aes(x = as.factor(reopening), y = median.value, fill = as.factor(belowCapacity)), shape = 21, show.legend = F, size = 2) +
  facet_wrap(~region, scales = "free") +
  scale_fill_manual(values = c("indianred", "deepskyblue3")) +
  geom_hline(aes(yintercept = medsurg_available)) +
  labs(x = "Reopening multiplier (%)", y = "Peak in non-ICU census until Jan 2020") +
  customThemeNoFacet

ggsave(paste0("nonICU_reopening_perCovidRegion.png"),
       plot = pplot, path = file.path(simulation_output, exp_name), width = 12, height = 8, device = "png"
)



pplot <- paramvalues %>%
  filter(date >= as.Date(Sys.Date()) & outcome == "critical_det") %>%
  group_by(region, reopening, outcome) %>%
  filter(median.value == max(median.value)) %>%
  left_join(capacityDat) %>%
  mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0)) %>%
  ggplot() +
  geom_errorbar(aes(x = as.factor(reopening), ymin = q2.5.value, ymax = q97.5.value), width = 0.3) +
  geom_point(aes(x = as.factor(reopening), y = median.value, fill = as.factor(belowCapacity)), shape = 21, show.legend = F, size = 2) +
  facet_wrap(~region, scales = "free") +
  scale_fill_manual(values = c("indianred", "deepskyblue3")) +
  geom_hline(aes(yintercept = icu_available)) +
  labs(x = "Reopening multiplier (%)", y = "Peak in ICU census until Jan 2020") +
  customThemeNoFacet

ggsave(paste0("ICU_reopening_perCovidRegion.png"),
       plot = pplot, path = file.path(simulation_output, exp_name), width = 12, height = 8, device = "png"
)



pplot <- paramvalues %>%
  filter(region%in% c(4)) %>%
  #filter(reopening <=10) %>%
  filter(date >=  as.Date(Sys.Date()) & date <= stopdate & outcome == "critical_det") %>%
  group_by(region, reopening, outcome) %>%
  filter(median.value == max(median.value)) %>%
  group_by(region, reopening, outcome) %>%
  filter(date == min(date)) %>%
  left_join(capacityDat) %>%
  mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0)) %>%
  ggplot() +
  geom_errorbar(aes(x = as.factor(reopening), ymin = q2.5.value, ymax = q97.5.value), width = 0.2) +
  geom_point(aes(x = as.factor(reopening), y = median.value, fill = as.factor(belowCapacity)), shape = 21, show.legend = F, size = 3) +
  scale_fill_manual(values = c("deepskyblue3", "deepskyblue3")) +
  geom_hline(aes(yintercept = icu_available), linetype="dashed") +
  labs(x = "Additional relaxation of COVID-19 restrictions (%)",
       y = "Predicted peak in ICU census until end of 2020") +
  customThemeNoFacet+
  background_grid()+
  geom_hline(yintercept = c(-Inf, Inf)) + geom_vline(xintercept = c(-Inf, Inf)) #+
 # scale_y_continuous(breaks=seq(0,300,50), labels=seq(0,300,50))

ggsave(paste0("ICU_reopening_region4.png"),
       plot = pplot, path = file.path(simulation_output, exp_name), width = 6, height = 4, device = "png"
)
ggsave(paste0("ICU_reopening_region4.pdf"),
       plot = pplot, path = file.path(simulation_output, exp_name), width = 6, height = 4, device = "pdf"
)




combinedPlot=F
if(combinedPlot){
  
  pdat1 <- paramvalues %>%
    filter(date >= as.Date(Sys.Date())  & outcome == "hospitalized_det") %>%
    group_by(region, reopening, outcome) %>%
    filter(median.value == max(median.value)) %>%
    filter(date ==min(date)) %>%
    filter(reopening ==  max(reopening)) %>%
    left_join(capacityDat) %>%
    mutate(belowCapacity = ifelse(median.value <= medsurg_available, 1, 0)) %>%
    filter(median.value <= medsurg_available & belowCapacity == 1)
  
  
  pdat2 <- paramvalues %>%
    filter(date >=as.Date(Sys.Date())  & outcome == "critical_det") %>%
    group_by(region, reopening, outcome) %>%
    filter(median.value == max(median.value)) %>%
    filter(date ==min(date)) %>%
    filter(reopening ==  max(reopening)) %>%
    left_join(capacityDat) %>%
    mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0)) %>%
    filter(median.value <= icu_available & belowCapacity == 1) %>%
    group_by(region) 
  
  dat <- rbind(pdat1, pdat2)
  
  datRib <- dat %>% 
    group_by(region, outcome) %>% 
    summarize(maxReopen=max(reopening_multiplier_4)) 
  
  
  pplot <- ggplot(data = dat) +
    #geom_hline(aes(yintercept = icu_available), col = "deepskyblue3", linetype = "dashed") +
    #geom_hline(aes(yintercept = medsurg_available), col = "orange", linetype = "dashed") +
    # geom_rect(data=datRib, aes(xmin=-Inf, xmax=maxReopen, ymin=-Inf, ymax=Inf, fill=as.factor(outcome)), alpha=0.3) +
    geom_errorbar(aes(x = as.factor(reopening), ymin = q2.5.value, ymax = q97.5.value, group = outcome), width = 0.3) +
    #  geom_errorbar(data=pdat2, aes(x=as.factor(reopening) , ymin=q2.5.value, ymax=q97.5.value), width=0.3) +
      geom_point(data=pdat2,aes(x=as.factor(reopening), y=median.value, group=as.factor(belowCapacity)),fill="deepskyblue3", shape=21, show.legend = F, size=2) +
   facet_wrap(~region) +
    labs(
      x = "Region",
      y = "Reopening multiplier (%)",
      title = "Reopening tolerance for ICU (blue) and non-ICU (orange) per covid region\n"
    ) +
    scale_fill_manual(values=c("deepskyblue3","orange")) + 
    background_grid() +
    customThemeNoFacet +
    theme(panel.grid.major.x = element_blank()) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))
  
  pplot <- ggplot(data = dat) +
     #geom_errorbar(data=pdat2, aes(x=as.factor(reopening) , ymin=q2.5.value, ymax=q97.5.value), width=0.3) +
    geom_bar(aes(x = as.factor(region), y = as.factor(reopening), fill = as.factor(outcome), group = interaction(outcome)), 
             show.legend = F, size = 2, stat="identity", position="dodge", width=0.8) +
    labs(
      x = "Region",
      y = "Reopening multiplier (%)",
      title = "Reopening tolerance for ICU (blue) and non-ICU (orange) per covid region\n"
    ) +
    scale_fill_manual(values=c("deepskyblue3","orange")) + 
    background_grid() +
    customThemeNoFacet +
    theme(panel.grid.major.x = element_blank()) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))
  
  
  ggsave(paste0("reopening_tolerance_perCovidRegion.png"),
         plot = pplot, path = file.path(simulation_output, exp_name), width = 12, height = 8, device = "png"
  )
  
}