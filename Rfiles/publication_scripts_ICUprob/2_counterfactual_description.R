#### --------------------------------------
#### 2 - reopening and counterfactual description
#### --------------------------------------

if(!exists("wdir")){
  library(tidyverse)
  library(cowplot)
  library(data.table)
  library(raster)
  
  source("load_paths.R")
  source("setup.R")
  source("processing_helpers.R")
  source("publication_scripts_ICUprob/functions.R")
  
  theme_set(theme_cowplot())
  customTheme <- f_getCustomTheme(fontscl = 0)
  
  SAVE <- FALSE
  analysis_dir <- file.path(simulation_output, "_overflow_simulations")
  civis_dir <- file.path(simulation_output, "_simulations_for_civis")
  outdir <- file.path(file.path(project_path, "/project_notes/publications/covid_model_IL_overflow/outputs"))
  simulation_output <- file.path(simulation_output, "_overflow_simulations")
}


exp_name <- exp_name_reopen
exp_dir <- exp_dir_reopen
simdat <- f_load_single_exp(exp_dir = exp_dir, paramvars = c("reopening_multiplier_4"))
unique(simdat$geography_name)
unique(simdat$reopening_multiplier_4)
pplot <- f_icu_timeline(dat = simdat, subregions = c("1"), selected_channel = "crit_det", facetVar = "reopening_multiplier_4")
pplot <- f_icu_timeline(dat = simdat, subregions = c("4"), selected_channel = "crit_det", facetVar = "reopening_multiplier_4")
pplot <- f_icu_timeline(dat = simdat, subregions = c("11"), selected_channel = "crit_det", facetVar = "reopening_multiplier_4")

## Rebound values
rebound <- f_get_rebound_values(dat = simdat)


### Timeline plot
for (i in unique(simdat$geography_name)) {
  selected_channel = "crit_det"
  pplot <- f_icu_timeline(dat = simdat, subregions = c(i), selected_channel = selected_channel, facetVar = "reopening_multiplier_4")
  ggsave(paste0("timeline_", selected_channel, "_region_", i, ".png"),
         plot = pplot, path = file.path(exp_dir, "_plots"), width = 14, height = 8, device = "png"
  )
}

##### Peak in ICU
ICUcumul_out <- f_describe_ICU_cumul(facetVar = "reopening_multiplier_4")

##### More ICU beds needed at peak
ICUpeak_out <- f_describe_ICU_peak(facetVar = "reopening_multiplier_4")
ICUpeak_out[[2]] %>% as.data.frame()


###-------------- Figure
startdate = as.Date("2020-08-01") 
stopdate = as.Date("2020-12-31") 

capacityDat <- load_new_capacity() %>% mutate(region = as.character(geography_name))
capacityDat$region_label <- factor(capacityDat$region, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))

### Facet A 
exp_name <- exp_name_reopen
exp_dir <- exp_dir_reopen
load(file = file.path(exp_dir, "aggregatedDat_forR.Rdata"))
dat$region_label <- factor(dat$region, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))

pplot_A <- dat %>%
  filter(outcome == "crit_det" & date >= startdate & date <= as.Date(stopdate)) %>%
  filter(region == 1) %>%
  group_by(region, reopening_multiplier_4, outcome) %>%
  left_join(capacityDat) %>%
  ggplot() +
  geom_ribbon(aes(x = date, ymin = q2.5.value, ymax = q97.5.value, fill = as.factor(reopening_multiplier_4), 
                  group = reopening_multiplier_4), alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = q25.value, ymax = q75.value, fill = as.factor(reopening_multiplier_4), 
                  group = reopening_multiplier_4), alpha = 0.3) +
  geom_line(aes(x = date, y = median.value, col = as.factor(reopening_multiplier_4), 
                group = reopening_multiplier_4), show.legend = F, size = 1) +
  facet_wrap(~region_label, scales = "free") +
  labs(color = "Reopening", fill = "Reopening") +
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  geom_hline(aes(yintercept = icu_available), linetype = "dashed") +
  labs(x = "", y = "predicted ICU census") +
  customThemeNoFacet


### Facet B
exp_name <- exp_name_reopen
exp_dir <- exp_dir_reopen
load(file = file.path(exp_dir, "aggregatedDat_forR.Rdata"))
dat <- dat %>% filter(region %in% c(1,4,11))
dat$region_label <- factor(dat$region, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))


fitDat_median <- get_fit(df=dat, outputVar ="median.value") %>% rename(reopening_multiplier_4=x) %>% dplyr::select(-c(fit ,lwr ,upr))
fitDat_lwr <- get_fit(df=dat, outputVar ="q2.5.value") %>% rename(reopening_multiplier_4=x) %>% dplyr::select(-c(fit ,lwr ,upr))
fitDat_upr <- get_fit(df=dat, outputVar ="q97.5.value") %>% rename(reopening_multiplier_4=x) %>% dplyr::select(-c(fit ,lwr ,upr))

mergevars <- colnames(fitDat_median)[colnames(fitDat_median) %in% colnames(fitDat_lwr)]
fitDat <- fitDat_median %>% left_join( fitDat_lwr,  by=mergevars) %>%
  left_join(fitDat_upr, by=mergevars)
fitDat$region  <- as.numeric(fitDat$region )
fitDat$region <- factor(fitDat$region , levels=c(1:11), labels=c(1:11))

tdat <-  dat %>%
  filter(date >= as.Date(simtoday) & date <= as.Date(stopdate) &  outcome == "crit_det") %>%
  group_by(region, reopening_multiplier_4, outcome) %>%
  filter(median.value == max(median.value)) %>%
  left_join(capacityDat) %>%
  mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0))
tdat$region  <- as.numeric(tdat$region )
tdat$region <- factor(tdat$region , levels=c(1:11), labels=c(1:11))

ggplot(data=subset(fitDat,!is.na(region))) +
  geom_ribbon(aes(x =reopening_multiplier_4,ymin=q2.5.value, ymax=q97.5.value), alpha=0.3) + 
  geom_line(aes(x =reopening_multiplier_4, y = median.value), size=1) +
  geom_point(data=subset(tdat,!is.na(region)),aes(x =reopening_multiplier_4, y = median.value, fill = as.factor(belowCapacity)), 
             shape = 21, show.legend = F, size = 2) +
  facet_wrap(~region, scales = "free") +
  scale_fill_manual(values = c("indianred", "deepskyblue3")) +
  geom_hline(aes(yintercept = icu_available)) +
  labs(x = "Reopening multiplier (%)", 
       y = "Peak in ICU census until Dec 31st 2020",
       color="Below capacity") +
  customThemeNoFacet


soft_reopen <- fitDat %>%
  mutate(aboveCapacity = ifelse(median.value >= icu_available, 1, 0)) %>%
  group_by(region) %>%
  filter(aboveCapacity == 1)  %>%
  filter(reopening_multiplier_4 == min(reopening_multiplier_4))   %>%
  mutate(reopen_soft = reopening_multiplier_4/100) %>%
  dplyr::select(region, reopen_soft)

hard_reopen <- fitDat %>% 
  mutate(aboveCapacity = ifelse(q2.5.value >= icu_available, 1, 0)) %>%
  group_by(region) %>%
  filter(aboveCapacity == 1)  %>%
  filter(reopening_multiplier_4 == min(reopening_multiplier_4))  %>%
  mutate(reopen_hard = reopening_multiplier_4/100) %>%
  dplyr::select(region, reopen_hard)

custom_reopen <- left_join(soft_reopen,hard_reopen, by="region" ) %>% arrange(region)

sink(file=file.path(exp_dir, "custom_reopen_subset_v2.txt"))
print(custom_reopen)
sink()


## Get resugence numbers 
Ki_dat_all <- f_initial_and_timevarying_Ki(exp_dir=exp_dir_reopen, param=c("reopening_multiplier_4"))
Ki_dat <- Ki_dat_all %>% 
          filter(date>=simtoday & date <= simtoday+1) %>% 
          ungroup() %>% 
          dplyr::select(-date) %>%
          mutate(region=as.character(region),
                 reopening_multiplier_4=round(reopening_multiplier_4*100,0),
                 Ki_rebound=round(Ki_rebound*100,0)) 

tempdat <-  dat %>%
  filter(region %in% c(1,4,11)) %>%
  filter(date >= simtoday & date <= as.Date(stopdate) &  outcome == "crit_det") %>%
  group_by(region, reopening_multiplier_4, outcome) %>%
  filter(median.value == max(median.value)) %>%
  left_join(capacityDat) %>%
  left_join(Ki_dat, by=c("reopening_multiplier_4","region")) %>%
  mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0),
         aboveCapacity = ifelse(q2.5.value >=icu_available, 1, 0))


###
tempdat <- tempdat %>% 
            group_by(region, Ki_rebound ,belowCapacity,aboveCapacity,icu_available) %>%
            summarize(median.value=mean(median.value),
                      q2.5.value=mean(q2.5.value),
                      q97.5.value=mean(q97.5.value)) %>%
             group_by(region,belowCapacity) %>%
             mutate(Ki_max_belowCapacity = ifelse(belowCapacity == 1, max(Ki_rebound), NA)) %>%
             group_by(region,aboveCapacity) %>%
             mutate(Ki_min_aboveCapacity = ifelse(aboveCapacity==1, min(Ki_rebound), NA)) %>%
             group_by(region) %>%
             mutate(rebound = ifelse(belowCapacity == 1 &  Ki_rebound==Ki_max_belowCapacity, "soft", NA),
                    rebound = ifelse( aboveCapacity==1 &  Ki_rebound== Ki_min_aboveCapacity, "hard", rebound)) %>%
              arrange(region)

ggplot(data=tempdat)+geom_point(aes(x=reopening_multiplier_4, y=Ki_rebound, col=region))

pplot_B <- ggplot(data=tempdat) +
  geom_errorbar(aes(x = as.factor(Ki_rebound), ymin = q2.5.value, ymax = q97.5.value), width = 0.2,size=1) +
  geom_point(aes(x = as.factor(Ki_rebound), y = median.value), fill='black',  shape = 21, show.legend = F, size = 2.5) +
  geom_errorbar(data=subset(tempdat, !is.na(rebound)), aes(x = as.factor(Ki_rebound), ymin = q2.5.value, ymax = q97.5.value,col = as.factor(rebound)), width = 0.2,size=1) +
  geom_point(data=subset(tempdat, !is.na(rebound)), aes(x = as.factor(Ki_rebound), y = median.value, fill = as.factor(rebound)),  shape = 21, size = 2.5) +
  facet_wrap(~region, scales = "free") +
  scale_color_manual(values = c("indianred", "deepskyblue3")) +
  scale_fill_manual(values = c("indianred", "deepskyblue3")) +
  geom_hline(aes(yintercept = icu_available)) +
  labs(x = "Resurgence in transmission (%)", 
       y = "Peak in ICU census",
        col="Resurgence\nscenario",
       fill="Resurgence\nscenario") +
  customThemeNoFacet+
  scale_y_continuous(labels=comma)
rm(tempdat)

### Facet C
#pplot_C <- Rt_dat ....


pplot_2 <- plot_grid(pplot_B, pplot_C)
pplot_2 <- plot_grid(pplot_A, pplot_2)


ggsave(paste0("timeline_ICU_perCovidRegion.png"),
       plot = pplot_2, path = file.path(plot_dir), width = 14, height = 8, device = "png"
)
ggsave(paste0("timeline_ICU_perCovidRegion.pdf"),
       plot = pplot_2, path = file.path(plot_dir), width = 14, height = 8, device = "pdf"
)
rm(pplot_2,pplot_B, pplot_C,pplot_B, pplot_A)

