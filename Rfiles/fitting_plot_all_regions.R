
library(tidyverse)
library(cowplot)
library(data.table)
library(raster)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

theme_set(theme_cowplot())
customTheme <- f_getCustomTheme(fontscl = -3)


exp_name <- "20201124_IL_ae_v8_fitting"
source("load_paths.R")
trajectoriesDat <- fread(file.path(simulation_output, exp_name, "trajectoriesDat.csv"))
# colnames(trajectoriesDat)

region_names <- c(paste0("EMS-", c(1:11)))

outcomevars <- c(
  paste0("deaths_", region_names),
  paste0("deaths_det_", region_names),
  paste0("hosp_det_", region_names),
  paste0("crit_det_", region_names)
)

keepvars <- c("time", "startdate", "scen_num", outcomevars)

simdat <- trajectoriesDat %>%
  dplyr::select(keepvars) %>%
  mutate(Date = as.Date(startdate) + time) %>%
  pivot_longer(cols = -c("Date", "time", "startdate", "scen_num"), names_to = "name") %>%
  separate(name, into = c("outome", "region"), sep = "_EMS-") %>%
  pivot_wider(names_from = "outome", values_from = "value") %>%
  dplyr::group_by(scen_num, region) %>%
  arrange(region, scen_num, Date) %>%
  mutate(
    covid_non_icu = hosp_det,
    confirmed_covid_icu = crit_det,
    death_det = deaths_det - lag(deaths_det),
    deaths = deaths - lag(deaths)
  ) %>%
  dplyr::select(scen_num, Date, region, covid_non_icu, confirmed_covid_icu, deaths) %>% # death_det
  pivot_longer(cols = -c("scen_num", "Date", "region")) %>%
  mutate(source = "sim") %>%
  dplyr::group_by(Date, name, source, region) %>%
  dplyr::summarize(
    median.val = median(value, na.rm = TRUE),
    mean.val = mean(value, na.rm = TRUE),
    n.val = n(),
    q25.val = quantile(value, probs = 0.25, na.rm = TRUE),
    q75.val = quantile(value, probs = 0.75, na.rm = TRUE),
    q2.5.val = quantile(value, probs = 0.025, na.rm = TRUE),
    q97.5.val = quantile(value, probs = 0.975, na.rm = TRUE)
  )



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



simdat$region <- factor(simdat$region, levels = c(1:11), labels = paste0("Region ", c(1:11)))
pplot7dAvr$region <- factor(pplot7dAvr$region, levels = c(1:11), labels = paste0("Region ", c(1:11)))

simdatSub <- subset(simdat, name == "confirmed_covid_icu")
pplot7dAvrSub <- subset(pplot7dAvr, name == "confirmed_covid_icu")

ggplot(data = subset(simdatSub, Date<=as.Date("2020-11-29"))) +
  geom_ribbon( aes(x = Date, ymin = q2.5.val, ymax = q97.5.val), fill = "#F77189", alpha = 0.3) +
  geom_ribbon( aes(x = Date, ymin = q25.val, ymax = q75.val), fill = "#F77189", alpha = 0.5) +
 # geom_line( aes(x = Date, y = median.val), col = "#F77189",size=1.5) +
  geom_line( aes(x = Date, y = mean.val), col = "#F77189",size=1.5) +
  geom_point(data = pplot7dAvrSub, aes(x = Date, y = value, col = source), size = 0.7) +
  geom_line(data = pplot7dAvrSub, aes(x = Date, y = value7, col = source), size = 1) +
  scale_color_manual(values = c("black", "gray50")) +
  customTheme +
  labs(
    x = "",
    color = "",
    y="ICU census\n(EMR)"
  ) +
  background_grid(major = "y") +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.position = "none") +
  scale_x_date(lim = c(as.Date("2020-03-01"), as.Date("2020-11-29")), date_breaks = "30 days", date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1))+
  facet_wrap(~region, ncol = 4, scales = "free") 


############################################################
## For paper
############################################################




exp_name <- "20200915_IL_RR_baseline_0"
source("load_paths.R")
trajectoriesDat <- fread(file.path(simulation_output, exp_name, "trajectoriesDat.csv"))
# colnames(trajectoriesDat)

region_names <- c(paste0("EMS-", c(1:11)))

outcomevars <- c(
  paste0("deaths_", region_names),
  paste0("deaths_det_", region_names),
  paste0("hosp_det_", region_names),
  paste0("crit_det_", region_names)
)

keepvars <- c("time", "startdate", "scen_num", outcomevars)

simdat <- trajectoriesDat %>%
  dplyr::select(keepvars) %>%
  mutate(Date = as.Date(startdate) + time) %>%
  pivot_longer(cols = -c("Date", "time", "startdate", "scen_num"), names_to = "name") %>%
  separate(name, into = c("outome", "region"), sep = "_EMS-") %>%
  pivot_wider(names_from = "outome", values_from = "value") %>%
  dplyr::group_by(scen_num, region) %>%
  arrange(region, scen_num, Date) %>%
  mutate(
    covid_non_icu = hosp_det,
    confirmed_covid_icu = crit_det,
    death_det = deaths_det - lag(deaths_det),
    deaths = deaths - lag(deaths)
  ) %>%
  dplyr::select(scen_num, Date, region, covid_non_icu, confirmed_covid_icu, deaths) %>% # death_det
  pivot_longer(cols = -c("scen_num", "Date", "region")) %>%
  mutate(source = "sim") %>%
  dplyr::group_by(Date, name, source, region) %>%
  dplyr::summarize(
    median.val = median(value, na.rm = TRUE),
    mean.val = mean(value, na.rm = TRUE),
    n.val = n(),
    q25.val = quantile(value, probs = 0.25, na.rm = TRUE),
    q75.val = quantile(value, probs = 0.75, na.rm = TRUE),
    q2.5.val = quantile(value, probs = 0.025, na.rm = TRUE),
    q97.5.val = quantile(value, probs = 0.975, na.rm = TRUE)
  )



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



simdat$region <- factor(simdat$region, levels = c(1:11), labels = paste0("Region ", c(1:11)))
pplot7dAvr$region <- factor(pplot7dAvr$region, levels = c(1:11), labels = paste0("Region ", c(1:11)))


simdatSub <- subset(simdat, 
                    Date <= as.Date('2020-12-31') &
                      name %in% c("confirmed_covid_icu","covid_non_icu","deaths") &
                      region %in% c('Region 1','Region 4','Region 11'))

pplot7dAvrSub <- subset(pplot7dAvr,
                        Date <= as.Date('2020-09-01') &
                          name %in% c("confirmed_covid_icu","covid_non_icu","deaths")  & 
                          region %in% c('Region 1','Region 4','Region 11'))

pplot7dAvrSub <- pplot7dAvrSub %>% filter((name=='confirmed_covid_icu' & source=='EMResource')  | 
                                            (name=='covid_non_icu' & source=='EMResource') |  
                                            (name=='deaths' & source=='LL'))


capacityDat <- load_new_capacity(filedate ="20200915")
capacityDat <- capacityDat %>% filter(geography_name %in% c(1,4,11)) %>%
                    pivot_longer(col=-c("geography_name"), names_to="var") %>% 
                    mutate(name = ifelse(var=="medsurg_available", "confirmed_covid_icu","covid_non_icu"))
capacityDat$region <- factor(capacityDat$geography_name , levels = c(1:11), labels = paste0("Region ", c(1:11)))


pplot <- ggplot(data = subset(simdatSub, Date<=as.Date("2020-11-29"))) +
  geom_ribbon( aes(x = Date, ymin = q2.5.val, ymax = q97.5.val), fill = "#F77189", alpha = 0.3) +
  geom_ribbon( aes(x = Date, ymin = q25.val, ymax = q75.val), fill = "#F77189", alpha = 0.5) +
  # geom_line( aes(x = Date, y = median.val), col = "#F77189",size=1.5) +
  geom_line( aes(x = Date, y = mean.val), col = "#F77189",size=1.3) +
  geom_point(data = pplot7dAvrSub, aes(x = Date, y = value), size = 0.7) +
  geom_line(data = pplot7dAvrSub, aes(x = Date, y = value7), size = 1) +
  scale_color_manual(values = c("black", "gray50")) +
  geom_hline(data=subset(capacityDat, name=="covid_non_icu"), aes(yintercept=value),col='red', linetype='dashed', size=1.2)+
  customTheme +
  labs(
    x = "",
    color = "",
    y="ICU census\n(EMR)"
  ) +
  background_grid(major = "y") +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.position = "none") +
  scale_x_date(lim = c(as.Date("2020-03-01"), as.Date("2020-11-29")), date_breaks = "30 days", date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1))+
  facet_wrap(name~region, ncol = 3, scales = "free") 

ggsave(paste0("fitting_plot.pdf"),
       plot = pplot, path = getwd(), width = 14, height = 8, device = "pdf"
)
