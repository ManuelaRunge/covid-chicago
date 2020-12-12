

library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")

simdate <- "20201121"
exp_name <- "20201107_IL_mr_resim_20201102_IL_mr_sm8and9"
exp_name_gradualReoepen <- "20201120_IL_mr_gradual_reopening2_Sep"
sim_dir <- file.path(simulation_output,'_overflow_simulations',simdate)
trajectoriesDat <- fread(file.path(sim_dir, exp_name, "trajectories_aggregated.csv"))


# simdate <- "20200919"
# exp_name_baseline <- "20200915_IL_RR_baseline_0"
# exp_name_gradualReoepen <- "20200919_IL_gradual_reopening_sm7"
# sim_dir <- file.path(simulation_output,'_overflow_simulations',simdate)
# trajectoriesDat <- fread(file.path(sim_dir, exp_name_baseline, "trajectoriesDat.csv"))
# region_names <- c(paste0("EMS-", c(1,4,11)))

simdat <- trajectoriesDat %>%
dplyr::select(-startdate, -time) %>%
 filter(geography_modeled  %in% c("covidregion_1","covidregion_4","covidregion_11")) %>%
  pivot_longer(cols = -c("date","ems","scenario_name","geography_modeled")) %>%
  mutate(name=gsub("_m","__m",name),
         name=gsub("_9","__9",name),
         name=gsub("_5","__5",name)) %>%
  separate(name , into=c("channel","stat"), sep="__") %>%
  filter(channel %in% c("hosp_det","crit_det","new_detected_deaths","new_deaths")) %>%
  pivot_wider(names_from="channel", values_from="value") %>%
  rename(
    covid_non_icu = hosp_det,
    confirmed_covid_icu = crit_det,
    death_det = new_detected_deaths,
    deaths =new_deaths
  ) %>%
  pivot_longer(cols = -c("date","ems","scenario_name","geography_modeled","stat")) %>%
  mutate(source = "sim") %>%
  pivot_wider(names_from="stat", values_from="value") %>%
  dplyr::rename(
    median.val = median,
    q25.val = `50CI_lower`,
    q75.val = `50CI_upper`,
    q2.5.val = `95CI_lower`,
    q97.5.val = `95CI_upper`
  )%>% mutate(Date=as.Date(date))

simdat$region <- factor(simdat$ems, levels = paste0("EMS-",c(1,4,11)), labels = paste0("Region ",  c(1,4,11)))


capacityDat <- load_new_capacity(filedate ="20200915")
capacityDat <- capacityDat %>% filter(geography_name %in% c(1,4,11)) %>%
  pivot_longer(col=-c("geography_name"), names_to="var") %>% 
  mutate(name = ifelse(var=="medsurg_available", "covid_non_icu","confirmed_covid_icu"))

capacityDat$region <- factor(capacityDat$geography_name , levels = c(1,4,11), labels = paste0("Region ", c(1,4,11)))

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
pplot7dAvr$region <- factor(pplot7dAvr$region, levels =  c(1,4,11), labels = paste0("Region ", c(1,4,11)))


simdatSub <- subset(simdat, 
                    Date <= as.Date('2020-12-31') &
                      name %in% c("confirmed_covid_icu","covid_non_icu","deaths") &
                      region %in% c('Region 1','Region 4','Region 11')&
                      Date > as.Date("2020-03-01") )

pplot7dAvrSub <- subset(pplot7dAvr,
                          Date > as.Date("2020-03-01") & 
                          Date <= as.Date('2020-09-01') &
                          name %in% c("confirmed_covid_icu","covid_non_icu","deaths")  & 
                          region %in% c('Region 1','Region 4','Region 11'))%>% 
                        filter((name=='confirmed_covid_icu' & source=='EMResource')  | 
                                            (name=='covid_non_icu' & source=='EMResource') |  
                                            (name=='deaths' & source=='LL'))

pplot <- ggplot(data = subset(simdatSub,  Date<=as.Date("2020-11-29"))) +
  geom_ribbon( aes(x = Date, ymin = q2.5.val, ymax = q97.5.val), fill = "#F77189", alpha = 0.3) +
  geom_ribbon( aes(x = Date, ymin = q25.val, ymax = q75.val), fill = "#F77189", alpha = 0.5) +
  # geom_line( aes(x = Date, y = median.val), col = "#F77189",size=1.5) +
  geom_line( aes(x = Date, y = median.val), col = "#F77189",size=1.3) +
  geom_point(data = pplot7dAvrSub, aes(x = Date, y = value), size = 0.7) +
  geom_line(data = pplot7dAvrSub, aes(x = Date, y = value7), size = 1) +
  scale_color_manual(values = c("black", "gray50")) +
  geom_hline(data=subset(capacityDat, name=="confirmed_covid_icu"),
             aes(yintercept=value),col='red', linetype='dashed', size=1.2)+
  scale_x_date(lim = c(as.Date("2020-03-01"), as.Date("2020-12-31")), date_breaks = "30 days", date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1))+
  facet_wrap(name~region, ncol = 3, scales = "free") +
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
  scale_y_log10()

ggsave(paste0("fitting_plot.pdf"),
       plot = pplot, path = getwd(), width = 14, height = 8, device = "pdf"
)

###--------------------------------------
###--------------------------------------
# colnames(trajectoriesDat)

trajectoriesDat <- fread(file.path(sim_dir, exp_name_gradualReoepen, "trajectoriesDat.csv"))

region_names <- c(paste0("EMS-", c(1,4,11)))
outcomevars <- c(
  paste0("deaths_", region_names),
  paste0("deaths_det_", region_names),
  paste0("hosp_det_", region_names),
  paste0("crit_det_", region_names)
)

keepvars <- c("time", "startdate", "scen_num", outcomevars)
reopenDat1 <- trajectoriesDat %>%
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
reopenDat1$region <- factor(reopenDat1$region, levels = c(1,4,11), labels = paste0("Region ",  c(1,4,11)))


# reopenDat <- fread(file.path(sim_dir, exp_name_gradualReoepen, "trajectories_aggregated.csv"))
# # colnames(trajectoriesDat)
# reopenDat1 <- reopenDat %>%
#  filter(geography_modeled  %in% c("covidregion_1","covidregion_4","covidregion_11")) %>%
#   pivot_longer(cols = -c("date","ems","scenario_name","geography_modeled",'reopening_multiplier_4')) %>%
#   mutate(name=gsub("_m","__m",name),
#          name=gsub("_9","__9",name),
#          name=gsub("_5","__5",name)) %>%
#   separate(name , into=c("channel","stat"), sep="__") %>%
#   filter(channel %in% c("hosp_det","crit_det","new_detected_deaths","new_deaths")) %>%
#   pivot_wider(names_from="channel", values_from="value") %>%
#   rename(
#     covid_non_icu = hosp_det,
#     confirmed_covid_icu = crit_det,
#     death_det = new_detected_deaths,
#     deaths =new_deaths
#   ) %>%
#   pivot_longer(cols = -c("date","ems","scenario_name","geography_modeled",'reopening_multiplier_4',"stat")) %>%
#   mutate(source = "sim") %>%
#   pivot_wider(names_from="stat", values_from="value") %>%
#   dplyr::rename(
#     median.val = median,
#     q25.val = `50CI_lower`,
#     q75.val = `50CI_upper`,
#     q2.5.val = `95CI_lower`,
#     q97.5.val = `95CI_upper`
#   )%>% mutate(Date=as.Date(date))

reopenDat1$region <- factor(reopenDat1$geography_modeled, 
                            levels =paste0("covidregion_", c(1,4,11)), 
                            labels = paste0("Region ",  c(1,4,11)))


simdatSub <- subset(simdat, 
                    Date <= as.Date('2020-12-31') &
                      name %in% c("confirmed_covid_icu","covid_non_icu","deaths") &
                      region %in% c('Region 1','Region 4','Region 11')&
                      Date > as.Date("2020-06-01") )

reopenDat1Sub <- subset(reopenDat1, 
                    Date <= as.Date('2020-12-31') &
                      name %in% c("confirmed_covid_icu","covid_non_icu","deaths") &
                      region %in% c('Region 1','Region 4','Region 11')&
                      Date > as.Date("2020-06-01")&
                      reopening_multiplier_4 <=0.2)


pplot7dAvrSub <- subset(pplot7dAvr,
                        Date <= as.Date('2020-09-01') &
                          name %in% c("confirmed_covid_icu","covid_non_icu","deaths")  & 
                          region %in% c('Region 1','Region 4','Region 11')&
                          Date > as.Date("2020-06-01"))

pplot7dAvrSub <- pplot7dAvrSub %>% filter((name=='confirmed_covid_icu' & source=='EMResource')  | 
                                            (name=='covid_non_icu' & source=='EMResource') |  
                                            (name=='deaths' & source=='LL'))

pplot <- ggplot(data = subset(simdatSub,  Date<=as.Date("2020-12-31"))) +
 # geom_ribbon(data=subset(reopenDat1Sub,Date >= as.Date("2020-06-01")), 
 #             aes(x = Date, ymin = q2.5.val, ymax = q97.5.val,fill = as.factor(reopening_multiplier_4)),  alpha = 0.3) +
 # geom_ribbon(data=reopenDat2Sub, aes(x = Date, ymin = q2.5.val, ymax = q97.5.val), fill = "#F77189", alpha = 0.3) +
  geom_line(data=subset(reopenDat1Sub,Date >= as.Date("2020-06-01")), 
            aes(x = Date, y = median.val,col=as.factor(reopening_multiplier_4)), size=1.3) +
  geom_ribbon( aes(x = Date, ymin = q2.5.val, ymax = q97.5.val), fill = "#F77189", alpha = 0.3) +
  geom_ribbon( aes(x = Date, ymin = q25.val, ymax = q75.val), fill = "#F77189", alpha = 0.5) +
  geom_line( aes(x = Date, y = median.val), col = "#F77189",size=1.3) +
  #geom_line(data=reopenDat2Sub, aes(x = Date, y = median.val), col = "#F77189",size=1.3) +
  geom_point(data = pplot7dAvrSub, aes(x = Date, y = value), size = 0.7) +
  geom_line(data = pplot7dAvrSub, aes(x = Date, y = value7), size = 1) +
  geom_hline(data=subset(capacityDat, name=="covid_non_icu"), aes(yintercept=value),col='red', linetype='dashed', size=1.2)+
  scale_x_date(lim = c(as.Date("2020-06-01"), as.Date("2020-12-31")), date_breaks = "30 days", date_labels = "%b") +
  facet_wrap(name~region, ncol = 3, scales = "free") +
  customTheme +
 # scale_color_manual(values = c("black", "gray50")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1))+
  labs(
    x = "",
    color = "",
    y="ICU census\n(EMR)"
  ) +
  background_grid(major = "y") +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.position = "none") #+
 # scale_y_log10()

ggsave(paste0("fitting_plot2.pdf"),
       plot = pplot, path = getwd(), width = 14, height = 8, device = "pdf"
)
