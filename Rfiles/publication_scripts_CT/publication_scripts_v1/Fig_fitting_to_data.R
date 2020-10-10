
## ================================================
###  Figure 1a Load  data and plot over time
## ================================================


pdfdir <- "C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/project_notes/publications/covid_model_IL/pdfs_ais/"

#### Plots edited for publication
library(tidyverse)
library(cowplot)
library(scales)
library(viridis)
library(data.table)
library(stringi)
library(lubridate)
library(zoo)

theme_set(theme_cowplot())

# setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
source("load_paths.R")
source("setup.R")
source("processing_helpers.R")

# region_cols <- c()
restoreRegion_cols <- c("Central" = "red2", "Northcentral" = "dodgerblue3", "Northeast" = "chartreuse4", "Southern" = "orchid4")

startdate <- "2020-07-01"
stopdate <- "2020-12-30"
reopen <- c(0, 0.05, 0.1)
customTheme <- f_getCustomTheme()

## ================================================
###  Figure 1a Load true data and plot over time
## ================================================

f_data_plot_emr <- function(SAVE = TRUE, simulation_output = simulation_output, exp_name = exp_name) {
  library(stringi)
  library(lubridate)

  emresource <- f_loadData(data_path)

  pplot <- emresource %>%
    mutate(
      Date = as.Date(Date),
      week = week(Date),
      month = month(Date),
      restore_region = str_to_sentence(restore_region)
    ) %>%
    dplyr::group_by(Date, month, week, restore_region) %>%
    dplyr::summarize(
      suspected_and_confirmed_covid_icu = sum(suspected_and_confirmed_covid_icu),
      confirmed_covid_deaths_prev_24h = sum(confirmed_covid_deaths_prev_24h),
      confirmed_covid_icu = sum(confirmed_covid_icu),
      covid_non_icu = sum(covid_non_icu)
    ) %>%
    dplyr::group_by(month, week, restore_region) %>%
    dplyr::summarize(
      Date = max(Date),
      suspected_and_confirmed_covid_icu = mean(suspected_and_confirmed_covid_icu),
      confirmed_covid_deaths_prev_24h = mean(confirmed_covid_deaths_prev_24h),
      confirmed_covid_icu = mean(confirmed_covid_icu),
      covid_non_icu = mean(covid_non_icu)
    ) %>%
    ggplot() +
    theme_cowplot() +
    geom_line(aes(x = Date, y = suspected_and_confirmed_covid_icu, group = restore_region, col = restore_region), size = 1.3) +
    scale_color_manual(values = restoreRegion_cols) +
    scale_y_log10() +
    scale_x_date(lim = c(as.Date("2020-03-01"), as.Date("2020-08-01")))

  if (SAVE) {
    ggsave(paste0("emresource_timeline", ".pdf"),
      plot = pplot, path = file.path(simulation_output, exp_name), width = 8, height = 4, device = "pdf"
    )
  }
}

f_data_plot_LL <- function(SAVE = TRUE, simulation_output = simulation_output, exp_name = exp_name) {
  library(stringi)
  library(lubridate)

  emresource <- f_loadData(data_path)

  emresource <- emresource %>%
    mutate(
      Date = as.Date(Date),
      week = week(Date),
      month = month(Date)
    ) %>%
    f_addRestoreRegion() %>%
    mutate(restore_region = str_to_sentence(restore_region)) %>%
    filter(!is.na(restore_region)) %>%
    dplyr::select(Date, month, week, restore_region, region, LL_cases, LL_deaths, LL_admissions)

  pplot <- emresource %>%
    dplyr::group_by(Date, month, week, restore_region) %>%
    dplyr::summarize(
      cases = sum(LL_cases),
      deaths = sum(LL_deaths),
      admissions = sum(LL_admissions)
    ) %>%
    dplyr::group_by(month, week, restore_region) %>%
    dplyr::summarize(
      Date = max(Date),
      cases = mean(cases),
      deaths = mean(deaths),
      admissions = mean(admissions)
    ) %>%
    ggplot() +
    theme_cowplot() +
    geom_line(aes(x = Date, y = deaths, group = restore_region, col = restore_region), size = 1.3) +
    scale_color_manual(values = restoreRegion_cols) +
    scale_y_log10() +
    scale_x_date(lim = c(as.Date("2020-03-01"), as.Date("2020-08-01")))

  if (SAVE) {
    ggsave(paste0("LL_deaths_timeline", ".pdf"),
      plot = pplot, path = file.path(simulation_output, exp_name), width = 8, height = 4, device = "pdf"
    )
  }
}

## ================================================
###  Figure 1b  Parameter figure and transmission over time
## ================================================

f_parameter_figure <- function(exp_name) {
  trajectoriesDat <- read.csv(file.path(simulation_output, exp_name, "trajectoriesDat.csv"))

  ### Note use Ki_EMS-  or Ki_EMS.   not Ki_EMS_  (time varying vs input parameter)
  colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))

  paramvars <- c(
    paste0("backtonormal_multiplier_1_", emsname, c(1:11)),
    paste0("Ki_t_", emsname, c(1:11))
  )

  keepvars <- c("time", "startdate", paramvars)


  paramDat <- trajectoriesDat %>%
    dplyr::select(keepvars) %>%
    dplyr::mutate(date = as.Date(startdate) + time) %>%
    pivot_longer(cols = -c("time", "date", "startdate"), names_to = "name") %>%
    separate(name, into = c("param", "region"), sep = "_EMS_") %>%
    dplyr::mutate(
      region = as.numeric(region),
      exp_name = exp_name
    ) %>%
    dplyr::group_by(date, region, param, exp_name) %>%
    dplyr::summarize(value = mean(value, na.rm = TRUE)) %>%
    f_addRestoreRegion() %>%
    pivot_wider(names_from = "param", values_from = "value")

  paramDat <- data.table(paramDat, key = c("region", "exp_name", "restore_region"))
  paramDat[, social_multiplier := (1 - (Ki_t[date >= as.Date("2020-06-01") & date <= as.Date("2020-06-02")] /
    Ki_t[date >= as.Date("2020-03-15") & date <= as.Date("2020-03-16")])),
  by = c("region", "exp_name", "restore_region")
  ]

  paramDat <- as.data.frame(paramDat)
  paramDat <- paramDat %>%
    pivot_longer(cols = c("Ki_t", "backtonormal_multiplier_1", "social_multiplier"), names_to = "param")

  paramDatRR <- paramDat %>%
    dplyr::group_by(restore_region, date, param, exp_name) %>%
    dplyr::summarize(value = mean(value))


  if (timeVarying == FALSE) {
    pplot <- paramDat %>%
      filter(date >= as.Date("2020-08-01") & date <= as.Date("2020-08-02")) %>%
      ggplot() +
      theme_cowplot() +
      geom_bar(aes(x = as.factor(region), y = value, group = region), stat = "identity", size = 1.3, position = "dodge") +
      scale_color_viridis(discrete = TRUE) +
      labs(
        y = gsub("_", " ", paramname), # "% relaxation"
        title = "",
        # subtitle = "Estimated relaxation of shelter-in-place polices\n (reopening 21st June)",
        subtitle = "",
        x = "region"
      ) +
      customThemeNoFacet +
      scale_y_continuous(expand = c(0, 0)) +
      facet_grid(param ~ restore_region)

    pplot <- paramDatRR %>%
      filter(!(param %in% c("Ki_t", "d_Sym_t")) & date >= as.Date("2020-08-01") & date <= as.Date("2020-08-02")) %>%
      mutate(param = ifelse(param == "social_multiplier", "\nReduction during lockdown\n", "\nIncrease during reopening\n")) %>%
      ggplot(aes(x = restore_region, y = value, label = round(value * 100, 1), fill = restore_region)) +
      theme_cowplot() +
      geom_bar(stat = "identity", size = 1.3, position = "dodge", width = 0.7) +
      geom_text(fill = "white", vjust = 2, size = 6) +
      # geom_line(aes(x = date, y = value, col = as.factor(restore_region), group = restore_region), size = 1.3) +
      scale_fill_manual(values = restoreRegion_cols) +
      labs(
        y = "",
        subtitle = "", # Estimated change in transmission intensity\n
        title = "",
        x = "",
        col = ""
      ) +
      facet_wrap(~param, scales = "free", ncol = 1) +
      theme(
        legend.position = "None",
        strip.text.x = element_text(size = 18, face = "bold"),
        strip.text.y = element_text(size = 18, face = "bold"),
        strip.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 16)
      ) +
      scale_y_continuous(expand = c(0, 0))

    ggsave(paste0("Transmission_multiplier", ".pdf"),
      plot = pplot, path = file.path(simulation_output, exp_name), width = 10, height = 10, device = "pdf"
    )
  }




  if (timeVarying == TRUE) {
    pplot <- paramDat %>%
      filter(param %in% c("Ki_t", "d_Sym_t") & date <= as.Date("2020-09-02")) %>%
      ggplot() +
      theme_cowplot() +
      geom_line(aes(x = date, y = value, col = as.factor(restore_region), group = region), size = 1.3) +
      scale_color_manual(values = restoreRegion_cols) +
      labs(
        y = gsub("_", " ", paramname),
        subtitle = "", # Estimated change in transmission intensity\n
        title = "",
        x = "",
        col = "covid region"
      ) +
      customThemeNoFacet +
      scale_y_continuous(expand = c(0, 0)) +
      facet_grid(param ~ restore_region)

    KIplot <- paramDatRR %>%
      filter(param %in% c("Ki_t") & date <= as.Date("2020-07-15")) %>%
      ggplot() +
      theme_cowplot() +
      geom_line(aes(x = date, y = value, col = as.factor(restore_region), group = restore_region), size = 1.3) +
      scale_color_manual(values = restoreRegion_cols) +
      labs(
        y = "Transmission rate",
        subtitle = "", # Estimated change in transmission intensity\n
        title = "",
        x = "",
        col = "covid region"
      ) +
      customThemeNoFacet +
      theme(legend.position = "None") +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_date(lim = c(as.Date("2020-03-01"), as.Date("2020-08-01")))

    ggsave(paste0("Ki_timeline", ".pdf"),
      plot = KIplot, path = file.path(simulation_output, exp_name), width = 10, height = 6, device = "pdf"
    )
  }
}

### Load trajectories Dat
exp_name <- "20200731_IL_reopen_counterfactual"
source("load_paths.R")
simulation_output <- file.path(simulation_output, "contact_tracing/20200731/")
f_parameter_figure(exp_name)



## ================================================
###  REVISED FIGURE
## ================================================

### Load simulations
exp_name <- "20200825_IL_RR_baseline_0"
source("load_paths.R")
trajectoriesDat <- read_csv(file.path(simulation_output, exp_name, "trajectoriesDat_trim.csv"),
  col_types = cols_only(
    scen_num = col_guess(),
    time = col_guess(),
    startdate = col_guess(),
    hosp_det_All = col_guess(),
    crit_det_All = col_guess(),
    death_det_cumul_All = col_guess(),
    deaths_All = col_guess()
  )
)

colnames(trajectoriesDat)
simdat <- trajectoriesDat %>%
  mutate(Date = as.Date(startdate) + time) %>%
  group_by(scen_num) %>%
  arrange(scen_num, Date) %>%
  mutate(
    covid_non_icu = hosp_det_All,
    confirmed_covid_icu = crit_det_All,
    death_det = death_det_cumul_All - lag(death_det_cumul_All),
    deaths = deaths_All - lag(deaths_All)
  ) %>%
  select(scen_num, Date, covid_non_icu, confirmed_covid_icu, deaths) %>% # death_det
  pivot_longer(cols = -c("scen_num", "Date")) %>%
  mutate(source = "sim") %>%
  group_by(Date, name, source) %>%
  summarize(
    median.val = median(value, na.rm = TRUE),
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
  select(Date, region, LL_admissions, LL_deaths) %>%
  rename(
    deaths = LL_deaths,
    covid_non_icu = LL_admissions
  ) %>%
  pivot_longer(cols = -c(Date, region)) %>%
  mutate(source = "LL")



emresource <- f_loadData(data_path) %>%
  mutate(
    Date = as.Date(Date),
    week = week(Date),
    month = month(Date)
  ) %>%
  rename(deaths = confirmed_covid_deaths_prev_24h) %>%
  select(Date, region, confirmed_covid_icu, covid_non_icu, deaths) %>%
  pivot_longer(cols = -c(Date, region)) %>%
  mutate(source = "EMResource") %>%
  rbind(LLdat)

pplot7dAvr <- emresource %>%
  dplyr::group_by(Date, name, source) %>%
  dplyr::summarize(
    value = sum(value, na.rm = TRUE),
  ) %>%
  group_by(name, source) %>%
  arrange(name, Date, source) %>%
  mutate(value7 = zoo::rollmean(value, k = 7, fill = NA)) %>%
  ungroup()


customTheme <- f_getCustomTheme(fontscl = 3)
pplot1 <- ggplot(data = pplot7dAvr) +
  geom_ribbon(data = simdat, aes(x = Date, ymin = q2.5.val, ymax = q97.5.val), fill = "#fd8d3c", alpha = 0.3) +
  geom_ribbon(data = simdat, aes(x = Date, ymin = q25.val, ymax = q75.val), fill = "#fd8d3c", alpha = 0.5) +
  geom_line(data = simdat, aes(x = Date, y = median.val), col = "#fd8d3c") +
  geom_point(data = pplot7dAvr, aes(x = Date, y = value, col = source), size = 1) +
  geom_line(data = pplot7dAvr, aes(x = Date, y = value7, col = source), size = 1) +
  scale_color_manual(values = c("black", "gray50")) +
  scale_x_date(lim = c(as.Date("2020-03-01"), as.Date("2020-08-01"))) +
  facet_wrap(~name) +
  scale_y_log10(label = comma) +
  customTheme +
  labs(
    x = "",
    y = "Total cases\n(log scale)",
    color = ""
  ) +
  background_grid(major = "y") + 
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf))


ggsave(paste0("IL_fitting_plot", ".pdf"),
  plot = pplot1, path = file.path(pdfdir), width = 12, height = 5, device = "pdf"
)


pplot7dAvr <- pplot7dAvr %>% filter(name=="confirmed_covid_icu") 
simdat <- simdat  %>% filter(name=="confirmed_covid_icu") 

customTheme <- f_getCustomTheme(fontscl = 3)

pplot1 <- ggplot(data = pplot7dAvr) +
  geom_ribbon(data = simdat, aes(x = Date, ymin = q2.5.val, ymax = q97.5.val), fill = "#fd8d3c", alpha = 0.3) +
  geom_ribbon(data = simdat, aes(x = Date, ymin = q25.val, ymax = q75.val), fill = "#fd8d3c", alpha = 0.5) +
  geom_line(data = simdat, aes(x = Date, y = median.val), col = "#fd8d3c") +
  geom_point(data = pplot7dAvr, aes(x = Date, y = value, col = source), size = 1) +
  geom_line(data = pplot7dAvr, aes(x = Date, y = value7, col = source), size = 1) +
  scale_color_manual(values = c("black", "gray50")) +
   scale_x_date(lim = c(as.Date("2020-03-01"), as.Date("2020-08-30")), date_breaks = "30 days", date_labels = "%b") +
  #scale_x_date(lim = c(as.Date("2020-08-01"), as.Date("2020-12-30")), date_breaks = "30 days", date_labels = "%b") +
  customTheme +
  labs(
    x = "",
    y = "Total cases\n(log scale)",
    color = ""
  ) +
  background_grid(major = "y") + 
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf))

if(uselog)  pplot1 <- pplot1 + scale_y_log10(label = comma) 


ggsave(paste0("IL_fitting_plot", ".pdf"),
       plot = pplot1, path = file.path(pdfdir), width = 12, height = 5, device = "pdf"
)

#### per covid region

## ================================================
###  REVISED FIGURE
## ================================================

### Load simulations
exp_name <- "20200825_IL_RR_baseline_0"
source("load_paths.R")
trajectoriesDat <- read_csv(file.path(simulation_output, exp_name, "trajectoriesDat.csv"))
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
  select(keepvars) %>%
  mutate(Date = as.Date(startdate) + time) %>%
  pivot_longer(cols = -c("Date", "time", "startdate", "scen_num"), names_to = "name") %>%
  separate(name, into = c("outome", "region"), sep = "_EMS-") %>%
  pivot_wider(names_from = "outome", values_from = "value") %>%
  group_by(scen_num, region) %>%
  arrange(region, scen_num, Date) %>%
  mutate(
    covid_non_icu = hosp_det,
    confirmed_covid_icu = crit_det,
    death_det = deaths_det - lag(deaths_det),
    deaths = deaths - lag(deaths)
  ) %>%
  select(scen_num, Date, region, covid_non_icu, confirmed_covid_icu, deaths) %>% # death_det
  pivot_longer(cols = -c("scen_num", "Date", "region")) %>%
  mutate(source = "sim") %>%
  group_by(Date, name, source, region) %>%
  summarize(
    median.val = median(value, na.rm = TRUE),
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
  select(Date, region, LL_admissions, LL_deaths) %>%
  rename(
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
  rename(deaths = confirmed_covid_deaths_prev_24h) %>%
  select(Date, region, confirmed_covid_icu, covid_non_icu, deaths) %>%
  pivot_longer(cols = -c("Date", "region")) %>%
  mutate(source = "EMResource") %>%
  rbind(LLdat)

pplot7dAvr <- emresource %>%
  dplyr::group_by(Date, name, source, region) %>%
  dplyr::summarize(
    value = sum(value, na.rm = TRUE),
  ) %>%
  group_by(name, source, region) %>%
  arrange(name, Date, source) %>%
  mutate(value7 = zoo::rollmean(value, k = 7, fill = NA)) %>%
  ungroup()

simdatSub <- subset(simdat, name == "confirmed_covid_icu")
pplot7dAvrSub <- subset(pplot7dAvr, name == "confirmed_covid_icu")

simdatSub$region <- factor(simdatSub$region, levels = c(1:11), labels = paste0("Region ", c(1:11)))
pplot7dAvrSub$region <- factor(pplot7dAvrSub$region, levels = c(1:11), labels = paste0("Region ", c(1:11)))


customTheme <- f_getCustomTheme(fontscl=-3)

for(uselog in c(TRUE, FALSE)){
  
pplot <- ggplot(data = pplot7dAvrSub) +
  geom_ribbon(data = simdatSub, aes(x = Date, ymin = q2.5.val, ymax = q97.5.val), fill = "#fd8d3c", alpha = 0.3) +
  geom_ribbon(data = simdatSub, aes(x = Date, ymin = q25.val, ymax = q75.val), fill = "#fd8d3c", alpha = 0.5) +
  geom_line(data = simdatSub, aes(x = Date, y = median.val), col = "#fd8d3c") +
  geom_point(data = pplot7dAvrSub, aes(x = Date, y = value, col = source), size = 1) +
  geom_line(data = pplot7dAvrSub, aes(x = Date, y = value7, col = source), size = 1) +
  scale_color_manual(values = c("black", "gray50")) +
  customTheme +
  labs(
    x = "",
    color = ""
  ) +
  background_grid(major = "y") + 
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.position = "none") +
 # scale_x_date(lim = c(as.Date("2020-03-01"), as.Date("2020-08-30")), date_breaks = "30 days", date_labels = "%b") +
  scale_x_date(lim = c(as.Date("2020-08-01"), as.Date("2020-12-30")), date_breaks = "30 days", date_labels = "%b") +
  theme(  axis.text.x = element_text( angle = 90, vjust = 0, hjust = 1))


if(uselog){
  pplot <- pplot + labs(    y = "Total cases\n(log scale)")+
    facet_wrap(~region, ncol = 4, scales="free") +
    scale_y_log10(label = comma) 
  

  
  ggsave(paste0("IL_covid_region_fitting_plot_confirmed_covid_icu", ".pdf"),
         plot = pplot, path = file.path(pdfdir), width = 10, height = 7, device = "pdf"
  )
}

if(uselog==FALSE){

  pplot <- pplot + labs(    y = "Total cases\n") +
    facet_wrap(~region, ncol = 4, scales="free") 
  
  ggsave(paste0("IL_covid_region_fitting_plot_confirmed_covid_icu_nolog_Decv2", ".pdf"),
         plot = pplot, path = file.path(pdfdir), width = 10, height =7, device = "pdf"
  )
}

}



#### Same plot for IL
