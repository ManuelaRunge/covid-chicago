

library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")

theme_set(theme_minimal())

plot_path <- file.path(project_path,"project_notes","publications","covid_model_IL_overflow","analysis_files","figures")

## Functions
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


#### Run script 
subregions = c(1,4,11)
stopdate=  as.Date("2020-09-01") #as.Date("2020-12-31")
enddate = as.character(stopdate)

simdate <- "20201212"
exp_name_gradualReoepen <- "20201120_IL_mr_gradual_reopening2_Sep"
exp_name_baseline <- "20201212_IL_fit_to_Sep_baseline/"

sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (!dir.exists(file.path(sim_dir, "data_comparison"))) dir.create(file.path(sim_dir, "data_comparison"))

trajectoriesDat_reopen <- fread(file.path(sim_dir, exp_name_gradualReoepen, "trajectories_aggregated.csv"))
simdat_reopen <- f_simdat(dat=trajectoriesDat %>% dplyr::select(-startdate, -time),
                          grpVars = c("date", "ems", "scenario_name", "geography_modeled","reopening_multiplier_4"))

trajectoriesDat_baseline <- fread(file.path(sim_dir, exp_name_baseline, "trajectories_aggregated.csv"))
simdat_baseline <- f_simdat(dat=trajectoriesDat %>% dplyr::select(-startdate, -time))


capacityDat <- load_new_capacity(filedate = "20200915")
capacityDat <- capacityDat %>%
  filter(geography_name %in% subregions) %>%
  pivot_longer(col = -c("geography_name"), names_to = "var") %>%
  mutate(name = ifelse(var == "medsurg_available", "covid_non_icu", "confirmed_covid_icu"))
capacityDat$region <- factor(capacityDat$geography_name, levels = subregions, labels = paste0("Region ",subregions))

pplot7dAvrSub <- f_load_ref_data(stopdate=stopdate)

###------------------------------
### Plot 
###------------------------------
simcolor <- "#F6921E" 
capacitycolor <- "dodgerblue2"
  
pplot <- ggplot(data = subset(simdatSub_baseline, Date <= stopdate)) +
  geom_hline(
    data = subset(capacityDat, name == "confirmed_covid_icu"),
    aes(yintercept = value+20),
    col = "white", linetype = "dashed", size = 1
  ) +
  geom_ribbon(aes(x = Date, ymin = q2.5.val, ymax = q97.5.val),
    fill = simcolor, alpha = 0.3
  ) +
  geom_ribbon(aes(x = Date, ymin = q25.val, ymax = q75.val),
    fill = simcolor, alpha = 0.5
  ) +
  geom_line(aes(x = Date, y = median.val),
    col = simcolor, size = 1.3
  ) +
  geom_point(
    data = pplot7dAvrSub, aes(x = Date, y = value),
    size = 0.7
  ) +
  geom_line(
    data = pplot7dAvrSub, aes(x = Date, y = value7),
    size = 1
  ) +
  geom_vline(xintercept = c(as.Date("2020-09-01")), col = "#c2390d") +
  geom_hline(
    data = subset(capacityDat, name == "confirmed_covid_icu"),
    aes(yintercept = value),
    col = capacitycolor, linetype = "dashed", size = 1
  ) +
  scale_x_date(lim=c(as.Date("2020-03-01"),stopdate),
    date_breaks = "30 days", date_labels = "%b"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1)) +
  facet_wrap(name ~ region, ncol = 3, scales = "free", strip.position = "top") +
  customTheme +
  labs(
    x = "",
    color = "",
    y = "ICU census\n(EMR)"
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0, 0))

pplot


pplot1 <- ggplot(data = subset(simdatSub_baseline, region == "Region 1" &
  name == "confirmed_covid_icu" &
  Date <= stopdate)) +
  geom_rect(
    xmin = as.Date("2020-09-01"), xmax = as.Date("2020-10-01"),
    ymin = -Inf, ymax = Inf, fill = "#c2390d", alpha = 0.002
  ) +
  geom_rect(
    xmin = as.Date("2020-10-01"), xmax = as.Date("2020-12-31"),
    ymin = -Inf, ymax = Inf, fill = "#ef4137", alpha = 0.002
  ) +
  geom_vline(xintercept = c(as.Date("2020-09-01")), col = "#c2390d") +
  geom_ribbon(aes(x = Date, ymin = q2.5.val, ymax = q97.5.val),
    fill = "#F6921E", alpha = 0.3
  ) +
  geom_ribbon(aes(x = Date, ymin = q25.val, ymax = q75.val),
    fill = "#F6921E", alpha = 0.5
  ) +
  geom_line(aes(x = Date, y = median.val),
    col = "#F6921E", size = 1.3
  ) +
  geom_line(
    data = subset(simdatSub, region == "Region 1" &
      name == "confirmed_covid_icu" &
      Date <= as.Date("2020-12-31")),
    aes(x = Date, y = median.val),
    col = "#F6921E", size = 1.3
  ) +
  geom_point(
    data = subset(pplot7dAvrSub, region == "Region 1" & name == "confirmed_covid_icu"),
    aes(x = Date, y = value),
    size = 0.7
  ) +
  geom_line(
    data = subset(pplot7dAvrSub, region == "Region 1" & name == "confirmed_covid_icu"),
    aes(x = Date, y = value7),
    size = 1
  ) +
  geom_hline(
    data = subset(capacityDat, region == "Region 1" & name == "confirmed_covid_icu"),
    aes(yintercept = value),
    col = "dodgerblue2", linetype = "dashed", size = 1
  ) +
  scale_x_date(
    lim = c(as.Date("2020-03-01"), as.Date("2020-12-31")),
    date_breaks = "30 days", date_labels = "%b"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1)) +
  facet_wrap(name ~ region, ncol = 3, scales = "free", strip.position = "top") +
  geom_text(x = as.Date("2020-04-01"), y = 123.5, label = "ICU capacity", col = "dodgerblue2", size = 5) +
  customTheme +
  labs(
    x = "",
    color = "",
    y = "ICU census\n(EMR)"
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0, 0))

pplot1


### --------------------------------------
### Add reopening
pplot2 <- pplot1 +
  geom_line(
    data = subset(simdatSub_reopen, name == "confirmed_covid_icu" & Date >= as.Date("2020-09-01")),
    aes(x = Date, y = median.val, col = as.factor(reopening_multiplier_4)), size = 1.3
  )+
  scale_color_brewer(palette="Dark2")
pplot2
if(stopdate==as.Date("2020-12-31")){
  pplot2<- pplot2+
    geom_point(
      data = subset(pplot7dAvrSub, region == "Region 1" & name == "confirmed_covid_icu"),
      aes(x = Date, y = value),
      size = 0.7
    ) +
    geom_line(
      data = subset(pplot7dAvrSub, region == "Region 1" & name == "confirmed_covid_icu"),
      aes(x = Date, y = value7),
      size = 1
    ) 
}

pplot_combined <- plot_grid(pplot2, pplot, ncol = 2, rel_widths = c(1, 1), rel_heights = c(0.4, 1))
pplot_combined


#ggsave(paste0("data_comparison_plot_",enddate,".pdf"),
#  plot = pplot, path = file.path(plot_path, "pdf"), width = 14, height = 8, device = "pdf"
#) 

#ggsave(paste0("data_comparison_plot_1_",enddate,".pdf"),
#  plot = pplot1, path = file.path(plot_path, "pdf"), width = 14, height = 8, device = "pdf"
#)

ggsave(paste0("data_comparison_combined_",enddate,".pdf"),
       plot = pplot_combined, path = file.path(plot_path, "pdf"), width = 18, height = 8, device = "pdf"
)
ggsave(paste0("data_comparison_combined_",enddate,".png"),
       plot = pplot_combined, path = file.path(plot_path), width = 18, height = 8, device = "png"
)


