### =============================================
### Plot ICU over time per region
### =============================================

library(tidyverse)
library(cowplot)
library(data.table)

theme_set(theme_minimal())
capacity_col <- "#2a3b90"

Location <- "Local"
simdate <- "20210107"
exp_stem <- paste0(simdate, "_IL_regreopen")

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")
customTheme <- f_getCustomTheme()

custom_date_breaks <- c(
  as.Date("2020-08-01"), as.Date("2020-09-01"),
  as.Date("2020-10-01"), as.Date("2020-11-01"),
  as.Date("2020-12-01"), as.Date("2021-01-01")
)

custom_date_breaks_JanOct <- c(
  as.Date("2020-01-01"), as.Date("2020-02-01"), as.Date("2020-03-01"),
  as.Date("2020-04-01"), as.Date("2020-05-01"), as.Date("2020-06-01"),
  as.Date("2020-07-01"), as.Date("2020-08-01"), as.Date("2020-09-01"),
  as.Date("2020-10-01")
)

if (Location == "Local") sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (Location == "NUCLUSTER") sim_dir <- "/projects/p30781/covidproject/covid-chicago/_temp"
if (!dir.exists(file.path(sim_dir, "ICU_bar_plots"))) dir.create(file.path(sim_dir, "ICU_bar_plots"))

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep(exp_stem, exp_names)]
exp_names <- exp_names[c(grep("Date", exp_names), grep("counterfactual", exp_names))]

datList <- list()
for(exp_name in exp_names){
  dat <- fread(file.path(sim_dir, exp_name ,"trajectories_aggregated.csv")) %>% 
              filter(date >=as.Date("2020-07-01") & ems %in% c("EMS-1","EMS-4","EMS-11")) %>%
              select(date, ems, geography_modeled,scenario_name, time_of_trigger,
                     crit_det_median, crit_det_50CI_lower, crit_det_50CI_upper, 
                     crit_det_95CI_lower, crit_det_95CI_upper)

  dat$date <- as.Date(dat$date)
  dat$triggerDate <- as.Date(dat$startdate) + dat$time_of_trigger
  dat$exp_name <- exp_name
  datList[[length(datList)+1]] <- dat
  rm(dat, samplesDat)
}
dat <- datList %>% bind_rows()
summary(dat$date)

if (!dir.exists(file.path(sim_dir, "ICU_timeline_plots"))) dir.create(file.path(sim_dir, "ICU_timeline_plots"))
if (!dir.exists(file.path(sim_dir, "ICU_timeline_plots", "pdf"))) dir.create(file.path(sim_dir, "ICU_timeline_plots", "pdf"))

subregions <- c("covidregion_1", "covidregion_4", "covidregion_11")
regions <- paste0("covidregion_", c(1, 4, 11))

### Data processing before plotting
dat$date <- as.Date(dat$date)
tapply(dat$date, dat$exp_name, summary)

dat <- dat %>%
  filter(date >= as.Date("2020-07-01") & date <= as.Date("2021-02-01")) %>%
  mutate(geography_modeled = gsub("EMS-", "covidregion_", ems)) %>%
  filter(ems != "All") %>%
  dplyr::select(-ems)%>%
  filter(geography_modeled %in% regions)  %>%
  f_get_scenVars()

dat$region <- factor(dat$geography_modeled, levels = regions, labels = gsub("covidregion_", "Region ", regions))

rollback_values <- unique(dat$rollback)
rollback_val <- rollback_values[2]

### check
str(dat)
table(dat$region, dat$exp_name)
table(dat$time_of_trigger)
table(dat$region, dat$reopen)
table(dat$region, dat$time_of_trigger)
table(dat$region, dat$triggerDate)
table(dat$region, dat$rollback)

length(unique(dat$date)) * length(unique(dat$triggerDate))  * length(unique(dat$region))  * length(unique(dat$exp_name))
dat %>% filter(time_of_trigger==274 & exp_name =="20210107_IL_regreopen50perc_Dates_pr8") %>% View()

### load capacity
capacityDat <- load_new_capacity(filedate = "20200915") %>%
  mutate(geography_modeled = paste0("covidregion_", geography_name))

dat <- f_addVar(dat, capacityDat) %>% filter(date >= as.Date("2020-09-01"))


### prepare
dat_counterfactual <- dat %>%
  filter(rollback == "counterfactual") %>%
  dplyr::select(-rollback, -delay, -capacity_multiplier_fct)
dat_counterfactual$reopen_fct <- gsub(
  "100perc", "High\ntransmission increase",
  gsub(
    "50perc", "Low\ntransmission increase",
    dat_counterfactual$reopen
  )
)


dat_scen <- dat %>% filter(rollback != "counterfactual" & delay == unique(dat$delay)[1])


#### ====================================
## Recommended versus selected
#### ====================================
unique(dat_scen$capacity_multiplier_fct)
dat_scen <- dat_scen %>%
  mutate(
    trigger_recommended = ifelse(geography_modeled == "covidregion_1", "60", "80"),
    trigger_recommended = ifelse(geography_modeled == "covidregion_4", "50", trigger_recommended),
    trigger_recommended = ifelse(geography_modeled == "covidregion_11", "70", trigger_recommended)
  )

dat_counterfactual_sub <- dat_counterfactual %>%
  filter( date >= as.Date("2020-09-01") &
           date <= as.Date("2020-12-31")) %>%
  mutate(grp = "counterfactual\n(no mitigation)")

dat_scen_sub1 <- dat_scen %>%
  filter( capacity_multiplier_fct == trigger_recommended) %>%
  mutate(grp = "recommended\n(mitigaton triggered at\n 60% (based on high transmission incr.)))")

dat_scen_sub2 <- dat_scen %>%
  filter(capacity_multiplier_fct == "80") %>%
  mutate(grp = "default\n(mitigaton triggered at 80%)")


plotdat <- rbind(dat_scen_sub1, dat_scen_sub2) %>%
  filter(reopen=="50perc" & rollback == rollback_values[3] & delay == delay_val &
           date >= as.Date("2020-09-01") &
           date <= as.Date("2020-12-31"))

colnames(plotdat) %in% colnames(dat_counterfactual_sub)
plotdat2 <- plotdat %>% dplyr::select(colnames(dat_counterfactual_sub)) %>%
            rbind(dat_counterfactual_sub)


#pplot_temp <- ggplot() +
  
####----------------
## DEATHS
###------------------
pplot <-
  ggplot(data = subset(plotdat2)) +
  geom_ribbon(aes( x = date, ymin = new_deaths_50CI_lower, ymax = new_deaths_50CI_upper,
                   fill = grp, group = grp ), alpha = 0.4) +
  geom_ribbon(  aes( x = date, ymin = new_deaths_95CI_lower, ymax = new_deaths_95CI_upper,
                     fill = grp, group = grp ), alpha = 0.2) +
  geom_line(
    aes( x = date, y = new_deaths_median,
         col = grp, group = grp), size = 1.3 ) +
  facet_wrap(reopen ~ region, scales = "free") +
  scale_x_date(breaks =custom_date_breaks, date_labels = "%b") +
  labs( x = "", y = "Predicted deaths",
        col = "ICU trigger threshold",  fill = "ICU trigger threshold",
        caption = "median and 50% IQR"
  ) +
  geom_vline(xintercept = as.Date("2020-11-20"))+
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  customTheme+
  theme(legend.position = "bottom")

ref_dat <- f_load_ref_data(subregions = c(1, 4, 11), 
                           startdate = as.Date("2020-09-01"), 
                           stopdate = as.Date("2020-12-12")) %>%
  filter(name == "deaths" & source != "EMResource")

pplot_wdata <- pplot +
  geom_point( data = ref_dat, aes(x = Date, y = value),
              size = 1 ) +
  geom_line(
    data = ref_dat, aes(x = Date, y = value7),
    size = 1.2
  )

pplot_wdata

###-----------------------
#### NON ICU
###-----------------------


pplot <-
  ggplot(data = subset(plotdat2)) +
  geom_ribbon(aes( x = date, ymin = hospitalized_50CI_lower, ymax = hospitalized_50CI_upper,
                   fill = grp, group = grp ), alpha = 0.4) +
  geom_ribbon(  aes( x = date, ymin = hospitalized_95CI_lower, ymax = hospitalized_95CI_upper,
                     fill = grp, group = grp ), alpha = 0.2) +
  geom_line(
    aes( x = date, y = hospitalized_median,
         col = grp, group = grp), size = 1.3 ) +
  facet_wrap(reopen ~ region, scales = "free") +
  scale_x_date(breaks =custom_date_breaks, date_labels = "%b") +
  labs( x = "", y = "Predicted non-ICU",
        col = "ICU trigger threshold",  fill = "ICU trigger threshold",
        caption = "median and 50% IQR"
  ) +
  geom_vline(xintercept = as.Date("2020-11-20"))+
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  customTheme+
  theme(legend.position = "bottom")

ref_dat <- f_load_ref_data(subregions = c(1, 4, 11), 
                           startdate = as.Date("2020-09-01"), 
                           stopdate = as.Date("2020-12-12")) %>%
  filter(name == "covid_non_icu" & source == "EMResource")

pplot_wdata <- pplot +
  geom_point( data = ref_dat, aes(x = Date, y = value),
              size = 1 ) +
  geom_line(
    data = ref_dat, aes(x = Date, y = value7),
    size = 1.2
  )

pplot_wdata


###-----------------------
#### ICU
###-----------------------


pplot <-
  ggplot(data = subset(plotdat2)) +
  geom_ribbon(aes( x = date, ymin = crit_det_50CI_lower, ymax = crit_det_50CI_upper,
      fill = grp, group = grp ), alpha = 0.4) +
  geom_ribbon(  aes( x = date, ymin = crit_det_95CI_lower, ymax = crit_det_95CI_upper,
      fill = grp, group = grp ), alpha = 0.2) +
  geom_line(
    aes( x = date, y = crit_det_median,col = grp, group = grp), size = 1.3 ) +
  geom_hline(aes(yintercept = icu_available),linetype = "dashed", col =capacity_col, size=1.3) +
  geom_hline(aes(yintercept = icu_available*0.8),linetype = "longdash", col =capacity_col) +
  geom_hline(aes(yintercept = icu_available*0.6),linetype = "dotted", col =capacity_col) +
  facet_wrap(reopen ~ region, scales = "free") +
  scale_x_date(breaks =custom_date_breaks, date_labels = "%b") +
  scale_y_continuous(expand=c(0,0))+
  labs( x = "", y = "Predicted ICU census",
    col = "ICU trigger threshold",  
    fill = "ICU trigger threshold",
    caption = "median and 50% IQR") +
  geom_vline(xintercept = as.Date("2020-11-20")) +
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  customTheme+
  theme(legend.position = "bottom")

pplot


#### Add data points
ref_dat <- f_load_ref_data(subregions = c(1, 4, 11), 
                           startdate = as.Date("2020-09-01"), 
                           stopdate = as.Date("2020-12-12")) %>%
  filter(name == "confirmed_covid_icu" & source == "EMResource")

pplot_wdata <- pplot +
  geom_point( data = ref_dat, aes(x = Date, y = value),
    size = 1 ) +
  geom_line(
    data = ref_dat, aes(x = Date, y = value7),
    size = 1.2
  )

pplot_wdata

#### Rel reduction barplot 
plotdat2$grp_fct <-paste0("grp_", as.numeric( as.factor(plotdat2$grp)))
table(plotdat2$grp_fct ,plotdat2$grp)

### TODO use new critical and new hospitalized 
cumulDat <- plotdat2 %>% 
              group_by(region, grp_fct) %>%
              summarize(crit_det_50CI_lower = sum(crit_det_50CI_lower),
                        crit_det_50CI_upper = sum(crit_det_50CI_upper),
                        crit_det_95CI_lower = sum(crit_det_95CI_lower),
                        crit_det_95CI_upper = sum(crit_det_95CI_upper),
                        crit_det_median = sum(crit_det_median),
                        new_deaths_50CI_lower = sum(new_deaths_50CI_lower),
                        new_deaths_50CI_upper = sum(new_deaths_50CI_upper),
                        new_deaths_95CI_lower = sum(new_deaths_95CI_lower),
                        new_deaths_95CI_upper = sum(new_deaths_95CI_upper),
                        new_deaths_median = sum(new_deaths_median),
                        hosp_det_50CI_lower = sum(hosp_det_50CI_lower),
                        hosp_det_50CI_upper = sum(hosp_det_50CI_upper),
                        hosp_det_95CI_lower = sum(hosp_det_95CI_lower),
                        hosp_det_95CI_upper = sum(hosp_det_95CI_upper),
                        hosp_det_median = sum(hospitalized_median)) %>%
            pivot_longer(cols=-c(region, grp_fct)) %>%
            pivot_wider(names_from="grp_fct", values_from="value") %>%
            mutate(grp1_3 = grp_1 - grp_2,
                   grp1_2 = grp_1 - grp_3,
                   grp2_3 = grp_2 - grp_3) %>%
            pivot_longer(cols=-c(region, name), names_to="grp") %>%
            mutate(name=gsub("crit_det_","crit-det_",name),
                   name=gsub("hosp_det_","hosp-det_",name),
                   name=gsub("new_deaths_","new-deaths_",name),
                   name=gsub("CI_","CI-",name))%>%
            separate(name, into=c("name","stat"), sep="_") %>%
            mutate(stat=paste0("q",gsub("CI-","CI_",stat))) %>%
            pivot_wider(names_from="stat", values_from="value")

cumulDat$name_fct <- factor(cumulDat$name, 
                            levels=c("hosp-det","crit-det","new-deaths"), 
                            labels=c("med/surg\ncensus","ICU\ncensus","daily\ndeaths"))

plot_2 <- ggplot(data=subset(cumulDat, grp %in% c("grp1_2","grp2_3"))) + 
  geom_bar(aes(x=grp, y=qmedian, fill=name_fct),stat="identity", position = position_dodge(width=0.9)) +
  #geom_errorbar(aes(x=grp, y=qmedian,ymin=q95CI_lower,ymax=q95CI_upper , group=name_fct),
  #              stat="identity", position = position_dodge(width=0.9),width=0, linetype="dashed") +
  geom_errorbar(aes(x=grp, y=qmedian,ymin=q50CI_lower,ymax=q50CI_upper , group=name_fct),
                stat="identity", position = position_dodge(width=0.9),width=0) +
  facet_wrap(~region, nrow=1)+
  labs( x = "", y = "Predicted number averted",
        col = "",  fill = "",
        caption = ""
  ) +
  geom_text(aes(x=grp, y=qmedian, label=round(qmedian,0)),stat="identity",  position = position_dodge(width=0.9))+
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(expand=c(0,0), lim=c(0,13500),labels = comma)+
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  customTheme+
  theme(legend.position = "bottom")
plot_2
  

plot_grid(pplot_wdata, plot_2, ncol=1, rel_heights = c(1, 0.6))

f_save_plot(plot_name=paste0("comparison_predicted_to_actual_trend_v2"), pplot = pplot_wdata, 
            plot_dir = file.path(sim_dir, "ICU_timeline_plots"), width = 16, height = 6)

f_save_plot(plot_name=paste0("comparison_predicted_to_actual_trend_barplot"), pplot = plot_2, 
            plot_dir = file.path(sim_dir, "ICU_timeline_plots"), width = 12, height = 4)

#####For text

cumulDat %>%
  filter( grp %in% c("grp1_2","grp2_3")) %>%
  group_by( name, grp) %>% 
  summarize(qmedian=sum(qmedian))


cumulDat %>%
  filter( grp %in% c("grp1_2","grp2_3")) %>%
  group_by(region, name, grp) %>% 
  summarize(qmedian=sum(qmedian)) %>%
 pivot_wider(names_from="name", values_from="qmedian")

###### Difference in time, trigger and peak



