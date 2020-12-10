
## set working directory to R.project location
library(ggplot2)
library(data.table)
library(tidyverse)
library(zoo) ## for 7 day rolling average

library(cowplot)
theme_set(theme_cowplot())


runInBatchMode <- F

if (runInBatchMode) {
  cmd_agrs <- commandArgs()
  length(cmd_agrs)
  exp_name <- cmd_agrs[length(cmd_agrs) - 3]
  useSmoothedData <- cmd_agrs[length(cmd_agrs) - 2]
  Location <- cmd_agrs[length(cmd_agrs) - 1]
  workingDir <- cmd_agrs[length(cmd_agrs)]

} else {
  exp_name <- "20201027_IL_mr_baseline"
  
  Location <- "Local"
  workingDir <- getwd()
}

## Print out for log
print(exp_name)
print(Location)

## --------------------------------
### Set working directory to the GitHub repository R files
## --------------------------------
source("load_paths.R")
source("processing_helpers.R")
setwd(workingDir)


NUdir <- file.path(project_path, "NU_civis_outputs")
simdate <- "20201027"
selectedRegions <- c("covidregion_1","covidregion_3","covidregion_6","covidregion_7")


sample_kI <- fread(file.path(simulation_output, exp_name,  "sampled_parameters.csv"))  %>% as.data.frame()
keepCols <- c("scen_num",colnames(sample_kI)[grep("ki_multiplier",colnames(sample_kI))])
keepCols <- keepCols[!grepl("time",keepCols)]
keepCols <- keepCols[!grepl("ki_multiplier_3",keepCols)]
keepCols <- keepCols[!grepl("ki_multiplier_4",keepCols)]
keepCols <- keepCols[!grepl("ki_multiplier_5",keepCols)]
keepCols <- keepCols[!grepl("ki_multiplier_6",keepCols)]

sample_kI <- sample_kI[, keepCols]

sample_kI <- sample_kI %>% 
                pivot_longer(cols=-c("scen_num")) %>%
                separate(name, into=c("ki_multiplier","region"), sep="_EMS_") %>%
                dplyr::select(-scen_num ) %>% unique() %>%
                pivot_wider(names_from="ki_multiplier", values_from="value")
  
table(sample_kI$ki_multiplier)
tapply(sample_kI$value, sample_kI$ki_multiplier, summary )



dat <- fread(file.path(NUdir, simdate, "csv", paste0("nu_",simdate,".csv"))) %>% mutate(date= as.Date(date))
table(dat$scenario_name)

popdat <- load_population() %>% rename(region=geography_name)
dat <- dat %>% mutate(region = gsub("covidregion_","",geography_modeled)) %>% left_join(popdat, by="region")

dat <- dat %>% group_by(date, geography_modeled, scenario_name) %>%
  mutate(pop=as.numeric(pop),
         prev_median= (cases_median/pop)*100,
         prev_lower= (cases_lower/pop)*100,
         prev_upper= (cases_upper/pop)*100)

dat$date <- as.Date(dat$date)


ggplot(data=dat) +
  geom_ribbon(aes(x=date, ymin=icu_lower, ymax=icu_upper, fill=scenario_name), alpha=0.3) +
  geom_line(aes(x=date, y=icu_median, col=scenario_name)) +
  facet_wrap(~geography_modeled, scales="free")+
  customTheme


ggplot(data=subset(dat, date >= as.Date("2020-07-01"))) +
  geom_ribbon(aes(x=date, ymin=rt_lower, ymax=rt_upper, fill=scenario_name), alpha=0.3) +
  geom_line(aes(x=date, y=rt_median, col=scenario_name)) +
  facet_wrap(~geography_modeled, scales="free")+
  customTheme+
  geom_hline(yintercept = 1)

dat$date <- as.Date(dat$date)
pplot <- ggplot(data=subset(dat, date >= as.Date("2020-07-01") & date<= as.Date("2020-11-30") & geography_modeled %in% selectedRegions)) +
  geom_ribbon(aes(x=date, ymin=rt_lower, ymax=rt_upper, fill=scenario_name), alpha=0.3) +
  geom_line(aes(x=date, y=rt_median, col=scenario_name)) +
  facet_wrap(~geography_modeled)+
  customTheme+
  background_grid()+
  geom_hline(yintercept = 1)+
  scale_x_date(date_breaks = "14 days", date_labels = "%d\n%b")+
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")

ggsave(paste0("Rt_scenario_plot.pdf"),
       plot = pplot,
       path = file.path(simulation_output, exp_name,"_plots"), width = 13, height = 10, device = "pdf"
)


dat$date <- as.Date(dat$date)
pplot <- ggplot(data=subset(dat, date >= as.Date("2020-06-29") & date<= as.Date("2020-12-15") & geography_modeled %in% selectedRegions)) +
  geom_ribbon(aes(x=date, ymin=rt_lower, ymax=rt_upper, fill=scenario_name), alpha=0.3) +
  geom_line(aes(x=date, y=rt_median, col=scenario_name)) +
  facet_wrap(~geography_modeled, ncol=1, scales="free_x")+
  customTheme+
  background_grid()+
  geom_hline(yintercept = 1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%d\n%b")+
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")

ggsave(paste0("Rt_scenario_plot2.pdf"),
       plot = pplot,
       path = file.path(simulation_output, exp_name,"_plots"), width = 14, height =16, device = "pdf"
)




dat$date <- as.Date(dat$date)
pplot <- ggplot(data=subset(dat, date >= as.Date("2020-06-01") & date<= as.Date("2020-12-15") )) +
  geom_ribbon(aes(x=date, ymin=rt_lower, ymax=rt_upper, fill=scenario_name), alpha=0.3) +
  geom_line(aes(x=date, y=rt_median, col=scenario_name)) +
  facet_wrap(~geography_modeled)+
  customTheme+
  background_grid()+
  geom_hline(yintercept = 1)+
  scale_x_date(date_breaks = "14 days", date_labels = "%d\n%b")+
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")

ggsave(paste0("Rt_scenario_plot_Northern.pdf"),
       plot = pplot,
       path = file.path(simulation_output, exp_name,"_plots"), width = 13, height = 10, device = "pdf"
)





ggplot(data=subset(dat, date >= as.Date("2020-07-01") & date<= as.Date("2020-10-30") & geography_modeled %in% selectedRegions)) +
 # geom_ribbon(aes(x=date, ymin=prev_lower, ymax=prev_upper, fill=scenario_name), alpha=0.3) +
  geom_line(aes(x=date, y=prev_median, col=scenario_name)) +
  facet_wrap(~geography_modeled, nrow=1)+
  customTheme+
  background_grid()+
  scale_x_date(date_breaks = "14 days", date_labels = "%d\n%b")





ggplot(data=subset(dat,geography_modeled=="illinois" )) +
  geom_ribbon(aes(x=date, ymin=icu_lower, ymax=icu_upper, fill=scenario_name), alpha=0.3) +
  geom_line(aes(x=date, y=icu_median, col=scenario_name)) +
  facet_wrap(~geography_modeled, scales="free")+
  customTheme


ggplot(data=subset(dat,geography_modeled %in% selectedRegions )) +
  geom_ribbon(aes(x=date, ymin=icu_lower, ymax=icu_upper, fill=scenario_name), alpha=0.3) +
  geom_line(aes(x=date, y=icu_median, col=scenario_name)) +
  facet_wrap(~geography_modeled, scales="free") +
  customTheme






