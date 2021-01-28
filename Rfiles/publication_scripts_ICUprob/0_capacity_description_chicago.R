

library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")

theme_set(theme_minimal())
outdir <- file.path(project_path, "project_notes", "publications/covid_model_IL_overflow/")

capacityDat_1208 <- fread(file.path(data_path, "covid_IDPH", "Corona virus reports", "hospital_capacity_thresholds/capacity_weekday_average_20201208.csv"))
capacityDat_1208 <- capacityDat_1208 %>%
  filter(geography_modeled %in% c("covidregion_1", "covidregion_4", "covidregion_11") &
           resource_type == "icu_availforcovid" &
           overflow_threshold_percent == 1) %>%
  select(-date_window_upper_bound) %>%
  unique() %>%
  mutate(geography_name = gsub("covidregion_", "", geography_modeled))


capacityDat_0915 <- fread(file.path(data_path, "covid_IDPH", "Corona virus reports", "hospital_capacity_thresholds/capacity_weekday_average_20200915.csv"))
capacityDat_0915 <- capacityDat_0915 %>%
  filter(geography_modeled %in% c("covidregion_1", "covidregion_4", "covidregion_11") &
           resource_type == "icu_availforcovid" &
           overflow_threshold_percent == 1) %>%
  select(-date_window_upper_bound) %>%
  unique() %>%
  mutate(geography_name = gsub("covidregion_", "", geography_modeled))

capacityDat <- fread(file.path(data_path, "covid_IDPH", "Corona virus reports", "capacity_by_covid_region.csv"))
capacityDat <- capacityDat %>%
  filter(geography_level == "covid region" & geography_name %in% c(1, 4, 11)) %>%
  mutate(date = as.Date(date))

capacityDat$geography_name <- factor(capacityDat$geography_name, levels = c(1:11), labels = c(1:11))
capacityDat_1208$geography_name <- factor(capacityDat_1208$geography_name, levels = c(1:11), labels = c(1:11))
capacityDat_0915$geography_name <- factor(capacityDat_0915$geography_name, levels = c(1:11), labels = c(1:11))
capacityDat$date <- as.Date(capacityDat$date)

pplot <- ggplot(data = subset(capacityDat,geography_name==11 & date <= as.Date("2020-12-31") )) +
  geom_line(aes(x = date, y = icu_total , group = 1), col = "dodgerblue",line=1.3, alpha = 0.3) +
  geom_area(aes(x = date, y = icu_total, group = 1), fill = "dodgerblue", alpha = 0.3) +
  geom_area(aes(x = date, y = icu_covid+icu_noncovid , group = 1), fill = "dodgerblue", alpha = 0.5) +
  geom_line(aes(x = date, y = icu_covid+icu_noncovid , group = 1), col = "dodgerblue2",line=1.3) +
  geom_area(aes(x = date, y = icu_covid , group = 1), fill = "dodgerblue3", alpha = 0.5) +
  geom_line(aes(x = date, y = icu_covid, group = 1), col = "dodgerblue3",line=1.3) +
  geom_line(aes(x=date, y= icu_availforcovid-icu_covid, group=1),col="red", linetype='solid',line=1.5)+
  geom_line(aes(x=date, y= icu_availforcovid, group=1),col="orange", linetype='solid',line=1.5)+
  geom_line(aes(x=date, y= icu_total-icu_noncovid, group=1),col="black", linetype='solid',line=1.5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0) ,labels=comma) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  customTheme +
  theme_minimal()+
  theme_cowplot()+
  labs(
    x = "", y = "Number of ICU beds",
    caption = "\ndata source: IDPH, Corona virus reports, capacity_by_covid_region.csv\nand capacity_weekday_average_20200915.csv, capacity_weekday_average_20201208.csv"
  )
pplot


ggsave(paste0("capacity_timeline_chicago.pdf"),
       plot = pplot,
       path = file.path(outdir), width = 12, height = 8, device = "pdf"
)


capacityDat %>% filter(geography_name==11 & date <= as.Date("2020-12-31")) %>%
  select(icu_total, icu_noncovid, icu_covid, icu_availforcovid)


capacityDat %>%
  filter(geography_name==11 &  date >= as.Date("2020-05-01") & date <= as.Date("2020-05-30")) %>%
  dplyr::select(icu_total, icu_noncovid, icu_covid, icu_availforcovid)%>%
  summarize(icu_total=mean(icu_total,na.rm=TRUE),
            icu_noncovid=mean(icu_noncovid,na.rm=TRUE),
            icu_covid=mean(icu_covid,na.rm=TRUE),
            icu_availforcovid=mean(icu_availforcovid,na.rm=TRUE))%>%
  mutate(perc_available1 =(icu_availforcovid - icu_covid)/icu_availforcovid  ,
         perc_available2 =(icu_total  - (icu_covid+icu_noncovid) )/icu_total   )

capacityDat %>%
  filter(geography_name==11 &  date >= as.Date("2020-08-01") & date <= as.Date("2020-08-30")) %>%
  dplyr::select(icu_total, icu_noncovid, icu_covid, icu_availforcovid)%>%
  summarize(icu_total=mean(icu_total,na.rm=TRUE),
            icu_noncovid=mean(icu_noncovid,na.rm=TRUE),
            icu_covid=mean(icu_covid,na.rm=TRUE),
            icu_availforcovid=mean(icu_availforcovid,na.rm=TRUE))%>%
  mutate(perc_available1 =(icu_availforcovid - icu_covid)/icu_availforcovid  ,
         perc_available2 =(icu_total  - (icu_covid+icu_noncovid) )/icu_total   )


capacityDat %>%
  filter(geography_name==11 &  date >= as.Date("2020-11-01") & date <= as.Date("2020-11-30")) %>%
  dplyr::select(icu_total, icu_noncovid, icu_covid, icu_availforcovid)%>%
  summarize(icu_total=mean(icu_total,na.rm=TRUE),
            icu_noncovid=mean(icu_noncovid,na.rm=TRUE),
            icu_covid=mean(icu_covid,na.rm=TRUE),
            icu_availforcovid=mean(icu_availforcovid,na.rm=TRUE))%>%
  mutate(perc_available1 =(icu_availforcovid - icu_covid)/icu_availforcovid  ,
         perc_available2 =(icu_total  - (icu_covid+icu_noncovid) )/icu_total   )

###############################



pplot <- ggplot(data = subset(capacityDat,geography_name==11 & date <= as.Date("2020-12-31") )) +
  geom_line(aes(x = date, y = icu_total , group = 1), col = "dodgerblue",line=1.3, alpha = 0.3) +
  geom_area(aes(x = date, y = icu_total, group = 1), fill = "dodgerblue", alpha = 0.3) +
  geom_area(aes(x = date, y = icu_noncovid , group = 1), fill = "dodgerblue3", alpha = 0.5) +
  geom_line(aes(x = date, y = icu_noncovid, group = 1), col = "dodgerblue3",line=1.3) +
  geom_area(aes(x = date, y = icu_covid+icu_noncovid , group = 1), fill = "dodgerblue", alpha = 0.5) +
  geom_line(aes(x = date, y = icu_covid+icu_noncovid , group = 1), col = "dodgerblue2",line=1.3) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0) ,labels=comma) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  customTheme +
  theme_minimal()+
  theme_cowplot()+
  labs(
    x = "", y = "Number of ICU beds",
    caption = "\ndata source: IDPH, Corona virus reports, capacity_by_covid_region.csv\nand capacity_weekday_average_20200915.csv, capacity_weekday_average_20201208.csv"
  )
pplot


ggsave(paste0("capacity_timeline_chicago.pdf"),
       plot = pplot,
       path = file.path(outdir), width = 12, height = 8, device = "pdf"
)



ggsave(paste0("capacity_timeline_chicago.pdf"),
       plot = pplot,
       path = file.path(outdir), width = 12, height = 8, device = "pdf"
)

