
library(readr)
library(tidyverse)
source("load_paths.R")

list.files(file.path(data_path, "covid_IDPH/Corona virus reports/hospital_capacity_thresholds_template"))
fname="capacity_weekday_average_20200908.csv"
dat <- read_csv(file.path(data_path, "covid_IDPH/Corona virus reports/hospital_capacity_thresholds_template", fname))

dat <- dat %>% 
        filter(overflow_threshold_percent==1) %>% 
        select(geography_modeled, resource_type, avg_resource_available_prev2weeks) %>% 
        unique() %>%
  pivot_wider(names_from="resource_type", values_from="avg_resource_available_prev2weeks")


#### Write yaml snippet 

cat("- {'low':" ,dat$hb_availforcovid ,", 'high':  ",dat$hb_availforcovid , "}")








load_new_capacity <- function(selected_ems = NULL, simdate = "20200825") {
  
  
  fname <- paste0("capacity_weekday_average_",simdate,".csv")
  df <- read.csv(file.path(data_path, "covid_IDPH/Corona virus reports/hospital_capacity_thresholds_template", fname))
  
  
  df <- df %>%
    filter(overflow_threshold_percent == 1) %>%
    select(geography_modeled, resource_type, avg_resource_available_prev2weeks) %>%
    unique() %>%
    pivot_wider(names_from = "resource_type", values_from = "avg_resource_available_prev2weeks") %>%
    mutate(geography_name = gsub("covidregion_", "", geography_modeled)) %>%
    select(geography_name, icu_availforcovid, hb_availforcovid)
  
  dfRR <- df %>%
    rename(region = geography_name) %>%
    f_addRestoreRegion() %>%
    group_by(restore_region) %>%
    summarize(
      icu_availforcovid = sum(icu_availforcovid),
      hb_availforcovid = sum(hb_availforcovid)
    ) %>%
    mutate(geography_name = tolower(restore_region)) %>%
    select(geography_name, icu_availforcovid, hb_availforcovid)
  
  dfIL <- df %>%
    summarize(
      icu_availforcovid = sum(icu_availforcovid),
      hb_availforcovid = sum(hb_availforcovid)
    ) %>%
    mutate(geography_name = "illinois") %>%
    select(geography_name, icu_availforcovid, hb_availforcovid)
  
  
  df <- rbind(df, dfRR, dfIL) %>%
    as.data.frame() %>%
    rename(
      icu_available = icu_availforcovid,
      medsurg_available = hb_availforcovid
    )
  
  if (!(is.null(selected_ems))) df <- df %>% filter(geography_name %in% selected_ems)
  
  return(df)
}

