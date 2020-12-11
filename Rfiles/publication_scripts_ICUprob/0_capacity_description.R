


library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")

theme_set(theme_minimal())
outdir <- file.path(project_path, "project_notes", "publications/covid_model_IL_overflow/data_plots")

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

pplot <- ggplot(data = capacityDat) +
  geom_area(aes(x = date, y = icu_total, group = 1), fill = "darkgrey", alpha = 0.4) +
  geom_area(aes(x = date, y = icu_availforcovid, group = 1), fill = "dodgerblue", alpha = 0.8) +
  geom_line(aes(x = date, y = icu_availforcovid, group = 1), col = "dodgerblue2") +
  # geom_line(aes(x=date, y= icu_availforcovid-icu_covid, group=1),col="red", linetype='solid')+
  geom_line(aes(x = date, y = icu_covid, group = 1), col = "black") +
  # geom_line(aes(x=date, y= icu_noncovid, group=1),col="blue")+
  geom_hline(
    data = capacityDat_0915,
    aes(yintercept = avg_resource_available_prev2weeks), col = "red", linetype = "dashed"
  ) +
  geom_hline(
    data = capacityDat_1208,
    aes(yintercept = avg_resource_available), col = "darkred", linetype = "dashed"
  ) +
  geom_text(
    data = capacityDat_0915,
    aes(x = as.Date("2020-05-12"), y = avg_resource_available_prev2weeks + 10), vjust = 0.5, col = "red", label = "capacity Sep 15"
  ) +
  geom_text(
    data = capacityDat_1208,
    aes(x = as.Date("2020-05-12"), y = avg_resource_available - 10), vjust = 0.5, col = "darkred", label = "capacity Dec 08"
  ) +
  geom_text(data = subset(capacityDat, date == as.Date("2020-08-12")), aes(
    x = as.Date("2020-08-12"),
    y = icu_total * 0.8
  ), vjust = 0.5, col = "gray33", label = "Total ICU beds") +
  geom_text(data = subset(capacityDat, date == as.Date("2020-08-12")), aes(
    x = as.Date("2020-08-12"),
    y = icu_availforcovid * 0.8
  ), vjust = 0.5, col = "dodgerblue4", label = "ICU beds available\nfor covid") +
  geom_text(data = subset(capacityDat, date == as.Date("2020-12-01")), aes(
    x = as.Date("2020-11-25"),
    y = icu_covid * 1.5
  ), vjust = 0.5, col = "black", label = "ICU beds used\nfor covid") +
  # geom_vline(xintercept = as.Date("2020-09-15"), linetype="dashed")+
  scale_x_date(date_breaks = "30 days", date_labels = "%b", expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  customTheme +
  labs(
    x = "", y = "Number of ICU beds",
    caption = "\ndata source: IDPH, Corona virus reports, capacity_by_covid_region.csv\nand capacity_weekday_average_20200915.csv, capacity_weekday_average_20201208.csv"
  )


pplot_wide <- pplot +   facet_wrap(~geography_name, scales = "free") 

ggsave(paste0("capacity_timeline_reg1-4-11_wide.pdf"),
  plot = pplot_wide,
  path = file.path(outdir, "pdf"), width = 16, height = 6, device = "pdf"
)

ggsave(paste0("capacity_timeline_reg1-4-11_wide.png"),
  plot = pplot_wide,
  path = file.path(outdir), width = 16, height = 6, device = "png"
)

pplot_long <- pplot +   facet_wrap(~geography_name, scales = "free", ncol=1) 

ggsave(paste0("capacity_timeline_reg1-4-11_long.pdf"),
       plot = pplot_long,
       path = file.path(outdir, "pdf"), width =6, height =  12, device = "pdf"
)

ggsave(paste0("capacity_timeline_reg1-4-11_long.png"),
       plot = pplot_long,
       path = file.path(outdir), width = 6, height = 12, device = "png"
)
