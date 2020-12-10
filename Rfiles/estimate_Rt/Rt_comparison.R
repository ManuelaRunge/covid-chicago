##==========================================
### Compare Rt estimates
##==========================================


library(readr)
library(tidyverse)
library(lubridate)
library(zoo)

source("load_paths.R")
source("setup.R")

outdir <- file.path(project_path, "Plots + Graphs/Rt_plots")

today <- gsub("-", "", Sys.Date())

nu_0 <- read_csv(file.path(project_path, "NU_civis_outputs/20200902/csv/nu_20200902.csv"))
nu_1 <- read_csv(file.path(project_path, "NU_civis_outputs/20200910/csv/nu_20200910.csv"))
nu_2 <- read_csv(file.path(project_path, "NU_civis_outputs/20200916/csv/nu_20200916.csv"))

nu_0$simdate <- "20200902"
nu_1$simdate <- "20200910"
nu_2$simdate <- "20200916"

nu_0_today <- nu_0 %>%
  filter(date == as.Date("2020-09-16")) %>%
  select(date, geography_modeled, rt_median) %>%
  rename(rt_median_0 = rt_median)
nu_1_today <- nu_1 %>%
  filter(date == as.Date("2020-09-16")) %>%
  select(date, geography_modeled, rt_median) %>%
  rename(rt_median_1 = rt_median)
nu_2_today <- nu_2 %>%
  filter(date == as.Date("2020-09-16")) %>%
  select(date, geography_modeled, rt_median) %>%
  rename(rt_median_2 = rt_median)

nu_rt_today <- nu_0_today %>%
  left_join(nu_1_today, by = c("date", "geography_modeled")) %>%
  left_join(nu_2_today, by = c("date", "geography_modeled")) %>%
  mutate(Rtdiff_12 = rt_median_2 - rt_median_1) %>%
  arrange(Rtdiff_12)


Rt_dat <- read_csv("nu_il_fromdata_estimated_Rt.csv") %>% mutate(rtfrom = "data")
Rt_sim <- nu_2 %>%
  mutate(covid_region = gsub("covidregion_", "", geography_modeled), rtfrom = "sim") %>%
  dplyr::select(colnames(Rt_dat))

Rt_compare <- rbind(Rt_dat, Rt_sim) %>% filter(date <= max(Rt_dat$date) + 14 & covid_region != "illinois")
Rt_compare$covid_region <- factor(Rt_compare$covid_region, levels = c(1:11), labels = c(1:11))


p_Rt_1 <- ggplot(data = Rt_compare) +
  theme_bw() +
  geom_line(aes(x = date, y = rt_median, col = rtfrom), size = 1.3) +
  geom_ribbon(aes(x = date, ymin = rt_lower, ymax = rt_upper, fill = rtfrom), alpha = 0.5) +
  facet_wrap(~covid_region, scales = "free_y") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(fill = "", color = "", x = "", y = expression(italic(R[t]))) +
  scale_color_manual(values = c("deepskyblue3", "orange")) +
  scale_fill_manual(values = c("deepskyblue3", "orange")) +
  customThemeNoFacet

p_Rt_2 <- ggplot(data = subset(Rt_compare, date >= as.Date("2020-07-01"))) +
  theme_bw() +
  geom_line(aes(x = date, y = rt_median, col = rtfrom), size = 1.3) +
  geom_ribbon(aes(x = date, ymin = rt_lower, ymax = rt_upper, fill = rtfrom), alpha = 0.5) +
  facet_wrap(~covid_region, scales = "free_y") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(fill = "", color = "", x = "", y = expression(italic(R[t]))) +
  scale_color_manual(values = c("deepskyblue3", "orange")) +
  scale_fill_manual(values = c("deepskyblue3", "orange")) +
  customThemeNoFacet


ggsave(paste0(today, "Rt_data_vs_sim.png"),
  plot = p_Rt_1, path = file.path(outdir), width = 14, height = 8, dpi = 300, device = "png"
)
ggsave(paste0(today, "Rt_data_vs_sim_zoom.png"),
  plot = p_Rt_2, path = file.path(outdir), width = 14, height = 8, dpi = 300, device = "png"
)
