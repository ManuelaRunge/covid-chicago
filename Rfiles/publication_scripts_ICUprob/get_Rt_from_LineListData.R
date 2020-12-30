## ============================================================
## R script to get R(t) from Line list admission data used for fitting
## ============================================================

# install.packages("devtools")
# library(devtools)
# install_github("annecori/EpiEstim", force = TRUE)

library(tidyverse)
library(EpiEstim)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("estimate_Rt/getRt_function.R")

outdir <- file.path(project_path, "cms_sim/simulation_output/_overflow_simulations/20201212/ICU_Rt_plots")
today <- gsub("-", "", Sys.Date())
data_date <- "201220"


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


### Load simulation outputs
dat <- read.csv(file.path(data_path, "covid_IDPH/Cleaned Data", paste0(data_date, "_jg_aggregated_covidregion.csv")))
summary(as.Date(dat$date))

datAll <- dat %>%
  group_by(date) %>%
  summarize(cases = sum(cases)) %>%
  mutate(covid_region = 0)

dat <- dat %>%
  select(colnames(datAll)) %>%
  bind_rows(datAll) %>%
  filter(covid_region %in% c(0, 1, 4, 11) & date <= as.Date("2020-10-01"))
table(dat$covid_region)

dat <- dat %>%
  arrange(covid_region, date) %>%
  group_by(covid_region) %>%
  mutate(Date = as.Date(date), time = c(1:n_distinct(date))) %>%
  filter(Date >= as.Date("2020-01-01"))

ggplot(data = dat) +
  geom_bar(aes(x = Date, y = cases), stat = "identity") +
  facet_wrap(~covid_region, scales = "free")

### Fill in missing dates ?

method <- "uncertain_si"
weekwindow <- 13
Rt_list <- list()
si_list <- list()

for (region in c(0, 1, 4, 11)) {
  # region = unique(dat$covid_region)[1]
  disease_incidence_data <- dat %>%
    filter(covid_region == region) %>%
    rename(I = cases)

  res <- getRt(disease_incidence_data, method = method, weekwindow = weekwindow)

  pplot <- plot(res)

  SAVE_plot <- T
  if (SAVE_plot) {
    ggsave(paste0(region, "_EpiEstim_default_", method, ".pdf"),
      plot = pplot, path = file.path(outdir, "pdf"), width = 6, height = 10, dpi = 300, device = "pdf"
    )
  }


  Rt_list[[length(Rt_list) + 1]] <- res$R %>% mutate(region = region)
}


Rt_dat <- Rt_list %>% bind_rows()

### Write csv file with Rt
Rt_dat <- Rt_dat %>%
  mutate(time = t_end) %>%
  rename(
    covid_region = region,
    rt_median = `Median(R)`,
    rt_lower = `Quantile.0.025(R)`,
    rt_upper = `Quantile.0.975(R)`
  ) %>%
  merge(unique(dat[, c("time", "date", "covid_region")]), by = c("time", "covid_region")) %>%
  select(date, covid_region, rt_median, rt_lower, rt_upper)

Rt_dat$date <- as.Date(Rt_dat$date)
write.csv(Rt_dat, file = file.path(outdir, "nu_il_fromdata_estimated_Rt.csv"), row.names = FALSE)
# Rt_dat <- fread(file.path(outdir,"nu_il_fromdata_estimated_Rt.csv"))
Rt_dat$date <- as.Date(Rt_dat$date)
Rt_dat$region <- factor(Rt_dat$covid_region, levels = c(0, 1, 4, 11), labels = c("Illinois", paste0("Region ", c(1, 4, 11))))
dat$region <- factor(dat$covid_region, levels = c(0, 1, 4, 11), labels = c("Illinois", paste0("Region ", c(1, 4, 11))))


### Generate plots

p_Rt <- ggplot(data = subset(Rt_dat, region != "Illinois")) +
  theme_bw() +
  geom_line(aes(x = date, y = rt_median), col = "deepskyblue4", size = 1.3) +
  geom_ribbon(aes(x = date, ymin = rt_lower, ymax = rt_upper), fill = "deepskyblue4", alpha = 0.5) +
  facet_wrap(~region, scales = "free") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(fill = "", color = "", x = "", y = expression(italic(R[t])), caption = "method = uncertain_si") +
  scale_x_date(
    breaks = custom_date_breaks_JanOct, date_labels = "%b", expand = c(0, 0),
    lim = c(as.Date("2020-02-01"), as.Date("2020-09-01"))
  ) +
  customThemeNoFacet


ggsave(paste0(today, "_Rt_estimated_from_data.pdf"),
  plot = p_Rt, path = file.path(outdir, "pdf"), width = 14, height = 4, device = "pdf"
)
ggsave(paste0(today, "_Rt_estimated_from_data.png"),
  plot = p_Rt, path = file.path(outdir), width = 14, height = 4, device = "png"
)


p_Rt <- ggplot(data = subset(Rt_dat, region != "Illinois" & date > as.Date("2020-05-01"))) +
  theme_bw() +
  geom_line(aes(x = date, y = rt_median), col = "deepskyblue4", size = 1.3) +
  geom_ribbon(aes(x = date, ymin = rt_lower, ymax = rt_upper), fill = "deepskyblue4", alpha = 0.5) +
  facet_wrap(~region, scales = "free") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(fill = "", color = "", x = "", y = expression(italic(R[t])), caption = "method = uncertain_si") +
  scale_x_date(
    breaks = custom_date_breaks_JanOct, date_labels = "%b", expand = c(0, 0),
    lim = c(as.Date("2020-02-01"), as.Date("2020-09-01"))
  ) +
  customThemeNoFacet


ggsave(paste0(today, "_Rt_estimated_from_data_zoom.pdf"),
  plot = p_Rt, path = file.path(outdir, "pdf"), width = 14, height = 4, device = "pdf"
)
ggsave(paste0(today, "_Rt_estimated_from_data_zoom.png"),
  plot = p_Rt, path = file.path(outdir), width = 14, height = 4, device = "png"
)



scl <- mean(dat$cases) / mean(Rt_dat$rt_median)
dat$date <- as.Date(dat$date)
Rt_dat$date <- as.Date(Rt_dat$date)

pplot <- ggplot(data = subset(Rt_dat, region != "Illinois" & date >= "2020-02-01")) +
  theme_bw() +
  geom_bar(
    data = subset(dat, covid_region != 0 & date >= "2020-02-01"),
    aes(x = date, y = cases / scl), fill = "grey", stat = "identity", alpha = 1
  ) +
  geom_line(aes(x = date, y = rt_median), col = "deepskyblue4", size = 1) +
  geom_ribbon(aes(x = date, ymin = rt_lower, ymax = rt_upper), fill = "deepskyblue4", alpha = 0.5) +
  facet_wrap(~region, scales = "free") +
  geom_hline(yintercept = 1, linetype = "dashed", col = "red") +
  scale_y_continuous(expression(italic(R[t])),
    sec.axis = sec_axis(~ . * scl, name = "New COVID-19 cases\n"), expand = c(0, 0)
  ) +
  labs(
    fill = "", color = "", x = "", y = expression(italic(R[t])),
    caption = "method = uncertain_si"
  ) +
  scale_x_date(
    breaks = custom_date_breaks_JanOct, date_labels = "%b", expand = c(0, 0),
    lim = c(as.Date("2020-02-01"), as.Date("2020-09-01"))
  ) +
  theme(panel.spacing = unit(1, "lines")) +
  customTheme
pplot


ggsave(paste0(today, "_Rt_and_cases_from_data.pdf"),
  plot = pplot, path = file.path(outdir, "pdf"), width = 14, height = 4, device = "pdf"
)
ggsave(paste0(today, "_Rt_and_cases_from_data.png"),
  plot = pplot, path = file.path(outdir), width = 14, height = 4, device = "png"
)



#### For text

rt1 <- Rt_dat %>%
  group_by(region) %>%
  filter(date <= as.Date("2020-03-31")) %>%
  group_by(region) %>%
  summarize(rt_median_initial = mean(rt_median))

rt2 <- Rt_dat %>%
  group_by(region) %>%
  filter(date == as.Date("2020-06-01")) %>%
  mutate(rt_median_lowest = rt_median) %>%
  select(region, rt_median_lowest)

rt3 <- Rt_dat %>%
  group_by(region) %>%
  filter(date == as.Date("2020-09-01")) %>%
  mutate(rt_median_current = rt_median) %>%
  select(region, rt_median_current)


rt123 <- f_addVar(rt1, rt2) %>%
  f_addVar(rt3) %>%
  mutate(
    rt_initial_lowest = (1 - (rt_median_lowest / rt_median_initial)) * 100,
    rt_lowest_current = ((rt_median_current / rt_median_lowest)) * 100
  )
rt123
