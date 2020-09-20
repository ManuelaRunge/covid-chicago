
library(tidyverse)
library(cowplot)
library(scales)
library(viridis)
require(data.table)

source("load_paths.R")
source("processing_helpers.R")

theme_set(theme_cowplot())

simdate <- "20200917"
exp_name <- "20200917_IL_gradual_reopening"
plot_dir <- file.path(simulation_output, exp_name, "_plots")


region_names <- c("All", paste0("EMS-", c(1:11)))
outcomevars <- c(
  paste0("hosp_det_", region_names),
  paste0("hospitalized_", region_names),
  paste0("crit_det_", region_names),
  paste0("critical_", region_names)
)

region_names <- paste0("EMS-", c(1:11))
outcomevars2 <- c(
  paste0("Ki_t_", region_names)
)

paramvars <- c("reopening_multiplier_4")
keepvars <- c("time", "startdate", "scen_num", "sample_num", paramvars, outcomevars, outcomevars2)



trajectoriesDat <- fread(file.path(simulation_output, exp_name, "trajectoriesDat.csv"), select = c(keepvars))

if (!("run_num" %in% colnames(trajectoriesDat))) {
  samplesDat <- read_csv(file.path(simulation_output, exp_name, "sampled_parameters.csv"))
  mergevars <- colnames(samplesDat)[colnames(samplesDat) %in% colnames(trajectoriesDat)]
  trajectoriesDat <- left_join(trajectoriesDat, samplesDat, by = mergevars)
}
### Note use Ki_EMS-  or Ki_EMS.   not Ki_EMS_  (time varying vs input parameter)
# colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))

table(trajectoriesDat[, c("reopening_multiplier_4")])
paramvalues <- trajectoriesDat %>%
  select(keepvars) %>%
  filter(time > 120) %>%
  mutate(date = as.Date(startdate) + time) %>%
  pivot_longer(cols = -c("time", "date", "startdate", "reopening_multiplier_4"), names_to = "region") %>%
  separate(region, into = c("outcome", "region"), sep = "_EMS-") %>%
  mutate(
    exp_name = exp_name,
  ) %>%
  group_by(date, region, exp_name, reopening_multiplier_4, outcome) %>%
  summarize(
    median.value = median(value, na.rm = TRUE),
    q25.value = quantile(value, probs = 0.25, na.rm = TRUE),
    q75.value = quantile(value, probs = 0.75, na.rm = TRUE),
    q2.5.value = quantile(value, probs = 0.025, na.rm = TRUE),
    q97.5.value = quantile(value, probs = 0.975, na.rm = TRUE)
  )


capacityDat <- load_new_capacity() %>% mutate(region = as.character(geography_name))

table(paramvalues$region, exclude = NULL)
paramvalues$region[is.na(paramvalues$region)] <- "illinois"
paramvalues$region_label <- factor(paramvalues$region, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))
capacityDat$region_label <- factor(capacityDat$region, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))
paramvalues$reopening_multiplier_4 <- round(paramvalues$reopening_multiplier_4, 2) * 100

paramvalues <- paramvalues[!is.na(paramvalues$region_label), ]
dat <- paramvalues
save(dat, file = file.path(simulation_output, exp_name, "aggregatedDat_forR.Rdata"))

# library(RColorBrewer)
# getPalette <- colorRampPalette(brewer.pal(9, "PuBuGn"))(12)

### ---------------------------------------
## Timeline plot
### ---------------------------------------

pplot <- paramvalues %>%
  filter(outcome == "crit_det" & date >= as.Date("2020-08-01") & date <= as.Date("2021-01-01")) %>%
  filter(region != "illinois") %>%
  # filter(reopening_multiplier_4  %in% c(0,22,44, 56, 78, 100)) %>%
  group_by(region, reopening_multiplier_4, outcome) %>%
  left_join(capacityDat) %>%
  ggplot() +
  geom_ribbon(aes(x = date, ymin = q2.5.value, ymax = q97.5.value, fill = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = q25.value, ymax = q75.value, fill = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), alpha = 0.3) +
  geom_line(aes(x = date, y = median.value, col = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), show.legend = F, size = 1) +
  facet_wrap(~region_label, scales = "free") +
  labs(color = "Reopening", fill = "Reopening") +
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  geom_hline(aes(yintercept = icu_available), linetype = "dashed") +
  labs(x = "", y = "predicted ICU census") +
  customThemeNoFacet

ggsave(paste0("timeline_ICU_perCovidRegion.png"),
  plot = pplot, path = file.path(plot_dir), width = 14, height = 8, device = "png"
)
ggsave(paste0("timeline_ICU_perCovidRegion.pdf"),
  plot = pplot, path = file.path(plot_dir), width = 14, height = 8, device = "pdf"
)
rm(pplot)



### Zoomed

pplot <- paramvalues %>%
  filter(outcome == "crit_det" & date >= as.Date("2020-10-01") & date <= as.Date("2021-01-01")) %>%
  filter(region != "illinois") %>%
  filter(reopening_multiplier_4 >= 9) %>%
  group_by(region, reopening_multiplier_4, outcome) %>%
  left_join(capacityDat) %>%
  ggplot() +
  geom_ribbon(aes(x = date, ymin = q2.5.value, ymax = q97.5.value, fill = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = q25.value, ymax = q75.value, fill = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), alpha = 0.3) +
  geom_line(aes(x = date, y = median.value, col = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), show.legend = F, size = 1) +
  facet_wrap(~region_label, scales = "free") +
  labs(color = "Reopening", fill = "Reopening") +
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  geom_hline(aes(yintercept = icu_available), linetype = "dashed") +
  labs(x = "", y = "predicted ICU census") +
  customThemeNoFacet

ggsave(paste0("timeline_ICU_perCovidRegion_zoom.png"),
  plot = pplot, path = file.path(plot_dir), width = 14, height = 8, device = "png"
)
ggsave(paste0("timeline_ICU_perCovidRegion_zoom.pdf"),
  plot = pplot, path = file.path(plot_dir), width = 14, height = 8, device = "pdf"
)
rm(pplot)

pplot <- paramvalues %>%
  filter(outcome == "crit_det_All" & date >= as.Date("2020-09-01") & date <= as.Date("2021-01-01")) %>%
  filter(region == "illinois") %>%
  ggplot() +
  geom_ribbon(aes(x = date, ymin = q2.5.value, ymax = q97.5.value, fill = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = q25.value, ymax = q75.value, fill = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), alpha = 0.3) +
  geom_line(aes(x = date, y = median.value, col = as.factor(reopening_multiplier_4), group = reopening_multiplier_4), show.legend = F, size = 1) +
  labs(color = "Coverage", fill = "Coverage") +
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  # geom_hline(aes(yintercept = icu_available), linetype="dashed") +
  labs(x = "Reopening multiplier (%)", y = "Peak in ICU census until Jan 2020") +
  customThemeNoFacet

ggsave(paste0("timeline_ICU_IL.png"),
  plot = pplot, path = file.path(plot_dir), width = 10, height = 5, device = "png"
)
rm(pplot)

###### REOPENING PLOT

paramvalues$region_label <- factor(paramvalues$region, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))
capacityDat$region_label <- factor(capacityDat$region, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))


pplot <- paramvalues %>%
  filter(date >= as.Date("2020-08-19") & outcome == "hosp_det") %>%
  group_by(region, reopening_multiplier_4, outcome) %>%
  filter(median.value == max(median.value)) %>%
  left_join(capacityDat, by = c("region")) %>%
  mutate(belowCapacity = ifelse(median.value <= medsurg_available, 1, 0)) %>%
  ggplot() +
  geom_errorbar(aes(x = as.factor(reopening_multiplier_4), ymin = q2.5.value, ymax = q97.5.value, col = as.factor(belowCapacity)), width = 0.3) +
  geom_point(aes(x = as.factor(reopening_multiplier_4), y = median.value, fill = as.factor(belowCapacity)), shape = 21, show.legend = F, size = 2) +
  facet_wrap(~region, scales = "free") +
  scale_fill_manual(values = c("indianred", "deepskyblue3")) +
  geom_hline(aes(yintercept = medsurg_available)) +
  labs(x = "Reopening multiplier (%)", y = "Peak in non-ICU census until Jan 2020") +
  customThemeNoFacet

ggsave(paste0("nonICU_reopening_perCovidRegion.png"),
  plot = pplot, path = file.path(plot_dir), width = 14, height = 8, device = "png"
)
ggsave(paste0("nonICU_reopening_perCovidRegion.pdf"),
  plot = pplot, path = file.path(plot_dir), width = 14, height = 8, device = "pdf"
)
rm(pplot)



pplot <- paramvalues %>%
  filter(date >= as.Date("2020-08-19") & outcome == "crit_det") %>%
  group_by(region, reopening_multiplier_4, outcome) %>%
  filter(median.value == max(median.value)) %>%
  left_join(capacityDat) %>%
  mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0)) %>%
  ggplot() +
  geom_errorbar(aes(x = as.factor(reopening_multiplier_4), ymin = q2.5.value, ymax = q97.5.value, col = as.factor(belowCapacity)), width = 0.3) +
  geom_point(aes(x = as.factor(reopening_multiplier_4), y = median.value, fill = as.factor(belowCapacity)), shape = 21, show.legend = F, size = 2) +
  facet_wrap(~region, scales = "free") +
  scale_fill_manual(values = c("indianred", "deepskyblue3")) +
  geom_hline(aes(yintercept = icu_available)) +
  labs(x = "Reopening multiplier (%)", y = "Peak in ICU census until Jan 2020") +
  customThemeNoFacet

ggsave(paste0("ICU_reopening_perCovidRegion.png"),
  plot = pplot, path = file.path(plot_dir), width = 14, height = 8, device = "png"
)
ggsave(paste0("ICU_reopening_perCovidRegion.pdf"),
  plot = pplot, path = file.path(plot_dir), width = 14, height = 8, device = "pdf"
)
rm(pplot)
