## ============================================================
## R script to get R(t) from simulation outputs
## ============================================================

# install.packages("devtools")
# library(devtools)
# install_github("annecori/EpiEstim", force = TRUE)
library(tidyverse)
library(EpiEstim)
library(readr)

runViaSource <- TRUE

simdate <- gsub("-", "", Sys.Date())
exp_scenario <- "baseline"

source("load_paths.R")
source("processing_helpers.R")
source("estimate_Rt/getRt_function.R")


### Load simulation outputs
exp_name <- "20200825_IL_RR_baseline_0"
# Rt_dat <- read.csv(file.path(simulation_output, exp_name, 'estimatedRt', 'combined_estimated_Rt.csv'))
load(file.path(simulation_output, exp_name, "estimatedRt", "combined_estimated_Rt.Rdata"))

### Combine list to dataframe
Rt_dat <- Rt_dat %>%
  mutate(time = t_end) %>%
  f_addRestoreRegion() %>%
  select(time, scen_num, restore_region, region, `Mean(R)`, `Median(R)`) %>%
  rename(meanRt = `Mean(R)`, medianRt = `Median(R)`)

Rt_datRR <- Rt_dat %>%
  group_by(restore_region, time, scen_num) %>%
  summarize(meanRt = mean(meanRt), medianRt = mean(medianRt)) %>%
  rename(geography_modeled = restore_region)

Rt_datIL <- Rt_dat %>%
  group_by(time, scen_num) %>%
  summarize(meanRt = mean(meanRt), medianRt = mean(medianRt)) %>%
  mutate(geography_modeled = "illinois")


Rt_dat <- Rt_dat %>%
  rename(geography_modeled = region) %>%
  select(-restore_region) %>%
  rbind(Rt_datRR) %>%
  rbind(Rt_datIL)

table(Rt_dat$geography_modeled)



RtdatCombined <- Rt_dat %>%
  dplyr::group_by(time, geography_modeled) %>%
  dplyr::summarize(
    Median.of.covid.19.Rt = median(medianRt),
    Lower.error.bound.of.covid.19.Rt = quantile(medianRt, probs = 0.025, na.rm = TRUE),
    Upper.error.bound.of.covid.19.Rt = quantile(medianRt, probs = 0.975, na.rm = TRUE)
  ) %>%
  dplyr::mutate(Date = as.Date("2020-02-13") + time) %>%
  dplyr::arrange(Date, geography_modeled) %>%
  filter(Date <= "2020-12-01") %>%
  mutate(geography_modeled = paste0("covidregion_", geography_modeled)) %>%
  ungroup() %>%
  dplyr::select(-time)

RtdatCombined$geography_modeled <- gsub("covidregion_Central", "Central", RtdatCombined$geography_modeled)
RtdatCombined$geography_modeled <- gsub("covidregion_illinois", "illinois", RtdatCombined$geography_modeled)
RtdatCombined$geography_modeled <- gsub("covidregion_Northcentral", "Northcentral", RtdatCombined$geography_modeled)
RtdatCombined$geography_modeled <- gsub("covidregion_Northeast", "Northeast", RtdatCombined$geography_modeled)
RtdatCombined$geography_modeled <- gsub("covidregion_Southern", "Southern", RtdatCombined$geography_modeled)
RtdatCombined$geography_modeled <- tolower(RtdatCombined$geography_modeled)


saveForCivis <- TRUE
if (saveForCivis) {
  simdate <- "20200825"
  fname <- paste0("nu_il_", exp_scenario, "_", simdate, ".csv")

  ## Combine to baseline csv file
  bsl <- read_csv(file.path(project_path, "NU_civis_outputs", simdate, "csv", fname))

  mergevars <- colnames(bsl)[colnames(bsl) %in% colnames(RtdatCombined)]
  dat <- merge(bsl, RtdatCombined, by = mergevars, all.x = TRUE)

  if (dim(bsl)[1] == dim(dat)[1]) {
    write.csv(dat, file = file.path(project_path, "NU_civis_outputs", simdate, "csv", fname), row.names = FALSE)
    write.csv(dat, file = file.path(simulation_output, exp_name, fname), row.names = FALSE)
  }
}

generatePlots <- F
if (generatePlots) {
  library(cowplot)

  RtdatCombined$region <- factor(RtdatCombined$geography_modeled, levels = paste0("covidregion_", c(1:11)), labels = c(1:11))

  pplot <- RtdatCombined %>%
    filter(Date >= "2020-04-01" & Date <= "2020-11-01") %>%
    filter(region %in% as.character(c(1:11))) %>%
    ggplot() +
    theme_cowplot() +
    geom_rect(xmin = -Inf, xmax = as.Date(Sys.Date()), ymin = -Inf, ymax = Inf, fill = "lightgrey", alpha = 0.02) +
    geom_ribbon(aes(x = Date, ymin = Lower.error.bound.of.covid.19.Rt, ymax = Upper.error.bound.of.covid.19.Rt), fill = "deepskyblue3", alpha = 0.3) +
    geom_line(aes(x = Date, y = Median.of.covid.19.Rt), col = "deepskyblue3") +
    facet_wrap(~region) +
    # background_grid() +
    # geom_hline(yintercept = seq(0.6, 1.4, 0.2), col="grey", size=0.7)+
    geom_hline(yintercept = 1) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    customThemeNoFacet

  ggsave(paste0("estimatedRt_overtime.png"),
    plot = pplot, path = file.path(simulation_output, exp_name, "estimatedRt"), width = 12, height = 6, device = "png"
  )
  rm(pplot)


  pplot <- RtdatCombined %>%
    filter(Date >= "2020-08-18" & Date < "2020-08-19") %>%
    filter(region %in% as.character(c(1:11))) %>%
    ggplot() +
    theme_cowplot() +
    geom_errorbar(aes(x = reorder(region, Median.of.covid.19.Rt), ymin = Lower.error.bound.of.covid.19.Rt, ymax = Upper.error.bound.of.covid.19.Rt), width = 0.3, alpha = 0.3) +
    geom_point(aes(x = reorder(region, Median.of.covid.19.Rt), y = Median.of.covid.19.Rt), col = "deepskyblue3", size = 2.5) +
    geom_hline(yintercept = 1) +
    customThemeNoFacet

  ggsave(paste0("estimatedRt_20200819.png"),
    plot = pplot, path = file.path(simulation_output, exp_name, "estimatedRt"), width = 8, height = 5, device = "png"
  )


  compareWithOverflow <- F
  if (compareWithOverflow) {
    nu_hospitaloverflow <- read.csv(file.path(simulation_output, exp_name, "nu_hospitaloverflow_20200826.csv"))


    dt <- nu_hospitaloverflow %>% mutate(date_window_upper_bound = as.Date(date_window_upper_bound), region = gsub("covidregion_", "", geography_modeled))

    dt$region <- factor(dt$region, levels = c(1:11), labels = c(1:11))
    RtdatCombined$region <- factor(RtdatCombined$region, levels = c(1:11), labels = c(1:11))

    pplot <- RtdatCombined %>%
      filter(Date >= "2020-07-01" & Date <= "2020-11-01") %>%
      filter(region %in% as.character(c(1:11))) %>%
      ggplot() +
      theme_cowplot() +
      geom_rect(xmin = -Inf, xmax = as.Date(Sys.Date()), ymin = -Inf, ymax = Inf, fill = "lightgrey", alpha = 0.02) +
      geom_ribbon(aes(x = Date, ymin = Lower.error.bound.of.covid.19.Rt, ymax = Upper.error.bound.of.covid.19.Rt), fill = "deepskyblue3", alpha = 0.4) +
      geom_line(aes(x = Date, y = Median.of.covid.19.Rt), col = "deepskyblue3") +
      geom_hline(yintercept = 1) +
      geom_line(
        data = subset(dt),
        aes(
          x = date_window_upper_bound, y = percent_of_simulations_that_exceed + 1,
          col = as.factor(overflow_threshold_percent), linetype = resource_type,
          group = interaction(resource_type, overflow_threshold_percent)
        ), size = 1
      ) +
      facet_wrap(~region, nrow = 3) +
      scale_y_continuous(expr(italic(R[t])), sec.axis = sec_axis(~ . - 1, name = " overflow\nprobability")) +
      labs(x = "", color = "overflow threshold (%)", linetype = "resource type") +
      geom_hline(yintercept = c(-Inf, Inf)) +
      geom_vline(xintercept = c(-Inf, Inf)) +
      scale_color_manual(values = c("lightcoral", "firebrick4")) +
      customThemeNoFacet

    ggsave(paste0("estimatedRt_overtime_withOverflow.png"),
      plot = pplot, path = file.path(simulation_output, exp_name, "estimatedRt"), width = 12, height = 6, device = "png"
    )
  }
}
