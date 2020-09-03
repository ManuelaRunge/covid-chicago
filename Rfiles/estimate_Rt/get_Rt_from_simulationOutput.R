## ============================================================
## R script to get R(t) from simulation outputs
## ============================================================

# install.packages("devtools")
# library(devtools)
# install_github("annecori/EpiEstim", force = TRUE)
library(tidyverse)
library(EpiEstim)
library(readr)
library(cowplot)

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
  rename(meanRt = `Mean(R)`, 
         medianRt = `Median(R)`,
         q2.5Rt = `Quantile.0.025(R)`,
         q25Rt = `Quantile.0.25(R)`,
         q75Rt = `Quantile.0.75(R)`,
         q97.5Rt = `Quantile.0.975(R)`)%>%
  rename(geography_modeled = region) 

Rt_datIL <- Rt_dat %>%
  group_by(time, scen_num) %>%
  summarize(meanRt = mean(meanRt), 
            medianRt = mean(medianRt),
            q2.5Rt = mean(q2.5Rt),
            q25Rt = mean(q25Rt),
            q75Rt = mean(q75Rt),
            q97.5Rt = mean(q97.5Rt)) %>%
  mutate(geography_modeled = "illinois")


Rt_dat <- Rt_dat %>%
  select(colnames(Rt_datIL)) %>%
  rbind(Rt_datIL)

table(Rt_dat$geography_modeled)


### Aggregate scenarios and use confidence intervals from Rt estimation
Rt_dat <- Rt_dat %>%
  group_by(time,geography_modeled ) %>%
  summarize(meanRt = mean(meanRt), 
            medianRt = mean(medianRt),
            q2.5Rt = mean(q2.5Rt),
            q25Rt = mean(q25Rt),
            q75Rt = mean(q75Rt),
            q97.5Rt = mean(q97.5Rt))


RtdatCombined <- Rt_dat %>%
  dplyr::group_by(time, geography_modeled) %>%
  dplyr::rename(
    Median.of.covid.19.Rt = medianRt,
    Lower.error.bound.of.covid.19.Rt = q2.5Rt,
    Upper.error.bound.of.covid.19.Rt = q97.5Rt
  ) %>%
  dplyr::mutate(Date = as.Date("2020-02-13") + time) %>%
  dplyr::arrange(Date, geography_modeled) %>%
  filter(Date <= "2020-12-01") %>%
  mutate(geography_modeled = paste0("covidregion_", geography_modeled)) %>%
  ungroup() %>%
  dplyr::select(-time)


RtdatCombined$geography_modeled <- gsub("covidregion_illinois", "illinois", RtdatCombined$geography_modeled)
RtdatCombined$geography_modeled <- tolower(RtdatCombined$geography_modeled)


saveForCivis <- TRUE
if (saveForCivis) {
  simdate <- "20200902"
  fname <- paste0("nu_", simdate, ".csv")

  ## Combine to baseline csv file
  bsl <- read_csv(file.path(project_path, "NU_civis_outputs", simdate, "csv", fname))

  mergevars <- colnames(bsl)[colnames(bsl) %in% colnames(RtdatCombined)]
  dat <- merge(bsl, RtdatCombined, by = mergevars, all.x = TRUE)

   dat <- dat %>%
     mutate(scenario_name="baseline") %>%
     filter(geography_modeled %in% c("illinois",paste0("covidregion_",c(1:11)))) %>%
     select(date ,geography_modeled ,scenario_name ,cases_median ,cases_lower ,cases_upper ,cases_new_median ,cases_new_lower ,cases_new_upper ,
            deaths_median ,deaths_lower ,deaths_upper ,deaths_det_median ,deaths_det_lower ,deaths_det_upper ,hosp_bed_median ,hosp_bed_lower ,hosp_bed_upper ,
            icu_median ,icu_lower ,icu_upper ,
            vent_median ,vent_lower ,vent_upper ,recovered_median ,recovered_lower ,recovered_upper ,rt_median ,rt_lower ,rt_upper)
  
  
  
  if (dim(bsl)[1] == dim(dat)[1]) {
    write.csv(dat, file = file.path(project_path, "NU_civis_outputs", simdate, "csv", fname), row.names = FALSE)
    write.csv(dat, file = file.path(simulation_output, exp_name, fname), row.names = FALSE)
  }
}

generatePlots <- F
if (generatePlots) {

  RtdatCombined$region <- factor(RtdatCombined$geography_modeled, levels = paste0("covidregion_", c(1:11)), labels = c(1:11))

  pplot <- RtdatCombined %>%
    filter(Date >= "2020-04-01" & Date <= "2020-11-01") %>%
    filter(region %in% as.character(c(1:11))) %>%
    ggplot() +
    theme_cowplot() +
   # geom_rect(xmin = -Inf, xmax = as.Date(Sys.Date()), ymin = -Inf, ymax = Inf, fill = "lightgrey", alpha = 0.02) +
    geom_ribbon(aes(x = Date, ymin = Lower.error.bound.of.covid.19.Rt, ymax = Upper.error.bound.of.covid.19.Rt), fill = "deepskyblue3", alpha = 0.3) +
    geom_line(aes(x = Date, y = Median.of.covid.19.Rt), col = "deepskyblue3") +
    facet_wrap(~region, scales="free_x") +
    theme(panel.spacing = unit(1, "lines"))+
    # background_grid() +
    # geom_hline(yintercept = seq(0.6, 1.4, 0.2), col="grey", size=0.7)+
    geom_hline(yintercept = 1) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    customThemeNoFacet+
    labs(y=expression(italic(R[t])), caption="Shaded area = uncertainity in Rt estimation")+
    scale_x_date(date_breaks = "30 days", date_labels = "%d\n%b")

  ggsave(paste0("estimatedRt_overtime.png"),
    plot = pplot, path = file.path(simulation_output, exp_name, "estimatedRt"), width = 12, height = 8, device = "png"
  )
  ggsave(paste0("estimatedRt_overtime.pdf"),
         plot = pplot, path = file.path(simulation_output, exp_name, "estimatedRt"), width = 12, height = 8, device = "pdf"
  )
  
  rm(pplot)

  
  pplot <- RtdatCombined %>%
    filter(Date >= "2020-04-01" & Date <= "2020-11-01") %>%
    filter(geography_modeled=="illinois") %>%
    ggplot() +
    theme_cowplot() +
   # geom_rect(xmin = -Inf, xmax = as.Date(Sys.Date()), ymin = -Inf, ymax = Inf, fill = "lightgrey", alpha = 0.02) +
    geom_ribbon(aes(x = Date, ymin = Lower.error.bound.of.covid.19.Rt, ymax = Upper.error.bound.of.covid.19.Rt), fill = "deepskyblue3", alpha = 0.3) +
    geom_line(aes(x = Date, y = Median.of.covid.19.Rt), col = "deepskyblue3") +
    # background_grid() +
    # geom_hline(yintercept = seq(0.6, 1.4, 0.2), col="grey", size=0.7)+
    geom_hline(yintercept = 1) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    customThemeNoFacet +
    labs(y=expression(italic(R[t])), caption="Shaded area = uncertainity in Rt estimation")+
    scale_x_date(date_breaks = "30 days", date_labels = "%d\n%b")
  
  ggsave(paste0("IL_estimatedRt_overtime.png"),
         plot = pplot, path = file.path(simulation_output, exp_name, "estimatedRt"), width = 8, height = 6, device = "png"
  )
  ggsave(paste0("IL_estimatedRt_overtime.pdf"),
         plot = pplot, path = file.path(simulation_output, exp_name, "estimatedRt"), width = 8, height = 6, device = "pdf"
  )
  
  rm(pplot)
  

  
  ### Change date to today
  pplot <- RtdatCombined %>%
    filter(Date >= "2020-08-18" & Date < "2020-08-19") %>%
    filter(region %in% as.character(c(1:11))) %>%
    ggplot() +
    theme_cowplot() +
    geom_errorbar(aes(x = reorder(region, Median.of.covid.19.Rt), ymin = Lower.error.bound.of.covid.19.Rt, ymax = Upper.error.bound.of.covid.19.Rt), width = 0.3, alpha = 0.3) +
    geom_point(aes(x = reorder(region, Median.of.covid.19.Rt), y = Median.of.covid.19.Rt), col = "deepskyblue3", size = 2.5) +
    geom_hline(yintercept = 1) +
    labs(caption="Error bounds based on uncertainty in Rt estimation")+
    customThemeNoFacet

  
  ### Change name suffix to today
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
