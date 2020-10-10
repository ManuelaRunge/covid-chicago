### =======================================================
#### Additional plots for contact tracing simulations
### =======================================================

pdfdir <- "C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/project_notes/publications/covid_model_IL/pdfs_ais/"

#### Plots edited for publication
library(tidyverse)
library(cowplot)
library(scales)
library(viridis)
library(data.table)

theme_set(theme_cowplot())

# setwd("/home/mrm9534/gitrepos/covid-chicago/Rfiles/")
source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("ct_analysis/helper_functions_CT.R")
# region_cols <- c()
restoreRegion_cols <- c("Central" = "red2", "Northcentral" = "dodgerblue3", "Northeast" = "chartreuse4", "Southern" = "orchid4")

startdate <- "2020-06-15"
stopdate <- "2020-12-30"
reopen <- c(0, 0.05, 0.1)
customTheme <- f_getCustomTheme()
ct_startdate <- as.Date("2020-07-30")



## ================================================
###  HS scenarios with contact tracing
## ================================================
f_getPredDat <- function(expDIR) {
  trajectoriesDat <- read.csv(file.path(expDIR, "trajectoriesDat.csv"))
  unique(trajectoriesDat$reopening_multiplier_4)
  unique(trajectoriesDat$fraction_critical)
  
  restore_region_names <- c("southern", "central", "northcentral", "northeast")
  
  colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))
  
  
  ### per restore region
  paramvars <- c(
    paste0("deaths_", restore_region_names),
    paste0("hosp_cumul_", restore_region_names),
    paste0("hospitalized_det_", restore_region_names),
    paste0("hospitalized_", restore_region_names),
    paste0("infected_", restore_region_names),
    paste0("deaths_det_", restore_region_names),
    paste0("prevalence_", restore_region_names),
    paste0("crit_cumul_", restore_region_names)
  )
  
  
  keepvars <- c("time", "startdate", "scen_num", "reopening_multiplier_4", "d_AsP_ct1_grp", "d_AsP_ct1", "reduced_inf_of_det_cases_ct1", paramvars)
  
  trajectoriesDat$d_AsP_ct1_grp <- NA
  trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 <= 0.05] <- "<0.05"
  trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 >= 0.05 & trajectoriesDat$d_AsP_ct1 <= 0.1] <- "<0.1"
  trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 >= 0.1 & trajectoriesDat$d_AsP_ct1 <= 0.2] <- "0.1-0.2"
  trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 >= 0.2 & trajectoriesDat$d_AsP_ct1 <= 0.3] <- "0.2-0.3"
  trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 >= 0.4 & trajectoriesDat$d_AsP_ct1 <= 0.4] <- "0.3-0.4"
  trajectoriesDat$d_AsP_ct1_grp[trajectoriesDat$d_AsP_ct1 >= 0.5] <- ">0.5"
  
  
  predDat <- trajectoriesDat %>%
    dplyr::select(keepvars) %>%
    dplyr::group_by(time, startdate, reopening_multiplier_4, d_AsP_ct1_grp) %>%
    dplyr::filter(reduced_inf_of_det_cases_ct1 == max(reduced_inf_of_det_cases_ct1)) %>%
    dplyr::mutate(date = as.Date(startdate) + time) %>%
    dplyr::filter(date <= as.Date("2020-12-31")) %>%
    pivot_longer(cols = -c("time", "date", "startdate", "reopening_multiplier_4", "d_AsP_ct1_grp"), names_to = "name") %>%
    dplyr::mutate(
      name = gsub("_cumul_", ".cumul_", name),
      name = gsub("_det_", ".det_", name)
    ) %>%
    separate(name, into = c("param", "restore_region"), sep = "_") %>%
    dplyr::mutate(
      param = gsub(".cumul", "_cumul", param),
      param = gsub(".det", "_det", param)
    ) %>%
    dplyr::mutate(
      exp_name = exp_name
    ) %>%
    dplyr::group_by(date, restore_region, param, exp_name, reopening_multiplier_4, d_AsP_ct1_grp) %>%
    dplyr::summarize(
      mean.val = mean(value, na.rm = TRUE),
      sd.val = sd(value, na.rm = TRUE),
      n.val = n(),
    ) %>%
    dplyr::mutate(
      se.val = sd.val / sqrt(n.val),
      lower.ci.val = mean.val - qt(1 - (0.05 / 2), n.val - 1) * se.val,
      upper.ci.val = mean.val + qt(1 - (0.05 / 2), n.val - 1) * se.val
    )
  
   predDat$restore_region <- str_to_sentence(predDat$restore_region)
  
  return(predDat)
}

combineDat=FALSE
if(combineDat){
  source("load_paths.R")
  exp_name <- "20200731_IL_reopen_contactTracing"
  expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)
  predDat_CT <- f_getPredDat(expDIR) %>% mutate(scenario = "CTonly")
  
  exp_name <- "20200731_IL_reopen_contactTracingHS40"
  expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)
  predDat_HS40 <- f_getPredDat(expDIR) %>% mutate(scenario = "HS40")
  
  exp_name <- "20200731_IL_reopen_contactTracingHS40TD"
  expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)
  predDat_HS40TD <- f_getPredDat(expDIR) %>% mutate(scenario = "HS40TD")
  
  exp_name <- "20200731_IL_reopen_contactTracingHS80"
  expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)
  predDat_HS80 <- f_getPredDat(expDIR) %>% mutate(scenario = "HS80")
  
  
  exp_name <- "20200731_IL_reopen_contactTracingHS80TD"
  expDIR <- file.path(simulation_output, "contact_tracing/20200731/", exp_name)
  predDat_HS80TD <- f_getPredDat(expDIR) %>% mutate(scenario = "HS80TD")
  
  
  predDat <- rbind(predDat_HS40, predDat_HS40TD, predDat_HS80, predDat_HS80TD)
  
  table(predDat$restore_region, predDat$scenario)
  tapply(predDat$date, predDat$scenario, summary)
  
  table(predDat$d_AsP_ct1_grp, predDat$scenario)
  table(predDat$reopening_multiplier_4, predDat$scenario)
  tapply(predDat$mean.val, predDat$scenario, summary)
  
  
  
  save(predDat, file = file.path(simulation_output, "contact_tracing/20200731/", "reopen_contactTracingAll.Rdata"))
  
}
load( file.path(simulation_output, "contact_tracing/20200731/", "reopen_contactTracingAll.Rdata"))


f_linePlotIL <- function() {
  ppredDat <- predDat %>%
    dplyr::mutate(geography_name = tolower(restore_region)) %>%
    dplyr::left_join(capacity, by = "geography_name") %>%
    dplyr::filter(param %in% c("hospitalized") &
                    date >= as.Date("2020-07-01") & date <= as.Date("2020-12-01")) %>%
    mutate(
      HSgrp = ifelse(scenario %in% c("HS40", "HS40TD"), "dSym 40%", "dSym 80%"),
      scenario2 = paste0(scenario, " ", d_AsP_ct1_grp),
      scenario3 = gsub("HS40", "", gsub("HS80", "", scenario2))
    ) %>%
    dplyr::group_by(date, reopening_multiplier_4, scenario, d_AsP_ct1_grp, scenario3) %>%
    dplyr::summarize(
      mean.val = sum(mean.val),
      medsurg_available = sum(medsurg_available)
    ) %>%
    ggplot() +
    theme_cowplot() +
    # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_line(aes(
      x = date,
      y = mean.val,
      col = as.factor(d_AsP_ct1_grp),
      group = interaction(d_AsP_ct1_grp, reopening_multiplier_4)
    ), size = 1.3) +
    geom_hline(aes(yintercept = medsurg_available),
               linetype = "dashed", col = "grey", size = 1.7
    ) +
    facet_wrap(reopening_multiplier_4 ~ scenario, scales = "free_y", nrow = 4) +
    scale_y_continuous(label = comma, expand = c(0, 0)) +
    scale_color_brewer(palette = "Dark2", drop=FALSE) +
    customThemeNoFacet +
    labs(y = "hospitalized", x = "") +
    theme(legend.position = "right")
  
  
  ggsave(paste0("HS_contactTracing_scenarios_hospitalized_IL", ".pdf"),
         plot = ppred, path = file.path(expDIR), width = 12, height = 12, device = "pdf"
  )
}


f_linePlotRR <- function() {
  ppredDat1 <- predDat %>%
    dplyr::mutate(geography_name = tolower(restore_region)) %>%
    dplyr::left_join(capacity, by = "geography_name") %>%
    dplyr::filter(param %in% c("hospitalized") &
                    date >= as.Date("2020-07-01") & date <= as.Date("2020-12-01")) %>%
    mutate(
      HSgrp = ifelse(scenario %in% c("HS40", "HS40TD"), "dSym 40%", "dSym 80%"),
      scenario2 = paste0(scenario, " ", d_AsP_ct1_grp),
      scenario3 = gsub("HS40", "", gsub("HS80", "", scenario2))
    )
  
  ppred <- ppredDat1 %>%
    filter(reopening_multiplier_4 == 0) %>%
    ggplot() +
    theme_cowplot() +
    # geom_ribbon(aes(x=date , ymin=lower.ci.val , ymax=  upper.ci.val , fill=restore_region , group=reopening_multiplier_4),size=1, alpha=0.3) +
    geom_line(aes(
      x = date,
      y = mean.val,
      col = as.factor(scenario2),
      group = interaction(scenario2)
    ), size = 1.3) +
    geom_hline(aes(yintercept = medsurg_available),
               linetype = "dashed", col = "grey", size = 1.7
    ) +
    facet_wrap(~restore_region, scales = "free_y", nrow = 4) +
    scale_y_continuous(label = comma, expand = c(0, 0)) +
    # scale_color_brewer(palette="Dark2")+
    customThemeNoFacet +
    labs(y = "hospitalized", x = "") +
    theme(legend.position = "right")
  
  
  ggsave(paste0("HS_contactTracing_scenarios_hospitalized_IL", ".pdf"),
         plot = ppred, path = file.path(expDIR), width = 12, height = 12, device = "pdf"
  )
}



if (testplot) {
  ppredDat <- predDat %>%
    dplyr::mutate(geography_name = tolower(restore_region)) %>%
    dplyr::left_join(capacity, by = "geography_name") %>%
    dplyr::filter(
      d_AsP_ct1_grp %in% c("<0.05", ">0.5"),
      param %in% c("hosp_cumul") &
        date >= as.Date("2020-12-30") & date <= as.Date("2020-12-31")
    )
  
  ppred <- ggplot(data = ppredDat) +
    theme_cowplot() +
    geom_bar(aes(
      x = scenario,
      y = mean.val,
      fill = as.factor(restore_region),
      group = interaction(restore_region)
    ), position = "stack", stat = "identity", size = 1.3) +
    facet_wrap(d_AsP_ct1_grp ~ reopening_multiplier_4, scales = "free_y", nrow = 2) +
    scale_y_continuous(label = comma, expand = c(0, 0)) +
    scale_fill_manual(values = restoreRegion_cols) +
    customThemeNoFacet +
    labs(y = "hospitalized", x = "") +
    theme(legend.position = "right") +
    coord_flip()
  
  
  ggsave(paste0("test_plot", ".pdf"),
         plot = ppred, path = file.path(expDIR), width = 12, height = 12, device = "pdf"
  )
}



ppredDat <- predDat %>%
  dplyr::mutate(geography_name = tolower(restore_region)) %>%
  dplyr::left_join(capacity, by = "geography_name") %>%
  dplyr::filter(
    d_AsP_ct1_grp %in% c("<0.05", "<0.1", "0.1-0.2", "0.2-0.3"),
    param %in% c("hosp_cumul") & date >= as.Date("2020-12-30") & date <= as.Date("2020-12-31")
  ) %>%
  mutate(
    HSgrp = ifelse(scenario %in% c("HS40", "HS40TD"), "dSym 40%", "dSym 80%"),
    scenario2 = paste0(scenario, " ", d_AsP_ct1_grp),
    scenario3 = gsub("HS40", "", gsub("HS80", "", scenario2))
  ) %>%
  dplyr::group_by(HSgrp, reopening_multiplier_4, d_AsP_ct1_grp, scenario, scenario2, scenario3) %>%
  dplyr::summarize(
    mean.val = sum(mean.val),
    hospitalized = sum(hospitalized)
  )


pall <- ppredDat %>%
  ggplot() +
  theme_cowplot() +
  geom_bar(aes(
    x = reorder(as.factor(scenario), mean.val, desc = FALSE),
    y = mean.val,
    fill = d_AsP_ct1_grp,
    group = scenario3
  ),
  position = position_dodge(width = 0.9), col = "darkgrey", stat = "identity", size = 0.7
  ) +
  facet_wrap(~reopening_multiplier_4, scales = "free") +
  scale_y_continuous(label = comma, expand = c(0, 0)) +
  scale_fill_brewer(palette = "Greens") +
  customThemeNoFacet +
  labs(y = "hospitalized", x = "") +
  theme(legend.position = "right")
ggsave(paste0("CT_scenarios_allReopen", ".pdf"),
       plot = pall, path = file.path(expDIR), width = 12, height = 7, device = "pdf"
)


p0 <- ppredDat %>%
  filter(reopening_multiplier_4 == 0 &
           scenario2) %>%
  ggplot() +
  theme_cowplot() +
  geom_bar(aes(
    x = reorder(as.factor(scenario), mean.val, desc = FALSE),
    y = mean.val,
    fill = d_AsP_ct1_grp,
    group = scenario3
  ),
  position = position_dodge(width = 0.9), col = "darkgrey", stat = "identity", size = 0.7
  ) +
  facet_wrap(~HSgrp, scales = "free_x") +
  scale_y_continuous(label = comma, expand = c(0, 0)) +
  scale_fill_brewer(palette = "Greens") +
  customThemeNoFacet +
  labs(y = "hospitalized", x = "") +
  theme(legend.position = "right")
ggsave(paste0("CT_scenarios_baseline", ".pdf"),
       plot = p0, path = file.path(expDIR), width = 12, height = 7, device = "pdf"
)




##### Over time
overTime <- FALSE
if (overTime) {
  ppredDat <- predDat %>%
    dplyr::mutate(geography_name = tolower(restore_region)) %>%
    dplyr::left_join(capacity, by = "geography_name") %>%
    dplyr::filter(
      d_AsP_ct1_grp %in% c("<0.05", "<0.1", "0.1-0.2", "0.2-0.3"),
      param %in% c("hospitalized") & date <= as.Date("2020-12-31")
    ) %>%
    mutate(
      HSgrp = ifelse(scenario %in% c("HS40", "HS40TD"), "dSym 40%", "dSym 80%"),
      scenario2 = paste0(scenario, " ", d_AsP_ct1_grp),
      scenario3 = gsub("HS40", "", gsub("HS80", "", scenario2))
    ) %>%
    dplyr::group_by(HSgrp, reopening_multiplier_4, d_AsP_ct1_grp, scenario, scenario2, scenario3) %>%
    dplyr::summarize(mean.val = sum(mean.val), hospitalized = sum(hospitalized))
  
  
  ppredDat %>%
    ggplot() +
    theme_cowplot() +
    geom_line(aes(
      x = reorder(as.factor(scenario), mean.val, desc = FALSE),
      y = mean.val,
      fill = d_AsP_ct1_grp,
      group = scenario3
    ),
    position = position_dodge(width = 0.9), col = "darkgrey", stat = "identity", size = 1.3
    ) +
    facet_wrap(~reopening_multiplier_4, scales = "free") +
    scale_y_continuous(label = comma, expand = c(0, 0)) +
    scale_fill_brewer(palette = "greens") +
    customThemeNoFacet +
    labs(y = "hospitalized", x = "") +
    theme(legend.position = "right")
}
