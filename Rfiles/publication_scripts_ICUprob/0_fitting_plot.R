

library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

theme_set(theme_minimal())
simcolor <- "#F6921E"
capacitycolor <- "dodgerblue2"
customTheme <- f_getCustomTheme()

### ------------------------------
### Run script
### ------------------------------
subregions <- c(1, 4, 11)
stopdate <- as.Date("2020-12-31") # as.Date("2020-09-01") #
stopdate_data <- as.Date("2020-09-01") # as.Date("2020-09-01") #
enddate <- as.character(stopdate)

simdate <- "20201212"
sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (!dir.exists(file.path(sim_dir, "ICU_data_comparison_plots"))) dir.create(file.path(sim_dir, "ICU_data_comparison_plots"))
if (!dir.exists(file.path(sim_dir, "ICU_data_comparison_plots", "pdf"))) dir.create(file.path(sim_dir, "ICU_data_comparison_plots", "pdf"))

exp_name_counterfactual_1 <- "20201212_IL_regreopen50perc_counterfactual"
exp_name_counterfactual_2 <- "20201212_IL_regreopen100perc_counterfactual"
exp_name_baseline <- "20201212_IL_fit_to_Sep_baseline"
fname <- "trajectories_aggregated.csv"

### Trajectories - baseline
trajectoriesDat_baseline <- fread(file.path(sim_dir, exp_name_baseline, fname))
simdat_baseline <- f_simdat(dat = trajectoriesDat_baseline)

### Trajectories - counterfactual
trajectoriesDat_counterfactual_1 <- fread(file.path(sim_dir, exp_name_counterfactual_1, fname))
trajectoriesDat_counterfactual_2 <- fread(file.path(sim_dir, exp_name_counterfactual_2, fname))

vars <- f_mergevars(trajectoriesDat_counterfactual_1,trajectoriesDat_counterfactual_2)

simdat_counterfactual_1 <- f_simdat(
  dat = trajectoriesDat_counterfactual_1 %>% dplyr::select(vars),
  grpVars = c("date", "ems", "scenario_name", "geography_modeled")
) %>% mutate(reopen="50perc")


simdat_counterfactual_2 <- f_simdat(
  dat = trajectoriesDat_counterfactual_2 %>% dplyr::select(vars) ,
  grpVars = c("date", "ems", "scenario_name", "geography_modeled")
) %>% mutate(reopen="100perc")

simdat_counterfactual <- rbind(simdat_counterfactual_1,simdat_counterfactual_2)
rm(simdat_counterfactual_1,simdat_counterfactual_2,trajectoriesDat_counterfactual_1, trajectoriesDat_counterfactual_2 )


### Capacity
capacityDat <- load_new_capacity(filedate = "20200915")
capacityDat <- capacityDat %>%
  filter(geography_name %in% c(1,4,11)) %>%
  pivot_longer(col = -c("geography_name"), names_to = "var") %>%
  mutate(name = ifelse(var == "medsurg_available", 
                       "covid_non_icu", "confirmed_covid_icu"))

capacityDat$region <- factor(capacityDat$geography_name, 
                             levels = c(1,4,11), 
                             labels = paste0("Region ", c(1,4,11)))


# ref_dat <- f_load_ref_data(subregions=c(1, 4, 11),startdate=as.Date("2020-09-01"), stopdate=as.Date("2020-12-31"))
pplot7dAvrSub <- f_load_ref_data(stopdate = stopdate_data)

### ------------------------------
### Plot
### ------------------------------

pplot <- ggplot(data = subset(simdat_baseline, Date <= stopdate)) +
  geom_hline(
    data = subset(capacityDat, name == "confirmed_covid_icu"),
    aes(yintercept = value + 20),
    col = "white", linetype = "dashed", size = 1
  ) +
  geom_ribbon(aes(x = Date, ymin = q2.5.val, ymax = q97.5.val),
    fill = simcolor, alpha = 0.3
  ) +
  geom_ribbon(aes(x = Date, ymin = q25.val, ymax = q75.val),
    fill = simcolor, alpha = 0.5
  ) +
  geom_line(aes(x = Date, y = median.val),
    col = simcolor, size = 1.3
  ) +
  geom_point(
    data = pplot7dAvrSub, aes(x = Date, y = value),
    size = 0.7
  ) +
  geom_line(
    data = pplot7dAvrSub, aes(x = Date, y = value7),
    size = 1
  ) +
  geom_vline(xintercept = c(as.Date("2020-09-01")), col = "#c2390d") +
  geom_hline(
    data = subset(capacityDat, name == "confirmed_covid_icu"),
    aes(yintercept = value),
    col = capacitycolor, linetype = "dashed", size = 1
  ) +
  scale_x_date(
    lim = c(as.Date("2020-03-01"), stopdate),
    date_breaks = "30 days", date_labels = "%b"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1)) +
  facet_wrap(name ~ region, ncol = 3, scales = "free_y", strip.position = "top") +
  customTheme +
  labs(
    x = "",
    color = "",
    y = "ICU census\n(EMR)"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line()
  ) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0, 0))

pplot


pplot1 <- ggplot(data = subset(simdat_baseline, region == "Region 1" &
  name == "confirmed_covid_icu" &
  Date <= stopdate_data)) +
  geom_vline(xintercept = c(as.Date("2020-09-01")), col = "#c2390d") +
  geom_ribbon(aes(x = Date, ymin = q2.5.val, ymax = q97.5.val),
    fill = "#F6921E", alpha = 0.3
  ) +
  geom_ribbon(aes(x = Date, ymin = q25.val, ymax = q75.val),
    fill = "#F6921E", alpha = 0.5
  ) +
  geom_line(aes(x = Date, y = median.val),
    col = "#F6921E", size = 1.3
  ) +
  geom_line(
    data = subset(simdat_baseline, region == "Region 1" &
      name == "confirmed_covid_icu" &
      Date <= as.Date("2020-12-31")),
    aes(x = Date, y = median.val),
    col = "#F6921E", size = 1.3
  ) +
  geom_point(
    data = subset(pplot7dAvrSub, region == "Region 1" &
                    name == "confirmed_covid_icu"),
    aes(x = Date, y = value),
    size = 0.7
  ) +
  geom_line(
    data = subset(pplot7dAvrSub, region == "Region 1" & 
                    name == "confirmed_covid_icu"),
    aes(x = Date, y = value7),
    size = 1
  ) +
  geom_hline(
    data = subset(capacityDat, region == "Region 1" & 
                    name == "confirmed_covid_icu"),
    aes(yintercept = value),
    col = "dodgerblue2", linetype = "dashed", size = 1
  ) +
  scale_x_date(
    lim = c(as.Date("2020-03-01"), as.Date("2020-12-31")),
    date_breaks = "30 days", date_labels = "%b"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1)) +
  facet_wrap(name ~ region, ncol = 3, scales = "free", strip.position = "top") +
  geom_text(x = as.Date("2020-04-01"), y = 123.5, label = "ICU capacity", 
            col = "dodgerblue2", size = 5) +
  customTheme +
  labs(
    x = "",
    color = "",
    y = "ICU census\n(EMR)"
  ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(
    legend.position = "none",
    axis.ticks = element_line()
  ) +
  scale_y_continuous(expand = c(0, 0))

pplot1


### --------------------------------------
### Add reopening
pplot2 <- pplot1 +
  geom_ribbon(
    data = subset(simdat_counterfactual, ems == "EMS-1" & 
                    name == "confirmed_covid_icu" & 
                    Date >= as.Date("2020-09-01")),
    aes(x = Date, ymin = q2.5.val, ymax = q97.5.val,  fill = reopen),
    alpha = 0.3
  ) +
  geom_ribbon(
    data = subset(simdat_counterfactual, ems == "EMS-1" & 
                    name == "confirmed_covid_icu" & 
                    Date >= as.Date("2020-09-01")),
    aes(x = Date, ymin = q25.val, ymax = q75.val,  fill = reopen),
              alpha = 0.4
  ) +
  geom_line(
    data = subset(simdat_counterfactual, ems == "EMS-1" & 
                    name == "confirmed_covid_icu" & 
                    Date >= as.Date("2020-09-01")),
    aes(x = Date, y = median.val, 
        col = as.factor(reopen)), size = 1.3
  ) +
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")
pplot2


### Add reopening - counterfactual
pplot_b <- pplot +
  geom_ribbon(
    data = subset(simdat_counterfactual,  
                    Date >= as.Date("2020-09-01")),
    aes(x = Date, ymin = q2.5.val, ymax = q97.5.val,  fill = reopen),
    alpha = 0.3
  ) +
  geom_ribbon(
    data = subset(simdat_counterfactual,  
                    Date >= as.Date("2020-09-01")),
    aes(x = Date, ymin = q25.val, ymax = q75.val,  fill = reopen),
    alpha = 0.4
  ) +
  geom_line(
    data = subset(simdat_counterfactual,
                    Date >= as.Date("2020-09-01")),
    aes(x = Date, y = median.val, 
        col = as.factor(reopen)), size = 1.3
  ) +
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  scale_y_log10()+
  geom_hline(yintercept = -Inf)
pplot_b


if (stopdate == as.Date("2020-12-31")) {
  pplot2 <- pplot2 +
    geom_point(
      data = subset(pplot7dAvrSub, region == "Region 1" & 
                      name == "confirmed_covid_icu"),
      aes(x = Date, y = value),
      size = 0.7
    ) +
    geom_line(
      data = subset(pplot7dAvrSub, region == "Region 1" & 
                      name == "confirmed_covid_icu"),
      aes(x = Date, y = value7),
      size = 1
    )
}

pplot_combined <- plot_grid(pplot2, pplot, ncol = 1, rel_widths = c(0.7, 1), rel_heights = c(0.7, 1))
pplot_combined


f_save_plot(
  plot_name = paste0("data_comparison_combined_", enddate, "_v3"), pplot = pplot_combined,
  plot_dir = file.path(sim_dir, "ICU_data_comparison_plots"), width = 12, height = 14
)



###----------------------------------------------
### Assess fitting performance
###----------------------------------------------

pplot7dAvrSub$date <- as.Date(pplot7dAvrSub$Date)
simdat_baseline$date <- as.Date(simdat_baseline$Date)

table(pplot7dAvrSub$name)
table(pplot7dAvrSub$name, pplot7dAvrSub$source)
table(simdat_baseline$name)

fitdat <- f_addVar(pplot7dAvrSub, simdat_baseline %>% 
          dplyr::select(-source,-Date)) %>% 
          filter(source=="EMResource" | name=='deaths') %>%
          mutate(firstWave = ifelse(date <= as.Date("2020-07-01"),"1",'0'),
                 month = month(date)) %>% 
          group_by(region, name) %>%
          arrange(date) %>%
          mutate(mae = MAE(value7, median.val),
                 mse = MSE(value7, median.val)) %>%
          group_by(region, name, month) %>%
          mutate(mae_mth = round(MAE(value7, median.val),2),
                 mse_mth = round(MSE(value7, median.val),2)) %>%
          filter(month<=8) 
        
# ggplot(data=fitdat)+
#   geom_point(aes(x=value7, y=median.val,col=firstWave))+
#   geom_smooth(aes(x=value7, y=median.val,col=firstWave), method = "lm")+
#   facet_wrap(region~name, scales="free")

###Describe MAE
fitdat  %>% group_by(region, name) %>% summarize(mae=mean(mae)) %>% pivot_wider(names_from="name", values_from="mae")
fitdat  %>% group_by(region, name, month) %>% summarize(mae_mth=mean(mae_mth)) %>% pivot_wider(names_from="name", values_from="mae_mth")
##MSE
fitdat  %>% group_by(region, name) %>% summarize(mse=mean(mse)) %>% pivot_wider(names_from="name", values_from="mse")
fitdat  %>% group_by(region, name, month) %>% summarize(mse_mth=mean(mse_mth)) %>% pivot_wider(names_from="name", values_from="mse_mth")


for( reg_label in c("Region 1","Region 4","Region 11")){
  
  pplot1 <- list()
  for(var in c("confirmed_covid_icu", "covid_non_icu", "deaths")){
  pplot1[[length(pplot1)+1]]  <- ggplot(data=subset(fitdat, region==reg_label & name ==var))+
    geom_point(aes(x=value7, y=median.val))+
    geom_smooth(aes(x=value7, y=median.val), method = "lm")+
    facet_wrap(~month+mae_mth, scales="free", nrow=1)+
    geom_abline(intercept=0, slope=1)+
    labs(title=var, subtitle="",
         x="reported 7day rolling average (data)",
         y="median prediction (simulation)")+
    customTheme
  
  }
  
  pplot <- plot_grid(pplot1[[1]], pplot1[[2]], pplot1[[3]], ncol=1, labels=reg_label)
  
  f_save_plot(
    plot_name = paste0("fitting_assessment_byMonth_", reg_label), pplot = pplot,
    plot_dir = file.path(sim_dir, "ICU_data_comparison_plots"), width = 12, height = 10
  )
  
  rm(pplot)

}




