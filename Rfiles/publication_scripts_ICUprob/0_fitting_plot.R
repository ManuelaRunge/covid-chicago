

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

###------------------------------
### Run script 
###------------------------------
subregions = c(1,4,11)
stopdate=  as.Date("2020-12-31") #as.Date("2020-09-01") #
stopdate_data=  as.Date("2020-09-01") #as.Date("2020-09-01") #
enddate = as.character(stopdate)

simdate <- "20201212"
sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (!dir.exists(file.path(sim_dir, "ICU_data_comparison_plots"))) dir.create(file.path(sim_dir, "ICU_data_comparison_plots"))
if (!dir.exists(file.path(sim_dir, "ICU_data_comparison_plots","pdf"))) dir.create(file.path(sim_dir, "ICU_data_comparison_plots","pdf"))

exp_name_gradualReoepen <- "20201120_IL_mr_gradual_reopening2_Sep"
exp_name_baseline <- "20201212_IL_fit_to_Sep_baseline"
fname <- "trajectories_aggregated.csv"

trajectoriesDat_reopen <- fread(file.path(sim_dir, exp_name_gradualReoepen, fname))
simdat_reopen <- f_simdat(dat=trajectoriesDat_reopen,
                          grpVars = c("date", "ems", "scenario_name", "geography_modeled","reopening_multiplier_4"))

trajectoriesDat_baseline <- fread(file.path(sim_dir, exp_name_baseline, fname))
simdat_baseline <- f_simdat(dat=trajectoriesDat_baseline)

capacityDat <- load_new_capacity(filedate = "20200915")
capacityDat <- capacityDat %>%
  filter(geography_name %in% subregions) %>%
  pivot_longer(col = -c("geography_name"), names_to = "var") %>%
  mutate(name = ifelse(var == "medsurg_available", "covid_non_icu", "confirmed_covid_icu"))
capacityDat$region <- factor(capacityDat$geography_name, levels = subregions, labels = paste0("Region ",subregions))


#ref_dat <- f_load_ref_data(subregions=c(1, 4, 11),startdate=as.Date("2020-09-01"), stopdate=as.Date("2020-12-31"))
pplot7dAvrSub <- f_load_ref_data(stopdate=stopdate_data)

###------------------------------
### Plot 
###------------------------------

pplot <- ggplot(data = subset(simdat_baseline, Date <= stopdate)) +
  geom_hline(
    data = subset(capacityDat, name == "confirmed_covid_icu"),
    aes(yintercept = value+20),
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
  scale_x_date(lim=c(as.Date("2020-03-01"),stopdate),
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
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line()) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0, 0))

pplot


pplot1 <- ggplot(data = subset(simdat_baseline, region == "Region 1" &
  name == "confirmed_covid_icu" &
  Date <= stopdate_data)) +
  geom_rect(
    xmin = as.Date("2020-09-01"), xmax = as.Date("2020-10-01"),
    ymin = -Inf, ymax = Inf, fill = "#c2390d", alpha = 0.002
  ) +
  geom_rect(
    xmin = as.Date("2020-10-01"), xmax = as.Date("2020-12-31"),
    ymin = -Inf, ymax = Inf, fill = "#ef4137", alpha = 0.002
  ) +
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
    data = subset(pplot7dAvrSub, region == "Region 1" & name == "confirmed_covid_icu"),
    aes(x = Date, y = value),
    size = 0.7
  ) +
  geom_line(
    data = subset(pplot7dAvrSub, region == "Region 1" & name == "confirmed_covid_icu"),
    aes(x = Date, y = value7),
    size = 1
  ) +
  geom_hline(
    data = subset(capacityDat, region == "Region 1" & name == "confirmed_covid_icu"),
    aes(yintercept = value),
    col = "dodgerblue2", linetype = "dashed", size = 1
  ) +
  scale_x_date(
    lim = c(as.Date("2020-03-01"), as.Date("2020-12-31")),
    date_breaks = "30 days", date_labels = "%b"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1)) +
  facet_wrap(name ~ region, ncol = 3, scales = "free", strip.position = "top") +
  geom_text(x = as.Date("2020-04-01"), y = 123.5, label = "ICU capacity", col = "dodgerblue2", size = 5) +
  customTheme +
  labs(
    x = "",
    color = "",
    y = "ICU census\n(EMR)"
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.position = "none",
    axis.ticks = element_line()) +
  scale_y_continuous(expand = c(0, 0))

pplot1


### --------------------------------------
### Add reopening
pplot2 <- pplot1 +
  geom_line(
    data = subset(simdat_reopen,  ems=="EMS-1" & name == "confirmed_covid_icu" & Date >= as.Date("2020-09-01")),
    aes(x = Date, y = median.val, col = as.factor(reopening_multiplier_4)), size = 1.3
  )+
  scale_color_brewer(palette="Dark2")
pplot2


### Add reopening
pplot_b <- pplot +
  geom_line(
    data = subset(simdat_reopen, Date >= as.Date("2020-09-01") & reopening_multiplier_4 <=0.1),
    aes(x = Date, y = median.val, col = as.factor(reopening_multiplier_4)), size = 1.3
  )+
  scale_color_brewer(palette="Dark2")
pplot_b

if(stopdate==as.Date("2020-12-31")){
  pplot2<- pplot2+
    geom_point(
      data = subset(pplot7dAvrSub, region == "Region 1" & name == "confirmed_covid_icu"),
      aes(x = Date, y = value),
      size = 0.7
    ) +
    geom_line(
      data = subset(pplot7dAvrSub, region == "Region 1" & name == "confirmed_covid_icu"),
      aes(x = Date, y = value7),
      size = 1
    ) 
}

pplot_combined <- plot_grid(pplot2, pplot, ncol = 1, rel_widths = c(0.7, 1), rel_heights = c(0.7, 1))
pplot_combined



ggsave(paste0("data_comparison_combined_",enddate,"_v2.pdf"),
       plot = pplot_combined, path = file.path(sim_dir, "ICU_data_comparison_plots","pdf"), width = 12, height = 14, device = "pdf"
)
ggsave(paste0("data_comparison_combined_",enddate,"_v2.png"),
       plot = pplot_combined, path = file.path(sim_dir, "ICU_data_comparison_plots"), width = 12, height = 14, device = "png"
)


