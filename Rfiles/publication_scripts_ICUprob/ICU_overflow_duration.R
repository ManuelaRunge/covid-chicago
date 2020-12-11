library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

## -------------------------------
## Run script
## -------------------------------

simdate <- "20200919"
# simdate <-'20201121'
sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (!dir.exists(file.path(sim_dir, "ICU_duration_plots"))) dir.create(file.path(sim_dir, "ICU_duration_plots"))


exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c(grep("daysdelay", exp_names), grep("counterfactual", exp_names))]

dat <- f_combineDat(sim_dir, exp_names, "peak_exceed_df.csv")

subregions <- c("covidregion_1", "covidregion_4", "covidregion_11")
dat <- dat %>% filter(geography_modeled %in% subregions)
dat$scen_name <- gsub(paste0(simdate, "_IL_regreopen"), "", dat$exp_name)
dat <- dat %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat$rollback[is.na(dat$rollback)] <- "counterfactual"

dat$capacity_multiplier_fct <- round(dat$capacity_multiplier * 100, 0)
fct_labels <- sort(unique(dat$capacity_multiplier_fct))
dat$capacity_multiplier_fct[dat$rollback == "counterfactual"] <- "counterfactual"
dat$capacity_multiplier_fct <- factor(dat$capacity_multiplier_fct,
  levels = c(fct_labels, "counterfactual"),
  labels = c(fct_labels, "counterfactual")
)
table(dat$capacity_multiplier_fct, exclude = NULL)


### for how long exceed capacity
pplot <- ggplot() +
  geom_bar(
    data = subset(dat, delay == "0daysdelay" & rollback == "sm4"),
    aes(
      x = interaction(delay, round(capacity_multiplier * 100, 1)),
      y = exceed_diff_1_median, group = interaction(capacity_multiplier, reopen),
      fill = reopen
    ), stat = "identity", position = position_dodge(width = 1)
  ) +
  geom_bar(
    data = subset(dat, delay == "0daysdelay" & rollback != "sm4"),
    aes(
      x = interaction(delay, round(capacity_multiplier * 100, 1)),
      y = exceed_diff_1_median, group = interaction(capacity_multiplier, reopen),
      fill = reopen
    ), alpha = 0.4, stat = "identity", position = position_dodge(width = 1)
  ) +
  geom_hline(data = subset(dat, delay == "counterfactual"), aes(yintercept = exceed_diff_1_median, col = reopen)) +
  facet_wrap(~region) +
  coord_flip() +
  scale_fill_manual(values = c("orange", "dodgerblue", "grey")) +
  scale_color_manual(values = c("orange", "dodgerblue", "grey")) +
  labs(y = "Days above ICU capacity limit", caption = "Timeframe beyond 2020, until March 2021")

ggsave(paste0("barplot_duration_exceed.pdf"),
  plot = pplot,
  path = file.path(sim_dir, "ICU_duration_plots"), width = 10, height = 6, device = "pdf"
)

### for text
subset(dat, delay == "0daysdelay") %>%
  filter(!is.na(exceed_diff_1_median)) %>%
  dplyr::select(region, reopen, rollback, capacity_multiplier_fct, exceed_diff_1_median) %>%
  unique() %>%
  as.data.frame() %>%
  arrange(region, reopen, rollback, capacity_multiplier_fct)

subset(dat, delay == "counterfactual") %>%
  filter(!is.na(exceed_diff_1_median)) %>%
  dplyr::select(region, reopen, rollback, capacity_multiplier_fct, exceed_diff_1_median) %>%
  unique() %>%
  as.data.frame() %>%
  arrange(region, reopen, rollback, capacity_multiplier_fct)



###########
dat <- f_combineDat(sim_dir, exp_names, "trigger_peak_exceed_df.csv")

subregions <- c("covidregion_1", "covidregion_4", "covidregion_11")
dat <- dat %>% filter(geography_modeled %in% subregions)
dat$scen_name <- gsub(paste0(simdate, "_IL_regreopen"), "", dat$exp_name)
dat <- dat %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat$rollback[is.na(dat$rollback)] <- "counterfactual"

dat$capacity_multiplier_fct <- round(dat$capacity_multiplier * 100, 0)
fct_labels <- sort(unique(dat$capacity_multiplier_fct))
dat$capacity_multiplier_fct[dat$rollback == "counterfactual"] <- "counterfactual"
dat$capacity_multiplier_fct <- factor(dat$capacity_multiplier_fct,
  levels = c(fct_labels, "counterfactual"),
  labels = c(fct_labels, "counterfactual")
)
table(dat$capacity_multiplier_fct, exclude = NULL)


pplot <- ggplot() +
  geom_bar(
    data = subset(dat, delay == "0daysdelay" & rollback == "sm4"),
    aes(
      x = interaction(delay, round(capacity_multiplier * 100, 1)),
      y = trigger_to_exceed_0.7_median, group = interaction(capacity_multiplier, rollback, reopen),
      fill = reopen
    ), stat = "identity", position = position_dodge(width = 1)
  ) +
  geom_bar(data = unique(subset(dat, delay == "0daysdelay" & rollback != "sm4")), aes(
    x = interaction(delay, round(capacity_multiplier * 100, 1)),
    y = trigger_to_exceed_0.7_median, group = interaction(capacity_multiplier, rollback, reopen),
    fill = reopen
  ), alpha = 0.3, stat = "identity", position = position_dodge(width = 1)) +
  # geom_hline(data=subset(dat,  delay=="counterfactual" ), aes(yintercept=trigger_to_exceed_0.7_median,col=reopen))+
  facet_wrap(~region) +
  coord_flip() +
  scale_fill_manual(values = c("orange", "dodgerblue", "grey")) +
  scale_color_manual(values = c("orange", "dodgerblue", "grey")) +
  labs(y = "Days above ICU capacity limit", caption = "Timeframe beyond 2020, until March 2021")


ggsave(paste0("barplot_duration_trigger_to_70perc.pdf"),
  plot = pplot,
  path = file.path(sim_dir, "ICU_duration_plots"), width = 12, height = 12, device = "pdf"
)



###### time between recommended and currently used trigger
