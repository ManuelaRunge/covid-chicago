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

# dat <- f_combineDat(sim_dir,exp_names, "trigger_peak_exceed_df.csv")
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
ggplot(data = dat) +
  geom_pointrange(aes(x = capacity_multiplier, y = exceed_diff_1_median, ymin = exceed_diff_1_95CI_lower, ymax = exceed_diff_1_95CI_upper)) +
  facet_wrap(~region)

ggplot(data = subset(dat, delay == "7daysdelay")) +
  geom_point(aes(
    x = interaction(reopen, capacity_multiplier_fct),
    y = exceed_diff_1_median, group = interaction(capacity_multiplier_fct, reopen), fill = reopen
  )) +
  facet_wrap(~region) +
  coord_flip() +
  scale_fill_manual(values = c("orange", "dodgerblue")) +
  labs(y = "Days above ICU capacity limit", caption = "Timeframe beyond 2020, until March 2021")


dat_wide <- dat %>%
  dplyr::select(region, reopen, capacity_multiplier_fct, rollback, delay, exceed_diff_1_median) %>%
  pivot_wider(names_from = "delay", values_from = "exceed_diff_1_median")

ggplot(data = dat_wide) +
  geom_errorbar(aes(
    x = interaction(reopen, capacity_multiplier_fct),
    ymin = `0daysdelay`, ymax = `7daysdelay`,
    group = interaction(capacity_multiplier_fct, reopen),
    col = reopen
  ), width = 0) +
  geom_point(data = subset(dat, rollback == "sm4"), aes(
    x = interaction(reopen, capacity_multiplier_fct),
    y = exceed_diff_1_median,
    group = interaction(capacity_multiplier_fct, reopen),
    col = reopen
  ), width = 0) +
  facet_wrap(~region) +
  coord_flip() +
  scale_fill_manual(values = c("orange", "dodgerblue")) +
  labs(y = "Days above ICU capacity limit", caption = "Timeframe beyond 2020, until March 2021")




pplot <- ggplot(data = subset(dat, delay == "7daysdelay")) +
  geom_boxplot(aes(
    x = interaction(reopen, round(capacity_multiplier * 100, 1)),
    y = exceed_diff_1_median, group = interaction(capacity_multiplier, reopen), fill = reopen
  )) +
  facet_wrap(~region) +
  coord_flip() +
  scale_fill_manual(values = c("orange", "dodgerblue")) +
  labs(y = "Days above ICU capacity limit", caption = "Timeframe beyond 2020, until March 2021")

ggsave(paste0("boxplot_duration_exceed.pdf"),
  plot = pplot,
  path = file.path(sim_dir, "ICU_duration_plots"), width = 10, height = 6, device = "pdf"
)


##########

datAggr <- dat %>%
  group_by(region, capacity_multiplier_fct, capacity_multiplier, reopen) %>%
  summarize(
    exceed_diff_1_mean = mean(exceed_diff_1_median),
    exceed_diff_1_low = min(exceed_diff_1_median),
    exceed_diff_1_high = max(exceed_diff_1_median)
  )

pplot <- ggplot(data = datAggr) +
  geom_point(data = dat, aes(
    x = interaction(reopen, capacity_multiplier_fct),
    y = exceed_diff_1, group = interaction(capacity_multiplier, reopen), fill = reopen
  ), shape = 21) +
  geom_pointrange(aes(
    x = interaction(reopen, capacity_multiplier_fct),
    y = exceed_diff_1_mean, ymin = exceed_diff_1_low, ymax = exceed_diff_1_high,
    group = interaction(capacity_multiplier, reopen), fill = reopen
  ), shape = 21) +
  facet_wrap(~region) +
  coord_flip() +
  scale_fill_manual(values = c("orange", "dodgerblue")) +
  labs(y = "Days above ICU capacity limit", caption = "Timeframe beyond 2020, until March 2021")


pplot <- ggplot() +
  geom_boxplot(
    data = subset(dat, delay == "0daysdelay"),
    aes(
      x = interaction(reopen, round(capacity_multiplier * 100, 1)),
      y = exceed_diff_1_median, group = interaction(capacity_multiplier, reopen), fill = reopen
    )
  ) +
  facet_wrap(~region) +
  coord_flip() +
  scale_fill_manual(values = c("orange", "dodgerblue")) +
  labs(y = "Days above ICU capacity limit", caption = "Timeframe beyond 2020, until March 2021")


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
