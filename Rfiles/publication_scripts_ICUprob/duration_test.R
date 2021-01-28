library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

TwoCols_seq <- c("#00a79d", "#f7941d")
capacity_col <- "#2a3b90"
customTheme <- f_getCustomTheme()
theme_set(theme_minimal())
## -------------------------------
## Run script
## -------------------------------
capacityDat <- load_new_capacity(filedate = "20200915")
capacityDat <- capacityDat %>%
  filter(geography_name %in% c(1, 4, 11)) %>%
  rename(avg_resource_available = icu_available)
capacityDat$region <- factor(capacityDat$geography_name, levels = c(1, 4, 11), labels = paste0("Region ", c(1, 4, 11)))


simdate <- "20201212"
sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (!dir.exists(file.path(sim_dir, "ICU_trajectories_plots"))) dir.create(file.path(sim_dir, "ICU_trajectories_plots"))
if (!dir.exists(file.path(sim_dir, "ICU_trajectories_plots", "pdf"))) dir.create(file.path(sim_dir, "ICU_trajectories_plots", "pdf"))

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c(grep("daysdelay", exp_names), grep("counterfactual", exp_names))]
exp_names <- exp_names[!(grepl("_reopen", exp_names))]

# save(plotdat, file = file.path(sim_dir, exp_name, paste0("trajectories_extract_",reg_nr,".Rdata")))
# save(plotdat_time, file = file.path(sim_dir, exp_name, paste0("trajectories_extract_time_",reg_nr,".Rdata")))
# save(plotdat_peak, file = file.path(sim_dir, exp_name, paste0("trajectories_extract_peak_",reg_nr,".Rdata")))
exp_name1 <- "20201212_IL_regreopen100perc_1daysdelay_pr6"
exp_name2 <- "20201212_IL_regreopen50perc_1daysdelay_pr6"

### Combine dataframes

plotdatList <- list()
for (exp_name in exp_names) {
  for (reg_nr in c(1, 4, 11)) {
    if (!file.exists(file.path(sim_dir, exp_name, paste0("trajectories_extract_", reg_nr, ".Rdata")))) next
    load(file.path(sim_dir, exp_name, paste0("trajectories_extract_", reg_nr, ".Rdata")))
    plotdatList[[length(plotdatList) + 1]] <- plotdat %>%
      filter(crit_det >= avg_resource_available) %>%
      mutate(exceed_diff = as.numeric(exceed_diff)) %>%
      filter(!is.na(exceed_diff))
    rm(plotdat)
  }
}

dat <- plotdatList %>% bind_rows()
table(dat$exp_name)
table(dat$region)


plotdatAggr_1 <- dat %>% 
  select(exp_name, region, capacity_multiplier, sample_num, scen_num, exceed_date_from, exceed_date_to, exceed_diff)  %>% 
  unique() %>%
  sample_n(100, replace=TRUE) %>%
  unique()


nsize=70
datAggr <- plotdatAggr_1 %>%
  ungroup() %>%
  group_by(region, exp_name, capacity_multiplier) %>%
  summarize(
    nsamples = n_distinct(sample_num),
    min.value = min(exceed_diff, na.rm = TRUE),
    max.value = max(exceed_diff, na.rm = TRUE),
    median.value = median(exceed_diff, na.rm = TRUE),
    q25.value = quantile(exceed_diff, probs = 0.25, na.rm = TRUE),
    q75.value = quantile(exceed_diff, probs = 0.75, na.rm = TRUE),
    q2.5.value = quantile(exceed_diff, probs = 0.025, na.rm = TRUE),
    q97.5.value = quantile(exceed_diff, probs = 0.975, na.rm = TRUE)
  ) %>%
  f_get_scenVars() %>%
  mutate(
    min.value = ifelse(nsamples > nsize, min.value, 0),
    max.value = ifelse(nsamples > nsize, max.value, 0),
    median.value = ifelse(nsamples > nsize, median.value, 0),
    q25.value = ifelse(nsamples > nsize, q25.value, 0),
    q75.value = ifelse(nsamples > nsize, q75.value, 0),
    q2.5.value = ifelse(nsamples > nsize, q2.5.value, 0),
    q97.5.value = ifelse(nsamples > nsize, q97.5.value, 0)
  )

summary(datAggr$nsamples)


pplot_top <- ggplot(data = subset(datAggr, delay=="1daysdelay")) +
  #geom_jitter(data=dat,aes(x = capacity_multiplier, y = exceed_diff, col = reopen), alpha = 0.3, width = 0.025, height = 0) +
  geom_line(
    aes(
      x = capacity_multiplier,
      y = median.value, 
      alpha=rollback,
      col = reopen
    ), size = 1
  ) +
  geom_point(
    aes(
      x = capacity_multiplier,
      y = median.value,
      alpha=rollback,
      fill = reopen
    )
  ) +
  # geom_errorbar(
  #   data = subset(datAggr),
  #   aes(
  #     x = capacity_multiplier,
  #     y = median.value, ymin = q25.value, ymax = q75.value,
  #     group = interaction(reopen, rollback, capacity_multiplier)
  #   ), width = 0
  # ) +
  facet_wrap(~region)+
  scale_color_manual(values=TwoCols_seq)+
  scale_fill_manual(values=TwoCols_seq)+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,1,0.2)*100)+
  customTheme+
  labs(x="Trigger threshold\n(% of ICU capacity)",
       y="Days",
       color="Transmission\nincrease",
       fill="Transmission\nincrease",
       alpha="Mitigation\nstrengths")

pplot_top


pplot_bottom <- ggplot(data = subset(datAggr, rollback=="pr8")) +
  #geom_jitter(data=dat,aes(x = capacity_multiplier, y = exceed_diff, col = reopen), alpha = 0.3, width = 0.025, height = 0) +
  geom_line(
    aes(
      x = capacity_multiplier,
      y = median.value, 
      alpha=delay,
      col = reopen
    ), size = 1
  ) +
  geom_point(
    aes(
      x = capacity_multiplier,
      y = median.value,
      alpha=delay,
      fill = reopen
    )
  ) +
  # geom_errorbar(
  #   data = subset(datAggr),
  #   aes(
  #     x = capacity_multiplier,
  #     y = median.value, ymin = q25.value, ymax = q75.value,
  #     group = interaction(reopen, rollback, capacity_multiplier)
  #   ), width = 0
  # ) +
  facet_wrap(~region)+
  scale_color_manual(values=TwoCols_seq)+
  scale_fill_manual(values=TwoCols_seq)+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,1,0.2)*100)+
  customTheme+
  labs(x="Trigger threshold\n(% of ICU capacity)",
       y="Days",
       color="Transmission\nincrease",
       fill="Transmission\nincrease",
       alpha="Mitigation\ndelay")


pplot <- plot_grid(pplot_top, pplot_bottom, ncol=1)
pplot

f_save_plot(plot_name=paste0("trajectories_duration_by_capacity_if_exceed"), pplot = pplot, 
            plot_dir = file.path(sim_dir, "ICU_duration_plots"), width = 12, height = 8)




pplot_hist <- ggplot(data=subset(datAggr, delay=="1daysdelay" & rollback=="pr8"))+
  geom_bar(aes(x=capacity_multiplier, y=nsamples, fill=reopen, group=reopen),stat="identity", position = position_dodge(width=0.1))+
  facet_wrap(~region)+
  scale_color_manual(values=TwoCols_seq)+
  scale_fill_manual(values=TwoCols_seq)

plot_grid(pplot_hist, pplot, ncol=1, rel_heights = c(0.2,1))





##############

datAggr_delay <- datAggr %>% ungroup() %>%
  dplyr::select(region, reopen,capacity_multiplier,  rollback, delay,median.value) %>%
  filter(!is.na(median.value) & delay!="counterfactual") %>%
  pivot_wider(names_from=delay, values_from=median.value) %>%
  mutate(delay_diff = `7daysdelay`- `1daysdelay`) 


ggplot(data = subset(datAggr_delay , rollback=="pr8")) +
  #geom_jitter(data=dat,aes(x = capacity_multiplier, y = exceed_diff, col = reopen), alpha = 0.3, width = 0.025, height = 0) +
  geom_line(
    aes(
      x = capacity_multiplier,
      y = delay_diff, 
      col = reopen
    ), size = 1
  ) +
  geom_point(
    aes(
      x = capacity_multiplier,
      y = delay_diff,
      fill = reopen
    )
  ) +
  facet_wrap(~region)+
  scale_color_manual(values=TwoCols_seq)+
  scale_fill_manual(values=TwoCols_seq)+
  customTheme+
  labs(x="Trigger threshold\n(% of ICU capacity)",
       y="Days",
       color="Transmission\nincrease",
       fill="Transmission\nincrease",
       alpha="Mitigation\ndelay")


datAggr_delay <- datAggr_delay %>%
  filter(delay_diff!=0) %>%
  group_by(region, reopen,  rollback) %>%
  summarize(delay_diff=mean(delay_diff))

summary(datAggr_delay$delay_diff)

ggplot(data = subset(datAggr_delay)) +
  geom_bar(
    aes(
      x = rollback,
      y = delay_diff,
      fill = reopen
    ),stat="identity", position = position_dodge(width=0.1)
  ) +
  facet_wrap(~region)+
  scale_color_manual(values=TwoCols_seq)+
  scale_fill_manual(values=TwoCols_seq)+
  customTheme+
  labs(x="Trigger threshold\n(% of ICU capacity)",
       y="Days",
       color="Transmission\nincrease",
       fill="Transmission\nincrease",
       alpha="Mitigation\ndelay")




datAggr_delay <- datAggr %>% 
                  group_by(region, reopen, rollback, delay) %>%
                  filter(!is.na(median.value) & delay!="counterfactual") %>%
                  summarize(median.value=mean(median.value, na.rm=TRUE)) %>%
                  pivot_wider(names_from=delay, values_from=median.value) %>%
                  mutate(delay_diff = `7daysdelay`- `1daysdelay`)


summary(datAggr_delay$delay_diff)


  
  
ggplot(data = subset(datAggr, delay=="1daysdelay")) +
  geom_raster(aes(x=capacity_multiplier, y=rollback, fill=median.value)) +
  facet_wrap(~region)+
  customTheme+
  scale_fill_viridis_c()+
  labs(x="Trigger threshold\n(% of ICU capacity)",
       y="Days",
       color="Duration",
       fill="Duration")


