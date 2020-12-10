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

simdate <-'20200919'
simdate <-'20201121'
sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)
if(!dir.exists(file.path(sim_dir, "ICU_bar_plots")))dir.create(file.path(sim_dir, "ICU_bar_plots"))


exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen",exp_names)]
exp_names <- exp_names[c(grep("daysdelay",exp_names),grep("counterfactual",exp_names))]

dat <- f_combineDat(sim_dir,exp_names, "ICU_peak.csv")

dat$scen_name <- gsub(paste0(simdate,"_IL_regreopen"), "", dat$exp_name)
dat <- dat %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
dat$rollback[is.na(dat$rollback)] <- "counterfactual"
rollback_val <- unique(dat$rollback)
delay_val <- unique(dat$delay)


dat$capacity_multiplier_fct <- round(dat$capacity_multiplier * 100, 0)
fct_labels <-  sort(unique(dat$capacity_multiplier_fct))
dat$capacity_multiplier_fct[dat$rollback == "counterfactual"] <- "counterfactual"
dat$capacity_multiplier_fct <- factor(dat$capacity_multiplier_fct,
  levels = c(fct_labels,"counterfactual"),
  labels = c(fct_labels,"counterfactual")
)
table(dat$capacity_multiplier_fct, exclude = NULL)

dat$region <- factor(dat$ems, levels = c(paste0("EMS-", c(1:11)), "All"), labels = c(c(1:11), "All"))

palldat_best <- dat %>%
  filter(ems %in% c("EMS-1", "EMS-4", "EMS-11") &
    (delay == "counterfactual" |
      delay == delay_val[1] & rollback == rollback_val[1]))

palldat_worst <- dat %>%
  filter(ems %in% c("EMS-1", "EMS-4", "EMS-11") &
           (delay == "counterfactual" |
              delay == delay_val[length(delay_val)] & rollback ==  rollback_val[length(rollback_val)]))

p1dat <- dat %>%
  filter(ems %in% c("EMS-1", "EMS-4", "EMS-11") &
           (delay != "counterfactual" &
              delay ==  delay_val[1] &
              rollback ==  rollback_val[1])) %>%
  as.data.frame()

p2dat <- subset(dat, ems %in% c("EMS-1", "EMS-4", "EMS-11") & (delay == "counterfactual")) %>% as.data.frame()


pall <- ggplot(data = palldat_best) +
  geom_bar(data=palldat_worst, aes(x = capacity_multiplier_fct, y = crit_det_median, group = interaction(delay, rollback, reopen), col = reopen),
           stat = "identity", position = position_dodge(width = 0.91), width = 0.7, fill=NA
  ) +
  geom_bar(aes(x = capacity_multiplier_fct, y = crit_det_median, group = interaction(delay, rollback, reopen), fill = reopen),
    stat = "identity", position = position_dodge(width = 0.91), width = 0.7
  ) +
  geom_errorbar(aes(x = capacity_multiplier_fct, ymin = crit_det_95CI_lower, ymax = crit_det_95CI_upper, group = interaction(delay, rollback, reopen)),
    position = position_dodge(width = 0.91), width = 0.1
  ) +
  geom_hline(aes(yintercept = icu_available)) +
  facet_wrap(~region, scales = "free", ncol = 1) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  scale_color_manual(values = c(TwoCols_seq)) +
  customTheme +
  theme_minimal()


p1 <- ggplot(data = p1dat) +
  geom_bar(aes(x = capacity_multiplier_fct, y = crit_det_median, group = interaction(delay, rollback, reopen), fill = reopen),
    stat = "identity", position = position_dodge(width = 0.91), width = 0.7
  ) +
  geom_errorbar(aes(x = capacity_multiplier_fct, ymin = crit_det_95CI_lower, ymax = crit_det_95CI_upper, group = interaction(delay, rollback, reopen)),
    position = position_dodge(width = 0.91), width = 0.1
  ) +
  geom_hline(aes(yintercept = icu_available)) +
  facet_wrap(~region, scales = "free", ncol = 1) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  customTheme +
  theme_minimal()


p2 <- ggplot(data = p2dat) +
  geom_bar(aes(x = capacity_multiplier_fct, y = crit_det_median, group = interaction(delay, rollback, reopen), fill = reopen),
    stat = "identity", position = position_dodge(width = 0.91), width = 0.7
  ) +
  geom_errorbar(aes(x = capacity_multiplier_fct, ymin = crit_det_95CI_lower, ymax = crit_det_95CI_upper, group = interaction(delay, rollback, reopen)),
    position = position_dodge(width = 0.91), width = 0.1
  ) +
  geom_hline(aes(yintercept = icu_available)) +
  facet_wrap(~region, scales = "free", ncol = 1) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  customTheme +
  theme_minimal()


ggsave(paste0("barplot_pall.pdf"),
  plot = pall,
  path = file.path(sim_dir, "ICU_bar_plots"), width = 6, height = 8, device = "pdf"
)
ggsave(paste0("barplot_p1.pdf"),
  plot = p1,
  path = file.path(sim_dir, "ICU_bar_plots"), width = 6, height = 8, device = "pdf"
)
ggsave(paste0("barplot_p2.pdf"),
  plot = p2,
  path = file.path(sim_dir, "ICU_bar_plots"), width = 6, height = 8, device = "pdf"
)


###### Text to describe figures


p2dat %>%
  group_by(ems, reopen, exp_name) %>%
  mutate(ratio = crit_det_median / icu_available) %>%
  as.data.frame() %>%
  group_by(reopen) %>%
  summarize(ratio = mean(ratio))
p2dat %>%
  group_by(ems, reopen, exp_name) %>%
  mutate(ratio = crit_det_95CI_lower / icu_available) %>%
  as.data.frame() %>%
  group_by(reopen) %>%
  summarize(ratio = mean(ratio))
p2dat %>%
  group_by(ems, reopen, exp_name) %>%
  mutate(ratio = crit_det_95CI_upper / icu_available) %>%
  as.data.frame() %>%
  group_by(reopen) %>%
  summarize(ratio = mean(ratio))

p2dat %>%
  group_by(ems, reopen, exp_name) %>%
  mutate(ratio = crit_det_median / icu_available) %>%
  as.data.frame() %>%
  group_by(ems, reopen, ) %>%
  summarize(ratio = mean(ratio))
p2dat %>%
  group_by(ems, reopen, exp_name) %>%
  mutate(ratio = crit_det_95CI_lower / icu_available) %>%
  as.data.frame() %>%
  group_by(ems, reopen, ) %>%
  summarize(ratio = mean(ratio))
p2dat %>%
  group_by(ems, reopen, exp_name) %>%
  mutate(ratio = crit_det_95CI_upper / icu_available) %>%
  as.data.frame() %>%
  group_by(ems, reopen, ) %>%
  summarize(ratio = mean(ratio))



### Relative reduction
#p1dat
colnames(p2dat)[c(4:8)] <- paste0("counter_",colnames(p2dat)[c(4:8)])

p1dat %>%
  left_join(p2dat[,c(1,4:8,14)], by=c("ems",'reopen')) %>%
  group_by(ems, exp_name,capacity_multiplier)%>%
  mutate(perc_red_median = (1-(crit_det_median / counter_crit_det_median))*100) %>%
  dplyr::select(ems, exp_name,delay, reopen, capacity_multiplier,crit_det_median,counter_crit_det_median,  perc_red_median) %>% as.data.frame()%>%
  arrange(capacity_multiplier,ems,reopen )
  
#palldat <- data.table(palldat, key =c( "geography_name","capacity_multiplier", "date",'reopen' ))
#palldat[,ICU_diff_median := crit_det_median[rollback=="sm4"] - crit_det_median[rollback=="counterfactual"], by = c( "geography_name","capacity_multiplier", "date",'reopen' ) ]



