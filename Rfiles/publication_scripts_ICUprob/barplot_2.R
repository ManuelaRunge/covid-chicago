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
#simdate <-'20201121'
sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)

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


datAggr <- dat %>% filter(ems %in% c("EMS-1", "EMS-4", "EMS-11")) %>% 
  group_by(ems,region, capacity_multiplier_fct,capacity_multiplier,reopen,icu_available) %>%
  summarize(crit_det_median_mean=mean(crit_det_median),
            crit_det_median_low = min(crit_det_median),
            crit_det_median_high = max(crit_det_median))


pall <- ggplot(data = datAggr) +
  geom_bar(aes(x = capacity_multiplier_fct, y = crit_det_median_mean, group = reopen, fill = reopen),
           stat = "identity", position = position_dodge(width = 0.91), width = 0.7
  ) +
  geom_errorbar(aes(x = capacity_multiplier_fct, ymin = crit_det_median_low, ymax = crit_det_median_high, group = reopen),
                position = position_dodge(width = 0.91), width = 0.1
  ) +
  geom_hline(aes(yintercept = icu_available)) +
  facet_wrap(~region, scales = "free", ncol = 1) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  scale_color_manual(values = c(TwoCols_seq)) +
  customTheme +
  theme_minimal()



datAggr <- dat %>% filter(ems %in% c("EMS-1", "EMS-4", "EMS-11")) %>% 
  group_by(ems,region, capacity_multiplier_fct,capacity_multiplier,icu_available) %>%
  summarize(crit_det_median_mean=mean(crit_det_median),
            crit_det_median_low = min(crit_det_median),
            crit_det_median_high = max(crit_det_median))

pall <- ggplot(data = datAggr) +
  geom_bar(aes(x = capacity_multiplier_fct, y = crit_det_median_mean, group = capacity_multiplier),
           stat = "identity", position = position_dodge(width = 0.91), width = 0.7, fill=TwoCols_seq[1]
  ) +
  geom_errorbar(aes(x = capacity_multiplier_fct, ymin = crit_det_median_low, ymax = crit_det_median_high, group = capacity_multiplier),
                position = position_dodge(width = 0.91), width = 0.1
  ) +
  geom_hline(aes(yintercept = icu_available), col='dodgerblue3', linetype='longdash') +
  facet_wrap(~region, scales = "free", ncol = 1, strip.position = "right") +
  scale_fill_manual(values = c(TwoCols_seq)) +
  scale_color_manual(values = c(TwoCols_seq)) +
  labs(title='',x="Trigger threshold\n(as % of ICU COVID-19 availability)", 
       y="Predicted peak in ICU census") +
  geom_text(aes(y=icu_available, x=0.7),label='ICU capacity', hjust=0, vjust=-0.4, col='dodgerblue3') +
  customTheme +
  theme_minimal()

datAggr <- dat %>% filter(ems %in% c("EMS-1", "EMS-4", "EMS-11")) %>% 
  group_by(ems,region, capacity_multiplier_fct,capacity_multiplier,icu_available) %>%
  summarize(perc_ICU_occup_median_mean=mean(perc_ICU_occup_median),
            perc_ICU_occup_median_low = min(perc_ICU_occup_median),
            perc_ICU_occup_median_high = max(perc_ICU_occup_median),
            crit_det_median_mean=mean(crit_det_median),
            crit_det_median_low = min(crit_det_median),
            crit_det_median_high = max(crit_det_median))

ggplot(data = datAggr) +
  geom_bar(aes(x = capacity_multiplier_fct, y = perc_ICU_occup_median_mean, group = region, fill=region),
           stat = "identity", position = position_dodge(width = 0.91), width = 0.7
  ) +
  geom_errorbar(aes(x = capacity_multiplier_fct, ymin = perc_ICU_occup_median_low, ymax = perc_ICU_occup_median_high, group = region),
                position = position_dodge(width = 0.91), width = 0.1
  ) +
  geom_hline(yintercept = 100, col='dodgerblue3', linetype='longdash') +
  #scale_fill_manual(values = c(TwoCols_seq)) +
 # scale_color_manual(values = c(TwoCols_seq)) +
  labs(title='',x="Trigger threshold\n(as % of ICU COVID-19 availability)", 
       y="Predicted peak in ICU census (%)") +
  geom_text(y=100, x=0.7,label='ICU capacity', hjust=0, vjust=-0.4, col='dodgerblue3') +
  customTheme +
  theme_minimal()


pplot <- ggplot(data = datAggr) +
  geom_bar(aes(x = capacity_multiplier_fct, y = perc_ICU_occup_median_mean),
           stat = "identity", position = position_dodge(width = 0.91), width = 0.7, fill=TwoCols_seq[1]
  ) +
  geom_errorbar(aes(x = capacity_multiplier_fct, ymin = perc_ICU_occup_median_low, ymax = perc_ICU_occup_median_high),
                position = position_dodge(width = 0.91), width = 0.1
  ) +
  geom_hline(yintercept = 100, col='dodgerblue3', linetype='longdash') +
  #scale_fill_manual(values = c(TwoCols_seq)) +
  # scale_color_manual(values = c(TwoCols_seq)) +
  labs(title='',x="Trigger threshold\n(as % of ICU COVID-19 availability)", 
       y="Percent of ICU beds filled\nat predictd peak") +
  facet_wrap(~region, scales = "free", ncol = 1, strip.position = "right") +
  geom_text(y=100, x=0.7,label='ICU capacity', hjust=0, vjust=-0.4, col='dodgerblue3') +
  customTheme +
  theme_minimal()

ggsave(paste0("barplot2_v2.pdf"),
       plot = p2,
       path = file.path(sim_dir, "ICU_bar_plots"), width = 6, height = 8, device = "pdf"
)


#### for text
#View(datAggr)

datAggr %>% as.data.frame() %>% filter(capacity_multiplier_fct=='counterfactual')
datAggr %>% as.data.frame() %>% filter(capacity_multiplier >= 0.7 & capacity_multiplier<0.8 )



### Relative reduction
datAggr <- data.table(datAggr, key =c( "ems" ))
datAggr[,ICU_diff_median := crit_det_median_mean[capacity_multiplier_fct=="counterfactual"] - crit_det_median_mean,by = c( "ems") ]
datAggr[,ICU_diff_lower := crit_det_median_low[capacity_multiplier_fct=="counterfactual"] - crit_det_median_low,by = c( "ems") ]
datAggr[,ICU_diff_upper := crit_det_median_high[capacity_multiplier_fct=="counterfactual"] - crit_det_median_high,by = c( "ems") ]

datAggr[,ICU_ratio_median := (1-( crit_det_median_mean / crit_det_median_mean[capacity_multiplier_fct=="counterfactual"]))*100,by = c( "ems") ]
datAggr[,ICU_ratio_lower  := (1-( crit_det_median_low / crit_det_median_low[capacity_multiplier_fct=="counterfactual"]))*100,by = c( "ems") ]
datAggr[,ICU_ratio_upper := (1-( crit_det_median_high / crit_det_median_high[capacity_multiplier_fct=="counterfactual"]))*100,by = c( "ems") ]


datAggr %>% as.data.frame() %>% filter(capacity_multiplier >= 0.7 & capacity_multiplier<0.8 )

