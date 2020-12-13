library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

TwoCols_seq <- c("#00a79d", "#f7941d")

## -------------------------------
## Run script
## -------------------------------

#simdate <-'20200919'
#simdate <-'20201121'
simdate <-'20201212'
sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)
if(!dir.exists(file.path(sim_dir, "ICU_bar_plots")))dir.create(file.path(sim_dir, "ICU_bar_plots"))


exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen",exp_names)]
exp_names <- exp_names[c(grep("daysdelay",exp_names),grep("counterfactual",exp_names))]
exp_names <- exp_names[!(grepl("_reopen",exp_names))]

dat <- f_combineDat(sim_dir,exp_names, "peak_exceed_df.csv")

dat$scen_name <- gsub(paste0(simdate,"_IL_regreopen"), "", dat$exp_name)
dat$scen_name <- gsub("20201210_IL_regreopen", "", dat$scen_name)
dat <- dat %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
table(dat$rollback, dat$exp_name)
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
dat$capacity_multiplier_fct

#dat$perc_ICU_occup_50CI_lower = (dat$critical_50CI_lower/  dat$avg_resource_available)*100
#dat$perc_ICU_occup_50CI_upper = (dat$critical_50CI_upper/  dat$avg_resource_available)*100
dat$perc_ICU_occup_95CI_lower = (dat$critical_95CI_lower/  dat$avg_resource_available)*100
dat$perc_ICU_occup_95CI_upper = (dat$critical_95CI_upper/  dat$avg_resource_available)*100
dat$perc_ICU_occup_median = (dat$critical_median/  dat$avg_resource_available)*100


dat$region <- factor(dat$ems, levels = c(paste0("EMS-", c(1:11)), "All"), labels = c(c(1:11), "All"))


palldat_best <- dat %>%
  filter(ems %in% c("EMS-1", "EMS-4", "EMS-11") &
    (delay == "counterfactual" |
      delay == delay_val[1] & rollback == rollback_val[2]))

palldat_worst <- dat %>%
  filter(ems %in% c("EMS-1", "EMS-4", "EMS-11") &
           (delay == "counterfactual" |
              delay == delay_val[length(delay_val)] & rollback ==  rollback_val[length(rollback_val)]))

p1dat <- dat %>%
  filter(ems %in% c("EMS-1", "EMS-4", "EMS-11") &
           (delay != "counterfactual" &
              delay ==  delay_val[1] &
              rollback ==  rollback_val[2])) %>%
  as.data.frame()


p1bdat <- dat %>%
  filter(ems %in% c("EMS-1", "EMS-4", "EMS-11") &
           (delay != "counterfactual" &
              delay ==  delay_val[1] &
              rollback ==  rollback_val[2])) %>%
  as.data.frame()

p2dat <- subset(dat, ems %in% c("EMS-1", "EMS-4", "EMS-11") & (delay == "counterfactual")) %>% as.data.frame()




p1 <- ggplot(data = p1dat) +
  geom_bar(aes(x = capacity_multiplier_fct, y = perc_ICU_occup_median, group = interaction(delay, rollback, reopen), fill = reopen),
    stat = "identity", position = position_dodge(width = 0.91), width = 0.7
  ) +
  geom_bar(data = p1bdat,aes(x = capacity_multiplier_fct, y = perc_ICU_occup_median, group = interaction(delay, rollback, reopen), fill = reopen),
           stat = "identity", position = position_dodge(width = 0.91), width = 0.7, alpha=0.4
  ) +
  geom_errorbar(aes(x = capacity_multiplier_fct, ymin = perc_ICU_occup_95CI_lower, ymax = perc_ICU_occup_95CI_upper, 
                    group = interaction(delay, rollback, reopen)),
    position = position_dodge(width = 0.91), width = 0.1
  ) +
  labs(y="% of ICU capacity at predicted peak\nuntil end of December", x="Trigger treshold\n(as % of ICU capacity at which mitigation is triggered )")+
  geom_hline(aes(yintercept = 100), linetype="dashed", col="dodgerblue") +
  facet_wrap(~region, scales = "free", ncol = 1) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  geom_hline(yintercept = c(0))+
  customTheme +
  theme_minimal()


p2 <- ggplot(data = p2dat) +
  geom_bar(aes(x = capacity_multiplier_fct, y = perc_ICU_occup_median, group = interaction(delay, rollback, reopen), fill = reopen),
    stat = "identity", position = position_dodge(width = 0.91), width = 0.7
  ) +
  geom_errorbar(aes(x = capacity_multiplier_fct, ymin = perc_ICU_occup_95CI_lower, ymax = perc_ICU_occup_95CI_upper, 
                    group = interaction(delay, rollback, reopen)),
    position = position_dodge(width = 0.91), width = 0.1
  ) +
  labs(y="% of ICU capacity at predicted peak\nuntil end of December", x="Trigger treshold\n(as % of ICU capacity at which mitigation is triggered )")+
  geom_hline(aes(yintercept = 100), linetype="dashed", col="dodgerblue") +
  facet_wrap(~region, scales = "free", ncol = 1) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  customTheme +
  geom_hline(yintercept = c(0))+
  theme_minimal()

pall <- plot_grid(p1+theme(legend.position = "none"),p2+labs(y=""), ncol=2, rel_widths = c(1,0.5))
pall

ggsave(paste0("barplot_pall.pdf"),
  plot = pall,
  path = file.path(sim_dir, "ICU_bar_plots"), width = 10, height = 8, device = "pdf"
)


#### Pointrange

ggplot(data = p1dat) +
  geom_pointrange(aes(x = capacity_multiplier_fct, y = perc_ICU_occup_median,  ymin = perc_ICU_occup_95CI_lower, ymax = perc_ICU_occup_95CI_upper, 
                      group = interaction(delay, rollback, reopen), fill = reopen),
            position = position_dodge(width = 0.91), shape=21
  ) +
  geom_line(aes(x = capacity_multiplier_fct, y = perc_ICU_occup_median,  
                      group = interaction(delay, rollback, reopen), fill = reopen),
                  position = position_dodge(width = 0.91), width = 0.7,shape=21
  ) +
  labs(y="% of ICU capacity at predicted peak\nuntil end of December", x="Trigger treshold\n(as % of ICU capacity at which mitigation is triggered )")+
  geom_hline(aes(yintercept = 100), linetype="dashed", col="dodgerblue") +
  facet_wrap(~region, scales = "free", ncol = 1) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  customTheme +
  geom_hline(yintercept = c(0))+
  theme_minimal()




###### Text to describe figures


p2dat %>%
  group_by(ems, reopen, exp_name) %>%
  mutate(ratio =  counter_critical_median / icu_available) %>%
  as.data.frame() %>%
  group_by(ems,reopen) %>%
  summarize(ratio = mean(ratio))

p2dat %>%
  group_by(ems, reopen, exp_name) %>%
  mutate(ratio = counter_crit_det_95CI_lower / icu_available) %>%
  as.data.frame() %>%
  group_by(reopen) %>%
  summarize(ratio = mean(ratio))

p2dat %>%
  group_by(ems, reopen, exp_name) %>%
  mutate(ratio = counter_crit_det_95CI_upper / icu_available) %>%
  as.data.frame() %>%
  group_by(reopen) %>%
  summarize(ratio = mean(ratio))

#####
p2dat %>%
  group_by(ems, reopen, exp_name) %>%
  mutate(ratio = critical_median / icu_available) %>%
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
  mutate(perc_red_median = (1-(critical_median / counter_critical_median))*100) %>%
  dplyr::select(ems, exp_name,delay, reopen, capacity_multiplier,critical_median,counter_critical_median,  perc_red_median) %>% as.data.frame()%>%
  arrange(capacity_multiplier,ems,reopen )
  
#palldat <- data.table(palldat, key =c( "geography_name","capacity_multiplier", "date",'reopen' ))
#palldat[,ICU_diff_median := crit_det_median[rollback=="sm4"] - crit_det_median[rollback=="counterfactual"], by = c( "geography_name","capacity_multiplier", "date",'reopen' ) ]



