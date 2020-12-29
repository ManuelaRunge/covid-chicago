### Describe Rt
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

custom_date_breaks=c(as.Date("2020-08-01"),as.Date("2020-09-01"),
                     as.Date("2020-10-01"), as.Date("2020-11-01"),
                     as.Date("2020-12-01"),as.Date("2021-01-01"))

simdate <- "20201212"
sim_dir <- file.path(simulation_output, "_overflow_simulations", simdate)
if (!dir.exists(file.path(sim_dir, "ICU_Rt_plots"))) dir.create(file.path(sim_dir, "ICU_Rt_plots"))


exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen", exp_names)]
exp_names <- exp_names[c(grep("daysdelay", exp_names), grep("counterfactual", exp_names))]
exp_names <- exp_names[!(grepl("_reopen", exp_names))]

dat <- f_combineDat(sim_dir, exp_names, "Rt_dat.csv")
dat$date_old <- as.Date(dat$date)
dat$date <- as.Date(dat$date)+ 61


subregions <- c("covidregion_1", "covidregion_4", "covidregion_11")
dat$region <- factor(dat$geography_modeled, levels = paste0("EMS-",c(1:11)), labels = paste0('Region ',c(1:11)))

dat <- dat %>% filter(geography_modeled %in% paste0("EMS-",c(1,4,11))) %>%
        f_get_scenVars()

table(dat$capacity_multiplier_fct, exclude = NULL)


###------------------------------
## Counterfactual
###------------------------------
dat_counterfactual <- dat %>% filter(rollback=="counterfactual")

ggplot(data=subset(dat_counterfactual, date <= as.Date("2020-12-31"))) +
  geom_ribbon(aes(x=date, ymin=rt_lower, ymax=rt_upper, fill=reopen_fct2), alpha=0.3)+
  geom_line(aes(x=date, y=rt_median, col=reopen_fct2))+
  facet_wrap(~region, ncol=1) +
  geom_hline(yintercept = 1)+
  geom_vline(xintercept = as.Date("2020-09-01"))+
  scale_y_continuous(lim=c(0.5,20)) +
  scale_x_date(date_breaks = "30 days", date_labels = "%b")+
  labs(x="",color="Transmission increase",fill="Transmission increase")+
  scale_color_manual(values = c(TwoCols_seq)) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  customTheme


pplot <- ggplot(data=subset(dat_counterfactual,  date >= as.Date("2020-08-01") &  date <= as.Date("2020-12-31"))) +
  geom_vline(xintercept = as.Date("2020-10-01"), col="lightgrey")+
  geom_rect(xmin=as.Date("2020-09-01"), xmax=as.Date("2020-10-01"), ymin=-Inf, ymax=Inf, alpha=0.03, fill="lightgrey")+
  geom_hline(yintercept = 1)+
  geom_ribbon(aes(x=date, ymin=rt_lower, ymax=rt_upper, fill=reopen_fct2), alpha=0.3)+
  geom_line(aes(x=date, y=rt_median, col=reopen_fct2))+
  facet_wrap(~region, ncol=1) +
  scale_x_date(breaks=c(as.Date("2020-08-01"), as.Date("2020-09-01"),as.Date("2020-10-01"),
                        as.Date("2020-11-01"),as.Date("2020-12-01"),as.Date("2021-01-01")), date_labels = "%b", expand=c(0,0))+
  labs(x="",color="Transmission increase",fill="Transmission increase")+
  scale_color_manual(values = c(TwoCols_seq)) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  customTheme
pplot

f_save_plot(
  plot_name = paste0("Rt_timeline_counterfactual"), pplot = pplot,
  plot_dir = file.path(sim_dir, "ICU_Rt_plots"), width = 6, height =8
)

dat_counterfactual %>% 
  filter(date==as.Date("2020-10-01"))   %>%
  select(region,  reopen, rt_median, rt_lower, rt_upper) %>%
  as.data.frame()

dat_counterfactual %>% 
  filter(date >= as.Date("2020-10-01"))  %>% 
  group_by(region, exp_name, reopen) %>%
  filter(rt_median==max(rt_median))  %>%  
  filter(date==min(date))  %>% 
  mutate(rt_median=round(rt_median,2),
         rt_lower=round(rt_lower,2),
         rt_upper=round(rt_upper,2)) %>%
  select(region,  reopen, rt_median, rt_lower, rt_upper) %>%
  as.data.frame()

dat_counterfactual %>% 
  filter(date >= as.Date("2020-10-01"))  %>% 
  group_by(region, exp_name, reopen) %>%
  filter(rt_median==max(rt_median))  %>%  
  filter(date==min(date))  %>% 
  group_by( exp_name, reopen_fct) %>%
  summarise(rt_median=round(mean(rt_median),2),
         rt_lower=round(mean(rt_lower),2),
         rt_upper=round(mean(rt_upper),2)) %>%
  select(  reopen, rt_median, rt_lower, rt_upper) %>%
  as.data.frame()

###------------------------------
## Scenarios
###------------------------------
dat_scenarios <- dat %>% filter(rollback!="counterfactual" & delay=="1daysdelay")

pplot <- ggplot(data=subset(dat_scenarios,  
                            rollback =="pr6" &  
                            date >= as.Date("2020-08-01") &  
                              date <= as.Date("2020-12-31"))) +
  geom_rect(xmin=-Inf, xmax=as.Date("2020-10-01"), ymin=-Inf, ymax=Inf, fill="lightgrey", alpha=0.05) + 
  geom_ribbon(aes(x=date, ymin=rt_lower, ymax=rt_upper, 
                  fill=reopen_fct2, group=interaction(capacity_multiplier_fct,reopen_fct2)), alpha=0.1)+
  geom_line(aes(x=date, y=rt_median, col=reopen_fct2, 
                linetype=capacity_multiplier_fct))+
  facet_grid(reopen_fct~region) +
  geom_hline(yintercept = 1)+
  scale_fill_manual(values = TwoCols_seq) +
  scale_color_manual(values = TwoCols_seq) +
  #scale_y_continuous(lim=c(0.5,2))+
  scale_x_date(breaks=custom_date_breaks, date_labels = "%b", expand=c(0,0))+
  labs(x="",y=expr(italic(R[t])), 
       color="Transmission\nincrease",
       fill="Transmission\nincrease",
       linetype="ICU trigger threshold (%)" )+
  customTheme+
  theme( panel.spacing = unit(1, "lines")) +
  guides(color = FALSE,fill = FALSE)
  
pplot


f_save_plot(plot_name=paste0("Rt_capacity_all_pr60"), 
            pplot = pplot, 
            plot_dir = file.path(sim_dir, "ICU_Rt_plots"), width = 12, height = 5)



pplot <- ggplot(data=subset(dat_scenarios, capacity_multiplier_fct=="80" &
                              delay=="1daysdelay" & 
                     date >= as.Date("2020-08-01") &  
                       date <= as.Date("2020-12-31"))) +
  geom_rect(xmin=-Inf, xmax=as.Date("2020-10-01"), ymin=-Inf, ymax=Inf, fill="lightgrey", alpha=0.05) + 
  geom_ribbon(aes(x=date, ymin=rt_lower, ymax=rt_upper, fill=reopen_fct2,
                  group=interaction(rollback_fct,reopen_fct2)), alpha=0.2)+
  geom_line(aes(x=date, y=rt_median, col=reopen_fct2, linetype=rollback_fct),size=1)+
  facet_grid(reopen_fct~region) +
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = c(-Inf, Inf))+ geom_vline(xintercept = c(-Inf, Inf)) + 
  scale_fill_manual(values = TwoCols_seq) +
  scale_color_manual(values = TwoCols_seq) +
  scale_x_date(breaks=custom_date_breaks, date_labels = "%b", expand=c(0,0))+
  labs(x="",y=expr(italic(R[t])), 
       color="Transmission\nincrease",
       fill="Transmission\nincrease",
       linetype="Mitigation strengths (%)" )+
  customTheme+
  theme(panel.spacing = unit(1, "lines")) +
  guides(color = FALSE,fill = FALSE,linetype = guide_legend(reverse=TRUE))

pplot

f_save_plot(plot_name=paste0("Rt_rollback_region_all_overview"), 
            pplot = pplot, 
            plot_dir = file.path(sim_dir, "ICU_Rt_plots"), width = 12, height = 5)



#### For text

dat_scenarios %>% 
  filter(capacity_multiplier_fct=="80" & 
                           delay=="1daysdelay"&
                           date == as.Date("2021-01-31")) %>%
                arrange(date, reopen,geography_modeled , rollback) %>%
                dplyr::select(date,geography_modeled , reopen, rollback,rt_median ,rt_lower , rt_upper ) %>%
                mutate(rt_median=round(rt_median,3),
                       rt_lower=round(rt_lower,3),
                       rt_upper=round(rt_upper,3))


dat_scenarios %>% 
  filter(capacity_multiplier_fct=="80" & 
           delay=="1daysdelay"&
           date == as.Date("2020-12-31")) %>%
  arrange(date, reopen,geography_modeled , rollback) %>%
  group_by(reopen, rollback ) %>%
  summarize(rt_median=round(mean(rt_median),3),
         rt_lower=round(mean(rt_lower),3),
         rt_upper=round(mean(rt_upper),3)) 
