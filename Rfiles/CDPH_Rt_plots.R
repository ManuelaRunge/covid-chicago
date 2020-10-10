
library(tidyverse)
library(data.table)
library(cowplot)

Ildat <- fread("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/NU_civis_outputs/20201006/csv/nu_20201006.csv") %>%
  filter(geography_modeled == "illinois")
Ildat$date <- as.Date(as.character(Ildat$date), format="%m/%d/%Y")
Ildat <- Ildat %>% filter(date <=as.Date("2020-10-08"))


dat <- fread("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/NU_civis_outputs/20201006/csv/nu_20201006.csv") %>%
        filter(geography_modeled == "covidregion_11")

dat$date <- as.Date(as.character(dat$date), format="%m/%d/%Y")
dat <- dat %>% filter(date <=as.Date("2020-10-08"))


subset(dat, date >=as.Date("2020-10-07") & date <=as.Date("2020-10-07"))
subset(dat, date <=as.Date("2020-03-15"))

outdir <- getwd()


source("processing_helpers.R")

p1 <- ggplot(data=dat)+
  theme_cowplot()+
  geom_hline(yintercept = Inf) +  geom_vline(xintercept = Inf) +
  geom_hline(yintercept = 1) +
  geom_ribbon(aes(x=date,ymin=rt_lower,ymax=rt_upper), fill="deepskyblue4", alpha=0.3) +
  geom_line(aes(x=date,y=rt_median), col="deepskyblue4",size=1) +
 # scale_y_continuous(breaks=seq(0.8,1.2,0.05)) +
  #background_grid()+
  labs(title="Chicago", 
       subtitle="Estimated Rt using NU's COVID transmission model \nEstimated Rt for Oct 7th: 1.013 (95%CI: 0.991 - 1.034) ",
       x="", caption="\n *Model fitted to hospitalizations, intensive care unit census and deaths\n *Rt estimated using EpiEstim with an uncertain SI distribution",
       y=expr(italic(R[t]))) +
  scale_x_date(date_breaks = "30 days", date_labels = "%b") 


p2 <- ggplot(data=subset(dat, date >=as.Date("2020-04-01")))+
  theme_cowplot()+
  #geom_hline(yintercept = c(1.05, 0.95), linetype="dashed") +
  geom_hline(yintercept = Inf) +  geom_vline(xintercept = Inf) +
  geom_hline(yintercept = 1) +
  geom_ribbon(aes(x=date,ymin=rt_lower,ymax=rt_upper), fill="deepskyblue4", alpha=0.3) +
  geom_line(aes(x=date,y=rt_median), col="deepskyblue4",size=1) +
  #scale_y_log10()+
  labs(title="Chicago", 
       subtitle="Estimated Rt using NU's COVID transmission model \nEstimated Rt for Oct 7th: 1.013 (95%CI: 0.991 - 1.034) ",
       x="", caption="\n *Model fitted to hospitalizations, intensive care unit census and deaths\n *Rt estimated using EpiEstim with an uncertain SI distribution",
       y=expr(italic(R[t]))) +
  scale_y_continuous(breaks=seq(0.8,1.2,0.05)) +
  background_grid()+
  scale_x_date(date_breaks = "30 days", date_labels = "%b") 


p2b <- ggplot(data=subset(dat, date >=as.Date("2020-03-15")))+
  theme_cowplot()+
  #geom_hline(yintercept = c(1.05, 0.95), linetype="dashed") +
  geom_hline(yintercept = Inf) +  geom_vline(xintercept = Inf) +
  geom_hline(yintercept = 1) +
  geom_ribbon(aes(x=date,ymin=rt_lower,ymax=rt_upper), fill="deepskyblue4", alpha=0.3) +
  geom_line(aes(x=date,y=rt_median), col="deepskyblue4",size=1) +
  #scale_y_log10()+
  labs(title="Chicago", 
       subtitle="Estimated Rt using NU's COVID transmission model \nEstimated Rt for Oct 7th: 1.013 (95%CI: 0.991 - 1.034) ",
       x="", caption="\n *Model fitted to hospitalizations, intensive care unit census and deaths\n *Rt estimated using EpiEstim with an uncertain SI distribution\nPlot truncated in March, before March 15th, Rt was estimated at 3.62 (95%CI 1.82-7.22) ",
       y=expr(italic(R[t]))) +
  scale_y_continuous(breaks=seq(0.8,1.2,0.05)) +
  background_grid()+
  scale_x_date(date_breaks = "30 days", date_labels = "%b") 


p3 <-  ggplot()+
  theme_cowplot()+
  #geom_hline(yintercept = c(1.05, 0.95), linetype="dashed") +
  geom_hline(yintercept = Inf) +  geom_vline(xintercept = Inf) +
  geom_hline(yintercept = 1) +
  geom_ribbon(data=subset(dat, date >=as.Date("2020-04-01")),aes(x=date,ymin=rt_lower,ymax=rt_upper), fill="deepskyblue3", alpha=0.3) +
  geom_line(data=subset(dat, date >=as.Date("2020-04-01")),aes(x=date,y=rt_median), col="deepskyblue3",size=1) +
  geom_ribbon(data=subset(Ildat, date >=as.Date("2020-04-01")),aes(x=date,ymin=rt_lower,ymax=rt_upper), fill="deepskyblue4", alpha=0.3) +
  geom_line(data=subset(Ildat, date >=as.Date("2020-04-01")),aes(x=date,y=rt_median), col="deepskyblue4",size=1) +
  #scale_y_log10()+
  labs(title="Chicago", 
       subtitle="Estimated Rt using NU's COVID transmission model \nEstimate for Oct 7th: 1.013 (95%CI: 0.991 - 1.034) ",
       x="", caption="\n *Model fitted to hospitalizations, intensive care unit census and deaths\n *Rt estimated using EpiEstim with an uncertain SI distribution",
       y=expr(italic(R[t]))) +
  scale_y_continuous(breaks=seq(0.8,1.2,0.05)) +
  background_grid()+
  scale_x_date(date_breaks = "30 days", date_labels = "%b") 




ggsave(paste0("Rt_chicago_Mar_Oct", ".png"),
       plot = p1, path = file.path(outdir), width = 10, height = 6, device = "png"
)
ggsave(paste0("Rt_chicago_Mar_Oct", ".pdf"),
       plot = p1, path = file.path(outdir), width = 10, height = 6, device = "pdf"
)

ggsave(paste0("Rt_chicago_Mar15_Oct", ".png"),
       plot = p2b, path = file.path(outdir), width = 10, height = 6, device = "png"
)
ggsave(paste0("Rt_chicago__Mar15_Oct", ".pdf"),
       plot = p2b, path = file.path(outdir), width = 10, height = 6, device = "pdf"
)

ggsave(paste0("Rt_chicago_Apr_Oct", ".png"),
       plot = p2, path = file.path(outdir), width = 10, height = 6, device = "png"
)
ggsave(paste0("Rt_chicago_Apr_Oct", ".pdf"),
       plot = p2, path = file.path(outdir), width = 10, height = 6, device = "pdf"
)


ggsave(paste0("Rt_chicago_illinois", ".png"),
       plot = p3, path = file.path(outdir), width = 10, height = 6, device = "png"
)
ggsave(paste0("Rt_chicago_illinois", ".pdf"),
       plot = p3, path = file.path(outdir), width = 10, height = 6, device = "pdf"
)

