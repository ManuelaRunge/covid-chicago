
## ============================================================
## Compare Rt estimates between simulated experiments
## ============================================================

library(tidyverse)
library(cowplot)


simdate <- gsub("-", "", Sys.Date())
plot_start_date = as.Date("2020-08-01")
plot_end_date = as.Date("2020-12-30")

source("load_paths.R")
source("processing_helpers.R")
source("estimate_Rt/getRt_function.R")

exp_name1 = "20200831_IL_regreopen100perc_7daysdelay_sm6"
exp_name2 = "20200831_IL_regreopen50perc_7daysdelay_sm6"

##-------------------------------------------
exp_name =exp_name1
fname = "_estimated_Rt"
Rt_dir <- file.path(simulation_output, exp_name,  "estimatedRt")
load(file.path(Rt_dir, paste0("combined",fname, ".Rdata")))
Rt_dat100 =Rt_dat%>% mutate(reopen="hard", exp="100perc")

rm(Rt_dat)
exp_name =exp_name2
fname = "_estimated_Rt"
Rt_dir <- file.path(simulation_output, exp_name,  "estimatedRt")
load(file.path(Rt_dir, paste0("combined",fname, ".Rdata")))
Rt_dat50 =Rt_dat%>% mutate(reopen="soft", exp="50perc")

Rt_dat= rbind(Rt_dat100, Rt_dat50)

### Combine list to dataframe
Rt_dat <- Rt_dat %>%
  mutate(time = t_end) %>%
  rename(meanRt = `Mean(R)`,
         medianRt = `Median(R)`,
         q2.5Rt = `Quantile.0.025(R)`,
         q97.5Rt = `Quantile.0.975(R)`,
         q25Rt = `Quantile.0.25(R)`,
         q75Rt = `Quantile.0.75(R)`)%>%
  dplyr::mutate(time=time+120) %>%
  dplyr::mutate(Date = as.Date("2020-02-13") + time) %>%
  filter(Date <= "2020-12-14") 


Rt_dat$region <- factor(Rt_dat$region, levels =c(1:11), labels = c(1:11))

pplot <- Rt_dat %>%
  filter(Date >= plot_start_date & Date <= plot_end_date) %>%
  #filter(scen_num %in% c(798)) %>%
  ggplot() +
  theme_cowplot() +
  geom_rect(xmin = -Inf, xmax = as.Date(Sys.Date()), ymin = -Inf, ymax = Inf, fill = "lightgrey", alpha = 0.02) +
  geom_ribbon(aes(x = Date, ymin = q2.5Rt, ymax = q97.5Rt, fill=reopen),  alpha = 0.3) +
  geom_ribbon(aes(x = Date, ymin = q25Rt, ymax = q75Rt, fill=reopen),  alpha = 0.4) +
  geom_line(aes(x = Date, y = medianRt, col=reopen)) +
  facet_wrap(~region) +
  # background_grid() +
  # geom_hline(yintercept = seq(0.6, 1.4, 0.2), col="grey", size=0.7)+
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  customThemeNoFacet+
  labs(x="",y=expression(italic(R[t])))+
  scale_color_manual(values=c("#084594","red"))+
  scale_fill_manual(values=c("#084594","red"))


ggsave(paste0("estimatedRt_overtime.png"),
       plot = pplot, path = file.path(simulation_output, exp_name, "estimatedRt"), width = 12, height = 6, device = "png"
)
ggsave(paste0("estimatedRt_overtime.pdf"),
       plot = pplot, path = file.path(simulation_output, exp_name, "estimatedRt"), width = 12, height = 6, device = "pdf"
)
rm(pplot)

