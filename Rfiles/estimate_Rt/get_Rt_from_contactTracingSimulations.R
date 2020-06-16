## ============================================================
## R script to get R(t) from simulation outputs
## ============================================================

# install.packages("devtools")
# library(devtools)
# install_github("annecori/EpiEstim", force = TRUE)
library(tidyverse)
library(EpiEstim)


source("load_paths.R")
source("processing_helpers.R")
outdir <- file.path("estimate_Rt/from_simulations")

simdate = "20200611_AsPSym"
### Load simulation outputs
dat <- read.csv(file.path(simulation_output, "contact_tracing",simdate,paste0("20200611_IL_EMS_AsPSym_rS6_heatmap/trajectoriesDat.csv")))
#dat <- read.csv(file.path(project_path, "NU_civis_outputs",simdate,paste0("csv/nu_il_baseline_",simdate,".csv")))
summary(as.Date(dat$Date))

dat <- dat %>% pivot_wider(names_from = outcome, values_from = value) %>%
          group_by(region, scen_num) %>%
          arrange(region, scen_num, Date) %>%
          mutate(new_infections =infected_cumul - lag(infected_cumul) )

method <- "uncertain_si"
Rt_list <- list()
si_list <- list()

count=0
for (reg in unique(dat$region)) {
  
  for (scen in unique(dat$scen_num)) {
    count = count + 1
    # scen = unique(dat$scen_num)[1]
    # reg = unique(dat$region)[1]
  disease_incidence_data <- dat %>%
    filter(region == reg ,   scen_num == scen) %>%
    rename(I = new_infections) %>%
    select(Date, I , infected, infected_cumul) %>%
    filter(!is.na(I))

  ## check what si_distr to assume, or calculate from predictions, here using an example from the package
  if(method=="non_parametric_si"){  
    si_distr <- c(0.000, 0.233, 0.359, 0.198, 0.103, 0.053, 0.027 ,0.014 ,0.007, 0.003, 0.002 ,0.001)
    res <- estimate_R(incid = disease_incidence_data$I,
                      method = "non_parametric_si",
                      config = make_config(list(si_distr = si_distr)))
    
  }
  
  ### use parametric_si
  if(method=="parametric_si"){  
    res <- estimate_R(incid = disease_incidence_data$I,
                      method = "parametric_si",
                      config = make_config(list(mean_si = 2.6, std_si = 1.5)))
  }
  
  ## biweekly sliding
  t_start <- seq(2, nrow(disease_incidence_data)-13)   
  t_end <- t_start + 13  
  
  ## estimate the reproduction number (method "uncertain_si")
  if(method=="uncertain_si"){
    res <- estimate_R(disease_incidence_data$I,
                      method = "uncertain_si",
                      config = make_config(list(
                        t_start = t_start, 
                        t_end = t_end,
                        mean_si = 4.6, std_mean_si = 1,
                        min_mean_si = 1, max_mean_si = 7.5,
                        std_si = 1.5, std_std_si = 0.5,
                        min_std_si = 0.5, max_std_si = 2.5,
                        n1 = 100, n2 = 100
                      ))
    )
  }

 # pplot <- plot(res)

 # ggsave(paste0(region, "_EpiEstim_default_",method,".pdf"),
 #   plot = pplot, path = file.path(outdir), width = 6, height = 10, dpi = 300, device = "pdf"
 # )

  Rt_tempdat  <- res$R %>% mutate(region = region)
  Rt_tempdat$scen_num = scen
  Rt_tempdat$region = reg
  if(count==1)Rt_tempdat_All  <- Rt_tempdat
  if(count!=1)Rt_tempdat_All  <- rbind(Rt_tempdat_All,Rt_tempdat)
  
  SI_tempdat  <- res$SI.Moments %>% mutate(region = region)
  SI_tempdat$scen_num = scen
  SI_tempdat$region = reg
  if(count==1)SI_tempdat_All  <- SI_tempdat
  if(count!=1)SI_tempdat_All  <- rbind(SI_tempdat_All,SI_tempdat) 
}

  Rt_list[[reg]] <- Rt_tempdat_All
  si_list[[reg]] <- SI_tempdat_All
  
}

### Combine list to dataframe 
Rt_dat <- Rt_list %>% bind_rows()
table(Rt_dat$region)
table(Rt_dat$scen_num, Rt_dat$t_start)

#Rt_dat2 <- Rt_dat %>% group_by(t_start, scen_num) %>% mutate(reg = unique(dat$region) )

dat <- dat %>%
  rename(geography_modeled = region)
table(dat$geography_modeled)

dat$EMS <- factor(dat$geography_modeled,  levels = c(1:11), labels = paste0("EMS_", c(1:11)))
table(dat$EMS)

dat <- dat %>%
  arrange(EMS, Date) %>%
  group_by(EMS, scen_num) %>%
  mutate(date = as.Date(Date), time = c(1:n_distinct(date)))


### Write csv file with Rt 
Rt_dat %>%
  merge(unique(dat[, c("time", "Date")]), by.x = "t_start", by.y = "time") %>%
  rename(geography_modeled = reg,
    Median.of.covid.19.Rt = `Median(R)`,
    Lower.error.bound.of.covid.19.Rt = `Quantile.0.025(R)`,
    Upper.error.bound.of.covid.19.Rt = `Quantile.0.975(R)`
  ) %>%
  arrange(Date, geography_modeled, scen_num) %>%
  filter(
    Date <= "2020-08-01") %>%
  select(Date, geography_modeled,scen_num,  Median.of.covid.19.Rt, Lower.error.bound.of.covid.19.Rt, Upper.error.bound.of.covid.19.Rt) %>%
  write.csv(paste0("estimated_Rt.csv"), row.names = FALSE)



#### Edit dataframe
Rt_dat <- Rt_dat %>% 
  mutate(EMS = factor(region, levels = c(1:11), labels = paste0("EMS_", c(1:11))))



ggplot(data = dat) +
  geom_point(aes(x = Date, y = new_infections, group=scen_num), stat = "identity") +
  facet_wrap(~EMS, scales = "free")



## Used for secondary axis in plot showing both cases and Rt
scl <- mean(dat$new_infections) / mean(Rt_dat$`Median(R)`)



### Generate plots 
pall <- ggplot(data = subset(Rt_dat, t_start <= 160)) +
  theme_bw() +
  geom_line(aes(x = t_start, y = `Median(R)`), col = "deepskyblue3", size = 1.3) +
  geom_ribbon(aes(x = t_start, ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`), fill = "deepskyblue3", alpha = 0.5) +
  facet_wrap(~EMS, scales = "free_y") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  customThemeNoFacet


pcut <- ggplot(data = subset(Rt_dat, t_start <= 160)) +
  theme_bw() +
  geom_line(aes(x = t_start, y = `Median(R)`), col = "deepskyblue3", size = 1.3) +
  geom_ribbon(aes(x = t_start, ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`), fill = "deepskyblue3", alpha = 0.5) +
  facet_wrap(~EMS, scales = "free_y") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  customThemeNoFacet


pplot <- ggplot(data = subset(Rt_dat, t_start <= 160)) +
  theme_bw() +
  geom_bar(data = subset(dat, time <= 210), aes(x = time, y = Number.of.Covid.19.new.infections / scl), fill = "grey", stat = "identity", alpha = 0.9) +
  geom_line(aes(x = t_start, y = `Median(R)`), col = "deepskyblue4", size = 1.3) +
  geom_ribbon(aes(x = t_start, ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`), fill = "deepskyblue4", alpha = 0.5) +
  facet_wrap(~EMS, scales = "free_y") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous("R0", sec.axis = sec_axis(~ . * scl, name = "Cases")) +
  labs(caption = "Using 'uncertain_si' distribution") +
  customThemeNoFacet


ggsave(paste0("Rt_simulation_uncertain_si_v3.pdf"),
  plot = pplot, path = file.path(outdir), width = 14, height = 8, dpi = 300, device = "pdf"
)
ggsave(paste0("Rt_simulation_uncertain_si_v3.png"),
  plot = pplot, path = file.path(outdir), width = 14, height = 8, dpi = 300, device = "png"
)
ggsave(paste0("Rt_simulation_uncertain_si_v2.pdf"),
  plot = pcut, path = file.path(outdir), width = 14, height = 8, dpi = 300, device = "pdf"
)
ggsave(paste0("Rt_simulation_uncertain_si_v2.png"),
  plot = pcut, path = file.path(outdir), width = 14, height = 8, dpi = 300, device = "png"
)

ggsave(paste0("Rt_simulation_uncertain_si_v1.png"),
  plot = pall, path = file.path(outdir), width = 14, height = 8, dpi = 300, device = "png"
)
