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

simdate = "20200615"
expname = "20200615_IL_test_TD_AsP0"
exp_dir <- file.path(simulation_output, "contact_tracing",simdate,expname)


reopeningdate <- as.Date("2020-06-01")
evaluation_window <- c(reopeningdate, reopeningdate + 60)

detectionVar <- "d_Sym_ct1"  # "d_As_ct1"
isolationVar <- "reduced_inf_of_det_cases_ct1"

### Load simulation outputs
dat <- read.csv(file.path(exp_dir, "trajectoriesDat.csv"))
dat <- subset(dat, time >= as.Date(reopeningdate) - as.Date(max(dat$startdate)))

dat$detection_success <- dat[, colnames(dat)==detectionVar]
dat$isolation_success <- 1-(dat[, colnames(dat)==isolationVar])

dat <- dat %>% mutate(
  startdate = as.Date(startdate),
  Date = as.Date(time + startdate)
)

summary(as.Date(dat$Date))



method <- "uncertain_si"
Rt_list <- list()
si_list <- list()

for(ems in c(1:11)){
  #ems=1
  print(ems)
  count=0
  tempdat = dat 
  colnames(tempdat)[colnames(tempdat)== paste0( "infected_cumul_EMS.",ems)]  = "infected_cumul"
  
  tempdat <- tempdat %>% 
    mutate(
      startdate = as.Date(startdate),
      Date = as.Date(time + startdate),
    ) %>%
    group_by( scen_num) %>%
    arrange( scen_num, Date) %>%
    mutate(region = ems, new_infections = infected_cumul - lag(infected_cumul) )
  

  
  for (scen in unique(tempdat$scen_num)) {
    count = count + 1
    # scen = unique(dat$scen_num)[1]
  disease_incidence_data <- tempdat %>%
    filter(region == ems ,   scen_num == scen) %>%
    rename(I = new_infections) %>%
    mutate(I = ifelse(I <0,0,I)) %>%
    select(Date, I ,  infected_cumul) %>%
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

  Rt_tempdat  <- res$R %>% mutate(region = ems)
  Rt_tempdat$scen_num = scen

  if(count==1)Rt_tempdat_All  <- Rt_tempdat
  if(count!=1)Rt_tempdat_All  <- rbind(Rt_tempdat_All,Rt_tempdat)
  
  SI_tempdat  <- res$SI.Moments %>% mutate(region = ems)
  SI_tempdat$scen_num = scen

  if(count==1)SI_tempdat_All  <- SI_tempdat
  if(count!=1)SI_tempdat_All  <- rbind(SI_tempdat_All,SI_tempdat) 
  
  rm(Rt_tempdat, SI_tempdat)
}

  save(Rt_tempdat_All, file=file.path(exp_dir, paste0(ems,"_temp_Rt_tempdat_All.Rdata")))
  Rt_list[[ems]] <- Rt_tempdat_All
  si_list[[ems]] <- SI_tempdat_All
  
  rm(Rt_tempdat_All, SI_tempdat_All)
}


### Combine list to dataframe 
Rt_dat <- Rt_list %>% bind_rows()
table(Rt_dat$region)
table(Rt_dat$scen_num, Rt_dat$t_start)

#Rt_dat2 <- Rt_dat %>% group_by(t_start, scen_num) %>% mutate(reg = unique(dat$region) )

dat <- dat %>%
  arrange( Date) %>%
  group_by( scen_num) %>%
  mutate(date = as.Date(Date), time = c(1:n_distinct(date)))


### Edit dataframe
Rt_dat2 <- Rt_dat %>%
  merge(unique(dat[, c("time", "Date","scen_num","time_to_detection","reduced_inf_of_det_cases_ct1","d_Sym_ct1", "fraction_symptomatic","fraction_severe")]),
        by.x = c("t_start", "scen_num"), by.y = c("time", "scen_num")) %>%
  mutate(EMS = factor(region, levels = c(1:11), labels = paste0("EMS_", c(1:11))),
         isolation_success  = 1-reduced_inf_of_det_cases_ct1,
         detection_success = d_Sym_ct1)


colnames(Rt_dat2) <- gsub("[(R]","",colnames(Rt_dat2))
colnames(Rt_dat2) <- gsub("[)]","",colnames(Rt_dat2))

Rt_dat2 <- Rt_dat2 %>% mutate(meanRtLE1 = ifelse(Median <1 , 1, 0))

summary(Rt_dat2$fraction_severe)
Rt_dat2$fraction_severe_fct = NA
Rt_dat2$fraction_severe_fct[Rt_dat2$fraction_severe > 0.08] =  ">0.8"
Rt_dat2$fraction_severe_fct[Rt_dat2$fraction_severe <= 0.08] = ">0.7"
Rt_dat2$fraction_severe_fct[Rt_dat2$fraction_severe <= 0.07] = ">0.6"
Rt_dat2$fraction_severe_fct[Rt_dat2$fraction_severe <= 0.06] = "<0.6"
table(Rt_dat2$fraction_severe_fct)

summary(Rt_dat2$fraction_symptomatic)
Rt_dat2$fraction_symptomatic_fct = NA
Rt_dat2$fraction_symptomatic_fct[Rt_dat2$fraction_symptomatic <= 0.7] = ">0.65"
Rt_dat2$fraction_symptomatic_fct[Rt_dat2$fraction_symptomatic <= 0.65] = ">0.6"
Rt_dat2$fraction_symptomatic_fct[Rt_dat2$fraction_symptomatic <= 0.6] = ">0.55"
Rt_dat2$fraction_symptomatic_fct[Rt_dat2$fraction_symptomatic <= 0.55] = ">0.5"
Rt_dat2$fraction_symptomatic_fct[Rt_dat2$fraction_symptomatic <= 0.5] = "<0.5"
table(Rt_dat2$fraction_symptomatic_fct)

write.csv(Rt_dat2, file=file.path(exp_dir, paste0("estimated_Rt.csv")), row.names = FALSE)

### Generate plots 

p1 <- ggplot(data = subset(Rt_dat2, t_start <= 160)) +
  theme_bw() +
  geom_line(aes(x = t_start, y = Median,  group=scen_num), col = "deepskyblue3", size = 1.3) +
  geom_ribbon(aes(x = t_start, ymin = Quantile.0.025, ymax = Quantile.0.975, group=scen_num), fill = "deepskyblue3", alpha = 0.3) +
  facet_grid(time_to_detection~EMS) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  customThemeNoFacet


p2 <- ggplot(data = subset(Rt_dat2,  t_start == 160 )) +
  theme_bw() +
  geom_point(aes(x = isolation_success, y=detection_success , col = as.factor(meanRtLE1), group= Median), size = 2) +
  facet_grid(time_to_detection ~ EMS) +
  theme(legend.position = "right") +
  scale_color_manual(values=c("deepskyblue3","darkorange")) +
  labs(color="Rt < 1")+
  customThemeNoFacet

# p3 <- ggplot(data = subset(Rt_dat2,  t_start == 160 & region==1)) +
#   theme_bw() +
#   geom_point(aes(x = isolation_success, y=detection_success , col = as.factor(meanRtLE1), group= Median), size = 2) +
#   facet_grid(fraction_severe_fct~time_to_detection) +
#   theme(legend.position = "right") +
#   scale_color_manual(values=c("deepskyblue3","darkorange")) +
#   labs(color="Rt < 1")+
#   customThemeNoFacet

ggsave(paste0("Rt_plot1.pdf"),
       plot = p1, path = file.path(exp_dir), width = 24, height = 12, dpi = 300, device = "pdf"
)

ggsave(paste0("Rt_plot2.pdf"),
  plot = p2, path = file.path(exp_dir), width = 24, height = 12, dpi = 300, device = "pdf"
)

