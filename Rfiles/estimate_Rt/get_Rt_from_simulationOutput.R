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

simdate = "20200624"
### Load simulation outputs
dat <- read.csv(file.path(project_path, "NU_civis_outputs",simdate,paste0("csv/nu_il_july1partial10_changeTDdetSym60AsP30_",simdate,".csv")))
summary(as.Date(dat$Date))



method <- "uncertain_si"
Rt_list <- list()
si_list <- list()

for (region in unique(dat$geography_modeled)) {
  # region = unique(dat$geography_modeled)[1]
  disease_incidence_data <- dat %>%
    filter(geography_modeled == region) %>%
    rename(I = Number.of.Covid.19.new.infections)

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

  pplot <- plot(res)

  ggsave(paste0(region, "_EpiEstim_default_",method,".pdf"),
    plot = pplot, path = file.path(outdir), width = 6, height = 10, dpi = 300, device = "pdf"
  )

  Rt_list[[region]] <- res$R %>% mutate(region = region)
  si_list[[region]] <- res$SI.Moments %>% mutate(region = region)
}


### Combine list to dataframe 
Rt_dat <- Rt_list %>% bind_rows()
table(Rt_dat$region)


dat <- dat %>%
  filter(geography_modeled %in% paste0("ems", c(1:11))) %>%
  rename(region = geography_modeled)

dat$EMS <- factor(dat$region,  levels = paste0("ems", c(1:11)), labels = paste0("EMS_", c(1:11)))


dat <- dat %>%
  arrange(EMS, Date) %>%
  group_by(EMS) %>%
  mutate(date = as.Date(Date), time = c(1:n_distinct(date)))


### Write csv file with Rt 
Rt_dat %>%
  merge(unique(dat[, c("time", "Date")]), by.x = "t_start", by.y = "time") %>%
  rename(geography_modeled = region,
    Median.of.covid.19.Rt = `Median(R)`,
    Lower.error.bound.of.covid.19.Rt = `Quantile.0.025(R)`,
    Upper.error.bound.of.covid.19.Rt = `Quantile.0.975(R)`
  ) %>%
  arrange(Date, geography_modeled) %>%
  filter(
    Date <= "2020-08-01") %>%
  select(Date, geography_modeled, Median.of.covid.19.Rt, Lower.error.bound.of.covid.19.Rt, Upper.error.bound.of.covid.19.Rt) %>%
  write.csv(paste0("estimate_Rt","/nu_il_july1partial10_changeTDdetSym60AsP30_estimated_Rt_",simdate,".csv"), row.names = FALSE)

