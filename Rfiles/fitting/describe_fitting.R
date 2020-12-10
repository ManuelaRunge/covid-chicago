###-------------------------------
### Rscript to describe fitting
###-------------------------------

library(readr)
library(data.table)
library(tidyverse)



#### RT reduction
nu_20200910 <- read_csv("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/NU_civis_outputs/20200910/csv/nu_20200910.csv")
ggplot(data=nu_20200910)+
  geom_line(aes(x=date, y=rt_median, col=geography_modeled)) +
  facet_wrap(~geography_modeled)+
  scale_x_date(breaks="30 days", date_labels = "%d\n%b")

Rtbase <- nu_20200910 %>% filter(date =="2020-03-06") %>% select(geography_modeled,rt_median, rt_lower ,rt_upper) 
RtMay <- nu_20200910 %>% filter(date =="2020-05-06") %>% select(geography_modeled,rt_median, rt_lower ,rt_upper)


trajectoriesDat <- read_csv("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/20200915_IL_mrfit_test/trajectoriesDat.csv")
paramname ="Ki_t_"
paramvars <- paste0(paramname, "EMS-", c(1:11))
keepvars <- c("time", "startdate", paramvars)

Kivalues <- trajectoriesDat %>%
  select(keepvars) %>%
  mutate(date = as.Date(startdate) + time) %>%
  pivot_longer(cols = -c("time", "date", "startdate"), names_to = "geography_modeled") %>%
  mutate(
    geography_modeled = gsub("EMS-", "", gsub(paramname, "", geography_modeled)),
    geography_modeled = as.numeric(geography_modeled)
  ) %>%
  group_by(date) %>%
  summarize(Ki_mean = mean(value)) 


Kibase <- Kivalues %>% filter(date > as.Date("2020-03-06") & date < as.Date("2020-03-07")) %>% select(Ki_mean) 
KiMay <- Kivalues %>% filter(date > as.Date("2020-05-06") & date < as.Date("2020-05-07")) %>% select(Ki_mean)

1-(KiMay/Kibase)


