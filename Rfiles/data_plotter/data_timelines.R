
library(tidyverse)
library(cowplot)
source("load_paths.R")
source("processing_helpers.R")
source("estimate_Rt/getRt_function.R")

dat <- f_loadData(data_path)
pdfdir <- "C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/project_notes/publications/covid_model_IL/pdfs_ais/"


dat %>%
  group_by(Date) %>%
  summarize(icu_cases=sum(suspected_and_confirmed_covid_icu),
            deaths=sum(confirmed_covid_deaths_prev_24h)) %>%
  pivot_longer(cols=-c(Date)) %>%
  filter(name!="deaths") %>%
ggplot() + 
  geom_line(aes(x=Date, y=value),size=1.3, col="deepskyblue3")+
  theme_cowplot()+
  scale_y_log10(expand=c(0,0))


pplot <- dat %>%
  group_by(Date) %>%
  filter(Date >= as.Date("2020-06-01")) %>%
  summarize(icu_cases=sum(suspected_and_confirmed_covid_icu + covid_non_icu),
            deaths=sum(confirmed_covid_deaths_prev_24h)) %>%
  pivot_longer(cols=-c(Date)) %>%
  filter(name!="deaths") %>%
  ggplot() + 
  geom_line(aes(x=Date, y=value),size=1.7, col="deepskyblue3")+
  theme_cowplot()+
  scale_y_continuous(lim=c(0,3000))+
  scale_x_date(date_breaks = "1 month", date_labels="%d\n%b")+
  labs(y="COVID-19 non-ICU and ICU census\n", x="")+
  customThemeNoFacet+
  geom_hline(yintercept = c(-Inf, Inf))+geom_vline(xintercept = c(-Inf, Inf))



ggsave(paste0("EMRresource_nonICU_ICU_timeline_IL", ".png"),
       plot = pplot, path = file.path(pdfdir), width = 6, height = 5, device = "png"
)
ggsave(paste0("EMRresource_nonICU_ICU_timeline_IL",  ".pdf"),
       plot = pplot, path = file.path(pdfdir ), width = 6, height = 5, device = "pdf"
)


dat %>%
  group_by(Date) %>%
  mutate(icu_cases=sum(suspected_and_confirmed_covid_icu),
            deaths=sum(confirmed_covid_deaths_prev_24h)) %>%
  select(Date, region, icu_cases, deaths)%>%
  pivot_longer(cols=-c(Date, region)) %>%
  filter(name!="deaths") %>%
  ggplot() + 
  geom_line(aes(x=Date, y=value, col=name))+
  facet_wrap(~region) +
  theme_cowplot()


dat$region <- factor(dat$region, levels=c(11:1), labels=c(11:1))
dat %>%
  group_by(Date) %>%
  mutate(icu_cases=sum(suspected_and_confirmed_covid_icu),
         deaths=sum(confirmed_covid_deaths_prev_24h)) %>%
  select(Date, region, icu_cases, deaths)%>%
  pivot_longer(cols=-c(Date, region)) %>%
  filter(name!="deaths") %>%
  ggplot() + 
  geom_area(aes(x=Date, y=value, fill=as.factor(region), group=region))+
  theme_cowplot()


