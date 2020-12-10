


library(tidyverse)
library(cowplot)
library(data.table)
library(raster)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")

theme_set(theme_cowplot())


dat1 <- f_loadData(data_path) %>% rename(covidregion=region, date=Date)

dat <- fread(file.path(data_path, "covid_IDPH","Corona virus reports", "CLI_admissions.csv"))
dat <- dat  %>% rename(County=region) %>%mutate(County=tolower(County))

dat_boundaries <- fread(file.path(data_path, "covid_IDPH","EMS Population", "covidregion_population_by_county.csv"))
dat_boundaries <- dat_boundaries %>%mutate(County=tolower(County))


dat <- left_join(dat,dat_boundaries, by="County" ) %>% rename(covidregion=new_restore_region)

dat$date <- as.Date(dat$date)
datAggr <-  dat %>% 
                  group_by(  date   , covidregion, age_group) %>%
                  summarize(inpatient=sum(inpatient))

datAggr2 <-   datAggr %>% 
                  group_by( date   ,covidregion) %>% 
                  summarize(inpatient=sum(inpatient))
write.csv(datAggr2,file.path(data_path, "covid_IDPH","Corona virus reports", "CLI_admissions_by_covidregion.csv") ,quote=FALSE)



datAggr2 <- left_join(datAggr2, dat1, by =c("covidregion","date"))

plotdat <- datAggr2 %>%
            filter( covidregion %in% c(1,2,5,10) & date >= as.Date("2020-04-01")) %>%
           # select() %>% 
            pivot_longer(cols=-c(date, covidregion,restore_region)) %>%
            group_by(covidregion, name) %>%
            mutate(value_avrg = rollapply(value,7,mean,align='right',fill=NA)) # rolling average 
  


p1 <- ggplot(data=subset(plotdat,date >= as.Date("2020-09-01")  & name%in% c('confirmed_covid_icu','LL_admissions',  'covid_non_icu', 'inpatient')))+  
  geom_point( aes(x=date, y=value, col=name), size=1) +
  geom_line(aes(x=date, y=value_avrg, col=name),size=1.3) +
  facet_wrap(~covidregion, scales="free", ncol=2)+
  labs(color="")+
  customThemeNoFacet+
  geom_hline(yintercept = Inf)+ geom_vline(xintercept = Inf)+
  scale_colour_manual(values=c("#fdbf11","#ec008b","#5c5859",'#1696d2'))+
  theme(legend.position = "none")

ggsave("20201009_for_civis_plot_1.pdf", plot = p1,  width = 10, height = 10, device = "pdf")
ggsave("20201009_for_civis_plot_1.png", plot = p1, width = 10, height = 10, device = "png")
rm(p1)


p1 <- ggplot(data=subset(plotdat, name%in% c('LL_admissions',  'inpatient')))+  
  geom_point( aes(x=date, y=value, col=name), size=1) +
  geom_line(aes(x=date, y=value_avrg, col=name),size=1.3) +
  facet_wrap(~covidregion, scales="free", ncol=2)+
  labs(color="")+
  customThemeNoFacet+
  geom_hline(yintercept = Inf)+ geom_vline(xintercept = Inf)+
  scale_colour_manual(values=c("#fdbf11","#ec008b","#5c5859",'#1696d2'))+
  theme(legend.position = "none")



p2 <- ggplot(data=subset(plotdat, date >= as.Date("2020-08-01") & name%in% c('LL_admissions',  'inpatient')))+  
  geom_point( aes(x=date, y=value, col=name), size=1) +
  geom_line(aes(x=date, y=value_avrg, col=name),size=1.3) +
  facet_wrap(~covidregion, scales="free", ncol=1)+
  labs(color="")+
  customThemeNoFacet+
  geom_hline(yintercept = Inf)+ geom_vline(xintercept = Inf)+
  scale_colour_manual(values=c("#fdbf11","#ec008b","#5c5859",'#1696d2'))+
  theme(legend.position = "none") #+
 # scale_y_log10()


p12 <- plot_grid(p1,p2, rel_widths = c(1,0.4))

ggsave("20201009_for_civis_plot.pdf", plot = p12,  width = 10, height = 10, device = "pdf")
ggsave("20201009_for_civis_plot.png", plot = p12, width = 10, height = 10, device = "png")


