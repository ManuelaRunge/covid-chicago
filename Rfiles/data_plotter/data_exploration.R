

library(tidyverse)
library(zoo)
library(lubridate)

source("load_paths.R")


simdate ='200917'
dat <- f_loadData(data_path)

spec <- read.csv(file.path(data_path, "covid_IDPH/Cleaned Data", paste0(simdate, "_jg_specimen_collection_covidregion.csv"))) %>%
          mutate(date=as.Date(date)) %>%
          mutate(cases_rl=rollapply(cases,7,mean,align='right',fill=NA)) %>%
          mutate(week=week(date)) %>%
          group_by(covid_region, week)%>%
          mutate(cases_wk = mean(cases))

pop <- load_population()

ggplot(data=subset(spec, date > as.Date("2020-03-01"))) +
  geom_line(aes(x=date, y=cases_wk, group=1)) +
  facet_wrap(~covid_region, scales="free")

ggplot(data=subset(spec, date > as.Date("2020-07-01"))) +
  geom_line(aes(x=date, y=cases_wk, group=1)) +
  facet_wrap(~covid_region, scales="free")




#### LL data

#### Look at line list data 
LLdat <- read_csv("R:/PrevMed/Covid-19-Modeling/IDPH line list/LL_200917_JGcleaned.csv")
LLdat <- LLdat %>% mutate(date=as.Date(as.character(gsub("/","-",specimen_collection)), format="%m-%d-%y"), week=week(date), month=month(date)) 


LLdat$age_group <- factor(LLdat$age_group , 
                          levels=c("Under 21","21-30", "31-40", "41-50","51-60", "61-70", "71-80", "Over 80", "#N/A"),
                          labels=c("Under 21","21-30", "31-40", "41-50","51-60", "61-70", "71-80", "Over 80", "#N/A"))

ggplot(data=LLdat) + geom_bar(aes(x=age_group )) + facet_wrap(~restore_region, scales="free")
ggplot(data=subset(LLdat, date > as.Date("2020-08-01"))) + geom_bar(aes(x=age_group )) + facet_wrap(~restore_region, scales="free")
ggplot(data=subset(LLdat, date < as.Date("2020-08-01"))) + geom_bar(aes(x=age_group )) + facet_wrap(~restore_region, scales="free")







library(raster)
library(shapefile)
shp_path <- file.path(data_path, "covid_IDPH/shapefiles/covid_regions")


shp <- shapefile(file.path(data_path, "covid_IDPH/shapefiles/covid_regions/covid_regions_editedForR.shp"))
shp_f <- fortify(shp, id = REGION)
shp_f$region <- as.numeric(factor(shp_f$id, levels = c(9, 8, 7, 6, 5, 4, 2, 1, 3, 10, 0), labels = c(1:11)))
shp_f <- left_join(shp_f, dat, by = "region")


pmap1 <- ggplot(data = shp_f) +
  geom_polygon(aes(x = long, y = lat, group = region), fill = "lightgrey", color = "black") +
  geom_polygon(aes(x = long, y = lat, fill = Median.of.covid.19.Rt, group = region), color = "black") +
  # scale_fill_gradient2(low = "#f7fcfd", high = "#542788") +
  scale_fill_viridis(direction = -1) +
  labs(fill = "Rt") +
  theme_map() +
  theme(legend.position = "bottom")





