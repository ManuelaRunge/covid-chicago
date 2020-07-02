

library(tidyverse)
library(viridis)
library(raster)
library(ggthemes)
shp <- shapefile(file.path("C:/Users/mrm9534/Box/NU-malaria-team/data/covid_IDPH/shapefiles/EMS_Regions/EMS_Regions.shp"))

thresholds_wide <- read.csv("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/contact_tracing/20200627/20200627_IL_EMS_stopSIP10_isolationvsdetAsP_bydetSym/thresholds_wide.csv")
thresholds_wide$region <- as.numeric(gsub("EMS ","", thresholds_wide$region))


thresholds_wide <- thresholds_wide %>%
  group_by(region, grpvar) %>%
  mutate(fitmax = max(fit, na.rm = TRUE))


tempdat <- subset(thresholds_wide, fit == fitmax)
summary(tempdat$detection_success)
tapply(tempdat$detection_success, tempdat$grpvar, summary)
table(tempdat$grpvar, tempdat$region)




tempdat <- tempdat %>% group_by(region, grpvar) %>% summarize(fit=mean(fit))

tempdat$fit_fct = NA
tempdat$fit_fct[tempdat$fit >=0.75] = ">0.75"
tempdat$fit_fct[tempdat$fit <0.75] = "<0.75"
tempdat$fit_fct[tempdat$fit <0.5] = "<0.5"
tempdat$fit_fct[tempdat$fit <0.25] = "<0.25"
table(tempdat$fit_fct, exclude=NULL)


plot(shp)
shp_f <-  fortify(shp, id =REGION)
shp_f$region = as.numeric(shp_f$id)+1
shp_f <- left_join(shp_f,tempdat, by ="region" )

ggplot(data=subset(shp_f,grpvar==0.17 )) + 
  theme_map() + 
  geom_polygon(aes(x=long , y =lat, fill =fit_fct, group=region ), color = "black")  + 
  scale_fill_brewer(palette = "Greems")+
  labs(fill="Minimum detection coverage")
  #scale_fill_viridis(discrete = TRUE, option="magma")


