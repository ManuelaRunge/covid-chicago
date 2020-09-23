### ------------------------------------------------------------------------
#### R scriot to plot the time varying Ki parameter per EMS
### -----------------------------------------------------------------------

library(viridis)
library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")

### Options for plot theme
customThemeNoFacet <- theme(
  strip.text.x = element_text(size = 12, face = "bold"),
  strip.text.y = element_text(size = 12, face = "bold"),
  strip.background = element_blank(),
  plot.title = element_text(size = 16, vjust = -1, hjust = 0),
  plot.subtitle = element_text(size = 12),
  plot.caption = element_text(size = 12),
  legend.title = element_text(size = 12),
  legend.text = element_text(size = 12),
  axis.title.x = element_text(size = 12),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text.y = element_text(size = 12)
)


### Load trajectories Dat
exp_names <- c("20200722_IL_EMS_endsip10")


Ki_datAll <- list()
for(exp_name in exp_names){
Ki_dat <- read.csv(file.path(simulation_output, exp_name, "trajectoriesDat.csv"))
# Ki_dat <- read.csv(file.path(project_path, "NU_civis_outputs", "20200624", "trajectories", "trajectoriesDat_baseline.csv"))


### Note use Ki_EMS-  or Ki_EMS.   not Ki_EMS_  (time varying vs input parameter)
colnames(Ki_dat) <- gsub("[.]", "-", colnames(Ki_dat))
Kivars <- paste0("Ki_EMS-", c(1:11))
keepvars <- c("time", "startdate", Kivars)

### Wide to long format and calculate date
### Aand aggregate samples
Ki_dat <- Ki_dat %>%
  select(keepvars) %>%
  mutate(date = as.Date(startdate) + time) 
  
Ki_dat <- Ki_dat %>%  
  pivot_longer(cols = -c("time", "date", "startdate"), names_to = "region") %>%
  mutate(
    region = gsub("EMS-", "", gsub("Ki_", "", region)),
    region = as.numeric(region)
  ) %>%
  group_by(date, region) %>%
  summarize(value = mean(value))%>%
  mutate(expname = exp_name)


Ki_dat <- data.table(Ki_dat, key = c("region"))

Ki_dat[,tenpercincr := value[date>as.Date("2020-06-01") & date<=as.Date("2020-06-02")] + 
         (0.1 * 
            (value[date>as.Date("2020-02-14") & date<=as.Date("2020-02-15")] - 
             value[date>as.Date("2020-06-01") & date<=as.Date("2020-06-02")]) ) , by = c( "region" ) ]




Ki_datAll[[length(Ki_datAll)+1]] <- Ki_dat
}


Ki_dat <- bind_rows(Ki_datAll)
table(Ki_dat$expname)
summary(Ki_dat$tenpercincr)

Ki_dat$expname <- gsub("20200720_IL_test1_","",Ki_dat$expname)
Ki_dat$expname <- gsub("20200720_IL_test2_","",Ki_dat$expname)

### Plot
pplot <- ggplot(data = subset(Ki_dat, date >= as.Date("2020-06-01") &  date <= as.Date("2020-08-01"))) +
  theme_minimal() +
  geom_ribbon(aes(x = date, ymin=min(tenpercincr), ymax=max(tenpercincr)), fill="grey", alpha=0.4) + 
  #geom_hline(aes(yintercept=tenpercincr)) + 
  geom_line(aes(x = date, y = value, col = as.factor(region), group = region), size = 1.3) +
  scale_color_viridis(discrete = TRUE) +
  labs(color = "EMS", y = "Transmission rate") +
  customThemeNoFacet +
  labs(x = "") +
  facet_wrap(~expname)+
  scale_y_continuous(expand=c(0,0))
  # scale_x_continuous(expand=c(0,0))


ggsave(paste0("test1_Ki_timeline.pdf"),
       plot = pplot, path = file.path(simulation_output), width = 10, height = 5, device = "pdf"
)
ggsave(paste0("test1_Ki_timeline.png"),
       plot = pplot, path = file.path(simulation_output), width = 10, height = 5, device = "png"
)



### Plot
Ki_dat$regionLabel <- factor(Ki_dat$region, levels=c(1:11), labels=paste0("covid region ",c(1:11)))

emsLabel =c("covid region 1\n 4.7% increase" ,
            "covid region 2\n 10.1% increase", 
            "covid region 3\n 18.1% increase" , 
            "covid region 4\n 6.4% increase",
            "covid region 5\n 3.6% increase" ,
            "covid region 6\n 0% increase" ,
            "covid region 7\n 4.4% increase" ,
            "covid region 8\n 3.8% increase" ,
            "covid region 9\n 2.2% increase" ,
            "covid region 10\n 5.0% increase" ,
            "covid region 11\n 5.4% increase")
Ki_dat$regionLabel2 <- factor(Ki_dat$region, levels=c(1:11), labels=emsLabel)

pplot <- ggplot(data = subset( Ki_dat, date >= as.Date("2020-03-01") &  date <= as.Date("2020-07-01"))) +
  theme_minimal() +
  # geom_ribbon(aes(x = date, ymin=tenpercincr, ymax=tenpercincr), fill="grey", alpha=0.4) + 
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept=tenpercincr), color="grey", size=1, linetype="dashed") + 
  geom_line(aes(x = date, y = value, group = expname), size = 1) +
  scale_color_viridis(discrete = TRUE) +
  labs(color = "EMS", y = "Transmission rate") +
  customThemeNoFacet +
  labs(title="Estimated change in transmission per covid region",subtitle="\nWith estimated increase for 21st June 2020 shown in the facet title",x = "", caption="grey dashed line shows 10% increase from initial reduction in March-April") +
  facet_wrap(~regionLabel2, scales="free")+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin())
# scale_x_continuous(expand=c(0,0))


ggsave(paste0("test1_Ki_timeline_perEMS.pdf"),
       plot = pplot, path = file.path(simulation_output), width = 10, height = 8, device = "pdf"
)
ggsave(paste0("test1_Ki_timeline_perEMS.png"),
       plot = pplot, path = file.path(simulation_output), width = 10, height = 8, device = "png"
)


library(raster)
library(ggrepel)
shp <- shapefile(file.path(data_path, "covid_IDPH/shapefiles/covid_regions/covid_regions.shp"))
# plot(shp)
shp_f <- fortify(shp, id = new_restor)
#shp_f$region <- shp_f$region
shp_f$region <- as.numeric(factor(shp_f$id
                                  , levels = c(9, 8, 7, 6, 5, 4, 2, 1, 3, 10, 0), labels = c(1:11)))
shp_f <- left_join(shp_f, Ki_dat, by = "region")


shp_f2 <- shp_f %>% group_by(region) %>% summarize(long=mean(long), lat=mean(lat), value=round(mean(value*100),0))

pmap1 <- ggplot(data = subset(shp_f,date>=as.Date("2020-07-01") & date <=as.Date("2020-07-02"))) +
  geom_polygon(aes(x = long, y = lat, group = region), fill = "lightgrey", color = "black") +
  geom_polygon(aes(x = long, y = lat, fill = value*100, group = region), color = "black") +
  geom_text_repel(data=shp_f2, aes(x = long, y = lat, label=value, group = region), color = "black",size=5, face="bold") +
  scale_fill_distiller(palette = "RdYlBu") +
#scale_fill_gradient2(low = "#f7fcfd", high = "#542788") +
  #scale_fill_viridis(direction = -1) +
  labs(fill = "Rt") +
  theme_map() 
#  theme(legend.position = "bottom")


#Ki_dat %>% filter(date>=as.Date("2020-07-01") & date <=as.Date("2020-07-02"))%>% write.csv("Kidat.csv")


ggsave(paste0("test1_Ki_timeline_perEMS.pdf"),
       plot = pplot, path = file.path(simulation_output), width = 10, height = 8, device = "pdf"
)
ggsave(paste0("test1_Ki_timeline_perEMS.png"),
       plot = pplot, path = file.path(simulation_output), width = 10, height = 8, device = "png"
)
