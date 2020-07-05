## ============================================================
## R script to generate a simple chloropleth map of minimum tresholds per EMS
## Script included via source in contactTracing_master.R
## To run as a standalone script, directories need to be specified separately
## ============================================================

# library(tidyverse)
# library(viridis)
library(raster)
library(ggthemes)

shp <- shapefile(file.path(data_path, "covid_IDPH/shapefiles/EMS_Regions/EMS_Regions.shp"))
# plot(shp)

# exp_dir <- file.path(ct_dir, simdate, "20200627_IL_EMS_stopSIP10_isolationvsdetAsP_bydetSym_lowerTD")
dat <- read.csv(file.path(exp_dir, "thresholds_loess.csv"))
dat$region <- as.numeric(gsub("EMS ", "", dat$region))


dat <- dat %>%
  group_by(region, grpvar) %>%
  mutate(isomax = max(isolation_success, na.rm = TRUE)) %>%
  filter(isolation_success == isomax) %>%
  group_by(region, grpvar) %>%
  summarize(detection_success = mean(detection_success, na.rm = TRUE))

dat$detection_success_fct <- NA
dat$detection_success_fct[dat$detection_success >= 0.75] <- ">0.75"
dat$detection_success_fct[dat$detection_success < 0.75] <- "<0.75"
dat$detection_success_fct[dat$detection_success < 0.5] <- "<0.5"
dat$detection_success_fct[dat$detection_success < 0.25] <- "<0.25"
table(dat$detection_success_fct, exclude = NULL)


## Combine with shapefile - spatial dataframe
shp_f <- fortify(shp, id = REGION)
shp_f$region <- as.numeric(shp_f$id) + 1
shp_f <- left_join(shp_f, dat, by = "region")

pmap <- ggplot() +
  theme_map() +
  geom_polygon(data = shp_f, aes(x = long, y = lat, group = region), fill = "lightgrey", color = "black") +
  geom_polygon(data = subset(shp_f, grpvar == 0.17), aes(x = long, y = lat, fill = detection_success, group = region), color = "black") +
  # scale_fill_distiller(palette = "BuPu") +
  scale_fill_gradient2(low = "#f7fcfd", high = "#542788") +
  labs(fill = "Minimum detection coverage")
# scale_fill_viridis(discrete = TRUE, option="magma")

ggsave(paste0("IL_thresholds_map.png"),
  plot = pmap, path = file.path(exp_dir), width = 5, height = 8, device = "png"
)

ggsave(paste0("IL_thresholds_map.pdf"),
  plot = pmap, path = file.path(exp_dir), width = 5, height = 8, device = "pdf"
)
