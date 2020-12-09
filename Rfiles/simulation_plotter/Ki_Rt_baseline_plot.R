###---------------------------------------------------------------------------------------------------------------------
### Rscript to compare fitted Ki with estimated Rt values at the beginning of the epidemic in March 2020 per EMS 
###---------------------------------------------------------------------------------------------------------------------

## Load packages
packages_needed <- c("tidyverse", "cowplot",  "viridis", "ggthemes", "raster")
lapply(packages_needed, require, character.only = TRUE)

source("load_paths.R")
source("processing_helpers.R")

shp <- shapefile(file.path(data_path, "covid_IDPH/shapefiles/EMS_Regions/EMS_Regions.shp"))
# plot(shp)

Rtdat <- read.csv(file.path(project_path, "NU_civis_outputs", "20200624", "csv", "nu_il_baseline_estimated_Rt_20200624.csv"))
Rtdat <- Rtdat[grep("ems", Rtdat$geography_modeled), ]
Rtdat <- Rtdat %>%
  mutate(region = as.numeric(gsub("ems", "", geography_modeled))) %>%
  filter(Date == as.Date("2020-03-07"))

### Compare Ki and Rt
Ki_dat <- read.csv(file.path(project_path, "NU_civis_outputs", "20200624", "trajectories", "trajectoriesDat_baseline.csv"))

### Get input Ki_EMS_  (time varying vs input parameter)
Kivars <- paste0("Ki_EMS_", c(1:11))
keepvars <- c("time", Kivars)

Ki_dat <- Ki_dat %>%
  filter(time == 0 & sample_num == 0) %>%
  dplyr::select(keepvars) %>%
  pivot_longer(cols = -c("time"), names_to = "region", values_to = "Ki") %>%
  dplyr::mutate(
    region = gsub("EMS_", "", gsub("Ki_", "", region)),
    region = as.numeric(region)
  )


dat <- left_join(Rtdat, Ki_dat, by = "region")

pscatter <- ggplot(data = dat, aes(x = Ki, y = Median.of.covid.19.Rt, label = region)) +
  theme_minimal() +
  geom_point(size = 2) +
  # geom_errorbar(aes(ymin = Lower.error.bound.of.covid.19.Rt, ymax=Upper.error.bound.of.covid.19.Rt )) +
  geom_text(hjust = 0.5, vjust = -1) +
  labs(x = "initial Ki", y = expression(initial * " " * R[t])) +
  customThemeNoFacet

ggsave(paste0("baseline_Ki_Rt_scatter.png"),
  plot = pscatter, path = file.path(simulation_output), width = 6, height = 6, device = "png"
)

ggsave(paste0("baseline_Ki_Rt_scatter.pdf"),
  plot = pscatter, path = file.path(simulation_output), width = 6, height = 6, device = "pdf"
)


## Combine with shapefile - spatial dataframe
## ID is not the regions in correct order !!
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



### Ki map
pmap2 <- ggplot(data = shp_f) +
  geom_polygon(aes(x = long, y = lat, group = region), fill = "lightgrey", color = "black") +
  geom_polygon(aes(x = long, y = lat, fill = Ki, group = region), color = "black") +
  # scale_fill_gradient2(low = "#f7fcfd", high = "#542788") +
  scale_fill_viridis(direction = -1) +
  labs(fill = "Ki") +
  theme_map() +
  theme(legend.position = "bottom")


pmap <- plot_grid(pmap1, pmap2, nrow = 1)

ggsave(paste0("baseline_Ki_Rt_maps.png"),
  plot = pmap, path = file.path(simulation_output), width = 10, height = 8, device = "png"
)

ggsave(paste0("baseline_Ki_Rt_maps.pdf"),
  plot = pmap, path = file.path(simulation_output), width = 10, height = 8, device = "pdf"
)
