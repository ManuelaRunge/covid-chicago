
library(tidyverse)
library(cowplot)
library(scales)
library(viridis)

source("load_paths.R")
source("processing_helpers.R")


### Load trajectories Dat
### Load trajectories Dat

exp_name1 <- "20200827_IL_test_delayedTrigger3"
exp_name2 <- "20200827_IL_test_Trigger2"



trajectoriesDat1 <- read_csv(file.path(simulation_output, exp_name1, "trajectoriesDat.csv"))
trajectoriesDat2 <- read_csv(file.path(simulation_output, exp_name2, "trajectoriesDat.csv"))

trajectoriesDat1$trigger <- "delayed"
trajectoriesDat2$trigger <- "immediate"

trajectoriesDat <- rbind(trajectoriesDat1, trajectoriesDat2)


paramvars <- paste0(paramname, emsname, c(1:11))
keepvars <- c("time", "startdate", "trigger", paramvars)

pplot <- trajectoriesDat %>%
  filter(scen_num == 1) %>%
  select(keepvars) %>%
  mutate(date = as.Date(startdate) + time) %>%
  pivot_longer(cols = -c("time", "date", "startdate", "trigger"), names_to = "region") %>%
  mutate(
    region = gsub(emsname, "", gsub(paramname, "", region)),
    region = as.numeric(region),
    exp_name = exp_name,
  ) %>%
  group_by(date, region, exp_name, trigger) %>%
  summarize(value = mean(value)) %>%
  filter(date >= as.Date("2020-07-02")) %>%
  ggplot() +
  theme_cowplot() +
  geom_line(aes(x = date, y = value, col = as.factor(trigger), group = region), size = 1.3) +
  scale_color_viridis(discrete = TRUE) +
  labs(
    y = gsub("_", " ", paramname),
    subtitle = "", # Estimated change in transmission intensity\n
    title = "",
    x = "",
    col = "covid region"
  ) +
  customThemeNoFacet +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~region) +
  geom_vline(xintercept = as.Date("2020-08-27"))
