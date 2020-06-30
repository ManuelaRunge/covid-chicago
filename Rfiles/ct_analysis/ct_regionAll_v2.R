

##--------------------------------------------
#### Summary plot of thresholds per region
##--------------------------------------------

### Load thresholds from lm model
thresholdsfiles <- list.files(file.path(exp_dir), pattern = "thresholds", recursive = TRUE, full.names = TRUE)
thresholdsfiles <- thresholdsfiles[grep(geography, thresholdsfiles)]
thresholdsfiles <- thresholdsfiles[grep("lm", thresholdsfiles)]

lmthresholdsDat <- sapply(thresholdsfiles, read.csv, simplify = FALSE) %>%
  bind_rows(.id = "id") %>%
  dplyr::mutate(
    region = as.character(region),
    outcome = as.character(outcome)
  ) 

lmthresholdsDat <- region_to_fct(lmthresholdsDat, geography)

### Load thresholds from predictions
thresholdsfiles <- list.files(file.path(exp_dir), pattern = "thresholds", recursive = TRUE, full.names = TRUE)
thresholdsfiles <- thresholdsfiles[grep(geography, thresholdsfiles)]
thresholdsfiles <- thresholdsfiles[!grepl("lm", thresholdsfiles)]

thresholdsDat <- sapply(thresholdsfiles, read.csv, simplify = FALSE) %>%
  bind_rows(.id = "id") %>%
  mutate(
    region = as.character(region),
    outcome = as.character(outcome),
    outcome = factor(outcome,
                     levels = c("hospitalized", "critical", "ventilators", "deaths"),
                     labels = c("hospitalized", "critical", "ventilators", "deaths")
    ),
    capacityLabel = paste0("Capacity: ", capacity)
  )

thresholdsDat <- region_to_fct(thresholdsDat, geography)

table(thresholdsDat$region)
table(thresholdsDat$region, thresholdsDat$detection_success)

capacityText <- thresholdsDat %>%
  select(region, outcome, capacityLabel) %>%
  unique()


lmthresholdsDat_wide = lmthresholdsDat %>% select(-X) %>% filter(detection_success!= 0  & isolation_success!= 0) %>%
  pivot_wider(names_from = statistic, values_from  =isolation_success ) %>%
  mutate(upr2 = ifelse(is.na(upr), 1, upr))


lmthresholdsDat <- subset(lmthresholdsDat,  (detection_success!= 0  & isolation_success!= 0 ))


write.csv(lmthresholdsDat_wide, file.path(exp_dir, "thresholds_wide.csv"), row.names = FALSE)

# #### Assemble polygon to fill boundary lines
# xpol <- c(lmthresholdsDat_wide$fit, rev(lmthresholdsDat_wide$fit))
# ypol <- c(lmthresholdsDat_wide$lwr, rev(lmthresholdsDat_wide$upr2))
# grpvar <- c(lmthresholdsDat_wide$grpvar, rev(lmthresholdsDat_wide$grpvar))
# region <- c(lmthresholdsDat_wide$region, rev(lmthresholdsDat_wide$region))
# 
# datpol <- as.data.frame(cbind(xpol, ypol, grpvar, region))
# 
# datpol$xpol <- as.numeric(datpol$xpol)
# datpol$ypol <- as.numeric(datpol$ypol)
# datpol$grpvar <- as.numeric(datpol$grpvar)
# 
# datpol$region <- factor(datpol$region, levels = c(1:11), labels = paste0("EMS ", c(1:11)))
# 
# 
#lmthresholdsDat_wide$detection_success <- as.numeric(lmthresholdsDat_wide$detection_success)
# lmthresholdsDat$detection_success <- as.numeric(lmthresholdsDat$detection_success)
# 
# lmthresholdsDat_wide$detection_success <- as.numeric(lmthresholdsDat_wide$detection_success)
# lmthresholdsDat$detection_success <- as.numeric(lmthresholdsDat$detection_success)


pplot <- ggplot(data = subset(lmthresholdsDat)) + theme_bw() +
  #geom_polygon(data = datpol, aes(x = xpol, y = ypol, fill = as.factor(grpvar)), alpha = 0.5) +
  geom_ribbon(data=lmthresholdsDat_wide, aes(x=detection_success , ymin=lwr , ymax=upr2, fill=as.factor(grpvar), group=interaction(grpvar)), alpha=0.3) + 
  geom_line(data = subset(lmthresholdsDat, statistic== "fit" ), 
            aes(x = detection_success, y = isolation_success, col=as.factor(grpvar), group=interaction(grpvar,statistic)), size = 1) +
  geom_line(data =subset(lmthresholdsDat, statistic!= "fit"  ), 
            aes(x = detection_success, y = isolation_success,  col=as.factor(grpvar), group=interaction(grpvar,statistic)), size = 1, linetype = "dashed") +  
  facet_wrap( ~ region) +
  labs(
    color = groupVar_label,
    fill = groupVar_label,
    x = "detection of As, P (%)",
    y = "isolation success of As, P (%)",
    caption = ""
  ) +
  scale_fill_viridis(discrete = TRUE, direction = -1) + 
  scale_color_viridis(discrete = TRUE, direction = -1) + 
  customThemeNoFacet +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_x_continuous(labels = seq(0, 1, 0.2)*100, breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(labels = seq(0, 1, 0.2)*100, breaks = seq(0, 1, 0.2))


ggsave(paste0( "all_capacity_thresholds_3.png"),
       plot = pplot, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
)

pplot <- ggplot(data = subset(lmthresholdsDat)) + theme_bw() +
  #geom_polygon(data = datpol, aes(x = xpol, y = ypol, fill = as.factor(grpvar)), alpha = 0.5) +
  #geom_ribbon(data=lmthresholdsDat_wide, aes(x=detection_success , ymin=lwr , ymax=upr2, fill=as.factor(grpvar), group=interaction(grpvar)), alpha=0.3) + 
  geom_line(data = subset(lmthresholdsDat, statistic== "fit" ), 
            aes(x = detection_success, y = isolation_success, col=as.factor(grpvar), group=interaction(grpvar,statistic)), size = 1) +
  #geom_line(data =subset(lmthresholdsDat, statistic!= "fit"  ), 
  #          aes(x = detection_success, y = isolation_success,  col=as.factor(grpvar), group=interaction(grpvar,statistic)), size = 1, linetype = "dashed") +  
  facet_wrap( ~ region) +
  labs(
    color = groupVar_label,
    fill = groupVar_label,
    x = "detection of As, P (%)",
    y = "isolation success of As, P (%)",
    caption = ""
  ) +
  scale_fill_viridis(discrete = TRUE, direction = -1) + 
  scale_color_viridis(discrete = TRUE, direction = -1) + 
  customThemeNoFacet +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_x_continuous(labels = seq(0, 1, 0.2)*100, breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(labels = seq(0, 1, 0.2)*100, breaks = seq(0, 1, 0.2))


ggsave(paste0( "all_capacity_thresholds_4.png"),
       plot = pplot, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
)
