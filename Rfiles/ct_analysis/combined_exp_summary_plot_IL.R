## ============================================================
## Combine simulated experiments and make summary plot
## Script currently specific for evaluating varying test delay options for 3 different reopening levels

## Test delay
#- "no reduction in test delay"
#- "reduced test delay As, Sym"
#- reduced test delay Sym only"

## Reopening
# 10%, 20% and 30%
## ============================================================

# selected_outcome <- "Rt" # critical"
### Load simulation data
exp_names <- list.dirs(file.path(ct_dir, simdate), recursive = FALSE, full.names = FALSE)
sim_dir <- file.path(ct_dir, simdate)

if (selected_outcome == "critical") csvfilename <- "thresholds_loess.csv"
if (selected_outcome == "Rt") csvfilename <- "Rt_thresholds_loess.csv"

### 20%
tdat_wide0 <- read.csv(file.path(sim_dir, exp_names[5], csvfilename))
tdat_wide1 <- read.csv(file.path(sim_dir, exp_names[6], csvfilename))
tdat_wide2 <- read.csv(file.path(sim_dir, exp_names[13], csvfilename))

### 10%
tdat_wide3 <- read.csv(file.path(sim_dir, exp_names[1], csvfilename))
tdat_wide4 <- read.csv(file.path(sim_dir, exp_names[2], csvfilename))
tdat_wide5 <- read.csv(file.path(sim_dir, exp_names[14], csvfilename))
### 30%
tdat_wide6 <- read.csv(file.path(sim_dir, exp_names[7], csvfilename))
tdat_wide7 <- read.csv(file.path(sim_dir, exp_names[8], csvfilename))
tdat_wide8 <- read.csv(file.path(sim_dir, exp_names[15], csvfilename))

tdat_wide0$sim <- "no reduction in test delay"
tdat_wide1$sim <- "reduced test delay As, Sym"
tdat_wide2$sim <- "reduced test delay Sym only"

tdat_wide3$sim <- "no reduction in test delay"
tdat_wide4$sim <- "reduced test delay As, Sym"
tdat_wide5$sim <- "reduced test delay Sym only"

tdat_wide6$sim <- "no reduction in test delay"
tdat_wide7$sim <- "reduced test delay As, Sym"
tdat_wide8$sim <- "reduced test delay Sym only"

#### reopening
tdat_wide0$reopening <- "20%"
tdat_wide1$reopening <- "20%"
tdat_wide2$reopening <- "20%"

tdat_wide3$reopening <- "10%"
tdat_wide4$reopening <- "10%"
tdat_wide5$reopening <- "10%"

tdat_wide6$reopening <- "30%"
tdat_wide7$reopening <- "30%"
tdat_wide8$reopening <- "30%"


tdat_wide <- rbind(tdat_wide0, tdat_wide1, tdat_wide2, tdat_wide3, tdat_wide4, tdat_wide5, tdat_wide6, tdat_wide7, tdat_wide8)
rm(tdat_wide0, tdat_wide1, tdat_wide2, tdat_wide3, tdat_wide4, tdat_wide5, tdat_wide6)

tdat_wide$region <- as.numeric(gsub(".csv", "", tdat_wide$region))


for (perc in unique(tdat_wide$reopening)) {
  for (reduction in unique(tdat_wide$sim)) {
    # perc <- "10%"
    # reduction <- "no reduction in test delay"

    pplot <- ggplot(data = subset(tdat_wide, reopening == perc & sim == reduction)) +
      theme_cowplot() +
      geom_ribbon(aes(
        x = detection_success,
        xmin = detection_success, xmax = 1,
        ymin = isolation_success, ymax = 1, fill = as.factor(grpvar), group = grpvar
      ), alpha = 0.5
      ) +
      geom_ribbon(
        data = subset(tdat_wide, reopening == perc & sim == reduction & grpvar == 0.585),
        aes(
          x = detection_success,
          xmin = detection_success, xmax = 1,
          ymin = isolation_success, ymax = 1
        ),
        fill = "white"
      ) +
      geom_ribbon(
        data = subset(tdat_wide, reopening == perc & sim == reduction & grpvar == 0.585), aes(
          x = detection_success,
          xmin = detection_success, xmax = 1,
          ymin = isolation_success, ymax = 1, fill = as.factor(grpvar), group = grpvar
        ), alpha = 0.5
      ) +
      geom_hline(yintercept = seq(0, 1, 0.1), alpha = 0.3, col = "lightgrey", size = 0.3) +
      geom_vline(xintercept = seq(0, 1, 0.1), alpha = 0.3, col = "lightgrey", size = 0.3) +
      geom_ribbon(
        data = subset(tdat_wide, reopening == perc & sim == reduction & grpvar == 0.17),
        aes(
          x = detection_success,
          xmin = detection_success, xmax = 1,
          ymin = isolation_success, ymax = 1
        ),
        fill = "white"
      ) +
      geom_ribbon(
        data = subset(tdat_wide, reopening == perc & sim == reduction & grpvar == 0.17), aes(
          x = detection_success,
          xmin = detection_success, xmax = 1,
          ymin = isolation_success, ymax = 1, fill = as.factor(grpvar), group = grpvar
        ), alpha = 0.5
      ) +
      geom_line(aes(x = detection_success, y = isolation_success, col = as.factor(grpvar), group = grpvar), size = 0.7) +
      facet_wrap(~region) +
      scale_color_viridis(option = "viridis", discrete = TRUE) +
      scale_fill_viridis(option = "viridis", discrete = TRUE) +
      customThemeNoFacet +
      scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
      scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
      labs(
        x = detectionVar_label,
        y = isolationVar_label,
        fill = groupVar_label,
        col = groupVar_label
      ) +
      theme(panel.spacing = unit(1, "lines")) +
      geom_hline(yintercept = c(-Inf, Inf)) +
      geom_vline(xintercept = c(-Inf, Inf))

    ggsave(paste0(selected_outcome, "_IL_thresholds_perEMS_loess_", gsub("%", "perc", perc), reduction, ".png"),
      plot = pplot, path = file.path(sim_dir), width = 10, height = 5, device = "png"
    )
    ggsave(paste0(selected_outcome, "_IL_thresholds_perEMS_loess_", gsub("%", "perc", perc), reduction, ".pdf"),
      plot = pplot, path = file.path(sim_dir), width = 10, height = 5, device = "pdf"
    )
  }
}


#### Extract minimum detection per region and aggregate


### add population for population weighting
pop <- c(736370, 1124941, 619366, 698886, 417674, 788985, 1814891, 1673408, 1970275, 1052839, 2716921)
popdat <- cbind(region = c(1:11), pop) %>% as.data.frame()

tdat_wide <- tdat_wide %>%
  left_join(popdat, by = "region") %>%
  group_by(region, grpvar, sim, reopening) %>%
  mutate(fitmax = max(isolation_success, na.rm = TRUE))


### Generate map 
if(generateMap){
  library(raster)
  library(ggthemes)
  
  shp <- shapefile(file.path(data_path, "covid_IDPH/shapefiles/EMS_Regions/EMS_Regions.shp"))
  # plot(shp)
  
  perc = "10%"
  reduction = c( "reduced test delay As, Sym" , "no reduction in test delay" ) # 
  grp = 0.17
  
  ### filter 
  dat <- tdat_wide %>%
    filter(isolation_success == fitmax)  %>%
    filter(reopening ==perc & sim  %in% reduction & grpvar ==grp)
  
  ## Combine with shapefile - spatial dataframe
  ## ID is not the regions in correct order !!
  shp_f <- fortify(shp, id = REGION)
  shp_f$region <- as.numeric(factor(shp_f$id , levels=c(9, 8, 7, 6, 5, 4,2, 1, 3,10, 0), labels=c(1:11)))
  shp_f <- left_join(shp_f, dat, by = "region")
  
  
  pmap <- ggplot(data=shp_f) +
    geom_polygon(aes(x = long, y = lat, group = region), fill = "lightgrey", color = "black") +
    geom_polygon(aes(x = long, y = lat, fill = detection_success, group = region), color = "black") +
    scale_fill_gradient2(low = "#f7fcfd", high = "#542788") +
    labs(fill = "Minimum detection coverage") +
    facet_wrap(~ sim) +
    customThemeNoFacet +
    theme_map() +
    theme(legend.position = "right")
  
  ggsave(paste0("IL_thresholds_map",gsub("%","perc",perc),"_", grp,".png"),
         plot = pmap, path = file.path(sim_dir,"maps"), width = 11, height = 8, device = "png"
  )
  
  ggsave(paste0("IL_thresholds_map",gsub("%","perc",perc),"_", grp,".pdf"),
         plot = pmap, path = file.path(sim_dir,"maps"), width = 11, height = 8, device = "pdf"
  )
  
}


## Select maximum isolation, for mnimum detection
### Aggregate data using custom weighting function
require(spatstat)
# tdat_wideAggrIL <- tdat_wide %>%
#   filter(isolation_success == fitmax) %>%
#   f_weighted.aggrDat(c("sim", "reopening", "grpvar"), "detection_success", "pop")

tdat_wideAggrIL <- tdat_wide %>%
   filter(isolation_success == fitmax) %>%
   group_by(sim, reopening, grpvar) %>% 
  summarize(mean.val=weighted.mean(detection_success, pop)) %>%
  group_by(sim, reopening) %>% 
  summarize(min.val=min(mean.val),
            max.val=max(mean.val),
            mean.val=mean(mean.val))

tdat_wideAggrIL$sim <- factor(tdat_wideAggrIL$sim,
  levels = c("no reduction in test delay", "reduced test delay Sym only", "reduced test delay As, Sym"),
  labels = c("no reduction in test delay", "reduced test delay Sym only", "reduced test delay As, Sym")
)


pplot <- ggplot(data = tdat_wideAggrIL) +
  theme_minimal() +
  geom_pointrange(aes(x = reopening, y = mean.val, ymin = min.val, ymax = max.val, shape = sim, col = sim, group = sim),
    size = 1, position = position_dodge(0.3)
  ) +
  labs(
    title = "",
    subtitle = "",
    x = "\nPartial reopening scenario",
    y = "Minimum detection of a- and presymptomatics\nto prevent exceeding ICU capacities\n",
    col = "",
    shape = ""
  ) +
  scale_color_manual(values = c("deepskyblue3", "orange", "mediumvioletred")) +
  customThemeNoFacet +
  scale_y_continuous(lim=c(0,1), breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10), expand=c(0,0)) +
  theme( panel.grid.major.x = element_blank() )


ggsave(paste0(selected_outcome, "_IL_thresholds_summary_loess.png"),
  plot = pplot, path = file.path(sim_dir), width = 8, height = 5, device = "png"
)
ggsave(paste0(selected_outcome, "_IL_thresholds_summary_loess.pdf"),
  plot = pplot, path = file.path(sim_dir), width = 10, height = 5, device = "pdf"
)


### Comparison plot
savefilename <- paste0(selected_outcome, "_aggregatedMinThresholds.csv")
write.csv(tdat_wideAggrIL, file.path(sim_dir, savefilename), row.names = FALSE)


###
if (compareOutcomes) {
  dat1 <- read.csv(file.path(sim_dir, paste0("Rt", "_aggregatedMinThresholds.csv")))
  dat2 <- read.csv(file.path(sim_dir, paste0("critical", "_aggregatedMinThresholds.csv")))

  dat1$outcome <- "Rt"
  dat2$outcome <- "critical"
  dat <- rbind(dat1, dat2)

  dat$sim <- factor(dat$sim,
    levels = c("no reduction in test delay", "reduced test delay Sym only", "reduced test delay As, Sym"),
    labels = c("no reduction in test delay", "reduced test delay Sym only", "reduced test delay As, Sym")
  )


  pplot <- ggplot(data = dat) +
    theme_minimal() +
    geom_pointrange(aes(x = reopening, y = mean.val, ymin = min.val, ymax = max.val, shape = outcome, col = sim, group = sim),
      size = 1, position = position_dodge(0.3)
    ) +
    labs(
      title = "",
      subtitle = "",
      x = "\nPartial reopening scenario",
      y = "Minimum detection of a- and presymptomatics\nto prevent exceeding ICU capacities\n",
      col = "",
      shape = ""
    ) +
    scale_color_manual(values = c("deepskyblue3", "orange", "mediumvioletred")) +
    customThemeNoFacet +
    scale_y_continuous(lim=c(0,1), breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10), expand=c(0,0)) +
    theme( panel.grid.major.x = element_blank() )


  ggsave(paste0("IL_thresholds_loess.png"),
    plot = pplot, path = file.path(sim_dir), width = 8, height = 5, device = "png"
  )
  ggsave(paste0("IL_thresholds_loess.pdf"),
    plot = pplot, path = file.path(sim_dir), width = 10, height = 5, device = "pdf"
  )
}


#c("deepskyblue3", "orange", "mediumvioletred")
simcols = c("no reduction in test delay"="deepskyblue3",  "reduced test delay As, Sym"="orange", "reduced test delay Sym only"="mediumvioletred")

p1 <- ggplot(data=dat) +
  theme_minimal() +
  geom_pointrange(aes(x = reopening, y = mean.val, ymin = min.val, ymax = max.val, group =interaction(outcome,sim)),
                  size = 1, position = position_dodge(0.7), col ="white",show.legend=FALSE
  ) +
  geom_pointrange(data = subset(dat, outcome=="critical" & sim  %in% c("no reduction in test delay")),
                  aes(x = reopening, y = mean.val, ymin = min.val, ymax = max.val, shape = outcome, col = sim, group = sim),
                  size = 1, position = position_dodge(0.7)
  ) +
  labs(
    title = "",
    subtitle = "",
    x = "\nPartial reopening scenario",
    y = "Minimum detection of As andP\nto prevent exceeding ICU capacities\n",
    col = "",
    shape = ""
  ) +
  scale_color_manual(values = simcols) +
  customThemeNoFacet +
  scale_y_continuous(lim=c(0,1), breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10), expand=c(0,0)) +
  theme( panel.grid.major.x = element_blank() , legend.position = "none") 


ggsave(paste0("p1.png"),
       plot = p1, path = file.path(sim_dir), width = 8, height = 5, device = "png"
)



p2 <- ggplot(data=dat) +
  theme_minimal() +
  geom_pointrange(aes(x = reopening, y = mean.val, ymin = min.val, ymax = max.val, group =interaction(outcome,sim)),
                  size = 1, position = position_dodge(0.7), col ="white",show.legend=FALSE
  ) +
  geom_pointrange(data = subset(dat, outcome=="critical" & sim  %in% c("no reduction in test delay",  "reduced test delay Sym only")),
                  aes(x = reopening, y = mean.val, ymin = min.val, ymax = max.val, shape = outcome, col = sim, group = sim),
                  size = 1, position = position_dodge(0.7)
  ) +
  labs(
    title = "",
    subtitle = "",
    x = "\nPartial reopening scenario",
    y = "Minimum detection of As andP\nto prevent exceeding ICU capacities\n",
    col = "",
    shape = ""
  ) +
  scale_color_manual(values = simcols) +
  customThemeNoFacet +
  scale_y_continuous(lim=c(0,1), breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10), expand=c(0,0)) +
  theme( panel.grid.major.x = element_blank() , legend.position = "none") 


ggsave(paste0("p2.png"),
       plot = p2, path = file.path(sim_dir), width = 8, height = 5, device = "png"
)



p3 <- ggplot(data=dat) +
  theme_minimal() +
  geom_pointrange(aes(x = reopening, y = mean.val, ymin = min.val, ymax = max.val, group =interaction(outcome,sim)),
                  size = 1, position = position_dodge(0.7), col ="white",show.legend=FALSE
  ) +
  geom_pointrange(data = subset(dat, outcome=="critical" ),
                  aes(x = reopening, y = mean.val, ymin = min.val, ymax = max.val, shape = outcome, col = sim, group = sim),
                  size = 1, position = position_dodge(0.7)
  ) +
  labs(
    title = "",
    subtitle = "",
    x = "\nPartial reopening scenario",
    y = "Minimum detection of As andP\nto prevent exceeding ICU capacities\n",
    col = "",
    shape = ""
  ) +
  scale_color_manual(values = simcols) +
  customThemeNoFacet +
  scale_y_continuous(lim=c(0,1), breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10), expand=c(0,0)) +
  theme( panel.grid.major.x = element_blank() , legend.position = "none") 


ggsave(paste0("p3.png"),
       plot = p3, path = file.path(sim_dir), width = 8, height = 5, device = "png"
)


p4 <- ggplot(data=dat) +
  theme_minimal() +
  geom_pointrange(aes(x = reopening, y = mean.val, ymin = min.val, ymax = max.val, group =interaction(outcome,sim)),
                  size = 1, position = position_dodge(0.7), col ="white",show.legend=FALSE
  ) +
  geom_pointrange(data = subset(dat ),
                  aes(x = reopening, y = mean.val, ymin = min.val, ymax = max.val, shape = outcome, col = sim, group = sim),
                  size = 1, position = position_dodge(0.7)
  ) +
  labs(
    title = "",
    subtitle = "",
    x = "\nPartial reopening scenario",
    y = "Minimum detection of As andP\nto prevent exceeding ICU capacities\n",
    col = "",
    shape = ""
  ) +
  scale_color_manual(values = simcols) +
  customThemeNoFacet +
  scale_y_continuous(lim=c(0,1), breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10), expand=c(0,0)) +
  theme( panel.grid.major.x = element_blank() , legend.position = "none") 


ggsave(paste0("p4.png"),
       plot = p4, path = file.path(sim_dir), width = 8, height = 5, device = "png"
)
