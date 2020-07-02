## ============================================================
## Combine simulated experiments and make summary plot
## ============================================================

selected_outcome <- "critical"
### Load simulation data
exp_names <- list.dirs(file.path(ct_dir, simdate), recursive = FALSE, full.names = FALSE)

sim_dir <- file.path(ct_dir, simdate)

### 20%
tdat_wide0 <- read.csv(file.path(sim_dir, exp_names[5], "thresholds_loess.csv"))
tdat_wide1 <- read.csv(file.path(sim_dir, exp_names[6], "thresholds_loess.csv"))
tdat_wide2 <- read.csv(file.path(sim_dir, exp_names[13], "thresholds_loess.csv"))

### 10%
tdat_wide3 <- read.csv(file.path(sim_dir, exp_names[1], "thresholds_loess.csv"))
tdat_wide4 <- read.csv(file.path(sim_dir, exp_names[2], "thresholds_loess.csv"))
tdat_wide5 <- read.csv(file.path(sim_dir, exp_names[14], "thresholds_loess.csv"))
### 30%
tdat_wide6 <- read.csv(file.path(sim_dir, exp_names[7], "thresholds_loess.csv"))
tdat_wide7 <- read.csv(file.path(sim_dir, exp_names[8], "thresholds_loess.csv"))
tdat_wide8 <- read.csv(file.path(sim_dir, exp_names[15], "thresholds_loess.csv"))

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

minIsolation <- tdat_wide %>%
  group_by(region, sim, reopening) %>%
  mutate(minIsolation = min(isolation_success, na.rm = TRUE))

for (perc in c("10%", "20%", "30%")) {
  for (reduction in c("no reduction in test delay", "reduced test delay As, Sym", "reduced test delay Sym only")) {
    pplot <- ggplot(data = subset(tdat_wide, reopening == perc & sim == reduction)) +
      theme_minimal() +
      geom_line(aes(x = detection_success, y = isolation_success, col = as.factor(grpvar), group = grpvar), size = 1.3) +
      # geom_hline(data=subset(minIsolation, reopening==perc & sim ==reduction), aes(yintercept = minIsolation), linetype="dashed") +
      facet_wrap(~region) +
      scale_color_viridis(option = "viridis", discrete = TRUE) +
      customThemeNoFacet +
      scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
      scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 100, expand = c(0, 0)) +
      labs(
        x = detectionVar_label,
        y = isolationVar_label,
        col = groupVar_label
      ) +
      theme(panel.spacing = unit(1.5, "lines"))

    ggsave(paste0(selected_outcome, "_IL_thresholds_perEMS_loess_", gsub("%", "perc", perc), reduction, ".png"),
      plot = pplot, path = file.path(sim_dir), width = 12, height = 7, device = "png"
    )
    ggsave(paste0(selected_outcome, "_IL_thresholds_perEMS_loess_", gsub("%", "perc", perc), reduction, ".pdf"),
      plot = pplot, path = file.path(sim_dir), width = 12, height = 7, device = "pdf"
    )
  }
}


#### Extract minimum detection per region and aggregate


### add population for population weighting
pop = c(736370,1124941,619366,698886,417674,788985,1814891,1673408,1970275,1052839,2716921)
popdat <- cbind(region=c(1:11), pop) %>% as.data.frame()

tdat_wide <- tdat_wide %>%
  left_join(popdat, by="region") %>%
  group_by(region, grpvar, sim, reopening) %>%
  mutate(fitmax = max(isolation_success, na.rm = TRUE))

tempdat <- subset(tdat_wide, isolation_success == fitmax)
summary(tempdat$detection_success)
tapply(tempdat$detection_success, tempdat$grpvar, summary)
table(tempdat$grpvar, tempdat$region)

tdat_wideAggrIL <- tdat_wide %>% f_weighted.aggrDat(c("sim", "reopening"), "detection_success", "pop")

tdat_wideAggrIL2 <- tdat_wideAggrIL %>%
  pivot_wider(names_from = sim, values_from = c("min.val", "max.val", "mean.val", "median.val", "sd.val", "n.val", "q25", "q75", "q2.5", "q97.5", "se.val", "lower.ci.val", "upper.ci.val"))


tdat_wideAggrIL$sim <- factor(tdat_wideAggrIL$sim,
  levels = c("no reduction in test delay", "reduced test delay Sym only", "reduced test delay As, Sym"),
  labels = c("no reduction in test delay", "reduced test delay Sym only", "reduced test delay As, Sym")
)


pplot <- ggplot(data = tdat_wideAggrIL) +
  theme_minimal() +
  geom_pointrange(
    data = tdat_wideAggrIL, aes(x = reopening, y = median.val, ymin = q25, ymax = q75, shape = sim, col = sim, group = sim),
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
  scale_y_continuous(breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10))


ggsave(paste0(selected_outcome, "_IL_thresholds_summary_loess.png"),
  plot = pplot, path = file.path(sim_dir), width = 8, height = 5, device = "png"
)
ggsave(paste0(selected_outcome, "_IL_thresholds_summary_loess.pdf"),
  plot = pplot, path = file.path(sim_dir), width = 10, height = 5, device = "pdf"
)




####################
### aggregate regions  - population weighted           
tdat_wideAggrIL <- tdat_wide %>% f_weighted.aggrDat(c("sim", "reopening",  "grpvar"), "detection_success", "pop")
### aggregate grpvar 
tdat_wideAggrIL <- tdat_wideAggrIL %>% f_aggrDat(c("sim", "reopening"), "mean.val")

tdat_wideAggrIL2 <- tdat_wideAggrIL %>%
  pivot_wider(names_from = sim, values_from = c("min.val", "max.val", "mean.val", "median.val", "sd.val", "n.val", "q25", "q75", "q2.5", "q97.5", "se.val", "lower.ci.val", "upper.ci.val"))


tdat_wideAggrIL$sim <- factor(tdat_wideAggrIL$sim,
  levels = c("no reduction in test delay", "reduced test delay Sym only", "reduced test delay As, Sym"),
  labels = c("no reduction in test delay", "reduced test delay Sym only", "reduced test delay As, Sym")
)


pplot <- ggplot(data = tdat_wideAggrIL) +
  theme_minimal() +
  geom_pointrange(
    data = tdat_wideAggrIL, aes(x = reopening, y = mean.val, ymin = min.val, ymax = max.val, shape = sim, col = sim, group = sim),
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
  scale_y_continuous(breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10))

ggsave(paste0(selected_outcome, "_IL_thresholds_summary4_loess.png"),
  plot = pplot, path = file.path(sim_dir), width = 8, height = 5, device = "png"
)
ggsave(paste0(selected_outcome, "_IL_thresholds_summary4_loess.pdf"),
  plot = pplot, path = file.path(sim_dir), width = 10, height = 5, device = "pdf"
)
