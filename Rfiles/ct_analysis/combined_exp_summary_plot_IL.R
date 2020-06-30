## ============================================================
## Combine simulated experiments and make summary plot
## ============================================================
selected_outcome="critical"
### Load simulation data
exp_names <- list.dirs(file.path(ct_dir, simdate), recursive = FALSE, full.names = FALSE)

sim_dir <- file.path(ct_dir, simdate)

### 20%
tdat_wide0 <- read.csv(file.path(sim_dir, exp_names[5], "thresholds_wide.csv"))
tdat_wide1 <- read.csv(file.path(sim_dir, exp_names[6], "thresholds_wide.csv"))
tdat_wide2 <- read.csv(file.path(sim_dir, exp_names[13], "thresholds_wide.csv"))

### 10%
tdat_wide3 <- read.csv(file.path(sim_dir, exp_names[1], "thresholds_wide.csv"))
tdat_wide4 <- read.csv(file.path(sim_dir, exp_names[2], "thresholds_wide.csv"))
tdat_wide5 <- read.csv(file.path(sim_dir, exp_names[14], "thresholds_wide.csv"))
### 30%
tdat_wide6 <- read.csv(file.path(sim_dir, exp_names[7], "thresholds_wide.csv"))
tdat_wide7 <- read.csv(file.path(sim_dir, exp_names[8], "thresholds_wide.csv"))
tdat_wide8 <- read.csv(file.path(sim_dir, exp_names[15], "thresholds_wide.csv"))

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


tdat_wide <- rbind(tdat_wide0 , tdat_wide1, tdat_wide2, tdat_wide3 , tdat_wide4, tdat_wide5, tdat_wide6, tdat_wide7, tdat_wide8)
rm(tdat_wide0 , tdat_wide1, tdat_wide2, tdat_wide3 , tdat_wide4, tdat_wide5, tdat_wide6)


tdat_wide <- tdat_wide %>%
  group_by(region, grpvar,sim) %>%
  mutate(fitmax = max(fit, na.rm = TRUE))


tempdat <- subset(tdat_wide, fit == fitmax)
summary(tempdat$x)
tapply(tempdat$x, tempdat$grpvar, summary)
table(tempdat$grpvar, tempdat$region)


tdat_wideAggrIL <- tdat_wide %>%  f_aggrDat(c("sim","reopening"), "detection_success")

tdat_wideAggrIL2 <- tdat_wideAggrIL %>%
  pivot_wider(names_from = sim, values_from = c("min.val", "max.val", "mean.val", "median.val", "sd.val", "n.val", "q25", "q75", "q2.5", "q97.5", "se.val", "lower.ci.val", "upper.ci.val"))



tdat_wideAggrIL$sim <- factor(tdat_wideAggrIL$sim, levels=c("no reduction in test delay","reduced test delay Sym only" ,"reduced test delay As, Sym"),
                              labels=c("no reduction in test delay","reduced test delay Sym only" ,"reduced test delay As, Sym"))


pplot <- ggplot(data = tdat_wideAggrIL) +
  theme_minimal() +
  geom_pointrange(data = tdat_wideAggrIL, aes(x = reopening, y = median.val,ymin = q25,ymax = q75, shape = sim, col = sim,group=sim),
                  size = 1, position = position_dodge(0.3)) +
  labs(
    title = "",
    subtitle = "",
    x = "\nPartial reopening scenario",
    y = "Detection of a- and presymptomatics\ngiven perfect isolation success (%)\n",
    col = "",
    shape = ""
  ) +
  scale_color_manual(values = c("deepskyblue3", "orange", 'mediumvioletred')) +
  customThemeNoFacet +
  scale_y_continuous(breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10))

ggsave(paste0(selected_outcome, "_IL_thresholds_summary3.png"),
       plot = pplot, path = file.path(sim_dir), width = 8, height = 5, dpi = 200, device = "png"
)
ggsave(paste0(selected_outcome, "_IL_thresholds_summary3.pdf"),
       plot = pplot, path = file.path(sim_dir), width = 10, height = 5, dpi = 200, device = "pdf"
)




####################


tdat_wideAggrIL <- tdat_wide %>%  f_aggrDat(c("sim","reopening","grpvar"), "detection_success")
tdat_wideAggrIL <- tdat_wideAggrIL %>%  f_aggrDat(c("sim","reopening"), "mean.val")

tdat_wideAggrIL2 <- tdat_wideAggrIL %>%
  pivot_wider(names_from = sim, values_from = c("min.val", "max.val", "mean.val", "median.val", "sd.val", "n.val", "q25", "q75", "q2.5", "q97.5", "se.val", "lower.ci.val", "upper.ci.val"))



tdat_wideAggrIL$sim <- factor(tdat_wideAggrIL$sim, levels=c("no reduction in test delay","reduced test delay Sym only" ,"reduced test delay As, Sym"),
                              labels=c("no reduction in test delay","reduced test delay Sym only" ,"reduced test delay As, Sym"))


pplot <- ggplot(data = tdat_wideAggrIL) +
  theme_minimal() +
  geom_pointrange(data = tdat_wideAggrIL, aes(x = reopening, y = mean.val,ymin = min.val,ymax = max.val, shape = sim, col = sim,group=sim),
                  size = 1, position = position_dodge(0.3)) +
  labs(
    title = "",
    subtitle = "",
    x = "\nPartial reopening scenario",
    y = "Detection of a- and presymptomatics\ngiven perfect isolation success (%)\n",
    col = "",
    shape = ""
  ) +
  scale_color_manual(values = c("deepskyblue3", "orange", 'mediumvioletred')) +
  customThemeNoFacet +
  scale_y_continuous(breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10))

ggsave(paste0(selected_outcome, "_IL_thresholds_summary4.png"),
       plot = pplot, path = file.path(sim_dir), width = 8, height = 5, dpi = 200, device = "png"
)
ggsave(paste0(selected_outcome, "_IL_thresholds_summary4.pdf"),
       plot = pplot, path = file.path(sim_dir), width = 10, height = 5, dpi = 200, device = "pdf"
)


