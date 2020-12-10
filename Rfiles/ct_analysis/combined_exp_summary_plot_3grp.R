## ============================================================
## Combine simulated experiments and make summary plot
## ============================================================
selected_outcome="critical"
### Load simulation data
exp_names <- list.dirs(file.path(ct_dir, simdate), recursive = FALSE, full.names = FALSE)

sim_dir <- file.path(ct_dir, simdate)

tdat_wide0 <- read.csv(file.path(sim_dir, exp_names[5], "tdat_wide.csv"))
tdat_wide1 <- read.csv(file.path(sim_dir, exp_names[6], "tdat_wide.csv"))
tdat_wide2 <- read.csv(file.path(sim_dir, exp_names[13], "tdat_wide.csv"))
#tdat_wide2 <- read.csv(file.path(sim_dir, exp_names[7], "tdat_wide.csv"))

tdat_wide0$sim <- "no reduction in test delay"
tdat_wide1$sim <- "reduced test delay As, Sym"
tdat_wide2$sim <- "reduced test delay Sym only"

tdat_wide <- rbind(tdat_wide0 , tdat_wide1, tdat_wide2)
rm(tdat_wide1, tdat_wide2)


tdat_wide <- tdat_wide %>%
  group_by(region, grpvar,sim) %>%
  mutate(fitmax = max(fit, na.rm = TRUE))
tdat_wide$region <- factor(tdat_wide$region, levels = c(1:11), labels = c(1:11))

tempdat <- subset(tdat_wide, fit == fitmax)
summary(tempdat$x)
tapply(tempdat$x, tempdat$grpvar, summary)
table(tempdat$grpvar, tempdat$region)


tdat_wideAggr <- tdat_wide %>% f_aggrDat(c("region", "sim"), "x")

tdat_wideAggr2 <- tdat_wideAggr %>%
  pivot_wider(names_from = sim, values_from = c("min.val", "max.val", "mean.val", "median.val", "sd.val", "n.val", "q25", "q75", "q2.5", "q97.5", "se.val", "lower.ci.val", "upper.ci.val"))


pplot <- ggplot(data = tdat_wideAggr) +
  theme_minimal() +
  geom_hline(yintercept = mean(tdat_wideAggr$min.val), linetype = "dashed", col = "darkgrey", size = 1.3) +
  geom_errorbar(data = tdat_wideAggr2, aes(
    x = reorder(region, `min.val_no reduction in test delay`),
    ymin = `min.val_no reduction in test delay`, ymax = `min.val_reduced test delay As, Sym`
  ), size = 1, width = 0) +
  geom_point(aes(x = reorder(region, min.val), y = min.val, shape = sim, col = sim), size = 3) +
  labs(
    title = "",
    subtitle = "",
    x = "\nEMS region",
    y = "Detection of a- and presymptomatics\ngiven perfect isolation success (%)\n",
    col = "Detection level mild infections",
    shape = "Detection level mild infections"
  ) +
  scale_color_manual(values = c("deepskyblue3", "orange", 'mediumvioletred')) +
  customThemeNoFacet +
  scale_y_continuous(breaks = seq(0, 1, 0.10), labels = seq(0, 100, 10))

ggsave(paste0(selected_outcome, "_stopSIP20_thresholds_summary2.png"),
       plot = pplot, path = file.path(sim_dir), width = 8, height = 5, dpi = 200, device = "png"
)


