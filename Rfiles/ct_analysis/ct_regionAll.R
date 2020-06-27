## ==================================================
# R script that analysis trajectoriesDat
## ==================================================


selected_outcome <- "critical"
flabel <- paste0("Predicted ", selected_outcome)

selected_ems <- c(1:11)
emsvars_temp <- c("critical_EMS.", "N_EMS_", "Ki_EMS_")

emsvars <- NULL
for (ems in selected_ems) {
  emsvars <- c(emsvars, paste0(emsvars_temp, ems))
}

groupvars <- c("time", "Date", "startdate", "scen_num", "sample_num", "run_num", "grpvar", "detection_success", "isolation_success")
(keepvars <- c(groupvars, emsvars))

subdat <- trajectoriesDat %>% dplyr::select(keepvars)
# rm(trajectoriesDat)

subdat <- subdat %>%
  pivot_longer(cols = -c(groupvars)) %>%
  mutate(
    name = gsub("All", "EMS.IL", name)
  ) %>%
  separate(name, into = c("outcome", "region"), sep = "_EMS[.]") %>%
  dplyr::filter(Date >= reopeningdate) %>%
  select(-c(time))

selected_outcome <- "critical"
capacity <- load_capacity("all")
capacity$region <- as.character(rownames(capacity))
capacity <- capacity %>%
  rename(capacity = selected_outcome) %>%
  select(region, capacity)

plotdat <- subdat %>%
  left_join(capacity, by = "region") %>%
  filter(outcome == selected_outcome) %>%
  as.data.frame()

peakTimes <- plotdat %>%
  group_by(region, capacity, isolation_success, detection_success, grpvar, scen_num, sample_num, run_num) %>%
  filter(value == max(value)) %>%
  rename(Date_peak = Date) %>%
  select(region, capacity, Date_peak, outcome, isolation_success, detection_success, grpvar, scen_num, sample_num, run_num)

## Add peak date to plotdat
plotdat <- plotdat %>%
  left_join(peakTimes, by = c("region", "capacity", "outcome", "scen_num", "sample_num", "run_num", "isolation_success", "detection_success", "grpvar"))


df <- subset(plotdat, Date == plotdat$Date_peak)

df$region_label <- factor(df$region, levels = c(1:11), labels = paste0("EMS_", c(1:11), "\n"))

testplot1 <- ggplot(data = df) +
  theme_classic() +
  geom_point(data = subset(df, value != capacity), aes(x = detection_success, y = isolation_success, group = grpvar), shape = 21, fill = "white", size = 2) +
  geom_point(data = subset(df, value <= capacity), aes(x = detection_success, y = isolation_success, fill = as.factor(grpvar)), shape = 21, size = 2) +
  scale_fill_viridis(option = "C", discrete = TRUE, direction = -1) +
  geom_smooth(data = subset(df, value <= capacity), aes(
    x = detection_success, y = isolation_success, col = as.factor(grpvar),
    fill = as.factor(grpvar)
  ), se = FALSE, method = "lm") +
  scale_color_viridis(option = "C", discrete = TRUE, direction = -1) +
  customThemeNoFacet +
  scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
  scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
  theme(panel.spacing = unit(2, "lines")) +
  labs(
    color = groupVar_label,
    subtitle = "",
    fill =groupVar_label,
    x = detectionVar_label,
    y = isolationVar_label
  ) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  theme(legend.position = "right") +
  facet_wrap(~region_label)

ggsave(paste0(selected_outcome, "_sample_scatterplot.png"),
       plot = testplot1, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
)



dfLM <- df %>%
  rename(
    x = detection_success,
    y = isolation_success,
    z = value
  ) %>%
  group_by(region, grpvar, outcome, capacity) %>%
  do(fitlm = lm(z ~ y + x, data = .))

(dfLMCoef <- tidy(dfLM, fitlm))
# augment(dfLM, fitlm)
glance(dfLM, fitlm)

## Parameter combinations that did run
sink(file.path(ems_dir, paste0("perEMS_", selected_outcome, "_linear_models.txt")))
cat("\ntidy(dfLM, fitlm)")
print(tidy(dfLM, fitlm))
cat("\naugment(dfLM, fitlm)")
print(augment(dfLM, fitlm))
cat("\nglance(dfLM, fitlm)")
print((d <- glance(dfLM, fitlm)))
print(summary(d$r.squared))
print(tapply(d$r.squared, d$region, summary))
sink()


### Generate prediction dataset
xnew <- seq(0, 1, 0.01)
ynew <- seq(0, 1, 0.01)

matdat_list <- list()
for (region_i in unique(dfLM$region)) {
  for (testDelay in unique(dfLM$grpvar)) {
    lmmodel <- dfLM %>% filter(grpvar == testDelay & region == region_i)

    t_matdat <- expand.grid(x = xnew, y = ynew)
    t_matdat <- as.data.frame(cbind(t_matdat, predict(lmmodel$fitlm[[1]], newdata = t_matdat, interval = "confidence")))
    t_matdat$grpvar <- testDelay
    t_matdat$region <- region_i
    t_matdat$capacity <- lmmodel$capacity

    matdat_list[[length(matdat_list) + 1]] <- t_matdat
  }
}

matdat <- matdat_list %>% bind_rows()
# table(matdat$grpvar)

tdat <- matdat %>%
  pivot_longer(cols = -c(region, capacity, x, y, grpvar), names_to = "statistic") %>%
  filter(value <= capacity) %>%
  dplyr::group_by(region, grpvar, x, statistic) %>%
  dplyr::summarize(ythreshold = min(y))

matdat <- left_join(matdat, tdat, by = c("grpvar", "x"))



matdat$grpvar <- round(matdat$grpvar, 2)
df$grpvar <- round(df$grpvar, 2)


tdat_wide <- tdat %>% pivot_wider(names_from = "statistic", values_from = "ythreshold")

tdat_wide %>%
  dplyr::filter(!is.na(fit)) %>%
  group_by(region, grpvar, x) %>%
  summarize(
    xmin = min(x), xmax = max(x),
    lwrmin = min(lwr), lwrmax = max(lwr)
  )


write.csv(tdat_wide,file.path(exp_dir,  "tdat_wide.csv"), row.names = FALSE)


#### Assemble polygon to fill boundary lines
xpol <- c(tdat_wide$x, rev(tdat_wide$x))
ypol <- c(tdat_wide$lwr, rev(tdat_wide$upr))
grpvar <- c(tdat_wide$grpvar, rev(tdat_wide$grpvar))
region <- c(tdat_wide$region, rev(tdat_wide$region))

datpol <- as.data.frame(cbind(xpol, ypol, grpvar, region))

datpol$xpol <- as.numeric(datpol$xpol)
datpol$ypol <- as.numeric(datpol$ypol)
datpol$grpvar <- as.numeric(datpol$grpvar)

datpol$region_label <- factor(datpol$region, levels = c(1:11), labels = paste0("EMS_", c(1:11), "\n"))
tdat_wide$region_label <- factor(tdat_wide$region, levels = c(1:11), labels = paste0("EMS_", c(1:11), "\n"))

datpol$region_label <- factor(datpol$region, levels = c(1:11), labels = paste0("EMS_", c(1:11), "\n"))
tdat_wide$region_label <- factor(tdat_wide$region, levels = c(1:11), labels = paste0("EMS_", c(1:11), "\n"))


datpol$grpvar <- round(datpol$grpvar, 2)
tdat_wide$grpvar <- round(tdat_wide$grpvar, 2)

regLabel <- df %>%
  select(region, capacity) %>%
  unique() %>%
  mutate(regLabel = paste0("EMS_", region, "\n limit: ", capacity))

datpol$region_label2 <- factor(datpol$region, levels = c(1:11), labels = labs)
tdat_wide$region_label2 <- factor(tdat_wide$region, levels = c(1:11), labels = labs)


matdatp2 <- ggplot(data = tdat_wide) +
  theme_cowplot() +
  geom_polygon(data = datpol, aes(x = xpol, y = ypol, fill = as.factor(grpvar)), alpha = 0.5) +
  geom_smooth(
    data = subset(tdat_wide),
    aes(x = x, y = fit, col = as.factor(grpvar), group = grpvar),
    method = "lm", size = 1.3, show.legend = FALSE
  ) +
  scale_fill_viridis(option = "C", discrete = TRUE, direction = -1) +
  scale_color_viridis(option = "C", discrete = TRUE, direction = -1) +
  customThemeNoFacet +
  scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
  scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
  theme(panel.spacing = unit(2, "lines")) +
  facet_wrap(~region_label2, scales = "free") +
  labs(
    color = grpvar,
    subtitle = "",
    fill = groupVar_label,
    x = detectionVar_label,
    y = isolationVar_label
  ) +
  theme(legend.position = "right")


ggsave(paste0("all_capacity_thresholds_2.png"),
  plot = matdatp2, path = file.path(exp_dir), width = 15, height = 10, dpi = 300, device = "png"
)
ggsave(paste0("all_capacity_thresholds_2.pdf"),
  plot = matdatp2, path = file.path(exp_dir), width = 15, height = 10, dpi = 300, device = "pdf"
)



############ Time of second weak plot 
plotdat$region <- factor(plotdat$region, levels = c(1:11), labels = c(1:11))
plotdatAggr1 <- plotdat %>%
  group_by(region, Date, scen_num) %>%
  summarize(value = mean(value))

plotdatAggr <- plotdat %>%
  group_by(region, Date) %>%
  summarize(value = mean(value))

l_plot2 <- ggplot() +
  theme_cowplot() +
  geom_line(data = plotdatAggr1, aes(
    x = Date, y = value,
    group = interaction(scen_num, region)
  ), size = 1, col = "grey", alpha = 0.3) +
  geom_line(data = plotdatAggr, aes(
    x = Date, y = value,
    col = as.factor(region),
    fill = as.factor(region),
    group = region
  ), size = 2) +
  labs(
    color = "EMS",
    fill = "EMS",
    y = "Required ICU beds", x = ""
  ) +
  customThemeNoFacet +
  theme(axis.text.x = element_text(size = 12, angle = 60, hjust = 1, vjust = 1)) +
  scale_color_viridis(option = "D", discrete = TRUE, direction = -1) +
  scale_fill_viridis(option = "D", discrete = TRUE, direction = -1) +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B")


ggsave(paste0(selected_outcome, "_peaks.png"),
  plot = l_plot2, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
)

