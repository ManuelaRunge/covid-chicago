## ============================================================
## Heatmap based on Rt and plot of Rt over time 
## ============================================================


#### COmbine Rt estimates per region
load(file.path(Rt_dir, "1_temp_Rt_tempdat_All.Rdata"))
Rt_dat <- Rt_tempdat_All
rm(Rt_tempdat_All)

for (i in c(1:11)) {
  load(file.path(Rt_dir, paste0(i, "_temp_Rt_tempdat_All.Rdata")))
  Rt_dat <- rbind(Rt_dat, Rt_tempdat_All)
  rm(Rt_tempdat_All)
}


table(Rt_dat$region)
table(Rt_dat$scen_num, Rt_dat$t_start)


### add variables from simulation dataset
selected_ems <- c(1:11)
emsvars_temp <- c(paste0(selected_outcome,"_EMS."), "N_EMS_", "Ki_EMS_")

emsvars <- NULL
for (ems in selected_ems) {
  emsvars <- c(emsvars, paste0(emsvars_temp, ems))
}

groupvars <- c("time", "Date", "startdate", "scen_num", "sample_num", "run_num", "grpvar", "detection_success", "isolation_success")
(keepvars <- c(groupvars, emsvars))

capacity <- load_capacity("all")
capacity$region <- as.character(rownames(capacity))
capacity <- capacity %>%
  rename(capacity = selected_outcome) %>%
  select(region, capacity)


dat <- trajectoriesDat %>%
  select(Date, scen_num, isolation_success, detection_success, grpvar) %>%
  arrange(Date) %>%
  group_by(scen_num) %>%
  mutate(date = as.Date(Date), time = c(1:n_distinct(date))) %>%
  unique()

subdat <- trajectoriesDat %>% dplyr::select(keepvars)
# rm(trajectoriesDat)

subdat <- trajectoriesDat %>%
  dplyr::select(keepvars) %>%
  pivot_longer(cols = -c(groupvars)) %>%
  mutate(
    name = gsub("All", "EMS.IL", name)
  ) %>%
  separate(name, into = c("outcome", "region"), sep = "_EMS[.]") %>%
  dplyr::filter(Date >= reopeningdate) %>%
  select(-c(time)) %>%
  left_join(capacity, by = "region") %>%
  filter(outcome == selected_outcome) %>%
  as.data.frame()


###############--------------------------


### Edit dataframe
Rt_dat2 <- Rt_dat %>%
  merge(unique(dat[, c("time", "Date", "scen_num", "isolation_success", "detection_success", "grpvar")]),
    by.x = c("t_start", "scen_num"), by.y = c("time", "scen_num")
  )

colnames(Rt_dat2) <- gsub("[(R]", "", colnames(Rt_dat2))
colnames(Rt_dat2) <- gsub("[)]", "", colnames(Rt_dat2))

Rt_dat2 <- Rt_dat2 %>% mutate(meanRtLE1 = ifelse(Median < 1, 1, 0))

write.csv(Rt_dat2, file = file.path(Rt_dir, paste0("EMS_combined_estimated_Rt.csv")), row.names = FALSE)
summary(Rt_dat2$Date)

### --------------------------------------------------

Rt_dat2$region_label <- factor(Rt_dat2$region, levels = c(1:11), labels = paste0("EMS_", c(1:11), "\n"))

unique(Rt_dat2$detection_success)
unique(Rt_dat2$detection_success)

pplot <- ggplot(data = subset(Rt_dat2, Date >= "2020-07-01" )) +
  theme_minimal() +
  geom_line(aes(x = Date, y = Mean, group = scen_num), col = "darkgrey") +
  # geom_smooth(aes(x=Date, y =Mean ),col="darkred") +
  scale_x_date(breaks = "1 month", labels = date_format("%b")) +
  geom_hline(yintercept = 1) +
  facet_wrap(~region_label) +
  scale_y_continuous(lim = c(0,2))


ggsave(paste0(selected_outcome, "_Rt_over_time.png"),
  plot = pplot, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
)


### Linear regression models and thresholds
df <- Rt_dat3 %>% filter(Date >= as.Date("2020-07-01") & Date <= as.Date("2020-12-30") ) %>%
                  group_by(region, region_label, Date, t_start, scen_num, t_end, isolation_success, detection_success, grpvar) %>% 
                  summarize(average_median_Rt = mean(Median))

df$region_label <- factor(df$region, levels = c(1:11), labels = paste0("EMS_", c(1:11), "\n"))
df$capacity =1

summary(df$average_median_Rt)
summary(df$Date)
tapply(df$average_median_Rt, df$region, summary)

#ggplot(data = subset(df, isolation_success ==max(isolation_success))) + geom_point(aes(x=detection_success, y = average_median_Rt, col=region_label)) 

testplot1 <- ggplot(data = df) +
  theme_classic() +
  geom_point(data = subset(df, average_median_Rt > 1), aes(x = detection_success, y = isolation_success, group = grpvar), shape = 21, fill = "white", size = 2) +
  geom_point(data = subset(df, average_median_Rt < 1), aes(x = detection_success, y = isolation_success, fill = as.factor(grpvar)), shape = 21, size = 2) +
  scale_fill_viridis(option = "C", discrete = TRUE, direction = -1) +
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

ggsave(paste0(selected_outcome, "_Rt_sample_scatterplot.png"),
       plot = testplot1, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
)


#####---------------------------------------------
###  Regresson model 
#####---------------------------------------------

dfLM <- df %>%
  rename(
    x = detection_success,
    y = isolation_success,
    z = average_median_Rt
  ) %>%
  group_by(region, grpvar, capacity) %>%
  do(fitlm = lm(z ~ y + x, data = .))

(dfLMCoef <- tidy(dfLM, fitlm))
# augment(dfLM, fitlm)
glance(dfLM, fitlm)

## Parameter combinations that did run
sink(file.path(ems_dir, paste0("perEMS_", selected_outcome, "_Rt_linear_models.txt")))
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

write.csv(tdat_wide,file.path(exp_dir,  "Rt_tdat_wide.csv"), row.names = FALSE)

matdatp2 <- ggplot(data = tdat_wide) +
  theme_cowplot() +
  geom_line(aes(x = x, y = fit, col = as.factor(grpvar), group = grpvar), size = 1.3, show.legend = FALSE) +
  scale_fill_viridis(option = "C", discrete = TRUE, direction = -1) +
  scale_color_viridis(option = "C", discrete = TRUE, direction = -1) +
  customThemeNoFacet +
  scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
  scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
  theme(panel.spacing = unit(2, "lines")) +
  facet_wrap(~region, scales = "free") +
  labs(
    color = grpvar,
    subtitle = "",
    fill = groupVar_label,
    x = detectionVar_label,
    y = isolationVar_label
  ) +
  theme(legend.position = "right")


ggsave(paste0("all_Rt_capacity_thresholds.png"),
       plot = matdatp2, path = file.path(exp_dir), width = 15, height = 10, dpi = 300, device = "png"
)


