## ============================================================
## Heatmap based on Rt and plot of Rt over time
## ============================================================


# #### Combine Rt estimates per region
# load(file.path(Rt_dir, "1_temp_Rt_tempdat_All.Rdata"))
# Rt_dat <- Rt_tempdat_All
# rm(Rt_tempdat_All)
# 
# for (i in c(1:11)) {
#   load(file.path(Rt_dir, paste0(i, "_temp_Rt_tempdat_All.Rdata")))
#   Rt_dat <- rbind(Rt_dat, Rt_tempdat_All)
#   rm(Rt_tempdat_All)
# }

# table(Rt_dat$region)
# table(Rt_dat$scen_num, Rt_dat$t_start)
# 
# ### add variables from simulation dataset
# selected_ems <- c(1:11)
# emsvars_temp <- c(paste0('critical', "_EMS."), "N_EMS_", "Ki_EMS_")
# 
# emsvars <- NULL
# for (ems in selected_ems) {
#   emsvars <- c(emsvars, paste0(emsvars_temp, ems))
# }
# 
# groupvars <- c("time", "Date", "startdate", "scen_num", "sample_num", "run_num", "grpvar", "detection_success", "isolation_success")
# (keepvars <- c(groupvars, emsvars))
# 
# dat <- trajectoriesDat %>%
#   dplyr::select(Date, scen_num, isolation_success, detection_success, grpvar) %>%
#   dplyr::arrange(Date) %>%
#   dplyr::group_by(scen_num) %>%
#   dplyr::mutate(date = as.Date(Date), time = c(1:n_distinct(date))) %>%
#   unique()
# 
# subdat <- trajectoriesDat %>% dplyr::select(keepvars)
# # rm(trajectoriesDat)
# 
# subdat <- trajectoriesDat %>%
#   dplyr::select(keepvars) %>%
#   pivot_longer(cols = -c(groupvars)) %>%
#   dplyr::mutate(
#     name = gsub("All", "EMS.IL", name)
#   ) %>%
#   separate(name, into = c("outcome", "region"), sep = "_EMS[.]") %>%
#   dplyr::filter(Date >= reopeningdate) %>%
#   dplyr::select(-c(time)) %>%
#   filter(outcome == selected_outcome) %>%
#   as.data.frame()
# 
# 
# ### Edit dataframe
# Rt_dat2 <- Rt_dat %>%
#   merge(unique(dat[, c("time", "Date", "scen_num", "isolation_success", "detection_success", "grpvar")]),
#     by.x = c("t_start", "scen_num"), by.y = c("time", "scen_num")
#   )
# 
# colnames(Rt_dat2) <- gsub("[(R]", "", colnames(Rt_dat2))
# colnames(Rt_dat2) <- gsub("[)]", "", colnames(Rt_dat2))
# 
# Rt_dat2 <- Rt_dat2 %>% mutate(meanRtLE1 = ifelse(Median < 1, 1, 0))

#write.csv(Rt_dat2, file = file.path(Rt_dir, paste0("EMS_combined_estimated_Rt.csv")), row.names = FALSE)
Rt_dat2 <- read.csv(file.path(Rt_dir,"EMS_combined_estimated_Rt.csv" ))
Rt_dat2$Date <- as.Date(Rt_dat2$Date)
summary(Rt_dat2$Date)
summary(Rt_dat2$t_start)


### Readjust date for 2 weeks estimation of Rt (to do double check)
Rt_dat2$Date <- Rt_dat2$Date+14
### --------------------------------------------------
Rt_dat2$region_label <- factor(Rt_dat2$region, levels = c(1:11), labels = paste0("EMS ", c(1:11), "\n"))

unique(Rt_dat2$detection_success)

pplot <- ggplot(data = subset(Rt_dat2, Date >= "2020-05-01" & Date < as.Date("2020-09-01"))) +
  theme_minimal() +
  geom_vline(xintercept = c(as.Date("2020-07-01"), as.Date("2020-08-01")), linetype = "dashed", size = 0.7, col = "darkgrey") +
  geom_line(aes(x = Date, y = Mean, group = scen_num), col = "deepskyblue3", size = 0.7) +
  # geom_smooth(aes(x=Date, y =Mean ),col="darkred") +
  scale_x_date(breaks = "2 weeks", labels = date_format("%b%d")) +
  geom_hline(yintercept = 1) +
  facet_wrap(~region_label) +
  scale_y_continuous(lim = c(0.5, 1.5)) +
  labs(y = expression(italic(R[t])), x = "") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())


ggsave(paste0(selected_outcome, "_Rt_over_time.png"),
  plot = pplot, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
)


### 
df <- Rt_dat2 %>%
  dplyr::filter(Date >= as.Date("2020-07-01") & Date < as.Date("2020-08-01")) %>%
  dplyr::group_by(region, region_label, Date, t_start, scen_num, t_end, isolation_success, detection_success, grpvar) %>%
  dplyr::summarize(average_median_Rt = mean(Mean)) %>%
  mutate(Rt_fct = ifelse(average_median_Rt < 1, "<1", ">=1"))

df$region_label <- factor(df$region, levels = c(1:11), labels = paste0("EMS_", c(1:11), "\n"))
df$capacity <- 1

summary(df$average_median_Rt)
summary(df$Date)
tapply(df$average_median_Rt, df$region, summary)

# ggplot(data = subset(df, isolation_success ==max(isolation_success))) + geom_point(aes(x=detection_success, y = average_median_Rt, col=region_label))

testplot1 <- ggplot(data = subset(df, grpvar == 0.17)) +
  theme_classic() +
  geom_point(aes(x = detection_success, y = isolation_success, fill = Rt_fct), shape = 21, size = 2) +
  scale_fill_viridis(option = "C", discrete = TRUE, direction = -1) +
  scale_color_viridis(option = "C", discrete = TRUE, direction = -1) +
  customThemeNoFacet +
  scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
  scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
  theme(panel.spacing = unit(2, "lines")) +
  labs(
    color = groupVar_label,
    subtitle = "",
    fill = groupVar_label,
    x = detectionVar_label,
    y = isolationVar_label
  ) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  theme(legend.position = "right") +
  facet_wrap(~region_label) +
  theme(panel.spacing = unit(1.5, "lines"))

ggsave(paste0(selected_outcome, "_Rt_sample_scatterplot.png"),
  plot = testplot1, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
)



##### ---------------------------------------------
###  Regresson model
##### ---------------------------------------------

for (ems in c(1:11)) {
  dat <- subset(df, region == ems)
  fitlist <- list()
  
 if( min(dat$average_median_Rt)>1)next

  for (grp in unique(dat$grpvar)) {
    # grp <- unique(dat$grpvar)[1]
    #### Do loess regression
    detection_success <- seq(0, 1, 0.001)
    isolation_success <- seq(0, 1, 0.001)
    t_matdat <- expand.grid(detection_success = detection_success, isolation_success = isolation_success)

    m <- loess(average_median_Rt ~ detection_success * isolation_success,
      span = 0.5,
      degree = 2, data = subset(dat, grpvar == grp)
    )

    temp.fit_mat <- predict(m, t_matdat)
    temp.fit <- melt(temp.fit_mat)
    temp.fit$detection_success <- gsub("detection_success=", "", temp.fit$detection_success)
    temp.fit$isolation_success <- gsub("isolation_success=", "", temp.fit$isolation_success)


    # library(plotly)
    # fig <- plot_ly(
    #   x = temp.fit$detection_success,
    #   y = temp.fit$isolation_success,
    #   z =  temp.fit$value, ,
    #   type = "contour"
    # )

    temp.fit$value_fct <- NA
    temp.fit$value_fct[temp.fit$value >= 1.02] <- ">1.02"
    temp.fit$value_fct[temp.fit$value < 1.02] <- "<1.02"
    temp.fit$value_fct[temp.fit$value < 1.01] <- "<1.01"
    temp.fit$value_fct[temp.fit$value < 1] <- "<1"
    temp.fit$value_fct[temp.fit$value < 0.99] <- "<0.99"
    temp.fit$value_fct[temp.fit$value < 0.98] <- "<0.98"

    table(temp.fit$value_fct, exclude = NULL)

    temp.fit$value_fct <- factor(temp.fit$value_fct,
      levels = c(">1.02", "<1.02", "<1.01", "<1", "<0.99", "<0.98"),
      labels = c(">1.02", "<1.02", "<1.01", "<1", "<0.99", "<0.98")
    )

    temp.fit$detection_success <- as.numeric(temp.fit$detection_success)
    temp.fit$isolation_success <- as.numeric(temp.fit$isolation_success)

    temp.fit$grpvar <- grp
    fitlist[[length(fitlist) + 1]] <- temp.fit

    rm(temp.fit_mat, temp.fit)
  }


  dtfit <- bind_rows(fitlist)
  rm(fitlist)


  dat$value_fct <- NA
  dat$value_fct[dat$average_median_Rt >= 1.02] <- ">1.02"
  dat$value_fct[dat$average_median_Rt < 1.02] <- "<1.02"
  dat$value_fct[dat$average_median_Rt < 1.01] <- "<1.01"
  dat$value_fct[dat$average_median_Rt < 1] <- "<1"
  dat$value_fct[dat$average_median_Rt < 0.99] <- "<0.99"
  dat$value_fct[dat$average_median_Rt < 0.98] <- "<0.98"

  table(dat$value_fct, exclude = NULL)

  dat$value_fct <- factor(dat$value_fct,
    levels = c(">1.02", "<1.02", "<1.01", "<1", "<0.99", "<0.98"),
    labels = c(">1.02", "<1.02", "<1.01", "<1", "<0.99", "<0.98")
  )


  thresholdDat <- dtfit %>%
    filter(value <= 1) %>%
    group_by(detection_success, grpvar) %>%
    filter(isolation_success == min(isolation_success)) %>%
    mutate(regin = ems)

  p1 <- ggplot(data = subset(dtfit, !is.na(value_fct)), aes(x = detection_success, y = isolation_success)) +
    theme_minimal() +
    geom_tile(aes(fill = value_fct), alpha = 0.8) +
    geom_line(data = subset(thresholdDat, isolation_success != min(isolation_success)), aes(x = detection_success, y = isolation_success), size = 1.3) +
    scale_fill_viridis(option = "C", discrete = TRUE) +
    labs(
      x = "detections (%)",
      y = "isolation success (%)",
      col = "",
      fill = expression(italic(R[t])),
      shape = "",
      linetype = ""
    ) +
    scale_x_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1) * 100, expand = c(0, 0)) +
    facet_wrap(~grpvar) +
    customThemeNoFacet

  p2 <- p1 + geom_point(data = dat, aes(x = detection_success, y = isolation_success, fill = value_fct), shape = 21, size = 3, show.legend = FALSE)

  plotname1 <- paste0("EMS", "_", ems, "_Rt_heatmap_loess")

  ggsave(paste0(plotname1, ".png"),
    plot = p2, path = file.path(ems_dir), width = 12, height = 5, dpi = 300, device = "png"
  )

  # ggsave(paste0(plotname1, ".pdf"),
  #       plot = p1, path = file.path(ems_dir), width = 12, height = 5, dpi = 300, device = "pdf"
  # )

  write.csv(thresholdDat, file = file.path(ems_dir, paste0(ems, "_Rt_loess_thresholds.csv")), row.names = FALSE)
}


### Combine all EMS thresholds into one file
thresholdsfiles <- list.files(file.path(ems_dir), pattern = "Rt_loess_thresholds", recursive = TRUE, full.names = TRUE)
#lmthresholdsDat <- sapply(thresholdsfiles, read.csv, simplify = FALSE) %>%
#  bind_rows(.id = "id")

datlist <- list()
for(i in c(1:length(thresholdsfiles))){
  temp <-  read.csv(thresholdsfiles[i])
  if(dim(temp)[1]<1)next
  temp$id <- thresholdsfiles[i]
  datlist[[length(datlist)+1]] <- temp 
  
}
lmthresholdsDat <- datlist %>% bind_rows()
lmthresholdsDat$id <- gsub(ems_dir, "", lmthresholdsDat$id)
lmthresholdsDat$region <- gsub("_Rt_loess_thresholds", "", lmthresholdsDat$id)
lmthresholdsDat$region <- gsub("[/]", "", lmthresholdsDat$region)

## Write results in csv table
write.csv(lmthresholdsDat, file.path(exp_dir, "Rt_thresholds_loess.csv"), row.names = FALSE)
