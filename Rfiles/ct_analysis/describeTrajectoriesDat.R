## ==================================================
# R script that analysis trajectoriesDat
## ==================================================

library(scales)
library(data.table)





emsvars_temp <- c("critical_EMS.","N_EMS_","Ki_EMS_" )

emsvars <- NULL
for (ems in selected_ems) {
  emsvars <- c(emsvars, paste0(emsvars_temp, ems))
}

groupvars <- c("startdate","Date","time", "backtonormal_multiplier", "scen_num", "sample_num", "run_num", "backtonormal_multiplier", "detection_success", "isolation_success", "grpvar"  )
(keepvars <- c(groupvars, emsvars))


subdat <- trajectoriesDat %>% dplyr::select(keepvars)

subdat <- subdat %>%
  pivot_longer(cols = -c(groupvars)) %>%
  dplyr::mutate( name = gsub("All", "EMS.IL", name)
  ) %>%
  dplyr::mutate(name = gsub("[.]", "_", name)) %>%
  separate(name, into = c("outcome", "region"), sep = "_EMS_") %>%
  dplyr::filter(Date >= reopeningdate) %>%
  dplyr::select(-c(time))

capacity <- load_capacity("all")
subdat <- merge(subdat, capacity, by.x="region", by.y="ems")

subdat$region_label <- factor(subdat$region, levels=c(1:11), labels=labs)
trajectoriesDatAggr = f_aggrDat(subdat, c("Date","region", "region_label", "outcome", "hospitalized", "critical","ventilators"), "value")


subdatAggr = subdat %>% dplyr::group_by(region,  scen_num, outcome ) %>% dplyr::mutate(valueMax = max(value, na.rm=TRUE)) %>%
  filter(valueMax<=critical) %>% 
  f_aggrDat( c("Date","region", "region_label", "outcome", "hospitalized", "critical","ventilators"), "value")


### Limit for ICU beds
ggplot(data = subset(subdat, outcome=="critical")) +
  theme_minimal() +
  geom_line(aes(x = Date, y = value, group=scen_num), col="deepskyblue4",size=1.3) +
  geom_hline(aes(yintercept=critical)) + 
  scale_color_viridis(discrete = TRUE) +
  labs(title=paste0("EMS ", ems), subtitle="" ,y="critical") +
  customThemeNoFacet +
  scale_x_date(breaks = "1 month", labels = date_format("%b"))  +
  facet_wrap(~region_label , scales="free")

l_plot2 <- ggplot(data = subset(trajectoriesDatAggr, outcome=="critical")) +
  theme_minimal() +
  geom_ribbon(aes(x = Date, ymin =min.val, ymax =max.val ), fill="deepskyblue3", alpha=0.2) +
  geom_ribbon(aes(x = Date, ymin = q2.5, ymax =q97.5 ), fill="deepskyblue3", alpha=0.35) +
  geom_ribbon(aes(x = Date, ymin = q25, ymax =q75 ), fill="deepskyblue3", alpha=0.5) +
  geom_ribbon(aes(x = Date, ymin = lower.ci.val, ymax =upper.ci.val ), fill="deepskyblue3", alpha=0.7) +
  geom_line(aes(x = Date, y = mean.val), col="deepskyblue4",size=1.3) +
  geom_ribbon(data = subset(subdatAggr, outcome=="critical"),aes(x = Date, ymin =min.val, ymax =max.val ), fill="brown3", alpha=0.2) +
  geom_ribbon(data = subset(subdatAggr, outcome=="critical"), aes(x = Date, ymin = q2.5, ymax =q97.5 ), fill="brown3", alpha=0.35) +
  geom_ribbon(data = subset(subdatAggr, outcome=="critical"), aes(x = Date, ymin = q25, ymax =q75 ), fill="brown3", alpha=0.5) +
  # geom_line(data = subset(subdat, outcome=="critical" & value<=critical ),aes(x = Date, y = value, group=scen_num), col="brown3",size=1.3) +
  geom_line(data = subset(subdatAggr, outcome=="critical" ),aes(x = Date, y = mean.val), col="brown3",size=1.3) +
  geom_hline(aes(yintercept=critical)) + 
  scale_color_viridis(discrete = TRUE) +
  labs(title="", subtitle="" ,y="Predicted ICU bed demand") +
  customThemeNoFacet +
  scale_x_date(breaks = "1 month", labels = date_format("%b"))  +
  facet_wrap(~region_label , scales="free")




subd =  subset(trajectoriesDat, Date == max(Date))
tapply(subd$crit_cumul_All,subd$grpvar, summary )




pplot <- ggplot(data = subset(trajectoriesDat, Date == max(Date)), aes(x=grpvar, y =crit_cumul_All, group=grpvar )) +
  theme_minimal() +
  geom_violin(fill="deepskyblue3", col="deepskyblue3")+
  stat_summary(fun=median, geom="point", size=2, color="black")+
  scale_y_continuous(expand=c(0,0), labels = scales::comma) +
  labs(y="Daily critical cases", x="")+
  customThemeNoFacet+
  theme( panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank() )+
  geom_hline(yintercept=20000)

ggsave(paste0("_boxplot_by_grp.png"),
       plot = pplot, path = file.path(exp_dir), width = 10, height = 6, device = "png"
)
ggsave(paste0("_boxplot_by_grp.pdf"),
       plot = pplot, path = file.path(exp_dir), width = 10, height = 6, device = "pdf"
)


#####  visualize detection variables
p1 <- ggplot(data =trajectoriesDat) +
  theme_bw() +
  geom_line(data=subset(trajectoriesDat,Date>="2020-06-14"),aes(
    x = Date, y = d_As_t, col =(round(detection_success, 1)),
    group = interaction(detection_success, scen_num)
  ), size = 1.3, show.legend = FALSE) +
  geom_line(data=subset(trajectoriesDat,Date<="2020-06-15"),aes(
    x = Date, y = d_As_t,
    group = interaction(detection_success, scen_num)
  ), size = 1,col="grey", show.legend = FALSE) +
  scale_color_viridis(discrete = FALSE) +
  labs(title=detectionVar_label, subtitle="") +
  customThemeNoFacet

p2 <-  ggplot(data = trajectoriesDat) +
     theme_bw() +
   geom_line(data=subset(trajectoriesDat,Date>="2020-06-14"),aes(
     x = Date, y = d_Sym_t, col = as.factor(round(grpvar, 1)),
     group = interaction(grpvar, detection_success,scen_num)
   ), size = 1.3, show.legend = FALSE) +
   geom_line(data=subset(trajectoriesDat,Date<="2020-06-15"), aes(
         x = Date, y = d_Sym_t,
         group = interaction(grpvar, detection_success,scen_num)
       ), size = 0.7,col="grey", show.legend = FALSE) +
     scale_color_viridis(discrete = TRUE) +
     labs(title=groupVar_label, subtitle="") +
     customThemeNoFacet

pplot <- plot_grid(p1, p2, nrow=1)

ggsave(paste0("detection_timeline.png"),
       plot = pplot, path = file.path(exp_dir), width = 14, height = 6, device = "png"
)

##### timeline of outcomes
pplot <- ggplot(data = subset(trajectoriesDat, isolation_success > 0.9)) +
  theme_bw() +
  geom_line(aes(
    x = Date, y = critical_All, col = as.factor(round(detection_success, 1)),
    group = interaction(detection_success, scen_num)
  ), size = 2, show.legend = TRUE) +
  facet_grid(backtonormal_multiplier ~ grpvar, labeller = labeller(.rows = label_both, .cols = label_both)) +
  scale_color_viridis(discrete = TRUE) +
  labs(color = "detection_success") +
  customThemeNoFacet

ggsave(paste0("sample_timeline.png"),
  plot = pplot, path = file.path(exp_dir), width = 12, height = 6, device = "png"
)


plot_list <- list()
for (ems in c(1:11)) {
  trajectoriesDat$value <- trajectoriesDat[, colnames(trajectoriesDat) == paste0("critical_EMS.", ems)]
  trajectoriesDatAggr = f_aggrDat(trajectoriesDat, c("Date"), "value")
  
  
  l_plot <- ggplot(data = subset(trajectoriesDat)) +
    theme_minimal() +
    geom_line(aes(
      x = Date, y = value, col = as.factor(round(detection_success, 1)),
      group = scen_num
    ), size = 1, show.legend = TRUE) +
    scale_color_viridis(discrete = TRUE) +
    labs(title=paste0("EMS ", ems), subtitle="" , color = "detection_success") +
    customThemeNoFacet +
    scale_x_date(breaks = "1 month", labels = date_format("%b")) 

  ggsave(paste0(ems, "_sample_timeline.png"),
    plot = l_plot, path = file.path(ems_dir), width = 12, height = 7, device = "png"
  )
# 
  # l_plot <- ggplot(data = subset(trajectoriesDat)) +
  #   theme_minimal() +
  #   geom_line(aes(
  #     x = Date, y = value, col = as.factor(round(detection_success, 1)),
  #     group = scen_num
  #   ), size = 1, show.legend = TRUE, col="grey") +
  #   geom_line(data = subset(trajectoriesDat, scen_num %in% c(1,7,40, 8, 12, 20, 4)), aes(
  #     x = Date, y = value, col = as.factor(round(detection_success, 1)),
  #     group = scen_num
  #   ), size = 1.5, show.legend = TRUE) +
  #   scale_color_viridis(discrete = TRUE) +
  #   labs(title=paste0("EMS ", ems), subtitle="" , color = "detection_success") +
  #   customThemeNoFacet +
  #   scale_x_date(breaks = "1 month", labels = date_format("%b")) 
  # 
  #  ggsave(paste0(ems, "_sample_timeline2.pdf"),
  #        plot = l_plot, path = file.path(ems_dir), width = 12, height = 7, device = "pdf"
  # )
  # 
  
  
  l_plot2 <- ggplot(data = subset(trajectoriesDatAggr)) +
    theme_minimal() +
    geom_ribbon(aes(x = Date, ymin = q2.5, ymax =q97.5 ), fill="deepskyblue3", alpha=0.3) +
    geom_ribbon(aes(x = Date, ymin = q25, ymax =q75 ), fill="deepskyblue3", alpha=0.5) +
    geom_ribbon(aes(x = Date, ymin = lower.ci.val, ymax =upper.ci.val ), fill="deepskyblue3", alpha=0.7) +
    geom_line(aes(x = Date, y = mean.val), col="deepskyblue4",size=1.3) +
    scale_color_viridis(discrete = TRUE) +
    labs(title=paste0("EMS ", ems), subtitle="" ,y="critical") +
    customThemeNoFacet +
    scale_x_date(breaks = "1 month", labels = date_format("%b")) 
  
  ggsave(paste0(ems, "_aggregated_timeline.png"),
         plot = l_plot2, path = file.path(ems_dir), width = 12, height =7, device = "png"
  )
  
  
  #### Descriptive plots
  p_i <- ggplot(data = trajectoriesDat) +
    theme_bw() +
    geom_jitter(aes(x = detection_success, y = isolation_success, col = value), size = 2, show.legend = TRUE) +
    facet_grid(~grpvar) +
    scale_color_viridis(discrete = FALSE, trans = "log") +
    customThemeNoFacet +
    labs(title = paste0("EMS ", ems), subtitle = paste0(groupVar_label))

  plot_list[[ems]] <- p_i

  rm(l_plot, ems)
}

pplot <- plot_grid(
  plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]], plot_list[[6]],
  plot_list[[7]], plot_list[[8]], plot_list[[9]], plot_list[[10]], plot_list[[11]]
)

ggsave(paste0("sample_points.png"),
  plot = pplot, path = file.path(ct_dir, simdate, exp_name), width = 26, height = 12, device = "png"
)
