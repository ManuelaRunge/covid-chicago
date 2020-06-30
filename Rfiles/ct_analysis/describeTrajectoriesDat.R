## ==================================================
# R script that analysis trajectoriesDat
## ==================================================


pplot <- ggplot(data = subset(trajectoriesDat, Date == max(Date))) +
  theme_minimal() +
  geom_boxplot(aes(x=grpvar, y =crit_cumul_All, group=grpvar ), fill="deepskyblue3")

ggsave(paste0("_boxplot_by_grp.png"),
       plot = pplot, path = file.path(exp_dir), width = 14, height = 6, device = "png"
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
