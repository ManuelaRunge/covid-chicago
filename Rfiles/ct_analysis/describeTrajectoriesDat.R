

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

  l_plot <- ggplot(data = subset(trajectoriesDat)) +
    theme_bw() +
    geom_line(aes(
      x = Date, y = value, col = as.factor(round(detection_success, 1)),
      group = interaction(detection_success, scen_num)
    ), size = 2, show.legend = TRUE) +
    facet_grid(backtonormal_multiplier ~ grpvar, labeller = labeller(.rows = label_both, .cols = label_both)) +
    scale_color_viridis(discrete = TRUE) +
    labs(color = "detection_success") +
    customThemeNoFacet

  ggsave(paste0(ems, "_sample_timeline.png"),
    plot = l_plot, path = file.path(ems_dir), width = 12, height = 6, device = "png"
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
