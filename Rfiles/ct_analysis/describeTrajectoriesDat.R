
#### Descriptive plots
pplot <- ggplot(data = trajectoriesDat) +
  theme_bw() +
  geom_jitter(aes(x = detection_success, y = isolation_success, col = critical_All), size = 2, show.legend = TRUE) +
  facet_grid(time_to_detection ~ backtonormal_multiplier, labeller = labeller(.rows = label_both, .cols = label_both))  +
  scale_color_viridis(discrete = FALSE,trans = "log") +
  customThemeNoFacet

ggsave(paste0("sample_points.png"),
  plot = pplot, path = file.path(ct_dir, simdate), width = 8, height = 6, device = "png"
)


pplot <- ggplot(data = subset(trajectoriesDat, isolation_success >0.9)) +
  theme_bw() +
  geom_line(aes(x = Date, y = critical_All, col = as.factor(round(detection_success, 1)),
               group = interaction(detection_success, scen_num)), size = 2, show.legend = TRUE) +
  facet_grid(backtonormal_multiplier~time_to_detection, labeller = labeller(.rows = label_both, .cols = label_both)) +
  scale_color_viridis(discrete = TRUE) +
  labs(color="detection_success") +
  customThemeNoFacet

ggsave(paste0("sample_timeline.png"),
  plot = pplot, path = file.path(ct_dir, simdate), width =12, height = 12, device = "png"
)



for(ems in c(1:11)){
  
  trajectoriesDat$value = trajectoriesDat[,colnames(trajectoriesDat) == paste0("critical_EMS.", ems)]
  
  l_plot <- ggplot(data = subset(trajectoriesDat)) +
    theme_bw() +
    geom_line(aes(x = Date, y = value, col = as.factor(round(detection_success, 1)),
                  group = interaction(detection_success, scen_num)), size = 2, show.legend = TRUE) +
    facet_grid(backtonormal_multiplier~time_to_detection, labeller = labeller(.rows = label_both, .cols = label_both)) +
    scale_color_viridis(discrete = TRUE) +
    labs(color="detection_success") +
    customThemeNoFacet
  
  ggsave(paste0(ems, "_sample_timeline.png"),
         plot = l_plot, path = file.path(ct_dir, simdate), width =12, height = 10, device = "png"
  )
  
  rm(l_plot, ems)
  
}


