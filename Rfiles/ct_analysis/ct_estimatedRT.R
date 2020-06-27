


load(file.path(Rt_dir, "1_temp_Rt_tempdat_All.Rdata"))
Rt_dat  = Rt_tempdat_All
rm(Rt_tempdat_All)

for(i in c(2:11)){
  load(file.path(Rt_dir, paste0(i,"_temp_Rt_tempdat_All.Rdata")))
  Rt_dat  = rbind(Rt_dat,Rt_tempdat_All)
  rm(Rt_tempdat_All)
}


table(Rt_dat$region)
table(Rt_dat$scen_num, Rt_dat$t_start)


dat <- trajectoriesDat %>% select(Date, scen_num, isolation_success, detection_success, grpvar) %>%
  arrange( Date) %>%
  group_by( scen_num) %>%
  mutate(date = as.Date(Date), time = c(1:n_distinct(date))) %>% unique()


### Edit dataframe
Rt_dat2 <- Rt_dat %>%
  merge(unique(dat[, c("time", "Date","scen_num", "isolation_success", "detection_success","grpvar")]),
        by.x = c("t_start", "scen_num"), by.y = c("time", "scen_num")) 

colnames(Rt_dat2) <- gsub("[(R]","",colnames(Rt_dat2))
colnames(Rt_dat2) <- gsub("[)]","",colnames(Rt_dat2))

Rt_dat2 <- Rt_dat2 %>% mutate(meanRtLE1 = ifelse(Median <1 , 1, 0))

write.csv(Rt_dat2, file=file.path(Rt_dir, paste0("EMS_combined_estimated_Rt.csv")), row.names = FALSE)
summary(Rt_dat2$Date)

Rt_dat2$region_label <- factor(Rt_dat2$region, levels = c(1:11), labels = paste0("EMS_", c(1:11), "\n"))


pplot <- ggplot(data= subset(Rt_dat2)) +
  theme_minimal() +
  geom_line(aes(x=Date, y =Mean , group=scen_num),col="darkgrey") +
  geom_smooth(aes(x=Date, y =Mean ),col="darkred") +
  scale_x_date(breaks = "1 month", labels = date_format("%b")) +
  geom_hline(yintercept = 1) +
  facet_wrap(~region_label)


ggsave(paste0(selected_outcome, "_Rt_over_time.png"),
       plot = pplot, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
)


Rt_dat3 <- Rt_dat2 %>%
  left_join(peakTimes, by = c("region", "scen_num", "isolation_success", "detection_success", "grpvar")) %>%
  filter(Date==Date_peak)


pplot <- ggplot(data= subset(Rt_dat2)) +
  theme_minimal() +
  geom_line(aes(x=Date, y =Mean , group=scen_num),col="darkgrey") +
  geom_smooth(aes(x=Date, y =Mean ),col="darkred") +
  scale_x_date(breaks = "1 month", labels = date_format("%b")) +
  geom_hline(yintercept = 1) +
  facet_wrap(~region_label) +
  geom_vline(data=Rt_dat3, aes(xintercept = Date_peak))

ggsave(paste0(selected_outcome, "_Rt_over_time_criticalPeakDates.png"),
       plot = pplot, path = file.path(exp_dir), width = 12, height = 7, dpi = 300, device = "png"
)






