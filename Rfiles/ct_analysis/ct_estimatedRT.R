


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

ggplot(data= subset(Rt_dat2, region==1)) +
  theme_minimal() +
  geom_line(aes(x=Date, y =Mean , group=scen_num),col="darkgrey") +
  geom_smooth(aes(x=Date, y =Mean ),col="darkred") +
  scale_x_date(breaks = "1 month", labels = date_format("%b")) +
  geom_hline(yintercept = 1)


