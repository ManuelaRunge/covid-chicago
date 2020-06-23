

#estimated_Rt <- read.csv(file.path(exp_dir,"20200615_IL_test_TD_AsP0", "estimated_Rt.csv"))

### Combine list to dataframe 
Rt_dat <- Rt_tempdat_All
rm(Rt_tempdat_All
)
table(Rt_dat$region)
table(Rt_dat$scen_num, Rt_dat$t_start)

#Rt_dat2 <- Rt_dat %>% group_by(t_start, scen_num) %>% mutate(reg = unique(dat$region) )

dat <- dat %>%
  arrange( Date) %>%
  group_by( scen_num) %>%
  mutate(date = as.Date(Date), time = c(1:n_distinct(date)))


### Edit dataframe
Rt_dat2 <- Rt_dat %>%
  merge(unique(dat[, c("time", "Date","scen_num", "d_Sym",
                       "reduce_testDelay_P_1", "reduce_testDelay_As_1","reduce_testDelay_Sym_1",
                       "reduced_inf_of_det_cases_ct1","d_Sym_ct1", "d_AsP_ct1", "fraction_symptomatic","fraction_severe")]),
        by.x = c("t_start", "scen_num"), by.y = c("time", "scen_num")) 

colnames(Rt_dat2) <- gsub("[(R]","",colnames(Rt_dat2))
colnames(Rt_dat2) <- gsub("[)]","",colnames(Rt_dat2))

Rt_dat2 <- Rt_dat2 %>% mutate(meanRtLE1 = ifelse(Median <1 , 1, 0))

write.csv(Rt_dat2, file=file.path(exp_dir, paste0(ems, "_estimated_Rt.csv")), row.names = FALSE)