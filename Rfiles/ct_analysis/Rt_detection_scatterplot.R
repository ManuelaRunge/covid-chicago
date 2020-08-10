

### FOR HS scenarios only
#baselineRt <- RtDatHS %>% filter(Date ==as.Date("2020-03-01")) %>% select(reopening_multiplier_4,scenario_fct ) %>% summarize(mean.val=mean.val)
#currentRt <- 
  

f_combineRtdat <- function(exp_name ="20200731_IL_reopen_contactTracing"){
EMS_combined_estimated_Rt  <- read.csv(file.path(simdir, exp_name , "estimatedRt/EMS_combined_estimated_Rt.csv"))

baselineRt <- EMS_combined_estimated_Rt %>% 
              filter(Date ==as.Date("2020-03-01")) %>% 
              dplyr::group_by(region, grpvar) %>% 
              dplyr::summarize(baselineRt=mean(Mean))

currentRt <- EMS_combined_estimated_Rt %>% 
              filter(Date ==as.Date("2020-07-20")) %>% 
              dplyr::group_by(region, grpvar) %>% 
              dplyr::summarize(currentRt=mean(Mean))
  
  
Rtbelow1 <- EMS_combined_estimated_Rt %>% 
  filter(Date == as.Date("2020-09-01")) %>% 
  filter(Mean < 1.0) %>%
  dplyr::group_by(region, grpvar) %>% 
  dplyr::summarize(Rtbelow1=mean(Mean),
                   detection_success =min(detection_success)) %>%
  left_join(baselineRt, by=c("region","grpvar")) %>%
  left_join(currentRt, by=c("region","grpvar")) %>%
  mutate(exp_name = exp_name)


return(Rtbelow1)
}


simdir = c("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/contact_tracing/20200731/")


exp_names <- list.dirs(simdir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("reopen_contactTracing",exp_names)]

RtDatList <- list()
for(exp_name in exp_names){
  RtDatList[[length(RtDatList)+1]] <- f_combineRtdat(exp_name)  
}

Rtbelow1 <- RtDatList %>% bind_rows()

Rtbelow1$scenario <- gsub("20200731_IL_reopen_contactTracing","", Rtbelow1$exp_name)
Rtbelow1$scenario[Rtbelow1$scenario ==""] <- "base"


ggplot(data=subset(Rtbelow1))+ 
  geom_point(aes(x=currentRt , y= detection_success, fill=scenario),col="azure4",shape=21, size=3) +
  geom_smooth(aes(x=currentRt , y= detection_success,col=scenario,fill=scenario, group=scenario), method="lm", alpha=0.3) +
  scale_y_continuous(lim=c(0,1)) +
  labs(y="detection As & P ",
       color="",fill="") +
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")


