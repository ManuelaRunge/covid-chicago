###===========================================
###  Plot minimum detection of As and P per Rt
###===========================================

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


simdir = file.path("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/contact_tracing/20200731/")
pdfdir = file.path("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/project_notes/publications/covid_model_IL/pdfs_ais/")

exp_names <- list.dirs(simdir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("reopen_contactTracing",exp_names)]

RtDatList <- list()
for(exp_name in exp_names){
  RtDatList[[length(RtDatList)+1]] <- f_combineRtdat(exp_name)  
}

Rtbelow1 <- RtDatList %>% bind_rows()

Rtbelow1$scenario <- gsub("20200731_IL_reopen_contactTracing","", Rtbelow1$exp_name)
Rtbelow1$scenario[Rtbelow1$scenario ==""] <- "counterfactual"


customTheme <- f_getCustomTheme()

Rtbelow1$scenario_fct <- factor(Rtbelow1$scenario,
                                 levels = c("counterfactual", "HS40", "HS80", "TDonly", "HS40TD", "HS80TD"),
                                 labels = c(
                                   "current trend (comparison)",
                                   "increase detections to 40%",
                                   "faster testing and isolation",
                                   "increase detections to 80%",
                                   "increase detections to 40%\n& faster testing and isolation",
                                   "increase detections to 80%\n& faster testing and isolation"
                                 ))
                                 

pplot <- ggplot(data=subset(Rtbelow1))+ 
  geom_point(aes(x=currentRt , y= detection_success, fill=scenario_fct),col="azure4",shape=21, size=3) +
  geom_smooth(aes(x=currentRt , y= detection_success,col=scenario_fct,fill=scenario_fct), method="lm", alpha=0.3) +
  scale_y_continuous(lim=c(0,1), labels = function(x) x * 100 , expand=c(0,0)) +
  labs(x= expr(italic(R[t]) * " before contact tracing start"),
       y="Minimum required detection of \n a - and pre-symptomatic infections (%) ",
       color="Testing improvements for mild symptoms",
       fill="Testing improvements for mild symptoms") +
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2") +
  customTheme +
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(ncol=2))+
  guides(color=guide_legend(ncol=2))


ggsave(paste0("RTLE1_contactTracing_scatterPlot.pdf"),
       plot = pplot, path = file.path(pdfdir), width = 9.5, height =7,  device = "pdf"
)


