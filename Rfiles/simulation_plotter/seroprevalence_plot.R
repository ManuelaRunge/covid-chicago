

library(readr)
seroprevalence_Oct1 <- read_csv("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/20200929_IL_mr_baseline/seroprevalence_Oct1.csv")
#seroprevalence_Oct1 <- read_csv("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/20200929_IL_mr_baseline/prevalence_Oct1.csv")

library(ggplot2)

seroprevalence_Oct1$region <- gsub("EMS-","",seroprevalence_Oct1$region)

pplot <- ggplot(data=subset(seroprevalence_Oct1, region!="All"))+theme_minimal() +
  geom_rect( xmin=-Inf, xmax=Inf,ymin=0.0424*100,ymax=0.142*100, fill="grey", alpha=0.05)+
  geom_hline(yintercept=c(Inf, -Inf)) + geom_vline(xintercept=c(Inf, -Inf)) + 
  geom_pointrange(aes(x=reorder(region,as.numeric(region)),y=CI_50*100,  ymin= CI_5*100, ymax=CI_95*100 ),color="deepskyblue4") +
  geom_hline(data=subset(seroprevalence_Oct1, region=="All"), aes(yintercept=CI_50*100), linetype="dashed")+
  labs(y="Seroprevalence (%)", x="Covid region")+
  customTheme


subset(seroprevalence_Oct1, region=="All")


ggsave(paste0("seroprevalence_Oct1.png"),
       plot = pplot,
       path = file.path("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/20200929_IL_mr_baseline/"), width = 8, height = 6, device = "png"
)
ggsave(paste0("seroprevalence_Oct1.pdf"),
       plot = pplot,
       path = file.path("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/20200929_IL_mr_baseline/"), width = 8, height = 6, device = "pdf"
)


