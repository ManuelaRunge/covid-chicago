

dir <- "C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/20200928_IL__test2_fitsm7/fitting/"


datList=list()
for(i in c(1:11)){
datList[[length(datList)+1]] <- read_csv(file.path(dir,"smooth_14/csv/", paste0("best_parameter_ranges_ems_",i,".csv")))
}



library(tidyverse)


datList %>% bind_rows() %>% group_by(region ) %>% filter( NLL==min( NLL))

dat <- datList %>% bind_rows() 
  
ggplot(data=dat, aes(x=socialDistance_time7, y=social_multiplier_7, col=NLL))+
  geom_point()



dat2 <- read_csv("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/20201005_IL_mr_fitsm7_local/fitting/smooth_7dayavrg_Aug1_Oct6/csv/best_parameter_ranges_combined.csv")

ggplot(data=dat2, aes(x=socialDistance_time7, y=social_multiplier_7, col=NLL))+
  geom_point()+
  scale_color_continuous(type = "viridis", direction=-1)

dat2 <- dat2 %>% group_by(region) %>% mutate(NLLgrp = cut(NLL, quantile(NLL)))

ggplot(data=dat2, aes(x=socialDistance_time7, y=social_multiplier_7,group=region, col=NLL))+
  geom_point()+
  scale_color_continuous(type = "viridis", direction=-1) +
  facet_wrap(~region)

pplist <-list()
for(i in c(1:11)){
  print(i)
  pplist[[length(pplist)+1]] <- ggplot(data=subset(dat2, region==i), aes(x=socialDistance_time7, y=social_multiplier_7,group=region, col=NLL))+
  geom_point()+theme_minimal()+
    geom_hline(yintercept = c(-Inf, Inf))+  geom_vline(xintercept = c(-Inf, Inf))+
  scale_color_continuous(type = "viridis", direction=-1) +
  facet_wrap(~region)
}


plot_grid(pplist[[1]],pplist[[2]],pplist[[3]],pplist[[4]],pplist[[5]],
          pplist[[6]],pplist[[7]],pplist[[8]],pplist[[9]],pplist[[10]],pplist[[11]])


