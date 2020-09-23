
library(tidyverse)
library(scales)
source("load_paths.R")

datAll_1 <- read_csv(file.path(simulation_output, "20200906_IL_quest_testSamples_rn21/trajectoriesDat.csv")) %>% mutate(exp_name="rn21")
uniqueScens = unique(datAll$scen_num)

datAll_2 <- read_csv(file.path(simulation_output, "20200903_IL_quest_testSamples_rn91/trajectoriesDat.csv")) %>% mutate(exp_name="rn91")
datAll_3 <- read_csv(file.path(simulation_output, "20200903_IL_quest_testSamples_rn58/trajectoriesDat.csv")) %>% mutate(exp_name="rn58")


datAll <- rbind(datAll_1,datAll_2 , datAll_3)
table(datAll$exp_name)


nsamples = c(25, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
nsamples = c(25, 50, 100, 200, 300, 400, 500, 600)

sampledats = list()
for(i in nsamples){
tempdat <- datAll %>% 
             mutate(time=as.numeric(time),
                    time=round(time,0)) %>% 
              group_by(exp_name) %>%
              filter(scen_num %in% 
              sample(uniqueScens,i, replace =T)) %>% 
              select( time,exp_name, scen_num, infected_All) %>%
              mutate(sample=i)
sampledats[[length(sampledats)+1]] <- tempdat
}

  
dat <- sampledats %>% bind_rows()

pplot <- ggplot(data=subset(dat, time==320)) +
  theme_minimal() +
  geom_boxplot(aes(x=as.factor(sample), y=infected_All, group=interaction(sample, exp_name), fill=exp_name))+
  labs(y="all infected",
       x="Number of samples",
       fill="random number",
       caption="Resample = TRUE")+
  scale_y_continuous(labels=comma)

ggsave(paste0( "Sample_comparison_resampled_v2.png"),
       plot = pplot, path = file.path(simulation_output, exp_name), width = 8, height = 5, device = "png"
)


nsamples = c(25, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)

sampledats = list()
for(j in c(1:50)){
for(i in nsamples){
  tempdat <- datAll_1 %>% 
    mutate(time=as.numeric(time),
           time=round(time,0)) %>% 
    group_by(exp_name) %>%
    filter(scen_num %in% 
             sample(uniqueScens,i, replace =T)) %>% 
    select( time,exp_name, scen_num, infected_All) %>%
    mutate(sample=i,
           ndraw=j)
  sampledats[[length(sampledats)+1]] <- tempdat
}
}


dat <- sampledats %>% bind_rows()

pplot <- dat %>%
  group_by(time,ndraw, exp_name, sample) %>%
  summarize(infected_All=median(infected_All)) %>%
  ggplot() +
  theme_minimal() +
  geom_line(aes(x=time, y=infected_All, group=interaction(ndraw, exp_name), col=exp_name))+
  labs(y="all infected",
       x="Time",
       fill="random number",
       caption="Resample = TRUE")+
  scale_y_continuous(labels=comma)+
  facet_wrap(~sample)

ggsave(paste0( "Sample_comparison_resampled_v1_overtime.png"),
       plot = pplot, path = file.path(simulation_output, exp_name), width = 8, height = 5, device = "png"
)



