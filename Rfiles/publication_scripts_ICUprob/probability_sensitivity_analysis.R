
#### Sensitivity analysis

library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")

theme_set(theme_bw())

simdate <-'20200919'
simdate <-'20201121'
sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)

exp_name <- "20201121_IL_regreopen50perc_7daysdelay_sm4"

sensitivity_dir <-  file.path(sim_dir,exp_name, "sensitivity_analysis_II")

run50_files <- list.files(sensitivity_dir, pattern="randsub_50")
run50_files <- list.files(sensitivity_dir, pattern="randsub_50")
run50_files <- list.files(sensitivity_dir, pattern="randsub_50")

all_files <-  list.files(sensitivity_dir, pattern="randsub")

datList <- list()
for(file in all_files){
  file_label <- gsub(".csv","", gsub("hospitaloverflow_randsub_","",file))
  file_label = strsplit(file_label, split="_rn")[[1]]
  datList[[length(datList)+1]] <- fread(file.path(sensitivity_dir, file)) %>% mutate(randsub=file_label[1], rn=file_label[2])
}

dat <- datList %>% bind_rows()


ggplot(data=dat)+
  geom_point(aes(x=capacity_multiplier, y=prob, group=interaction(rn,randsub), col=randsub))+
  facet_grid(randsub~geography_modeled, scales="free")


ggplot(data=dat)+
  geom_point(aes(x=capacity_multiplier, y=prob, group=interaction(rn,randsub)),col="grey",alpha=0.4)+
  geom_smooth(aes(x=capacity_multiplier, y=prob, group=randsub, col=randsub))+
  facet_grid(~geography_modeled, scales="free")



datAggr <- dat %>% group_by(capacity_multiplier,geography_modeled,randsub) %>% 
  summarize(  min.prob = min(prob, na.rm = TRUE),
              max.prob = max(prob, na.rm = TRUE),
              median.prob = median(prob, na.rm = TRUE),
              q25.prob = quantile(prob, probs = 0.25, na.rm = TRUE),
              q75.prob = quantile(prob, probs = 0.75, na.rm = TRUE),
              q2.5.prob = quantile(prob, probs = 0.025, na.rm = TRUE),
              q97.5.prob = quantile(prob, probs = 0.975, na.rm = TRUE))%>%
  mutate(randsub=as.factor(as.numeric(randsub)))


ggplot(data=subset(datAggr))+
  geom_ribbon(aes(x=capacity_multiplier, ymin=q25.prob, ymax=q75.prob, group=randsub, fill=randsub),alpha=0.4)+
  geom_ribbon(aes(x=capacity_multiplier, ymin=q2.5.prob, ymax=q97.5.prob, group=randsub, fill=randsub),alpha=0.4)+
  geom_line(aes(x=capacity_multiplier, y=median.prob, group=randsub, col=randsub),size=1)+
  facet_grid(randsub~geography_modeled, scales="free")+
  scale_y_continuous(breaks=seq(0,1,0.1))

ggplot(data=subset(datAggr))+
  geom_ribbon(aes(x=capacity_multiplier, ymin=q25.prob, ymax=q75.prob, group=randsub, fill=randsub),alpha=0.4)+
 # geom_ribbon(aes(x=capacity_multiplier, ymin=q2.5.prob, ymax=q97.5.prob, group=randsub, fill=randsub),alpha=0.4)+
  geom_line(aes(x=capacity_multiplier, y=median.prob, group=randsub, col=randsub),size=1)+
  facet_grid(~geography_modeled, scales="free")
