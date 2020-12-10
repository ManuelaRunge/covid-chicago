

library(readxl)
library(tidyverse)
library(cowplot)
Book1 <- read_excel("C:/Users/mrm9534/Box/MR_archive/NU/projects/covid/Book1.xlsx",  sheet = "model_time_events")
Book1$Date <- as.Date(Book1$Date)

ggplot(data=Book1,aes(x=Date,y=ymax, ymax=ymax, ymin=ymin, label=nr, col=label, group=interaction(label, nr) )) + 
  geom_pointrange(size=1)+
  geom_label(size=5, vjust=0.2, show.legend = F)+
  scale_y_continuous(expand=c(0,0), lim=c(0, 1.1))+
  theme_cowplot()+
  labs(y="", color="")+
  theme(axis.line.y =element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y =element_blank())+
  scale_color_brewer(palette="Dark2")+
  scale_x_date(breaks = "2 weeks",date_labels = "%d\n%b")
