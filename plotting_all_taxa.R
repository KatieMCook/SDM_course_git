library(ggplot2)
library(dplyr)

setwd("S:/Beger group/Katie Cook/Japan_data/SDM_course_git")


algae<-read.csv('dif_values_all_algae_norm.csv')
coral<-read.csv('dif_values_all_coral_norm.csv')
fish<-read.csv('dif_values_all_fish_norm.csv')
molls<-read.csv('dif_values_all_molluscs_norm.csv')


algae$taxa<-'algae'
coral$taxa<-'coral'
fish$taxa<-'fish'
molls$taxa<-'molluscs'

all_taxa<-list(algae, coral, fish, molls)

all_taxa<- do.call(rbind, all_taxa)

all_taxa$group<-as.factor(all_taxa$group)
all_taxa$taxa<-as.factor(all_taxa$taxa)

ggplot(all_taxa, aes(x=slope, y=values, col=group))+
  geom_smooth(method='loess')+
  facet_grid(taxa~climate, scales='free')+
  theme_bw()+
  labs(x='slope', y='change in abundance between 2014-2050')

ggplot(all_taxa, aes(x=y, y=values, col=group))+
  geom_smooth(method='loess')+
  facet_grid(taxa~climate)+ #scales='free'
  theme_bw()+
  labs(x='Latitude', y='change in abundance between 2014-2050')
