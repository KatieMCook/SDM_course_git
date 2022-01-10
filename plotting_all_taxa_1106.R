library(ggplot2)
library(dplyr)

#setwd("S:/Beger group/Katie Cook/Japan_data/SDM_course_git")
setwd("D:/corona_contingency/SDM_course_git")

algae<-read.csv('dif_values_all_algae_norm.csv')
coral<-read.csv('dif_values_all_coral_morph.csv')
fish<-read.csv('dif_values_all_fish_jan.csv')
molls<-read.csv('dif_values_all_molluscs_norm.csv')
echinos<-read.csv('dif_values_all_echinos_norm.csv')

algae$taxa<-'algae'
coral$taxa<-'coral'
fish$taxa<-'fish'
molls$taxa<-'molluscs'
echinos$taxa<-'echinoderms'

all_taxa<-list(algae, coral, fish, molls, echinos)

all_taxa<- do.call(rbind, all_taxa)

all_taxa$group<-as.factor(all_taxa$group)
all_taxa$taxa<-as.factor(all_taxa$taxa)

#remove group 1 for now as its messing things up 
torm<-which(all_taxa$group==1 & all_taxa$taxa=='fish')

wogr1<- all_taxa[-c(torm),]

library(RColorBrewer)

ggplot(wogr1, aes(x=y, y=values, col=group))+
  geom_smooth(method='loess')+
  facet_grid(taxa~climate, scales='free')+
  geom_hline(yintercept=0, linetype='dotted')+
  scale_color_brewer(palette='Paired' )+
  theme_bw()+
  labs(x='Latitude', y='Change in abundance between 2014-2050')



ggplot(all_taxa, aes(x=y, y=values, col=group))+
  geom_smooth(method='loess')+
  facet_grid(taxa~climate)+ #scales='free'
  theme_bw()+
  labs(x='Latitude', y='change in abundance between 2014-2050')

#just rcp8.5

all_taxa85<- all_taxa %>% filter(climate=='RCP85')

#fish group 1 declines 
p<- ggplot(all_taxa85, aes(x=y, y=values, col=group))+
  geom_smooth(method='loess')+
  facet_wrap(~taxa, scales='free' )+
  scale_color_brewer(palette='Paired' )+
  theme_bw()+
  labs(x='Latitude', y='change in abundance between 2014-2050')


#reorder
#remove group 1 for now as its messing things up 
torm<-which(all_taxa85$group==1 & all_taxa85$taxa=='fish')
all_taxa85<-all_taxa85[-torm,]



all_taxa85$taxa<- factor(all_taxa85$taxa, levels=c('fish','algae','molluscs','echinoderms','coral'   ))
  
p<- ggplot(all_taxa85, aes(x=y, y=values, col=group))+
  geom_smooth(method='loess')+
  geom_hline(yintercept = 0)+
  facet_wrap(~taxa, scales='free' )+
  scale_color_brewer(palette='Paired' )+
  theme_bw()+
  labs(x='Latitude', y='change in abundance between 2014-2050')

p

p +guides(fill=guide_legend(ncol=2,byrow=TRUE)) 
  
  theme(legend.position = c(0.9, 0.1)
          ,legend.background = element_rect(fill = "white", colour = NA))

p
  

library(gridExtra)
coral85<-coral %>% filter(climate=='RCP85')

#remove group 1 for now as its messing things up 
torm<-which(all_taxa85$group==1 & all_taxa85$taxa=='fish')
all_taxa85<-all_taxa85[-torm,]

cplot<- ggplot(all_taxa85, aes(x=y, y=values, col=group))+
                 geom_smooth(method='loess')+
  theme_bw()

cploy


#make a plot of matching patterns 
 
fish6<-all_taxa85[all_taxa85$group==6 & all_taxa85$taxa=='fish',]
algae2<-all_taxa85[all_taxa85$group==2 & all_taxa85$taxa=='algae',]
echino3<-all_taxa85[all_taxa85$group==3 & all_taxa85$taxa=='echinoderms',]

pattern1<-rbind(fish6,algae2)
pattern1<-rbind(pattern1,echino3)

p1<- ggplot(pattern1, aes(x=y, y=values, col=group))+
  geom_smooth(method='loess')+
  geom_hline(yintercept = 0)+
  facet_wrap(~taxa, scales='free' )+
  scale_color_brewer(palette='Paired' )+
  theme_bw()+
  labs(x='Latitude', y='change in abundance between 2014-2050')
p1


fish8<-all_taxa85[all_taxa85$group==8 & all_taxa85$taxa=='fish',]
algae3<-all_taxa85[all_taxa85$group==3 & all_taxa85$taxa=='algae',]
echino3<-all_taxa85[all_taxa85$group==3 & all_taxa85$taxa=='echinoderms',]

