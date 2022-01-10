###### dif site PCA ####################
setwd("D:/corona_contingency/SDM_course_git")

library(vegan)
library(GGally)
library(tidyverse)
library(reshape2)

######################################

algae<- read.csv('dif_site_alage.csv')
coral<-read.csv('dif_site_coral.csv')
fish<-read.csv('dif_site_fish.csv')
molls<-read.csv('dif_site_mollusc.csv')
echino<-read.csv('dif_site_echino.csv')

#standardise within group

algae<- algae %>% group_by(group) %>% mutate(stand_change= scale(change))

coral<- coral %>% group_by(group) %>% mutate(stand_change= scale(change))

fish<- fish %>% group_by(group) %>% mutate(stand_change= scale(change))

molls<- molls %>% group_by(group) %>% mutate(stand_change= scale(change))

echino<- echino %>% group_by(group) %>% mutate(stand_change= scale(change))

sd(coral$stand_change[coral$group=='coral1'])
sd(algae$stand_change[algae$group=='algae2'])
sd(fish$stand_change[fish$group=='fish1'])
sd(molls$stand_change[molls$group=='mollusc1'])
sd(echino$stand_change[echino$group=='echino1'])


all_taxa<-list(algae, coral, fish, molls, echino)




#merge together

all_taxa<-do.call(rbind, all_taxa)

all_taxa<- all_taxa[,-1]

#spread into wide format
all_taxa$site<-as.character(all_taxa$site)
all_taxa$group<-as.character(all_taxa$group)

#scale each 


all_wide<- acast(all_taxa, group~site, value.var='stand_change')

#ok

#now nmds/ pca
##NAs in all wide dataset (some coral sites missing, replace with median data)
#all_wide[is.na(all_wide)]<-median(all_wide, na.rm=TRUE)

write.csv(all_wide, 'all_taxa_site_dif_stand.csv')

pca1<-rda(all_wide)
biplot(pca1)

#better plot, 
pca2<- prcomp(all_wide)
biplot(pca2)

pcout<-as.data.frame(pca2$x)
rownames(pcout)

pca2$x

p<-ggplot(pcout, aes(x=PC1, y=PC2, label=rownames(pcout)))+
   geom_text(position=position_jitter(width=1,height=1))

#colour by taxa
taxa<-data.frame(species=rownames(pcout), taxa='')
taxa$taxa<-as.character(taxa$taxa)
taxa[c(1:5),2]<-'algae'
taxa[c(6:11),2]<-'coral'
taxa[c(12:14),2]<-'echinoderm'
taxa[c(15:25),2]<-'fish'
taxa[c(26:31),2]<-'mollusc'

pcout$species<- rownames(pcout) 
pcout<-left_join(pcout, taxa, by='species')
  
p<-ggplot(pcout, aes(x=PC1, y=PC2, col=taxa ,label=species))+
 geom_text(position=position_jitter(width=1,height=1))+
  theme_bw()
 # geom_label_repel()

p

#try another plot
library(ggfortify)
install.packages('ggrepel')
library(ggrepel)


autoplot(prcomp(all_wide), data=taxa, col='taxa', shape=FALSE, label=TRUE, label.size= 4,
         loadings=TRUE, loadings.colour='grey', loadings.label=TRUE, loadings.label.size=3,
         loadings.label.repel=T,
         loadings.label.colour='dark grey')+
  #geom_label_repel(label=loadings.label)+
  theme_bw()




biplot(pca1,
       display = c("sites", 
                   "species"),
       type = c("text",
                "points"))

#Add "hulls"
ordihull(pca1,
         group = 'species')


data.dist

pca

mds1<- metaMDS(all_wide, distance='euclidian')

plot(mds1)

mds_plot<-ordiplot(mds1, type='n')
text(mds_plot, what=c('sites', 'species'), col='purple')

ordipointlabel(mds_plot, display='sites')
points(mds_plot, what='sites')
ordihull(mds_plot, groups=dune_habitat$Use, label=TRUE, col=cols_Use)


#run pca for change within 2 pixels from the coast 
algae<-read.csv('dif_values_all_algae_norm.csv')
coral<-read.csv('dif_values_all_coral_morph.csv')
fish<-read.csv('dif_values_all_fish_jan.csv')
molls<-read.csv('dif_values_all_molluscs_norm.csv')
echinos<-read.csv('dif_values_all_echinos_norm.csv')

algae$taxa<-'algae'
coral$taxa<-'coral'
fish$taxa<-'fish'
molls$taxa<-'molluscs'
echinos$taxa<-'echinos'

algae<- algae %>% filter(climate=='RCP85')
coral<- coral %>% filter(climate=='RCP85')
fish<-fish %>% filter(climate=='RCP85')
molls<- molls %>% filter(climate=='RCP85')
echinos<-echinos %>% filter(climate=='RCP85')

algae<- algae %>% group_by(group) %>% mutate(stand_change= scale(values))

coral<- coral %>% group_by(group) %>% mutate(stand_change= scale(values))

fish<- fish %>% group_by(group) %>% mutate(stand_change= scale(values))

molls<- molls %>% group_by(group) %>% mutate(stand_change= scale(values))

echinos<- echinos %>% group_by(group) %>% mutate(stand_change= scale(values))



all_taxa<-list(algae, coral, fish, molls, echinos)

all_taxa<- do.call(rbind, all_taxa)

all_taxa$group<-as.factor(all_taxa$group)
all_taxa$taxa<-as.factor(all_taxa$taxa)

all_taxa85<-all_taxa

all_taxa85$group<- as.character(all_taxa85$group)
all_taxa85$taxa<- as.character(all_taxa85$taxa)

all_taxa85<-all_taxa85[,-7]

library(tidyr)
all_taxa85 <- all_taxa85 %>% unite('group2', taxa:group)

pca_input<- all_taxa85[,c(3,6,7)]

pca_input$group2<-as.factor(pca_input$group2)

pca_input$y<- round(pca_input$y, digits=4)

pca_input_av<- pca_input %>% group_by(y, group2 ) %>% summarise(meanchange=mean(stand_change))




library(reshape2)

#make it wide
pca_input_wide<- dcast(pca_input_av,group2 ~y, value.var = 'meanchange')

rownames(pca_input_wide)<- pca_input_wide[,1]
pca_input_wide<- pca_input_wide[,-1]


#ok now run pca
pca2pixel<-prcomp(pca_input_wide)

biplot(pca2pixel)

pcout<-as.data.frame(pca2pixel$x)
rownames(pcout)



p<-ggplot(pcout, aes(x=PC1, y=PC2, label=rownames(pcout)))+
  geom_text(position=position_jitter(width=1,height=1))

p
#colour by taxa
taxa<-data.frame(species=rownames(pcout), taxa='')
taxa$taxa<-as.character(taxa$taxa)
taxa[c(1:5),2]<-'algae'
taxa[c(6:9),2]<-'coral'
taxa[c(10:12),2]<-'echinoderm'
taxa[c(13:23),2]<-'fish'
taxa[c(24:29),2]<-'mollusc'

taxa$species<-as.character(taxa$species)


pcout$species<- rownames(pcout) 
pcout<-left_join(pcout, taxa, by='species')

p<-ggplot(pcout, aes(x=PC1, y=PC2, col=taxa ,label=species))+
  geom_text(position=position_jitter(width=1,height=1))+
  theme_bw()
# geom_label_repel()

#colour by behaviour
#add the behaviours in excel as it's easier
write.csv2(taxa, 'group_behaviour.csv')



