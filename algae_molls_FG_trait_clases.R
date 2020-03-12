## algae and molls fgS
library(ggplot2)
library(dplyr)

setwd("S:/Beger group/Katie Cook/Japan_data/SDM_course_git")

groups<-read.csv('algae_clust_1107.csv')

length(unique(groups$Species))

groups$group<-as.factor(groups$group)

#first box plots for numeric

attach(groups)
par(mfrow=c(1,2))
boxplot(Thallus.Height..cm....max~group, main='Max Height')
boxplot(Depthrange~group, main='Depth Range')


#now barcharts
#holdfast
par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
hold<-table(Holdfast.morphology, group)
hold
barplot(hold, main = "Holdfast Morphology", col=topo.colors(length(rownames(hold))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = topo.colors(length(rownames(hold))), legend = rownames(hold))

#support
support<-table(Support.mechanism.KMC, group)
barplot(support, main='Support Mechanism', col=topo.colors(length(rownames(support))), width=2)
legend ("topright", inset = c(-0.25, 0), fill = topo.colors(length(rownames(support))), legend = rownames(support))

#Substrate
sub<-table(Substrate, group)
barplot(sub, main='Substrate', col=topo.colors(length(rownames(sub))), width=2)
legend ("topright", inset = c(-0.25, 0), fill = topo.colors(length(rownames(sub))), legend = rownames(sub))

#Tidal Zone 
tide<-table(Tidal.Zone, group)
barplot(tide, main='Tidal Zone', col=topo.colors(length(rownames(tide))), width=2)
legend ("topright", inset = c(-0.25, 0), fill = topo.colors(length(rownames(tide))), legend = rownames(tide))

#Reproduction
rep<- table(Reproduction, group)

barplot(rep, main='Reproduction', col=topo.colors(length(rownames(rep))), width=2)
legend ("topleft", inset = c(-0.25, 0), fill = topo.colors(length(rownames(rep))), legend = rownames(rep))



#now molls
groups<- read.csv('mollusc_clust1107.csv')

length(unique(groups$Mollusc.Species))

#first numeric
attach(groups)
par(mfrow=c(1,2))

boxplot(depth_range~ group_k7, main='Depth Range')
boxplot(max_size_mm~ group_k7, main='Max Size')

groups<-rename(group= group_k7, groups)

detach(groups)
#Tidal Zone 
par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
tide<-table(groups$Tidal.Zone, groups$group)
barplot(tide, main='Tidal Zone', col=topo.colors(length(rownames(tide))), width=2)
legend ("topright", inset = c(-0.25, 0), fill = topo.colors(length(rownames(tide))), legend = rownames(tide))

#trophic
food<-table(groups$Trophic, groups$group)
barplot(food, main='Trophic Level', col=topo.colors(length(rownames(food))), width=2)
legend ("topright", inset = c(-0.25, 0), fill = topo.colors(length(rownames(food))), legend = rownames(food))

#habitat
hab<-table(groups$Habitat_KMC, groups$group)
barplot(hab, main='Habitat', col=topo.colors(length(rownames(hab))), width=2)
legend ("topright", inset = c(-0.25, 0), fill = topo.colors(length(rownames(hab))), legend = rownames(hab))
hab

#position
pos<- table(groups$position_KMC, groups$group)
pos

#mobility
mob<-table(groups$adult.mobility..Paganelli.et.al.., groups$group)
mob

#shell cat
shell<-table(groups$Shell_category_KMC, groups$group)
shell
