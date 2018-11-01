library(reshape2)
library(ape)
library(vegan)
library(nlme)
library(dplyr)
library(FD)
library(ggplot2)
library(ggfortify)

######## ANALYSING FISH DATA #########

#read in fish survey data japan 
fish_survey<-read.csv('FishData_JP_2016_final.csv')

#read in trait data
fish_trait<-read.csv('database_index10_25Oct2018.csv')

#read in australia survey species

aus_species_list<-read.csv('LongTransect_Subtropical_fish_list.csv')

aus_species<-aus_species_list$Fish


#check fish survey data and remove blank rows (7534 onwards) and columns (12:14)
glimpse(fish_survey)


fish_survey<-fish_survey[1:7533,1:11]

fish_survey$SpeciesFish<-as.character(fish_survey$SpeciesFish)

#remove duplicate species
#species name is wrong so change (JPN data )
fish_survey$SpeciesFish[fish_survey$SpeciesFish=="Apogon aureus"]<- "Ostorhinchus aureus"
fish_survey$SpeciesFish[fish_survey$SpeciesFish=='PLectroglyphidodon dickii']<-'Plectroglyphidodon dickii'
fish_survey$SpeciesFish[fish_survey$SpeciesFish=='Goniistius zebra']<-'Cheilodactylus zebra'
fish_survey$SpeciesFish[fish_survey$SpeciesFish=='Diagramma picta']<-'Diagramma pictum'
fish_survey$SpeciesFish[fish_survey$SpeciesFish=='Goniistius zonatus']<-'Cheilodactylus zonatus'
fish_survey$SpeciesFish[fish_survey$SpeciesFish=='Halichoeres poecilopterus']<-'Parajulis poecilepterus'
fish_survey$SpeciesFish[fish_survey$SpeciesFish=='Sebasticus marmoratus']<-'Sebastiscus marmoratus'
fish_survey$SpeciesFish[fish_survey$SpeciesFish=="Apogon doederleini"]<-'Ostorhinchus doederleini'
fish_survey$SpeciesFish[fish_survey$SpeciesFish=='Siganus stellatus']<-'Siganus punctatus'
fish_survey$SpeciesFish[fish_survey$SpeciesFish=="Apogon limenus"]<-'Ostorhinchus limenus'
fish_survey$SpeciesFish[fish_survey$SpeciesFish=="Chaetodon modestus"]<-'Roa modesta'


#now clean aus species
aus_species<-as.character(aus_species)

#add extra aus species 
aus_species<-c(aus_species, 'Scarus chameleon','Triaenodon obesus','Cheilinus aerenatus',
               'Scyliorhinidae','Priacanthus macracanthus','Priacanthus blochii','Cheilodipterus artus',
               'Pomacanthus imperator','Scarus flavipectoralis','Scarus rubroviolaceaus','Scarus fraenatus')


#species names wrong in aus data change/ edit
aus_species[aus_species== "Archamia zosterophora"]<-'Taeniamia zosterophora'
aus_species[aus_species=='Scarus rubroviolaceaus']<-'Scarus rubroviolaceus'
aus_species[aus_species=="Scarus fraenatus"]<-'Scarus frenatus'
aus_species[aus_species=='Cheilinus aerenatus']<-'Oxycheilinus arenatus'
aus_species[aus_species=='Archamia fucata']<-'Taeniamia fucata'
aus_species[aus_species=='Apogon limenus']<-'Ostorhinchus limenus'
aus_species[aus_species=="Chaetodon modestus"]<-'Roa modesta'
aus_species[aus_species=="Apogon capricornis"]<-'Ostorhinchus capricornis'
aus_species[aus_species=='Archamia leai']<-'Taeniamia leai'


#Fish Traits Cleaning----

#filter for just survey species
jp_species<-unique(fish_survey$SpeciesFish)
jp_species<-as.character(jp_species)
aus_species<-as.character(aus_species)

#combine japan and australia species
all_species<-c(jp_species, aus_species)

all_species<-unique(all_species)

#filter for our species
fish_trait$Species<-as.character(fish_trait$Species)

fish_trait_filter<- filter(fish_trait, Species %in% all_species ) 

all_species[c(which(!all_species %in% fish_trait$Species ))] #check which species are missing from traits- some missing from traits DB?


#Mouillot traits + extra relevant +maria 
colnames(fish_trait_filter)


#remove unnecessary traits
fish_trait_select<- fish_trait_filter[,c(2,11,17,21,30,36,38,39,41)]

colnames(fish_trait_select)

fish.traits<-fish_trait_select

rownames(fish.traits)<-fish.traits$Species

fish.traits<-fish.traits[,-1]

colnames(fish.traits)

#make sure all the blanks are NA
fish.traits[fish.traits == '']<-NA
fish.traits[fish.traits == ' ']<-NA

#check class
sapply(fish.traits, class)




#find species with lots of NAs in trait data

#which species have more then three NA 
which(rowSums(is.na(fish.traits)) >= 3)

fish.traits[417,]

#only one with >3NA ok. 

#check factor levels are ok
fish.traits$EnvTemp<-factor(fish.traits$EnvTemp)
levels(fish.traits$EnvTemp)

fish.traits$Trophic<-factor(fish.traits$Trophic)
levels(fish.traits$Trophic)

fish.traits$Aggregation<-factor(fish.traits$Aggregation)
levels(fish.traits$Aggregation)

#which(fish.traits$Aggregation=='harems')
#fish.traits$Aggregation[fish.traits$Aggregation=='harems']<-'groups'
#fish.traits$Aggregation<-factor(fish.traits$Aggregation)
#levels(fish.traits$Aggregation)

fish.traits$Position<-factor(fish.traits$Position)
levels(fish.traits$Position)

fish.traits$ParentalMode<-factor(fish.traits$ParentalMode)
levels(fish.traits$ParentalMode)

which(fish.traits$ParentalMode=='Nesters')
which(fish.traits$ParentalMode=='nesters')
fish.traits$ParentalMode[fish.traits$ParentalMode=='nesters']<-'Nesters'

fish.traits$ParentalMode<-factor(fish.traits$ParentalMode)
levels(fish.traits$ParentalMode)

#making trait ring ----
#making trait ring

#make distance matrix 
Fdissim = gowdis (fish.traits, asym.bin = NULL)

FDis_clust = hclust (Fdissim, method = "ward.D") 

dis.mat<-as.matrix(Fdissim)

### display as ring
FDisClus_phy = as.phylo(FDis_clust)
labelClust = as.character (row.names(fish.traits))
FDisClus_phy$tip.label = labelClust

colours=c('red','green','blue','orange','forestgreen', 'purple','yellow', 'black', 'grey')

clus9<-cutree(FDis_clust, k=9) #this needs to be changed?

plot(FDisClus_phy, type="fan", use.edge.length = TRUE, node.pos = NULL,
     show.tip.label = TRUE, show.node.label = FALSE, tip.color = colours[clus9] ,
     edge.width = 1, edge.lty = 1, font = 2, cex = 0.5, label.offset = 0.1)


fish_groups<-data.frame(cutree(FDis_clust, k=9))
colnames(fish_groups)<- 'func_group'

#write functional group
write.csv(fish_groups, 'fish_groups.csv')

