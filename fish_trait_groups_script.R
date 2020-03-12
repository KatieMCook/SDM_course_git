#checking trait groups 
setwd("S:/Beger group/Katie Cook/Japan_data/SDM_course_git")

library(dplyr)

groups<-read.csv('fishtraitgroups_2301.csv')

length(unique(groups$Species))

groups<-groups[-c(which(groups$group==5)),]
groups$group[groups$group==13]<-5

groups<-groups[-c(which(groups$group==12)),]
groups$group[groups$group==14]<-12


towrite<- groups[,c(1,15)]

write.csv(towrite, 'fish_final_groups.csv')

traits<-groups

names(traits)

names(traits)<- c("Species"   ,      "ThermalAffinity" ,"BodySize" ,       "DepthRange" ,     "PLD"    ,         "Diet"   ,         "Aggregation" ,    "Position"  ,     
                  "ParentalMode"  ,  "JPN_sp"      ,    "AUS_sp"      ,    "RMI_sp"     ,     "CHK_sp"     ,     "MLD_sp"      ,    "cluster"      ,     "group_log"      
                  )

### trophic groups
par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
food = table(traits$Diet, traits$cluster)
barplot(food, main = "Diet", col=terrain.colors(length(rownames(food))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(food))), legend = rownames(food))

### Aggregation
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
agg = table(traits$Aggregation, traits$cluster)
barplot(agg, main = "Aggregation", col=terrain.colors(length(rownames(agg))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(agg))), legend = rownames(agg))

### Habitat Association - Position
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
pos = table(traits$Position, traits$cluster)
barplot(pos, main = "Habitat Association", col=terrain.colors(length(rownames(pos))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(pos))), legend = rownames(pos))

### Parental Mode
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
parent = table(traits$ParentalMode, traits$cluster)
barplot(parent, main = "Parental Mode", col=rainbow(length(rownames(parent))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = rainbow(length(rownames(parent))), legend = rownames(parent))



## First, do numerical traits with boxplots:
er = par(mfrow = c(2,2), pty = "s", mai=c(0.5,0.5,0.5,0.5))
boxplot(traits$DepthRange ~ traits$cluster, main = "DRange")
boxplot(traits$BodySize ~ traits$cluster, main = "MaxLength")
boxplot(traits$PLD ~ traits$cluster, main = "PLD")




#plot the abundance of these traits across space 
#add latitude
latlon<-read.csv('JP2015_16_waypoints.csv')
latlon<-latlon[,c(2,3,4)]

latlon$Site<-as.character(latlon$Site)

#add survey data
#read in fish survey data japan 
fish_survey<-read.csv('FishData_JP_2016_final.csv')
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

#see how fish ring groups are distributed across sites
fish.groups<-groups
fish.groups<-fish.groups[,c(1,2)]
names(fish.groups)<-c('SpeciesFish', 'group')

fish_survey_group<-left_join(fish.groups, fish_survey, by='SpeciesFish')

colnames(fish_survey_group)
fish_survey_group<-fish_survey_group[,c(1,2,3,7,8)]

#remove NA (so australia species)
fish_survey_group<-fish_survey_group[complete.cases(fish_survey_group),]

fish_survey_group<- fish_survey_group %>% group_by(SpeciesFish, SiteID, group) %>% summarise (number=sum(Number))

#summarise by number of group per site
func_group_site<- fish_survey_group %>% group_by(SiteID, group) %>% summarise(number=sum(number))

names(func_group_site)<-c('Site', 'group', 'number')




func_group_site$Site<-as.character(func_group_site$Site)

func_group_site<-merge(func_group_site, latlon, by='Site')

func_group_site<- func_group_site %>% group_by(Site) %>% mutate(total_no=sum(number))


func_group_site<-func_group_site %>% group_by(Site, group) %>% mutate(prop_total=number/total_no)

func_group_site$Site<-as.factor(func_group_site$Site)
func_group_site$group<-as.factor(func_group_site$group)


#plot
library(ggplot2)

ggplot(func_group_site, aes(x=lat, y=prop_total, col=group))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  ylim(0, 0.6)+
  labs(x='Latitude', y='Proportion of Community')+
  theme_bw()
