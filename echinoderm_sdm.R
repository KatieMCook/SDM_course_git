###ECHINODERMS SCRIPT ######

library(sdmpredictors)
library(dismo)
library(rJava)
library(reshape2)
library(dplyr)
library(maptools)
library(mapdata)
library(ggmap)
library(sf)
library(rgeos)
library(vegan)
library(corrplot)
library(usdm)
library(RStoolbox)
library(sdm)
library(ggmap)
library(ggplot2)
library(raster)
library(randomForest)

setwd("S:/Beger group/Katie Cook/Japan_data/SDM_course_git")

#read in Japan if starting ###### START HERE 
japan_outline<-readOGR('plotting/japan_outline.shp')

#check
plot(japan_outline)


##explore the data 
marine_layers<- list_layers(marine=TRUE)


#list layers from bio-oracle
layers.bio2 <- list_layers( datasets="Bio-ORACLE" ) 

#explore future layers 
future_layers<-list_layers_future( marine = TRUE)


#extract data 
current_preds<-load_layers(c('BO_sstmin','BO2_curvelmax_ss', 'BO2_salinitymean_ss'))
res(current_preds)
plot(current_preds)

chlo<-load_layers('BO2_chlomean_ss')
res(chlo)
plot(chlo)

#get my extent to crop
#add lat lon 
latlon<-read.csv('JP2015_16_waypoints.csv')
site_group<-read.csv('SiteID_Name_map.csv')
names(site_group)<-c('Name', 'Site')

site_group_lat<-left_join(latlon, site_group, by='Site')
write.csv(site_group_lat, 'site_group_lat.csv')

# Determine geographic extent of our data
max.lat <- ceiling(max(latlon$lat)+0.5)
min.lat <- floor(min(latlon$lat)-0.5)

max.lon <- ceiling(max(latlon$lon)+0.5)
min.lon <- floor(min(latlon$lon)-0.5)
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

#crop the data to extent and combine
current_preds<-crop(current_preds, geographic.extent)

#plot
plot(current_preds)

#crop chloro
chlo<-crop(chlo, geographic.extent)

plot(chlo)

current_preds<-stack(current_preds, chlo)

plot(current_preds, main=c('SST', 'Current Velocity', 'Salinity', 'Chlorophyll'), mai = c(1, 0.1, 0.1, 0.1))
plot(japan_outline, add=TRUE)


#extract future 
#RCP 85 2050
future_2050<-filter(future_layers, year==2050, scenario=='RCP85')

RCP85_2050<-load_layers(c(c('BO2_RCP85_2050_tempmin_ss','BO2_RCP85_2050_curvelmean_bdmin',
                            'BO2_RCP85_2050_salinitymean_ss')))

chlo_future<-load_layers('BO2_RCP85_2050_chlomean_ss')

#now crop
RCP85_2050<-crop(RCP85_2050, geographic.extent)

chlo_future<-crop(chlo_future, geographic.extent)

RCP85_2050<-stack(RCP85_2050, chlo_future)

#RCP 26 2050
future_2050<-filter(future_layers, year==2050, scenario=='RCP26')

RCP26_2050<-load_layers(c(c('BO2_RCP26_2050_tempmin_ss','BO2_RCP26_2050_curvelmean_bdmin',
                            'BO2_RCP26_2050_salinitymean_ss')))

chlo_future26<-load_layers('BO2_RCP26_2050_chlomean_ss')

#RCP26
RCP26_2050<-crop(RCP26_2050, geographic.extent)
chlo_future26<-crop(chlo_future26, geographic.extent)

RCP26_2050<-stack(RCP26_2050, chlo_future26)

plot(RCP26_2050)



#read in predict area
predict_area<-readOGR('plotting/japan_predictarea.shp')

plot(predict_area)

predict_area<-aggregate(predict_area)

plot(predict_area)

crs(predict_area)

plot(japan_outline)

plot(predict_area, add=TRUE)

crs(predict_area)<-('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' )

crs(predict_area)


#ok good

crs(predict_area)
crs(japan_outline)

#mask with the area

plot(current_preds[[1]])

area_pred<-mask(current_preds, predict_area)

plot(area_pred[[1]])

plot(japan_outline, add=TRUE)

crs(japan_outline)

future_preds<-mask(RCP85_2050, predict_area)

RCP85_2050<-mask(RCP85_2050, predict_area)
RCP26_2050<-mask(RCP26_2050, predict_area)


#now add the survey data
#read in echinoderm data
ech<-read.csv('echinoderms_survey_clean.csv')

groups<-read.csv('ech_clust_clean.csv')
groups<-groups[,c(18:21)]


#check if theres species that dont match 
groups$new_names<-as.character(groups$new_names)
ech$Species<-as.character(ech$Species)

not_in_groups<- which ( ! unique (groups$new_names) %in% unique (ech$Species))
not_in_surv<- which ( ! unique (ech$Species) %in% unique (groups$new_names))

groups$new_names[not_in_groups]
ech$Species[not_in_surv] #just acanthaster planci 3 times? sort this later if need to 


jpn_filter<-which(groups$new_names %in% ech$Species)

groups<-groups[jpn_filter,]

summary(groups)

groups<- groups %>% mutate(sum=1)

k3<- groups %>% group_by(groupk3) %>% summarise(number=sum(sum))
k11<-groups %>% group_by(groupk11) %>% summarise (number=sum(sum))

#ok merge by k11


which ( ! unique (groups$new_names) %in% unique (ech$Species))
which ( ! unique (ech$Species) %in% unique (groups$new_names))

ech[10,3]<-"Acanthaster planci"
ech[17,3]<-"Acanthaster planci"
ech[29,3]<-"Acanthaster planci"

k11<-groups[,c(2,4)]

names(k11)<-c('group','Species')

ech_group<-left_join(ech, k11, by='Species')

torm<-which(is.na(ech_group$group))
ech_group[871,]

ech_group<-ech_group[-c(torm),]

#ok now summarise groups over site
group_sum<- ech_group %>% group_by(SiteID, lat, lon, group) %>% summarise(abundance=sum(number))

group_sum$group<-as.factor(group_sum$group)

#ok plot abundance by lat 
ggplot(group_sum, aes(x=lat, y=abundance))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~group, scales='free')+
  theme_bw()

