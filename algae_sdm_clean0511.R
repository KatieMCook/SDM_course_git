
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
current_preds<-load_layers(c('BO_sstmin','BO2_curvelmax_ss', 'BO2_salinitymean_ss','BO2_lightbotltmax_bdmin'))
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

plot(current_preds, main=c('SST', 'Current Velocity', 'Salinity', 'Bottom Light', 'Chlorophyll'), mai = c(1, 0.1, 0.1, 0.1))
plot(japan_outline, add=TRUE)


#extract future 
#RCP 85 2050
future_2050<-filter(future_layers, year==2050, scenario=='RCP85')


RCP85_2050<-load_layers(c(c('BO2_RCP85_2050_tempmin_ss','BO2_RCP85_2050_curvelmean_bdmin',
                            'BO2_RCP85_2050_salinitymean_ss')))

light<-load_layers('BO2_lightbotltmax_bdmin')
chlo_future<-load_layers('BO2_RCP85_2050_chlomean_ss')


#RCP 26 2050
future_2050<-filter(future_layers, year==2050, scenario=='RCP26')

RCP26_2050<-load_layers(c(c('BO2_RCP26_2050_tempmin_ss','BO2_RCP26_2050_curvelmean_bdmin',
                           'BO2_RCP26_2050_salinitymean_ss')))

chlo_future26<-load_layers('BO2_RCP26_2050_chlomean_ss')


#now crop
#RCP85
RCP85_2050<-crop(RCP85_2050, geographic.extent)

light<-crop(light, geographic.extent)
chlo_future<-crop(chlo_future,  geographic.extent)

RCP85_2050<-stack(RCP85_2050, light, chlo_future)

#RCP26
RCP26_2050<-crop(RCP26_2050, geographic.extent)
chlo_future26<-crop(chlo_future26, geographic.extent)

RCP26_2050<-stack(RCP26_2050, light, chlo_future26)

plot(RCP26_2050)


#read in predict area
predict_area<-readOGR('plotting/japan_predictarea.shp')

plot(predict_area)

predict_area<-aggregate(predict_area)

plot(predict_area)

predict_area_sf<-st_as_sf(predict_area)

st_write(predict_area_sf, 'predict_area_crop.shp')

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

RCP85_2050<-mask(RCP85_2050, predict_area)
RCP26_2050<-mask(RCP26_2050, predict_area)

#ok now get algae data (cleaned from algae_sdm script)
algae_all<-read.csv('algae_FG_site_abun.csv')
algae_all<-algae_all[,-1]

#now summarise by site
algae_all<- algae_all %>% group_by(Site, group) %>% summarise(abundance=sum(abundance))

#now add lat lon

latlon<-latlon[,c(2:4)]

algae_all<-left_join(algae_all, latlon, by='Site')
algae_all<-algae_all[,c(2:5)]

algae_all$group<-as.factor(algae_all$group)

#now plot groups across latitude
ggplot(algae_all, aes(y=abundance, x=lat, col=group))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)


#plot proportions 
site_prop<- algae_all %>% group_by( lat, lon) %>% mutate(total_abun= sum(abundance)) %>% mutate(prop= abundance/total_abun)

ggplot(site_prop, aes(y=prop, x=lat, col=group))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)


#write just in case
write.csv(algae_all, 'algae_site_FG.csv')

#ok now split into individual groups----
for (i in 1:length(unique(algae_all$group))){
  rows<-which(algae_all$group==i)
  assign(paste0("Obvs_gr_", i), data.frame(algae_all[rows,]))
}


#loop all groups ----

v1<-vifstep(current_preds, th=10) #nothing got rid of
v1

library(lme4)
library(mgcv)
library(gamm4)

rmse.bootstrap<-data.frame('glm'=1, 'glmer'=1, 'gam'=1, numbertest=1)

rmse <- function(o,p) {
  e <- o - p
  sqrt(mean(e^2,na.rm=T))
}



##now run loop for alllllllll ----

#extract the environmental values at lat lons 

group_list<-lapply(ls(pattern="Obvs_gr_"),get)

extract_km<-function(data){
  latlon<-data.frame(lon=data$lon, lat=data$lat)
  extract1<-as.data.frame(extract(current_preds, latlon))
  extract1$lat<-latlon$lat
  extract1$lon<-latlon$lon
  extract1$abundance<-data$abundance
  extract1
  return(extract1)
}

extract_all<-lapply(group_list, extract_km)

#create models 
#now model in loop with 15% test 85% training (n=5)



models_all<- function(data){
  
  rmse_all<-data.frame(glmR=1, gamR=1, rfR=1, glmP=1, gamP=1, RFP=1)  

  
  for (i in 1:100){
    
    #define test and training data
    test<- data[sample(nrow(data), size=5, replace=FALSE),]  
    train<- data[(! row.names(data) %in% row.names(test)), ]
    
    obvs<-test$abundance
    
    #make models
    glm1<-glm(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss +BO2_lightbotltmax_bdmin  , family=poisson, data=train)
    gam1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss,k=5) + s( BO2_lightbotltmax_bdmin, k=5 ), family=poisson, data=train)
    rf1<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss +BO2_lightbotltmax_bdmin +
                        BO2_chlomean_ss, data=train, ntree=300, importance=TRUE   )
    
    
    #predict models for test
    prglm<-predict( glm1, test)
    prgam<-predict(gam1, test)
    prRF<-predict(rf1, test)
    
    #now rmse for all
    rmse_all[i,1]<-rmse(obvs, prglm)
    rmse_all[i,2]<-rmse(obvs, prgam)
    rmse_all[i,3]<-rmse(obvs, prRF)
    
    #now pearsons correlation for all 
   rmse_all[i,4]<-cor(obvs, prglm, method = c("pearson"))
  rmse_all[i,5]<-cor(obvs, prgam, method = c("pearson"))
   rmse_all[i,6]<-cor(obvs, prRF, method = c("pearson"))
  }
  
  return(rmse_all) 

  
}

allrmse<-lapply(extract_all, models_all)
allrmse[[1]]

#5 is weird? 
allrmse[[5]]
extract_all[[5]]  #only present in 5 sites? how do I get around this 

allrmse[[4]]
extract_all[[4]]

#get averages RMSE

average_km<-function(data){
  
  glm_av<- mean(data$glmR)
  gam_av<-mean(data$gamR)
  rf_av<-mean(data$rfR)
  
  averages<-data.frame(glm_av, gam_av, rf_av)
  
  return(averages)
  
}

averages<-lapply(allrmse, average_km)

averages[[1]]
averages[[2]]
averages[[3]]
averages[[4]]
averages[[5]]


#averages pearsons 
average_p<-function(data){
  
  glm_av<- mean(data$glmP, na.rm=TRUE)
  gam_av<-mean(data$gamP, na.rm=TRUE)
  rf_av<-mean(data$RFP, na.rm=TRUE)
  
  averages<-data.frame(glm_av, gam_av, rf_av)
  
  return(averages)
  
}

average_pear<-lapply(allrmse, average_p)

average_pear[[1]]
average_pear[[2]]
average_pear[[3]]
average_pear[[4]]
average_pear[[5]]

#gam is actually ok, use #no wait actuakky they massively overfit,,, get rid of them 

#full model, predict and ensemble   #loop

for (i in 1:length(extract_all)) {
  
  glm_gr<-glm(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , family=poisson, data=extract_all[[i]])
 # gam_gr<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss,k=5) + s( BO2_lightbotltmax_bdmin, k=5 ), family=poisson, data=extract_all[[i]])
  rf_gr<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + 
                        BO2_chlomean_ss, data=extract_all[[i]], ntree=300, importance=TRUE   )
  
  assign(paste0('glm_gr', i), glm_gr)
 # assign(paste0('gam_gr', i), gam_gr)
  assign(paste0('rf_gr', i), rf_gr)
  
  pr_glm<-predict(area_pred, glm_gr)
#  pr_gam<-predict(area_pred, gam_gr)
  pr_rf<-predict(area_pred, rf_gr)    
  
  assign(paste0('pr_glm_gr', i), pr_glm)
 # assign(paste0('pr_gam_gr', i), pr_gam)
  assign(paste0('pr_rf_gr', i), pr_rf)
  
  props<-data.frame(glmR=1, rfR=1, glmP=1, rfP=1)
  props[1,1]<-1-(averages[[i]][1,1]/(averages[[i]][1,1]+averages[[i]][1,3]))
  props[1,2]<-1-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,3]))

  props[1,3]<-abs(average_pear[[i]][1,1])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,3]))
  props[1,4]<-abs(average_pear[[i]][1,3])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,3]))
  
  props<-data.frame(glm=((props[1,1]+props[1,3])/(props[1,1]+props[1,2]+props[1,3]+props[1,4])), 
                    rf=((props[1,2]+props[1,4])/(props[1,1]+props[1,2]+props[1,3]+props[1,4])))
  
  assign(paste0('prop_gr', i), props)
  
  ensemble<- ((pr_glm*props[1,1])+(pr_rf*props[1,2]))
  
  assign(paste0('ensemble_gr', i), ensemble)
  
  
}

#plot ensembles
library(viridis)
pal<-viridis(option='plasma', direction=-1, n=40)


ens_list<-lapply(ls(pattern="ensemble_gr"),get)

par(mar=c(1.2,1.2,1.2,1.2))
par(mfrow=c(2,3))

for ( i in 1:length(ens_list)){
  plot(ens_list[[i]], main= paste0('Group',i), col=pal )
  plot(japan_outline, add=TRUE, col='light grey', border='black')
  box()
  
}


#now future RCP85
rm(glm_gr)

glm_list<-lapply(ls(pattern='glm_gr'), get)

rm(rf_gr)

rf_list<-lapply(ls(pattern='rf_gr'), get)

#make sure the colnames match
names(RCP85_2050)<-names(current_preds)
plot(future_preds)


for (i in 1:length(extract_all)) {
  
  pr_glm_fut<-predict(RCP85_2050, glm_list[[i]])
  pr_rf_fut<-predict(RCP85_2050, rf_list[[i+5]])
  
  assign(paste0('glm_fut_85', i), pr_glm_fut)
  assign(paste0('rf_fut_85', i), pr_rf_fut)
  
  props<-data.frame(glmR=1, rfR=1, glmP=1, rfP=1)
  props[1,1]<-1-(averages[[i]][1,1]/(averages[[i]][1,1]+averages[[i]][1,3]))
  props[1,2]<-1-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,3]))
  
  props[1,3]<-abs(average_pear[[i]][1,1])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,3]))
  props[1,4]<-abs(average_pear[[i]][1,3])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,3]))
  
  props<-data.frame(glm=((props[1,1]+props[1,3])/(props[1,1]+props[1,2]+props[1,3]+props[1,4])), rf=((props[1,2]+props[1,4])/(props[1,1]+props[1,2]+props[1,3]+props[1,4])))
  
  ensemble_fut<- ((pr_glm_fut*props[1,1])+(pr_rf_fut*props[1,2]))
  
  assign(paste0('fut_ensemble_85_gr', i), ensemble_fut)
  
  
  
}
plot(fut_ensemble_85_gr5)

fut_ens85_list<-lapply(ls(pattern='fut_ensemble_85_gr'), get)


#plot
par(mfrow=c(2,3))

for (i in 1:length(fut_ens85_list)){
  plot(fut_ens85_list[[i]], col=pal, main=paste0('Group', i))
  plot(japan_outline, add=TRUE, col='light grey', border='black')
  box()

  }


#NOW RCP26!!!!
#make sure the colnames match
names(RCP26_2050)<-names(current_preds)
plot(RCP26_2050)


for (i in 1:length(extract_all)) {
  
  pr_glm_fut<-predict(RCP26_2050, glm_list[[i]])
  pr_rf_fut<-predict(RCP26_2050, rf_list[[i+5]])
  
  assign(paste0('glm_fut_26', i), pr_glm_fut)
  assign(paste0('rf_fut_26', i), pr_rf_fut)
  
  props<-data.frame(glmR=1, rfR=1, glmP=1, rfP=1)
  props[1,1]<-1-(averages[[i]][1,1]/(averages[[i]][1,1]+averages[[i]][1,3]))
  props[1,2]<-1-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,3]))
  
  props[1,3]<-abs(average_pear[[i]][1,1])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,3]))
  props[1,4]<-abs(average_pear[[i]][1,3])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,3]))
  
  props<-data.frame(glm=((props[1,1]+props[1,3])/(props[1,1]+props[1,2]+props[1,3]+props[1,4])), rf=((props[1,2]+props[1,4])/(props[1,1]+props[1,2]+props[1,3]+props[1,4])))
  
  ensemble_fut<- ((pr_glm_fut*props[1,1])+(pr_rf_fut*props[1,2]))
  
  assign(paste0('fut_ensemble_26_gr', i), ensemble_fut)
  
  
  
}

fut_ens26_list<-lapply(ls(pattern='fut_ensemble_26_gr'), get)


#plot
par(mfrow=c(2,3))

for (i in 1:length(fut_ens26_list)){
  plot(fut_ens26_list[[i]], col=pal, main=paste0('Group', i))
  plot(japan_outline, add=TRUE, col='light grey', border='black')
  box()
  
}

#before getting difference standardise abundance between zero and 1 ----

normalize <- function(x) {
  return ((x -  x@data@min)/ (x@data@max -  x@data@min))
}

fut_ens85_norm<- lapply(fut_ens85_list, normalize)

ens_list_norm<-lapply(ens_list, normalize)

fut_ens26_norm<-lapply(fut_ens26_list, normalize)





#now get difference between two and plot
#RCP85
for ( i in 1:length(fut_ens85_norm)){
  
  diff<- fut_ens85_norm[[i]] - ens_list_norm[[i]]
  assign(paste0('dif_gr', i), diff)
}

dif_list85<-lapply(ls(pattern='dif_gr'), get)

par(mfrow=c(3,3))
for (i in 1:length(dif_list85)){
  plot(dif_list85[[i]], main=paste0('Group ', i), col=pal)
  plot(japan_outline, add=TRUE, col='light grey', border='black')
  box()
}

#RCP26
for ( i in 1:length(fut_ens26_norm)){
  
  diff<- fut_ens26_norm[[i]] - ens_list_norm[[i]]
  assign(paste0('dif_gr', i), diff)
}

dif_list26<-lapply(ls(pattern='dif_gr'), get)

par(mfrow=c(3,3))
for (i in 1:length(dif_list26)){
  plot(dif_list26[[i]], main=paste0('Group ', i), col=pal)
  plot(japan_outline, add=TRUE, col='light grey', border='black')
  box()
}




#now buffer 30km from the coast and crop by predict area----
#crs(japan_outline) #in wgs project into something in km 
#(predict_area)

#japan_outline_proj<-spTransform(japan_outline, '+proj=utm +zone=54 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ')

#crs(japan_outline_proj)
#par(mfrow=c(1,1))
#plot(japan_outline_proj)

#predict_area_proj<- spTransform(predict_area, '+proj=utm +zone=54 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ')
#crs(predict_area_proj)

#plot(predict_area_proj, add=TRUE, col='pink')

#crop this by predict area
#crop_outline<-gIntersection(japan_outline_proj, predict_area_proj)

#plot(crop_outline)

#buffer this by 30km 
#hmm doesn't work project back 

#outline_30k<- buffer(crop_outline, width=30000, dissolve=TRUE)
#plot(outline_30k, col='yellow')
#plot(crop_outline, col='pink', add=TRUE)


#dif list in different CRS so project back
#crs(dif_list85[[1]])

#crop_outline<- spTransform(crop_outline, ' +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ')

buffer30k<-readOGR('30k_coastplot/outline30k.shp')
crs(buffer30k)

buffer30k<-spTransform(buffer30k,  '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' )

#now mask dif 85 by buffer crop

for (i in 1:length(dif_list85)){
  dif_mask<- mask(dif_list85[[i]], buffer30k)
  assign(paste0('dif85_mask_gr', i), dif_mask)
}

dif_mask85<-lapply(ls(pattern='dif85_mask_gr'), get)

plot(dif_mask85[[1]])

par(mfrow=c(3,2))

for (i in 1:length(dif_mask85)){
  plot(dif_mask85[[i]])
}

#ok now extract the lat lons of the raster and plot 
#turn all the variables to points then get the coords of points

par(mfrow=c(1,1))
points<- rasterToPoints(dif_mask85[[2]])
plot(points)
coords<-as.data.frame(coordinates(points))
coords<-coords[,-3]

#extract from the coords
values<- extract(dif_list85[[2]], coords)

coords$values<-values

coords$slope<-coords$x * coords$y

ggplot(coords, aes(x=slope, y=values))+
  geom_point()+
  geom_smooth(method='lm')


#make it for all RCP85
for (i in 1:length(dif_mask85)){
  points<-rasterToPoints(dif_mask85[[i]])
  coords<-as.data.frame(coordinates(points))
  coords<-coords[-3]
  values<-extract(dif_list85[[i]], coords)
  coords$values<-values
  coords$slope<-coords$ x * coords$y
  assign(paste0('dif85_values_gr', i), coords)
}

dif85_values_all<-lapply(ls(pattern='dif85_values_gr'), get)


#group into 1 df
for (i in 1: length(dif85_values_all)){
  dif85_values_all[[i]]$group<-i
  dif85_values_all[[i]]$climate<-'RCP85'
}

View(dif85_values_all[[1]])

#now merge them all together 
dif85_all_df<-do.call(rbind, dif85_values_all)

dif85_all_df$group<-as.factor(dif85_all_df$group)


library('ggforce')

ggplot(dif85_all_df, aes(x=slope, y=values, col=group))+
  geom_smooth(method='loess', se=FALSE)
# facet_zoom(ylim=c(-2, 2))  #zooms in on the smaller ones 


#RCP 26

#now mask dif 85 by buffer crop

for (i in 1:length(dif_list26)){
  dif_mask<- mask(dif_list26[[i]], buffer30k)
  assign(paste0('dif26_mask_gr', i), dif_mask)
}

dif_mask26<-lapply(ls(pattern='dif26_mask_gr'), get)

plot(dif_mask26[[1]])

par(mfrow=c(3,2))

for (i in 1:length(dif_mask26)){
  plot(dif_mask26[[i]])
}

#ok now extract the lat lons of the raster and plot 
#turn all the variables to points then get the coords of points


for (i in 1:length(dif_mask26)){
  points<-rasterToPoints(dif_mask26[[i]])
  coords<-as.data.frame(coordinates(points))
  coords<-coords[-3]
  values<-extract(dif_list26[[i]], coords)
  coords$values<-values
  coords$slope<-coords$ x * coords$y
  assign(paste0('dif26_values_gr', i), coords)
}

dif26_values_all<-lapply(ls(pattern='dif26_values_gr'), get)


#group into 1 df
for (i in 1: length(dif26_values_all)){
  dif26_values_all[[i]]$group<-i
  dif26_values_all[[i]]$climate<-'RCP26'
}

View(dif26_values_all[[1]])

#now merge them all together 
dif26_all_df<-do.call(rbind, dif26_values_all)

dif26_all_df$group<-as.factor(dif26_all_df$group)


ggplot(dif26_all_df, aes(x=slope, y=values, col=group))+
  geom_smooth(method='loess', se=FALSE)
# facet_zoom(ylim=c(-2, 2))  #zooms in on the smaller ones 

#now merge both climate scenarios
dif_values_all<- rbind(dif85_all_df, dif26_all_df)
dif_values_all$climate<-as.factor(dif_values_all$climate)


ggplot(dif_values_all, aes(x=slope, y=values, col=group))+
 geom_smooth(method='loess', se=FALSE)+
  facet_wrap(~climate)


#ok now add on 'algae column'
write.csv(dif_values_all, 'dif_values_all_algae_norm.csv')

#ooookkkkk
#now can find where the areas change the most


#split into below zero and above zero

for ( i in 1:length(dif_list)){
  layer<-dif_list[[i]]
  increase<-layer
  decrease<-layer
  increase[increase < 0]<-NA
  decrease[decrease>0]<-NA
  assign(paste0('increase_gr', i),increase)
  assign(paste0('decrease_gr', i),decrease)
  
}

increase_list<-lapply(ls(pattern='increase_gr'), get)
decrease_list<-lapply(ls(pattern='decrease_gr'), get)

increase_stack<-stack(increase_list)
decrease_stack<-stack(decrease_list)

plot(increase_stack)
plot(decrease_stack)



#subtropical group; 2, 6,9
#tropical group: 1, 3, 4, 5, 7, 8

#split in tropical and subtropical
increase_trop_stack<-stack(increase_list[1])
plot(increase_trop_stack)

increase_subtrop_stack<-stack(increase_list[c(2:4)])
plot(increase_subtrop_stack)

decrease_trop_stack<-stack(decrease_list[c(1)])
plot(decrease_trop_stack)

decrease_subtrop_stack<-stack(decrease_list[c(2:4)])
plot(decrease_subtrop_stack)

#where changes the most? values between 0, 1 for increase 
library(sdmvspecies)

rescale_increase_trop<-rescale(increase_trop_stack)
plot(rescale_increase_trop)

rescale_increase_subtrop<-rescale(increase_subtrop_stack)

trop_increase<-sum(rescale_increase_trop)

sub_trop_increase<-sum(rescale_increase_subtrop)

plot(trop_increase)
plot(japan_outline, add=TRUE)

#flip the decreases so that highest rates of change are highest(positive values)

flip<-function(x){
  x/-1
}
r <- calc(s, fun=sum)


decrease_trop_stack<-flip(decrease_trop_stack)
decrease_trop_stack<-stack(decrease_trop_stack)

rescale_decrease_trop<-rescale(decrease_trop_stack)

trop_decrease<-(sum(rescale_decrease_trop)/-1)

plot(trop_decrease) 

decrease_subtrop_stack<-flip(decrease_subtrop_stack)
decrease_subtrop_stack<-stack(decrease_subtrop_stack)

rescale_decrease_subtrop<-rescale(decrease_subtrop_stack)
subtrop_decrease<- (sum(rescale_decrease_subtrop)/-1)

plot(subtrop_decrease)

library(RColorBrewer)
library(viridis)

#pal <- viridis(n=20, option='plasma' )
pal<-colorRampPalette(c('green3', 'yellow', 'darksalmon', 'white'))


par(mfrow=c(1,2))
plot(trop_increase, main='Tropical FG Increase')
plot(japan_outline, add=TRUE)
plot(sub_trop_increase, main='Subtropical FG Increase')
plot(japan_outline, add=TRUE)

par(mfrow=c(1,2))
plot(trop_decrease, col=pal(50), main='Tropical FG Decrease')
plot(japan_outline, add=TRUE)
plot(subtrop_decrease, col=pal(50), main='Subtropical FG Decrease')
plot(japan_outline, add=TRUE)

#need to get the same legend
stack_posterplot<-stack(dif_list[[2]], dif_list[[4]], dif_list[[5]], dif_list[[8]])

par(mfrow=c(2,2),  mai = c(0.3,0.2,0.2,0.2))

plot(stack_posterplot)

p1<-spplot(stack_posterplot, col.regions=terrain.colors)
p1
p1+layer(sp.polygons(japan_outline))










