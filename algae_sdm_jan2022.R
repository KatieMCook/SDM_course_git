
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

#now crop
RCP85_2050<-crop(RCP85_2050, geographic.extent)

light<-crop(light, geographic.extent)
chlo_future<-crop(chlo_future,  geographic.extent)

RCP85_2050<-stack(RCP85_2050, light, chlo_future)


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

#gam is bad so dont use

#full model, predict and ensemble   #loop

for (i in 1:length(extract_all)) {
  
  glm_gr<-glm(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , family=poisson, data=extract_all[[i]])
  rf_gr<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + 
                        BO2_chlomean_ss, data=extract_all[[i]], ntree=300, importance=TRUE   )
  
  assign(paste0('glm_gr', i), glm_gr)
  assign(paste0('rf_gr', i), rf_gr)
  
  pr_glm<-predict(area_pred, glm_gr)
  pr_rf<-predict(area_pred, rf_gr)    
  
  assign(paste0('pr_glm_gr', i), pr_glm)
  assign(paste0('pr_rf_gr', i), pr_rf)
  
  props<-data.frame(glmR=1, rfR=1, glmP=1, rfP=1)
  props[1,1]<-1-(averages[[i]][1,1]/(averages[[i]][1,1]+averages[[i]][1,3]))
  props[1,2]<-1-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,3]))
  
  props[1,3]<-abs(average_pear[[i]][1,1])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,3]))
  props[1,4]<-abs(average_pear[[i]][1,3])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,3]))
  
  props<-data.frame(glm=((props[1,1]+props[1,3])/(props[1,1]+props[1,2]+props[1,3]+props[1,4])), rf=((props[1,2]+props[1,4])/(props[1,1]+props[1,2]+props[1,3]+props[1,4])))
  
  assign(paste0('prop_gr', i), props)
  
  ensemble<- ((pr_glm*props[1,1])+(pr_rf*props[1,2]))
  
  assign(paste0('ensemble_gr', i), ensemble)
  
  
}

#plot ensembles

ens_list<-lapply(ls(pattern="ensemble_gr"),get)

par(mar=c(1.2,1.2,1.2,1.2))
par(mfrow=c(2,3))

for ( i in 1:length(ens_list)){
  plot(ens_list[[i]], main= i )
  plot(japan_outline, add=TRUE)
  
}

library(viridis)
pal<-viridis(option='plasma', direction=-1, n=40)

ens_list_all<-list(ensemble_gr1, ensemble_gr2, ensemble_gr3, ensemble_gr4, ensemble_gr5)

par(mfrow=c(2,3))
for(i in 1:length(ens_list_all)){
  plot(ens_list_all[[i]], col=pal, main=paste0('Group ', i))
  plot(japan_outline, add=TRUE, col='light grey', border='black')
  box()
  
}





#now future 
rm(glm_gr)

glm_list<-lapply(ls(pattern='glm_gr'), get)

rm(rf_gr)

rf_list<-lapply(ls(pattern='rf_gr'), get)

#make sure the colnames match
names(future_preds)<-names(current_preds)
plot(future_preds)


for (i in 1:length(extract_all)) {
  
  pr_glm_fut<-predict(future_preds, glm_list[[i]])
  pr_rf_fut<-predict(future_preds, rf_list[[i+5]])
  
  assign(paste0('glm_fut', i), pr_glm_fut)
  assign(paste0('rf_fut', i), pr_rf_fut)
  
  props<-data.frame(glmR=1, rfR=1, glmP=1, rfP=1)
  props[1,1]<-1-(averages[[i]][1,1]/(averages[[i]][1,1]+averages[[i]][1,3]))
  props[1,2]<-1-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,3]))
  
  props[1,3]<-abs(average_pear[[i]][1,1])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,3]))
  props[1,4]<-abs(average_pear[[i]][1,3])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,3]))
  
  props<-data.frame(glm=((props[1,1]+props[1,3])/(props[1,1]+props[1,2]+props[1,3]+props[1,4])), rf=((props[1,2]+props[1,4])/(props[1,1]+props[1,2]+props[1,3]+props[1,4])))
  
  ensemble_fut<- ((pr_glm_fut*props[1,1])+(pr_rf_fut*props[1,2]))
  
  assign(paste0('fut_ensemble_gr', i), ensemble_fut)
  
  
  
}
plot(fut_ensemble_gr5)

fut_ens_list<-lapply(ls(pattern='fut_ensemble_gr'), get)


#plot
par(mfrow=c(2,3))

for (i in 1:length(fut_ens_list)){
  plot(fut_ens_list[[i]], col=pal, main=paste0('Group', i))
  plot(japan_outline, add=TRUE, col='light grey', border='black')
  box()

  }


#ok export for marxan
setwd("D:/corona_contingency/SDM_course_git/algae_predictions_jan")

#stack
ens_list_stack<- stack(ens_list)

plot(ens_list_stack)

#change string 
crs(ens_list_stack)<-CRS(SRS_string = "EPSG:4326")


#now write 
for(i in 1:length(ens_list_stack)){
  towrite<-ens_list_stack[[i]]
  writeRaster(towrite, paste0('algae_pred_now_',i,'.tif'))
}

#repeatfor RCP85
#stack
fut_ens_stack<- stack(fut_ens85_list)

plot(fut_ens_stack)

#change string 
crs(fut_ens_stack)<-CRS(SRS_string = "EPSG:4326")


#now write 
for(i in 1:length(fut_ens_stack)){
  towrite<-fut_ens_stack[[i]]
  writeRaster(towrite, paste0('algae_pred_RCP85_',i,'.tif'))
}

#ok now RCP26
#stack
fut_ens26_stack<- stack(fut_ens26_list)

plot(fut_ens26_stack)

#change string 
crs(fut_ens26_stack)<-CRS(SRS_string = "EPSG:4326")


#now write 
for(i in 1:length(fut_ens26_stack)){
  towrite<-fut_ens26_stack[[i]]
  writeRaster(towrite, paste0('algae_pred_RCP26_',i,'.tif'))
}

setwd("D:/corona_contingency/SDM_course_git")

#now get difference between two and plot
for ( i in 1:length(fut_ens_list)){
  
  diff<- fut_ens_list[[i]] - ens_list[[i]]
  assign(paste0('dif_gr', i), diff)
}

dif_list<-lapply(ls(pattern='dif_gr'), get)

par(mfrow=c(2,3))
for (i in 1:length(dif_list)){
  plot(dif_list[[i]], main=paste0('Group ', i), col=pal)
  plot(japan_outline, add=TRUE, col='light grey', border='black')
  box()
}


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










