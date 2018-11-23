#SDM Practise##
setwd("~/PhD/SDM_course_git")

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
current_preds<-load_layers(c('BO_sstmean','BO2_curvelmax_ss', 'BO2_salinitymean_ss'))
res(current_preds)
plot(current_preds)

chlo<-load_layers('BO2_chlomean_ss')
res(chlo)
plot(chlo)

#get my extent to crop
#add lat lon 
latlon<-read.csv('JP2015_16_waypoints.csv')

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

plot(current_preds)



#extract future 
#RCP 85 2050
future_2050<-filter(future_layers, year==2050, scenario=='RCP85')

RCP85_2050<-load_layers(c(c('BO2_RCP85_2050_tempmean_ss','BO2_RCP85_2050_curvelmean_bdmin',
                            'BO2_RCP85_2050_salinitymean_ss')))

chlo_future<-load_layers('BO2_RCP85_2050_chlomean_ss')

#now crop
RCP85_2050<-crop(RCP85_2050, geographic.extent)

chlo_future<-crop(chlo_future, geographic.extent)

RCP85_2050<-stack(RCP85_2050, chlo_future)




#get bathy layer for mask
#compare with the marspec bathy
bathy2<-load_layers(c('MS_bathy_5m'))

res(bathy2)

bathy2_jpn<-crop(bathy2, geographic.extent)

bathy_shallow2<-bathy2_jpn

bathy_shallow2[bathy_shallow2 < -100]<-NA

my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(bathy_shallow2, col=my.colors(1000))   #bathy_shallow2 = better 



#ok now get data
#Fish data
fish_data<-read.csv('FishData_JP_2016_final.csv')

fish_data<-fish_data[1:7533,1:9]

fish_survey<-fish_data

fish_survey$SpeciesFish<-as.character(fish_survey$SpeciesFish)

#make sure fish names are correct
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


#ok now all names ok 

#merge with functional groups
fgroup<-read.csv('fish_groups.csv')

fgroup$X<-as.character(fgroup$X)

names(fgroup)<-c('Species', 'group'  )

names(fish_survey)<-c("SiteID" ,     "Name"   ,     "Year"  ,      "Date"    ,    "Transect"   , "Species", "Number" ,     "SizeCm"  ,    "B_g" ) 

#merge
func_group_all<-left_join(fish_survey, fgroup, by='Species')

#remove unesessary columns and summarise by site, transect, species
func_group_all<- func_group_all[, c(1,5,7,10)]

fgroup_site<-func_group_all %>% group_by(SiteID, group,Transect ) %>% summarise(abundance=sum(Number) )

fgroup_site<-fgroup_site %>% group_by(SiteID, group) %>% summarise(abundance=mean(abundance))

##rename cols
names(fgroup_site)<- c('Site', 'group', 'abundance')

fgroup_site$Site<-as.character(fgroup_site$Site)

#remove na
which(is.na(fgroup_site$group))
fgroup_site<-fgroup_site[-223,]

#add zeros (by making a matrix and making NA values 0 and converting back)
site_group_matrix<-acast(fgroup_site, Site~group, value.var='abundance')

which(is.na(site_group_matrix))
site_group_matrix[which(is.na(site_group_matrix))]<-0

fgroup_site<-melt(site_group_matrix, value.name = 'abundance')

names(fgroup_site)<-c('Site', 'group', 'abundance')

#add lat lon data to obs
latlon<-latlon[,c(2,3,4)]

latlon$Site<-as.character(latlon$Site)


#merge lat lon and functional group data

for (i in 1:nrow(fgroup_site)){
  for (j in 1:nrow(latlon)){
    
    if (fgroup_site[i, 1] == latlon[j, 3]) {
      fgroup_site[i, 4]<- latlon[j, 1]
      fgroup_site[i, 5]<-latlon[j, 2]
      
    }
  }
}


#add names
names(fgroup_site)

names(fgroup_site)<-c( "Site"  ,    "group"  ,   "abundance" , "lat"    ,    "lon" )

#plot to check
# Plot the base map
plot(japan_outline, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     col = "grey95",
     axes = TRUE
)

crs(japan_outline)
# Add the points for individual observation
points(x = fgroup_site$lon, 
       y = fgroup_site$lat, 
       col = "red", 
       pch = 20, 
       cex = 0.75)
# And draw a little box around the graph
box()



#check for NA group site
#which(is.na(fgroup_site$group)) #223
#fgroup_site<-fgroup_site[-223,]


#split data by functional group 
for (i in 1:length(unique(fgroup_site$group))){
  rows<-which(fgroup_site$group==i)
  assign(paste0("Obvs_gr_", i), data.frame(fgroup_site[rows,]))
}


#now with group one create SPDF
latlon<-data.frame(lon=Obvs_gr_1$lon, lat=Obvs_gr_1$lat)

abundance<-data.frame(abundance=round(Obvs_gr_1$abundance))

group1<- SpatialPointsDataFrame(coords=latlon, data=abundance,
                                proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#mask areas less then 100m
#mask <- bathy_shallow2

#current_preds<-mask(current_preds, bathy_shallow2)

#now check for colinearity 
pairs(current_preds) # not great but ok

v1<-vifstep(current_preds, th=10) #nothing got rid of


#now make sdmData
d<-sdmData(abundance~. , train= group1 , predictors =current_preds)
d@errorLog
group1@data  #check the mask 
#now run sdm 


#now do sdm
m1<-sdm(abundance~. , data=d, methods=c('glm', 'brt', 'rf'), 
       replication=c('boot'),n=3,  
       modelSettings=list(brt=list(distribution='poisson',n.minobsinnode =5,bag.fraction =1), glm=list(family='poisson')))   ##do evaluation separately 

m@data@features.name
m1


#library(MASS)
#gnb <- glm.nb(abundance~.,data=dd[,-1])

#extract the columns that are used in bootstapping for model 1  (test points)
w <- m@replicates$abundance[[1]]$test


dd <- as.data.frame(d)
#get the real values for the test points
ddt <- dd[dd$rID %in% w ,]
obs <- ddt$abundance

#get the predicted value for the test points (first bootstrapping)
pr <- predict(m,ddt,run=1)

rmse <- function(o,p) {
  e <- o - p
  sqrt(mean(e^2,na.rm=T))
}

#get the root mean square error for model 3
rmse(obs,pr[,1] )
rmse(obs,pr[,2] )
rmse(obs,pr[,3] )

#predict and plot 
predict_current<-predict(m, current_preds, filename='predict_current_group1.img', mean= T, overwrite=TRUE) #mean=T the mean of the predictions for each algorithm
#.w <- which(predict_current[[1]][] > 200)
#predict_current[[1]][.w] <- NA
#par(mfrow=c(2,2))

plot(predict_current)

#looks weird so use MEOW?
meow<-readOGR('meow/MEOW/meow_ecos.shp')

#plot
par(mfrow=c(1,1))
plot(meow)


##crop by ecoregion
eco<-meow@data$ECOREGION
which(eco=='Central Kuroshio Current')

kuroshio.map<-meow[meow$ECOREGION %in% c('Central Kuroshio Current','South Kuroshio'),]

plot(kuroshio.map)

crs(kuroshio.map)

points(group1, col='red')

#merge shapefiles
dissolve<- aggregate(kuroshio.map, dissolve=T)

plot(dissolve)

plot(japan_outline, add=TRUE)

#meow preds
plot(current_preds[[1]])


meow_pred<-mask(current_preds, dissolve)

plot(meow_pred[[1]])

plot(japan_outline, add=TRUE)

points(group1)

crs(japan_outline)
crs(meow_pred)

#use meow preds for predict

plot(current_preds, main=c('SST', 'Current Velocity', 'Salinity', 'Chlorophyll'))



meow_now<-predict(m, meow_pred, filename='predict_current_group1.img', mean= T, overwrite=TRUE) #mean=T the mean of the predictions for each algorithm


plot(meow_now, main=c('GLM', 'Boosted Regression Tree', 'Random Forest'))



#for future RCP 85  2050
RCP85_2050_meow<-mask(RCP85_2050, dissolve)

names(RCP85_2050_meow)<-names(current_preds)

RCP85_2050p<-predict(m, RCP85_2050_meow, filename='predict_future_group1.img', mean=T, overwrite=TRUE)



plot(RCP85_2050p, main=c('GLM', 'Boosted Regression Tree', 'Random Forest'))
plot(j)


m
##ENSEMBLE MODEL for now 
ens<-ensemble(m, newdata=meow_pred, setting=list(method='unweighted', id=c(4:9) ))

par(mfrow=c(1,1))
plot(meow_now)


#plot_ensemble ----
plot(ens, main= '2000-2014 Ensemble')
plot(japan_outline, add=TRUE)




#ENSEMBLE MODEL FOR FUTURE
ensF<-ensemble(m, newdata=RCP85_2050_meow, setting=list(method='unweighted', id=c(4:9)))

plot(ensF, main="RCP8.5 2050 Ensemble")
plot(japan_outline, add=TRUE)
points(group1, col='red')








###### write a loop/ functions for all groups#########

#now with group one create SPDF
#list<-c(paste0('Obvs_gr_', 1:length(unique(fgroup_site$group))))
list_all<-list(Obvs_gr_1, Obvs_gr_2, Obvs_gr_3, Obvs_gr_4, Obvs_gr_5, Obvs_gr_6, Obvs_gr_7, Obvs_gr_8, Obvs_gr_9)

spdf<- function(data) {
  abundance<-data.frame(abundance=round(data$abundance)) 
  lonlat<-data.frame(data$lon, data$lat )
  spdf_new<- SpatialPointsDataFrame(coords=lonlat, data=abundance, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") )
  spdf_new
  return(spdf_new)
}

spdf_all<-lapply(list_all, spdf)

plot(spdf_all[[1]]) 


#loop through to create sdm data for all
length(spdf_all)

for (i in 1: length(spdf_all)){
 sdm_data<- sdmData(abundance~. , train= spdf_all[[i]], predictors = current_preds)
 assign(paste0('sdm_data_group_', i), sdm_data)
}

#loop through to create model

sdm_data_list<-lapply(ls(pattern='sdm_data_group_'), get)

for (i in 1:length(sdm_data_list)) {
  m<-sdm(abundance~. , data= sdm_data_list[[i]], methods=c('glm', 'brt', 'rf'), 
            replication=c('boot'),n=3,  
            modelSettings=list(brt=list(distribution='poisson',n.minobsinnode =5,bag.fraction =1), glm=list(family='poisson')))   ##do evaluation separately 
  assign(paste0('m_gr', i), m)
  print(i)
  
}


#loop to check rmse
list_models<-lapply(ls(pattern="m_gr"),get)

list_models[[1]]
list_models[[2]]
list_models[[3]]  #check ok


#rmse function

rmse_all<- function (model, sdmdata) { 
  w<- model@replicates$abundance[[1]]$test
  dd<-as.data.frame(as.data.frame(sdmdata))
  ddt<-dd[dd$rID %in% w ,]
  obvs<-ddt$abundance
  pr<-predict(model, ddt, run=1)
  e1<- obvs - pr[,1]
  e2<- obvs - pr[,2]
  e3<- obvs - pr[,3]
 rmse1<- sqrt(mean(e1^2,na.rm=T))
 rmse2<-sqrt(mean(e2^2,na.rm=T))
 rmse3<-sqrt(mean(e3^2,na.rm=T))
 rmse_123<-c(rmse1, rmse2, rmse3)
}

group1_rmse<-rmse_all(list_models[[1]], sdm_data_list[[1]])
group1_rmse

rmse_list<- mapply(rmse_all, list_models, sdm_data_list)

rmse_list[,1]  #columns are the rmse 
rmse_list[] #can see all the results, row 1= glm, row2= brt, row 3= rf


#ok now predict for all groups

for (i in 1: length(list_models)){
  meow_now<-predict(list_models[[i]], meow_pred, mean=T)
  assign(paste0('predict_now_gr', i), meow_now)
  print(i)
}


plot(predict_now_gr1, main=c('GLM', 'Boosted Regression Tree', 'Random Forest'))

#.w <- which(predict_now_gr1[[1]][] > 200)
#predict_now_gr1[[1]][.w] <- NA

#predict future for all
for ( i in 1:length(list_models)){
  RCP85_2050p<-predict(list_models[[i]], RCP85_2050_meow, mean=T)
  assign(paste0('predict_RCP85_gr', i), RCP85_2050p)
  print(i)
}


plot(predict_RCP85_gr1, main=c('GLM', 'Boosted Regression Tree', 'Random Forest'))



##ENSEMBLE MODEL for now 

#create ensemble loop 
for (i in 1: length(list_models)){
  ens<-ensemble(list_models[[i]], newdata=meow_pred, setting=list(method='unweighted', id=c(4:9) ))  #ensemble for brt and rf
  assign(paste0('ens_gr', i), ens)
}

#plot_ensemble ----
plot(ens, main= '2000-2014 Ensemble')
plot(japan_outline, add=TRUE)

#future ensemble 

#ENSEMBLE MODEL FOR FUTURE
for (i in 1:length(list_models)){
 ensF<-ensemble(list_models[[i]], newdata=RCP85_2050_meow, setting=list(method='unweighted', id=c(4:9))) 
 assign(paste0('ensF_gr', i), ensF)
}

#get the difference between them 
dif<-function (ens_future, ens_now){
  ens_future-ens_now
}

diff1<-dif(ensF_list[[1]], ens_list[[1]])
plot(diff1)


ens_list<-lapply(ls(pattern="ens_gr"),get)
ensF_list<-lapply(ls(pattern='ensF_gr'), get)

# dif_all<-lapply(ensF_list, dif, ens_now=ens_list) doesn't work loop

for ( i in 1:length(ens_list)){
 diff<- dif(ensF_list[[i]], ens_list[[i]])
 assign(paste0('dif_gr', i), diff)
}

dif_list<-lapply(ls(pattern='dif_gr'), get)

par(mfrow=c(3,3))

for ( i in 1:length(dif_list)){
  plot(dif_list[[i]], main= i )
  plot(japan_outline, add=TRUE)
  
}
plot(ensF, main="RCP8.5 2050 Ensemble")
plot(japan_outline, add=TRUE)
points(group1, col='red')




