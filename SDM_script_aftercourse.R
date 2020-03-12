#SDM Practise##
jan

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




#get bathy layer for mask
#compare with the marspec bathy
bathy2<-load_layers(c('MS_bathy_5m'))

res(bathy2)

bathy2_jpn<-crop(bathy2, geographic.extent)

my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 

plot(bathy2_jpn, col=my.colors(1000) )

bathy_shallow2<-bathy2_jpn

bathy_shallow2[bathy_shallow2 < -100]<-NA


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
fgroup<-read.csv('fish_groups_MM.csv')

fgroup$Species<-as.character(fgroup$Species)

fgroup<-fgroup[,c(1,2)]

names(fgroup)<-c('Species', 'group'  )

names(fish_survey)<-c("SiteID" ,     "Name"   ,     "Year"  ,      "Date"    ,    "Transect"   , "Species", "Number" ,     "SizeCm"  ,    "B_g" ) 

fgroup<-fgroup[-c(which(fgroup$group==4)),]

fgroup$group[fgroup$group==10]<-4  ##only one in group 4, remove and make 10s 4

which(fgroup$group==4)

#merge
func_group_all<-left_join(fish_survey, fgroup, by='Species')

nasp<-which(is.na(func_group_all$group))
unique(func_group_all[c(nasp),6]) #missing FGs

which( !(fgroup$Species %in% unique(func_group_all$Species)))
fgroup[184,]  #scarus psittacus/ spinus sp group 3

func_group_all$group[func_group_all$Species == "Scarus spinus"]<-fgroup$group[fgroup$Species=='Scarus psittacus/spinus']
func_group_all$group[func_group_all$Species == "Scarus psittacus"]<-fgroup$group[fgroup$Species=='Scarus psittacus/spinus']

which(func_group_all$Species=="Ostracion immaculatus")
which(func_group_all$Species=="Roa modesta")

torm<-which(is.na(func_group_all$group))
func_group_all<-func_group_all[-c(torm),]

#remove unesessary columns and summarise by site, transect, species
func_group_all<- func_group_all[, c(1,5,7,10)]

fgroup_site<-func_group_all %>% group_by(SiteID, group,Transect ) %>% summarise(abundance=sum(Number) )

fgroup_site<-fgroup_site %>% group_by(SiteID, group) %>% summarise(abundance=mean(abundance))



##rename cols
names(fgroup_site)<- c('Site', 'group', 'abundance')

fgroup_site$Site<-as.character(fgroup_site$Site)


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


### PLOTS FOR PRESENTATION
#make group a factor
fgroup_site$group<-as.factor(fgroup_site$group)

#plot functional groups by lat and lon (by abundance)
ggplot(fgroup_site, aes(x=lat, y=abundance, col=group))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  labs(x='Latitude', y='Abundance')

#plot by proportion
fgroup_site_prop<- fgroup_site %>% group_by(Site, lat, lon) %>% mutate(total_abun= sum(abundance)) %>% mutate(prop= abundance/total_abun)

ggplot(fgroup_site_prop, aes(x=lat, y=prop, col=group))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  labs(x='Latitude', y='Proportion of Community')+
  theme_light()+
  ylim(0,0.7)


for (i in 1: length(unique(fgroup_site_prop$group))){
 filter_group<-filter(fgroup_site_prop, group==i) 
 assign(paste0 ('group_prop_lat_', i), filter_group)
}

ggplot(group_prop_lat_1, aes(x=lat, y=prop))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  labs(x='Latitude', y='Proportion of Community', title='Group 1')+
  theme_light()+
  ylim(0, 0.7)
  

ggplot(group_prop_lat_2, aes(x=lat, y=prop))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  labs(x='Latitude', y='Proportion of Community', title='Group 2')+
  theme_light()+
  ylim(0, 0.7)

ggplot(group_prop_lat_3, aes(x=lat, y=prop))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  labs(x='Latitude', y='Proportion of Community', title='Group 3')+
  theme_light()+
  ylim(0, 0.7)

ggplot(group_prop_lat_4, aes(x=lat, y=prop))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  labs(x='Latitude', y='Proportion of Community', title='Group 4')+
  theme_light()+
  ylim(0, 0.7)

ggplot(group_prop_lat_5, aes(x=lat, y=prop))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  labs(x='Latitude', y='Proportion of Community', title='Group 5')+
  theme_light()+
  ylim(0, 0.7)

ggplot(group_prop_lat_6, aes(x=lat, y=prop))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  labs(x='Latitude', y='Proportion of Community', title='Group 6')+
  theme_light()+
  ylim(0, 0.7)

ggplot(group_prop_lat_7, aes(x=lat, y=prop))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  labs(x='Latitude', y='Proportion of Community', title='Group 7')+
  theme_light()+
  ylim(0, 0.7)


ggplot(group_prop_lat_8, aes(x=lat, y=prop))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  labs(x='Latitude', y='Proportion of Community', title='Group 8')+
  theme_light()+
  ylim(0, 0.7)


ggplot(group_prop_lat_9, aes(x=lat, y=prop))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  labs(x='Latitude', y='Proportion of Community', title='Group 9')+
  theme_light()+
  ylim(0, 0.7)


for (i in 1: length(spdf_all)){
  sdm_data<- sdmData(abundance~. , train= spdf_all[[i]], predictors = current_preds)
  assign(paste0('sdm_data_group_', i), sdm_data)
}


par(mfrow=c(1,1))
#plot to check MAP
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
v1

#now make sdmData
d<-sdmData(abundance~. , train= group1 , predictors =current_preds)
d@errorLog
group1@data  #check the mask 

#now run sdm 


#now do sdm
m1<-sdm(abundance~. , data=d, methods=c('glm', 'brt', 'rf'), 
       replication='boot',test.percent=10 ,n=5, 
       modelSettings=list(brt=list(distribution='poisson',n.minobsinnode =5,bag.fraction =1), glm=list(family='poisson')))   ##do evaluation separately 

m1@data@features.name

m1


#library(MASS)
#gnb <- glm.nb(abundance~.,data=dd[,-1])

#extract the columns that are used in bootstapping for model 1  (test points)



w <- m1@replicates$abundance[[1]]$test


dd <- as.data.frame(d)
#get the real values for the test points
ddt <- dd[dd$rID %in% w ,]
obs <- ddt$abundance

#get the predicted value for the test points (first bootstrapping)
pr <- predict(m1,ddt,run=1)

rmse <- function(o,p) {
  e <- o - p
  sqrt(mean(e^2,na.rm=T))
}


#get the root mean square error for model 1,2,3
rmse1<-rmse(obs,pr[,1] )
rmse2<-rmse(obs,pr[,2] )
rmse3<-rmse(obs,pr[,3] )

#normalised RMSE
rangeobs<-max(obs)-min(obs)

nrmse1<-rmse(obs,pr[,1] )/rangeobs
nrmse2<-rmse(obs,pr[,2] )/rangeobs
nrmse3<-rmse(obs,pr[,3] )/rangeobs

#get the rmse for training points?
w1<-m1@replicates$abundance[[1]]$train

dds<-dd[dd$rID %in% w1 ,]
obs<-dds$abundance

pr<-predict(m1, dds, run=1)

trainrmse1<-rmse(obs, pr[,1])
trainrmse2<-rmse(obs, pr[,2])
trainrmse3<-rmse(obs, pr[,3])

#test
rmse_df<-data.frame(rep=1,test_train='train', glm=1, brt=1, rf=1 )
rmse_df$test_train<-as.character(rmse_df$test_train)

for (i in 1:5){
  w <- m1@replicates$abundance[[i]]$test
  
  dd <- as.data.frame(d)
  #get the real values for the test points
  ddt <- dd[dd$rID %in% w ,]
  obs <- ddt$abundance
  
  #get the predicted value for the test points (first bootstrapping)
  pr <- predict(m1,ddt,run=1)
  
  #get the root mean square error for model 1,2,3
  rmse_df[i,3]<-rmse(obs,pr[,1] )
  rmse_df[i,4]<-rmse(obs,pr[,2] )
  rmse_df[i,5]<-rmse(obs,pr[,3] )
  
  rmse_df[i, 1]<-i
  rmse_df[i,2]<-'test'
  
}


#train
for (i in 1:3){
  w <- m1@replicates$abundance[[i]]$train
  
  dd <- as.data.frame(d)
  #get the real values for the test points
  ddt <- dd[dd$rID %in% w ,]
  obs <- ddt$abundance
  
  #get the predicted value for the test points (first bootstrapping)
  pr <- predict(m1,ddt,run=1)
  
  #get the root mean square error for model 1,2,3
  rmse_df[i+3,3]<-rmse(obs,pr[,1] )
  rmse_df[i+3,4]<-rmse(obs,pr[,2] )
  rmse_df[i+3,5]<-rmse(obs,pr[,3] )
  
  rmse_df[i+3, 1]<-i
  rmse_df[i+3,2]<-'train'
  
}


#predict and plot 
predict_current<-predict(m1, current_preds, filename='predict_current_group1.img', mean= T, overwrite=TRUE) #mean=T the mean of the predictions for each algorithm


#only plot areas with less than 200
.w <- which(predict_current[[1]][] > 200)
predict_current[[1]][.w] <- NA
par(mfrow=c(2,2))

plot(predict_current)

#looks weird so use MEOW?
meow<-readOGR('MEOW/meow_ecos.shp')

#plot
par(mfrow=c(1,1))
plot(meow)


#get the right ecoregion
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
#plotting the ecoregion map
#make transparent colours
t_col <- function(color, percent = 50, name = NULL) {
  #	  color = color name
  #	percent = % transparency
  #	   name = an optional name for the color
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-percent)*255/100,
               names = name)
  ## Save the color
  invisible(t.col)
  
  
}
my.col<- t_col('cornflowerblue', percent = 50, name= 'transblue')
## END


#meow preds

plot(current_preds[[1]])


meow_pred<-mask(current_preds, dissolve)

plot(meow_pred[[1]])

plot(japan_outline, add=TRUE)

points(group1, col='red')

crs(japan_outline)
crs(meow_pred)

#use meow preds for predict

plot(current_preds, main=c('SST', 'Current Velocity', 'Salinity', 'Chlorophyll'))



meow_now<-predict(m1, meow_pred, filename='predict_current_group1.img', mean= T, overwrite=TRUE) #mean=T the mean of the predictions for each algorithm


plot(meow_now, main=c('GLM', 'Boosted Regression Tree', 'Random Forest'))

#glm still wierd limit it by 200
.w <- which(meow_now[[1]][] > 200)
meow_now[[1]][.w] <- NA
par(mfrow=c(2,2))

plot(meow_now, main=c('GLM', 'Boosted Regression Tree', 'Random Forest'))


#for future RCP 85  2050
RCP85_2050_meow<-mask(RCP85_2050, dissolve)

names(RCP85_2050_meow)<-names(current_preds)

RCP85_2050p<-predict(m1, RCP85_2050_meow, filename='predict_future_group1.img', mean=T, overwrite=TRUE)


plot(RCP85_2050p, main=c('GLM', 'Boosted Regression Tree', 'Random Forest')) #glm overpredicts, dont use


m1
##ENSEMBLE MODEL for now 
ens<-ensemble(m1, newdata=meow_pred, setting=list(method='unweighted', id=c(4:9) )) #changed the ID from 4:9? (3 models in each)
plot(ens)

par(mfrow=c(1,1))
plot(meow_now)


#plot_ensemble ---- 
w.<-which(ens[]>200)
ens[w.]<-NA
ens

plot(ens, main= '2000-2014 Ensemble')
plot(japan_outline, add=TRUE)


#ENSEMBLE MODEL FOR FUTURE
ensF<-ensemble(m1, newdata=RCP85_2050_meow, setting=list(method='unweighted', id=c(4:9)))

plot(ensF, main="RCP8.5 2050 Ensemble")

plot(japan_outline, add=TRUE)
points(group1, col='red')


#plot by two panel
par(mfrow=c(1,2))

plot(ens, main= '2000-2014 Ensemble')
plot(japan_outline, add=TRUE)

plot(ensF, main="RCP8.5 2050 Ensemble")
plot(japan_outline, add=TRUE)




#saves the file directly to directory
png(file = 'plotting/group1.png')
par(mfrow=c(1,2))

plot(ens, main= '2000-2014 Ensemble')
plot(japan_outline, add=TRUE)

plot(ensF, main="RCP8.5 2050 Ensemble")
plot(japan_outline, add=TRUE)
dev.off()

## NMDS of traits and sites
library(vegan)
library(reshape2)

#make matrix of trait abundances by site
group_site_matrix<- acast(fgroup_site, Site~group, value.var = 'abundance')

ord<-metaMDS(group_site_matrix)
par(mfrow=c(1,1))

plot(ord, type='n')
orditorp(ord, disp='sites', col='red', pch=21, bg='yellow')
orditorp(ord, disp='species', col='black')


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
list_models[[4]]
list_models[[5]]
list_models[[6]]
list_models[[7]]
list_models[[8]]
list_models[[9]]
list_models[[10]] 

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

 nrmse1<-rmse1/(max(obs)-min(obs))
 nrmse2<-rmse2/(max(obs)-min(obs))
 nrmse3<-rmse3/(max(obs)-min(obs))
 nrmse_123<-c(nrmse1, nrmse2, nrmse3)
 #rmse_123<-c(rmse1, rmse2, rmse3) #if you want true rmse not normalised then unhash this 
}

group1_rmse<-rmse_all(list_models[[1]], sdm_data_list[[1]])
group1_rmse

rmse_list<- mapply(rmse_all, list_models, sdm_data_list)

rmse_list[,1]  #columns are the rmse 
rmse_list[,2] #can see all the results, row 1= glm, row2= brt, row 3= rf

rmse_list[]
rmse_listdf<-as.data.frame(rmse_list)
colnames(rmse_listdf)<-c(1:9)
rownames(rmse_listdf)<-c('GLM', 'BRT', 'RF')
nrmsedf<-rmse_listdf

write.csv(nrmsedf, 'normalisedRMSE.csv')

#get normalised rmse



#ok now predict for all groups

for (i in 1: length(list_models)){
  meow_now<-predict(list_models[[i]], meow_pred, mean=T)
  assign(paste0('predict_now_gr', i), meow_now)
  print(i)
}



#.w <- which(predict_now_gr1[[1]][] > 200)
#predict_now_gr1[[1]][.w] <- NA

#predict future for all
for ( i in 1:length(list_models)){
  RCP85_2050p<-predict(list_models[[i]], RCP85_2050_meow, mean=T)
  assign(paste0('predict_RCP85_gr', i), RCP85_2050p)
  print(i)
}


plot(predict_RCP85_gr1, main=c('GLM', 'Boosted Regression Tree', 'Random Forest'))

plot(predict_RCP85_gr2, main=c('GLM', 'Boosted Regression Tree', 'Random Forest'))

plot(predict_RCP85_gr3, main=c('GLM', 'Boosted Regression Tree', 'Random Forest'))

#glm overplotting 

##ENSEMBLE MODEL for now 

#create ensemble loop 
for (i in 1: length(list_models)){
  ens<-ensemble(list_models[[i]], newdata=meow_pred, setting=list(method='unweighted', id=c(4:9) ))  #ensemble for brt and rf
  assign(paste0('ens_gr', i), ens)
}

#plot_ensemble ----
par(mfrow=c(1,2))
plot(ens_gr1, main= 'Group 1')
plot(japan_outline, add=TRUE)
plot(ens_gr6, main= 'Group 6')


plot(japan_outline, add=TRUE)

#future ensemble 

#ENSEMBLE MODEL FOR FUTURE
for (i in 1:length(list_models)){
 ensF<-ensemble(list_models[[i]], newdata=RCP85_2050_meow, setting=list(method='unweighted', id=c(4:9))) 
 assign(paste0('ensF_gr', i), ensF)
}

ens_list<-lapply(ls(pattern="ens_gr"),get)
#get the difference between them 

dif<-function (ens_future, ens_now){
  ens_future-ens_now
}

ensF_list<-lapply(ls(pattern='ensF_gr'), get)

diff1<-dif(ensF_list[[1]], ens_list[[1]])
plot(diff1)




# dif_all<-lapply(ensF_list, dif, ens_now=ens_list) doesn't work loop

for ( i in 1:length(ens_list)){
 diff<- dif(ensF_list[[i]], ens_list[[i]])
 assign(paste0('dif_gr', i), diff)
}

dif_list<-lapply(ls(pattern='dif_gr'), get)


par(mfrow=c(3,3))

#remove group 4 (only one observation)


par(mfrow=c(3,3))

for ( i in 1:length(dif_list)){
  plot(dif_list[[i]], main= i )
  plot(japan_outline, add=TRUE)
  
}


par(mfrow=c(3,3))
for ( i in 1:length(ens_list)){
  plot(ens_list[[i]], main= i )
  plot(japan_outline, add=TRUE)
  
}


for ( i in 1:length(ensF_list)){
  plot(ensF_list[[i]], main= i )
  plot(japan_outline, add=TRUE)
  
}

plot(ensF, main="RCP8.5 2050 Ensemble")
plot(japan_outline, add=TRUE)
points(group1, col='red')

##for poster plot an example of each
par(mfrow=c(2,2),  mai = c(0.3,0.2,0.2,0.2))

plot(dif_list[[1]],xaxt='n', ann=FALSE  )
plot(japan_outline, add=TRUE)
title(main='Group 1')

plot(dif_list[[2]], xaxt='n', ann=FALSE )
plot(japan_outline, add=TRUE)
title(main= 'Group 2')

plot(dif_list[[5]] )
plot(japan_outline, add=TRUE)
title(main='Group 5')

plot(dif_list[[6]] )
plot(japan_outline, add=TRUE)
title(main='Group 6')
#1,5,6,7

par(mfrow=c(1,4))

plot(dif_list[[1]], main= 1 )
plot(japan_outline, add=TRUE)

plot(dif_list[[5]], main= 5 )
plot(japan_outline, add=TRUE)

plot(dif_list[[6]], main= 6 )
plot(japan_outline, add=TRUE)

plot(dif_list[[7]], main= 7)
plot(japan_outline, add=TRUE)

par(mfrow=c(1,3))
plot(dif_list[[2]], main= 2 )
plot(japan_outline, add=TRUE)

plot(dif_list[[3]], main= 3 )
plot(japan_outline, add=TRUE)

plot(dif_list[[8]], main= 8 )
plot(japan_outline, add=TRUE)


par(mfrow=c(1,2))
plot(dif_list[[4]], main= 4 )
plot(japan_outline, add=TRUE)

plot(dif_list[[9]], main= 9 )
plot(japan_outline, add=TRUE)



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


#split in tropical and subtropical
increase_trop_stack<-stack(increase_list[c(1,3,4,5,7,8)])
plot(increase_trop_stack)

increase_subtrop_stack<-stack(increase_list[c(2,6,9)])
plot(increase_subtrop_stack)

decrease_trop_stack<-stack(decrease_list[c(1,3,4,5,7,8)])
plot(decrease_trop_stack)

decrease_subtrop_stack<-stack(decrease_list[c(2,6,9)])
plot(decrease_subtrop_stack)

#where changes the most? values between 0, 1 for increase 
install.packages('sdmvspecies')
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

###test with GBIF? ----
jp_species<- data.frame(species=unique(fish_survey$Species))
jp_species

/...
#split in sp genus
jp_species$species<-as.character(jp_species$species)
jp_split<-strsplit(jp_species$species, '\\s+' )

jp_split<-as.data.frame(jp_split)
jp_split<-as.matrix(jp_split)

jp_split<-t(jp_split)
jp_split<-as.data.frame(jp_split)
names(jp_split)<-c('genus','species')

names(jp_species)<-'Sciname'

jp_species$genus<-jp_split$genus
jp_species$species<-jp_split$species

jp_species$genus<-as.character(jp_species$genus)
jp_species$species<-as.character(jp_species$species)

#get gbif data
species1<-gbif(genus=jp_species[1,2], species=jp_species[1,3])

install.packages('rgbif')
library(rgbif)

species1<-occ_search(scientificName = jp_species[1,1], return= 'data', geometry=c(123,23,141,36))
species1<-st_as_sf(species1, coords=c("decimalLongitude", "decimalLatitude" ), crs=4326)

par(mfrow=c(1,1))
plot(species1$geometry)
plot(japan_outline, add=TRUE)

#loop to get all gbif data for all species 

for (i in 1:nrow(jp_species)){
  data<-occ_search(scientificName = jp_species[i,1], return= 'data', geometry=c(123,23,141,36))
  assign(paste0("gbifsp_", i), data)
  print(i)
  
}
    
  
#remove those with no data
rm(gbifsp_113, gbifsp_122, gbifsp_150, gbifsp_192,gbifsp_218, gbifsp_236, gbifsp_255,gbifsp_262, gbifsp_277,gbifsp_288,gbifsp_309,gbifsp_321,
   gbifsp_342, gbifsp_349, gbifsp_352,gbifsp_354,gbifsp_357,gbifsp_365,gbifsp_370,gbifsp_371,gbifsp_380, gbifsp_382,gbifsp_385,gbifsp_389,gbifsp_390,gbifsp_60, gbifsp_85,
   gbifsp_94)
rm(gbifsp_339, gbifsp_341,gbifsp_343,gbifsp_392)

#list all the others

gbif_list<-lapply(ls(pattern='gbifsp_'),get)
  
selectcols<-function(data){
 selected<- select(data, scientificName,decimalLatitude,decimalLongitude, species)
 return(selected)
}

gbif_sp1test<-selectcols(gbifsp_1)

#get the relevant column
gbif_list_f<-lapply(gbif_list, selectcols)

gbif_comb<-do.call('rbind', gbif_list_f)


#now merge with fish group data
fgroup

names(fgroup)<-c('species', 'group')

gbif_group<-left_join(gbif_comb, fgroup, by='species')

nasp<-which(is.na(gbif_group$group))

gbif_group$species[nasp]

#change the na species
which(fgroup$species=='Cantherhines pardalis') #same as ostraction
fgroup[266,]

gbif_group$group[gbif_group$species=='Ostracion immaculatus']<-5

which(fgroup$species=='Scarus psittacus/spinus')
fgroup[184,]  #group3

gbif_group$group[gbif_group$species=="Scarus psittacus"]<-3
gbif_group$group[gbif_group$species=="Scarus spinus"]<-3


nasp<-which(is.na(gbif_group$group))

gbif_group$species[nasp]  #just remove them
gbif_group<-gbif_group[-c(nasp),]


write.csv(gbif_group, 'gbif_group.csv')

#summarise by group
names(gbif_group)
gbif_group$obs<-1
gbif_group<-gbif_group %>% group_by( decimalLatitude,  decimalLongitude, group  ) %>% summarise(abundance=sum(obs))


#ok so have group observation by latitude for each group
#split for the groups so unique df df for group

gbif_group$group<-as.factor(gbif_group$group)


#plot functional groups by lat and lon (by abundance) some patterns? are they the same 
ggplot(gbif_group, aes(x=decimalLatitude, y=log(abundance), col=group))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  labs(x='Latitude', y='Abundance')


#split data by functional group 
for (i in 1:length(unique(gbif_group$group))){
  rows<-which(gbif_group$group==i)
  assign(paste0("gbif_gr_", i), data.frame(gbif_group[rows,]))
}

plot(gbif_gr_1$abundance~gbif_gr_1$decimalLatitude)
#now we can use these to check models?
#model of predicted vs observed
#so extract values from ensemble model at the lat lon of gbif and regression

latlon1<-data.frame(lon=gbif_gr_1$decimalLongitude, lat=gbif_gr_1$decimalLatitude)
View(latlon1)

extract_gr1<-extract(ens_gr1, latlon1)
gbif_gr_1$ens<-extract_gr1

View(gbif_gr_1)

gbif_gr_1<-na.omit(gbif_gr_1)

ggplot(gbif_gr_1, aes(y=(abundance), x=ens))+
  geom_point()+
  geom_smooth(method='lm')

cor(gbif_gr_1$abundance, gbif_gr_1$ens)


#generate pseudo absences
#split abundance data into presence absense
#plot?
