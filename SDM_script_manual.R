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




##get bathy layer for mask
##compare with the marspec bathy

#bathy2<-load_layers(c('MS_bathy_5m'))

#res(bathy2)

#bathy2_jpn<-crop(bathy2, geographic.extent)
#
#my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 

#plot(bathy2_jpn, col=my.colors(1000) )

#bathy_shallow2<-bathy2_jpn

#bathy_shallow2[bathy_shallow2 < -100]<-NA


#plot(bathy_shallow2, col=my.colors(1000))   #bathy_shallow2 = better 


#ok now crop by 200km buffer from coastline within MEOW 
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


plot(dissolve)# crop this with coastline buffer


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

#meow preds

plot(current_preds[[1]])

area_pred<-mask(current_preds, predict_area)

plot(area_pred[[1]])

plot(japan_outline, add=TRUE)

points(group1, col='red')

crs(japan_outline)




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

#now extract the values from the raster stack at the points 
extract<-as.data.frame(extract(current_preds, latlon))
extract$lat<-latlon$lat
extract$lon<-latlon$lon
extract$abundance<-abundance$abundance

site_group_lat<-site_group_lat[,c(2,9)]
names(site_group_lat)<-c('lat', 'island')

#group sites by islands
extract<-left_join(extract, site_group_lat, by='lat')

#ok now model!!


hist(extract$abundance) #poissan 

library(lme4)

#1st make model, then bootstrap
m1<-glmer(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss + (1|island), family=poisson, data=extract)
m1

m2<-glm(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , family=poisson, data=extract) #including random effects significantly reduces AIC
m2

m3<-glm(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss + island, family=poisson, data=extract)
m3

library(mgcv)

m4<-gam(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss + island, family=poisson, data=extract)
m4

library(gamm4)
m5<-gamm4(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss, random=~(1|island), family=poisson, data=extract)
m5
#gamm4 doesnt work with predict



#now bootstrap? then RMSE
#ok bootstrap in loop 

#now loop through to find optimum % to cross validate with 
rmse.bootstrap<-data.frame('glm'=1, 'glmer'=1, 'gam'=1, numbertest=1)

rmse <- function(o,p) {
  e <- o - p
  sqrt(mean(e^2,na.rm=T))
}

for (j in 1:10){
  for (i in 1:100){
  #define test and training data
  test<- extract[sample(nrow(extract), size=j, replace=FALSE),]  
  train<- extract[(! row.names(extract) %in% row.names(test)), ]
  
  obvs<-test$abundance
  
  #make models
  glm1<-glm(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , family=poisson, data=train)
  glmer1<-glmer(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss + (1|island), family=poisson, data=train)
  gam1<-gam(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss, family=poisson, data=train)
  
  #predict models for test
  prglm<-predict( glm1, test)
  prglmer<-predict(glmer1, test, allow.new.levels = TRUE)
  prgam<-predict(gam1, test)
  
  
  #now rmse for all
  rmse.bootstrap[(j*100)-100+i,1]<-rmse(obvs, prglm)
  rmse.bootstrap[(j*100)-100+i,2]<-rmse(obvs,prglmer)
  rmse.bootstrap[(j*100)-100+i,3]<-rmse(obvs,prgam)
  
  rmse.bootstrap[(j*100)-100+i,4]<-j

  }
}

write.csv(rmse.bootstrap, 'rmse.bootstrap2.csv')

rmse.bootstrap<-read.csv('rmse.bootstrap2.csv')




#plot
rmse.bootstrap$numbertest<-as.factor(rmse.bootstrap$numbertest)

ggplot(rmse.bootstrap, aes(x=numbertest, y=glm))+
  geom_violin()

ggplot(rmse.bootstrap, aes(x=numbertest, y=glmer))+
  geom_violin()

ggplot(rmse.bootstrap, aes(x=numbertest, y=gam))+
  geom_violin()


#get average RMSE for groups
av_rmse<-rmse.bootstrap %>% group_by(numbertest) %>% summarise(mean_glm=mean(glm), mean_glmer=mean(glmer), mean_gam=mean(gam))

ggplot(av_rmse, aes(x=numbertest, y=mean_glm))+
  geom_point()+
  geom_smooth()

ggplot(av_rmse, aes(x=numbertest, y=mean_glmer))+
  geom_point()+
  geom_smooth()

ggplot(av_rmse, aes(x=numbertest, y=mean_gam))+
  geom_point()+
  geom_smooth()

# ok cross validate with 5 (15%)


#NOW DO RANDOM FOREST
m6<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + 
                   BO2_chlomean_ss, data=extract, ntree=300, importance=TRUE   )
m6

predictrf<-predict(current_preds, m6)

importance(m6)

plot(predictrf)

#maybe just dont use island as it becomes the most important variable

#now model in loop with 15% test 85% training (n=5)
rmse_all<-data.frame(glm=1, gam=1, rf=1)  

for (i in 1:100){
    #define test and training data
    test<- extract[sample(nrow(extract), size=5, replace=FALSE),]  
    train<- extract[(! row.names(extract) %in% row.names(test)), ]
    
    obvs<-test$abundance
    
    #make models
    glm1<-glm(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , family=poisson, data=train)
    gam1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss,k=5), family=poisson, data=train)
    rf1<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + 
                        BO2_chlomean_ss, data=train, ntree=300, importance=TRUE   )
    

    #predict models for test
    prglm<-predict( glm1, test)
    prgam<-predict(gam1, test)
    prRF<-predict(rf1, test)
    
    #now rmse for all
    rmse_all[i,1]<-rmse(obvs, prglm)
    rmse_all[i,2]<-rmse(obvs, prgam)
    rmse_all[i,3]<-rmse(obvs, prRF)

    
  }

write.csv(rmse_all, 'rmse_all_2806.csv')

#now get averages
glm_av<- mean(rmse_all$glm)
gam_av<-mean(rmse_all$gam)
rf_av<-mean(rmse_all$rf)

averages<-data.frame(glm_av, gam_av, rf_av)

#make ensemble model based on the averaged rmse 

glm1
gam1
summary(rf1)
rf1

#get the model coefs to average #no this is wrong
coef(glm1)
coef(glm1)[1]
coef<-data.frame(intercept=coef(glm1)[1], sst=coef(glm1)[2], cur=coef(glm1)[3],sal=coef(glm1)[4], chlo=coef(glm1)[5], model='glm')

coef[1, 1:5]<-coef(glm1)[1:5]

#ok make the full models then ensemble with the average weights
glm_gr1<-glm(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , family=poisson, data=extract)
gam_gr1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss,k=5), family=poisson, data=extract)
rf_gr1<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + 
                    BO2_chlomean_ss, data=extract, ntree=300, importance=TRUE   )

gam.check(gam_gr1)
par(mfrow=c(1,1))
plot.gam(gam_gr1)

#now predict and ensemble
pr_glm1<-predict(area_pred, glm_gr1)
pr_gam1<-predict(area_pred, gam_gr1)
pr_rf1<-predict(area_pred, rf_gr1)

par(mfrow=c(2,2))
plot(pr_glm1)
plot(pr_gam1)
  plot(pr_rf1)

#ok now ensemble with averages
props<-data.frame(glm=1, gam=1, rf=1)
props[1,1]<-1-(averages[1,1]/(averages[1,1]+averages[1,2]+averages[1,3]))
props[1,2]<-1-(averages[1,2]/(averages[1,1]+averages[1,2]+averages[1,3]))
props[1,3]<-1-(averages[1,3]/(averages[1,1]+averages[1,2]+averages[1,3]))

ensemble<- ((pr_glm1)*props[1,1]+pr_gam1*props[1,2]+pr_rf1*props[1,3])/3

par(mfrow=c(1,1))
plot(ensemble)

##