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
library(mgcv)
library(MASS)

setwd("S:/Beger group/Katie Cook/Japan_data/SDM_course_git")

#read in Japan if starting ###### START HERE 
japan_outline<-readOGR('plotting/japan_outline.shp')

#check
plot(japan_outline)

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

points(group1, col='red')

crs(japan_outline)

future_preds<-mask(RCP85_2050, predict_area)

RCP85_2050<-mask(RCP85_2050, predict_area)
RCP26_2050<-mask(RCP26_2050, predict_area)


#standardise coefficients
standard<- function(data){
  stand_dat<-(data[]-mean(data[], na.rm=TRUE))
  return(stand_dat)
  
}

#test with 1 of area pred 1 
stand_ras<-standard(area_pred[[1]])
stand_ras[]

test_ras<-area_pred[[1]]

test_ras[]<-stand_ras[]

par(mfrow=c(1,2))

plot(area_pred[[1]])
plot(test_ras)

#now write loop to standardise all 
length(area_pred)

par(mfrow=c(2,2))

for (i in 1:nlayers(area_pred)){
  stand_ras<-standard(area_pred[[i]])
  
  test_ras<-area_pred[[i]]
  
  test_ras[]<-stand_ras[]
  
  assign(paste0('stand_', i), test_ras)
  
}

plot()

stand_stack<-stack(stand_1, stand_2, stand_3, stand_4)

names(stand_stack)

plot(stand_stack)


#ok now standardise future 
#RCP26
for (i in 1:nlayers(RCP26_2050)){
  stand_ras<-standard(RCP26_2050[[i]])
  
  test_ras<-RCP26_2050[[i]]
  
  test_ras[]<-stand_ras[]
  
  assign(paste0('stand_', i), test_ras)
  
}


RCP26_2050<-stack(stand_1, stand_2, stand_3, stand_4)

names(stand_stack)

plot(RCP26_2050)

#RCP85_2050

for (i in 1:nlayers(RCP85_2050)){
  stand_ras<-standard(RCP85_2050[[i]])
  
  test_ras<-RCP85_2050[[i]]
  
  test_ras[]<-stand_ras[]
  
  assign(paste0('stand_', i), test_ras)
  
}


RCP85_2050<-stack(stand_1, stand_2, stand_3, stand_4)

names(stand_stack)

plot(RCP85_2050)





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
  labs(x='Latitude', y='Abundance')+
  facet_wrap(~group, scales = 'free')

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
par(mar=c(3,2,2,2))
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



##now run loop for alllllllll ----

rmse <- function(o,p) {
  e <- o - p
  sqrt(mean(e^2,na.rm=T))
}


#extract the environmental values at lat lons 

group_list<-lapply(ls(pattern="Obvs_gr_"),get)

extract_km<-function(data){
  latlon<-data.frame(lon=data$lon, lat=data$lat)
  extract1<-as.data.frame(extract(stand_stack, latlon))
  extract1$lat<-latlon$lat
  extract1$lon<-latlon$lon
  extract1$abundance<-data$abundance
  extract1
  return(extract1)
}

extract_all<-lapply(group_list, extract_km)

for (i in 1: length(extract_all)){
  extract_all[[i]]$abundance<-ceiling(extract_all[[i]]$abundance)
}


#testing glm step 
glmtest1<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , data=extract_all[[3]] )
glmtest1
summary(glmtest1)

glmtest_step<- step(glmtest1, trace = 0, na.action=na.omit)
summary(glmtest_step) #ok 

#testing standardise coeffs



#testing gam step 
gam1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss,k=5), family=nb(), data=extract_all[[3]])
summary(gam1)

#remove lat lon col
abundance<-extract_all[[3]]$abundance
abundance<-as.data.frame(abundance)


#make df for loop to fill 
gam_step_table<-data.frame(summary(gam1)$s.table)
out_varib<-row.names(gam_step_table[gam_step_table$p.value>=0.1,])

#set up formula to change 

form<-formula(paste(abundance, "~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss, k=5)", sep=""))

for(g in out_varib)
{
  g_temp<-paste(unlist(strsplit(g, "\\)")),", k=5)", sep="")
  
  if(g_temp=="s(BO_sstmin, k=5)"){form_g1<-update(form, ~. -s(BO_sstmin, k=5, k=5))}
  if(g_temp=="s(BO2_curvelmax_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_curvelmax_ss, k=5)) }
  if(g_temp=="s(BO2_salinitymean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_salinitymean_ss, k=5))}
  if(g_temp=="s(BO2_chlomean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_chlomean_ss, k=5))}
  
  gam2 <-gam(form_g1, data=extract_all[[3]],  family=nb(), na.action=na.omit)
  
  if(AIC(gam2)<=AIC(gam1)){form<-form_g1
  print(paste(g, " dropped", sep=""))}
}

gam_final <-gam(form, data=extract_all[[3]],  family=nb(), na.action=na.omit)

summary(gam_final)
AIC(gam_final)
AIC(gam1)



#create models 
#now model in loop with 15% test 85% training (n=5)

models_all<- function(data){

  rmse_all<-data.frame(glmR=1, gamR=1, rfR=1, glmP=1, gamP=1, RFP=1) 

for (i in 1:100){
  tryCatch( {
  
  #define test and training data   ---------- problem with drop- if no variables are significant it removes them all! need to check thisss.
  test<- data[sample(nrow(data), size=5, replace=FALSE),]  
  train<- data[(! row.names(data) %in% row.names(test)), ]
  
  obvs<-test$abundance
  
  #make models
  
  #GLM
  glm1<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , data=train, na.action = na.omit )
  glm1<- step(glm1, trace = 0, na.action=na.omit)
  
  
  #GAM
  gam1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss,k=5), family=nb() , data=train)  
  #extract abundance 
  abundance<-train$abundance
  abundance<-as.data.frame(abundance)
  
  
  #make df for loop to fill 
  gam_step_table<-data.frame(summary(gam1)$s.table)
  out_varib<-row.names(gam_step_table[gam_step_table$p.value>=0.1,])
  
  #set up formula to change 
  form<-formula(paste(abundance, "~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss, k=5)", sep=""))
  
  #run step loop 
  for(g in out_varib)
  {
    g_temp<-paste(unlist(strsplit(g, "\\)")),", k=5)", sep="")
    
    if(g_temp=="s(BO_sstmin, k=5)"){form_g1<-update(form, ~. -s(BO_sstmin, k=5, k=5))}
    if(g_temp=="s(BO2_curvelmax_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_curvelmax_ss, k=5)) }
    if(g_temp=="s(BO2_salinitymean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_salinitymean_ss, k=5))}
    if(g_temp=="s(BO2_chlomean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_chlomean_ss, k=5))}
    
    gam2 <-gam(form_g1, data=train,  family=nb(), na.action=na.omit)
    
    if(AIC(gam2)<=AIC(gam1)){form<-form_g1
    print(paste(g, " dropped", sep=""))}
  }
  
  gam1 <-gam(form, data=train,  family=nb(), na.action=na.omit)
  
  
  #rf 
  
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
  
  #now pearsons correlation for all 
  rmse_all[i,4]<-cor(obvs, prglm, method = c("pearson"))
  rmse_all[i,5]<-cor(obvs, prgam, method = c("pearson"))
  rmse_all[i,6]<-cor(obvs, prRF, method = c("pearson"))
  
  print(i)

} , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}
  
  return(rmse_all) 
 
  
}

allrmse<-lapply(extract_all, models_all)


#testing the models
glmtest1<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , data=extract_all[[1]] )
glmtest1
summary(glmtest1)

glmtest2<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , data=extract_all[[2]] )
glmtest2
summary(glmtest2)

glmtest3<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , data=extract_all[[3]] )
glmtest3
summary(glmtest3)

glmtest4<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , data=extract_all[[4]])
glmtest4
summary(glmtest4)

glmtest5<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , data=extract_all[[5]])
glmtest5
summary(glmtest5)

glmtest6<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , data=extract_all[[6]] )
glmtest6
summary(glmtest6)


glmtest7<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , data=extract_all[[7]])
glmtest7
summary(glmtest7)

glmtest8<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , data=extract_all[[8]] )
glmtest8
summary(glmtest8)

glmtest9<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , data=extract_all[[9]])
glmtest9
summary(glmtest9)

#get averages as it didnt work 

average_km<-function(data){
  
  glm_av<- mean(data$glmR, na.rm=TRUE)
  gam_av<-mean(data$gamR, na.rm=TRUE)
  rf_av<-mean(data$rfR, na.rm=TRUE)
  
  averages<-data.frame(glm_av, gam_av, rf_av)
  
  return(averages)
  
}

averages<-lapply(allrmse, average_km)

averages.df<-data.frame(glm=1, gam= 1, rf=1)

for (i in 1: length(averages)){
  averages.df[i,]<-(averages[[i]])
}

old_averages<-averages.df

#plot group abundances
par(mfrow=c(3,3))
for (i in 1:length(group_list)){
  hist(group_list[[i]]$abundance, main=i)
}


#averages pearsons 
average_p<-function(data){
  
  glm_av<- mean(data$glmP, na.rm=TRUE)
  gam_av<-mean(data$gamP, na.rm=TRUE)
  rf_av<-mean(data$RFP, na.rm=TRUE)
  
  averages<-data.frame(glm_av, gam_av, rf_av)
  
  return(averages)
  
}

average_pear<-lapply(allrmse, average_p)

for (i in 1: length(average_pear)){
print(  average_pear[[i]])
}




#find best model for each and use to predict. 


#full model, predict and ensemble   #loop

for (i in 1:length(extract_all)) {
  
  
  #GLM
  glm_gr<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , data=extract_all[[i]])
  glm_gr<-step(glm_gr, trace = 0, na.action=na.omit)
  
  #GAM
  gam1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss,k=5), family=nb() , data=extract_all[[i]])  
  #extract abundance 
  abundance<-extract_all[[i]]$abundance
  abundance<-as.data.frame(abundance)
  
  
  #make df for loop to fill 
  gam_step_table<-data.frame(summary(gam1)$s.table)
  out_varib<-row.names(gam_step_table[gam_step_table$p.value>=0.1,])
  
  #set up formula to change 
  form<-formula(paste(abundance, "~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss, k=5)", sep=""))
  
  #run step loop 
  for(g in out_varib)
  {
    g_temp<-paste(unlist(strsplit(g, "\\)")),", k=5)", sep="")
    
    if(g_temp=="s(BO_sstmin, k=5)"){form_g1<-update(form, ~. -s(BO_sstmin, k=5, k=5))}
    if(g_temp=="s(BO2_curvelmax_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_curvelmax_ss, k=5)) }
    if(g_temp=="s(BO2_salinitymean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_salinitymean_ss, k=5))}
    if(g_temp=="s(BO2_chlomean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_chlomean_ss, k=5))}
    
    gam2 <-gam(form_g1, data=extract_all[[i]],  family=nb(), na.action=na.omit)
    
    if(AIC(gam2)<=AIC(gam1)){form<-form_g1
    print(paste(g, " dropped", sep=""))}
  }
  
  gam_gr<-gam(form, data=extract_all[[i]],  family=nb(), na.action=na.omit)  
    
    
    
    
  #RF  
  rf_gr<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + 
                         BO2_chlomean_ss, data=extract_all[[i]], ntree=300, importance=TRUE  )
  
  assign(paste0('glm_gr', i), glm_gr)
  assign(paste0('gam_gr', i), gam_gr)
  assign(paste0('rf_gr', i), rf_gr)
  
  pr_glm<-predict(area_pred, glm_gr)
  pr_gam<-predict(area_pred, gam_gr)
  pr_rf<-predict(area_pred, rf_gr)    
  
  assign(paste0('pr_glm_gr', i), pr_glm)
  assign(paste0('pr_gam_gr', i), pr_glm)
  assign(paste0('pr_rf_gr', i), pr_rf)
  
  #make the ensemble model from RMSE and Pearsons proportions 
  props<-data.frame(glmR=1, gamR=1, rfR=1, glmP=1, gamP=1, rfP=1)
  props[1,1]<-1-(averages[[i]][1,1]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  props[1,2]<-1-(averages[[i]][1,2]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  props[1,3]<-1-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  
  props[1,4]<-abs(average_pear[[i]][1,1])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  props[1,5]<-abs(average_pear[[i]][1,2])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  props[1,6]<-abs(average_pear[[i]][1,3])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  
  props<-data.frame(glm=((props[1,1]+props[1,4])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])), 
                    gam=((props[1,2]+props[1,5])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])),
                    rf=((props[1,3]+props[1,6])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])))
  
  assign(paste0('prop_gr', i), props)
  
  ensemble<- ((pr_glm*props[1,1])+(pr_gam*props[1,2])+ (pr_rf*props[1,3]))
  
  assign(paste0('ensemble_gr', i), ensemble)
  
 
  
 }

#plot ensembles
library(viridis)
pal <- viridis(n=40, option='plasma', direction=-1)

ens_list<-lapply(ls(pattern="ensemble_gr"),get)

par(mar=c(1.2,1.2,1.2,1.2))
par(mfrow=c(3,3))

for ( i in 1:length(ens_list)){
  plot(ens_list[[i]], main= paste0('Group ',i), col=pal )
  plot(japan_outline, add=TRUE, col='grey', border='black')
  box()
}

#plot trop subtrop example 
par(mfrow=c(1,2))
plot(fut_ens_list[[1]], col=pal, main='Group 1')
plot(japan_outline, add=TRUE, col='light grey', border='black')
box()
plot(fut_ens_list[[9]], col=pal, main='Group 9')
plot(japan_outline, add=TRUE, col='light grey', border='black')
box()


#now future RCP85----
rm(glm_gr)

glm_list<-lapply(ls(pattern='glm_gr'), get)

rm(gam_gr)
gam_list<-lapply(ls(pattern='gam_gr'), get)

rm(rf_gr)

rf_list<-lapply(ls(pattern='rf_gr'), get)

#make sure the colnames match
names(RCP85_2050)<-names(current_preds)


for (i in 1:length(extract_all)) {
  
  pr_glm_fut<-predict(RCP85_2050, glm_list[[i]])
  pr_gam_fut<-predict(RCP85_2050, gam_list[[i]])
  pr_rf_fut<-predict(RCP85_2050, rf_list[[i+9]])
  
  assign(paste0('glm_fut_85', i), pr_glm_fut)
  assign(paste0('gam_fut_85', i), pr_gam_fut)
  assign(paste0('rf_fut_85', i), pr_rf_fut)
  
  #make the ensemble model from RMSE and Pearsons proportions 
  props<-data.frame(glmR=1, gamR=1, rfR=1, glmP=1, gamP=1, rfP=1)
  props[1,1]<-1-(averages[[i]][1,1]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  props[1,2]<-1-(averages[[i]][1,2]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  props[1,3]<-1-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  
  props[1,4]<-abs(average_pear[[i]][1,1])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  props[1,5]<-abs(average_pear[[i]][1,2])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  props[1,6]<-abs(average_pear[[i]][1,3])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  
  props<-data.frame(glm=((props[1,1]+props[1,4])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])), 
                    gam=((props[1,2]+props[1,5])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])),
                    rf=((props[1,3]+props[1,6])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])))
  
  
  ensemble_fut<- ((pr_glm_fut*props[1,1])+(pr_gam_fut*props[1,2])+(pr_rf_fut*props[1,3]))
  
  assign(paste0('fut_ensemble_85_gr', i), ensemble_fut)
  
  
  
}
plot(fut_ensemble_85_gr7)

fut_ens85_list<-lapply(ls(pattern='fut_ensemble_85_gr'), get)


#plot
par(mfrow=c(3,3))

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
  pr_gam_fut<-predict(RCP26_2050, gam_list[[i]])
  pr_rf_fut<-predict(RCP26_2050, rf_list[[i+9]])
  
  assign(paste0('glm_fut_26', i), pr_glm_fut)
  assign(paste0('gam_fut_26', i), pr_gam_fut)
  assign(paste0('rf_fut_26', i), pr_rf_fut)
  
  props<-data.frame(glmR=1, gamR=1, rfR=1, glmP=1, gamP=1, rfP=1)
  props[1,1]<-1-(averages[[i]][1,1]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  props[1,2]<-1-(averages[[i]][1,2]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  props[1,3]<-1-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  
  props[1,4]<-abs(average_pear[[i]][1,1])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  props[1,5]<-abs(average_pear[[i]][1,2])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  props[1,6]<-abs(average_pear[[i]][1,3])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  
  props<-data.frame(glm=((props[1,1]+props[1,4])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])), 
                    gam=((props[1,2]+props[1,5])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])),
                    rf=((props[1,3]+props[1,6])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])))
  
  
  
  ensemble_fut<- ((pr_glm_fut*props[1,1])+(pr_gam_fut*props[1,2])+(pr_rf_fut*props[1,3]))
  
  assign(paste0('fut_ensemble_26_gr', i), ensemble_fut)
  
  
  
}

fut_ens26_list<-lapply(ls(pattern='fut_ensemble_26_gr'), get)


#plot
par(mfrow=c(3,3))

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
#crs(predict_area)

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

#outline_30k<-st_as_sf(outline_30k)
#plot(outline_30k)

#st_write(outline_30k, '30k_coastplot', 'outline30k.shp', driver='ESRI Shapefile')

buffer30k<-readOGR('30k_coastplot/outline30k.shp')
crs(buffer30k)

buffer30k<-spTransform(buffer30k,  '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' )

crs(japan_outline)
#now mask dif 85 by buffer crop

for (i in 1:length(dif_list85)){
  dif_mask<- mask(dif_list85[[i]], buffer30k)
  assign(paste0('dif85_mask_gr', i), dif_mask)
}

dif_mask85<-lapply(ls(pattern='dif85_mask_gr'), get)

plot(dif_mask85[[1]])

par(mfrow=c(3,3))

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

ggplot(dif85_all_df, aes(x=slope, y=values, col=group))+
  geom_smooth(method='loess', se=FALSE)+
# facet_zoom(ylim=c(-1, 1.5))+
  theme_bw()#zooms in on the smaller ones 


#RCP 26

#now mask dif 85 by buffer crop

for (i in 1:length(dif_list26)){
  dif_mask<- mask(dif_list26[[i]], buffer30k)
  assign(paste0('dif26_mask_gr', i), dif_mask)
}

dif_mask26<-lapply(ls(pattern='dif26_mask_gr'), get)

plot(dif_mask26[[1]])

par(mfrow=c(3,3))

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
  geom_smooth(method='loess', se=FALSE)+
#facet_zoom(ylim=c(-1, 1.6)) +
  theme_bw()#zooms in on the smaller ones 

#now merge both climate scenarios
dif_values_all<- rbind(dif85_all_df, dif26_all_df)
dif_values_all$climate<-as.factor(dif_values_all$climate)


ggplot(dif_values_all, aes(x=slope, y=values, col=group))+
  geom_smooth(method='loess', se=FALSE)+
  scale_color_brewer(palette = 'Set1' )+
  facet_wrap(~climate)

write.csv(dif_values_all, 'dif_values_all_fish_norm.csv')



































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
  pr_rf_fut<-predict(future_preds, rf_list[[i+9]])
  
  assign(paste0('glm_fut', i), pr_glm_fut)
  assign(paste0('rf_fut', i), pr_rf_fut)
  
  props<-data.frame(glm=1, rf=1)
  props[1,1]<-1-(averages[[i]][1,1]/(averages[[i]][1,1]++averages[[i]][1,3]))
  props[1,2]<-1-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,3]))
  
  ensemble_fut<- ((pr_glm_fut*props[1,1])+(pr_rf_fut*props[1,2]))
  
  assign(paste0('fut_ensemble_gr', i), ensemble_fut)
  
  
  
}
plot(fut_ensemble_gr8)

fut_ens_list<-lapply(ls(pattern='fut_ensemble_gr'), get)


#plot
par(mfrow=c(3,3))

for (i in 1:length(fut_ens_list)){
  plot(fut_ens_list[[i]])
}



#now get difference between two and plot
for ( i in 1:length(fut_ens_list)){
 
   diff<- fut_ens_list[[i]] - ens_list[[i]]
  assign(paste0('dif_gr', i), diff)
}

dif_list<-lapply(ls(pattern='dif_gr'), get)

par(mfrow=c(3,3))
for (i in 1:length(dif_list)){
  plot(dif_list[[i]], col=pal, main=paste0('Group ', i))
  plot(japan_outline, add=TRUE, col='light grey', border='black')
  box()
}

#plot trop subtrop example 
par(mfrow=c(1,2))
plot(dif_list[[1]], col=pal, main='Group 1')
plot(japan_outline, add=TRUE, col='light grey', border='black')
box()
plot(dif_list[[9]], col=pal, main='Group 9')
plot(japan_outline, add=TRUE, col='light grey', border='black')
box()

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
increase_trop_stack<-stack(increase_list[c(1,3,4,5,7,8)])

plot(increase_trop_stack)

increase_subtrop_stack<-stack(increase_list[c(2,6,9)])
plot(increase_subtrop_stack)

decrease_trop_stack<-stack(decrease_list[c(1,3,4,5,7,8)])
plot(decrease_trop_stack)

decrease_subtrop_stack<-stack(decrease_list[c(2,6,9)])
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

#scale between 0 - 1 for all
trop_increase<-rescale(trop_increase)
sub_trop_increase<-rescale(sub_trop_increase)

trop_decrease<-(rescale(trop_decrease))/-1
subtrop_decrease<-(rescale(subtrop_decrease)/-1)

#get rid of the subtrop increase below -1
sub_trop_increase[sub_trop_increase<0.1]<-NA

library(RColorBrewer)
library(viridis)

pal <- viridis(n=40, option='plasma' , direction = -1)
palm<-viridis(n=40, option='plasma' )


par(mfrow=c(1,2))
plot(trop_increase, main='Tropical FG Increase', col=pal)
plot(japan_outline, border='light grey', col='light grey', add=TRUE)
box()

plot(sub_trop_increase, main='Subtropical FG Increase', col=pal)
plot(japan_outline, add=TRUE, border='light grey', col='light grey')
box()

par(mfrow=c(1,2))
plot(trop_decrease, main='Tropical FG Decrease', col=palm)
plot(japan_outline, border='light grey', col='light grey', add=TRUE)
box()

plot(subtrop_decrease, main='Subtropical FG Decrease', col=palm)
plot(japan_outline, add=TRUE, border='light grey', col='light grey')
box()


#now write raster 
writeRaster(trop_increase, 'change_hotspots/fish_trop_increase.tif', format='GTiff')
writeRaster(sub_trop_increase, 'change_hotspots/fish_subtrop_increase.tif', format='GTiff')
writeRaster(trop_decrease, 'change_hotspots/fish_trop_decrease.tif', format='GTiff')
writeRaster(subtrop_decrease, 'change_hotspots/fish_subtrop_decrease.tif', format='GTiff')

#need to get the same legend
stack_posterplot<-stack(dif_list[[2]], dif_list[[4]], dif_list[[5]], dif_list[[8]])

par(mfrow=c(2,2),  mai = c(0.3,0.2,0.2,0.2))

plot(stack_posterplot)

p1<-spplot(stack_posterplot, col.regions=terrain.colors)
p1
p1+layer(sp.polygons(japan_outline))

