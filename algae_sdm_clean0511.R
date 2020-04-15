
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

setwd("D:/corona_contingency/SDM_course_git")

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
  geom_smooth(method='lm', se=FALSE)+
  facet_wrap(~group, scales='free')


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


par(mfrow=c(3,2))
for (i in 1:length(extract_all)){
  hist(extract_all[[i]]$abundance, main=i)
}


#create models ----
#now model in loop with 15% test 80% training (n=6)
library(MASS)


models_all<- function(data){
  
  rmse_all<-data.frame(glmR=1, gamR=1, rfR=1, glmP=1, gamP=1, RFP=1)  

  
  for (i in 1:1000){
    tryCatch( {
    #define test and training data
    test<- data[sample(nrow(data), size=6, replace=FALSE),]  
    train<- data[(! row.names(data) %in% row.names(test)), ]
    
    obvs<-test$abundance
    
    #make models
    
    ##GLM
    glm1<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss +BO2_lightbotltmax_bdmin, data=train)
    glm1<- step(glm1, trace = 0, na.action=na.omit)
    
    ##GAM
    gam1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss, k=5) + s(BO2_lightbotltmax_bdmin, k=5), family=nb(), data=train)
    
    abundance<-train$abundance
    abundance<-as.data.frame(abundance)
    
    
    #make df for loop to fill 
    gam_step_table<-data.frame(summary(gam1)$s.table)
    out_varib<-row.names(gam_step_table[gam_step_table$p.value>=0.1,])
    
    #set up formula to change 
    form<-formula(paste(abundance, "~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss, k=5) + s(BO2_lightbotltmax_bdmin, k=5)", sep=""))
    
    #run step loop 
    for(g in out_varib)
    {
      g_temp<-paste(unlist(strsplit(g, "\\)")),", k=5)", sep="")
      
      if(g_temp=="s(BO_sstmin, k=5)"){form_g1<-update(form, ~. -s(BO_sstmin, k=5, k=5))}
      if(g_temp=="s(BO2_curvelmax_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_curvelmax_ss, k=5)) }
      if(g_temp=="s(BO2_salinitymean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_salinitymean_ss, k=5))}
      if(g_temp=="s(BO2_chlomean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_chlomean_ss, k=5))}
      if(g_temp=="s(BO2_lightbotltmax_bdmin, k=5)"){form_g1<-update(form, ~. -s(BO2_lightbotltmax_bdmin, k=5))}
      
      gam2 <-gam(form_g1, data=train,  family=nb(), na.action=na.omit)
      
      if(AIC(gam2)<=AIC(gam1)){form<-form_g1
      print(paste(g, " dropped", sep=""))}
    }
    
    gam1 <-gam(form, data=train,  family=nb(), na.action=na.omit)
    
    
    #RF
    rf1<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss +BO2_lightbotltmax_bdmin +
                        BO2_chlomean_ss, data=train, ntree=300, importance=TRUE   )
    
    
    #predict models for test
    prglm<-predict( glm1, test, type='response')
    prgam<-predict(gam1, test, type='response')
    prRF<-predict(rf1, test, type='response')
    
    #now rmse for all
    rmse_all[i,1]<-rmse(obvs, prglm)
    rmse_all[i,2]<-rmse(obvs, prgam)
    rmse_all[i,3]<-rmse(obvs, prRF)
    
    #now pearsons correlation for all 
   rmse_all[i,4]<-cor(obvs, prglm, method = c("pearson"))
  rmse_all[i,5]<-cor(obvs, prgam, method = c("pearson"))
   rmse_all[i,6]<-cor(obvs, prRF, method = c("pearson"))
   
   plot(obvs~prglm)
   print(i)
   
    } , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
   }
  
  return(rmse_all) 

  
}

#run the models 
allrmse<-lapply(extract_all, models_all)





#5 is weird? 
allrmse[[5]]
extract_all[[5]]  #only present in 5 sites? how do I get around this 

allrmse[[4]]       #5 and 4 are issues#?
extract_all[[4]]

#get averages RMSE

average_km<-function(data){
  
  glm_av<- mean(data$glmR , na.rm=TRUE)
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


#averages pearsons 
average_p<-function(data){
  
  glm_av<- mean(data$glmP, na.rm=TRUE)
  gam_av<-mean(data$gamP, na.rm=TRUE)
  rf_av<-mean(data$RFP, na.rm=TRUE)
  
  averages<-data.frame(glm_av, gam_av, rf_av)
  
  return(averages)
  
}

average_pear<-lapply(allrmse, average_p)


averages_pear.df<-data.frame(glm=1, gam= 1, rf=1)

for (i in 1: length(average_pear)){
  averages_pear.df[i,]<-(average_pear[[i]])
}


#problem. how do we get around lack of data?!

#gam is actually ok, use #no wait actuakky they massively overfit,,, get rid of them 





###full model, predict and ensemble   #loop

for (i in 1:length(extract_all)) {
 
  #GLM 
  glm_gr<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss + BO2_lightbotltmax_bdmin,  data=extract_all[[i]])
  glm_gr<-step(glm_gr, trace = 0, na.action=na.omit)
  
  
  #GAM
  gam1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss, k=5) + s(BO2_lightbotltmax_bdmin, k=5), family=nb(), data=extract_all[[i]])
  
  #extract abundance 
  abundance<-extract_all[[i]]$abundance
  abundance<-as.data.frame(abundance)
  
  
  #make df for loop to fill 
  gam_step_table<-data.frame(summary(gam1)$s.table)
  out_varib<-row.names(gam_step_table[gam_step_table$p.value>=0.1,])
  
  #set up formula to change 
  form<-formula(paste(abundance, "~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss, k=5) + s(BO2_lightbotltmax_bdmin, k=5)", sep=""))
  
  #run step loop 
  for(g in out_varib)
  {
    g_temp<-paste(unlist(strsplit(g, "\\)")),", k=5)", sep="")
    
    if(g_temp=="s(BO_sstmin, k=5)"){form_g1<-update(form, ~. -s(BO_sstmin, k=5, k=5))}
    if(g_temp=="s(BO2_curvelmax_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_curvelmax_ss, k=5)) }
    if(g_temp=="s(BO2_salinitymean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_salinitymean_ss, k=5))}
    if(g_temp=="s(BO2_chlomean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_chlomean_ss, k=5))}
    if(g_temp=="s(BO2_lightbotltmax_bdmin, k=5)"){form_g1<-update(form, ~. -s(BO2_lightbotltmax_bdmin, k=5))}
    
    gam2 <-gam(form_g1, data=extract_all[[i]],  family=nb(), na.action=na.omit)
    
    if(AIC(gam2)<=AIC(gam1)){form<-form_g1
    print(paste(g, " dropped", sep=""))}
  }
  
  gam_gr<-gam(form, data=extract_all[[i]],  family=nb(), na.action=na.omit)  
  
  
  
  #RF
  rf_gr<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + 
                        BO2_chlomean_ss + BO2_lightbotltmax_bdmin, data=extract_all[[i]], ntree=300, importance=TRUE   )
  
  
  #make it so models with significant coefficients go to the ensemble 
  
  
 
  #make all the RMSE values 1/ themselves so that the larger errors become smaller proportions
    averages[[i]]<- averages[[i]]/((averages[[i]])^2)
  
  
  #extract p values of glm coefs 
  glm_pvals<- summary(glm_gr)$coefficients[,4]
  #remove intercept column 
  glm_pvals<- glm_pvals[-1]
  
  #trues are 1 and false 0. if sum =length then they are all over 0.05 and this makes averages 0
  if (sum(glm_pvals > 0.05) == length(glm_pvals)){
    averages[[i]][,1]<-0
    average_pear[[i]][,1]<- 0
  }         
  
  #now say if the RMSE is larger then the mean, proportion= 0
  if (averages.df[i, 1]> (0.5*(max(extract_all[[i]]$abundance)-min(extract_all[[i]]$abundance)))){
    averages[[i]][,1]<- 0 
    average_pear[[i]][,1]<-0
  }
  
  #now if the pearsons is smaller than 0.25 make it 0
  if (averages_pear.df[i, 1]< 0.25) {
    averages[[i]][,1]<- 0 
    average_pear[[i]][,1]<-0
  }
  
  #now same for gam 
  gam_step_table<-data.frame(summary(gam_gr)$s.table)
  
  if (sum (gam_step_table$p.value > 0.05)== length(gam_step_table$p.value)) {
    averages[[i]][,2]<-0
    average_pear[[i]][,2]<-0
  }
  
  #if pearsons less than 0.25 make zero
  if (averages_pear.df[i, 2]< 0.25) {
    averages[[i]][,2]<- 0 
    average_pear[[i]][,2]<-0
  }
  
  #if RMSE is larger then mean make it zero
  if (averages.df[i, 2]> (0.5*(max(extract_all[[i]]$abundance)-min(extract_all[[i]]$abundance)))){
    averages[[i]][,2]<- 0 
    average_pear[[i]][,2]<- 0
  }
  
  #finally for RF
  if (averages.df[i, 3]> (0.5*(max(extract_all[[i]]$abundance)-min(extract_all[[i]]$abundance)))){
    averages[[i]][,3]<- 0
    average_pear[[i]][,3]<-0
  }
  
  #if RMSE is larger then mean make it zero
  if (averages.df[i, 3]> (0.5*(max(extract_all[[i]]$abundance)-min(extract_all[[i]]$abundance)))){
    averages[[i]][,3]<- 0 
    average_pear[[i]][,3]<- 0
  }
  
  #if pearsons less than 0.25 make zero
  if (averages_pear.df[i, 3]< 0.25) {
    averages[[i]][,3]<- 0 
    average_pear[[i]][,3]<-0
  }
  
  
  
  
  assign(paste0('glm_gr', i), glm_gr)
 assign(paste0('gam_gr', i), gam_gr)
  assign(paste0('rf_gr', i), rf_gr)
  
  pr_glm<-predict(area_pred, glm_gr, type='response')
pr_gam<-predict(area_pred, gam_gr, type='response')
  pr_rf<-predict(area_pred, rf_gr, type='response')    
  
  assign(paste0('pr_glm_gr', i), pr_glm)
 assign(paste0('pr_gam_gr', i), pr_gam)
  assign(paste0('pr_rf_gr', i), pr_rf)
  
  averages<- averages
  
  #make the ensemble model from RMSE and Pearsons proportions 
  props<-data.frame(glmR=1, gamR=1, rfR=1, glmP=1, gamP=1, rfP=1)
  props[1,1]<-(averages[[i]][1,1]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  props[1,2]<-(averages[[i]][1,2]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  props[1,3]<-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  
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



####now get RMSE and pear for ensemble #####

#set up data frame to be filled

for (i in 1:length(extract_all)){
  df<-data.frame(RMSE=1, pear=1)
  assign(paste0('ensemble_df',i ), df)
}

ensemble_df<-lapply(ls(pattern="ensemble_df"),get)

#now make loop

for (j in 1:length(extract_all))  {
  data<-extract_all[[j]]
  for (i in 1:1000){
    tryCatch( {
      
      #define test and training data
      test<- data[sample(nrow(data), size=6, replace=FALSE),]  
      train<- data[(! row.names(data) %in% row.names(test)), ]
      
      obvs<-test$abundance
      
      
      #GLM 
      glm_gr<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss + BO2_lightbotltmax_bdmin,  data=train)
      glm1<-step(glm_gr, trace = 0, na.action=na.omit)
      
      
      #GAM
      gam1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss, k=5) + s(BO2_lightbotltmax_bdmin, k=5), family=nb(), data=train)
      
      #extract abundance 
      abundance<-train$abundance
      abundance<-as.data.frame(abundance)
      
      
      #make df for loop to fill 
      gam_step_table<-data.frame(summary(gam1)$s.table)
      out_varib<-row.names(gam_step_table[gam_step_table$p.value>=0.1,])
      
      #set up formula to change 
      form<-formula(paste(abundance, "~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss, k=5) + s(BO2_lightbotltmax_bdmin, k=5)", sep=""))
      
      #run step loop 
      for(g in out_varib)
      {
        g_temp<-paste(unlist(strsplit(g, "\\)")),", k=5)", sep="")
        
        if(g_temp=="s(BO_sstmin, k=5)"){form_g1<-update(form, ~. -s(BO_sstmin, k=5, k=5))}
        if(g_temp=="s(BO2_curvelmax_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_curvelmax_ss, k=5)) }
        if(g_temp=="s(BO2_salinitymean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_salinitymean_ss, k=5))}
        if(g_temp=="s(BO2_chlomean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_chlomean_ss, k=5))}
        if(g_temp=="s(BO2_lightbotltmax_bdmin, k=5)"){form_g1<-update(form, ~. -s(BO2_lightbotltmax_bdmin, k=5))}
        
        gam2 <-gam(form_g1, data=train,  family=nb(), na.action=na.omit)
        
        if(AIC(gam2)<=AIC(gam1)){form<-form_g1
        print(paste(g, " dropped", sep=""))}
      }
      
      gam1<-gam(form, data=train,  family=nb(), na.action=na.omit)  
      
      
      
      #RF
      rf1<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + 
                            BO2_chlomean_ss + BO2_lightbotltmax_bdmin, data=train, ntree=300, importance=TRUE   )
      
    
      #predict models for test
      test_prglm<-predict( glm1, test, type='response')
      test_prgam<-predict(gam1, test, type='response')
      test_prRF<-predict(rf1, test, type='response')
      
      
      #ok now ensemble these models
      #make the ensemble model from RMSE and Pearsons proportions 
      props<-data.frame(glmR=1, gamR=1, rfR=1, glmP=1, gamP=1, rfP=1)
      props[1,1]<-(averages[[j]][1,1]/(averages[[j]][1,1]+averages[[j]][1,2]+averages[[j]][1,3]))
      props[1,2]<-(averages[[j]][1,2]/(averages[[j]][1,1]+averages[[j]][1,2]+averages[[j]][1,3]))
      props[1,3]<-(averages[[j]][1,3]/(averages[[j]][1,1]+averages[[j]][1,2]+averages[[j]][1,3]))
      props[1,4]<-abs(average_pear[[j]][1,1])/(abs(average_pear[[j]][1,1])+abs(average_pear[[j]][1,2])+abs(average_pear[[j]][1,3]))
      props[1,5]<-abs(average_pear[[j]][1,2])/(abs(average_pear[[j]][1,1])+abs(average_pear[[j]][1,2])+abs(average_pear[[j]][1,3]))
      props[1,6]<-abs(average_pear[[j]][1,3])/(abs(average_pear[[j]][1,1])+abs(average_pear[[j]][1,2])+abs(average_pear[[j]][1,3]))
      
      props<-data.frame(glm=((props[1,1]+props[1,4])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])), 
                        gam=((props[1,2]+props[1,5])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])),
                        rf=((props[1,3]+props[1,6])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])))
      
      
      ensemble<- ((test_prglm*props[1,1])+(test_prgam*props[1,2])+ (test_prRF*props[1,3]))
      
      #now rmse for all
      ensemble_df[[j]][i,1]<-rmse(obvs, ensemble)
      
      #now pearsons correlation for all 
      ensemble_df[[j]][i,2]<-cor(obvs, ensemble, method = c("pearson"))
      
      print(i)
      
    } , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    print(j)
  }
  
  
}

j=1
i=1

#now get ens averages
#ok that worked # get averages 
average_ens_func<-function(data){
  
  rmse_av<- mean(data$RMSE , na.rm=TRUE)
  pear_av<-mean(data$pear, na.rm=TRUE)
  
  averages<-data.frame(RMSE=rmse_av, pear=pear_av)
  
  return(averages)
  
}

average_ens<-lapply( ensemble_df, average_ens_func)




######START HERE ANOTHER DAY,.. CRAZY VALUES NOT GOOD ## need to add the predictive capacity into the looooop


plot(ensemble_gr5)

ensemble_gr1
#look at models 
summary(glm_gr1)
summary(gam_gr1)
print(rf_gr1)
importance(rf_gr1)

summary(glm_gr2)
summary(gam_gr2)
print(rf_gr2)

summary(glm_gr3)
summary(gam_gr3)
print(rf_gr3)

summary(glm_gr4)
summary(gam_gr4) #bad

summary(glm_gr5) #also bad 
summary(gam_gr5) #p value is 1 lol    #problem is this are the subtropical species... hmmmmmmmm
print(rf_gr5)

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

setwd("S:/Beger group/Katie Cook/Japan_data/SDM_course_git/plotting/algae/current")

for (i in 1:length(ens_list)){
  writeRaster(ens_list[[i]], paste0('ens_', i), format='GTiff', overwrite=TRUE)
}

test<-raster('ens_1.tif')
plot(test)

#now future RCP85
rm(glm_gr)

glm_list<-lapply(ls(pattern='glm_gr'), get)

rm(rf_gr)

rf_list<-lapply(ls(pattern='rf_gr'), get)

rm(gam_gr)

gam_list<-lapply(ls(pattern='gam_gr'), get)


#make sure the colnames match
names(RCP85_2050)<-names(current_preds)
plot(RCP85_2050)

for (i in 1:length(extract_all)) {
  
  pr_glm_fut<-predict(RCP85_2050, glm_list[[i]], type='response')
  pr_gam_fut<-predict(RCP85_2050, gam_list[[i]], type='response')
  pr_rf_fut<-predict(RCP85_2050, rf_list[[i+5]], type='response')
  
  assign(paste0('glm_fut_85', i), pr_glm_fut )
  assign(paste0('gam_fut_85', i), pr_gam_fut)
  assign(paste0('rf_fut_85', i), pr_rf_fut)
  
  props<-data.frame(glmR=1, gamR=1, rfR=1, glmP=1, gamP=1, rfP=1)
  props[1,1]<-(averages[[i]][1,1]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  props[1,2]<-(averages[[i]][1,2]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  props[1,3]<-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  
  props[1,4]<-abs(average_pear[[i]][1,1])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  props[1,5]<-abs(average_pear[[i]][1,2])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  props[1,6]<-abs(average_pear[[i]][1,3])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  
  props<-data.frame(glm=((props[1,1]+props[1,4])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])), 
                    gam=((props[1,2]+props[1,5])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])),
                    rf=((props[1,3]+props[1,6])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])))
  
  assign(paste0('prop_gr', i), props)
  
  
  ensemble_fut<- ((pr_glm_fut*props[1,1])+(pr_gam_fut*props[1,2]) +(pr_rf_fut*props[1,3]))
  
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
  
  pr_glm_fut<-predict(RCP26_2050, glm_list[[i]], type='response')
  pr_gam_fut<-predict(RCP26_2050, gam_list[[i]], type='response')
  pr_rf_fut<-predict(RCP26_2050, rf_list[[i+5]], type='response')
  
  assign(paste0('glm_fut_26', i), pr_glm_fut)
  assign(paste0('gam_fut_26', i), pr_gam_fut)
  assign(paste0('rf_fut_26', i), pr_rf_fut)
  
  props<-data.frame(glmR=1, gamR=1, rfR=1, glmP=1, gamP=1, rfP=1)
  props[1,1]<-(averages[[i]][1,1]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  props[1,2]<-(averages[[i]][1,2]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  props[1,3]<-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  
  props[1,4]<-abs(average_pear[[i]][1,1])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  props[1,5]<-abs(average_pear[[i]][1,2])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  props[1,6]<-abs(average_pear[[i]][1,3])/(abs(average_pear[[i]][1,1])+abs(average_pear[[i]][1,2])+abs(average_pear[[i]][1,3]))
  
  props<-data.frame(glm=((props[1,1]+props[1,4])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])), 
                    gam=((props[1,2]+props[1,5])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])),
                    rf=((props[1,3]+props[1,6])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])))
  
  assign(paste0('prop_gr', i), props)
  
  
  ensemble_fut<- ((pr_glm_fut*props[1,1])+(pr_gam_fut*props[1,2]) +(pr_rf_fut*props[1,3]))
  
  
  
  
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

setwd("S:/Beger group/Katie Cook/Japan_data/SDM_course_git")
#try cropping first?

buffer30k<-readOGR('30k_coastplot/outline30k.shp')
crs(buffer30k)

buffer30k<-spTransform(buffer30k,  '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' )

#now mask ens 85 by buffer crop

for (i in 1:length(fut_ens85_list)){
  ens_mask<- mask(fut_ens85_list[[i]], buffer30k)
  assign(paste0('ens85_mask_gr', i), ens_mask)
}

ens_mask85_list<-lapply(ls(pattern='ens85_mask_gr'), get)

plot(ens_mask85_list[[1]])

#mask 26
for (i in 1:length(fut_ens26_list)){
  ens_mask<- mask(fut_ens26_list[[i]], buffer30k)
  assign(paste0('ens26_mask_gr', i), ens_mask)
}

ens_mask26_list<-lapply(ls(pattern='ens26_mask_gr'), get)


#before getting difference standardise abundance between zero and 1 ----

#normalize <- function(x) {
  #return ((x -  x@data@min)/ (x@data@max -  x@data@min))
#}

fut_ens85_norm<- ens_mask85_list

ens_list_norm<-ens_list

fut_ens26_norm<-ens_mask26_list

plot(fut_ens85_norm[[1]])
plot(fut_ens26_norm[[1]])


library(viridis)
pal <- viridis(n=20, option='plasma' )


#now get difference between two and plot
#RCP85
for ( i in 1:length(fut_ens85_norm)){
  
  diff<- fut_ens85_norm[[i]] - ens_list_norm[[i]]
  assign(paste0('dif_gr', i), diff)
}

dif_list85<-lapply(ls(pattern='dif_gr'), get)

par(mfrow=c(2,3))
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

par(mfrow=c(2,3))
for (i in 1:length(dif_list26)){
  plot(dif_list26[[i]], main=paste0('Group ', i), col=pal)
  plot(japan_outline, add=TRUE, col='light grey', border='black')
  box()
}


plot(dif_list26[[1]])

#now write the lists so you dont have to rerun 
setwd("S:/Beger group/Katie Cook/Japan_data/SDM_course_git/plotting/algae/RCP85")

names(dif_list85)<-c(1:length(dif_list85))

for (i in 1:length(dif_list85)){
  writeRaster(dif_list85[[i]], paste0('RCP85_dif', i), format='GTiff', overwrite=TRUE)
}

names(dif_list85)

setwd("S:/Beger group/Katie Cook/Japan_data/SDM_course_git/plotting/algae/RCP26")

for (i in 1:length(dif_list26)){
  writeRaster(dif_list26[[i]], paste0('RCP26_dif', i), format='GTiff', overwrite=TRUE)
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

#buffer30k<-readOGR('30k_coastplot/outline30k.shp')
#crs(buffer30k)

#buffer30k<-spTransform(buffer30k,  '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' )

#now mask dif 85 by buffer crop

#for (i in 1:length(dif_list85)){
#  dif_mask<- mask(dif_list85[[i]], buffer30k)
#  assign(paste0('dif85_mask_gr', i), dif_mask)
#}

#dif_mask85<-lapply(ls(pattern='dif85_mask_gr'), get)

#plot(dif_mask85[[1]])

#par(mfrow=c(3,2))

#for (i in 1:length(dif_mask85)){
#  plot(dif_mask85[[i]])
#}

dif_mask85<-dif_list85
dif_mask26<-dif_list26

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

ggplot(coords, aes(x=y, y=values))+
  geom_point()+
  geom_smooth(method='auto')


#make it for all RCP85
for (i in 1:length(dif_mask85)){
  points<-rasterToPoints(dif_mask85[[i]])
  coords<-as.data.frame(coordinates(points))
  coords<-coords[-3]
  values<-extract(dif_list85[[i]], coords)
  coords$values<-values
  coords$slope<-(coords$x * coords$y)
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

ggplot(dif85_all_df, aes(x=y, y=values, col=group))+
  geom_point()+
  geom_smooth(method='loess', col='black')+
  facet_wrap(~group)+
  geom_hline(yintercept=0, linetype='dotted')
  

#RCP 26

#now mask dif 85 by buffer crop

#for (i in 1:length(dif_list26)){
 # dif_mask<- mask(dif_list26[[i]], buffer30k)
 # assign(paste0('dif26_mask_gr', i), dif_mask)
#}

#dif_mask26<-lapply(ls(pattern='dif26_mask_gr'), get)

#plot(dif_mask26[[1]])

##par(mfrow=c(3,2))

#for (i in 1:length(dif_mask26)){
#  plot(dif_mask26[[i]])
#}

#ok now extract the lat lons of the raster and plot 
#turn all the variables to points then get the coords of points


for (i in 1:length(dif_mask26)){
  points<-rasterToPoints(dif_mask26[[i]])
  coords<-as.data.frame(coordinates(points))
  coords<-coords[-3]
  values<-extract(dif_list26[[i]], coords)
  coords$values<-values
  coords$slope<-coords$x * coords$y
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


ggplot(dif_values_all, aes(x=y, y=values, col=group))+
  geom_smooth(method='loess')+
  #geom_smooth(method='gam' ,formula = y~s(x))+
  facet_wrap(~climate)+
  geom_hline(yintercept=0, linetype='dotted')+
  theme_bw()+
  xlab('Latitude')+ ylab('Change in abundance')



#ok now add on 'algae column'
write.csv(dif_values_all, 'dif_values_all_algae_norm.csv')



#ooookkkkk
#now can find where the areas change the most

#now actually standardise
standard<- function (x){
  return((x- mean(x[x]))/sd(x[x]) )
}


stand_list<-lapply(dif_list85, standard )
par(mfrow=c(3,3))
for (i in 1:length(stand_list)){
  plot(stand_list[[i]])
} 


#split in tropical and subtropical
trop_stack<-stack(stand_list[1])
subtrop_stack<-stack(stand_list[c(2,3,4,5)])

par(mfrow=c(1,1))

plot(trop_stack)

plot(subtrop_stack)


#where changes
trop_stack_sum<-(sum(trop_stack)/1) 
plot(trop_stack_sum)


subtrop_stack_sum<-(sum(subtrop_stack)/4)
plot(subtrop_stack_sum)

pall <- c('red3', 'lightsalmon1', 'white', 'cadetblue1', 'blue2')

par(mfrow=c(1,2))
plot(trop_stack_sum, col=colorRampPalette(pall[1:5])(25), main='Tropical')
plot(japan_outline, col='grey68', border='grey68', add=TRUE)
box()
plot(trop_stack_sum, col=colorRampPalette(pall[1:5])(25),add=TRUE)


plot(subtrop_stack_sum, col=colorRampPalette(pall[1:5])(25), main='Subtropical')
plot(japan_outline, col='grey68', border='grey68', add=TRUE)
box()
plot(subtrop_stack_sum, col=colorRampPalette(pall[1:5])(25),add=TRUE)

setwd("S:/Beger group/Katie Cook/Japan_data/SDM_course_git")

writeRaster(subtrop_stack_sum, 'change_hotspots/algae_subtrop_hotspots.tif', format='GTiff')
writeRaster(trop_stack_sum, 'change_hotspots/algae_trop_hotspots.tif', format='GTiff')

#plot hotspot areas
#split in tropical and subtropical
trop_stack<-stack(dif_list85[c(1)])
subtrop_stack<-stack(dif_list85[c(2,3,4,5)])



plot(trop_stack)
plot(subtrop_stack)



#where changes
trop_stack_sum<-(sum(trop_stack)) 
plot(trop_stack_sum)


subtrop_stack_sum<-(sum(subtrop_stack)/2)
plot(subtrop_stack_sum)

#trop_stack_sum[trop_stack_sum==0]<-NA
#subtrop_stack_sum[subtrop_stack_sum==0]<-NA

plot(subtrop_stack_sum)

pall <- c('red3', 'lightsalmon1', 'white', 'cadetblue1', 'blue2')

par(mfrow=c(1,2))
plot(trop_stack_sum, col=colorRampPalette(pall[1:5])(25), main='Tropical')
plot(japan_outline, col='grey68', border='grey68', add=TRUE)
box()
plot(trop_stack_sum, col=colorRampPalette(pall[1:5])(25),add=TRUE)


plot(subtrop_stack_sum, col=colorRampPalette(pall[1:5])(25), main='Subtropical')
plot(japan_outline, col='grey68', border='grey68', add=TRUE)
box()
plot(subtrop_stack_sum, col=colorRampPalette(pall[1:5])(25),add=TRUE)



writeRaster(subtrop_stack_sum, 'change_hotspots/algae_subtrop_hotspots.tif', format='GTiff', overwrite=TRUE)
writeRaster(trop_stack_sum, 'change_hotspots/algae_trop_hotspots.tif', format='GTiff', overwrite=TRUE)


#need to get the same legend
stack_posterplot<-stack(dif_list[[2]], dif_list[[4]], dif_list[[5]], dif_list[[8]])

par(mfrow=c(2,2),  mai = c(0.3,0.2,0.2,0.2))

plot(stack_posterplot)

p1<-spplot(stack_posterplot, col.regions=terrain.colors)
p1
p1+layer(sp.polygons(japan_outline))


### GET DF FOR PCA ----
#extract dif at lat lons
lonlat<-data.frame(lon=latlon$lon, lat=latlon$lat)

for (i in 1:length(dif_list85)){
  dif_sites<- extract(dif_list85[[i]], lonlat )
  dif_sites<-data.frame(site= latlon$Site, change= dif_sites)
  assign(paste0('dif_sites', i), dif_sites)
}

rm(dif_sites)
dif_site_list<- lapply(ls(pattern='dif_sites'), get)

#now add on group number to list and unlist into one big df.
for(i in 1:length(dif_site_list)){
  dif_site_list[[i]]$group<-paste0('algae', i)
}

dif_site_all<-do.call(rbind, dif_site_list)



#now write csv
write.csv(dif_site_all, 'dif_site_alage.csv')








#split into below zero and above zero
dif_list<-dif_list85

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



#subtropical group; 3,4,5
#tropical group: 1, 

#split in tropical and subtropical
increase_trop_stack<-stack(increase_list[1])
plot(increase_trop_stack)

increase_subtrop_stack<-stack(increase_list[c(2:5)])
plot(increase_subtrop_stack)

decrease_trop_stack<-stack(decrease_list[c(1)])
plot(decrease_trop_stack)

decrease_subtrop_stack<-stack(decrease_list[c(2:5)])
plot(decrease_subtrop_stack)

#where changes the most? values between 0, 1 for increase 
library(sdmvspecies)

rescale_increase_trop<-rescale(increase_trop_stack)
plot(rescale_increase_trop)

rescale_increase_subtrop<-rescale(increase_subtrop_stack)

trop_increase<-sum(rescale_increase_trop)

sub_trop_increase<-rescale(sum(rescale_increase_subtrop))

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
subtrop_decrease<-(rescale(sum(rescale_decrease_subtrop))/-1)

plot(subtrop_decrease)

library(RColorBrewer)
library(viridis)

#pal <- viridis(n=20, option='plasma' )
invpal<-colorRampPalette(c('white', 'yellow', 'orange', 'deeppink2', 'darkviolet'))
pal<-colorRampPalette(c('darkviolet', 'deeppink2', 'orange','yellow', 'white'))


par(mfrow=c(2,2))
plot(trop_increase, col=invpal(50), main='Tropical FG Increase')
plot(japan_outline, col='grey68',border='grey68', add=TRUE)
box()
plot(sub_trop_increase, col=invpal(50), main='Subtropical FG Increase')
plot(japan_outline, col='grey68', border='grey68', add=TRUE)
box()


plot(trop_decrease, col=pal(50), main='Tropical FG Decrease')
plot(japan_outline, col='grey68',border='grey68', add=TRUE)
box()
plot(subtrop_decrease, col=pal(50), main='Subtropical FG Decrease')
plot(japan_outline,col='grey68',border='grey68', add=TRUE)
box()


trop_increase_un<-unstack(trop_increase)
trop_decrease_un<-unstack(trop_decrease)

plot(trop_increase_un[[1]])
plot(trop_decrease_un[[1]])

tropical<- calc(stack(trop_increase_un[[1]], trop_decrease_un[[1]]), sum, na.rm = T)

plot(tropical)


subtropical<-calc(stack(sub_trop_increase, subtrop_decrease), sum, na.rm=T)
plot(subtropical)


invpal<-colorRampPalette(c('red3', 'lightsalmon1', 'white', 'cadetblue1', 'blue2'))


par(mfrow=c(1,2))
plot(tropical, col=invpal(50), main='Tropical')
plot(japan_outline,col='grey68',border='grey68', add=TRUE)
box()

plot(subtropical, col=invpal(50), main='Subtropical')
plot(japan_outline, col='grey68', border='grey68', add=TRUE)
box()

#now export these rasters
writeRaster(tropical, 'plotting/tropical_algae_hotspots.tiff', overwrite=TRUE)
writeRaster(subtropical, 'plotting/subtropical_algae_hotspots.tiff')

#need to get the same legend
stack_posterplot<-stack(dif_list[[2]], dif_list[[4]], dif_list[[5]], dif_list[[8]])

par(mfrow=c(2,2),  mai = c(0.3,0.2,0.2,0.2))

plot(stack_posterplot)

p1<-spplot(stack_posterplot, col.regions=terrain.colors)
p1
p1+layer(sp.polygons(japan_outline))










