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

groups<-read.csv('0503clust_ech.csv') 
groups<-groups[,c(1,16,17)] 


#check if theres species that dont match 
groups$Species<-as.character(groups$Species)
ech$Species<-as.character(ech$Species)

not_in_groups<- which ( ! unique (groups$new_names) %in% unique (ech$Species))
not_in_surv<- which ( ! unique (ech$Species) %in% unique (groups$new_names))

groups$new_names[not_in_groups]
ech$Species[not_in_surv] #just acanthaster planci 3 times? sort this later if need to 


summary(groups)

groups<- groups %>% mutate(sum=1)

k8<- groups %>% group_by(groupk8) %>% summarise(number=sum(sum))
k12<-groups %>% group_by(groupk12) %>% summarise (number=sum(sum))

k8
k12

#ok merge by k8 (try this first)


which ( ! unique (groups$Species) %in% unique (ech$Species))
which ( ! unique (ech$Species) %in% unique (groups$Species))


ech[1,3]
groups[1,1]<-'Acanthaster planci'

ech[32,3]
groups[7,1]<-'Anneissia bennetti'


k8<-groups[,c(1,2)]

names(k8)<-c('Species','group')

ech_group<-left_join(ech, k8, by='Species')

torm<-which(is.na(ech_group$group))

ech_group$Species[torm]

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

#remove the groups with too many zeros
which( group_sum$abundance[group_sum$group==1]==0) #5 zeros
length(which( group_sum$abundance[group_sum$group==2]==0)) #17 zeros
length(which( group_sum$abundance[group_sum$group==3]==0)) #8 zeros
length(which( group_sum$abundance[group_sum$group==4]==0) ) #25 zeros
length(which( group_sum$abundance[group_sum$group==5]==0) ) #8 zeros
length(which( group_sum$abundance[group_sum$group==6]==0) ) #28 zeros
length(which( group_sum$abundance[group_sum$group==7]==0) )  #20 zeros
length(which( group_sum$abundance[group_sum$group==8]==0) ) #29 zeros

#rm 4,6 and 8? ]
#group 4, only 2 sp,
#group 6 only 2 sp
#group 8 only 1 sp 

torm<- which(group_sum$group==8)
torm
group_sum<-group_sum[-torm,]

#make group 7 group 4 and remove group 6
torm<- which(group_sum$group==4)
group_sum<-group_sum[-torm,]

change<- which(group_sum$group==7) 
group_sum$group[change]<- 4

torm<- which(group_sum$group==6)
torm
group_sum<-group_sum[-torm,]

#so group 7 = group 4. 

#check
#ok plot abundance by lat 
ggplot(group_sum, aes(x=lat, y=abundance))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~group, scales='free')+
  theme_bw()



###need to change but write the code so it runs for echinos 


#ok round up

group_sum$abundance<- ceiling(group_sum$abundance)


#now histogram #looks like poisson
ggplot(group_sum, aes(x=abundance))+
  geom_histogram()+
  facet_wrap(~group, scales='free')

#here remove small groups----

#looks poisson dist. Use neg binomial.

#ok now split into individual groups----
for (i in 1:length(unique(group_sum$group))){
  rows<-which(group_sum$group==i)
  assign(paste0("Obvs_gr_", i), data.frame(group_sum[rows,]))
}


#
#loop all groups ----

v1<-vifstep(current_preds, th=10) #nothing got rid of
v1

library(lme4)
library(mgcv)
library(gamm4)

#rmse function 
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
  
  
  for (i in 1:100){
    tryCatch( {
      #define test and training data
      test<- data[sample(nrow(data), size=6, replace=FALSE),]  
      train<- data[(! row.names(data) %in% row.names(test)), ]
      
      obvs<-test$abundance
      
      #make models
      
      ##GLM
      glm1<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , data=train)
      glm1<- step(glm1, trace = 0, na.action=na.omit)
      
      ##GAM
      gam1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss, k=5), family=nb(), data=train)
      
      abundance<-train$abundance
      abundance<-as.data.frame(abundance)
      
      
      #make df for loop to fill 
      gam_step_table<-data.frame(summary(gam1)$s.table)
      out_varib<-row.names(gam_step_table[gam_step_table$p.value>=0.1,])
      
      #set up formula to change 
      form<-formula(paste(abundance, "~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss, k=5) ", sep=""))
      
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
      
      
      #RF
      rf1<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss  +
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




###full model, predict and ensemble   #loop

for (i in 1:length(extract_all)) {
  
  #GLM 
  glm_gr<-glm.nb(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss,  data=extract_all[[i]])
  glm_gr<-step(glm_gr, trace = 0, na.action=na.omit)
  
  
  #GAM
  gam1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss, k=5), family=nb(), data=extract_all[[i]])
  
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
                        BO2_chlomean_ss, data=extract_all[[i]], ntree=300, importance=TRUE   )
  
  
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

plot(ensemble_gr1)

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




#now future RCP85
rm(glm_gr)

glm_list<-lapply(ls(pattern='glm_gr'), get)

rm(gam_gr)
gam_list<-lapply(ls(pattern='gam_gr'), get)

rm(rf_gr)

rf_list<-lapply(ls(pattern='rf_gr'), get)


#make sure the colnames match
names(RCP85_2050)<-names(current_preds)

for (i in 1:length(extract_all)) {
  
  pr_glm_fut<-predict(RCP85_2050, glm_list[[i]], type='response')
  pr_gam_fut<-predict(RCP85_2050, gam_list[[i]], type='response')
  pr_rf_fut<-predict(RCP85_2050, rf_list[[i+5]], type='response')
  
  assign(paste0('glm_fut_85', i), pr_glm_fut)
  assign(paste0('gam_fut_85', i), pr_gam_fut)
  assign(paste0('rf_fut_85', i), pr_rf_fut)
  
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
  
  
  ensemble_fut<- ((pr_glm_fut*props[1,1])+(pr_gam_fut*props[1,2])+(pr_rf_fut*props[1,3]))
  
  assign(paste0('fut_ensemble_85_gr', i), ensemble_fut)
  
  
  
}

plot(fut_ensemble_85_gr2)

fut_ens85_list<-lapply(ls(pattern='fut_ensemble_85_gr'), get)




#plot
par(mar=c(2,2,2,2))
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

#ens list 
for (i in 1:length(ens_list)){
  ens_mask<-mask(ens_list[[i]], buffer30k)
  assign(paste0('ens_mask_gr', i),ens_mask)
}

ens_mask<-lapply(ls(pattern='ens_mask_gr'),get)

#now get difference between two and plot
#before getting difference standardise abundance between zero and 1 ----

#normalize <- function(x) {
#return ((x -  x@data@min)/ (x@data@max -  x@data@min))
#}

#fut_ens85_norm<- lapply(ens_mask85_list, normalize)

#ens_list_norm<-lapply(ens_mask, normalize)

#fut_ens26_norm<-lapply(ens_mask26_list, normalize)

fut_ens85_norm<-ens_mask85_list
ens_list_norm<-ens_mask
fut_ens26_norm<-ens_mask26_list

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


plot(fut_ens85_norm[[6]])
plot(ens_list_norm[[6]])

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

#buffer30k<-readOGR('30k_coastplot/outline30k.shp')
#crs(buffer30k)

#buffer30k<-spTransform(buffer30k,  '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' )
#
#crs(japan_outline)
#now mask dif 85 by buffer crop

#for (i in 1:length(dif_list85)){
# dif_mask<- mask(dif_list85[[i]], buffer30k)
# assign(paste0('dif85_mask_gr', i), dif_mask)
#}

#dif_mask85<-lapply(ls(pattern='dif85_mask_gr'), get)



dif_mask85<-dif_list85



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
values<- extract(dif_mask85[[2]], coords)

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

#NOW REMOVE GROUP 2 & 4
dif85_values_all<-dif85_values_all[-4]
dif85_values_all<-dif85_values_all[-2]

par(mfrow=c(3,3))
for (i in 1: length(dif85_values_all)){
  plot(values~x, data=dif85_values_all[[i]])
}

#group into 1 df
for (i in 1: length(dif85_values_all)){
  dif85_values_all[[i]]$group<-i
  dif85_values_all[[i]]$climate<-'RCP85'
}

dif85_values_all[[2]]$group<-3
dif85_values_all[[3]]$group<-5

View(dif85_values_all[[1]])



#now merge them all together 
dif85_all_df<-do.call(rbind, dif85_values_all)

dif85_all_df$group<-as.factor(dif85_all_df$group)

ggplot(dif85_all_df, aes(x=y, y=values, col=group))+
  #geom_point()+
  geom_smooth(method='loess')+
  theme_bw()+
  ylim(10,-10)+
  geom_hline(yintercept=0, linetype='dotted')


#RCP 26

#now mask dif 85 by buffer crop

dif_mask26<-dif_list26

#for (i in 1:length(dif_list26)){
#dif_mask<- mask(dif_list26[[i]], buffer30k)
#assign(paste0('dif26_mask_gr', i), dif_mask)
#}

#dif_mask26<-lapply(ls(pattern='dif26_mask_gr'), get)

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

dif26_values_all<-dif26_values_all[-4]
dif26_values_all<-dif26_values_all[-2]

#group into 1 df
for (i in 1: length(dif26_values_all)){
  dif26_values_all[[i]]$group<-i
  dif26_values_all[[i]]$climate<-'RCP26'
}

dif26_values_all[[2]]$group<-3
dif26_values_all[[3]]$group<-5

View(dif26_values_all[[1]])

#now merge them all together 
dif26_all_df<-do.call(rbind, dif26_values_all)





dif26_all_df$group<-as.factor(dif26_all_df$group)


ggplot(dif26_all_df, aes(x=slope, y=values, col=group))+
  geom_smooth(method='loess')+
  theme_bw()
# facet_zoom(ylim=c(-2, 2))  #zooms in on the smaller ones 

#now merge both climate scenarios
dif_values_all<- rbind(dif85_all_df, dif26_all_df)
dif_values_all$climate<-as.factor(dif_values_all$climate)

#plot group 1 separately 
group1<-dif_values_all[dif_values_all$group==1,]
torm<- which(dif_values_all$group==1)

dif_values_filter<-dif_values_all[-torm,]

ggplot(dif_values_filter, aes(x=x, y=values, col=group))+
  geom_smooth(method='loess')+
  labs(x='Latitude', y=' Change in abundance')+
  facet_wrap(~climate)+
  geom_hline(yintercept=0, linetype='dotted')+
  theme_bw()

ggplot(group1, aes(x=x, y=values, col=group))+
  geom_smooth(method='loess')+
  labs(x='Latitude', y=' Change in abundance')+
  facet_wrap(~climate)+
  geom_hline(yintercept=0, linetype='dotted')+
  theme_bw()


write.csv(dif_values_all, 'dif_values_all_molluscs_norm.csv')

ggplot(dif_values_all[dif_values_all$group==1,], aes(x=y, y=values) )+
  geom_point()+
  geom_smooth(method='loess')



#ooookkkkk
#now can find where the areas change the most


#split into trop/subtrop

#FIX THIS 

#subtropical group; 
#tropical group: 1

#standardize the groups (test)
stand<- (dif_list85[[1]]- mean(dif_list85[[1]][dif_list85[[1]]]))/sd(dif_list85[[1]][dif_list85[[1]]])
stand2<- (dif_list85[[2]]- mean(dif_list85[[2]][dif_list85[[2]]]))/sd(dif_list85[[2]][dif_list85[[2]]])

plot(stand)
plot(dif_list85[[1]])

plot(stand2)
plot(dif_list85[[2]]) 

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
trop_stack<-stack(stand_list[c(3,7)])
subtrop_stack<-stack(stand_list[c(1,2,4,6)])



plot(trop_stack)
plot(subtrop_stack)


#where changes
trop_stack_sum<-(sum(trop_stack)/2) 
plot(trop_stack_sum)


subtrop_stack_sum<-(sum(subtrop_stack)/4)
pall <- c('red3', 'lightsalmon1', 'white', 'cadetblue1', 'blue2')

par(mfrow=c(1,2))
plot(trop_stack_sum, col=colorRampPalette(pall[1:5])(25), main='Tropical')
plot(japan_outline, col='grey68', border='grey68', add=TRUE)
box()
plot(trop_stack_sum, col=colorRampPalette(pall[1:5])(25),add=TRUE)


plot(subtrop_stack_sum, col=colorRampPalette(pall[1:3])(25), main='Subtropical')
plot(japan_outline, col='grey68', border='grey68', add=TRUE)
box()
plot(subtrop_stack_sum, col=colorRampPalette(pall[1:3])(25),add=TRUE)



writeRaster(subtrop_stack_sum, 'change_hotspots/molls_subtrop_hotspots.tif', format='GTiff')
writeRaster(trop_stack_sum, 'change_hotspots/molls_trop_hotspots.tif', format='GTiff')


#need to get the same legend
stack_posterplot<-stack(dif_list[[2]], dif_list[[4]], dif_list[[5]], dif_list[[8]])

par(mfrow=c(2,2),  mai = c(0.3,0.2,0.2,0.2))

plot(stack_posterplot)

p1<-spplot(stack_posterplot, col.regions=terrain.colors)
p1
p1+layer(sp.polygons(japan_outline))

















