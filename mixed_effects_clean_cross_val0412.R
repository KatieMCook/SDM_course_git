#####OK new script for mixed effects models that makes sense! #####tropicalising fish 2

library(dplyr)
library(sdmpredictors)
library(dismo)
library(reshape2) #needs stringi?
library(maptools)
library(mapdata)
library(ggmap)
library(sf)
library(rgeos)
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

#load in fish workspace Jan
#setwd("S:/Beger group/Katie CookJapan_data/SDM_course_git")
setwd("D:/corona_contingency/SDM_course_git")
#setwd("F:/corona_contingency/SDM_course_git")
#read in Japan if starting ###### START HERE 
japan_outline<-readOGR('plotting/japan_outline.shp')


options(sdmpredictors_datadir="D:/corona_contingency/SDM_course_git")

#change predictors 

#extract data 
current_preds<-load_layers(c('BO_sstmin','BO21_dissoxmean_bdmean' ,
                             'BO_damean', 'BO2_curvelmax_ss'))

#current_preds<-load_layers(c('BO_sstmin','BO21_dissoxmean_bdmean' ,
#        'BO2_curvelmax_ss'))



res(current_preds)
plot(current_preds)





#extract data 
#current_preds<-load_layers(c('BO21_temprange_ss', 'BO21_dissoxmean_bdmean',
#  'BO_parmean' ,'BO_damean', 'BO2_curvelmax_ss'))


#chlorophyll correlated with temp so remove
#salinity correlated with ox so remove
##temp min correlated with temp range so remove ss
#remove temp range mean because we want surface


plot(current_preds[[1]])

#now crop to 30k
#buffer30k<-readOGR('30k_coastplot/outline30k.shp')
#crs(buffer30k)

#buffer30k<-spTransform(buffer30k,  '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' )

#plot(buffer30k)

#10k coast buffer
buffer10k<-readOGR('10k_coastplot/buffer_10k_proj.shp')
crs(buffer10k)



current_preds<-crop(current_preds, extent(buffer10k))

plot(current_preds)

#also crop with predict_area
predict_area<-readOGR('plotting/japan_predictarea.shp')

predict_area<-aggregate(predict_area)

crs(predict_area)<-('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' )

current_preds<-mask(current_preds, predict_area)

par(mfrow=c(3,4))

plot(current_preds)

#mask by 10k
current_preds<-mask(current_preds, buffer10k)

plot(current_preds)

#names(current_preds)<-c('temp_range','o2','par','da','current')

names(current_preds)<-c('temp_min','o2','da','current')
par(mfrow=c(2,2))

#names(current_preds)<-c('temp_min','o2','current')



plot(current_preds[[1]])


#now check co-linearity
v1<-vifstep(current_preds, th=10)

v1@results    

rastervif<-exclude(current_preds, v1)
plot(rastervif) #ok

#add lat lon data to obs
latlon<-read.csv('JP2015_16_waypoints.csv')

latlon<- latlon[,c(3,2)]

plot(current_preds)

#extract values at lat lon
env_extract<- raster::extract(current_preds, latlon)

env_extract<-data.frame(env_extract)

#check correlations
correlations<-data.frame(cor(env_extract))

#ok so temp_range_SS, ox, par, bathy, da and current


#FISH SPECIES DATA----
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
fgroup<-read.csv('fishtraitgroups_2301.csv')

fgroup$Species<-as.character(fgroup$Species)

fgroup<-fgroup[,c(1,15)]

names(fgroup)<-c('Species', 'group'  )

names(fish_survey)<-c("SiteID" ,     "Name"   ,     "Year"  ,      "Date"    ,    "Transect"   , "Species", "Number" ,     "SizeCm"  ,    "B_g" ) 


#remove group 5 and 12 (only 1 species in each and replace with group 13 and 14)
fgroup<-fgroup[-c(which(fgroup$group==5)),]
fgroup$group[fgroup$group==13]<-5

fgroup<-fgroup[-c(which(fgroup$group==12)),]
fgroup$group[fgroup$group==14]<-12


#merge
func_group_all<-left_join(fish_survey, fgroup, by='Species')


#ok now get the species we want across lat
func_group_all<-func_group_all[,c(1,5,6,7,8,9)]

#add latitude
#add lat lon data to obs
latlon<-read.csv('JP2015_16_waypoints.csv')

latlon<-latlon[,c(2,3,4)]
latlon$Site<-as.character(latlon$Site)

names(latlon)<- c( "lat",  "lon"  ,"SiteID")

func_group_all$SiteID<-as.character(func_group_all$SiteID)

site_sp<-left_join(func_group_all, latlon, by="SiteID")

#1) SPECIES MODELS 
#species that have >10 observations 
#ok now work out which species have >15 observations 
#total species 
length(unique(site_sp$Species)) #392

site_sp<- site_sp %>% group_by(Species, SiteID, lat, lon) %>% summarise(abundance=mean(Number))

site_matrix<- acast(site_sp, SiteID~Species, value.var = 'abundance')

site_matrix_pres<- site_matrix

site_matrix_pres[site_matrix_pres > 0]<-1  

site_pres<-data.frame(colSums(site_matrix_pres, na.rm = TRUE))

nrow(site_pres)

length(which(site_pres$colSums.site_matrix_pres..na.rm...TRUE. >  10))  #50 species, create models for these 

subset_ten<-which(site_pres$colSums.site_matrix_pres..na.rm...TRUE.>10)

filter10<-as.data.frame(site_pres)

filter10<-as.data.frame(site_pres[subset_ten,])

names<-colnames(site_matrix_pres)

subset_ten<- names[subset_ten]


rownames(filter10)<-subset_ten

write.csv(filter10, 'subset10_n.csv')

#ok now get abscences
site_matrix[is.na(site_matrix)]<-0

site_matrix_df<-data.frame(site_matrix)

site_abs<- reshape2::melt(site_matrix, value.name = 'abundance')

names(site_abs)<- c('SiteID', 'Species', 'abundance')

subset_ten<- site_abs[(site_abs$Species %in% subset_ten),]

names(subset_ten)

#ok now add the lat lon back on 
subset_10<-left_join(subset_ten, latlon, by='SiteID')

names(subset_10)<- c('SiteID', 'Species', 'abundance', 'lat', 'lon')

#now add the FGs back on 
subset_10<-left_join(subset_10, fgroup, by='Species')

hist(subset_10$group)

unique(subset_10$group) #group 7 10 12 missing


#ok 
env_extract$lat<-latlon$lat
subset_10<- left_join(subset_10, env_extract, by='lat')

names(env_extract)



#make models 

library(lme4)


subset_10$Species<-as.character(subset_10$Species)

#standardise the coefficients 

subset_10$temp_min.sc<- scale(subset_10$temp_min)
subset_10$o2.sc<- scale(subset_10$o2)
subset_10$da.sc<- scale(subset_10$da)
subset_10$current.sc<- scale(subset_10$current)

length(unique(subset_10$Species))

#now make it so the data is numeric
subset_10$abundance.sc <- round(sqrt((subset_10$abundance * 100) + 1)) 


#model ONE SPECIES
m1 <- glmer(abundance.sc ~ 
              temp_min.sc +
              o2.sc+
              #par.sc+
              da.sc+
              current.sc+
              #(1 | group) + 
              (1 | Species) + 
              
              (0 + temp_min.sc | Species) + 
              (0 + o2.sc | Species) + 
              # (0 + par.sc | Species) + 
              (0 + da.sc | Species) + 
              (0 + current.sc | Species) ,
            data = subset_10,  family = poisson(link = "log"))

#check for singularity?

isSingular(m1) #FALSE

library(DHARMa)

#test for dispersion 
testDispersion(m1) #not dispersed? 

simulationOutput <- simulateResiduals(fittedModel = m1, plot = F)

residuals(simulationOutput)

plot(simulationOutput)

qqnorm(residuals(m1))

#ok now plot the coefficient plots 1. FIXED EFFECTS


#extract confidence intervals to make a plot
sp_fe_sum<-data.frame(summary(m1)$coefficients)

sp_fe_sum<-sp_fe_sum[-1,]


confint<- data.frame(confint(m1, method='Wald', level = 0.5))
confint2<- data.frame(confint(m1, method='Wald', level = 0.95))
confint<-confint[c(7:10),]
confint2<-confint2[c(7:10),]

sp_fe_sum<-cbind(sp_fe_sum, confint)
sp_fe_sum<-cbind(sp_fe_sum, confint2)

names(sp_fe_sum)

#Now make a boxplot
test <- sp_fe_sum
test$names <- row.names(test)

names(test) <- c("Estimate","Std..Error" ,"z.value" ,   "Pr" ,  "X25"  ,    "X75" ,     "X2.5"   ,  "X97.5" ,   "names" )

coef_boxplot<-ggplot()+
  geom_boxplot(data=test, 
               stat = "identity", 
               aes(y = names,
                   xmin =  X2.5, 
                   xlower = X25, 
                   xmiddle = Estimate, 
                   xupper = X75, 
                   xmax = X97.5,
                   group = names), fill='lightgray') +
  scale_x_continuous( limits= c(-1, 1))


coef_boxplot+geom_vline(xintercept=0, linetype="dashed", color = "lightsalmon2")+theme_bw()



coef_lineplot<-ggplot()+
  geom_linerange(data=test, 
                 stat = "identity", 
                 aes(y = names,
                     xmin =  X2.5, 
                     xmax = X97.5,
                     group = names), color='darkblue', size=1 ) +
  scale_x_continuous( limits= c(-1, 1))


#add axis labels with line break 
addline_format <- function(x,...){
  gsub('\\s','\n',x) 
}



coef_lineplot<- coef_lineplot+geom_point(data=test, aes(y=names, x=Estimate), size=2, color='darkblue')

coef_lineplot<- coef_lineplot+geom_vline(xintercept=0, linetype="dashed", color = "lightsalmon2")+theme_bw()+
  labs(y='Coefficient', y='Value')+
  scale_y_discrete(labels = addline_format(c('Current','Diffuse Attenuation', 'Dissolved Oxygen', 'Minimum Temperature')))

coef_lineplot+theme(panel.background = element_rect(fill = "lightgray"))

summary(sp_mod)

#ok now make ranef plot 

#remove the intercept plot
ranef_species<- data.frame(ranef(m1, condvar=TRUE))

ranef_species$term<-as.character(ranef_species$term)


torm<- which(ranef_species$term=='(Intercept)')

ranef_species<-ranef_species[-c(torm),]

ranef_species$term[ranef_species$term=='temp_min.sc']<-'Minimum Temperature'
ranef_species$term[ranef_species$term=='current.sc']<-'Current Velocity'
ranef_species$term[ranef_species$term=='o2.sc']<-'Oxygen Concentration'
ranef_species$term[ranef_species$term=='da.sc']<-'Diffuse Attenuation'

fgroup_merge<-fgroup

names(fgroup_merge)<-c("grp", "group"  )

ranef_species<- left_join(ranef_species, fgroup_merge, by='grp')

ranef_species_sort<-ranef_species[order(ranef_species$group),]

ranef_species_sort$group<-as.factor(ranef_species_sort$group)

#relevel to plot in order of groups 
levels_want<- unique(ranef_species_sort$grp)

ranef_species_sort$grp<-factor(ranef_species_sort$grp, levels=c(levels_want))

levels(ranef_species_sort$grp)

ggplot(ranef_species_sort, aes(x=condval, y=grp, col=group))+
  geom_point(size=2 )+ #color='darkblue'
  scale_color_manual(values=c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",  "#FF7F00", "#CAB2D6", 'darkgoldenrod1'  ))+ #"#FFFF99" 
  facet_wrap(~term)+
  geom_linerange(aes(xmin=condval-condsd, xmax=condval+condsd), color='darkblue')+
  geom_vline(xintercept=0, linetype="dashed", color = "lightsalmon2")+theme_light(base_size=12)+theme(axis.text.y = element_text(face='italic',size=8),
                                                                                                      axis.title = element_text(face="bold"),
                                                                                                      strip.text = element_text(size=15))+
  labs(y='Species', x='Conditional Variance')


summary(m1)

#ok now make models  for functional groups with ALL data, removing out comparaison species as we go 

#ok for model with  ALL groups (not yet) 
fgroup_site<-read.csv('func_group_site.csv')

fgroup_site<-fgroup_site[,-1]

fgroup_site<-left_join(fgroup_site, env_extract, by='lat')

#right now loop through removing the species data and rebuild model and check 

# add the lat lon back on to data with zeros
site_abs<-left_join(site_abs, latlon, by='SiteID')

names(site_abs)<- c('SiteID', 'Species', 'abundance', 'lat', 'lon')

#now add the FGs back on 
site_abs<-left_join(site_abs, fgroup, by='Species')

torm<-which(is.na(site_abs$group))

site_abs<-site_abs[-c(torm),]

#ok loop through, remove each name sequentially and rebuild functional group model without the species

#set up coefficients table
sp_gr_coef<- data.frame(intercept=1, temp_min=1, o2=1, da=1, current=1, group=1, species='species')

sp_gr_coef$species<-as.character(sp_gr_coef$species)

#remove species in names but not groups
subset_names<-unique(subset_10$Species)

#now loop through 

for (i in 1:length(subset_names)){   #names= fish names
  torm<- which(site_abs$Species==subset_names[i])
  site_abs_rm<- site_abs[-c(torm),]
  
  fg_site<-site_abs_rm %>% group_by(SiteID, lat, lon, group) %>% summarise(abundance=sum(abundance))
  
  fg_site<- left_join(fg_site,env_extract, by='lat' )
  
  
  #ok now build model
  #standadise coefs 
  fg_site$temp_min.sc<-scale(fg_site$temp_min)
  fg_site$o2.sc<-scale(fg_site$o2)
  fg_site$da.sc<-scale(fg_site$da)
  fg_site$current.sc<-scale(fg_site$current)
  
  
  #model time!
  fg_site$abundance.sc<- round(sqrt((fg_site$abundance*100)+1))
  
  #model
  m2 <- glmer(abundance.sc ~ 
                temp_min.sc +
                o2.sc+
                da.sc+
                current.sc+
                (1 | group) + 
                (0 + temp_min.sc | group) + 
                (0 + o2.sc | group) + 
                (0 + da.sc | group) + 
                (0 + current.sc | group) ,
              data = fg_site,  family = poisson(link = "log"))
  
  #groups coef
  groups_coef<- as.data.frame(coef(m2)$group)  
  
  group<-fgroup$group[fgroup$Species==subset_names[i]]
  
  groups_coef$group<-rownames(groups_coef)
  
  coefs<- groups_coef[groups_coef$group==group,]
  
  sp_gr_coef[i,7]<-subset_names[i]
  sp_gr_coef[i, c(1:6)]<- coefs[1,]
  
}


#ok now combine with the species data 
species_coef<- coef(m1)$Species

species_coef$Species<-rownames(species_coef)

#species coefficients and group coefficients in the same df
combined<- species_coef[,c(2:6)]

names(sp_gr_coef)<- c( "intercept"  ,"temp_min", "o2"       ,  "da"    ,         "current"  ,  "group"  , 'Species' ) 


combined<- left_join(combined, sp_gr_coef, by='Species')


#ok now plot them together
library(tidyr)

#make it long form so it can be ggplotted
combined_coef_s<-gather(combined, variable, value, c(temp_min.sc,o2.sc, da.sc, current.sc))

combined_coef_g<-gather(combined, variable, value, c(temp_min,o2, da,current))

combined_coef_s<- combined_coef_s[,c(1,7,8,9)]

combined_coef_g<-combined_coef_g[,c(5,7,8,9)]

names(combined_coef_s)<- c ("species" , "group"  ,  "variable" ,"value_species")  
names(combined_coef_g)<-c ("species" , "group"  ,  "variable" ,"value_group")  

combined_coef_s$value_group<-combined_coef_g$value_group

unique(combined_coef_s$group)

#NOW PLOT
combined_coef_s$Functional_Group<-factor(combined_coef_s$group, levels=c("1" ,"2","3" , "4" , "5" , "6", "8", "9" ,"11"))

library(wesanderson)

combined_coef_s$group<-as.factor(combined_coef_s$group)
combined_coef_s$value_group<-as.numeric(combined_coef_s$value_group)

p1<- ggplot(combined_coef_s, aes(x=variable, y=value_species) )+
  geom_hline(yintercept=0, linetype="dashed", color = "lightsalmon2")+
  #scale_fill_manual(values=c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",  "#FF7F00", "#CAB2D6",  "#FFFF99" ))+
#  geom_boxplot(color='grey36', fill='grey73')+
  geom_boxplot(color='grey36', aes(fill=Functional_Group))+
  facet_wrap(~Functional_Group)+
  scale_fill_manual(values=c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",  "#FF7F00", "#CAB2D6",  "#FFFF99" ))+
    labs(x='Environmental Coefficient', y='Model Coefficient')+
  #scale_fill_manual(values=wes_palette('Darjeeling1', n=5))+
  scale_x_discrete(labels = c('Current','Dissolved Oxygen', 'Diffuse Attenuation', 'Min Temperature'), guide = guide_axis(n.dodge = 2))+
  theme_light(base_size = 12)+theme(strip.text = element_text(size=15),axis.title = element_text(face="bold"))
p1

vars <- c("Group Level"="black")



p1

p1<-p1+ geom_point(data=combined_coef_s, aes(x=variable, y=value_group) , pch=22, col='black', fill='darkgrey', size=2)

p1

p1+geom_hline(yintercept=0, linetype="dashed", color = "lightsalmon2")




#to get the palette cols
library(RColorBrewer)
f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
cols_paired<-(cols <- f("Paired"))



#OK now finally, check PCA of groups
#ok now we've done the boxplot try running the pCA on this 
#try running a PCA on this 
sp_PCA<- data.frame(coef(m1)$Species)

sp_PCA<-sp_PCA[,-1]

library(vegan)
library(ggfortify)

pca<-prcomp(sp_PCA)

autoplot(pca)

group_sp<-which(fgroup$Species %in% rownames(sp_PCA))

group_50<- fgroup[group_sp,]

sp_PCA$Species<-rownames(sp_PCA)

sp_PCA<-left_join(sp_PCA, group_50, by='Species')

sp_PCA$group<- as.factor(sp_PCA$group)

autoplot(pca, data=sp_PCA, colour='group', loadings=TRUE,loadings.label = TRUE, loadings.label.size = 3,
         frame = TRUE, )
frame.type = 'norm'  #doesn't really show anything 




#RIGHT NOW WE CAN USE THESE TO PREDICT THE NEW RANGES

#FIRST MAKE A OVERALL GROUP MODEL 

#func group by abundance

fg_site<-site_abs %>% group_by(SiteID, lat, lon, group) %>% summarise(abundance=sum(abundance))

fg_site<- left_join(fg_site,env_extract, by='lat' )


#ok now build model
#standadise coefs 
fg_site$temp_min.sc<-scale(fg_site$temp_min)
fg_site$o2.sc<-scale(fg_site$o2)
fg_site$da.sc<-scale(fg_site$da)
fg_site$current.sc<-scale(fg_site$current)

fg_site$temp_min.test<- (fg_site$temp_min-mean(fg_site$temp_min))/sd(fg_site$temp_min)

(fg_site$temp_min.sc == fg_site$temp_min.test) #ok 


temp_min_mean<- mean(fg_site$temp_min)
temp_min_sd<- sd(fg_site$temp_min)

o2_mean<- mean(fg_site$o2)
o2_sd<-sd(fg_site$o2)

da_mean<- mean(fg_site$da)
da_sd<-sd(fg_site$da)


current_mean<- mean(fg_site$current)
current_sd<-sd(fg_site$current)

#model time!
fg_site$abundance.sc<- round(sqrt((fg_site$abundance*100)+1))

#model
m3 <- glmer(abundance.sc ~ 
              temp_min.sc +
              o2.sc+
              da.sc+
              current.sc+
              (1 | group) + 
              (0 + temp_min.sc | group) + 
              (0 + o2.sc | group) + 
              (0 + da.sc | group) + 
              (0 + current.sc | group) ,
            data = fg_site,  family = poisson(link = "log"))

#ok now use dharma 
qqnorm(resid(m3))


#test for dispersion 
testDispersion(m3) #not dispersed? 

simulationOutput <- simulateResiduals(fittedModel = m3, plot = F)

residuals(simulationOutput)

plot(simulationOutput)

qqnorm(residuals(m3))


#ok now plot the coefficient plots 1. FIXED EFFECTS


#extract confidence intervals to make a plot
gr_fe_sum<-data.frame(summary(m3)$coefficients)

gr_fe_sum<-gr_fe_sum[-1,]


confint<- data.frame(confint(m3, method='Wald', level = 0.5))
confint2<- data.frame(confint(m3, method='Wald', level = 0.95))
confint<-confint[c(7:10),]
confint2<-confint2[c(7:10),]

gr_fe_sum<-cbind(gr_fe_sum, confint)
gr_fe_sum<-cbind(gr_fe_sum, confint2)

names(gr_fe_sum)

#Now make a boxplot
test <- gr_fe_sum
test$names <- row.names(test)

names(test) <- c("Estimate","Std..Error" ,"z.value" ,   "Pr" ,  "X25"  ,    "X75" ,     "X2.5"   ,  "X97.5" ,   "names" )


coef_lineplot<-ggplot()+
  geom_linerange(data=test, 
                 stat = "identity", 
                 aes(y = names,
                     xmin =  X2.5, 
                     xmax = X97.5,
                     group = names), color='darkblue', size=1 ) +
  scale_x_continuous( limits= c(-1, 1))


#add axis labels with line break 
addline_format <- function(x,...){
  gsub('\\s','\n',x) 
}



coef_lineplot<- coef_lineplot+geom_point(data=test, aes(y=names, x=Estimate), size=2, color='darkblue')

coef_lineplot<- coef_lineplot+geom_vline(xintercept=0, linetype="dashed", color = "lightsalmon2")+theme_bw()+
  labs(y='Coefficient', y='Value')+
  scale_y_discrete(labels = addline_format(c('Current','Diffuse Attenuation', 'Dissolved Oxygen', 'Minimum Temperature')))

coef_lineplot+theme(panel.background = element_rect(fill = "lightgray"))

summary(sp_mod)

#ok now make ranef plot 

#remove the intercept plot
ranef_gr<- data.frame(ranef(m3, condvar=TRUE))

ranef_gr$term<-as.character(ranef_gr$term)


torm<- which(ranef_gr$term=='(Intercept)')

ranef_gr<-ranef_gr[-c(torm),]

ranef_gr$term[ranef_gr$term=='temp_min.sc']<-'Minimum Temperature'
ranef_gr$term[ranef_gr$term=='current.sc']<-'Current Velocity'
ranef_gr$term[ranef_gr$term=='o2.sc']<-'Oxygen Concentration'
ranef_gr$term[ranef_gr$term=='da.sc']<-'Diffuse Attenuation'

library(RColorBrewer)

levels(ranef_gr$grp)
ranef_gr$grp<-factor(ranef_gr$grp, levels=c(1:12))

ggplot(ranef_gr, aes(x=condval, y=grp, col=grp))+
  geom_point(size=4)+
  scale_color_manual(values=c("#A6CEE3" ,"#1F78B4" ,"#B2DF8A", "#33A02C" ,"#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00" ,"#CAB2D6", "#6A3D9A" ,"darkgoldenrod1", "#B15928"))+
  facet_wrap(~term)+
  geom_linerange(aes(xmin=condval-condsd, xmax=condval+condsd), color='darkblue', size=1)+
  geom_vline(xintercept=0, linetype="dashed", color = "lightsalmon2")+theme_light(base_size = 18)+theme(strip.text = element_text(size=25))+
  labs(y='Functional Group', x='Conditional Variance')

brewer.pal(12, 'Paired')



#ok now use this to predict 

#set up prediction dataframes for each environmental variable 

temp_min<-as.data.frame(current_preds[[1]], row.names=NULL, optional=FALSE, xy=TRUE, 
                        na.rm=TRUE, long=FALSE)

o2<- as.data.frame(current_preds[[2]], row.names=NULL, optional=FALSE, xy=TRUE, 
                   na.rm=TRUE, long=FALSE)

da<- as.data.frame(current_preds[[3]], row.names=NULL, optional=FALSE, xy=TRUE, 
                   na.rm=TRUE, long=FALSE)

current<- as.data.frame(current_preds[[4]], row.names=NULL, optional=FALSE, xy=TRUE, 
                        na.rm=TRUE, long=FALSE)


#create prediction data frame 
predict_df<-data.frame(temp_min[,c(1:3)], o2$o2, da$da, current$current)

#standardise predictor variables (WITH THE DATA MEAN!) ----


predict_df$temp_min.sc<- (predict_df$temp_min- temp_min_mean)/temp_min_sd
predict_df$o2.sc<-(predict_df$o2- o2_mean)/o2_sd
predict_df$da.sc<-(predict_df$da- da_mean)/da_sd
predict_df$current.sc<- (predict_df$current - current_mean)/current_sd

predict_df$group<-1

library(mefa)

length(predict_df$x)

predict_df<- rep(predict_df, times=12)

predict_df$group<-rep(c(1:12), each=1028)


#ok now we can predict with this?
summary(m3)

#predict for all groups
prediction<-predict(m3, newdata=predict_df, type='response')


lme4::predict()

#extract group 1 as a test 
length(which(predict_df$group==1))

group1<- prediction[c(1:1028)]

#untransform for final values 
group1_trans<- ((group1^2)-1)/100 

#get lat lon values to make back into raster 
lat_lon<- predict_df[c(1:1028),c(1,2)]

group1_df<-data.frame(lon=lat_lon$x, lat=lat_lon$y, value=group1_trans)

#now make into a raster
group1_pred<- rasterize(lat_lon, current_preds[[1]], field=group1_df$value )

par(mfrow=c(1,1))
plot(group1_pred)

#ok now repeat for other groups 
1028*2
4901+2450
7351+2450
9800+2450
12250+2450
14700+2450
17150+2450
19601+2450
22051+2450
24501+2450
26950+2450

1028*5


group2<- prediction[c(1029:2056)]
group3<- prediction[c(2057:3084)]
group4<-prediction[c(3085:4112)]
group5<-prediction[c(4113:5140)]
group6<-prediction[c(5141:6168)]
group7<-prediction[c(6169:7196)]
group8<-prediction[c(7197:8224)]
group9<-prediction[c(8225:9252)]
group10<-prediction[c(9253:10280)]
group11<-prediction[c(10281:11308)]
group12<-prediction[c(11309:12336)]

groups<-data.frame(group1=group1, group2=group2, group3=group3, group4=group4, group5=group5, group6=group6, group7=group7, group8=group8, group9=group9, 
                   group10=group10, group11=group11, group12=group12)

for (i in 1:ncol(groups)){
  groupx<- groups[,i]
  
  trans<- ((groupx^2)-1)/100
  
  df<-data.frame(lon=lat_lon$x, lat=lat_lon$y, value=trans)
  
  #now make into a raster
  pred<- rasterize(lat_lon, current_preds[[1]], field=df$value )
  
  assign(paste0('raster_pred_gr_', LETTERS[i]), pred)
  
}


plot(raster_pred_gr_C)

high<-which(raster_pred_gr_A[] > 1000)

raster_pred_gr_A[high]<- NA

plot(raster_pred_gr_A)

raster_preds<- lapply(ls(pattern='raster_pred_gr_'), get)

par(mfrow=c(3,3))

pred_stack<-stack(raster_preds)

plot(pred_stack)

par(mfrow=c(1,1))

plot(raster_preds[[12]])


par(mfrow=c(1,1))

plot(raster_preds[[1]])


#cut the deep areas
#test with bio oracle bathy 
bathy<- load_layers('BO_bathymin')
bathy<-crop(bathy, extent(raster_preds[[1]]))

plot(bathy)
bathy_shallow<-bathy

keep<- which(bathy_shallow[] < -500)

bathy_shallow[keep]<-NA

plot(bathy_shallow)

writeRaster(bathy_shallow, 'bioOrBathy_500.tif')

bathy_mask<-bathy_shallow

bathy_mask[is.na(bathy_mask[])]<-0

bathy_mask[! bathy_mask[]==0]<-1

plot(bathy_mask)   #ok now use these to crop the rasters 

test_3<-raster_preds[[1]]

test_3<- test_3*bathy_mask

plot(test_3)

test_3[test_3[]==0]<-NA

plot(test_3)

plot(raster_preds[[1]])

i=1


layer_check<- data.frame(list_layers(marine=TRUE))
#ok now loop for all groups 
for (i in 1: length(raster_preds)){
  
  crop<-raster_preds[[i]]
  
  masked<- crop*bathy_mask
  
  masked[masked[]==0]<-NA
  
  assign(paste0('raster_pred_gr_shal_', LETTERS[i]), masked)
  
}

#list together 
raster_pred_shal<- lapply(ls(pattern='raster_pred_gr_shal_'), get)


par(mfrow=c(4,3))

for (i in 1:length(raster_pred_shal)){
  plot(raster_pred_shal[[i]])
}

writeRaster(raster_pred_shal[[1]],'test_gr1.tif')

par(mfrow=c(1,1))
plot(raster_pred_shal[[8]])
plot(japan_outline, add=TRUE)
points(latlon$lon, latlon$lat, col='red')

izu_crop<-readOGR('plotting/buffer_izu_gone_proj.shp')
crs(izu_crop)

raster_pred_stack_shal<- stack(raster_pred_shal)

raster_pred_stack_shal<-mask(raster_pred_stack_shal, izu_crop)
plot(raster_pred_stack_shal)

par(mar=c(1,1,1,1))
par(mfrow=c(4,3))

for (i in 1:length(raster_pred_stack_shal)){
  plot(raster_pred_stack_shal[[i]])
}

i=1

#write rasters
for(i in 1:length(raster_pred_stack_shal)){
  towrite<-raster_pred_stack_shal[[i]]
  writeRaster(towrite, paste0('raster_pred_shal',i,'.tif'))
}


setwd("D:/corona_contingency/SDM_course_git")


par(mar=c(2,2,2,2))
par(mfrow=c(1,1))
plot(raster_pred_stack_shal[[2]])

plot(raster_pred_stack_shal[[8]])
torm<- which(raster_pred_stack_shal[[8]][]>500)
raster_pred_stack_shal[[8]][torm]<- NA


test<-raster_pred_shal[[10]]
length(which(test[]>10)) #3 pixcels
test[test[]>10]<-NA
plot(test)

hist(test[])

#8 seems to be overfitting still.... 
par(mfrow=c(1,1))
plot(raster_pred_shal[[12]])
plot(japan_outline, add=TRUE)

test<-raster_pred_shal[[8]]

length(test[test[] >500]) #only 13 pixels

test[test[] >500] <- NA

plot(test)

length( (test[test[]>0.1]))

par(mfrow=c(1,1))
plot(raster_pred_shal[[3]])
plot(japan_outline, add=TRUE)

test<-raster_pred_shal[[8]]

length(test[test[] >500]) #only 13 pixels

test[test[] >500] <- NA

plot(test)

length( (test[test[]>0.1]))



#cross validate the models 
#1) extract values at lat lons

lonlat<-data.frame(lon=latlon$lon, lat=latlon$lat)

extract_all_layers<- data.frame( raster::extract(pred_stack, lonlat))

extract_all_layers$lon<-lonlat$lon
extract_all_layers$lat<-lonlat$lat

i=1

colnames(extract_all_layers)<- c(1,2,3,4,5,6,7,8,9,10, 11,12, 'lon','lat')

model_fits<- data.frame(group=1, RMSE=1,NRMSE=1 ,pearsons=1)

rmse <- function(o,p) {
  e <- o - p
  sqrt(mean(e^2,na.rm=T))
}

i=1


#now run this to get the RMSE and pearsons 

for (i in 1:length(unique(fgroup_site$group))){
  
  tryCatch( {
    group_data<- fgroup_site[fgroup_site$group==i,]
    group_data<-group_data[,c(3,4)]
    
    names(group_data)<-c('observed','lat')
    
    col<-which(colnames(extract_all_layers)==i)
    
    model_data<- extract_all_layers[,c(col, 13,14)]
    
    names(model_data)<-c('modelled', 'lon','lat')
    
    df<- left_join(model_data, group_data, by='lat')
    
    assign(paste0('obs_exp_gr',i), df)
    
    
    
    model_fits[i,1]<-i
    model_fits[i,2]<-rmse(df$observed, df$modelled )
    model_fits[i,3]<-(rmse(df$observed, df$modelled ))/(max(df$observed, na.rm = TRUE)-(min(df$observed, na.rm=TRUE)))
    model_fits[i,4]<-cor(df$observed, df$modelled, method = c("pearson"), use="complete.obs")
  } , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


function(x){
  
}


i=1

#OK NOW CROSS VALIDATE WITH INDEPENDENT DATA

  
  model_fits2<- data.frame(group=1, RMSE=1,NRMSE=1 ,pearsons=1)
  
  RMSE<- data.frame(gr1=1, gr2=2, gr3=3,gr4=4,gr5=5,gr6=6,gr7=7,gr8=8,gr9=9,gr10=10, gr11=11, gr12=12)
  NRMSE<- data.frame(gr1=1, gr2=2, gr3=3,gr4=4,gr5=5,gr6=6,gr7=7,gr8=8,gr9=9,gr10=10, gr11=11, gr12=12)
  pearsons<-data.frame(gr1=1, gr2=2, gr3=3,gr4=4,gr5=5,gr6=6,gr7=7,gr8=8,gr9=9,gr10=10, gr11=11, gr12=12)
  
  data<-fg_site
  
  i=2
  j=1
  for (i in 1:1000) {
    
    tryCatch ( {
      
      sites<-unique(as.character(fgroup_site$Site))
      test_sites<- sample(sites, size=3)
      
      test<- data[data$SiteID %in% test_sites,]  
      train<- data[! data$SiteID %in% test_sites, ]
      
      obvs<-test$abundance
      
      #make models
      
      #GLM
      #model
      m_fit <- glmer(abundance.sc ~ 
                    temp_min.sc +
                    o2.sc+
                    da.sc+
                    current.sc+
                    (1 | group) + 
                    (0 + temp_min.sc | group) + 
                    (0 + o2.sc | group) + 
                    (0 + da.sc | group) + 
                    (0 + current.sc | group) ,
                  data = train,  family = poisson(link = "log"))
      
      #now predict for the whole area with the training model
      
      temp_min<-as.data.frame(current_preds[[1]], row.names=NULL, optional=FALSE, xy=TRUE, 
                              na.rm=TRUE, long=FALSE)
      
      o2<- as.data.frame(current_preds[[2]], row.names=NULL, optional=FALSE, xy=TRUE, 
                         na.rm=TRUE, long=FALSE)
      
      da<- as.data.frame(current_preds[[3]], row.names=NULL, optional=FALSE, xy=TRUE, 
                         na.rm=TRUE, long=FALSE)
      
      current<- as.data.frame(current_preds[[4]], row.names=NULL, optional=FALSE, xy=TRUE, 
                              na.rm=TRUE, long=FALSE)
      
      
      #create prediction data frame 
      predict_df1<-data.frame(temp_min[,c(1:3)], o2$o2, da$da, current$current)
      
      
      #standardise predictor variables (WITH THE DATA MEAN!) ----
      
      
      predict_df1$temp_min.sc<- (predict_df1$temp_min- temp_min_mean)/temp_min_sd
      predict_df1$o2.sc<-(predict_df1$o2- o2_mean)/o2_sd
      predict_df1$da.sc<-(predict_df1$da- da_mean)/da_sd
      predict_df1$current.sc<- (predict_df1$current - current_mean)/current_sd
      
      
      predict_df1$group<-1
      
      length(predict_df1$x)
      
      predict_df1<- rep(predict_df1, times=12)
      
      predict_df1$group<-rep(c(1:12), each=1028)
      
      #predict for all groups
      prediction1<-predict(m_fit, newdata=predict_df1, type='response')
      
      prediction1<-((prediction1^2)-1)/100
      
      length(which(predict_df1$group==1))
      
      group1_fit<- prediction1[c(1:1028)]
      group2_fit<- prediction1[c(1029:2056)]
      group3_fit<- prediction1[c(2057:3084)]
      group4_fit<-prediction1[c(3085:4112)]
      group5_fit<-prediction1[c(4113:5140)]
      group6_fit<-prediction1[c(5141:6168)]
      group7_fit<-prediction1[c(6169:7196)]
      group8_fit<-prediction1[c(7197:8224)]
      group9_fit<-prediction1[c(8225:9252)]
      group10_fit<-prediction1[c(9253:10280)]
      group11_fit<-prediction1[c(10281:11308)]
      group12_fit<-prediction1[c(11309:12336)]
      
      
      groups_fits<-data.frame(group1=group1_fit, group2=group2_fit, group3=group3_fit, group4=group4_fit, group5=group5_fit, group6=group6_fit, group7=group7_fit,
                              group8=group8_fit, group9=group9_fit, 
                         group10=group10_fit, group11=group11_fit, group12=group12_fit)
      
      for (j in 1:ncol(groups_fits)){
        groupx<- groups_fits[,j]
        
        trans<- groupx
        
        df<-data.frame(lon=lat_lon$x, lat=lat_lon$y, value=trans)
        
        pred<- rasterize(lat_lon, current_preds[[1]], field=df$value )
        
        lat_lon_test<-data.frame(x=unique(test$lon), y=unique(test$lat))
        
        pred_val<- raster::extract(pred, lat_lon_test)
        
        obs_val<-test[test$group == j,]
    
        obs_val<-obs_val$abundance
        
        pred_val
        obs_val
      
        rmse_vals<-rmse(pred_val, obs_val) 
        nrmse_vals<- rmse_vals/(max(obs_val)-min(obs_val))
        pearsons_val<- cor(pred_val, obs_val)
    
        RMSE[i, j]<-rmse_vals
        NRMSE[i,j]<-nrmse_vals
        pearsons[i,j]<- pearsons_val
        
        }
      
    })
  print(i)
 }

  
  
  allrmse<-lapply(extract_all, models_all)
  
  length(which(NRMSE$gr2>20))

NRMSE_clean<- do.call(data.frame,lapply(NRMSE, function(x) replace(x, is.infinite(x),NA)))
RMSE_clean<- do.call(data.frame,lapply(RMSE, function(x) replace(x, is.infinite(x),NA)))  
pearsons_clean<- do.call(data.frame,lapply(pearsons, function(x) replace(x, is.infinite(x),NA)))


which(NRMSE_clean$gr2>20)


#set up new df for the means
model_means<- data.frame(FG=1, RMSE=1, NRMSE=2, Pearsons=3)

i=1

for (i in 1:ncol(NRMSE)){
  means_NRMSE<- mean(NRMSE_clean[,i], na.rm=TRUE)
  means_RMSE<- mean(RMSE_clean[,i], na.rm=TRUE)
  means_pearsons<- mean(pearsons_clean[,i], na.rm=TRUE)
  model_means[i,1]<-i
  model_means[i,2]<-means_RMSE
  model_means[i,3]<-means_NRMSE
  model_means[i,4]<-means_pearsons
}

hist(NRMSE$gr8)

 

write.csv(model_means, 'mixed_ef_cross_val_fits.csv')
#extract at 

write.csv(model_fits, 'mixed_ef_pred_model_fits.csv')


fg_site$group<-factor(fg_site$group, levels = c(1,2,3,4,5,6,7,8,9,10,11,12))

#fig_site_fig
fg_site_long<- fg_site[,c(2,4,5)]
fg_site_long$abundance<-ceiling(fg_site_long$abundance)

new_df<- data.frame(lat=1, group=1, abundance=1)

i=1

for (i in 1:nrow(fg_site_long)){
  n.times<-fg_site_long[i,3]
  n.times<-as.numeric(n.times)
  new<- rep(fg_site_long[i,], each=n.times)
  new_df<-rbind(new_df, new)
}

new_df<-new_df[-1,]

new_df$group<-factor(new_df$group,levels = c(1,2,3,4,5,6,7,8,9,10,11,12 ))



#ok CAN BOXPLOT THIS, OOPS IT ALL DELETED

library(ggpubr)
library('gghalves')

library(RColorBrewer)

library(ggridges)


ggplot(new_df, aes(y=lat, x=group, fill=group))+
  geom_violin(width=6, alpha=0.8 )+
  scale_fill_brewer(palette='Paired')+
  theme_bw()

ggplot(fg_site, aes(x=lat, y=abundance, col=group))+
  geom_point(col='grey57')+       
  geom_smooth(method='lm', fill='grey42', size=1.2)+
  scale_color_brewer(palette='Paired')+
 facet_wrap(~group, scales='free')+
  theme(strip.text = element_text( face = "bold", size=10))+
  labs(x='Latitude', y='Functional Group Abundance')

theme_set(theme_ridges(center_axis_labels = TRUE))

ggplot(new_df, aes(x=lat, y=group, group=group, fill=group))+
  geom_density_ridges(scale=5, alpha=0.7)+
  scale_fill_brewer(palette='Paired')+
  xlim(24.3, 35)+
  labs(x='Latitude', y='Functional Group')+
  theme(legend.position = "none")

ggplot(new_df, aes(x=lat, y=group, group=group, fill=group))+
  geom_density_ridges(scale=3, alpha=0.7)+
  scale_fill_brewer(palette='Paired')+
  xlim(24.3, 35)+
  labs(x='Latitude', y='Functional Group')+
  theme(legend.position = "none", axis.text = element_text(size = 20),  axis.title = element_text(size=14, face="bold"))



min(fg_site$lat)
max(fg_site$lat)

ggviolin(new_df, y='lat', x='group', fill='group')




#ok now  check traits! trait plots----
#read in traits 

traits<-read.csv('fishtraitgroups_2301.csv')
to_keep<-which(fgroup$Species %in% traits$Species)

traits_all<-traits[to_keep,]

#keep the traits we want 

#remove group 5 and 12 (only 1 species in each and replace with group 13 and 14)
traits_all<-traits_all[-c(which(traits_all$group==5)),]
traits_all$group[traits_all$group==13]<-5

traits_all<-traits_all[-c(which(traits_all$group==12)),]
traits_all$group[traits_all$group==14]<-12

traits<-traits_all

traits_group_2022<-traits[,c(1,15)]

write.csv(traits_group_2022, 'fish_trait_groups_jan2022.csv')
#now plot the groups to see

for(i in 1:length(unique(traits_all$group))){
 toprint<- length(which(traits$group==i))
 print(toprint)
}



par(mfrow=c(1,1))
### trophic groups
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
food = table(traits$Diet, traits$group)
barplot(food, main = "Diet", col=terrain.colors(length(rownames(food))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(food))), legend = rownames(food))

### Aggregation
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
agg = table(traits$Aggregation, traits$group)
barplot(agg, main = "Aggregation", col=terrain.colors(length(rownames(agg))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(agg))), legend = rownames(agg))

### Habitat Association - Position
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
pos = table(traits$Position, traits$group)
barplot(pos, main = "Habitat Association", col=terrain.colors(length(rownames(pos))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = terrain.colors(length(rownames(pos))), legend = rownames(pos))

### Parental Mode
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
parent = table(traits$ParentalMode, traits$group)
barplot(parent, main = "Parental Mode", col=rainbow(length(rownames(parent))), width = 2)
legend ("topright", inset = c(-0.25, 0), fill = rainbow(length(rownames(parent))), legend = rownames(parent))



## First, do numerical traits with boxplots:
er = par(mfrow = c(2,2), pty = "s", mai=c(0.5,0.5,0.5,0.5))
boxplot(traits$DepthRange ~ traits$group, main = "DRange")
boxplot(traits$BodySize ~ traits$group, main = "MaxLength")
boxplot(traits$PLD ~ traits$group, main = "PLD")


#ok now functional trait space across whole area 

#first make a sp by traits matrix for all
trait_sp<- traits[,c(1,3,4,5,6,7,8,9)]

trait_sp<- trait_sp[order(trait_sp$Species),]

rownames(trait_sp)<-trait_sp[,1]

trait_sp<-trait_sp[,-1] #matrix of functional traits 



#now need abundance of species  #site_matrix_df

library(vegan)
library(FD)

gower_distance<-gowdis(trait_sp) #try making the distance matrix first

#make them match
site_matrix_names<-colnames(site_matrix_df)

site_matrix_names<-sub("[.]", " ", site_matrix_names)

torm<-which(! site_matrix_names %in% rownames(trait_sp))

site_matrix_traitsp<- site_matrix_df

colnames(site_matrix_traitsp)<- site_matrix_names

site_matrix_traitsp<-site_matrix_traitsp[,-c(torm)]

identical(rownames(trait_sp), colnames(site_matrix_traitsp))

length(which(rownames(trait_sp) %in% colnames(site_matrix_traitsp)))

#ok can now get the dbfd      
database<-dbFD(gower_distance, site_matrix_traitsp, corr="cailliez") #WORK







