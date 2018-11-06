install.packages('corrplot')
install.packages('usdm')
#SDM Practise##

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

#read in Japan if starting ###### START HERE 
japan_outline<-readOGR('plotting/japan_outline.shp')

#check
plot(japan_outline)


#get environmental data----

#explore the data 
list_layers()

layers<-list_layers()

marine_layers<-filter(layers, marine=='TRUE')

#list layers from bio-oracle
layers.bio2 <- list_layers( datasets="Bio-ORACLE" ) 

list_datasets()

#example extracting data----

#extract bathymetry data
bathy <- load_layers(c( "BO_bathymean")) 

#create japan extent
#japan_extent<-extent(120.9605,150.057849,23.6978,43.878871 )

japan_extent<-extent(123, 140, 24, 35)

#crop to extent
bathy_japan <- crop(bathy, japan_extent) 

projection(bathy_japan)

#plot
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(bathy_japan,col=my.colors(1000),axes=FALSE, box=FALSE) 
title(cex.sub = 1.25, sub = "Depth (m)") 

plot(japan_outline, add=TRUE)

#make mask for SDM (values areas less then 100m deep ) 
bathy_shallow<-bathy_japan

bathy_shallow[bathy_shallow < -100]<-NA

plot(bathy_shallow, col=my.colors(1000))


#compare with the marspec bathy
bathy2<-load_layers(c('MS_bathy_5m'))

bathy2_jpn<-crop(bathy2, japan_extent)

my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(bathy2_jpn,col=my.colors(1000),axes=FALSE, box=FALSE) 
title(cex.sub = 1.25, sub = "Depth (m)") 
plot(japan_outline, add=TRUE)

bathy_shallow2<-bathy2_jpn

bathy_shallow2[bathy_shallow2 < -100]<-NA

plot(bathy_shallow2, col=my.colors(1000))   #bathy_shallow2 = better 


#32 sites so maximum six environmental variables?


#extract temp data (SST min)
sst_min <- load_layers("BO_sstmin") 

temp_japan<-crop(sst_min, japan_extent)

plot(temp_japan, col=my.colors(1000), axes=FALSE, box=FALSE)
title(cex.sub=1.25, sub='Maximum temperature at the sea bottom (ºC)')

#extract primary production
#pp<-load_layers('BO2_ppmax_bdmax')

#pp_japan<-crop(pp, japan_extent)

#my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
#plot(pp_japan,col=my.colors(1000),axes=FALSE, box=FALSE) 
#title(cex.sub = 1.25, sub = "Primary Production") 

#extract chlorophyll conc (mean at min depth) 
chloro<-load_layers('BO2_chlomean_bdmin')

chloro_japan<-crop(chloro, japan_extent)

plot(chloro_japan, col=my.colors(1000))

#extract bottom light (mean par)   #maybe not for fish?
light<-load_layers('BO_parmean')
light_japan<-crop(light, japan_extent)

my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(light_japan,col=my.colors(1000),axes=FALSE, box=FALSE) 
title(cex.sub = 1.25, sub = "Mean PAR") 

#extract current velocity (mean at min depth)
current<-load_layers('BO2_curvelmean_bdmin')

current_japan<-crop(current, japan_extent)

plot(current_japan, col=my.colors(1000))


#extract 02 conc, mean at min depth
ox<-load_layers('BO2_dissoxmean_bdmin')

ox_japan<-crop(ox, japan_extent)

plot(ox_japan, col=my.colors(1000))


##extract pH
#ph<-load_layers('BO_ph')
#ph_japan<-crop(ph, japan_extent)

#my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
#plot(ph_japan,col=my.colors(1000),axes=FALSE, box=FALSE) 
#title(cex.sub = 1.25, sub = "pH") 

###load functional group data and prepare----
#read in fish data
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

#add lat lon 
latlon<-read.csv('JP2015_16_waypoints.csv')

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

#plot points 
#get japan map
japan_map<-map('japan',col= 'black',  interior = FALSE) #can probs make better map
map(japan_map)
points(fgroup_site$lon, fgroup_site$lat, pch=20, col='red')


#make dataframes for each functional group----
#split the data by functional group so have observation of each functional group and lat/long

which(is.na(fgroup_site$group)) #223
fgroup_site<-fgroup_site[-223,]

for (i in 1:length(unique(fgroup_site$group))){
  rows<-which(fgroup_site$group==i)
  assign(paste0("Obvs_gr_", i), data.frame(fgroup_site[rows,]))
}

#SDM----

#create an output file for model outputs
dir.create(path = "output")

#rename observation data
obs.data<-fgroup_site

# Determine geographic extent of our data
max.lat <- ceiling(max(obs.data$lat))
min.lat <- floor(min(obs.data$lat))
max.lon <- ceiling(max(obs.data$lon))
min.lon <- floor(min(obs.data$lon))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

#plot data to check (already done but good to do it here)
# Load the data to use for our base map


# Plot the base map
plot(japan_outline, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     col = "grey95",
     axes = TRUE
     )

# Add the points for individual observation
points(x = obs.data$lon, 
       y = obs.data$lat, 
       col = "red", 
       pch = 20, 
       cex = 0.75)
# And draw a little box around the graph
box()


#here you could crop the occurance data but i've done that

#sort out the environmental data
#stack all the predictors into one rasterstack
predictors<-stack(light_japan, chloro_japan, temp_japan, ox_japan, current_japan)


#using the VIF? Doesn't seem to do anything? ---- 
v1<-vifstep(predictors, th=10)

v1@results    

rastervif<-exclude(predictors, v1)
plot(rastervif)

plot(temp_japan)

#check all the names
names(predictors)

#plot all the predictors 
plot(predictors)


#example plot again (dont need this- pretty much the same)
# first layer of the RasterStack
plot(predictors, 1)

# note the "add=TRUE" argument with plot

plot(japan_outline, add=TRUE)
# with the points function, "add" is implicit
points(obs.data$lon, obs.data$lat, col='blue')



####need to exclude land and deep areas for the location of fish #make a mask ----

# we use the first file to create a RasterLayer

mask <- bathy_shallow2

# select 500 random points
# set seed to assure that the examples will always
# have the same random sample.
#set.seed(1963)
#bg <- randomPoints(mask, 500 )

#And inspect the results by plotting

# set up the plotting area for two maps
#par(mfrow=c(1,2))
#plot(!is.na(mask), legend=FALSE)
#points(bg, cex=0.5)

# now we repeat the sampling, but limit
# the area of sampling using a spatial extent
#bg2 <- randomPoints(mask, 50, ext=geographic.extent)
#plot(!is.na(mask), legend=FALSE)
#plot(geographic.extent, add=TRUE, col='red')
#points(bg2, cex=0.5)




### now extract the values of the predictors at the location of the points

#use group 1
group1<-Obvs_gr_1[,c(4,5)]

#points need to be x-y so long lat
group1<-data.frame(group1$lon, group1$lat)

presvals <- extract(predictors, group1)

presvals<-data.frame(presvals)

#add the abundances
presvals$abundance<-Obvs_gr_1$abundance

#call this sdm data
sdmdata<-presvals

# setting random seed to always create the same  #for prescence absence, not what we have?
# random set of points for this example
#set.seed(0)
#backgr <- randomPoints(predictors, 500)
#absvals <- extract(predictors, backgr)
#pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
#sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))

#check for co-linearity of data
pairs(sdmdata[,1:5], cex=0.1)  #co-linearity, learn how to deal with this?

corr<-sdmdata[,1:5]
m<-cor(corr)

corrplot(m, method='number')

#don't use chlorophyll and dissolved oxygen in model as very correlated with SST (-0.92, -0.96)

# OR PCA on the predictor variables
#OR calculate VIF 




#mask out the deep areas
#mask the areas deeper then 100
predictors<-mask(predictors, bathy_shallow2)

plot(predictors)


#save the sdm data and presvals to disk 
#saveRDS(sdmdata, "sdm.Rds")
#saveRDS(presvals, "pvals.Rds")

#now create model ----
#GLM 

names(sdmdata)

#check ditribution of abundance
ggplot(sdmdata, aes(x=abundance))+
  geom_histogram()

m1 <- glm(abundance ~ BO_parmean + BO_sstmin + BO2_curvelmean_bdmin ,family=poisson , data=sdmdata)  ##need to reconsider as mean of transects= non integer 

class(m1)

summary(m1)

#testing the model 
1-pchisq(m1$deviance, m1$df.residual) #0 =bad
plot(m1, which=c(1,2,4))


#plot predictors with model 
p<-predict(predictors, m1)

names(predictors)


par(mfrow=c(1,1))
plot(p, col=my.colors(1000))

#where abundance is more then 0 
p1<-p

p1[p1<4]<-NA

plot(p1, col=my.colors(1000))
plot(japan_outline, add=TRUE)

#plot(japan_outline, add=TRUE, border='lightgrey')


##for group 3, automate soon 
#use group 3               #lon , lat order
group3<-Obvs_gr_3[,c(5,4)]

predictors<-stack(light_japan, temp_japan, current_japan)


presvals <- extract(predictors, group3) #masked so doesnt work? #####PROBLEM MASK MASKS OUT SOME OF THE DATAPOINTS?? #problem:
#sites such as yukushima drop off to >100m so fast (within the pixel) that the mask cuts them out 

plot(bathy_shallow2)
points(x=group3$lon, y=group3$lat, col='red', pch=20, cex=0.75)

points(x = obs.data$lon, 
       y = obs.data$lat, 
       col = "red", 
       pch = 20, 
       cex = 0.75)

presvals<-data.frame(presvals)

#add the abundances
presvals$abundance<-Obvs_gr_3$abundance

#call this sdm data
sdmdata<-presvals

# setting random seed to always create the same  #for prescence absence, not what we have?
# random set of points for this example
#set.seed(0)
#backgr <- randomPoints(predictors, 500)
#absvals <- extract(predictors, backgr)
#pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
#sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))

#check for co-linearity of data
pairs(sdmdata[,1:3], cex=0.1)  #co-linearity, learn how to deal with this?

#mask 
predictors<-mask(predictors, bathy_shallow2)

#save the sdm data and presvals to disk 
#saveRDS(sdmdata, "sdm.Rds")
#saveRDS(presvals, "pvals.Rds")

#now create model ----
#GLM 

names(sdmdata)

m1 <- glm(abundance ~ BO_parmean + BO2_curvelmean_bdmin + BO_sstmin ,family=poisson , data=sdmdata)

class(m1)

summary(m1)

#testing the model 
1-pchisq(m1$deviance, m1$df.residual) #0 =bad
plot(m1, which=c(1,2,4))


#plot predictors with model 
p<-predict(predictors, m1)

names(predictors)


par(mfrow=c(1,1))
plot(p, col=my.colors(1000))

#where abundance is more then 0 
p1<-p

p1[p1<8]<-NA

plot(p1, col=my.colors(1000))
plot(japan_outline, add=TRUE)



map.world <- map_data("world")

#model evaluation ----



#plot the distributions of func group at each site -poisson
ggplot(fgroup_site, aes(x=abundance))+
  geom_histogram()+
  facet_wrap(~group)

