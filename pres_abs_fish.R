###load in fish workspace jan####


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

install.packages('randomForest')

View(extract_all[[1]])

extract_pres<- extract_all

i=1
for (i in 1:length(extract_pres)){
  x<- which (extract_pres[[i]]$abundance>0)
    extract_pres[[i]]$abundance[x]<-1
  }

View(extract_pres[[3]])
#ok now we have pres/abs data
#check which groups have zeros
extract_pres[[1]]$abundance #yes

for(i in 1:length(extract_pres)){
  print(i)
  print(extract_pres[[i]]$abundance)
}

#which groups don't have zeros
#3,4,5

#testing glm step 
glmtest2<-glm(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , data=extract_pres[[2]], family=binomial(link='logit') )

summary(glmtest2)

glmtest_step<- step(glmtest2, trace = 0, na.action=na.omit)
summary(glmtest_step) #ok 

#testing standardise coeffs




#testing gam step

#remove lat lon col
abundance<-extract_pres[[2]]$abundance
abundance<-as.data.frame(abundance)
abundance

gam1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss,k=5), family=binomial(link='logit'), data=extract_pres[[2]])
summary(gam1)

#make df for loop to fill 
gam_step_table2<-data.frame(summary(gam1)$s.table)
out_varib<-row.names(gam_step_table2[gam_step_table2$p.value>=0.1,])

#set up formula to change 

form<-formula(paste(abundance, "~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss, k=5)", sep=""))

for(g in out_varib)
{
  g_temp<-paste(unlist(strsplit(g, "\\)")),", k=5)", sep="")
  
  if(g_temp=="s(BO_sstmin, k=5)"){form_g1<-update(form, ~. -s(BO_sstmin, k=5, k=5))}
  if(g_temp=="s(BO2_curvelmax_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_curvelmax_ss, k=5)) }
  if(g_temp=="s(BO2_salinitymean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_salinitymean_ss, k=5))}
  if(g_temp=="s(BO2_chlomean_ss, k=5)"){form_g1<-update(form, ~. -s(BO2_chlomean_ss, k=5))}
  
  gam2 <-gam(form_g1, data=extract_pres[[2]],  family=binomial(link='logit'), na.action=na.omit)
  
  if(AIC(gam2)<=AIC(gam1)){form<-form_g1
  print(paste(g, " dropped", sep=""))}
}

gam_final <-gam(form, data=extract_pres[[2]],  family=binomial(link='logit'), na.action=na.omit)

summary(gam_final)
AIC(gam_final)
AIC(gam1)



#create models ########start here 
#now model in loop with 15% test 85% training (n=5)


library(lme4)
library(mgcv)
library(gamm4)
library(MASS)

models_all_pres<- function(data){
  
  rmse_pres<-data.frame(glmR=1, gamR=1, rfR=1, glmP=1, gamP=1, RFP=1) 
  
  for (i in 1:1000){
    tryCatch( {
      
      #define test and training data   ---------- problem with drop- if no variables are significant it removes them all! need to check thisss.
      test<- data[sample(nrow(data), size=6, replace=FALSE),]  
      train<- data[(! row.names(data) %in% row.names(test)), ]
      
      obvs<-test$abundance
      
      #make models
      
      #GLM
      glm1<-glm(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , family=binomial(link='logit'), data=train )
      glm1<- step(glm1, trace = 0)
      
      
      #GAM
      gam1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss,k=5), family=binomial(link='logit') , data=train)  
      
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
        
        gam2 <-gam(form_g1, data=train,  family=binomial(link='logit'), na.action=na.omit)
        
        if(AIC(gam2)<=AIC(gam1)){form<-form_g1
        print(paste(g, " dropped", sep=""))}
      }
      
      gam1 <-gam(form, data=train,  family=binomial(link='logit'), na.action=na.omit)
      
      
      #rf 
      
      rf1<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + 
                          BO2_chlomean_ss, data=train, ntree=300, importance=TRUE   )
      
      
      #predict models for test
      prglm<-predict( glm1, test, type='response')
      prgam<-predict(gam1, test, type='response')
      prRF<-predict(rf1, test, type='response')
      
      #now rmse for all  
      rmse_pres[i,1]<-rmse(obvs, prglm)
      rmse_pres[i,2]<-rmse(obvs, prgam)
      rmse_pres[i,3]<-rmse(obvs, prRF )
      
      #now pearsons correlation for all 
      rmse_pres[i,4]<-cor(obvs, prglm, method = c("pearson"))
      rmse_pres[i,5]<-cor(obvs, prgam, method = c("pearson"))
      rmse_pres[i,6]<-cor(obvs, prRF, method = c("pearson"))
      
      print(i)
      
    } , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }
  
  return(rmse_pres) 
  
  
}

allrmse_pres<-lapply(extract_pres, models_all_pres)

#get averages as it didnt work 

average_km<-function(data){
  
  glm_av<- mean(data$glmR, na.rm=TRUE)
  gam_av<-mean(data$gamR, na.rm=TRUE)
  rf_av<-mean(data$rfR, na.rm=TRUE)
  
  averages<-data.frame(glm_av, gam_av, rf_av)
  
  return(averages)
  
}

averages<-lapply(allrmse_pres, average_km)

averages.df_pres<-data.frame(glm=1, gam= 1, rf=1)

for (i in 1: length(averages)){
  averages.df_pres[i,]<-(averages[[i]])
}

old_averages_pres<-averages.df_pres

#averages pearsons 
average_p<-function(data){
  
  glm_av<- mean(data$glmP, na.rm=TRUE)
  gam_av<-mean(data$gamP, na.rm=TRUE)
  rf_av<-mean(data$RFP, na.rm=TRUE)
  
  averages<-data.frame(glm_av, gam_av, rf_av)
  
  return(averages)
  
}

average_pear_pres<-lapply(allrmse_pres, average_p)

averages_pear.df_pres<-data.frame(glm=1, gam= 1, rf=1)

for (i in 1: length(average_pear_pres)){
  averages_pear.df_pres[i,]<-(average_pear_pres[[i]])
}


#full model, predict and ensemble   #loop (make the models with coefs that are not significant excluded)
#COME BACK HERE----

for (i in 1:length(extract_pres)) {
  tryCatch( {
  
  #GLM
  glm_gr<-glm(abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + BO2_chlomean_ss , data=extract_pres[[i]],family=binomial(link='logit') )
  glm_gr<-step(glm_gr, trace = 0, na.action=na.omit)
  
  #GAM
  gam1<-gam(abundance ~ s(BO_sstmin, k=5) + s(BO2_curvelmax_ss, k=5) + s(BO2_salinitymean_ss, k=5) + s(BO2_chlomean_ss,k=5), family=binomial(link='logit') , data=extract_pres[[i]])  
  #extract abundance 
  abundance<-extract_pres[[i]]$abundance
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
    
    gam2 <-gam(form_g1, data=extract_pres[[i]],  family=binomial(link='logit'), na.action=na.omit)
    
    if(AIC(gam2)<=AIC(gam1)){form<-form_g1
    print(paste(g, " dropped", sep=""))}
  }
  
  gam_gr<-gam(form, data=extract_pres[[i]],  family=binomial(link='logit'), na.action=na.omit)  
  
  summary(glm_grF)
  
  
  #RF  
  rf_gr<-randomForest(formula=abundance ~ BO_sstmin + BO2_curvelmax_ss + BO2_salinitymean_ss + 
                        BO2_chlomean_ss, data=extract_pres[[i]], ntree=300, importance=TRUE  )
  
  assign(paste0('glm_gr', LETTERS[i]), glm_gr)
  assign(paste0('gam_gr', LETTERS[i]), gam_gr)
  assign(paste0('rf_gr', LETTERS[i]), rf_gr)
  
  pr_glm<-predict(area_pred, glm_gr, type='response')
  pr_gam<-predict(area_pred, gam_gr, type='response')
  pr_rf<-predict(area_pred, rf_gr, type='response')    
  
  assign(paste0('pr_glm_gr', LETTERS[i]), pr_glm)
  assign(paste0('pr_gam_gr', LETTERS[i]), pr_glm)
  assign(paste0('pr_rf_gr', LETTERS[i]), pr_rf)
  
  
  #make it so only the significant models are included 
  
  #make all the RMSE values 1/ themselves so that the larger errors become smaller proportions
  averages[[i]]<- averages[[i]]/((averages[[i]])^2)
  
  
  #extract p values of glm coefs 
  glm_pvals<- summary(glm_gr)$coefficients[,4]
  #remove intercept column 
  glm_pvals<- glm_pvals[-1]
  
  #trues are 1 and false 0. if sum =length then they are all over 0.05 and this makes averages 0
  if (sum(glm_pvals > 0.05) == length(glm_pvals)){
    averages[[i]][,1]<-0
    average_pear_pres[[i]][,1]<- 0
  }                        
  
  #now say if the RMSE is larger then the mean, proportion= 0
  if (averages.df_pres[i, 1]> (0.5*(max(extract_pres[[i]]$abundance)-min(extract_pres[[i]]$abundance)))){
    averages[[i]][,1]<- 0 
    average_pear_pres[[i]][,1]<-0
  }
  
  
  #now if the pearsons is smaller than 0.25 make it 0
  if (averages_pear.df_pres[i, 1]< 0.25) {
    averages[[i]][,1]<- 0 
    average_pear_pres[[i]][,1]<-0
  }
  #now same for gam 
  gam_step_table<-data.frame(summary(gam_gr)$s.table)
  
  if (sum (gam_step_table$p.value > 0.05)== length(gam_step_table$p.value)) {
    averages[[i]][,2]<-0
    average_pear_pres[[i]][,2]<-0
  }
  
  #if RMSE is larger then mean make it zero
  if (averages.df_pres[i, 2]> (0.5*(max(extract_pres[[i]]$abundance)-min(extract_pres[[i]]$abundance)))){
    averages[[i]][,2]<- 0 
    average_pear_pres[[i]][,2]<- 0
  }
  
  
  #if pearsons less than 0.25 make zero
  if (averages_pear.df_pres[i, 2]< 0.25) {
    averages[[i]][,2]<- 0 
    average_pear_pres[[i]][,2]<-0
  }
  
  
  #finally for RF
  if (averages.df_pres[i, 3]> (0.5*(max(extract_pres[[i]]$abundance)-min(extract_pres[[i]]$abundance)))){
    averages[[i]][,3]<- 0
    average_pear_pres[[i]][,3]<-0
  }
  
  
  #if RMSE is larger then mean make it zero
  if (averages.df_pres[i, 3]> (0.5*(max(extract_pres[[i]]$abundance)-min(extract_pres[[i]]$abundance)))){
    averages[[i]][,3]<- 0 
    average_pear_pres[[i]][,3]<- 0
  }
  
  #if pearsons less than 0.25 make zero
  if (averages_pear.df_pres[i, 3]< 0.25) {
    averages[[i]][,3]<- 0 
    average_pear_pres[[i]][,3]<-0
  }
  
  #make the ensemble model from RMSE and Pearsons proportions 
  props<-data.frame(glmR=1, gamR=1, rfR=1, glmP=1, gamP=1, rfP=1)
  props[1,1]<-(averages[[i]][1,1]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  props[1,2]<-(averages[[i]][1,2]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  props[1,3]<-(averages[[i]][1,3]/(averages[[i]][1,1]+averages[[i]][1,2]+averages[[i]][1,3]))
  
  props[1,4]<-abs(average_pear_pres[[i]][1,1])/(abs(average_pear_pres[[i]][1,1])+abs(average_pear_pres[[i]][1,2])+abs(average_pear_pres[[i]][1,3]))
  props[1,5]<-abs(average_pear_pres[[i]][1,2])/(abs(average_pear_pres[[i]][1,1])+abs(average_pear_pres[[i]][1,2])+abs(average_pear_pres[[i]][1,3]))
  props[1,6]<-abs(average_pear_pres[[i]][1,3])/(abs(average_pear_pres[[i]][1,1])+abs(average_pear_pres[[i]][1,2])+abs(average_pear_pres[[i]][1,3]))
  
  props<-data.frame(glm=((props[1,1]+props[1,4])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])), 
                    gam=((props[1,2]+props[1,5])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])),
                    rf=((props[1,3]+props[1,6])/(props[1,1]+props[1,2]+props[1,3]+props[1,4]+props[1,5]+props[1,6])))
  
  } , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  assign(paste0('prop_gr', LETTERS[i]), props)
  
  ensemble<- ((pr_glm*props[1,1])+(pr_gam*props[1,2])+ (pr_rf*props[1,3]))
  
  assign(paste0('ensemble_gr', LETTERS[i]), ensemble)
  
  
  
} 

library(viridis)
pal <- viridis(n=40, option='plasma', direction=-1)

ens_list<-lapply(ls(pattern="ensemble_gr"),get)

par(mar=c(1.2,1.2,1.2,1.2))
par(mfrow=c(4,3))

for ( i in 1:length(ens_list)){
  plot(ens_list[[i]], main= paste0('Group ',i), col=pal )
  plot(japan_outline, add=TRUE, col='grey', border='black')
  box()
}

summary(gam_grC)
gam_grC


gam.check(gam_grB)

