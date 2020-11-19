library(knitr)
library(ggplot2)
library(skimr)
library(xtable)
library(rms) #for VIF
library(MASS)
library(pander)
library(arm)
library(pROC)
library(caret)
require(gridExtra)
library(lattice)
library(sjPlot)
library(mice)
  ride <- read.table ("rideshare_kaggle.csv", header=T, sep=",")
  #Focus on Lyft
  ride<-ride[which(ride$cab_type=='Lyft'),]
  #Only contain the variables you are interested in looking
  r1 <- ride[, names(ride) %in% c("hour","source","destination","name","price","distance", "surge_multiplier", "temperature", 
                                   "apparentTemperature","precipIntensity","pressure","windSpeed", "windGust", "icon","windSpeed", "visibility","windBearing","long_summary")]
  remove(ride)
  
  r1$icon<-factor(r1$icon)
  r1$source<-factor(r1$source)
  r1$destination <- relevel(factor(r1$destination), ref = 12)
  r1$name<-factor(r1$name)
  r1$long_summary<-factor(r1$long_summary)
  #Remove surge effects
  #r1$price2<-r1$price/r1$surge_multiplier
  r1$logprice<-log(r1$price)
  #Delete rows that have null values
  r1_final <- na.omit(r1)
  remove(r1)
  #Mean_Centered
  r1_final$distance_cent<-r1_final$distance-mean(r1_final$distance)
  r1_final$temp_cent<-r1_final$temperature-mean(r1_final$temperature)
  r1_final$atemp_cent<-r1_final$apparentTemperature-mean(r1_final$apparentTemperature)
  r1_final$precip_cent<-r1_final$precipIntensity-mean(r1_final$precipIntensity)
  r1_final$ws_cent<-r1_final$windSpeed-mean(r1_final$windSpeed)
  r1_final$wg_cent<-r1_final$windGust-mean(r1_final$windGust)
  r1_final$vis_cent<-r1_final$visibility-mean(r1_final$visibility)
  r1_final$pre_cent<-r1_final$pressure-mean(r1_final$pressure)
  r1_final$wb_cent<-r1_final$windBearing-mean(r1_final$windBearing)
  #Categorize Rush Hours
  r1_final$rush<-r1_final$hour
  r1_final$rush[r1_final$rush<=9 & r1_final$rush>=7]<-'Y'
  r1_final$rush[r1_final$rush<=19 & r1_final$rush>=17]<-'Y'
  r1_final$rush[r1_final$rush!='Y']<-'N'
  r1_final$rush<-factor(r1_final$rush)
  #Categorize Distance
  # r1_final$dist_cat<-r1_final$distance
  # r1_final$dist_cat[r1_final$dist_cat<1 & r1_final$dist_cat>=0]<-'0~1'
  # r1_final$dist_cat[r1_final$dist_cat<2 & r1_final$dist_cat>=1]<-'1~2'
  # r1_final$dist_cat[r1_final$dist_cat<3 & r1_final$dist_cat>=2]<-'2~3'
  # r1_final$dist_cat[r1_final$dist_cat<4 & r1_final$dist_cat>=3]<-'3~4'
  # r1_final$dist_cat[r1_final$dist_cat<7 & r1_final$dist_cat>=4]<-'4~7'
  # r1_final$dist_cat<-factor(r1_final$dist_cat)
  #Categorize Surge Multiplier
  # r1_final$sm<-r1_final$surge_multiplier
  # r1_final$sm[r1_final$sm<=3 & r1_final$sm>=2.5]<-'2.5 & Above'
  # r1_final$sm[r1_final$sm==1]<-'1'
  # r1_final$sm[r1_final$sm==1.25]<-'1.25'
  # r1_final$sm[r1_final$sm==1.5]<-'1.5'
  # r1_final$sm[r1_final$sm==1.75]<-'1.75'
  # r1_final$sm[r1_final$sm==2]<-'2'
  # r1_final$sm<-factor(r1_final$sm)
  r1_final$sm<-r1_final$surge_multiplier
  r1_final$sm[r1_final$sm>1]<-'Above 1'
  r1_final$sm[r1_final$s==1]<-'Equal to 1'
  r1_final$sm<-factor(r1_final$sm)
  #Sampling
  set.seed(1)
  sample<-r1_final[sample(nrow(r1_final), 50000, replace = FALSE, prob = NULL),]
  remove(r1_final)
  sample_ins<-subset(sample,sample$price<9.2)
  #dim(sample)

########################################################################################
############################# EDA (Linear regression) ##################################
########################################################################################

#---NORMALITY---# (Slightly skewed to the left)
ggplot(sample,aes(price)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) + theme(legend.position="none") +
  geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Price",y="Price") + theme_classic()+ theme(plot.title = element_text(hjust = 0.5))
#Log Price looks better...
ggplot(sample,aes(logprice)) +
geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) + theme(legend.position="none") +
geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
labs(title="Distribution of Log Price",y="Log Price") + theme_classic()+ theme(plot.title = element_text(hjust = 0.5))
#---NUMERIC---# 
ggplot(sample,aes(x=distance, y=price)) +
  geom_point() + #coord_flip()# +
  scale_fill_brewer(palette="Blues") +
  labs(title="Distanc vs Price",
       x="Distance",y='Price') + 
  geom_smooth(col="red3", method="lm") +
  theme_classic() + theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))
#Distance VS Price <-Positive Association
ggplot(sample,aes(x=distance_cent, y=logprice)) +
  geom_point() + #coord_flip()# +
  scale_fill_brewer(palette="Blues") +
  labs(title="Distanc vs Price",
       x="Distance",y='Price') + 
  geom_smooth(col="red3", method="lm") +
  theme_classic() + theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))
# #Day VS Price <-No association
# ggplot(sample,aes(x=day, y=logprice)) +
#   geom_point() + #coord_flip()# +
#   scale_fill_brewer(palette="Blues") +
#   labs(title="Day vs Price",
#        x="Day",y='Price') + 
#   geom_smooth(col="red3", method="lm") +
#   theme_classic() + theme(legend.position="none")+
#   theme(plot.title = element_text(hjust = 0.5))
#Temperature VS Price <-No association
ggplot(sample,aes(x=temp_cent, y=logprice)) +
  geom_point() + #coord_flip()# +
  scale_fill_brewer(palette="Blues") +
  labs(title="Temperature vs Price",
       x="Temperature",y='Price') + 
  geom_smooth(col="red3", method="lm") +
  theme_classic() + theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))
#Apparent Temperature VS Price <-No association
ggplot(sample,aes(x=atemp_cent, y=logprice)) +
  geom_point() + #coord_flip()# +
  scale_fill_brewer(palette="Blues") +
  labs(title="Apparent Temperature vs Price",
       x="Apparent Temperature",y='Price') + 
  geom_smooth(col="red3", method="lm") +
  theme_classic() + theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))
#Precip Intensity VS Price <-No association
ggplot(sample,aes(x=precip_cent, y=logprice)) +
  geom_point() + #coord_flip()# +
  scale_fill_brewer(palette="Blues") +
  labs(title="precipIntensity vs Price",
       x="precipIntensity",y='Price') + 
  geom_smooth(col="red3", method="lm") +
  theme_classic() + theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))
#windSpeed VS Price <-No association
ggplot(sample,aes(x=ws_cent, y=logprice)) +
  geom_point() + #coord_flip()# +
  scale_fill_brewer(palette="Blues") +
  labs(title="windSpeed vs Price",
       x="windSpeed",y='Price') + 
  geom_smooth(col="red3", method="lm") +
  theme_classic() + theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))
#windGust VS Price <-No association
ggplot(sample,aes(x=wg_cent, y=logprice)) +
  geom_point() + #coord_flip()# +
  scale_fill_brewer(palette="Blues") +
  labs(title="windGust vs Price",
       x="windGust",y='Price') + 
  geom_smooth(col="red3", method="lm") +
  theme_classic() + theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))
#Visibility VS Price <-No association
ggplot(sample,aes(x=vis_cent, y=logprice)) +
  geom_point() + #coord_flip()# +
  scale_fill_brewer(palette="Blues") +
  labs(title="Visibility vs Price",
       x="Visibility",y='Price') + 
  geom_smooth(col="red3", method="lm") +
  theme_classic() + theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))
#pressure VS Price <-No association
ggplot(sample,aes(x=pre_cent, y=logprice)) +
  geom_point() + #coord_flip()# +
  scale_fill_brewer(palette="Blues") +
  labs(title="Pressure vs Price",
       x="Pressure",y='Price') + 
  geom_smooth(col="red3", method="lm") +
  theme_classic() + theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))
#windBearing VS Price <-No association
ggplot(sample,aes(x=wb_cent, y=logprice)) +
  geom_point() + #coord_flip()# +
  scale_fill_brewer(palette="Blues") +
  labs(title="windBearing vs Price",
       x="windBearing",y='Price') + 
  geom_smooth(col="red3", method="lm") +
  theme_classic() + theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))

#---CATEGORICAL---# 
#Distance Category VS Price
ggplot(sample,aes(x=dist_cat, y=logprice, fill=dist_cat)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Distance vs Price",
       x="Distance",y="Price") +
  theme_classic() + theme(legend.position="none")
#Rush VS Price<-No association
ggplot(sample,aes(x=rush, y=logprice, fill=rush)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Rush hours vs Price",
       x="Rush Hours",y="Price") +
  theme_classic() + theme(legend.position="none")
#Cab Name VS Price<- Difference in distribution and median
ggplot(sample,aes(x=name, y=logprice, fill=name)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Ride Types vs Price",
       x="Ride Types",y="Price") +
  theme_classic() + theme(legend.position="none")+ theme(plot.title = element_text(hjust = 0.5))
#Icon VS Price<-No association
ggplot(sample,aes(x=icon, y=logprice, fill=icon)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Icon vs Price",
       x="Icon",y="Price") +
  theme_classic() + theme(legend.position="none")
#Destination VS Price<- Difference in distribution and median
ggplot(sample,aes(x=destination, y=logprice, fill=destination)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Destination vs Price",
       x="Destination",y="Price") +
  theme_classic() + theme(legend.position="none")+theme(plot.title = element_text(hjust = 0.5))
#Source VS Price<- Difference in distribution and median
ggplot(sample,aes(x=source, y=logprice, fill=source)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Source vs Price",
       x="Source",y="Price") +
  theme_classic() + theme(legend.position="none")
#Surge VS Price<- Difference in distribution and median
ggplot(sample,aes(x=sm, y=logprice, fill=sm)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Surge Multiplier vs Price",
       x="Surge Multiplier",y="Price") +
  theme_classic() + theme(legend.position="none")+theme(plot.title = element_text(hjust = 0.5))
# #Long Summary VS Price<-Slight difference in median
# ggplot(sample,aes(x=long_summary, y=logprice, fill=long_summary)) +
#   geom_boxplot() + #coord_flip() +
#   scale_fill_brewer(palette="Reds") +
#   labs(title="Long Summary vs Price",
#        x="Long Summary",y="Price") +
#   theme_classic() + theme(legend.position="none")

#######################
##### Interaction #####
#######################

#########################################
### surge_multiplier vs all categorical predictors ### 
### surge & source ### <-No difference
ggplot(sample,aes(x=sm, y=logprice)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Surge Multiplier vs Price by Source",x="Surge Multiplier",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9)) +
  facet_wrap( ~ source,ncol=2)
### surge & destination ### <- No difference 
ggplot(sample,aes(x=sm, y=logprice)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Surge Multiplier vs Price by Destination",x="Surge Multiplier",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9)) +
  facet_wrap( ~ destination,ncol=2)
### surge & cab_type ### <-Uber does not have surge multipler
# ggplot(sample,aes(x=sm, y=logprice)) +
#   geom_boxplot() + #coord_flip() +
#   scale_fill_brewer(palette="Blues") + theme_classic() +
#   labs(title="Surge Multiplier vs Price by Cab Type",x="Surge Multiplier",y="Price") +
#   theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
#         axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9)) +
#   facet_wrap( ~ cab_type,ncol=2)
### surge & long summary ### <-no difference
# ggplot(sample,aes(x=sm, y=logprice)) +
#   geom_boxplot() + #coord_flip() +
#   scale_fill_brewer(palette="Blues") + theme_classic() +
#   labs(title="Surge Multiplier vs Price by Long Summary",x="Surge Multiplier",y="Price") +
#   theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
#         axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9)) +
#   facet_wrap( ~ icon,ncol=2)
### surge & rush hour ### <-no difference in trend
ggplot(sample,aes(x=sm, y=logprice)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Surge Multiplier vs Price by Rush",x="Surge Multiplier",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9)) +
  facet_wrap( ~ rush,ncol=2)
### surge & icon ### <-No difference
ggplot(sample,aes(x=sm, y=logprice)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Surge Multiplier vs Price by Icon",x="Surge Multiplier",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9)) +
  facet_wrap( ~ icon,ncol=2)
### surge_multiplier vs all numeric predictors ### 
### surge & temperature ### <-slight difference
ggplot(sample,aes(x=temp_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Temperature vs Price by Surge Multiplier",x="Temperature",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ sm,ncol=3)
### surge & apparentTemperature ### <-slight difference
ggplot(sample,aes(x=atemp_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Apparent Temperature vs Price by Surge Multiplier",x="Apparent Temperature",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ sm,ncol=3)
### surge & precipIntensity ### <-no difference
ggplot(sample,aes(x=precip_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Precipation Intensity vs Price by Surge Multiplier",x="Precipitation Intensity",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ sm,ncol=3)
### surge & windSpeed ### <-no difference
ggplot(sample,aes(x=ws_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Wind Speed vs Price by Surge Multiplier",x="Wind Speed",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ sm,ncol=3)
### surge & windGust ### <-no difference
ggplot(sample,aes(x=wg_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Wind Gust vs Price by Surge Multiplier",x="Wind Gust",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ sm,ncol=3)
### surge & visibility ### <-no difference
ggplot(sample,aes(x=vis_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Visibility vs Price by Surge Multiplier",x="Visibility",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ sm,ncol=3)
### surge & pressure ### <-no difference
ggplot(sample,aes(x=pre_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Pressure vs Price by Surge Multiplier",x="Pressure",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ sm,ncol=3)
### surge & windBearing ### <-no difference
ggplot(sample,aes(x=wb_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Wind Bearing vs Price by Surge Multiplier",x="Wind Bearing",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ sm,ncol=3)
------------------------------------------------------------------------------------------
### name vs all numeric predictors ###
#name & temperature <-no obvious difference
  ggplot(sample,aes(x=temp_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Temperature vs Price by Cab Name",x="Temperature",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ name,ncol=3)
### name & apparentTemperature ### <-no obvious difference
ggplot(sample,aes(x=atemp_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Apparent Temperature vs Price by Surge Cab Name",x="Apparent Temperature",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ name,ncol=3)
### name & precipIntensity ### <-no obvious difference
ggplot(sample,aes(x=precip_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Precipation Intensity vs Price by Cab Name",x="Precipitation Intensity",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ name,ncol=3)
### name & windSpeed ### <-no obvious difference
ggplot(sample,aes(x=ws_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Wind Speed vs Price by Surge Multiplier",x="Wind Speed",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ name,ncol=3)
### name & windGust ### <-no obvious difference
ggplot(sample,aes(x=wg_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Wind Gust vs Price by Cab Name",x="Wind Gust",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ name,ncol=3)
### name & visibility ### <-no obvious difference
ggplot(sample,aes(x=vis_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Visibility vs Price by Cab Name",x="Visibility",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ name,ncol=3)
### name & pressure ### <-no obvious difference
ggplot(sample,aes(x=pre_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Pressure vs Price by Cab Name",x="Pressure",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ name,ncol=3)
### namee & windBearing ### <-no obvious difference
ggplot(sample,aes(x=wb_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Wind Bearing vs Price by Cab Name",x="Wind Bearing",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ name,ncol=3)
### name vs all categorical predictors ### 
### name & source ### <-no obvious difference
ggplot(sample,aes(x=source, y=logprice)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Source vs Price by Cab Name",x="Surge Multiplier",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9)) +
  facet_wrap( ~ name,ncol=2)
### name & destination ### <-no obvious difference
ggplot(sample,aes(x=destination, y=logprice)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Destination vs Price by Cab Name",x="Surge Multiplier",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9)) +
  facet_wrap( ~ name,ncol=2)

### name & rush hour ### <-no difference in trend
ggplot(sample,aes(x=rush, y=logprice)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Rush Hour vs Price by Cab Name",x="Rush Hour",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9)) +
  facet_wrap( ~ name,ncol=2)
### name & icon ### <-No obvious difference in distribution
ggplot(sample,aes(x=icon, y=logprice)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Icon vs Price by Cab Name",x="Icon",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9)) +
  facet_wrap( ~ name,ncol=2)
---------------------------------------------------------------------------------------------------------
### icon vs all temperature related  predictors ###
  #icon & temperature <-slight difference in trend (fog)
  ggplot(sample,aes(x=temp_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Temperature vs Price by Icon",x="Temperature",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ icon,ncol=3)
### icon & apparentTemperature ### <-slight difference (fog)
ggplot(sample,aes(x=atemp_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Apparent Temperature vs Price by Icon",x="Apparent Temperature",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ icon,ncol=3)
### icon & precipIntensity ### <-obvious difference in distribution
ggplot(sample,aes(x=precip_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Precipation Intensity vs Price by Icon",x="Precipitation Intensity",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ icon,ncol=3)
### icon & windSpeed ### <-no obvious difference
ggplot(sample,aes(x=ws_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Wind Speed vs Price by Icon",x="Wind Speed",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ icon,ncol=3)
### icon & windGust ### <-obvious difference in distribution
ggplot(sample,aes(x=wg_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Wind Gust vs Price by Icon",x="Wind Gust",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ icon,ncol=3)
### icon & visibility ### <-obvious difference in distribution
ggplot(sample,aes(x=vis_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Visibility vs Price by Icon",x="Visibility",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ icon,ncol=3)
### icon & pressure ### <-obvious difference in distribution
ggplot(sample,aes(x=pre_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Pressure vs Price by Icon",x="Pressure",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ icon,ncol=3)
### icon & windBearing ### <-obvious difference in distribution
ggplot(sample,aes(x=wb_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Wind Bearing vs Price by Icon",x="Wind Bearing",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ icon,ncol=3)

### distance & destination ### <-no difference in distribution
ggplot(sample,aes(x=distance_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Distance vs Price by Destination",x="Distance",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ destination,ncol=3)
### distance & rush ### <-no difference in distribution
ggplot(sample,aes(x=distance_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Distance vs Price by Rush",x="Distance",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ rush,ncol=3)
### distance & surge multiplier ### <-no difference in distribution
ggplot(sample,aes(x=distance_cent, y=logprice)) +
  geom_point() + #coord_flip() +
  geom_smooth(col='red3', method='lm') +
  scale_fill_brewer(palette="Blues") + theme_classic() +
  labs(title="Distance vs Price by Surge Multiplier",x="Distance",y="Price") +
  theme(legend.position="none", axis.title.x =element_text(size=15), axis.title.y=element_text(size=15),
        axis.text = element_text(size=14), plot.title = element_text(size = rel(1.5), lineheight = .9,family = "Times", face = "bold.italic")) +
  facet_wrap( ~ sm,ncol=3)
############################# stepwise ############################# 
NullModel<-lm(logprice~1,data=sample)
FullModel<-lm(logprice~destination+name+distance_cent+sm*(icon+temp_cent+atemp_cent+precip_cent+ws_cent+wg_cent+vis_cent+pre_cent+wb_cent+rush)+name*(icon+temp_cent+atemp_cent+precip_cent+ws_cent+wg_cent+vis_cent+pre_cent+wb_cent+rush)
              +icon*(temp_cent+atemp_cent+precip_cent+ws_cent+wg_cent+vis_cent+pre_cent+wb_cent)+ distance_cent*(rush+sm+name+destination),data=sample)
model_back <- step(NullModel, scope = formula(FullModel),direction="both",trace=0)
# Let's see the variables the model selected
summary(model_back)
#################################  anova #######################################
# 0.0624 (keep sm:rush)
model_1_1 <-lm(formula = logprice ~ name + distance_cent + sm + destination + 
                 rush + name:distance_cent + distance_cent:destination + distance_cent:sm + 
                 name:rush, data = sample)
anova(model_back, model_1_1)
# 0.02046 (keep name:rush)
model_1_2 <-lm(formula = logprice ~ name + distance_cent + sm + destination + 
                 rush + name:distance_cent + distance_cent:destination + distance_cent:sm
               + sm:rush, data = sample)
anova(model_back, model_1_2)
# eliminate  0.86 (discard icon)
model_1_3 <-lm(formula = logprice ~ name + distance_cent + sm + destination + 
                 rush + name:distance_cent + distance_cent:destination + distance_cent:sm + 
                 name:rush + sm:rush + icon, data = sample)
anova(model_back, model_1_3)
# eliminate  0.89 (discard temperature)
model_1_4 <-lm(formula = logprice ~ name + distance_cent + sm + destination + 
                 rush + name:distance_cent + distance_cent:destination + distance_cent:sm + 
                 name:rush + sm:rush + temp_cent, data = sample)
anova(model_back, model_1_4)
# eliminate  0.65 (discard apparent temperature)
model_1_5 <-lm(formula = logprice ~ name + distance_cent + sm + destination + 
                 rush + name:distance_cent + distance_cent:destination + distance_cent:sm + 
                 name:rush + sm:rush + atemp_cent, data = sample)
anova(model_back, model_1_5)
# eliminate  0.82 (discard precipitation)
model_1_6 <-lm(formula = logprice ~ name + distance_cent + sm + destination + 
                 rush + name:distance_cent + distance_cent:destination + distance_cent:sm + 
                 name:rush + sm:rush + precip_cent, data = sample)
anova(model_back, model_1_6)
# eliminate  0.5 (discard wind speed)
model_1_7 <-lm(formula = logprice ~ name + distance_cent + sm + destination + 
                 rush + name:distance_cent + distance_cent:destination + distance_cent:sm + 
                 name:rush + sm:rush + ws_cent, data = sample)
anova(model_back, model_1_7)
# eliminate  0.48 (discard wind gust)
model_1_8 <-lm(formula = logprice ~ name + distance_cent + sm + destination + 
                 rush + name:distance_cent + distance_cent:destination + distance_cent:sm + 
                 name:rush + sm:rush + wg_cent, data = sample)
anova(model_back, model_1_8)
# eliminate  0.34 (discard visibility)
model_1_9 <-lm(formula = logprice ~ name + distance_cent + sm + destination + 
                 rush + name:distance_cent + distance_cent:destination + distance_cent:sm + 
                 name:rush + sm:rush + vis_cent, data = sample)
anova(model_back, model_1_9)
# eliminate  0.82 (discard precipitation)
model_1_10 <-lm(formula = logprice ~ name + distance_cent + sm + destination + 
                  rush + name:distance_cent + distance_cent:destination + distance_cent:sm + 
                  name:rush + sm:rush + precip_cent, data = sample)
anova(model_back, model_1_10)
# eliminate  0.27 (discard wind bearing)
model_1_11 <-lm(formula = logprice ~ name + distance_cent + sm + destination + 
                  rush + name:distance_cent + distance_cent:destination + distance_cent:sm + 
                  name:rush + sm:rush + wb_cent, data = sample)
anova(model_back, model_1_11)
# close to 0 (keep surge multiplier:destination)
model_1_12 <-lm(formula = logprice ~ name + distance_cent + sm + destination + 
                  rush + name:distance_cent + distance_cent:destination + distance_cent:sm + 
                  name:rush + sm:rush + sm:destination, data = sample)
anova(model_back, model_1_12)
# close to 0.1 (keep surge multiplier:name)
model_1_13 <-lm(formula = logprice ~ name + distance_cent + sm + destination + 
                  rush + name:distance_cent + distance_cent:destination + distance_cent:sm + 
                  name:rush + sm:rush + sm:destination + sm:name, data = sample)
anova(model_1_12, model_1_13)

################################################################################
############################ Checking Assumptinos ##############################
################################################################################
model_final <-lm(formula = logprice ~ name + distance_cent + sm + destination + 
                   rush + name:distance_cent + distance_cent:destination + distance_cent:sm + 
                   name:rush + sm:rush + sm:destination, data = sample)

## linearity
ggplot(sample,aes(x=distance_cent,y=model_final$residual)) + geom_point(alpha=.7) + 
  geom_hline(yintercept=0, col='red3') + theme_classic() +
  labs(title = 'Distance VS Residual', x='Distance',y="Residuals")

ggplot(sample,aes(x=temp_cent,y=model_final$residual)) + geom_point(alpha=.7) + 
  geom_hline(yintercept=0, col='red3') + theme_classic() +
  labs(title = 'Temperature VS Residual', x='Temperature',y="Residuals")

# Independence and Equal variance
plot(model_final, which=1, col=c('blue4'))

# Normality
plot(model_final, which=2, col=c('blue4'))


################################### Ourliers ###################################

# Influential Points
plot(model_final,which=5,col=c("blue4"))

# Leverage
n <- nrow(model.matrix(model_final)); p <- ncol(model.matrix(model_final))
lev_scores <- hatvalues(model_final) 
plot(lev_scores,col=ifelse(lev_scores > (2*p/n), 'red2', 'navy'),type="h",
     ylab="Leverage score",xlab="Index",main="Leverage Scores for all observations")
text(x=c(1:n)[lev_scores > (2*p/n)]+c(rep(2,4),-2,2),y=lev_scores[lev_scores > (2*p/n)],
     labels=c(1:n)[lev_scores > (2*p/n)])


############################## multicollinearity ###############################

model_vif <- lm(formula = logprice ~ name + distance_cent + sm + destination + 
                  rush, data = sample)
vif(model_vif)


y_predict_total <- predict(model_final, wages)
RSME_total <- (wages$diff - y_predict_total)^2
mean(RSME_total)
