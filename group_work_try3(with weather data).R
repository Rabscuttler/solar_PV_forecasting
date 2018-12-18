library(lattice)
library(neuralnet)
# library(h2o)
library(caret)
library(glmnet)
library(lmridge)
library(ggforce)
library(psych)
library(ggExtra)
library(tidyverse)
library(corrplot)
library(forcats)
library(lubridate)
library(broom)
library(modelr)
library(data.table)

# Data preparation all years ----------------------------------------------

# Notes
# Change 2015 meteo filename to match other filenames
# Change 2016 meteo column header Direct -> Diffuse to match other years

df<-data.frame()
for(i in 1:4){
  k<-(i+2013)
  file_name=paste("data/sunlab-faro-pv-",k,".csv",sep="")
  data<-read.csv(file=file_name, stringsAsFactors = FALSE, sep=';')
  df<-rbind(df,data)
}

df <- drop_na(df)
df$Datetime <- lubridate::ymd_hms(df$Datetime)
df$Year <- lubridate::year(df$Datetime)
df$Month <- lubridate::month(df$Datetime, label = TRUE)
df$YDay <- lubridate::yday(df$Datetime)
df$Hour <- lubridate::hour(df$Datetime)
df$Minute <- lubridate::minute(df$Datetime)
df$Month <- as.numeric(df$Month)
df2 <- select(df, c("Datetime", "Year", "Month", "YDay", "Hour", "Minute"), contains("Optimal"))
sunlab_A <-  select(df2, c("Datetime", "Year", "Month", "YDay", "Hour", "Minute"), contains("A_"))
sunlab_B <-  select(df2, c("Datetime", "Year", "Month", "YDay", "Hour", "Minute"), contains("B_"))

# Add weather data
df1<-data.frame()
for(i in 1:4){
  k<-(i+2013)
  file_name=paste("data/sunlab-faro-meteo-",k,".csv",sep="")
  data<-read.csv(file=file_name, stringsAsFactors = FALSE, sep=';')
  df1<-rbind(df1,data)
}

sunlab_meteo_A <- df1[,c(1:7)]

sunlab_meteo_A$Datetime <- ymd_hms(sunlab_meteo_A$Datetime)
sunlab_A <- left_join(sunlab_A ,sunlab_meteo_A,by=c("Datetime"="Datetime"))

setnames(sunlab_A, old=c("Ambient.Temperature..ºC.", "Global.Radiation..W.m2.","Diffuse.Radiation..W.m2.",
                         "Ultraviolet..W.m2.","Wind.Velocity..m.s.","Wind.Direction..º."),
                   new=c("ambient_temperature","global_radiation","diffuse_radiation",
                         "ultraviolet","wind_velocity","wind_direction"))
# ,"Precipitation..mm.","Atmospheric.pressure..hPa. "precipitation","atmospheric_pressure"


sunlab_pv_17 <- filter(df, Year=="2017")

# data analysis for 2017 -----------------------------------------------------------

# the trend of a day
ggplot(sunlab_pv_17)+geom_boxplot(aes(x=as.factor(Hour),y=A_Optimal...Power.DC..W.))
ggplot(sunlab_pv_17)+geom_boxplot(aes(x=as.factor(Hour),y=A_Optimal...Current.DC..A.))
ggplot(sunlab_pv_17)+geom_boxplot(aes(x=as.factor(Hour),y=A_Optimal...Voltage.DC..V.))

# Power generation can be more simlar with current production. 
ggplot(sunlab_pv_17)+geom_boxplot(aes(x=as.factor(Month),y=A_Optimal...Power.DC..W.))
ggplot(sunlab_pv_17)+geom_boxplot(aes(x=as.factor(Month),y=A_Optimal...Voltage.DC..V.))
ggplot(sunlab_pv_17)+geom_boxplot(aes(x=as.factor(Month),y=A_Optimal...Current.DC..A.))

# can observe that the trends of A and B are quite similar
ggplot(sunlab_pv_17)+geom_boxplot(aes(x=as.factor(Month),y=A_Optimal...Power.DC..W.))
ggplot(sunlab_pv_17)+geom_boxplot(aes(x=as.factor(Month),y=B_Optimal...Power.DC..W.))

# boxplot to explain three angles' generation condition for the trend of a year
# the difference causes by the proportion of a-Si and mc-Si
ggplot(sunlab_pv_17)+geom_boxplot(aes(x=as.factor(Month),y=A_Horizontal...Power.DC..W.))
ggplot(sunlab_pv_17)+geom_boxplot(aes(x=as.factor(Month),y=A_Vertical...Power.DC..W.))
ggplot(sunlab_pv_17)+geom_boxplot(aes(x=as.factor(Month),y=A_Optimal...Power.DC..W.))

# Production by month
sunlab_pv_17$sum<-apply(sunlab_pv_17[,c(10,16)],1,sum,na.rm=T)
ggplot(sunlab_pv_17)+geom_boxplot(aes(x=as.factor(Month),y=sum))

# day of year test
ggplot(sunlab_pv_17)+geom_boxplot(aes(x=as.factor(YDay),y=A_Optimal...Power.DC..W.))

# meteo factor test
# TODO use a library to do the 'whitening' here - mean 0 and SD 1
sunlab_all_2017$ambient_temperature <- round(sunlab_all_2017$ambient_temperature)
sunlab_all_2017$global_radiation <- round(sunlab_all_2017$global_radiation/50)
sunlab_all_2017$diffuse_radiation <- round(sunlab_all_2017$diffuse_radiation/10)
sunlab_all_2017$ultraviolet <- round(sunlab_all_2017$ultraviolet/3)
sunlab_all_2017$wind_velocity <- round(sunlab_all_2017$wind_velocity*5)
sunlab_all_2017$wind_direction <- round(sunlab_all_2017$wind_direction/10)
sunlab_all_2017$precipitation <- round(sunlab_all_2017$precipitation)
sunlab_all_2017$atmospheric_pressure <- round(sunlab_all_2017$atmospheric_pressure-1000)

# 1.Ambient_temperature
# it is linear in the graph
ggplot(sunlab_all_2017)+geom_boxplot(aes(x=as.factor(ambient_temperature),
                                    y=A_Optimal...Power.DC..W.))

# 2.global_radiation
# because the sze of data in 23-30 is small, 
# it can be considered as a 0-21 reflecting 0 degree to 90 degree
ggplot(sunlab_all_2017)+geom_boxplot(aes(x=as.factor(global_radiation),
                                    y=A_Optimal...Power.DC..W.))
a<-sunlab_all_2017
# after mean comparison, 21 is the best
a<- filter(a,global_radiation=="21")
a$A_Optimal...Power.DC..W. <- as.numeric(a$A_Optimal...Power.DC..W.)
a <- drop_na(a)
mean(a$A_Optimal...Power.DC..W.)

# 3.diffuse_radiation
# For the time being, consider it as a linear fit, but the relationship should be small.
# various hour test get this conclusion 
ggplot(sunlab_all_2017)+geom_boxplot(aes(x=as.factor(diffuse_radiation),
                                    y=A_Optimal...Power.DC..W.))
# 4.ultraviolet
# because the sze of data in 24-30 is small, 
# it can be considered as a 0-22 reflecting 0 degree to 90 degree
ggplot(sunlab_all_2017)+geom_boxplot(aes(x=as.factor(ultraviolet),
                                    y=A_Optimal...Power.DC..W.))
b<-sunlab_all_2017
# after mean comparison, 22 is the best
b<- filter(b,ultraviolet=="22")
b$A_Optimal...Power.DC..W. <- as.numeric(b$A_Optimal...Power.DC..W.)
b <- drop_na(b)
mean(b$A_Optimal...Power.DC..W.)

# 5.wind_velocity
# after checking the wind velocity generally changes with the hour factor
# and in various hour test, it shows wind velocity is not a key factor
ggplot(sunlab_all_2017)+geom_boxplot(aes(x=as.factor(wind_velocity),
                                    y=A_Optimal...Power.DC..W.))

# Is wind_velocity correlated with hour? What if more wind during daylight
ggplot(sunlab_all_2017)+geom_boxplot(aes(x=as.factor(Hour),y=wind_velocity)) + ylim(0,50)
#Answer: somewhat

# 6.wind_direction----the strangest one
ggplot(sunlab_all_2017)+geom_boxplot(aes(x=as.factor(wind_direction),
                                    y=A_Optimal...Power.DC..W.))

# 7.precipitation
# seems not like a key factor
ggplot(sunlab_all_2017)+geom_boxplot(aes(x=as.factor(precipitation),
                                    y=A_Optimal...Power.DC..W.))
# Disagree - if it is raining (1 or 2) then power will likely be much lower.

# 8.atmospheric_pressure
# semms like a linear model
ggplot(sunlab_all_2017)+geom_boxplot(aes(x=as.factor(atmospheric_pressure),
                                    y=A_Optimal...Power.DC..W.))
# Low pressure means air rises, condenses and causes rain (and clouds). 
# High pressure means cloudless skies.

# focus on the same hour 
# because hour is the most important part according to precious linear regression
sunlab_hour10 <- filter(sunlab_all_2017,Hour=="12")
ggplot(sunlab_hour10)+geom_boxplot(aes(x=as.factor(ambient_temperature), y=A_Optimal...Power.DC..W.))
ggplot(sunlab_hour10)+geom_boxplot(aes(x=as.factor(global_radiation),y=A_Optimal...Power.DC..W.))
ggplot(sunlab_hour10)+geom_boxplot(aes(x=as.factor(diffuse_radiation),y=A_Optimal...Power.DC..W.))
ggplot(sunlab_hour10)+geom_boxplot(aes(x=as.factor(ultraviolet),y=A_Optimal...Power.DC..W.))
ggplot(sunlab_hour10)+geom_boxplot(aes(x=as.factor(wind_velocity),y=A_Optimal...Power.DC..W.))

# compare six different hours
sunlab_hour_vary <- filter(sunlab_all_2017,Hour=="8"|Hour=="10"|Hour=="12"|Hour=="14"|
                          Hour=="16"|Hour=="18")
ggplot(sunlab_hour_vary)+
  geom_boxplot(aes(x=as.factor(ambient_temperature),y=A_Optimal...Power.DC..W.))+
  facet_wrap(~Hour,nrow=2)

ggplot(sunlab_hour_vary)+
  geom_boxplot(aes(x=as.factor(global_radiation),y=A_Optimal...Power.DC..W.))+
  facet_wrap(~Hour,nrow=2)

ggplot(sunlab_hour_vary)+
  geom_boxplot(aes(x=as.factor(diffuse_radiation),y=A_Optimal...Power.DC..W.))+
  facet_wrap(~Hour,nrow=2)

ggplot(sunlab_hour_vary)+
  geom_boxplot(aes(x=as.factor(ultraviolet),y=A_Optimal...Power.DC..W.))+
  facet_wrap(~Hour,nrow=2)

ggplot(sunlab_hour_vary)+
  geom_boxplot(aes(x=as.factor(wind_velocity),
                   y=A_Optimal...Power.DC..W.))+
  facet_wrap(~Hour,nrow=2)

# ridge regression without weather-----------------------------------------
 sunlab_model <- lm(A_Optimal...Power.DC..W. ~ Month+YDay+Hour+Year,
                    data=sunlab_A)
 # find the key values
 mod_output <- tidy(sunlab_model)
 mod_output
 mod_output$p.value<.05
 
 x <- as.matrix( sunlab_A[,2:6])
 y <- as.matrix( sunlab_A[,12])
 # alpha=0 means ridge regression
 ridge <- glmnet(x, y, family = "gaussian", alpha = 0)
 print(ridge)
 
 # the value of punishment tiem as x label 
 plot(ridge,label=TRUE)
 # the logarithmic form of lambda as x label
 plot(ridge,xvar="lambda",label=TRUE)
 
 # find the optimum value
 cvfit = cv.glmnet(x, y)
 plot(cvfit)
 
 # min value and 1st standard error value
 cvfit$lambda.min
 cvfit$lambda.1se
 log(cvfit$lambda.1se)
 
 # the intercept and cofficients of 1 standard error method
 ridge.coef.1se <- coef(cvfit, s = "lambda.1se")
 round(ridge.coef.1se,2)
 
 # the intercept and cofficients of min method
 ridge.coef.min <- coef(cvfit, s = "lambda.min")
 round(ridge.coef.min,2)
 # in my opinion, the min method is better in the case
 
 # predict the generation values to show the effect of min method 
 # and 1 standard error method
 p <- predict(cvfit,x,s="lambda.min")
 q <- predict(cvfit,x,s="lambda.1se")
 
 sunlab_A$min <- p
 sunlab_A$fst <- q
 sunlab_pred <- sunlab_A[,c(1:6,12,19,20)]

 sunlab_pred$min <- as.numeric(sunlab_pred$min)
 sunlab_pred$fst <- as.numeric(sunlab_pred$fst)
 
 # select one month to show the difference
 sunlab_pred1 <- filter(sunlab_pred,Year=="2017",Month=="1")
 
 # the Visualization
 ggplot( sunlab_pred1 )+geom_text(aes(x=Datetime,y=A_Optimal...Power.DC..W.,
                                      label=Year),check_overlap = T)+
   geom_point(aes(x=Datetime,y=A_Optimal...Power.DC..W.))+
   geom_point(aes(x=Datetime,y=min),col='red')+
   theme_bw()
 
 tidy(sunlab_model)
 glance(sunlab_model)
 
# ridge regression with weather-----------------------------------------
 
 # Zoom in plot to see monthly and daily power curves.
sunlab_A_slice <- sunlab_A %>% 
                  filter(Datetime > as.Date("2017-07-01")) %>%
                  filter(Datetime < as.Date("2017-08-01"))

 ggplot(sunlab_A_slice, aes(Datetime, A_Optimal...Power.DC..W., group=1)) +
   geom_line(linetype="dotted", alpha=1) +
   facet_zoom(x = Datetime > as.Date("2017-07-11") & Datetime < as.Date("2017-07-15"),
              horizontal = FALSE, zoom.size = 0.6)
 
 # BOD1$Time <- factor(BOD1$Time) 
 # ggplot(sunlab_A, aes(x=Datetime, y=A_Optimal...Power.DC..W., group=1)) + geom_line(linetype="dotted")
 
 # four years total average generation varying with month
 # ggplot(sunlab_A)+geom_boxplot(aes(x=as.factor(Month),y=A_Optimal...Power.DC..W.))
 # four years total average generation varying with hour
 # ggplot(sunlab_A)+geom_boxplot(aes(x=as.factor(Hour),y=A_Optimal...Power.DC..W.))
 # four years total average generation varying with day of year
 # ggplot(sunlab_A)+geom_boxplot(aes(x=as.factor(YDay),y=A_Optimal...Power.DC..W.))
 
 # March and September got the largest value
 
sunlab_A <- filter(sunlab_A,Minute=="10"| Minute=="20"| Minute=="30"| Minute=="40" |Minute=="50" |Minute=="0" )

sunlab_A$Hour <- as.numeric(sunlab_A$Hour)
max(sunlab_A$Hour)
min(sunlab_A$Hour)
sunlab_A$hour_factor<-sin(((sunlab_A$Hour-5)/14)*2*pi-0.5*pi)

sunlab_A$Month <- as.numeric(sunlab_A$Month)
sunlab_A$month_factor1<-sin(((sunlab_A$Month)/12-0.125)*2*pi)
sunlab_A$month_factor2<-cos(((sunlab_A$Month)/12-0.125)*2*pi) 
sunlab_A$month_factor<-apply(sunlab_A[,c("month_factor1", "month_factor2")],1,sum,na.rm=T)
 max(sunlab_A$month_factor)

sunlab_A$ambient_temperature  <- as.numeric(sunlab_A$ambient_temperature )
sunlab_A$global_radiation<-sin((sunlab_A$global_radiation/1050)*0.5*pi)
sunlab_A$diffuse_radiation <- as.numeric(sunlab_A$diffuse_radiation ) 
sunlab_A$ultraviolet<-sin((sunlab_A$ultraviolet/66)*0.5*pi) 

sunlab_A$wind_velocity <- round(sunlab_A$wind_velocity*5)
sunlab_A$wind_direction <- round(sunlab_A$wind_direction/10)
# sunlab_A$precipitation <- round(sunlab_A$precipitation)
# sunlab_A$atmospheric_pressure <- (sunlab_A$atmospheric_pressure-1000)
 
 setnames(sunlab_A, old="A_Optimal...Temperature..ºC.", new="Optimal_Temperature")

 sunlab_A <- drop_na(sunlab_A)
  
 sunlab_model <- lm(A_Optimal...Power.DC..W. ~ 
                      month_factor+YDay+hour_factor+Minute+Year+Optimal_Temperature+
                      ambient_temperature+global_radiation+diffuse_radiation+
                      ultraviolet,
                    data=sunlab_A)
 
 #find the key values
 mod_output <- tidy(sunlab_model)
 mod_output
 mod_output$p.value<.05
 glance(sunlab_model)

# Ridge regression 
sunlab_A <- filter(sunlab_A, Year==2014)

 A<- sunlab_A[,c("Year","Month","YDay","Hour","ambient_temperature","global_radiation",
                                 "diffuse_radiation","ultraviolet","wind_velocity","wind_direction",
                                 "Optimal_Temperature","hour_factor", "month_factor")]
 x <- as.matrix(A)
 y <- sunlab_A[,12]
 #alpha=0 means ridge regression
 ridge <- glmnet(x, y, family = "gaussian", alpha = 0)
 print(ridge)

 tidy(ridge)
 glance(ridge)
 summary(ridge)
 
 # pR2 = 1 - mod$deviance / mod$null.deviance # works for glm
 # pr2 = sum(1 - ridge$dev.ratio / ridge$nulldev)
 # pr2
 
 # the value of punishment tiem as x label 
 plot(ridge,label=TRUE)
 # the logarithmic form of lambda as x label
 plot(ridge,xvar="lambda",label=TRUE)
 
 # find the optimum value
 cvfit = cv.glmnet(x, y)
 plot(cvfit)
 
 # min value and 1st standard error value
 cvfit$lambda.min
 cvfit$lambda.1se
 log(cvfit$lambda.1se)
 
 # the intercept and cofficients of 1 standard error method
 ridge.coef.1se <- coef(cvfit, s = "lambda.1se")
 round(ridge.coef.1se,2)
 
 # the intercept and cofficients of min method
 ridge.coef.min <- coef(cvfit, s = "lambda.min")
 round(ridge.coef.min,2)

 
 # predict the generation values to show the effect of min method and 
 # 1 standard error method
 s <- predict(cvfit,x,s="lambda.min")
 r <- predict(cvfit,x,s="lambda.1se")
 
 sunlab_A$min <- s
 sunlab_A$fst <- r
 sunlab_pred <- sunlab_A[,c(1:6,12,31,32)]
 
 # test the result
 # test <- sample(dim( sunlab_pred)[1], dim( sunlab_pred)[1] * 0.01)
 # sunlab_pred1 <- sunlab_pred[test, ]

 sunlab_pred$min <- as.numeric(sunlab_pred$min)
 sunlab_pred$fst <- as.numeric(sunlab_pred$fst)
 
 # choose the test data group
 sunlab_pred1 <- filter(sunlab_pred,Year=="2017",Month=="4")

 # the Visualization
 # min method
 ggplot( sunlab_pred1 )+
   geom_point(aes(x=Datetime,y=A_Optimal...Power.DC..W.), alpha=0.6)+
   geom_point(aes(x=Datetime,y=min),col='red', alpha=0.6)+
   theme_bw() 
  
 # 1st method
 ggplot( sunlab_pred1 )+
   geom_point(aes(x=Datetime,y=A_Optimal...Power.DC..W.))+
   geom_point(aes(x=Datetime,y=fst),col='red')+
   theme_bw()
 
 # after check, the min method and 1st method is similar in this situation.
 

# Ridge Using lmridge package ---------------------------------------------
 sunlab_R <- sunlab_A %>%
   filter(Year==2014) %>%
   filter(Hour==12)

 sunlab_R <- drop_na(sunlab_R)
  
 mod <- lmridge(A_Optimal...Power.DC..W. ~ 
                   YDay +Minute+Optimal_Temperature+
                   ambient_temperature+global_radiation+diffuse_radiation+
                   ultraviolet,
                   data=sunlab_R, K = 1, scaling = "sc")

?lmridge
?rstats1
rstats1(mod)

pred2 <-  predict(mod)

# choose the test data group
pred2 <- filter(pred2,Year=="2017",Month=="1")

# the Visualization
# min method
ggplot(pred2)+
  geom_point(aes(x=Datetime,y=A_Optimal...Power.DC..W.), alpha=0.6)+
  geom_point(aes(x=Datetime,y=fst),col='red', alpha=0.6)+
  theme_bw()

ggplot(pred2)+
  geom_point()+
  theme_bw()



