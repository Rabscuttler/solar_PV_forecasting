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

# Zoom in plot to see monthly and daily power curves.
sunlab_A_slice <- sunlab_A %>% 
  filter(Datetime > as.Date("2017-07-01")) %>%
  filter(Datetime < as.Date("2017-08-01"))

ggplot(sunlab_A_slice, aes(Datetime, A_Optimal...Power.DC..W., group=1)) +
  geom_line(linetype="dotted", alpha=1) +
  facet_zoom(x = Datetime > as.Date("2017-07-11") & Datetime < as.Date("2017-07-15"),
             horizontal = FALSE, zoom.size = 0.6)

# Big plots
# ggplot(sunlab_A, aes(x=Datetime, y=A_Optimal...Power.DC..W., group=1)) + geom_line(linetype="dotted")

# four years total average generation varying with month
# ggplot(sunlab_A)+geom_boxplot(aes(x=as.factor(Month),y=A_Optimal...Power.DC..W.))
# four years total average generation varying with hour
# ggplot(sunlab_A)+geom_boxplot(aes(x=as.factor(Hour),y=A_Optimal...Power.DC..W.))
# four years total average generation varying with day of year
# ggplot(sunlab_A)+geom_boxplot(aes(x=as.factor(YDay),y=A_Optimal...Power.DC..W.))
