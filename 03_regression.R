
# Functions ---------------------------------------------------------------

# R squared function 
rsquared <- function(pred, test){
  mean <- 0
  tot <- 0
  res <- 0
  for (i in seq_along(pred)){
    mean <- mean + test[i]
  }
  mean <- mean/length(pred)
  for (i in seq_along(pred)){
    tot <- tot+(test[i]-mean)^2
  }
  for (i in seq_along(pred)){
    res <- res+(test[i]-pred[i])^2
  }
  R_square <- (1-res/tot)
  return(R_square)
}

# Experiments and understanding --------------------------------------------------

# Visualise hour factor transformation
sunlab_A %>% mutate(date = as_date(Datetime)) %>%
 filter(date > as_date("2017-01-01") & date < as_date("2017-02-01")) %>%
 ggplot(aes(date, as.numeric(hour_factor))) + geom_point() +
  geom_jitter(width=0.2)


# Prepare data ------------------------------------------------------------

sunlab_A <- filter(sunlab_A,Minute=="10"| Minute=="20"| Minute=="30"| Minute=="40" |Minute=="50" |Minute=="0" )
sunlab_A$Hour <- as.numeric(sunlab_A$Hour)

# Linearise the data
sunlab_A$hour_factor<-sin(((sunlab_A$Hour-5)/14)*2*pi-0.5*pi)
# sunlab_A$hour_factor <- sunlab_A$Hour
sunlab_A %<>% mutate(time_factor = Hour + Minute/60)
sunlab_A$time_factor<-sin(((sunlab_A$time_factor-5)/14)*2*pi-0.5*pi)
sunlab_A$Month <- as.numeric(sunlab_A$Month)
sunlab_A$month_factor1<-sin(((sunlab_A$Month)/12-0.125)*2*pi)
sunlab_A$month_factor2<-cos(((sunlab_A$Month)/12-0.125)*2*pi) 
sunlab_A$month_factor<-apply(sunlab_A[,c("month_factor1", "month_factor2")],1,sum,na.rm=T)
max(sunlab_A$month_factor)

# Normalise the data
sunlab_A$ambient_temperature  <- as.numeric(sunlab_A$ambient_temperature) 
sunlab_A$ambient_temperature  <- (621.9*sin(0.005941*sunlab_A$ambient_temperature-0.02528)+ 47.3*sin(0.08446*sunlab_A$ambient_temperature-0.1815) )                                          
sunlab_A$global_radiation<- (527.9*sin(sunlab_A$global_radiation*0.00229+0.1611)+393.3*sin(sunlab_A$global_radiation*0.002521+3.334))
sunlab_A$diffuse_radiation <- as.numeric(sunlab_A$diffuse_radiation ) 
sunlab_A$diffuse_radiation <- (1243*sin(0.006054*sunlab_A$diffuse_radiation-0.9234) +1126*sin(0.006491*sunlab_A$diffuse_radiation+2.039))
sunlab_A$ultraviolet<- 169.7*sin(sunlab_A$ultraviolet*0.02647+0.02187) 
sunlab_A$wind_velocity <- round(sunlab_A$wind_velocity*5)
sunlab_A$wind_direction <- round(sunlab_A$wind_direction/10)
sunlab_A$precipitation <- round(sunlab_A$precipitation)
sunlab_A$atmospheric_pressure <- (sunlab_A$atmospheric_pressure-1000)
setnames(sunlab_A, old="A_Optimal...Temperature..ºC.", new="Optimal_Temperature")
sunlab_A <- drop_na(sunlab_A)


# LM and ridge regression without weather-----------------------------------------
############# Simple Linear Model
sunlab_model <- lm(A_Optimal...Power.DC..W. ~ Month+YDay+hour_factor+Year, data=sunlab_A)
tidy(sunlab_model) # find the key values
glance(sunlab_model)

############ Ridge Regression
x <- as.matrix( sunlab_A[,c("Year", "Month", "YDay","time_factor")])
y <- as.matrix( sunlab_A[,"A_Optimal...Power.DC..W."])
ridge <- glmnet(x, y, family = "gaussian", alpha = 0) # alpha=0 means ridge regression

# plot(ridge,label=TRUE) # # the value of punishment time as x label 
# plot(ridge,xvar="lambda",label=TRUE) # # the logarithmic form of lambda as x label

cvfit = cv.glmnet(x, y) # find the optimum value
# plot(cvfit)

# cvfit$lambda.min # # min value and 1st standard error value
# cvfit$lambda.1se
# log(cvfit$lambda.1se)

ridge.coef.1se <- coef(cvfit, s = "lambda.1se") # the intercept and cofficients of 1 standard error method
round(ridge.coef.1se,2)
ridge.coef.min <- coef(cvfit, s = "lambda.min") # the intercept and cofficients of min method
round(ridge.coef.min,2) # in my opinion, the min method is better in the case

# predict the generation values to show the effect of min method 
# and 1 standard error method
sunlab_A$min <- predict(cvfit,x,s="lambda.min")
sunlab_A$fst <- predict(cvfit,x,s="lambda.1se")
sunlab_pred <- sunlab_A[,c("Datetime", "Year", "Month", "YDay","time_factor","Minute",
                          "A_Optimal...Power.DC..W.","min", "fst")]
sunlab_pred$min <- as.numeric(sunlab_pred$min)
sunlab_pred$fst <- as.numeric(sunlab_pred$fst)
sunlab_pred1 <- filter(sunlab_pred,Year=="2017",Month=="1") # select one month to show the difference

ggplot( sunlab_pred1 )+
 geom_point(aes(x=Datetime,y=A_Optimal...Power.DC..W.))+
 geom_point(aes(x=Datetime,y=min),col='red')

rsquared(sunlab_pred$fst, sunlab_pred$A_Optimal...Power.DC..W.) # Rsquared
  
# ridge regression with weather-----------------------------------------
  
sunlab_model <- lm(A_Optimal...Power.DC..W. ~ 
                    YDay+hour_factor+Minute+Year+Optimal_Temperature+
                    ambient_temperature+global_radiation+diffuse_radiation+
                    ultraviolet + precipitation + atmospheric_pressure,
                  data=sunlab_A)
 
#find the key values
tidy(sunlab_model)
glance(sunlab_model)

corPlot(select(sunlab_A, c("A_Optimal...Power.DC..W.","YDay","hour_factor","Minute",
               "Year","Optimal_Temperature","ambient_temperature","global_radiation",
               "diffuse_radiation","ultraviolet","precipitation","atmospheric_pressure"), -"Datetime"))


# Ridge regression 

A<- sunlab_A[,c("Year","Month","YDay","Hour","ambient_temperature","global_radiation",
                                 "diffuse_radiation","ultraviolet","wind_velocity","wind_direction",
                                 "Optimal_Temperature","hour_factor", "month_factor", 
                                  "precipitation", "atmospheric_pressure")]
x <- as.matrix(A)
y <- sunlab_A[,9]
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
# sunlab_pred <- sunlab_A[,c(1:6,9,23,24)]
sunlab_pred <- sunlab_A[,c("Datetime", "Year", "Month", "YDay","Hour","Minute", "A_Optimal...Power.DC..W.", "min", "fst")]

# test the result
test <- sample(dim( sunlab_pred)[1], dim( sunlab_pred)[1] * 0.01)
sunlab_pred1 <- sunlab_pred[test, ]

sunlab_pred$min <- as.numeric(sunlab_pred$min)
sunlab_pred$fst <- as.numeric(sunlab_pred$fst)
 
 # choose the test data group
sunlab_pred1 <- filter(sunlab_pred,Year=="2017",Month=="4")

 # the Visualization
 # min method
ggplot(sunlab_pred1)+
   geom_point(aes(x=Datetime,y=A_Optimal...Power.DC..W.), alpha=0.6)+
   geom_point(aes(x=Datetime,y=min),col='red', alpha=0.6)+
   theme_bw() 
  
 # 1st method
 ggplot( sunlab_pred1 )+
   geom_point(aes(x=Datetime,y=A_Optimal...Power.DC..W.))+
   geom_point(aes(x=Datetime,y=fst),col='red')+
   theme_bw()
 
ggplot(sunlab_pred1) +
  geom_line(aes(x=Datetime,y=A_Optimal...Power.DC..W.), linetype="dotted", alpha=1) +
  geom_line(aes(x=Datetime,y=fst),col='red', alpha=1) +
  facet_zoom(x = Datetime > as.Date("2017-04-27") & Datetime < as.Date("2017-05-01"),
              horizontal = FALSE, zoom.size = 0.6)
 
# sunlab_A %>% filter(Datetime > as_date("2017-04-01") & Datetime < as_date("2017-04-05")) %>%
#   select(A_Optimal...Power.DC..W., Datetime) %>%
#   ggplot(aes(x=Datetime, y=A_Optimal...Power.DC..W.)) + geom_line()

 
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
                   data=sunlab_R, K = seq(0, 0.1, 0.01), scaling = "sc")

# plot(mod)
 
# rstats1(mod)

pred2 <-  predict(mod)

# choose the test data group
pred2 <- filter(pred2, Year=="2017",Month=="1")

pred2

filter# the Visualization
# min method
ggplot(pred2)+
  geom_point(aes(x=Datetime,y=A_Optimal...Power.DC..W.), alpha=0.6)+
  geom_point(aes(x=Datetime,y=fst),col='red', alpha=0.6)+
  theme_bw()

ggplot(pred2)+
  geom_point()+
  theme_bw()







