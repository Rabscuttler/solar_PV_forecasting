
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

# MAPE function
mape <- function(pred, test){
  tot <- 0
  for (i in seq_along(pred)){
    tot <- tot + abs((test[i]-pred[i])/test[i])
  }
  result <- (1*tot)/length(pred)
  return(result)
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
sunlab_A$ambient_temperature  <- as.numeric(sunlab_A$ambient_temperature )
sunlab_A$global_radiation<-sin((sunlab_A$global_radiation/1050)*0.5*pi)
sunlab_A$diffuse_radiation <- as.numeric(sunlab_A$diffuse_radiation ) 
sunlab_A$ultraviolet<-sin((sunlab_A$ultraviolet/66)*0.5*pi) 
sunlab_A$wind_velocity <- sunlab_A$wind_velocity
sunlab_A$wind_direction <- sunlab_A$wind_direction
sunlab_A$precipitation <- sunlab_A$precipitation
sunlab_A$atmospheric_pressure <- (sunlab_A$atmospheric_pressure-1000)
setnames(sunlab_A, old="A_Optimal...Temperature..ºC.", new="Optimal_Temperature")
sunlab_A <- drop_na(sunlab_A)

# Training and test data
sunlab_A_test<-filter(sunlab_A,Month>9&Year=="2017")
sunlab_A_train <- filter(sunlab_A,Month<10&Year=="2017"|Year=="2016"|Year=="2015"|Year=="2014")

# LM and ridge regression without weather-----------------------------------------
############# Simple Linear Model
sunlab_model <- lm(A_Optimal...Power.DC..W. ~ Month+YDay+hour_factor+Year, data=sunlab_A_train)
tidy(sunlab_model) # find the key values
glance(sunlab_model) # rsquared 0.667

lm_no_weather <- as_tibble(predict(sunlab_model, newdata = sunlab_A_test))

rsquared(lm_no_weather$value, sunlab_A_test$A_Optimal...Power.DC..W.) # rsquared 0.365 (on test data)
mape(lm_no_weather$value, sunlab_A_test$A_Optimal...Power.DC..W.) # MAPE 2.97
mape2(lm_no_weather$value, sunlab_A_test$A_Optimal...Power.DC..W.)

############ Ridge Regression
x <- as.matrix( sunlab_A_train[,c("Year", "Month", "YDay","time_factor")])
y <- as.matrix( sunlab_A_train[,"A_Optimal...Power.DC..W."])
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
sunlab_A_test$min <- predict(cvfit, as.matrix(sunlab_A_test[,c("Year", "Month", "YDay","time_factor")]), 
                                                      s="lambda.min")
sunlab_A_test$fst <- predict(cvfit, as.matrix(sunlab_A_test[,c("Year", "Month", "YDay","time_factor")]), 
                             s="lambda.1se")

sunlab_A_test$min <- as.numeric(sunlab_A_test$min)
sunlab_A_test$fst <- as.numeric(sunlab_A_test$fst)

ggplot(filter(sunlab_A_test,Year=="2017",Month=="10"))+
 geom_point(aes(x=Datetime,y=A_Optimal...Power.DC..W.))+
 geom_point(aes(x=Datetime,y=min),col='red')

rsquared(sunlab_A_test$fst, sunlab_A_test$A_Optimal...Power.DC..W.) # Rsquared 0.523
mape(sunlab_A_test$fst, sunlab_A_test$A_Optimal...Power.DC..W.) # mape 2.28
  
# LM and ridge regression with weather-----------------------------------------

######### Linear model
sunlab_model <- lm(A_Optimal...Power.DC..W. ~ 
                    YDay+hour_factor+Minute+Year+Optimal_Temperature+
                    ambient_temperature+global_radiation+diffuse_radiation+ ultraviolet + 
                     precipitation + atmospheric_pressure, data=sunlab_A_train)
tidy(sunlab_model) #find the key values
glance(sunlab_model)

pred_lm <- as_tibble(predict(sunlab_model, newdata = sunlab_A_test))
pred_lm$datetime <- sunlab_A_test$Datetime

rsquared(pred_lm$value, sunlab_A_test$A_Optimal...Power.DC..W.) #0.676
mape(pred_lm$value, sunlab_A_test$A_Optimal...Power.DC..W.) # 0.889

corPlot(select(sunlab_A, c("A_Optimal...Power.DC..W.","YDay","hour_factor","Minute",
               "Year","Optimal_Temperature","ambient_temperature","global_radiation",
               "diffuse_radiation","ultraviolet","precipitation","atmospheric_pressure"), -"Datetime"))

################### Ridge regression 
A<- sunlab_A_train[,c("Year","Month","YDay","Hour","ambient_temperature","global_radiation",
                "diffuse_radiation","ultraviolet","wind_velocity","wind_direction",
                "Optimal_Temperature","hour_factor", "month_factor", "precipitation", "atmospheric_pressure")]
B<- sunlab_A_test[,c("Year","Month","YDay","Hour","ambient_temperature","global_radiation",
                      "diffuse_radiation","ultraviolet","wind_velocity","wind_direction",
                      "Optimal_Temperature","hour_factor", "month_factor", "precipitation", "atmospheric_pressure")]
x <- as.matrix(A)
y <- sunlab_A_train[,"A_Optimal...Power.DC..W."]
x2 <- as.matrix(B)
y2 <- sunlab_A_test[,"A_Optimal...Power.DC..W."]
ridge <- glmnet(x, y, family = "gaussian", alpha = 0) # alpha=0 means ridge regression
# tidy(ridge)
# glance(ridge)
# plot(ridge,label=TRUE) # the value of punishment time as x label 
# plot(ridge,xvar="lambda",label=TRUE) # the logarithmic form of lambda as x label
cvfit = cv.glmnet(x, y) # find the optimum value
# plot(cvfit)
ridge.coef.1se <- coef(cvfit, s = "lambda.1se") # the intercept and cofficients of 1 standard error method
# round(ridge.coef.1se,2)
ridge.coef.min <- coef(cvfit, s = "lambda.min") # the intercept and cofficients of min method
# round(ridge.coef.min,2)
# predict the generation values to show the effect of min method and 1 standard error method
sunlab_A_test$min <- predict(cvfit,x2,s="lambda.min")
sunlab_A_test$fst <- predict(cvfit,x2,s="lambda.1se")
 
# Zoom plot - remember can choose between min and fst
sunlab_A_test %>% filter( Year=="2017", Month=="10") %>% 
  ggplot() + geom_line(aes(x=Datetime,y=A_Optimal...Power.DC..W.), linetype="dotted", alpha=1) +
  geom_line(aes(x=Datetime,y=min),col='red', alpha=1) +
  facet_zoom(x = Datetime > as.Date("2017-10-15") & Datetime < as.Date("2017-10-19"), horizontal = FALSE, zoom.size = 0.6)
 
rsquared(sunlab_A_test$min, sunlab_A_test$A_Optimal...Power.DC..W.) # min 0.724
mape(sunlab_A_test$min, sunlab_A_test$A_Optimal...Power.DC..W.) # min 0.685
rsquared(sunlab_A_test$fst, sunlab_A_test$A_Optimal...Power.DC..W.) # fst 0.804
mape(sunlab_A_test$fst, sunlab_A_test$A_Optimal...Power.DC..W.) # min 0.706

# SVR ---------------------------------------------------------------------
library(e1071)
library(readr)

regressor = svm(formula = A_Optimal...Power.DC..W. ~ 
                 month_factor+YDay+hour_factor+Year+Optimal_Temperature+
                 ambient_temperature+global_radiation+diffuse_radiation+
                 ultraviolet+wind_velocity +wind_direction+precipitation+atmospheric_pressure,
               data = sunlab_A_train,
               type = 'eps-regression',
               kernel = 'radial')

sunlab_A_test$SVM <- predict(regressor, newdata = sunlab_A_test)
rsquared(sunlab_A_test$SVM, sunlab_A_test$A_Optimal...Power.DC..W.) # rsquared 0.659
mape(sunlab_A_test$SVM, sunlab_A_test$A_Optimal...Power.DC..W.) # Get mape 0.817

sunlab_1 <- filter(sunlab_A,Year=="2017",Month=="1")
ggplot(sunlab_1) +
  geom_point(aes(x=Datetime,y=A_Optimal...Power.DC..W.))+
  geom_point(aes(x=Datetime,y=predict(regressor, newdata = sunlab_1)),col='red')+
  theme_bw()

sunlab_A_test %>% filter( Year=="2017", Month=="10") %>% select(Datetime, A_Optimal...Power.DC..W.,SVM) %>%
  gather(type,value, A_Optimal...Power.DC..W.,SVM) %>%
  ggplot(aes(x=Datetime,y=value, group=type)) + geom_line(aes(linetype=type, color=type), alpha=0.5) + 
  geom_point(aes(color=type), size=0.2) +
  facet_zoom(x = Datetime > as.Date("2017-10-24") & Datetime < as.Date("2017-10-28"), horizontal = FALSE, zoom.size = 0.6)+
  scale_color_manual(name="Data", values=c("black", "red"), labels=c("Actual", "Predicted")) + 
  scale_linetype_manual(name="Data", values=c("solid", "dotted"), labels=c("Actual", "Predicted")) + 
  labs(x="Date, 2017", y="Power (W)") + theme_bw() + theme(legend.position = "bottom")

