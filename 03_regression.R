
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
  
# LM and ridge regression with weather-----------------------------------------
sunlab_model <- lm(A_Optimal...Power.DC..W. ~ 
                    YDay+hour_factor+Minute+Year+Optimal_Temperature+
                    ambient_temperature+global_radiation+diffuse_radiation+ ultraviolet + 
                     precipitation + atmospheric_pressure, data=sunlab_A)
tidy(sunlab_model) #find the key values
glance(sunlab_model)
corPlot(select(sunlab_A, c("A_Optimal...Power.DC..W.","YDay","hour_factor","Minute",
               "Year","Optimal_Temperature","ambient_temperature","global_radiation",
               "diffuse_radiation","ultraviolet","precipitation","atmospheric_pressure"), -"Datetime"))

# Ridge regression 
A<- sunlab_A[,c("Year","Month","YDay","Hour","ambient_temperature","global_radiation",
                "diffuse_radiation","ultraviolet","wind_velocity","wind_direction",
                "Optimal_Temperature","hour_factor", "month_factor", "precipitation", "atmospheric_pressure")]
x <- as.matrix(A)
y <- sunlab_A[,"A_Optimal...Power.DC..W."]
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
sunlab_A$min <- predict(cvfit,x,s="lambda.min")
sunlab_A$fst <- predict(cvfit,x,s="lambda.1se")
 
# Zoom plot - remember can choose between min and fst
sunlab_A %>% filter( Year=="2017", Month=="4") %>% 
  ggplot() + geom_line(aes(x=Datetime,y=A_Optimal...Power.DC..W.), linetype="dotted", alpha=1) +
  geom_line(aes(x=Datetime,y=min),col='red', alpha=1) +
  facet_zoom(x = Datetime > as.Date("2017-04-27") & Datetime < as.Date("2017-05-01"), horizontal = FALSE, zoom.size = 0.6)
 
rsquared(sunlab_A$min, sunlab_A$A_Optimal...Power.DC..W.) # again pick min or fst


# SVR ---------------------------------------------------------------------
library(e1071)
library(readr)

regressor = svm(formula = A_Optimal...Power.DC..W. ~ 
                 month_factor+YDay+hour_factor+Minute+Year+Optimal_Temperature+
                 ambient_temperature+global_radiation+diffuse_radiation+
                 ultraviolet+wind_velocity +wind_direction+precipitation+atmospheric_pressure,
               data = sunlab_A,
               type = 'eps-regression',
               kernel = 'radial')

sunlab_A$SVM <- predict(regressor, newdata = sunlab_A)
rsquared(sunlab_A$SVM, sunlab_A$A_Optimal...Power.DC..W.) # Get rsquared

sunlab_1 <- filter(sunlab_A,Year=="2017",Month=="1")
ggplot(sunlab_1) +
  geom_point(aes(x=Datetime,y=A_Optimal...Power.DC..W.))+
  geom_point(aes(x=Datetime,y=predict(regressor, newdata = sunlab_1)),col='red')+
  theme_bw()

sunlab_A %>% filter( Year=="2017", Month=="1") %>% select(Datetime, A_Optimal...Power.DC..W.,SVM) %>%
  gather(type,value, A_Optimal...Power.DC..W.,SVM) %>%
  ggplot(aes(x=Datetime,y=value, group=type)) + geom_line(aes(linetype=type, color=type)) + 
  geom_point(aes(color=type), size=0.2) +
  facet_zoom(x = Datetime > as.Date("2017-01-24") & Datetime < as.Date("2017-01-28"), horizontal = FALSE, zoom.size = 0.6)+
  scale_color_manual(name="Data", values=c("black", "red"), labels=c("Actual", "Predicted")) + 
  scale_linetype_manual(name="Data", values=c("solid", "dotted"), labels=c("Actual", "Predicted")) + 
  labs(x="Date, 2017", y="Power (W)") + theme(legend.position = "bottom")

