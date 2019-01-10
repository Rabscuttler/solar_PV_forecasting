
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
 
sunlab_A <- filter(sunlab_A,Minute=="10"| Minute=="20"| Minute=="30"| Minute=="40" |Minute=="50" |Minute=="0" )

sunlab_A$Hour <- as.numeric(sunlab_A$Hour)
max(sunlab_A$Hour)
min(sunlab_A$Hour)

# Linearise the data
sunlab_A$hour_factor<-sin(((sunlab_A$Hour-5)/14)*2*pi-0.5*pi)
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
sunlab_A$wind_velocity <- round(sunlab_A$wind_velocity*5)
sunlab_A$wind_direction <- round(sunlab_A$wind_direction/10)
sunlab_A$precipitation <- round(sunlab_A$precipitation)
sunlab_A$atmospheric_pressure <- (sunlab_A$atmospheric_pressure-1000)
 
setnames(sunlab_A, old="A_Optimal...Temperature..ºC.", new="Optimal_Temperature")
sunlab_A <- drop_na(sunlab_A)
  
 sunlab_model <- lm(A_Optimal...Power.DC..W. ~ 
                      month_factor+YDay+hour_factor+Minute+Year+Optimal_Temperature+
                      ambient_temperature+global_radiation+diffuse_radiation+
                      ultraviolet + precipitation + atmospheric_pressure,
                    data=sunlab_A)
 
 #find the key values
 mod_output <- tidy(sunlab_model)
 mod_output
 mod_output$p.value<.05
 glance(sunlab_model)
 
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
 sunlab_pred <- sunlab_A[,c(1:6,9,23,24)]
 
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
  facet_zoom(x = Datetime > as.Date("2017-04-11") & Datetime < as.Date("2017-04-13"),
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



