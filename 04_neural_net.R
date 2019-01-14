# Neural network try 1 -------------------------------------------------------

sunlab_pv <-read.csv("data/sunlab-faro-pv-2017.csv", stringsAsFactors = F,sep=';')
sunlab_pv$Datetime <- ymd_hms(sunlab_pv$Datetime)
sunlab_pv$Date <- as_date(sunlab_pv$Datetime)
sunlab_pv$Year <- year(sunlab_pv$Datetime)
sunlab_pv$Month <- month(sunlab_pv$Datetime)
sunlab_pv$MDay <- mday(sunlab_pv$Datetime)
sunlab_pv$Hour <- hour(sunlab_pv$Datetime)
sunlab_pv$Minute <- minute(sunlab_pv$Datetime)
sunlab_pv <- sunlab_pv[,c(1,26:31,2:25)]
sunlab_pv$YDay <- yday(sunlab_pv$Datetime)
sunlab_meteo <-read.csv("data/sunlab-faro-meteo-2017.csv", stringsAsFactors = F,sep=';')
sunlab_meteo$Datetime <- ymd_hms(sunlab_meteo$Datetime)
sunlab_all <- left_join(sunlab_pv ,sunlab_meteo,by=c("Datetime"="Datetime"))
colnames(sunlab_all)[33] <- "ambient_temperature"
colnames(sunlab_all)[34] <- "global_radiatione"
colnames(sunlab_all)[35] <- "diffuse_radiation"
colnames(sunlab_all)[36] <- "ultraviolet"
colnames(sunlab_all)[37] <- "wind_velocity"
colnames(sunlab_all)[38] <- "wind_direction"
colnames(sunlab_all)[39] <- "precipitation"
colnames(sunlab_all)[40] <- "atmospheric_pressure"
sunlab_neural<-sunlab_all
sunlab_neural$Hour <- as.numeric(sunlab_neural$Hour)
sunlab_neural$hour_factor<-sin(((sunlab_neural$Hour-5)/14)*2*pi-0.5*pi)
sunlab_neural$YDay <- as.numeric(sunlab_neural$YDay)
sunlab_neural$YDay<-cos(((sunlab_neural$YDay-95)/365)*2*pi)
sunlab_neural$Month <- as.numeric(sunlab_neural$Month)
sunlab_neural$month_factor1<-sin(((sunlab_neural$Month)/12-0.125)*2*pi)
sunlab_neural$month_factor2<-cos(((sunlab_neural$Month)/12-0.125)*2*pi) 
sunlab_neural$month_factor<-apply(sunlab_neural[,c(42,43)],1,sum,na.rm=T)
sunlab_neural$ambient_temperature  <- as.numeric(sunlab_neural$ambient_temperature )
sunlab_neural$global_radiatione<-sin((sunlab_neural$global_radiatione/1050)*0.5*pi)
sunlab_neural$diffuse_radiation <- as.numeric(sunlab_neural$diffuse_radiation ) 
sunlab_neural$ultraviolet<-sin((sunlab_neural$ultraviolet/66)*0.5*pi) 
sunlab_all$wind_velocity <- sunlab_all$wind_velocity*5
sunlab_all$wind_direction <- sunlab_all$wind_direction/10
sunlab_neural$precipitation <- as.numeric(sunlab_neural$precipitation)
sunlab_neural$atmospheric_pressure <- (sunlab_neural$atmospheric_pressure-1000)

# normalise function
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
sunlab_neural <- drop_na(sunlab_neural)
sunlab_neural <- filter(sunlab_neural, Minute=="0"| Minute=="20"|Minute=="40" )
# |Minute=="10"|Minute=="30"|Minute=="50" # twice data size will makes the process extremely slow
sunlab_norm<-sunlab_neural[,c(44,32,41,7,33:40,13)]
# Datetime<-sunlab_neural[,c(1)]
# normalise the data
sunlab_norm <- as.data.frame(lapply(sunlab_norm,normalise))
# sunlab_norm$Datetime<-Datetime
# sunlab_norm <- filter(sunlab_norm,Minute=="0" ) # to reduce the size of data
# sunlab_norm<-sunlab_norm [,-c(4)]


test_sample <- sample(dim(sunlab_norm)[1], dim(sunlab_norm)[1] * 0.3)
sunlab_test <- sunlab_norm[test_sample, ]
sunlab_train <- sunlab_norm[-test_sample, ]
f <- reformulate(names(sunlab_norm[,1:12]),response = "A_Optimal...Power.DC..W.")

#### Do with new test sample
sunlab_A_nn_train <- filter(sunlab_A_train, Minute=="0")
sunlab_A_nn_train <- sunlab_A_nn[,c(2,4,10:16,18,21,9)]
sunlab_A_nn_train <- as.data.frame(lapply(sunlab_A_nn,normalise))
sunlab_A_nn_train <- drop_na(sunlab_A_nn)
f <- reformulate(names(sunlab_A_nn[,1:11]),response = "A_Optimal...Power.DC..W.")

# stepmax need to be enlarged here 
# reference running time: 18-25 min for desktop
neural_network <- neuralnet(formula = f,
                            data = sunlab_A_nn_train,
                            stepmax = 1e+08,
                            hidden = c(6,4),
                            linear.output=T)
# plot(neural_network,fontsize = 8)
sunlab_A_test$nn <- compute(neural_network,sunlab_A_test[,1:12])$net.result
rsquared(sunlab_A_test$nn, sunlab_test$A_Optimal...Power.DC..W.)  # the calculation of R square
# 0.9129906844 no wind -> 0.8976351618


####### test for a random month (the comparison plot is obvious for concentrated data)

neural_test1 <- sunlab_neural
datatime<-neural_test1[,c(1)]
Month<-neural_test1[,c(4)]
neural_test1<-neural_test1[,c(44,32,41,7,33:40,13)]
neural_test1 <- as.data.frame(lapply(neural_test1,normalise))
neural_test1$Datetime <-datatime 
neural_test1$Month <-Month 
# neural_test1 <- filter(neural_test1,Minute=="0")
# neural_test1<-neural_test1 [,-c(4)]
pred1 <- compute(neural_network,neural_test1[,1:12])$net.result
neural_test1$pred1 <- pred1
neural_test1$pred1<-as.numeric(neural_test1$pred1)
neural_test_month <- filter(neural_test1,Month=="10")

rsquared(pred1, neural_test1$A_Optimal...Power.DC..W.)

# Zoom Plot
ggplot( neural_test_month )+
  geom_line(aes(x=Datetime,y=A_Optimal...Power.DC..W., group=1, alpha=0.5))+
  geom_line(aes(x=Datetime,y=pred1),col='red', alpha=0.5) + theme_bw() + 
  facet_zoom(x=Datetime >as.Date("2017-10-12") & Datetime < as.Date("2017-10-17"))

# Neural network try 2  -------------------------------------------

# Normalise all data
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#### Get initial data and filter it
sunlab_A_nn <- filter(sunlab_A, Minute=="10"| Minute=="20"| Minute=="30"| Minute=="40" |Minute=="50" |Minute=="0" )
norm_max <- max(sunlab_A_nn$A_Optimal...Power.DC..W.)
norm_min <- min(sunlab_A_nn$A_Optimal...Power.DC..W.)
sunlab_A_nn %<>% select(c("Year","YDay","time_factor","Month", "month_factor","A_Optimal...Power.DC..W.","Optimal_Temperature",
                      "ambient_temperature","global_radiation","diffuse_radiation","ultraviolet","wind_velocity",
                      "wind_direction"))
# Copy out year and month to filter later
sunlab_A_nn_Year<-sunlab_A_nn[,c(1)]
sunlab_A_nn_Month<-sunlab_A_nn[,c(4)]

# Normalise everything
sunlab_A_nn <- as.data.frame(lapply(sunlab_A_nn,normalise))

# Add back in year and month
sunlab_A_nn$year_t <-sunlab_A_nn_Year
sunlab_A_nn$month_t <-sunlab_A_nn_Month 

# Select Training and test data
sunlab_A_nn_test<-filter(sunlab_A_nn,month_t>9 & year_t=="2017")
sunlab_A_nn_train <- filter(sunlab_A_nn,month_t<10&year_t=="2017"|year_t<"2017")

# Remove Month, month_t and year_t
sunlab_A_nn_train <- sunlab_A_nn_train[,-c(4,14,15)]
sunlab_A_nn_test <- sunlab_A_nn_test[,-c(4,14,15)]

# Reorder variables so power is at the end
sunlab_A_nn_train <- sunlab_A_nn_train[,c(1:4, 6:12, 5)]
sunlab_A_nn_test <- sunlab_A_nn_test[,c(1:4, 6:12, 5)]


# sunlab_A_nn_train <- as.data.frame(lapply(sunlab_A_nn_train,normalise))
# sunlab_A_nn_test <- as.data.frame(lapply(sunlab_A_nn_test,normalise))
# Set target data
trainingtarget <- sunlab_A_nn_train$A_Optimal...Power.DC..W.
testtarget <- sunlab_A_nn_test$A_Optimal...Power.DC..W.

# Turn to matrix form
train_m <- as.matrix(sunlab_A_nn_train)
test_m <- as.matrix(sunlab_A_nn_test)

# Create the model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 100, activation = 'relu', input_shape= c(12)) %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units = 100, activation = 'relu') %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units = 100, activation = 'relu') %>%
  layer_dense(units = 1)

# Compile
model %>% compile(loss = 'mse',
                  optimizer = optimizer_rmsprop(lr=0.001),
                  metric = 'mae')

# Fit model
mymodel <- model %>% 
  fit(train_m, 
      trainingtarget,
      epochs = 5,
      batch_size = 32,
      validation_split = 0.2)

# Evaluate model
model %>% evaluate(test_m, testtarget)
nn <- model %>% predict(sunlab_A_nn_test)
rsquared(nn, sunlab_A_nn_test$A_Optimal...Power.DC..W.)
mape(nn, sunlab_A_nn_test$A_Optimal...Power.DC..W.)

# Unnormalise for power output
unnormalise <- function(x){
  # return((norm_max - norm_min)*x + norm_min)
  return(370*x)
}

nn2 <- as.data.frame(lapply(nn, unnormalise))
sunlab_A_nn_test_2 <- filter(sunlab_A, Month>9 &Year=="2017" & (Minute=="10"| Minute=="20"| Minute=="30"| Minute=="40" |Minute=="50" |Minute=="0"))
sunlab_A_nn_test_2$nn <- t(nn2)
sunlab_A_test$nn <- t(nn2)

sunlab_A_nn_test_2 %>% 
  filter(Datetime> as_date("2017-10-01") & Datetime < as_date("2017-11-01")) %>%
  gather(type,value, A_Optimal...Power.DC..W., nn) %>%
  ggplot(aes(x=Datetime,y=value, group=type)) + geom_line(aes(linetype=type, color=type), alpha=0.5) + 
  geom_point(aes(color=type), size=0.2) +
  facet_zoom(x = Datetime > as.Date("2017-10-24") & Datetime < as.Date("2017-10-28"), horizontal = FALSE, zoom.size = 0.6)+
  scale_color_manual(name="Data", values=c("black", "red"), labels=c("Actual", "Predicted")) + 
  scale_linetype_manual(name="Data", values=c("solid", "dotted"), labels=c("Actual", "Predicted")) + 
  labs(x="Date, 2017", y="Power (W)") + theme_bw() + theme(legend.position = "bottom")


write_csv(sunlab_A_nn_test_2, "nn.csv")

