# Neural network try 1 -------------------------------------------------------

sunlab_pv <-read.csv("sunlab-faro-pv-2017.csv", stringsAsFactors = F,sep=';')
sunlab_pv$Datetime <- ymd_hms(sunlab_pv$Datetime)
sunlab_pv$Date <- as_date(sunlab_pv$Datetime)
sunlab_pv$Year <- year(sunlab_pv$Datetime)
sunlab_pv$Month <- month(sunlab_pv$Datetime, label = TRUE)
#month day
sunlab_pv$MDay <- mday(sunlab_pv$Datetime)
sunlab_pv$Hour <- hour(sunlab_pv$Datetime)
sunlab_pv$Minute <- minute(sunlab_pv$Datetime)
sunlab_pv <- sunlab_pv[,c(1,26:31,2:25)]
tail(sunlab_pv)
sunlab_pv$YDay <- yday(sunlab_pv$Datetime)

sunlab_meteo <-read.csv("sunlab-faro-meteo-2017.csv", stringsAsFactors = F,sep=';')
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
# sunlab_all$wind_velocity <- round(sunlab_all$wind_velocity*5)
# sunlab_all$wind_direction <- round(sunlab_all$wind_direction/10)
sunlab_neural$precipitation <- as.numeric(sunlab_neural$precipitation)
sunlab_neural$atmospheric_pressure <- (sunlab_neural$atmospheric_pressure-1000)



# normalise function
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
sunlab_neural <- drop_na(sunlab_neural)
sunlab_neural <- filter(sunlab_neural, Minute=="0"| Minute=="20"|Minute=="40" )

# twice data size will makes the process extremely slow
# |Minute=="10"|Minute=="30"|Minute=="50"

sunlab_norm<-sunlab_neural[,c(44,32,41,7,33:40,13)]
# Datetime<-sunlab_neural[,c(1)]
# normalise the data
sunlab_norm <- as.data.frame(lapply(sunlab_norm,normalise))
# sunlab_norm$Datetime<-Datetime

# to reduce the size of data
# sunlab_norm <- filter(sunlab_norm,Minute=="0" )
# sunlab_norm<-sunlab_norm [,-c(4)]


test_sample <- sample(dim(sunlab_norm)[1], dim(sunlab_norm)[1] * 0.3)
sunlab_test <- sunlab_norm[test_sample, ]
sunlab_train <- sunlab_norm[-test_sample, ]


f <- reformulate(names(sunlab_norm[,1:12]),
                 response = "A_Optimal...Power.DC..W.")

# stepmax need to be enlarged here 
# reference running time: 18-25 min for desktop
neural_network <- neuralnet(formula = f,
                            data = sunlab_train,
                            stepmax = 1e+08,
                            hidden = c(6,4),
                            linear.output=T)

plot(neural_network)


######## for test dataset

pred <- compute(neural_network,sunlab_test[,1:12])$net.result
# the calculation of R square
mean<-0
for (i in seq_along(pred)){
  mean<- mean+sunlab_test$A_Optimal...Power.DC..W.[i]}
mean<-mean/length(pred)

tot<-0
for (i in seq_along(pred)){
  tot<- tot+(sunlab_test$A_Optimal...Power.DC..W.[i]-mean)^2}

res<-0
for (i in seq_along(pred)){
  res<- res+(sunlab_test$A_Optimal...Power.DC..W.[i]-pred[i])^2}

R_square<- (1-res/tot)
R_square


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

ggplot( neural_test_month )+
  geom_point(aes(x=Datetime,y=A_Optimal...Power.DC..W.))+
  geom_point(aes(x=Datetime,y=pred1),col='red')+
  theme_bw()

# the calculation of R square
mean1<-0
for (i in seq_along(pred1)){
  mean1<- mean1+neural_test1$A_Optimal...Power.DC..W.[i]}
mean1<-mean1/length(pred1)

tot1<-0
for (i in seq_along(pred1)){
  tot1<- tot1+(neural_test1$A_Optimal...Power.DC..W.[i]-mean1)^2}

res1<-0
for (i in seq_along(pred1)){
  res1<- res1+(neural_test1$A_Optimal...Power.DC..W.[i]-pred1[i])^2}

R_square1<- (1-res1/tot1)
R_square1



# Neural network try 2 - Tutorial with Boston Housing Data ------------------------------

# Following this tutorial:
# https://www.youtube.com/watch?v=SrQw_fWo4lw

data("BostonHousing")
boston <- BostonHousing 

# Convert all factor variables to numeric
boston %<>% mutate_if(is.factor, as.numeric)

# Neural network visualisation - we use keras to implement this later.
# Lifesign just specifies how much output the model shows

?neuralnet
n <- neuralnet(medv ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat,
               data = boston,
               hidden = c(10,5),
               linear.output = F,
               lifesign = 'full',
               rep = 1)
# We can visualise this simple neural net with two hidden layers of size 10 and 5.
plot(n)

# And again hiding weights and looking nicer
plot(n, 
     col.hidden = 'darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

# Create input matrix
boston <- as.matrix(boston)
dimnames(boston) <- NULL

# Partition
set.seed(1234) # Set seed so our results will be the same each time after sampling
ind <- sample(2, nrow(boston), replace=T, prob=c(.7, .3))
training <- boston[ind==1, 1:13]
test <- boston[ind==2, 1:13]
trainingtarget <- boston[ind==1, 14]
testtarget <- boston[ind==2, 14]

# Normalize = (Value - mean) / standard deviation
m <- colMeans(training)
s <- apply(training, 2, sd) 
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)  

# Create the model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 5, activation = 'relu', input_shape= c(13)) %>%
  layer_dense(units = 1)

# Compile the model. 
# Use mean absolute error (mae). 
# We can try varying learning rate if desired with optimizer_rmsprop(lr=0.001)
model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metric = 'mae')

# Fit model
mymodel <- model %>% 
  fit(training, 
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

# Evaluate model
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred)^2) # Same as mse
plot(testtarget, pred) # Plot predictions and data - want diagonal line for perfect predictions

# Fine-tune model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 10, activation = 'relu', input_shape= c(13)) %>%
  layer_dense(units = 5, activation = 'relu') %>%
  layer_dense(units = 1)
summary(model)

# Then compile and fit again
 
# Fine-tune model again
model <- keras_model_sequential()
model %>%
  layer_dense(units = 100, activation = 'relu', input_shape= c(13)) %>%
  layer_dropout(rate=0.4) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate=0.3) %>%
  layer_dense(units = 20, activation = 'relu') %>%
  layer_dropout(rate=0.2) %>%
  layer_dense(units = 1)

# Compile and fit again
# Results much better after the third fine-tuning!


# Neural network try 3 - sunlab -------------------------------------------

# Reduce the data size
sun_nn <- sunlab_A %>% 
          # filter(Year==2017) %>%
          filter(Minute=="10"| Minute=="20"| Minute=="30"| Minute=="40" |Minute=="50" |Minute=="0" ) %>%
          select(Year, Month, YDay, Hour, Minute, A_Optimal...Temperature..ºC., ambient_temperature, 
                 global_radiation, diffuse_radiation, ultraviolet, A_Optimal...Power.DC..W.) %>%
          drop_na()
sun_nn <- as.matrix(sun_nn)
dimnames(sun_nn) <- NULL

# Partition
set.seed(1234) # Set seed so our results will be the same each time after sampling
ind <- sample(2, nrow(sun_nn), replace=T, prob=c(.7, .3))
training <- sun_nn[ind==1, 1:10]
test <- sun_nn[ind==2, 1:10]
trainingtarget <- sun_nn[ind==1, 11]
testtarget <- sun_nn[ind==2, 11]

# Normalize = (Value - mean) / standard deviation
m <- colMeans(training)
s <- apply(training, 2, sd) 
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)  

# Create the model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 100, activation = 'relu', input_shape= c(10)) %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units = 100, activation = 'relu') %>%
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
  fit(training, 
      trainingtarget,
      epochs = 25,
      batch_size = 32,
      validation_split = 0.2)

# Evaluate model
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
plot(testtarget, pred) # Plot predictions and data - want diagonal line for perfect predictions

?predict


