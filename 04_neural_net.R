# Neural network try 1 -------------------------------------------------------

# data cleaning
# normalise function
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
sunlab_Neural <- drop_na(sunlab_pv)
sunlab_Neural$Month <- as.numeric(sunlab_Neural$Month)

# Why this step?
sunlab_Neural <- filter(sunlab_Neural,Month=="1"| Month=="2")

# Why this step?
sunlab_Neural <- filter(sunlab_Neural,Minute=="10"| Minute=="20"| Minute=="30"|
                          Minute=="40" |Minute=="50" |Minute=="0" )
# set as normal value
sunlab_norm <- as.data.frame(lapply(
  select(sunlab_Neural,-c(1:3,20:31)),normalise))

sunlab_reduced<- sunlab_norm[c(1:4,7,10,13)]

# Why?
# to reduce the size of data
sunlab_small <- filter(sunlab_reduced,Minute=="0" )

# Create training and test data
set.seed(123)
test_sample <- sample(dim(sunlab_small)[1], dim(sunlab_small)[1] * 0.3)
sunlab_test <- sunlab_reduced[test_sample, ]
sunlab_train <- sunlab_reduced[-test_sample, ]

f <- reformulate(names(sunlab_small[,1:3]),
                 response = "A_Optimal...Power.DC..W.")
# Note hidden = 5!
#stepmax???ಽ??
neural_network <- neuralnet(formula = f,
                            data = sunlab_train,
                            stepmax = 1e+08,
                            hidden = c(4,3),
                            act.fct = "logistic")


#active function???߼??ع?

nn <- compute(neural_network,sunlab_test[,-3])$net.result

head(nn)
nn <- round(nn)

#????????
confusion <- table(nn, volta_test$appliance)
#׼ȷ??p
accuracy <- sum(diag(confusion))/sum(confusion)

#??????????????????ÿ????��?????ӣ?????Խ��ԽС
#???????ӣ?????ʱ?????ӣ?׼ȷ??????
#active function?????ı?׼ȷ??̫??
plot(neural_network)



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


