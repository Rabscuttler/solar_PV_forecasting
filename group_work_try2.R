
library(tidyverse)
library(corrplot)
library(lubridate)
library(neuralnet)

library(broom)
library(forcats)
library(modelr)
library(lattice)
library(caret)
library(glmnet)

library(psych)
library(ggExtra)

sunlab_pv <-read.csv("data/sunlab-faro-pv-2017.csv", stringsAsFactors = F,sep=';')
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

# data analysis -----------------------------------------------------------


# the trend of a day
ggplot(sunlab_pv)+geom_boxplot(aes(x=as.factor(Hour),y=A_Optimal...Power.DC..W.))
ggplot(sunlab_pv)+geom_boxplot(aes(x=as.factor(Hour),y=A_Optimal...Current.DC..A.))
ggplot(sunlab_pv)+geom_boxplot(aes(x=as.factor(Hour),y=A_Optimal...Voltage.DC..V.))

# Power generation can be more simlar with current production. 
ggplot(sunlab_pv)+geom_boxplot(aes(x=as.factor(Month),y=A_Optimal...Power.DC..W.))
ggplot(sunlab_pv)+geom_boxplot(aes(x=as.factor(Month),y=A_Optimal...Voltage.DC..V.))
ggplot(sunlab_pv)+geom_boxplot(aes(x=as.factor(Month),y=A_Optimal...Current.DC..A.))

# can observe that the trends of A and B are quite similar
ggplot(sunlab_pv)+geom_boxplot(aes(x=as.factor(Month),y=A_Optimal...Power.DC..W.))
ggplot(sunlab_pv)+geom_boxplot(aes(x=as.factor(Month),y=B_Optimal...Power.DC..W.))

# boxplot to explain three angles' generation condition for the trend of a year
# the difference causes by the proportion of a-Si and mc-Si
ggplot(sunlab_pv)+geom_boxplot(aes(x=as.factor(Month),y=A_Horizontal...Power.DC..W.))
ggplot(sunlab_pv)+geom_boxplot(aes(x=as.factor(Month),y=A_Vertical...Power.DC..W.))
ggplot(sunlab_pv)+geom_boxplot(aes(x=as.factor(Month),y=A_Optimal...Power.DC..W.))

sunlab_pv$sum<-apply(sunlab_pv[,c(10,16)],1,sum,na.rm=T)
ggplot(sunlab_pv)+geom_boxplot(aes(x=as.factor(Month),y=sum))

# day of year test
ggplot(sunlab_pv)+geom_boxplot(aes(x=as.factor(YDay),y=A_Optimal...Power.DC..W.))


# Neural network ----------------------------------------------------------

# data cleaning
# normalise function
# normalise <- function(x) {
#   return((x - min(x)) / (max(x) - min(x)))
# }
# sunlab_Neural <- drop_na(sunlab_pv)
# sunlab_Neural$Month <- as.numeric(sunlab_Neural$Month)
# sunlab_Neural <- filter(sunlab_Neural,Month=="1"| Month=="2")
# 
# 
# sunlab_Neural <- filter(sunlab_Neural,Minute=="10"| Minute=="20"| Minute=="30"|
#                           Minute=="40" |Minute=="50" |Minute=="0" )
# # set as normal value
# sunlab_norm <- as.data.frame(lapply(
#   select(sunlab_Neural,-c(1:3,20:31)),normalise))
# 
# sunlab_reduced<- sunlab_norm[c(1:4,7,10,13)]
# 
# # to reduce the size of data
# sunlab_small <- filter(sunlab_reduced,Minute=="0" )
# 
# 
# 
# #ȡǰʮ????????????
# #ǰ????��???????飬??????��?Ǵ˴βɼ???????
# #sample.int(length(x), size, replace, prob)
# #??1????????ѡ?ģ???Ϊÿ??��??????��??һ????
# #????ȡ??????��?ν????ǲ?һ???ģ?ֻ?Ǹ???????30%??????��
# test_sample <- sample(dim(sunlab_small)[1], dim(sunlab_small)[1] * 0.3)
# #ѡ?е???????Ϊtest??
# sunlab_test <- sunlab_reduced[test_sample, ]
# #-??ʣ??????the rest
# #ʣ?µ???????Ϊtrain??
# sunlab_train <- sunlab_reduced[-test_sample, ]
# 
# #1-7??Ϊ?Ա?��???ڰ???applice??Ϊ??Ӧ
# #???¶???Ϊf???൱??һ???߼??ع?????ʽ????????????׼????
# f <- reformulate(names(sunlab_small[,1:3]),
#                  response = "A_Optimal...Power.DC..W.")
# # Note hidden = 5!
# #stepmax???ಽ??
# neural_network <- neuralnet(formula = f,
#                             data = sunlab_train,
#                             stepmax = 1e+08,
#                             hidden = c(4,3),
#                             act.fct = "logistic")
# #active function???߼??ع?
# 
# #Ԥ?????????ĵ???
# #???㣬??????????????test??
# appliance_pred <- compute(neural_network,
#                           sunlab_test[,-3])$net.result
# head(appliance_pred)
# appliance_pred <- round(appliance_pred)
# 
# #????????
# confusion <- table(appliance_pred, volta_test$appliance)
# #׼ȷ??p
# accuracy <- sum(diag(confusion))/sum(confusion)
# 
# #??????????????????ÿ????��?????ӣ?????Խ��ԽС
# #???????ӣ?????ʱ?????ӣ?׼ȷ??????
# #active function?????ı?׼ȷ??̫??
# plot(neural_network)
# 
# 
# # neutral network try2 ----------------------------------------------------
# 
#  attach(iris)
#  network<-neuralnet(A_Optimal...Power.DC..W.~names(sunlab_small[,1:3]),
#                     sunlab_train,hidden = 3)
#  network



# ridge regression without weather-----------------------------------------

 # data cleaning
 file_number<-matrix(0,ncol=4,nrow=1)
 df<-data.frame()
 for(i in seq_along(file_number)){
   k<-(i+2013)
   file_name=paste("data/sunlab-faro-pv-",k,".csv",sep="")
   data<-read.csv(file=file_name, stringsAsFactors = FALSE, sep=';')
   df<-rbind(df,data)
 }
 
 sunlab_A <-drop_na(df)
 sunlab_A <-  sunlab_A[,-c(14:25)]

 sunlab_A$Datetime <- ymd_hms(sunlab_A$Datetime)
 sunlab_A$Year <- year(sunlab_A$Datetime)
 sunlab_A$Month <- month(sunlab_A$Datetime, label = TRUE)
 sunlab_A$MDay <- mday(sunlab_A$Datetime)
 sunlab_A$Hour <- hour(sunlab_A$Datetime)
 sunlab_A$Minute <- minute(sunlab_A$Datetime)
 sunlab_A <- sunlab_A[,c(1,14:18,2:13)]
 
 sunlab_A$Month <- as.numeric(sunlab_A$Month)
 sunlab_A <- filter(sunlab_A,Minute=="10"| Minute=="20"| Minute=="30"|
                      Minute=="40" |Minute=="50" |Minute=="0" )


 sunlab_model <- lm(A_Optimal...Power.DC..W. ~ Month+MDay+Hour+Minute+Year,
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
 
 # data cleaning
 file_number<-matrix(0,ncol=4,nrow=1)
 df<-data.frame()
 for(i in seq_along(file_number)){
   k<-(i+2013)
   file_name=paste("data/sunlab-faro-pv-",k,".csv",sep="")
   data<-read.csv(file=file_name, stringsAsFactors = FALSE, sep=';')
   df<-rbind(df,data)
 }
 
 
 sunlab_B <- drop_na(df)
 sunlab_B <- sunlab_B[,-c(2:13)] 
 sunlab_B$Datetime <- ymd_hms(sunlab_B$Datetime)
 
 ggplot(sunlab_B, aes(x=Datetime, y=B_Optimal...Power.DC..W., group=1)) +
   geom_line(linetype="dotted")
 
 
 sunlab_A <- drop_na(df)
 sunlab_A <- sunlab_A[,-c(14:25)]
 
 sunlab_A$Datetime <- ymd_hms(sunlab_A$Datetime)
 sunlab_A$Year <- year(sunlab_A$Datetime)
 sunlab_A$Month <- month(sunlab_A$Datetime, label = TRUE)
 sunlab_A$MDay <- mday(sunlab_A$Datetime)
 sunlab_A$Hour <- hour(sunlab_A$Datetime)
 sunlab_A$Minute <- minute(sunlab_A$Datetime)
 sunlab_A <- sunlab_A[,c(1,14:18,2:13)]
 
 # sunlab_A$YDay <- yday(sunlab_A$Datetime)
 
 
 # BOD1$Time <- factor(BOD1$Time) 
 ggplot(sunlab_A, aes(x=Datetime, y=A_Optimal...Power.DC..W., group=1)) +
   geom_line(linetype="dotted")
 
 # four years total average generation varying with month
 ggplot(sunlab_A)+geom_boxplot(aes(x=as.factor(Month),y=A_Optimal...Power.DC..W.))
 # four years total average generation varying with hour
 ggplot(sunlab_A)+geom_boxplot(aes(x=as.factor(Hour),y=A_Optimal...Power.DC..W.))
 # four years total average generation varying with day of year
 # ggplot(sunlab_A)+geom_boxplot(aes(x=as.factor(YDay),y=A_Optimal...Power.DC..W.))
 
 
 
 # March and September got the largest value
 
 sunlab_A <- filter(sunlab_A,Minute=="10"| Minute=="20"| Minute=="30"|
                      Minute=="40" |Minute=="50" |Minute=="0" )

 sunlab_A$Hour <- as.numeric(sunlab_A$Hour)
 max(sunlab_A$Hour)
 min(sunlab_A$Hour)
 sunlab_A$hour_factor<-sin(((sunlab_A$Hour-5)/14)*pi)
 
 
 sunlab_A$Month <- as.numeric(sunlab_A$Month)
 
 sunlab_A$month_factor1<-sin(((sunlab_A$Month)/12-0.125)*2*pi)
 sunlab_A$month_factor2<-cos(((sunlab_A$Month)/12-0.125)*2*pi) 
 sunlab_A$month_factor<-apply(sunlab_A[,c(20,21)],1,sum,na.rm=T)
 
 max(sunlab_A$month_factor)
 
 
 colnames(sunlab_A)[17] <- "Optimal_Temperature"
 sunlab_model <- lm(A_Optimal...Power.DC..W. ~ 
                      month_factor+MDay+hour_factor+Minute+Year+Optimal_Temperature,
                    data=sunlab_A)
 
 #find the key values
 mod_output <- tidy(sunlab_model)
 mod_output
 mod_output$p.value<.05
 
 sunlab_ridge<- sunlab_A[,c(2,22,4,19,6,17,12)]
 x <- as.matrix( sunlab_ridge[,1:6])
 y <- as.matrix( sunlab_ridge[,7])
 #alpha=0 means ridge regression
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

 
 # predict the generation values to show the effect of min method and 
 # 1 standard error method
 s <- predict(cvfit,x,s="lambda.min")
 r <- predict(cvfit,x,s="lambda.1se")
 
 sunlab_A$min <- s
 sunlab_A$fst <- r
 sunlab_pred <- sunlab_A[,c(1:6,12,23,24)]
 
 # test the result
 # test <- sample(dim( sunlab_pred)[1], dim( sunlab_pred)[1] * 0.01)
 # sunlab_pred1 <- sunlab_pred[test, ]

 sunlab_pred$min <- as.numeric(sunlab_pred$min)
 sunlab_pred$fst <- as.numeric(sunlab_pred$fst)
 
 # choose the test data group
 sunlab_pred1 <- filter(sunlab_pred,Year=="2017",Month=="1")

 # the Visualization
 # min method
 ggplot( sunlab_pred1 )+
   geom_point(aes(x=Datetime,y=A_Optimal...Power.DC..W.))+
   geom_point(aes(x=Datetime,y=min),col='red')+
   theme_bw()
 
 tidy(sunlab_model)
 glance(sunlab_model)

  
 # 1st method
 ggplot( sunlab_pred1 )+
   geom_point(aes(x=Datetime,y=A_Optimal...Power.DC..W.))+
   geom_point(aes(x=Datetime,y=fst),col='red')+
   theme_bw()
 
 tidy(sunlab_model)
 glance(sunlab_model) 
 # after check, the min method and 1st method is similar in this situation.
 
   