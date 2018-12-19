
# Neural network ----------------------------------------------------------

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


# neutral network try2 ----------------------------------------------------

attach(iris)
network<-neuralnet(A_Optimal...Power.DC..W.~names(sunlab_small[,1:3]),
                   sunlab_train,hidden = 3)
network

