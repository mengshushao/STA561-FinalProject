setwd("C:/MS_Biostatistics/2015Fall/561/Final Project")
data<-read.table("wdbc.data",sep = ",",header=FALSE)
sum(data$V2=="M")
names(data)<-c("id","diagnosis",
               "radius_m","texture_m","perimeter_m","area_m",
               "smoothness_m","compactness_m","concavity_m",
               "concave_points_m","symmetry_m","fractal_m",
               "radius_sd","texture_sd","perimeter_sd",
               "area_sd","smoothness_sd","compactness_sd",
               "concavity_sd","concave_points_sd","symmetry_sd",
               "fractal_sd","radius_w","texture_w","perimeter_w",
               "area_w","smoothness_w","compactness_w",
               "concavity_w","concave_points_w","symmetry_w","fractal_w")
cor <-cor(data[-c(1,2)])

########################################
# this chunck of code is for logistic, #
# QDA and LDA, 10-fold cross validation#
########################################
data$dia_code=ifelse(data$diagnosis=="M",1,0)

#variable selection 

cor(data[,-c(1,2)])  #linear correlation 
#area_m,perimeter_m,radius_m, area_w,perimeter_w,radius_w highly correlated ==> choose area_m
#area_sd, perimeter_sd,radius_sd==>choose area_sd
variable=c("dia_code","texture_m","area_m","smoothness_m","compactness_m","concavity_m","concave_points_m","symmetry_m","fractal_m","texture_sd","radius_sd","smoothness_sd","compactness_sd","concavity_sd","concave_points_sd","symmetry_sd","fractal_sd","texture_w","smoothness_w","compactness_w","concavity_w","concave_points_w","symmetry_w","fractal_w")

train<-data[variable]



#stepwise selection

train.null<- glm(dia_code~1, data =train, family = "binomial")
train.full<-glm(dia_code~.,data=train,family="binomial")
step(train.null, scope = list(upper=train.full), data=train, direction="both") 
#result of stepwise selection 
fit1<-glm(formula = dia_code ~ concave_points_w + area_m + texture_w + 
            radius_sd + compactness_sd + symmetry_w + smoothness_w + 
            concavity_w + texture_sd, family = "binomial", data = train)



summary(fit1)


#logistic 

match_logi=0

set.seed(1234)



for(i in 1:10){
  
  random=sample(nrow(train),57)
  
  fit2<-glm(formula = dia_code ~ concave_points_w + area_m + texture_w + 
              radius_sd + compactness_sd +  smoothness_w + 
              concavity_w + texture_sd, family = "binomial", data = train[-random,])
  
  pred=predict(fit2,newdata=train[random,],type="response")
  a=ifelse(pred>0.5,1,0)
  truea=a==data$dia_code[random]
  
  match_logi=match_logi+sum(truea)
  
}

match_logi/570
match_logi 

#qda 

library(MASS)

set.seed(1234)

match_qda=0

train=data.frame(train)

for (i in 1:10){
  
  random=sample(nrow(train),57)
  
  
  
  train.qda=qda(dia_code~concave_points_w + area_m + texture_w + 
                  radius_sd + compactness_sd +  smoothness_w + 
                  concavity_w + texture_sd, data=train[-random,])# how to randomly pick obs
  p_class=predict(train.qda,newdata=train[random,c("concave_points_w","area_m","texture_w","radius_sd", "compactness_sd","smoothness_w", "concavity_w","texture_sd")])$class
  b=p_class==train$dia_code[random]
  match_qda=match_qda+sum(b)
  
}


plot(train.lda)
plot(train.qda)


match_qda/570 

#lda

set.seed(1234)

match_lda=0

for (i in 1:10){
  
  random=sample(nrow(train),57)
  
  
  
  train.lda=lda(dia_code~ concave_points_w + area_m + texture_w + 
                  radius_sd + compactness_sd +  smoothness_w + 
                  concavity_w + texture_sd, data=train[-random,])# how to randomly pick obs
  p_class=predict(train.lda,newdata=train[random,c("concave_points_w","area_m","texture_w","radius_sd", "compactness_sd","smoothness_w", "concavity_w","texture_sd")])$class
  c=p_class==train$dia_code[random]# what is this name for 
  match_lda=match_lda+sum(c)
  
}

match_lda/570  

#####################################
# this chunck of code is for LASSO, #
# and Ridge regression              #
#####################################
# Lasso regression
for (i in 1:dim(data)[1]){
  if (data$diagnosis[i] == "M") {data$status[i] = 1} else {data$status[i] = 0}
}
predictors = as.matrix(data[,-c(1,2,33)])
predictors = scale(predictors)
outcome = as.factor(data[,33])
require("glmnet")
set.seed(1234)
cv.lassomodel = cv.glmnet(x=predictors,y=as.numeric(data[,33]),alpha=1,nfolds=10,
                          family="binomial",type.measure="class")
best_lambda <- cv.lassomodel$lambda.min
best_lambda
coef(cv.lassomodel,s=best_lambda)
min(cv.lassomodel$cvm)

# ridge regression
set.seed(1234)
predictors2 = data.frame(data[,c("texture_m","concavity_m","concave_points_m",
                                 "fractal_m","radius_sd","texture_sd",
                                 "smoothness_sd","compactness_sd","fractal_sd",
                                 "radius_w","texture_w","smoothness_w","concavity_w",
                                 "concave_points_w","symmetry_w")])
ridgemodel= cv.glmnet(x=as.matrix(predictors2),
                      y=as.numeric(data[,33]),nfolds=10,
                      family="binomial",alpha=0,type.measure="class")
plot(ridgemodel)
ridgemodel$lambda.min
coef=coef(ridgemodel,s="lambda.min")
min(ridgemodel$cvm)

#############################
# this chunck of code is for#
# support vector machine    #
#############################
install.packages("e1071")
library(e1071)
cost = seq(0.1,1,by=0.1)
accuracy = rep(NA,10)
for (i in 1:10)
{
  svm.fit<-svm(diagnosis ~ concave_points_w + area_m + texture_w + 
                 radius_sd + compactness_sd +  smoothness_w + 
                 concavity_w + texture_sd, data = data, cross=10, cost = cost[i])
  accuracy[i] = svm.fit$tot.accuracy
}


