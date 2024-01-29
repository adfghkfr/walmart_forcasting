setwd("E:/")
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)  # data manipulation and visualization
library(gridExtra)
library(FactoMineR)
require("Gifi")
data<-read.csv("D:/report/train_fin.csv")
tail(data)
dim(data)
str(data)
library(ISLR)
names(data)


data$month<-match(data$Month,month.abb)
data
tail(data)

data$Date <- with(data, sprintf("%d-%02d", Year, month))
data
data1<-arrange(data,Year,month)
data1
tail(data1)
dim(data1)
library(e1071)
library(PCAmixdata)
data2<-subset(data1,select=-c(X,Month,id))
data2
sell<-data2$sold*data2$price
data2<-data.frame(sell,data2)
data2
data3<-subset(data2,select=-c(sold,price,Date))
data3
data3[is.na(data3)] <- 0
data3
dim(data2)

data3<-subset(data3,sell!="0") ##刪掉還沒開始賣的資料
dim(data3)
#HCPC
library(clustMixType)
library(factoextra)
library(FactoMineR)
library(missMDA)
library(tidyverse)
library(mice)
library(stringr)
library(clustertend)
library(NbClust)
library(factoextra)
library(ClusterR)
library(fpc)
library(clusterSim)
library(psych)
library(FactoMineR)

library(interp)
library(DataExplorer)
library(ggplot2)
library(dbscan)

##如何處理item_id?(共有3049個類別)
##先用one hot encoding處理其他類別型資料再思考item id
##比較:one hot encoding+kmeans會導致資料很大kmeans運行很慢/
##k-prototype/dbscan用於SVR預測銷售額
##one hot encoding+kmeans
##one hot encoding+dbscan
##kprototype
##famd(like pca)
##比較正確率與運算量
#write.csv(data3,"E:/write.csv")

dim(data)

#####1.k-prototype+svr
library(BBmisc)
library(clustMixType)
#原始資料
#由於item_id太過複雜，我們計算不同item_id下其銷售額的平均數來進行替代
###target encoding
mean<-data.frame(aggregate(data3[, 1], list(data3$item_id), mean))
#mean[,2]<-normalize(mean[,2],range=c(0,1))
mean
names(mean)[1]<-"item_id"
names(mean)[2]<-"itemsellmean"
variance<-data.frame(aggregate(data3[, 1], list(data3$item_id), var))
#variance[,2]<-normalize(variance[,2],range=c(0,1))
names(variance)[1]<-"item_id"
names(variance)[2]<-"itemsellvar"
variance
skew<-data.frame(aggregate(data3[, 1], list(data3$item_id), skew))
#skew[,2]<-normalize(skew[,2],range=c(0,1))
names(skew)[1]<-"item_id"
names(skew)[2]<-"itemsellskew"
skew
kur<-data.frame(aggregate(data3[, 1], list(data3$item_id),kurtosis))
#kur[,2]<-normalize(kur[,2],range=c(0,1))
names(kur)[1]<-"item_id"
names(kur)[2]<-"itemsellkur"
kur

bind<-data.frame(mean,variance[,2],skew[,2],kur[,2])
names(bind)[1]<-"item_id"
names(bind)[2]<-"itemsellmean"
names(bind)[3]<-"itemsellvar"
names(bind)[4]<-"itemsellskew"
names(bind)[5]<-"itemsellkur"
bind

kprodata<-left_join(data3,bind,by="item_id")
kprodata
kprodata<-subset(kprodata,Year>2010,select=-c(item_id))
dim(kprodata) ##1352302
str(kprodata)

library(mltools)
library(data.table)
library(cattonum)
#catto_freq(kprodata,kprodata[,3])

storemean<-data.frame(aggregate(kprodata[,1], list(kprodata$store_id), mean))
names(storemean)[1]<-"store_id"
names(storemean)[2]<-"storemean"
storevar<-data.frame(aggregate(kprodata[,1], list(kprodata$store_id),var))
names(storevar)[1]<-"store_id"
names(storevar)[2]<-"storevar"
store<-left_join(storemean,storevar,by="store_id")
store


deptmean<-data.frame(aggregate(kprodata[,1], list(kprodata$dept_id), mean))
deptmean
names(deptmean)[1]<-"dept_id"
names(deptmean)[2]<-"deptmean"
deptvar<-data.frame(aggregate(kprodata[,1], list(kprodata$dept_id),var))
deptvar
names(deptvar)[1]<-"dept_id"
names(deptvar)[2]<-"deptvar"
dept<-left_join(deptmean,deptvar,by="dept_id")
dept


catmean<-data.frame(aggregate(kprodata[,1], list(kprodata$cat_id), mean))
names(catmean)[1]<-"cat_id"
names(catmean)[2]<-"catmean"
catvar<-data.frame(aggregate(kprodata[,1], list(kprodata$cat_id),var))
names(catvar)[1]<-"cat_id"
names(catvar)[2]<-"catvar"
cat<-left_join(catmean,catvar,by="cat_id")
cat


statemean<-data.frame(aggregate(kprodata[,1], list(kprodata$state_id), mean))
names(statemean)[1]<-"state_id"
names(statemean)[2]<-"statemean"
statevar<-data.frame(aggregate(kprodata[,1], list(kprodata$state_id),var))
names(statevar)[1]<-"state_id"
names(statevar)[2]<-"statevar"
state<-left_join(statemean,statevar,by="state_id")
state

kpro<-left_join(kprodata,store,by="store_id")
kpro1<-left_join(kpro,dept,by="dept_id")
kpro2<-left_join(kpro1,cat,by="cat_id")
kprodata<-left_join(kpro2,state,by="state_id")
kprodata



library(plyr)
###預測模型的test資料集test###
###2016年2月到2016年4月###
test1<-subset(kprodata,Year==2016 & month>=2)
test<-test1[,-(3:6)]
test
tail(test)
dim(test) ##85361筆

traino<-subset(kprodata,Year<2016)
dim(traino) ##1239469
trainon<-subset(kprodata,Year==2016 & month==1)
dim(trainon) ##27472
trainty<-rbind(traino,trainon)
dim(trainty) ##1266941
###預測模型的train資料集trainty###
###2013年1月到2016年1月###

##連同test data一起kmeans##
#kproone<-one_hot(as.data.table(kprodata))
#clus1<-kproto(kproone,6)
#clus2<-clus1$cluster
#link<-data.frame(clus2,kproone)

#test1<-subset(link,Year==2016 & month>=2)
#tail(test1)
#dim(test1) ##85361筆
#traino1<-subset(link,Year<2016)
#dim(traino1) ##1239469
#trainon1<-subset(link,Year==2016 & month==1)
#dim(trainon1) ##27472
#trainty1<-rbind(traino1,trainon1)
#dim(trainty1) ##1266941



####train model method####
##找出trainty資料集最佳的分群數##
mydata <- trainty[,1:9]
wss<-vector()
for (i in 2:15){ 
  wss[i] <- sum(kproto(trainty,i)$withinss)}

par(mfrow=c(1,1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2) 
##最佳分群數:k=6##

clus1<-kproto(trainty,5)
clus1
clus<-clus1$cluster
clus
length(clus)#1266941

data44<-cbind(clus,trainty) 
data44
dim(trainty) #1266941
dim(data44)
#data44<-subset(data44,select=-c(store_id,dept_id,cat_id,state_id))
#data44
data44<-data44[,-(4:7)]
data44

###將預測模型的train資料集再隨機切割成train跟validation用以建立分類模型###
library(e1071)
smp.size=floor(0.8*nrow(data44)) 
set.seed(1)                     
train.ind=sample(seq_len(nrow(data44)), smp.size)
train=data44[train.ind,] 
vali=data44[-train.ind,]
dim(train) ##1013552
dim(vali)  ##253389
dim(data44)

library(mltools)
library(data.table)

trainonehot1<-subset(train,select=-c(sell))
valionehot1<-subset(vali,select=-c(sell))

#trainonehot<-one_hot(as.data.table(trainonehot1))
#dim(trainonehot)
#valionehot<-one_hot(as.data.table(valionehot1))
#dim(valionehot)

trainonehot<-as.data.table(trainonehot1)#1013552
valionehot<-as.data.table(valionehot1)#253389

cluster1<-subset(trainonehot,clus==1)
cluster2<-subset(trainonehot,clus==2)
cluster3<-subset(trainonehot,clus==3)
cluster4<-subset(trainonehot,clus==4)
cluster5<-subset(trainonehot,clus==5)
dim(subset(trainonehot,clus==1))#339872
dim(subset(trainonehot,clus==2))#197592
dim(subset(trainonehot,clus==3))#192540
dim(subset(trainonehot,clus==4))#135997
dim(subset(trainonehot,clus==5))#147551

###建立分類模型(用以判斷validation/test所屬群集k=1,2,..6)###
##model clus1##
library(xgboost)
library(Matrix)
library(data.table)
trainonehot2<-data.table(trainonehot,keep.rownames=F)
valionehot2<-data.table(valionehot,keep.rownames=F)
trainonehot2$clus<-as.numeric(trainonehot$clus)-1  ##for cv.model
valionehot2$clus<-as.numeric(valionehot$clus)-1  ##for cv.model


sparse_matrix <- sparse.model.matrix(clus~.-1,data=trainonehot2) #902504x25
output_vector = trainonehot2[,clus]
sparse_matrix1 <- sparse.model.matrix(clus~.-1,data =valionehot2)#70698x27
output_vector1 = valionehot2[,clus]

dtrain = xgb.DMatrix(data=sparse_matrix,label=output_vector)
dtest = xgb.DMatrix(data=sparse_matrix1,label=output_vector1)
# Set parameters(default)
params <- list(booster = "gbtree", objective = "multi:softprob", num_class = 5, 
                eval_metric = "mlogloss")
#Calculate of folds for cross-validation#
xgbcv <- xgb.cv(params = params, data =dtrain, nrounds = 50, nfold = 10, showsd = TRUE, 
            stratified = TRUE, 
            print_every_n = 10, early_stop_round = 20, maximize = FALSE, prediction = TRUE)
#Function to compute classification error#
classification_error <- function(conf_mat) {
     conf_mat = as.matrix(conf_mat)
     error = 1 - sum(diag(conf_mat)) / sum(conf_mat)
    return (error)
}
#Mutate xgb output to deliver hard predictions#
xgb_train_preds <- data.frame(xgbcv$pred) %>% mutate(max = max.col(., ties.method = "last"),
                  label = output_vector + 1)

head(xgb_train_preds)

#Confustion Matrix#
xgb_conf_mat <- table(true = output_vector + 1, pred = xgb_train_preds$max)

#Error# 
cat("XGB Training Classification Error Rate:", classification_error(xgb_conf_mat), "\n")
#XGB Training Classification Error Rate: 0.00166 
library(caret)
xgb_conf_mat_2 <- confusionMatrix(factor(xgb_train_preds$label),
                    factor(xgb_train_preds$max),mode = "everything")
print(xgb_conf_mat_2)#0.95##



#Create the model
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 50)
#Predict for validation set
xgb_val_preds <- predict(xgb_model, newdata = dtest)
 
xgb_val_out <- matrix(xgb_val_preds, nrow = 5, ncol = length(xgb_val_preds) / 5) %>% 
  t() %>%
  data.frame() %>%
  mutate(max = max.col(., ties.method = "last"), label = output_vector1 + 1) 

#Confustion Matrix#
xgb_val_conf <- table(true = output_vector1 + 1, pred = xgb_val_out$max)
#error rate#
cat("XGB Validation Classification Error Rate:", classification_error(xgb_val_conf), "\n")

#XGB Validation Classification Error Rate: 0.00151
#Automated confusion matrix using#
xgb_val_conf2 <- confusionMatrix(factor(xgb_val_out$label),
              factor(xgb_val_out$max),mode = "everything")
print(xgb_val_conf2)


#timeseries<-read.csv("D:/report/train2.csv")
###預測模型用##
library(e1071)
smp.size=floor(0.8*nrow(data44)) 
set.seed(1)                     
train.ind=sample(seq_len(nrow(data44)), smp.size)
train1=data44[train.ind,] 
vali1=data44[-train.ind,]
dim(train1) ##1213552##
dim(vali1)  ##253389##


##經過分類模型份類後的全部資料##
#predictdata11<-rbind(valdata,tradata)
#dim(predictdata11) ##1128130 29

##原本的總共資料####data44####
#trainone<-one_hot(as.data.table(train))
#valione<-one_hot(as.data.table(vali))

trainone<-as.data.table(train1)
valione<-as.data.table(vali1)

predictdata<-rbind(trainone,valione)
dim(predictdata) ##1266941 29


##用原始結果分群##
cluster1<-subset(predictdata,clus==1)
cluster2<-subset(predictdata,clus==2)
cluster3<-subset(predictdata,clus==3)
cluster4<-subset(predictdata,clus==4)
cluster5<-subset(predictdata,clus==5)

dim(cluster1)#425082
dim(cluster2)#246583
dim(cluster3)#240697
dim(cluster4)#170368
dim(cluster5)#184211

cluster1<-subset(cluster1,select=c(-clus))
cluster2<-subset(cluster2,select=c(-clus))
cluster3<-subset(cluster3,select=c(-clus))
cluster4<-subset(cluster4,select=c(-clus))
cluster5<-subset(cluster5,select=c(-clus))



###cluster1 切成train與validation###
smp.size=floor(0.8*nrow(cluster1)) 
set.seed(1)                     
train.ind=sample(seq_len(nrow(cluster1)), smp.size)
trainclus1=cluster1[train.ind,] 
valiclus1=cluster1[-train.ind,]
dim(trainclus1) ###366994
dim(valiclus1)  ###91749
str(trainclus1)
trainsell1<-trainclus1$sell
valisell1<-valiclus1$sell


###cluster1 切成train與validation###
smp.size=floor(0.8*nrow(cluster2)) 
set.seed(1)                     
train.ind=sample(seq_len(nrow(cluster2)), smp.size)
trainclus2=cluster2[train.ind,] 
valiclus2=cluster2[-train.ind,]
dim(trainclus2) ##272171
dim(valiclus2)  ##68043
str(trainclus2)
trainsell2<-trainclus2$sell
valisell2<-valiclus2$sell


###cluste2 建立模型###
###cluster1 切成train與validation###
smp.size=floor(0.8*nrow(cluster3)) 
set.seed(1)                     
train.ind=sample(seq_len(nrow(cluster3)), smp.size)
trainclus3=cluster3[train.ind,] 
valiclus3=cluster3[-train.ind,]
dim(trainclus3) ##418690
dim(valiclus3)  ##104673
str(trainclus3)
trainsell3<-trainclus3$sell
valisell3<-valiclus3$sell

###cluste2 建立模型###
###cluster1 切成train與validation###
smp.size=floor(0.8*nrow(cluster4)) 
set.seed(1)                     
train.ind=sample(seq_len(nrow(cluster4)), smp.size)
trainclus4=cluster4[train.ind,] 
valiclus4=cluster4[-train.ind,]
dim(trainclus4) ##263999 ##260844
dim(valiclus4)  ##66000 ##65211
str(trainclus4)
trainsell4<-trainclus4$sell
valisell4<-valiclus4$sell

###cluste2 建立模型###
###cluster1 切成train與validation###
smp.size=floor(0.8*nrow(cluster5)) 
set.seed(1)                     
train.ind=sample(seq_len(nrow(cluster5)), smp.size)
trainclus5=cluster5[train.ind,] 
valiclus5=cluster5[-train.ind,]
dim(trainclus5) ##263999 ##260844
dim(valiclus5)  ##66000 ##65211
str(trainclus5)
trainsell5<-trainclus5$sell
valisell5<-valiclus5$sell

dim(trainclus1) ##340065
dim(valiclus1)    #85017
dim(trainclus2) ##197266
dim(valiclus2)    #49317
dim(trainclus3) ##192557
dim(valiclus3)    #48140
dim(trainclus4) ##136294
dim(valiclus4)    #34074
dim(trainclus5) ##147368
dim(valiclus5)    #36843




names(getModelInfo())
set.seed(1)
?train()
tune1<-train(sell ~itemsellmean+storemean+deptmean+itemsellvar+catmean+storevar+
               itemsellskew+itemsellkur+statemean+Year,data=trainclus1,
             method="svmRadial",modelType="regression",
             trControl=trainControl(method="cv"))
tune2<-train(sell ~itemsellmean+storemean+itemsellvar+storevar+itemsellskew+deptmean+
               itemsellskew+deptmean+itemsellkur+statemean+Year,
             data=trainclus2,method="svmRadial",modelType="regression",
             trControl=trainControl(method="cv"))
tune3<-train(sell ~itemsellmean+storemean+deptmean+itemsellvar+statemean+
               itemsellskew+itemsellkur+Year+catmean+storevar+month,
             data=trainclus3,method="svmRadial",modelType="regression",
             trControl=trainControl(method="cv"))
tune4<-train(sell ~storevar+itemsellmean+itemsellvar+storemean+Year+
               itemsellskew+month+itemsellkur+event_count+deptmean+statemean,
             data=trainclus4,method="svmRadial",modelType="regression",
             trControl=trainControl(method="cv"))
tune5<-train(sell ~itemsellmean+storemean+itemsellvar+itemsellskew+itemsellkur+
               storevar+Year+statemean+month+deptmean+deptvar+catvar+event_count+
               catmean,data=trainclus5,method="svmRadial",modelType="regression",
             trControl=trainControl(method="cv"))
?tune()
tune11<-tune(svm,sell~itemsellmean+storemean+deptmean+itemsellvar+catmean+storevar+
               itemsellskew+itemsellkur+statemean+Year,
             data=trainclus1,type = "eps-regression",
             ranges = list(elsilon=c(0.01,0.1,1),gamma = 2^(-8:3),
                           cost = 2^(-3:7)),scale=FALSE,
             tunecontrol=tune.control(cross=5))

tune22<-tune(svm,sell~itemsellmean+storemean+itemsellvar+storevar+itemsellskew+deptmean+
               itemsellskew+deptmean+itemsellkur+statemean+Year,
             data=trainclus2,type = "eps-regression",
             ranges = list(elsilon=c(0.01,0.1,1),gamma = 2^(-8:3),
                           cost = 2^(-3:7)),scale=FALSE,
             tunecontrol=tune.control(cross=5))

tune33<-tune(svm,sell~itemsellmean+storemean+deptmean+itemsellvar+statemean+
               itemsellskew+itemsellkur+Year+catmean+storevar+month,
             data=trainclus3,type = "eps-regression",
             ranges = list(elsilon=c(0.01,0.1,1),gamma = 2^(-8:3),
                           cost = 2^(-3:7)),scale=FALSE,
             tunecontrol=tune.control(cross=5))

tune44<-tune(svm,sell~storevar+itemsellmean+itemsellvar+storemean+Year+
               itemsellskew+month+itemsellkur+event_count+deptmean+statemean,
             data=trainclus4,type = "eps-regression",
             ranges = list(elsilon=c(0.01,0.1,1),gamma = 2^(-8:3),
                           cost = 2^(-3:7)),scale=FALSE,
             tunecontrol=tune.control(cross=5))

tune55<-tune(svm,sell~itemsellmean+storemean+itemsellvar+itemsellskew+itemsellkur+
               storevar+Year+statemean+month+deptmean+deptvar+catvar+event_count+
               catmean,
             data=trainclus5,type = "eps-regression",
             ranges = list(elsilon=c(0.01,0.1,1),gamma = 2^(-8:3),
                           cost = 2^(-3:7)),scale=FALSE,
             tunecontrol=tune.control(cross=5))

svmfit1<-svm(sell~itemsellmean+storemean+deptmean+itemsellvar+catmean+storevar+
               itemsellskew+itemsellkur+statemean+Year,
             data=trainclus1,type="eps-regression",kernal="radial",
            elsilon=0.1,cost=200,gamma=0.005,scale=FALSE)


svmfit2<-svm(sell~itemsellmean+storemean+itemsellvar+storevar+itemsellskew+deptmean+
               itemsellskew+deptmean+itemsellkur+statemean+Year,
             data=trainclus2,type="eps-regression",kernal="radial",
            elsilon=1,cost=1,scale=FALSE)
summary(svmfit2)

svmfit3<-svm(sell~itemsellmean+storemean+deptmean+itemsellvar+statemean+
               itemsellskew+itemsellkur+Year+catmean+storevar+month,
             data=trainclus3,type="eps-regression",kernal="radial",
            elsilon=1,cost=1,scale=FALSE)
summary(svmfit3)

svmfit4<-svm(sell~sell~storevar+itemsellmean+itemsellvar+storemean+Year+
               itemsellskew+month+itemsellkur+event_count+deptmean+statemean,
             data=trainclus4,type="eps-regression",kernal="radial",
            elsilon=0.1,cost=200,gamma=0.005,scale=FALSE)
summary(svmfit4)

svmfit5<-svm(sell~itemsellmean+storemean+itemsellvar+itemsellskew+itemsellkur+
               storevar+Year+statemean+month+deptmean+deptvar+catvar+event_count+
               catmean,data=trainclus5,type="eps-regression",kernal="radial",
            elsilon=1,cost=1,scale=FALSE)
summary(svmfit5)

svrpred1 = predict(svmfit1,trainclus1)
svrpred2 = predict(svmfit2,trainclus2)
svrpred3 = predict(svmfit3,trainclus3)
svrpred4 = predict(svmfit4,trainclus4)
svrpred5 = predict(svmfit5,trainclus5)

postResample(pred = svrpred1, obs = trainsell1)
postResample(pred = svrpred2, obs = trainsell2)
postResample(pred = svrpred3, obs = trainsell3)
postResample(pred = svrpred4, obs = trainsell4)
postResample(pred = svrpred5, obs = trainsell5)

svrvali1 = predict(svmfit1,valiclus1)
svrvali2 = predict(svmfit2,valiclus2)
svrvali3 = predict(svmfit3,valiclus3)
svrvali4 = predict(svmfit4,valiclus4)
svrvali5 = predict(svmfit5,valiclus5)

postResample(pred = svrpred1, obs = trainsell1)
postResample(pred = svrpred2, obs = trainsell2)
postResample(pred = svrpred3, obs = trainsell3)
postResample(pred = svrpred4, obs = trainsell4)
postResample(pred = svrpred5, obs = trainsell5)


dim(test)
testone1<-subset(test,select=c(-sell))
testone2<-as.matrix(testone1)
sapply(testone2, mode)
testsell<-test$sell
str(testone2)

class(testone2)
testclass<-predict(xgb_model,testone2)
testclass
xgb_val_test <- matrix(testclass, nrow = 6, ncol = length(testclass) / 6) %>% 
  t() %>%
  data.frame() %>%
  mutate(max = max.col(., ties.method = "last")) 

dim(xgb_val_test)
dim(test)

classsvr<-xgb_val_test$max
classsvr
classsvrtest<-data.table(classsvr,testone2)
classsvrtest
dim(classsvrtest)
str(classsvrtest)
predclus1<-subset(classsvrtest,classsvr==1)
predclus2<-subset(classsvrtest,classsvr==2)
predclus3<-subset(classsvrtest,classsvr==3)
predclus4<-subset(classsvrtest,classsvr==4)
predclus5<-subset(classsvrtest,classsvr==5)
dim(predclus1)#5933
dim(predclus2)#0
dim(predclus3)#66550
dim(predclus4)#314
dim(predclus5)#12564


predclus1<-subset(predclus1,select=-c(classsvr))
svrresult1<-predict(svmfit1,predclus1)
svrresult1
svr1<-data.frame(svrresult,predclus1)
dim(svr1)
str(svr1)

predclus2<-subset(predclus2,select=-c(classsvr))
svrresult2<-predict(svmfit2,predclus2)
svrresult2
svr2<-data.frame(svrresult2,predclus2)
dim(svr2)
str(svr2)

predclus3<-subset(predclus3,select=-c(classsvr))
svrresult3<-predict(svmfit3,predclus3)
svrresult3
svr3<-data.frame(svrresult3,predclus3)
dim(svr3)
str(svr3)

predclus4<-subset(predclus4,select=-c(classsvr))
svrresult4<-predict(svmfit4,predclus4)
svrresult4
svr4<-data.frame(svrresult4,predclus4)
dim(svr4)
str(svr4)

predclus5<-subset(predclus5,select=-c(classsvr))
svrresult5<-predict(svmfit5,predclus5)
svrresult5
svr5<-data.frame(svrresult5,predclus5)
dim(svr5)
str(svr5)








# convert your dataframes (except for the "sell" column) into matrices and then Dmatrixs
train_data1 <- data.matrix(trainclus1[, -1]) 
train_label1 <- trainclus1$sell
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_label1)

test_data1 <- data.matrix(valiclus1[, -1])
test_label1 <- valiclus1$sell
dtest1 <- xgb.DMatrix(data = test_data1, label= test_label1)
params1 <- list(booster = "gbtree", objective = "reg:squarederror",
              eval_metric = "rmse",subsample=0.5,gamma=0.01,min_child_weight=20,
              colsample_bytree=1,max_depth=10,eta=0.1)
#Calculate of folds for cross-validation#
xgbcv1 <- xgb.cv(params = params1, data =dtrain1, nrounds = 100, nfold = 10, showsd = TRUE, 
               stratified = TRUE, 
                print_every_n = 20, early_stop_round = 20, 
                maximize = FALSE, prediction = TRUE)

 #建立模型#
xgb.model1 = xgb.train(paras=params1,data=dtrain1,
                      nrounds=100)

##重要特徵畫圖##
imp_fearture1 <- xgb.importance(colnames(dtrain1), model = xgb.model1)
print(imp_fearture1)
xgb.ggplot.importance(imp_fearture1)

##prediction train
train.pred1 = predict(xgb.model1, dtrain1)
mse1 = mean((train_label1 - train.pred1)^2)
mse1  
mae1 = caret::MAE(train_label1, train.pred1)
mae1 
rmse1 = caret::RMSE(train_label1, train.pred1)
rmse1 
postResample(pred = train.pred1, obs = train_label1)

##prediction validation
valipred1=predict(xgb.model1,dtest1)
mse11 = mean((test_label1 - valipred1)^2)
mse11 
mae11 = caret::MAE(test_label1, valipred1)
mae11  
rmse11 = caret::RMSE(test_label1, valipred1)
rmse11  
postResample(pred = valipred1, obs = test_label1)


###cluste2 建立模型###

# convert your dataframes (except for the "sell" column) into matrices and then Dmatrixs
train_data2 <- data.matrix(trainclus2[, -1]) 
train_label2 <- trainclus2$sell
dtrain2 <- xgb.DMatrix(data = train_data2, label= train_label2)

test_data2 <- data.matrix(valiclus2[, -1])
test_label2 <- valiclus2$sell
dtest2 <- xgb.DMatrix(data = test_data2, label= test_label2)
params2 <- list(booster = "gbtree", objective = "reg:squarederror",
               eval_metric = "rmse",subsample=0.5,gamma=1,min_child_weight=20,
               colsample_bytree=1,max_depth=20,eta=0.1)
#Calculate of folds for cross-validation#
xgbcv2 <- xgb.cv(params = params2, data =dtrain2, nrounds = 200, nfold = 5, showsd = TRUE, 
                stratified = TRUE, 
                print_every_n = 20, early_stop_round = 20, 
                maximize = FALSE, prediction = TRUE)
#建立模型#
xgb.model2 = xgb.train(paras=params2,data=dtrain2,
                      nrounds=200)

##重要特徵畫圖##
imp_fearture2 <- xgb.importance(colnames(dtrain2), model = xgb.model2)
print(imp_fearture2)
xgb.ggplot.importance(imp_fearture2)

##prediction train
train.pred2 = predict(xgb.model2, dtrain2)
mse2 = mean((train_label2 - train.pred2)^2)
mse2  #1493
mae2 = caret::MAE(train_label2, train.pred2)
mae2 #25.27
rmse2 = caret::RMSE(train_label2, train.pred2)
rmse2 #38.64
postResample(pred = train.pred2, obs = train_label2)


##prediction validation
valipred2=predict(xgb.model2,dtest2)
mse22 = mean((test_label2 - valipred2)^2)
mse22 #1598
mae22 = caret::MAE(test_label2, valipred2)
mae22  #25.92
rmse22 = caret::RMSE(test_label2, valipred2)
rmse22  #39.98
postResample(pred = valipred2, obs = test_label2)


# convert your dataframes (except for the "sell" column) into matrices and then Dmatrixs
train_data3 <- data.matrix(trainclus3[, -1]) 
train_label3 <- trainclus3$sell
dtrain3 <- xgb.DMatrix(data = train_data3, label= train_label3)

test_data3 <- data.matrix(valiclus3[, -1])
test_label3 <- valiclus3$sell
dtest3 <- xgb.DMatrix(data = test_data3, label= test_label3)
params3 <- list(booster = "gbtree", objective = "reg:squarederror",
                eval_metric = "rmse",subsample=1,gamma=1,min_child_weight=20,
                colsample_bytree=1,max_depth=20,eta=0.05)
#Calculate of folds for cross-validation#
xgbcv3 <- xgb.cv(params = params3, data =dtrain3, nrounds = 100, nfold = 10, showsd = TRUE, 
                 stratified = TRUE, 
                 print_every_n = 20, early_stop_round = 20, 
                 maximize = FALSE, prediction = TRUE)
#建立模型#
xgb.model3 = xgb.train(paras=params3,data=dtrain3,
                       nrounds=100)

##重要特徵畫圖##
imp_fearture3 <- xgb.importance(colnames(dtrain3), model = xgb.model3)
print(imp_fearture3)
xgb.ggplot.importance(imp_fearture3)

##prediction train
train.pred3 = predict(xgb.model3, dtrain3)
mse3 = mean((train_label3 - train.pred3)^2)
mse3  #1493
mae3 = caret::MAE(train_label3, train.pred3)
mae3 #25.27
rmse3 = caret::RMSE(train_label3, train.pred3)
rmse3 #38.64
postResample(pred = train.pred3, obs = train_label3)


##prediction validation
valipred3=predict(xgb.model3,dtest3)
mse33 = mean((test_label3 - valipred3)^2)
mse33 #1598
mae33 = caret::MAE(test_label3, valipred3)
mae33  #25.92
rmse33 = caret::RMSE(test_label3, valipred3)
rmse33  #39.98
postResample(pred = valipred3, obs = test_label3)




# convert your dataframes (except for the "sell" column) into matrices and then Dmatrixs
train_data4 <- data.matrix(trainclus4[, -1]) 
train_label4 <- trainclus4$sell
dtrain4 <- xgb.DMatrix(data = train_data4, label= train_label4)

test_data4 <- data.matrix(valiclus4[, -1])
test_label4 <- valiclus4$sell
dtest4 <- xgb.DMatrix(data = test_data4, label= test_label4)
params4 <- list(booster = "gbtree", objective = "reg:squarederror",
                eval_metric = "rmse",subsample=1,gamma=1,min_child_weight=20,
                colsample_bytree=1,max_depth=20,eta=1)
#Calculate of folds for cross-validation#
xgbcv4 <- xgb.cv(params = params4, data =dtrain4, nrounds = 100, nfold = 5, showsd = TRUE, 
                 stratified = TRUE, 
                 print_every_n = 20, early_stop_round = 20, 
                 maximize = FALSE, prediction = TRUE)
#建立模型#
xgb.model4 = xgb.train(paras=params4,data=dtrain4,
                       nrounds=100)
##重要特徵畫圖##
imp_fearture4 <- xgb.importance(colnames(dtrain4), model = xgb.model4)
print(imp_fearture4)
xgb.ggplot.importance(imp_fearture4)


##prediction train
train.pred4 = predict(xgb.model4, dtrain4)
mse4 = mean((train_label4 - train.pred4)^2)
mse4  #1493
mae4 = caret::MAE(train_label4, train.pred4)
mae4 #25.27
rmse4 = caret::RMSE(train_label4, train.pred4)
rmse4 #38.64
postResample(pred = train.pred4, obs = train_label4)


##prediction validation
valipred4=predict(xgb.model4,dtest4)
mse44 = mean((test_label4 - valipred4)^2)
mse44 #1598
mae44 = caret::MAE(test_label4, valipred4)
mae44  #25.92
rmse44 = caret::RMSE(test_label4, valipred4)
rmse44  #39.98
postResample(pred = valipred4, obs = test_label4)


# convert your dataframes (except for the "sell" column) into matrices and then Dmatrixs
train_data5 <- data.matrix(trainclus5[, -1]) 
train_label5 <- trainclus5$sell
dtrain5 <- xgb.DMatrix(data = train_data5, label= train_label5)

test_data5 <- data.matrix(valiclus5[, -1])
test_label5 <- valiclus5$sell
dtest5 <- xgb.DMatrix(data = test_data5, label= test_label5)
params5 <- list(booster = "gbtree", objective = "reg:squarederror",
                eval_metric = "rmse",subsample=1,gamma=5,min_child_weight=20,
                colsample_bytree=1,max_depth=20,eta=0.05)
#Calculate of folds for cross-validation#
xgbcv5 <- xgb.cv(params = params5, data =dtrain5, nrounds = 100, nfold = 10, showsd = TRUE, 
                 stratified = TRUE, 
                 print_every_n = 20, early_stop_round = 20, 
                 maximize = FALSE, prediction = TRUE)
#建立模型#
xgb.model5 = xgb.train(paras=params5,data=dtrain5,
                       nrounds=100)

##重要特徵畫圖##
imp_fearture5 <- xgb.importance(colnames(dtrain5), model = xgb.model5)
print(imp_fearture5)
xgb.ggplot.importance(imp_fearture5)

##prediction train
train.pred5 = predict(xgb.model5, dtrain5)
mse5 = mean((train_label5 - train.pred5)^2)
mse5  #1493
mae5 = caret::MAE(train_label5, train.pred5)
mae5 #25.27
rmse5 = caret::RMSE(train_label5, train.pred5)
rmse5 #38.64
postResample(pred = train.pred5, obs = train_label5)


##prediction validation
valipred5=predict(xgb.model5,dtest5)
mse55 = mean((test_label5 - valipred5)^2)
mse55 #1598
mae55 = caret::MAE(test_label5, valipred5)
mae55  #25.92
rmse55 = caret::RMSE(test_label5, valipred5)
rmse55  #39.98
postResample(pred = valipred5, obs = test_label5)

RMSPE(train.pred5,train_label5)
mape(train.pred5,train_label5)
rmse(train.pred5,train_label5)

RMSPE(train.pred4,train_label4)
mape(train.pred4,train_label4)
rmse(train.pred4,train_label4)

RMSPE(train.pred3,train_label3)
mape(train.pred3,train_label3)
rmse(train.pred3,train_label3)

RMSPE(train.pred2,train_label2)
mape(train.pred2,train_label2)
rmse(train.pred2,train_label2)

RMSPE(train.pred1,train_label1)
mape(train.pred1,train_label1)
rmse(train.pred1,train_label1)

RMSPE(valipred5,test_label5)
mape(valipred5,test_label5)
rmse(valipred5,test_label5)

RMSPE(valipred4,test_label4)
mape(valipred4,test_label4)
rmse(valipred4,test_label4)

RMSPE(valipred3,test_label3)
mape(valipred3,test_label3)
rmse(valipred3,test_label3)

RMSPE(valipred2,test_label2)
mape(valipred2,test_label2)
rmse(valipred2,test_label2)

RMSPE(valipred1,test_label1)
mape(valipred1,test_label1)
rmse(valipred1,test_label1)



####prediction testing####
sparse_matrix <- sparse.model.matrix(clus~.-1,data=trainonehot) #902504x25
output_vector = trainonehot[,clus]
sparse_matrix1 <- sparse.model.matrix(clus~.-1,data =valionehot)#70698x27
output_vector1 = valionehot[,clus]
dtrain = xgb.DMatrix(data=sparse_matrix,label=output_vector)
dtest = xgb.DMatrix(data=sparse_matrix1,label=output_vector1)
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100)
xgb_val_preds <- predict(xgb_model, newdata = dtest)



testone<-as.data.table(test)
testone1<-subset(testone,select=c(-sell))
testone2<-as.matrix(testone1)
sapply(testone2, mode)
str(testone2)

class(testone)
testclass<-predict(xgb_model,testone2)
testclass
xgb_val_test <- matrix(testclass, nrow = 5, ncol = length(testclass) / 5) %>% 
  t() %>%
  data.frame() %>%
  mutate(max = max.col(., ties.method = "last")) 

dim(xgb_val_test)
dim(test)

classpredict<-xgb_val_test$max
classpredict
classtest<-data.table(classpredict,testone)
classtest
classtest<-as.matrix(classtest)
dim(classtest)
str(classtest)
predictclus1<-subset(classtest,classpredict==1) #0
predictclus2<-subset(classtest,classpredict==2)#570
predictclus3<-subset(classtest,classpredict==3)#740
predictclus4<-subset(classtest,classpredict==4)#0
predictclus5<-subset(classtest,classpredict==5)#85980
dim(predictclus1)#29169
dim(predictclus2)#16699
dim(predictclus3)#16151
dim(predictclus4)#11308
dim(predictclus5)#12034


predictclus1<-subset(predictclus1,select=-c(classpredict,sell))
#predictclus22<-data.frame(sell2,predictclus2)
predictclus1<-as.matrix(predictclus1)
str(predictclus1)
sapply(predictclus1, mode)
result<-predict(xgb.model1,predictclus1)
result
set1<-data.frame(result,predictclus1)
dim(set1)
str(set1)
set1

predictclus2<-subset(predictclus2,select=-c(classpredict,sell))
#predictclus22<-data.frame(sell2,predictclus2)
predictclus2<-as.matrix(predictclus2)
str(predictclus2)
sapply(predictclus2, mode)
result<-predict(xgb.model2,predictclus2)
result
set2<-data.frame(result,predictclus2)
dim(set2)
str(set2)


predictclus3<-subset(predictclus3,select=-c(classpredict,sell))
#predictclus22<-data.frame(sell2,predictclus2)
predictclus3<-as.matrix(predictclus3)
str(predictclus3)
sapply(predictclus3, mode)
result<-predict(xgb.model3,predictclus3)
result
set3<-data.frame(result,predictclus3)
dim(set3)
str(set3)

predictclus4<-subset(predictclus4,select=-c(classpredict,sell))
#predictclus22<-data.frame(sell2,predictclus2)
predictclus4<-as.matrix(predictclus4)
str(predictclus4)
sapply(predictclus4, mode)
result<-predict(xgb.model4,predictclus4)
result
set4<-data.frame(result,predictclus4)
dim(set4)



predictclus5<-subset(predictclus5,select=-c(classpredict,sell))
#predictclus22<-data.frame(sell2,predictclus2)
predictclus5<-as.matrix(predictclus5)
str(predictclus5)
sapply(predictclus5, mode)
result<-predict(xgb.model5,predictclus5)
result
set5<-data.frame(result,predictclus5)
dim(set5)

dim(testone)
dim(test)
dim(resultyaya)
resultyaya<-rbind(set1,set2,set3,set4,set5)
resultyaya
dim(resultall) #85361
resultall1<-left_join(test1,resultyaya,by=c("Year"="Year","event_count"="event_count",
                                         "month"="month","itemsellmean"="itemsellmean",
                                         "itemsellvar"="itemsellvar",
                                         "itemsellskew"="itemsellskew",
                                         "itemsellkur"="itemsellkur",
                                         "storemean"="storemean","storevar"="storevar",
                                         "deptmean"="deptmean","deptvar"="deptvar",
                                         "catmean"="catmean","catvar"="catvar",
                                         "statemean"="statemean","statevar"="statevar"))
resultall1
resultall<-one_hot(as.data.table(resultall1))

hobbies1_ca_2<-subset(resultall,
                     month==2&state_id_CA==1&dept_id_HOBBIES_1==1)
hobbies1_tx_2<-subset(resultall,
                     month==2&state_id_TX==1&dept_id_HOBBIES_1==1)
hobbies1_wi_2<-subset(resultall,
                     month==2&state_id_WI==1&dept_id_HOBBIES_1==1)
hobbies2_ca_2<-subset(resultall,
                     month==2&state_id_CA==1&dept_id_HOBBIES_2==1)
hobbies2_tx_2<-subset(resultall,
                     month==2&state_id_TX==1&dept_id_HOBBIES_2==1)
hobbies2_wi_2<-subset(resultall,
                     month==2&state_id_WI==1&dept_id_HOBBIES_2==1)
foods1_ca_2<-subset(resultall,
                     month==2&state_id_CA==1&dept_id_FOODS_1==1)
foods1_tx_2<-subset(resultall,
                     month==2&state_id_TX==1&dept_id_FOODS_1==1)
foods1_wi_2<-subset(resultall,
                     month==2&state_id_WI==1&dept_id_FOODS_1==1)
foods2_ca_2<-subset(resultall,
                     month==2&state_id_CA==1&dept_id_FOODS_2==1)
foods2_tx_2<-subset(resultall,
                     month==2&state_id_TX==1&dept_id_FOODS_2==1)
foods2_wi_2<-subset(resultall,
                     month==2&state_id_WI==1&dept_id_FOODS_2==1)
foods3_ca_2<-subset(resultall,
                     month==2&state_id_CA==1&dept_id_FOODS_3==1)
foods3_tx_2<-subset(resultall,
                     month==2&state_id_TX==1&dept_id_FOODS_3==1)
foods3_wi_2<-subset(resultall,
                     month==2&state_id_WI==1&dept_id_FOODS_3==1)
household1_ca_2<-subset(resultall,
                     month==2&state_id_CA==1&dept_id_HOUSEHOLD_1==1)
household1_tx_2<-subset(resultall,
                     month==2&state_id_TX==1&dept_id_HOUSEHOLD_1==1)
household1_wi_2<-subset(resultall,
                     month==2&state_id_WI==1&dept_id_HOUSEHOLD_1==1)
household2_ca_2<-subset(resultall,
                      month==2&state_id_CA==1&dept_id_HOUSEHOLD_2==1)
household2_tx_2<-subset(resultall,
                      month==2&state_id_TX==1&dept_id_HOUSEHOLD_2==1)
household2_wi_2<-subset(resultall,
                      month==2&state_id_WI==1&dept_id_HOUSEHOLD_2==1)

dim(hobbies1_ca_2) #1664
dim(hobbies1_tx_2)#1248
dim(hobbies1_wi_2)#1248
dim(hobbies2_ca_2)#1664
dim(hobbies2_tx_2)#1248
dim(hobbies2_wi_2)#1248
dim(foods1_ca_2)#864
dim(foods1_tx_2)#648
dim(foods1_wi_2)#648
dim(foods2_ca_2)#1592
dim(foods2_tx_2)#1194
dim(foods2_wi_2)#1194
dim(foods3_ca_2)#3292
dim(foods3_tx_2)#2469
dim(foods3_wi_2)#2469
dim(household1_ca_2)#2128
dim(household1_tx_2)#1596
dim(household1_wi_2)#1596
dim(household2_ca_2)#2060
dim(household2_tx_2)#1545
dim(household2_wi_2)#1545

sell21<-sum(hobbies1_ca_2$result)##113385
sell21
sell22<-sum(hobbies1_tx_2$result)##85271
sell22
sell23<-sum(hobbies1_wi_2$result)##86625
sell23
sell24<-sum(hobbies2_ca_2$result)##113385
sell24
sell25<-sum(hobbies2_tx_2$result)##85271
sell25
sell26<-sum(hobbies2_wi_2$result)##86625
sell27<-sum(foods1_ca_2$result)##94111
sell28<-sum(foods1_tx_2$result)##72428
sell29<-sum(foods1_wi_2$result)
sell210<-sum(foods2_ca_2$result)
sell211<-sum(foods2_tx_2$result)
sell212<-sum(foods2_wi_2$result)
sell213<-sum(foods3_ca_2$result)
sell214<-sum(foods3_tx_2$result)
sell215<-sum(foods3_wi_2$result)
sell216<-sum(household1_ca_2$result)
sell217<-sum(household1_tx_2$result)
sell218<-sum(household1_wi_2$result)
sell219<-sum(household2_ca_2$result)
sell220<-sum(household2_tx_2$result)
sell221<-sum(household2_wi_2$result)

p21<-subset(resultall,month==2&state_id_CA==1&dept_id_HOBBIES_1==1)
p22<-subset(resultall,month==2&state_id_TX==1&dept_id_HOBBIES_1==1)
p23<-subset(resultall,month==2&state_id_WI==1&dept_id_HOBBIES_1==1)
p24<-subset(resultall,month==2&state_id_CA==1&dept_id_HOBBIES_2==1)
p25<-subset(resultall,month==2&state_id_TX==1&dept_id_HOBBIES_2==1)
p26<-subset(resultall,month==2&state_id_WI==1&dept_id_HOBBIES_2==1)
p27<-subset(resultall,month==2&state_id_CA==1&dept_id_FOODS_1==1)
p28<-subset(resultall,month==2&state_id_TX==1&dept_id_FOODS_1==1)
p29<-subset(resultall,month==2&state_id_WI==1&dept_id_FOODS_1==1)
p210<-subset(resultall,month==2&state_id_CA==1&dept_id_FOODS_2==1)
p211<-subset(resultall,month==2&state_id_TX==1&dept_id_FOODS_2==1)
p212<-subset(resultall,month==2&state_id_WI==1&dept_id_FOODS_2==1)
p213<-subset(resultall,month==2&state_id_CA==1&dept_id_FOODS_3==1)
p214<-subset(resultall,month==2&state_id_TX==1&dept_id_FOODS_3==1)
p215<-subset(resultall,month==2&state_id_WI==1&dept_id_FOODS_3==1)
p216<-subset(resultall,month==2&state_id_CA==1&dept_id_HOUSEHOLD_1==1)
p217<-subset(resultall,month==2&state_id_TX==1&dept_id_HOUSEHOLD_1==1)
p218<-subset(resultall,month==2&state_id_WI==1&dept_id_HOUSEHOLD_1==1)
p219<-subset(resultall,month==2&state_id_CA==1&dept_id_HOUSEHOLD_2==1)
p220<-subset(resultall,month==2&state_id_TX==1&dept_id_HOUSEHOLD_2==1)
p221<-subset(resultall,month==2&state_id_WI==1&dept_id_HOUSEHOLD_2==1)

p1<-sum(p21$sell)
p2<-sum(p22$sell)
p3<-sum(p23$sell)
p4<-sum(p24$sell)
p5<-sum(p25$sell)
p6<-sum(p26$sell)
p7<-sum(p27$sell)
p8<-sum(p28$sell)
p9<-sum(p29$sell)
p10<-sum(p210$sell)
p11<-sum(p211$sell)
p12<-sum(p212$sell)
p13<-sum(p213$sell)
p14<-sum(p214$sell)
p15<-sum(p215$sell)
p16<-sum(p216$sell)
p17<-sum(p217$sell)
p18<-sum(p218$sell)
p19<-sum(p219$sell)
p20<-sum(p220$sell)
p21<-sum(p221$sell)
predicfeb<-c(sell21,sell22,sell23,sell24,sell25,sell26,sell27,sell28,sell29,sell210,
             sell211,sell212,sell213,sell214,sell215,sell216,sell217,sell218,sell219,
             sell220,sell221)
real<-c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,
        p18,p19,p20,p21)
feb<-data.frame(predicfeb,real)
feb



#####march#####
hobbies1_ca_3<-subset(resultall,
                      month==3&state_id_CA==1&dept_id_HOBBIES_1==1)
hobbies1_tx_3<-subset(resultall,
                      month==3&state_id_TX==1&dept_id_HOBBIES_1==1)
hobbies1_wi_3<-subset(resultall,
                      month==3&state_id_WI==1&dept_id_HOBBIES_1==1)
hobbies2_ca_3<-subset(resultall,
                      month==3&state_id_CA==1&dept_id_HOBBIES_2==1)
hobbies2_tx_3<-subset(resultall,
                      month==3&state_id_TX==1&dept_id_HOBBIES_2==1)
hobbies2_wi_3<-subset(resultall,
                      month==3&state_id_WI==1&dept_id_HOBBIES_2==1)
foods1_ca_3<-subset(resultall,
                    month==3&state_id_CA==1&dept_id_FOODS_1==1)
foods1_tx_3<-subset(resultall,
                    month==3&state_id_TX==1&dept_id_FOODS_1==1)
foods1_wi_3<-subset(resultall,
                    month==3&state_id_WI==1&dept_id_FOODS_1==1)
foods2_ca_3<-subset(resultall,
                    month==3&state_id_CA==1&dept_id_FOODS_2==1)
foods2_tx_3<-subset(resultall,
                    month==3&state_id_TX==1&dept_id_FOODS_2==1)
foods2_wi_3<-subset(resultall,
                    month==3&state_id_WI==1&dept_id_FOODS_2==1)
foods3_ca_3<-subset(resultall,
                    month==3&state_id_CA==1&dept_id_FOODS_3==1)
foods3_tx_3<-subset(resultall,
                    month==3&state_id_TX==1&dept_id_FOODS_3==1)
foods3_wi_3<-subset(resultall,
                    month==3&state_id_WI==1&dept_id_FOODS_3==1)
household1_ca_3<-subset(resultall,
                        month==3&state_id_CA==1&dept_id_HOUSEHOLD_1==1)
household1_tx_3<-subset(resultall,
                        month==3&state_id_TX==1&dept_id_HOUSEHOLD_1==1)
household1_wi_3<-subset(resultall,
                        month==3&state_id_WI==1&dept_id_HOUSEHOLD_1==1)
household2_ca_3<-subset(resultall,
                        month==3&state_id_CA==1&dept_id_HOUSEHOLD_2==1)
household2_tx_3<-subset(resultall,
                        month==3&state_id_TX==1&dept_id_HOUSEHOLD_2==1)
household2_wi_3<-subset(resultall,
                        month==3&state_id_WI==1&dept_id_HOUSEHOLD_2==1)

dim(hobbies1_ca_3) #1664
dim(hobbies1_tx_3)#1248
dim(hobbies1_wi_3)#1248
dim(hobbies2_ca_3)#1664
dim(hobbies2_tx_3)#1248
dim(hobbies2_wi_3)#1248
dim(foods1_ca_3)#864
dim(foods1_tx_3)#648
dim(foods1_wi_3)#648
dim(foods2_ca_3)#1592
dim(foods2_tx_3)#1194
dim(foods2_wi_3)#1194
dim(foods3_ca_3)#3292
dim(foods3_tx_3)#2469
dim(foods3_wi_3)#2469
dim(household1_ca_3)#2128
dim(household1_tx_3)#1596
dim(household1_wi_3)#1596
dim(household2_ca_3)#2060
dim(household2_tx_3)#1545
dim(household2_wi_3)#1545

sell31<-sum(hobbies1_ca_3$result) ##113385
sell31
sell32<-sum(hobbies1_tx_3$result)
sell32
sell33<-sum(hobbies1_wi_3$result)
sell33
sell34<-sum(hobbies2_ca_3$result)
sell34
sell35<-sum(hobbies2_tx_3$result)
sell35
sell36<-sum(hobbies2_wi_3$result)
sell37<-sum(foods1_ca_3$result)
sell38<-sum(foods1_tx_3$result)
sell39<-sum(foods1_wi_3$result)
sell310<-sum(foods2_ca_3$result)
sell311<-sum(foods2_tx_3$result)
sell312<-sum(foods2_wi_3$result)
sell313<-sum(foods3_ca_3$result)
sell314<-sum(foods3_tx_3$result)
sell315<-sum(foods3_wi_3$result)
sell316<-sum(household1_ca_3$result)
sell317<-sum(household1_tx_3$result)
sell318<-sum(household1_wi_3$result)
sell319<-sum(household2_ca_3$result)
sell320<-sum(household2_tx_3$result)
sell321<-sum(household2_wi_3$result)

p31<-subset(resultall,month==3&state_id_CA==1&dept_id_HOBBIES_1==1)
p32<-subset(resultall,month==3&state_id_TX==1&dept_id_HOBBIES_1==1)
p33<-subset(resultall,month==3&state_id_WI==1&dept_id_HOBBIES_1==1)
p34<-subset(resultall,month==3&state_id_CA==1&dept_id_HOBBIES_2==1)
p35<-subset(resultall,month==3&state_id_TX==1&dept_id_HOBBIES_2==1)
p36<-subset(resultall,month==3&state_id_WI==1&dept_id_HOBBIES_2==1)
p37<-subset(resultall,month==3&state_id_CA==1&dept_id_FOODS_1==1)
p38<-subset(resultall,month==3&state_id_TX==1&dept_id_FOODS_1==1)
p39<-subset(resultall,month==3&state_id_WI==1&dept_id_FOODS_1==1)
p310<-subset(resultall,month==3&state_id_CA==1&dept_id_FOODS_2==1)
p311<-subset(resultall,month==3&state_id_TX==1&dept_id_FOODS_2==1)
p312<-subset(resultall,month==3&state_id_WI==1&dept_id_FOODS_2==1)
p313<-subset(resultall,month==3&state_id_CA==1&dept_id_FOODS_3==1)
p314<-subset(resultall,month==3&state_id_TX==1&dept_id_FOODS_3==1)
p315<-subset(resultall,month==3&state_id_WI==1&dept_id_FOODS_3==1)
p316<-subset(resultall,month==3&state_id_CA==1&dept_id_HOUSEHOLD_1==1)
p317<-subset(resultall,month==3&state_id_TX==1&dept_id_HOUSEHOLD_1==1)
p318<-subset(resultall,month==3&state_id_WI==1&dept_id_HOUSEHOLD_1==1)
p319<-subset(resultall,month==3&state_id_CA==1&dept_id_HOUSEHOLD_2==1)
p320<-subset(resultall,month==3&state_id_TX==1&dept_id_HOUSEHOLD_2==1)
p321<-subset(resultall,month==3&state_id_WI==1&dept_id_HOUSEHOLD_2==1)

b1<-sum(p31$sell)
b2<-sum(p32$sell)
b3<-sum(p33$sell)
b4<-sum(p34$sell)
b5<-sum(p35$sell)
b6<-sum(p36$sell)
b7<-sum(p37$sell)
b8<-sum(p38$sell)
b9<-sum(p39$sell)
b10<-sum(p310$sell)
b11<-sum(p311$sell)
b12<-sum(p312$sell)
b13<-sum(p313$sell)
b14<-sum(p314$sell)
b15<-sum(p315$sell)
b16<-sum(p316$sell)
b17<-sum(p317$sell)
b18<-sum(p318$sell)
b19<-sum(p319$sell)
b20<-sum(p320$sell)
b21<-sum(p321$sell)
predicmar<-c(sell31,sell32,sell33,sell34,sell35,sell36,sell37,sell38,sell39,sell310,
             sell311,sell312,sell313,sell314,sell315,sell316,sell317,sell318,sell319,
             sell320,sell321)
realmar<-c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,
        b18,b19,b20,b21)
mar<-data.frame(predicmar,realmar)
mar





#####april#####

hobbies1_ca_4<-subset(resultall,
                      month==4&state_id_CA==1&dept_id_HOBBIES_1==1)
hobbies1_tx_4<-subset(resultall,
                      month==4&state_id_TX==1&dept_id_HOBBIES_1==1)
hobbies1_wi_4<-subset(resultall,
                      month==4&state_id_WI==1&dept_id_HOBBIES_1==1)
hobbies2_ca_4<-subset(resultall,
                      month==4&state_id_CA==1&dept_id_HOBBIES_2==1)
hobbies2_tx_4<-subset(resultall,
                      month==4&state_id_TX==1&dept_id_HOBBIES_2==1)
hobbies2_wi_4<-subset(resultall,
                      month==4&state_id_WI==1&dept_id_HOBBIES_2==1)
foods1_ca_4<-subset(resultall,
                    month==4&state_id_CA==1&dept_id_FOODS_1==1)
foods1_tx_4<-subset(resultall,
                    month==4&state_id_TX==1&dept_id_FOODS_1==1)
foods1_wi_4<-subset(resultall,
                    month==4&state_id_WI==1&dept_id_FOODS_1==1)
foods2_ca_4<-subset(resultall,
                    month==4&state_id_CA==1&dept_id_FOODS_2==1)
foods2_tx_4<-subset(resultall,
                    month==4&state_id_TX==1&dept_id_FOODS_2==1)
foods2_wi_4<-subset(resultall,
                    month==4&state_id_WI==1&dept_id_FOODS_2==1)
foods3_ca_4<-subset(resultall,
                    month==4&state_id_CA==1&dept_id_FOODS_3==1)
foods3_tx_4<-subset(resultall,
                    month==4&state_id_TX==1&dept_id_FOODS_3==1)
foods3_wi_4<-subset(resultall,
                    month==4&state_id_WI==1&dept_id_FOODS_3==1)
household1_ca_4<-subset(resultall,
                        month==4&state_id_CA==1&dept_id_HOUSEHOLD_1==1)
household1_tx_4<-subset(resultall,
                        month==4&state_id_TX==1&dept_id_HOUSEHOLD_1==1)
household1_wi_4<-subset(resultall,
                        month==4&state_id_WI==1&dept_id_HOUSEHOLD_1==1)
household2_ca_4<-subset(resultall,
                        month==4&state_id_CA==1&dept_id_HOUSEHOLD_2==1)
household2_tx_4<-subset(resultall,
                        month==4&state_id_TX==1&dept_id_HOUSEHOLD_2==1)
household2_wi_4<-subset(resultall,
                        month==4&state_id_WI==1&dept_id_HOUSEHOLD_2==1)

dim(hobbies1_ca_4) #1664
dim(hobbies1_tx_4)#1248
dim(hobbies1_wi_4)#1248
dim(hobbies2_ca_4)#1664
dim(hobbies2_tx_4)#1248
dim(hobbies2_wi_4)#1248
dim(foods1_ca_4)#864
dim(foods1_tx_4)#648
dim(foods1_wi_4)#648
dim(foods2_ca_4)#1592
dim(foods2_tx_4)#1194
dim(foods2_wi_4)#1194
dim(foods3_ca_4)#3292
dim(foods3_tx_4)#2469
dim(foods3_wi_4)#2469
dim(household1_ca_4)#2128
dim(household1_tx_4)#1596
dim(household1_wi_4)#1596
dim(household2_ca_4)#2060
dim(household2_tx_4)#1545
dim(household2_wi_4)#1545

sell41<-sum(hobbies1_ca_4$result) ##113385
sell41
sell42<-sum(hobbies1_tx_4$result)
sell42
sell43<-sum(hobbies1_wi_4$result)
sell43
sell44<-sum(hobbies2_ca_4$result)
sell44
sell45<-sum(hobbies2_tx_4$result)
sell45
sell46<-sum(hobbies2_wi_4$result)
sell47<-sum(foods1_ca_4$result)
sell48<-sum(foods1_tx_4$result)
sell49<-sum(foods1_wi_4$result)
sell410<-sum(foods2_ca_4$result)
sell411<-sum(foods2_tx_4$result)
sell412<-sum(foods2_wi_4$result)
sell413<-sum(foods3_ca_4$result)
sell414<-sum(foods3_tx_4$result)
sell415<-sum(foods3_wi_4$result)
sell416<-sum(household1_ca_4$result)
sell417<-sum(household1_tx_4$result)
sell418<-sum(household1_wi_4$result)
sell419<-sum(household2_ca_4$result)
sell420<-sum(household2_tx_4$result)
sell421<-sum(household2_wi_4$result)


p41<-subset(resultall,month==4&state_id_CA==1&dept_id_HOBBIES_1==1)
p42<-subset(resultall,month==4&state_id_TX==1&dept_id_HOBBIES_1==1)
p43<-subset(resultall,month==4&state_id_WI==1&dept_id_HOBBIES_1==1)
p44<-subset(resultall,month==4&state_id_CA==1&dept_id_HOBBIES_2==1)
p45<-subset(resultall,month==4&state_id_TX==1&dept_id_HOBBIES_2==1)
p46<-subset(resultall,month==4&state_id_WI==1&dept_id_HOBBIES_2==1)
p47<-subset(resultall,month==4&state_id_CA==1&dept_id_FOODS_1==1)
p48<-subset(resultall,month==4&state_id_TX==1&dept_id_FOODS_1==1)
p49<-subset(resultall,month==4&state_id_WI==1&dept_id_FOODS_1==1)
p410<-subset(resultall,month==4&state_id_CA==1&dept_id_FOODS_2==1)
p411<-subset(resultall,month==4&state_id_TX==1&dept_id_FOODS_2==1)
p412<-subset(resultall,month==4&state_id_WI==1&dept_id_FOODS_2==1)
p413<-subset(resultall,month==4&state_id_CA==1&dept_id_FOODS_3==1)
p414<-subset(resultall,month==4&state_id_TX==1&dept_id_FOODS_3==1)
p415<-subset(resultall,month==4&state_id_WI==1&dept_id_FOODS_3==1)
p416<-subset(resultall,month==4&state_id_CA==1&dept_id_HOUSEHOLD_1==1)
p417<-subset(resultall,month==4&state_id_TX==1&dept_id_HOUSEHOLD_1==1)
p418<-subset(resultall,month==4&state_id_WI==1&dept_id_HOUSEHOLD_1==1)
p419<-subset(resultall,month==4&state_id_CA==1&dept_id_HOUSEHOLD_2==1)
p420<-subset(resultall,month==4&state_id_TX==1&dept_id_HOUSEHOLD_2==1)
p421<-subset(resultall,month==4&state_id_WI==1&dept_id_HOUSEHOLD_2==1)

c1<-sum(p41$sell)
c2<-sum(p42$sell)
c3<-sum(p43$sell)
c4<-sum(p44$sell)
c5<-sum(p45$sell)
c6<-sum(p46$sell)
c7<-sum(p47$sell)
c8<-sum(p48$sell)
c9<-sum(p49$sell)
c10<-sum(p410$sell)
c11<-sum(p411$sell)
c12<-sum(p412$sell)
c13<-sum(p413$sell)
c14<-sum(p414$sell)
c15<-sum(p415$sell)
c16<-sum(p416$sell)
c17<-sum(p417$sell)
c18<-sum(p418$sell)
c19<-sum(p419$sell)
c20<-sum(p420$sell)
c21<-sum(p421$sell)
predicapr<-c(sell41,sell42,sell43,sell44,sell45,sell46,sell47,sell48,sell49,sell410,
             sell411,sell412,sell413,sell414,sell415,sell416,sell417,sell418,sell419,
             sell420,sell421)
realapr<-c(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,
           c18,c19,c20,c21)
apr<-data.frame(predicapr,realapr)
apr

realll<-data.frame(real,realmar,realapr)
realll
kmeansxgboost<-data.frame(predicfeb,predicmar,predicapr)
write.csv(kmeansxgboost,"E:/kmeans1.csv")
write.csv(realll,"E:/real.csv")
#####2.one hot encoding+dbscan+svr
library(clustMixType)
library(fpc)
library(dbscan)
traintyy<-one_hot(as.data.table(trainty))
dim(trainty)
set.seed(222)
db = fpc::dbscan(traintyy, eps = 0.15, MinPts = 5)
plot(db, traintyy, main = "DBSCAN", frame = FALSE)

###contrast running speed of dbscan/kprototype/without clustering###
###contrast clusterting efficacy of dbscan/kprototype 