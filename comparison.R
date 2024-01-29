setwd("E:/")
single<-read.csv("E:/singlexg.csv")
kmeans<-read.csv("E:/kmeans1.csv")
true<-read.csv("D:/report/train2.csv")
real<-read.csv("E:/real.csv")

singfeb<-c(single[,2])
singfeb
singmar<-c(single[,3])
singapr<-c(single[,4])

kfeb<-c(kmeans[,2])
kmar<-c(kmeans[,3])
kapr<-c(kmeans[,4])

dim(true)
tail(true)
truefeb<-c(as.numeric(true[61,4:24]))
truemar<-c(as.numeric(true[62,4:24]))
trueapr<-c(as.numeric(true[63,4:24]))

#realfeb<-c(as.numeric(real[,2]))
#realmar<-c(as.numeric(real[,3]))
#realapr<-c(as.numeric(real[,4]))



tsfeb<-c(224662.8,168258.9,100211.3,9073.8,7671.9,5819.6,113606.3,
         68208.1,76125.9,221792.5,133276.6,235861.9,533492.9,304970.3,
         367622.6,372192.3,271868.7,228059.4,133089.9,73418.7,60446.3)

tsmar<-c(237717.1,173194.3,104235.7,9814.4,8718.8,6174.4,107261.9,66877.4,
         76577.4,237420.8,151986.8,243363.5,600439.1,374376.2,379567.6,
         416474.8,298183.4,243774.7,156377,81700.4,68062.8)

tsapr<-c(224372.5,156462.9,105161.9,10310.6,7150.8,5876.6,109539.3,71340,
         83153.1,228775.6,141563.4,235411,590232.6,402498.2,396867.1,
         389054,273678,222803.2,140473,77794.9,56187.7)


library(Metrics)
library(MLmetrics)

ape(truefeb,singfeb)
ape(truefeb,kfeb)

mae(truefeb,singfeb)
mae(truefeb,kfeb)

mape(truefeb,singfeb)
mape(truefeb,kfeb)

mse(truefeb,singfeb)
mse(truefeb,kfeb)



ape(truemar,singmar)
ape(truemar,kmar)

mae(truemar,singmar)
mae(truemar,kmar)

mape(truemar,singmar)
mape(truemar,kmar)

mse(truemar,singmar)
mse(truemar,kmar)



data.frame(RMSPE(truefeb,kfeb),RMSPE(truefeb,singfeb),RMSPE(truemar,tsfeb))
data.frame(RMSPE(truemar,kmar),RMSPE(truemar,singmar),RMSPE(truemar,tsmar))
#data.frame(RMSPE(trueapr,kapr),RMSPE(trueapr,singapr),RMSPE(trueapr,tsapr))

data.frame(rmse(truefeb,kfeb),rmse(truefeb,singfeb),rmse(truemar,tsfeb))
data.frame(rmse(truemar,kmar),rmse(truemar,singmar),rmse(truemar,tsmar))
#data.frame(rmse(trueapr,kapr),rmse(trueapr,singapr),rmse(trueapr,tsapr))

data.frame(mape(truefeb,kfeb),mape(truefeb,singfeb),mape(truemar,tsfeb))
data.frame(mape(truemar,kmar),mape(truemar,singmar),mape(truemar,tsmar))
#data.frame(RMSPE(realfeb,kfeb),RMSPE(realfeb,singfeb),RMSPE(realmar,tsfeb))
#data.frame(RMSPE(realmar,kmar),RMSPE(realmar,singmar),RMSPE(realmar,tsmar))
#data.frame(RMSPE(realapr,kapr),RMSPE(realapr,singapr),RMSPE(realapr,tsapr))

#data.frame(rmse(realfeb,kfeb),rmse(realfeb,singfeb),rmse(realmar,tsfeb))
#data.frame(rmse(realmar,kmar),rmse(realmar,singmar),rmse(realmar,tsmar))
#data.frame(rmse(realapr,kapr),rmse(realapr,singapr),rmse(realapr,tsapr))
data.frame(mae(truefeb,kfeb),mae(truefeb,singfeb),mae(truemar,tsfeb))
data.frame(mae(truemar,kmar),mae(truemar,singmar),mae(truemar,tsmar))

