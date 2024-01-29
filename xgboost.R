
##原本的總共資料####data44####
predictdata<-rbind(trainone,valione)
dim(predictdata) ##1266941 29

###cluster1 切成train與validation###
smp.sizeall=floor(0.8*nrow(predictdata)) 
set.seed(1)                     
train.indall=sample(seq_len(nrow(predictdata)), smp.sizeall)
trainall1=predictdata[train.indall,] 
valiall1=predictdata[-train.indall,]
dim(trainall1) ##1013552
dim(valiall1)  ##253389
str(trainall)
trainall<-trainall1[,-1]
trainall
valiall<-valiall1[,-1]


# convert your dataframes (except for the "sell" column) into matrices and then Dmatrixs
train_dataall <- data.matrix(trainall[, -1]) 
train_labelall <- trainall$sell
dtrainall <- xgb.DMatrix(data = train_dataall, label= train_labelall)

test_dataall <- data.matrix(valiall[, -1])
test_labelall <- valiall$sell
dtestall <- xgb.DMatrix(data = test_dataall, label= test_labelall)
paramsall <- list(booster="gbtree",objective = "reg:squarederror",
                eval_metric="rmse",subsample=1,gamma=0.01,min_child_weight=20,
                colsample_bytree=1,max_depth=20,eta=0.01)
#Calculate of folds for cross-validation#
xgbcvall <- xgb.cv(params = paramsall, data =dtrainall, nrounds = 100, nfold = 10, 
                   showsd = TRUE, 
                 stratified = TRUE, 
                 print_every_n = 20, early_stop_round = 20, 
                 maximize = FALSE, prediction = TRUE)

 #建立模型#
xgb.modelall = xgb.train(paras=paramsall,data=dtrainall,
                       nrounds=100)

##重要特徵畫圖##
imp_feartureall <- xgb.importance(colnames(dtrainall), model = xgb.modelall)
print(imp_feartureall)
xgb.ggplot.importance(imp_feartureall)

##prediction train
train.predall = predict(xgb.modelall, dtrainall)
mseall = mean((train_labelall - train.predall)^2)
mseall  #0.016
maeall = caret::MAE(train_labelall, train.predall)
maeall #0.065
rmseall = caret::RMSE(train_labelall, train.predall)
rmseall #0.1275
postResample(pred =train.predall, obs = train_labelall)


##prediction validation
valipredall=predict(xgb.modelall,dtestall)
mseall = mean((test_labelall - valipredall)^2)
mseall #3.6
maeall = caret::MAE(test_labelall, valipredall)
maeall  #0.08
rmseall = caret::RMSE(test_labelall, valipredall)
rmseall  #1.89
postResample(pred =valipredall, obs = test_labelall)

###2016年2月到2016年4月###
test<-subset(kprodata,Year==2016 & month>=2)
test
test2<-test[,-(3:6)]
testone<-one_hot(as.data.table(test))
tail(testone)
dim(test) ##85361筆

predict<-subset(test2,select=-c(sell))
dim(predict)
predict<-as.matrix(predict)
sapply(predict, mode)

result2<-predict(xgb.modelall,predict)
result2

rea<-subset(testone,select=-c(sell))
rea
resultall<-data.frame(result2,rea)
resultall
str(rea)
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

p21<-subset(testone,month==2&state_id_CA==1&dept_id_HOBBIES_1==1)
p22<-subset(testone,month==2&state_id_TX==1&dept_id_HOBBIES_1==1)
p23<-subset(testone,month==2&state_id_WI==1&dept_id_HOBBIES_1==1)
p24<-subset(testone,month==2&state_id_CA==1&dept_id_HOBBIES_2==1)
p25<-subset(testone,month==2&state_id_TX==1&dept_id_HOBBIES_2==1)
p26<-subset(testone,month==2&state_id_WI==1&dept_id_HOBBIES_2==1)
p27<-subset(testone,month==2&state_id_CA==1&dept_id_FOODS_1==1)
p28<-subset(testone,month==2&state_id_TX==1&dept_id_FOODS_1==1)
p29<-subset(testone,month==2&state_id_WI==1&dept_id_FOODS_1==1)
p210<-subset(testone,month==2&state_id_CA==1&dept_id_FOODS_2==1)
p211<-subset(testone,month==2&state_id_TX==1&dept_id_FOODS_2==1)
p212<-subset(testone,month==2&state_id_WI==1&dept_id_FOODS_2==1)
p213<-subset(testone,month==2&state_id_CA==1&dept_id_FOODS_3==1)
p214<-subset(testone,month==2&state_id_TX==1&dept_id_FOODS_3==1)
p215<-subset(testone,month==2&state_id_WI==1&dept_id_FOODS_3==1)
p216<-subset(testone,month==2&state_id_CA==1&dept_id_HOUSEHOLD_1==1)
p217<-subset(testone,month==2&state_id_TX==1&dept_id_HOUSEHOLD_1==1)
p218<-subset(testone,month==2&state_id_WI==1&dept_id_HOUSEHOLD_1==1)
p219<-subset(testone,month==2&state_id_CA==1&dept_id_HOUSEHOLD_2==1)
p220<-subset(testone,month==2&state_id_TX==1&dept_id_HOUSEHOLD_2==1)
p221<-subset(testone,month==2&state_id_WI==1&dept_id_HOUSEHOLD_2==1)

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
postResample(pred =predicfeb, obs =real)


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

p31<-subset(testone,month==3&state_id_CA==1&dept_id_HOBBIES_1==1)
p32<-subset(testone,month==3&state_id_TX==1&dept_id_HOBBIES_1==1)
p33<-subset(testone,month==3&state_id_WI==1&dept_id_HOBBIES_1==1)
p34<-subset(testone,month==3&state_id_CA==1&dept_id_HOBBIES_2==1)
p35<-subset(testone,month==3&state_id_TX==1&dept_id_HOBBIES_2==1)
p36<-subset(testone,month==3&state_id_WI==1&dept_id_HOBBIES_2==1)
p37<-subset(testone,month==3&state_id_CA==1&dept_id_FOODS_1==1)
p38<-subset(testone,month==3&state_id_TX==1&dept_id_FOODS_1==1)
p39<-subset(testone,month==3&state_id_WI==1&dept_id_FOODS_1==1)
p310<-subset(testone,month==3&state_id_CA==1&dept_id_FOODS_2==1)
p311<-subset(testone,month==3&state_id_TX==1&dept_id_FOODS_2==1)
p312<-subset(testone,month==3&state_id_WI==1&dept_id_FOODS_2==1)
p313<-subset(testone,month==3&state_id_CA==1&dept_id_FOODS_3==1)
p314<-subset(testone,month==3&state_id_TX==1&dept_id_FOODS_3==1)
p315<-subset(testone,month==3&state_id_WI==1&dept_id_FOODS_3==1)
p316<-subset(testone,month==3&state_id_CA==1&dept_id_HOUSEHOLD_1==1)
p317<-subset(testone,month==3&state_id_TX==1&dept_id_HOUSEHOLD_1==1)
p318<-subset(testone,month==3&state_id_WI==1&dept_id_HOUSEHOLD_1==1)
p319<-subset(testone,month==3&state_id_CA==1&dept_id_HOUSEHOLD_2==1)
p320<-subset(testone,month==3&state_id_TX==1&dept_id_HOUSEHOLD_2==1)
p321<-subset(testone,month==3&state_id_WI==1&dept_id_HOUSEHOLD_2==1)

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
postResample(pred =predicmar, obs =realmar)




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


p41<-subset(testone,month==4&state_id_CA==1&dept_id_HOBBIES_1==1)
p42<-subset(testone,month==4&state_id_TX==1&dept_id_HOBBIES_1==1)
p43<-subset(testone,month==4&state_id_WI==1&dept_id_HOBBIES_1==1)
p44<-subset(testone,month==4&state_id_CA==1&dept_id_HOBBIES_2==1)
p45<-subset(testone,month==4&state_id_TX==1&dept_id_HOBBIES_2==1)
p46<-subset(testone,month==4&state_id_WI==1&dept_id_HOBBIES_2==1)
p47<-subset(testone,month==4&state_id_CA==1&dept_id_FOODS_1==1)
p48<-subset(testone,month==4&state_id_TX==1&dept_id_FOODS_1==1)
p49<-subset(testone,month==4&state_id_WI==1&dept_id_FOODS_1==1)
p410<-subset(testone,month==4&state_id_CA==1&dept_id_FOODS_2==1)
p411<-subset(testone,month==4&state_id_TX==1&dept_id_FOODS_2==1)
p412<-subset(testone,month==4&state_id_WI==1&dept_id_FOODS_2==1)
p413<-subset(testone,month==4&state_id_CA==1&dept_id_FOODS_3==1)
p414<-subset(testone,month==4&state_id_TX==1&dept_id_FOODS_3==1)
p415<-subset(testone,month==4&state_id_WI==1&dept_id_FOODS_3==1)
p416<-subset(testone,month==4&state_id_CA==1&dept_id_HOUSEHOLD_1==1)
p417<-subset(testone,month==4&state_id_TX==1&dept_id_HOUSEHOLD_1==1)
p418<-subset(testone,month==4&state_id_WI==1&dept_id_HOUSEHOLD_1==1)
p419<-subset(testone,month==4&state_id_CA==1&dept_id_HOUSEHOLD_2==1)
p420<-subset(testone,month==4&state_id_TX==1&dept_id_HOUSEHOLD_2==1)
p421<-subset(testone,month==4&state_id_WI==1&dept_id_HOUSEHOLD_2==1)

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
postResample(pred =predicapr, obs =realapr)


singlexgboost<-data.frame(predicfeb,predicmar,predicapr)
write.csv(singlexgboost,"E:/singlexg.csv")
realvalue<-read.csv("D:/report/train2.csv")
dim(realvalue)
feb<-c(as.numeric(realvalue[61,4:24]))
feb
pre2<-data.frame(predicfeb,feb)
pre2

mar<-c(as.numeric(realvalue[62,4:24]))
mar
pre3<-data.frame(predicmar,mar)
pre3

apr<-c(as.numeric(realvalue[63,4:24]))
apr
pre4<-data.frame(predicapr,apr)
pre4

