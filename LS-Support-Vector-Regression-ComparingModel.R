library (car) # for VIF
library(carData)
library(xlsx)
library(quantreg)
library(ggplot2)
library(Metrics)
library(ggpubr)
library(magrittr)
library(e1071)
library(caret)
library(lattice)
library(fRegression)
library(timeDate)
library(timeSeries)
library(fBasics)
library(nortest)

dat<-read.xlsx("E:\\Magister ITS\\tugas\\Analisis Data\\qsar_aquatic_toxicity.xlsx",1)
colnames(dat)<-c("X1","X2","X3","X4","X5","X6","X7","X8","Y")
head(dat)
#summary
summary(dat)
XVar<-dat[,-9]
round(cor(XVar),2)

#Data Training dan Tes
training.samples <- dat$Y %>%
  createDataPartition(p = 0.95, list = FALSE)
#Diambil dari sampling
train.data<-read.xlsx("E:\\Magister ITS\\tugas\\Analisis Data\\Data.Train74.xlsx",1)
test.data<-read.xlsx("E:\\Magister ITS\\tugas\\Analisis Data\\Data.Test74.xlsx",1)


#Cara Manual
n.train<-round(0.8*length(dat$Y))
n<-nrow(dat)
train.data.m  <- dat[1:n.train, ]
test.data.m <- dat[(n.train+1):n, ]
n.test<-nrow(test.data)
#EDA
#cek missing data
table(is.na(train.data))

#histogram Masing-masing variabel 
attach(train.data)
png ( filename = " Hist.png", width = 1000 ,
      height = 1000 , units = "px", pointsize = 12,
      bg = " white ", res = NA , restoreConsole = TRUE )
par(mfrow=c(3,3))
hist(train.data$Y, col=" lightblue ")
hist(train.data$X1, col=" lightblue ")
hist(train.data$X2, col=" lightblue ")
hist(train.data$X3, col=" lightblue ")
hist(train.data$X4, col=" lightblue ")
hist(train.data$X5, col=" lightblue ")
hist(train.data$X6, col=" lightblue ")
hist(train.data$X7, col=" lightblue ")
hist(train.data$X8, col=" lightblue ")
detach(train.data)
dev.off()

#Plot Variable Prediktor dengan Respons
attach(train.data)
png ( filename = "PlotXY.png", width = 1000 ,
      height = 1000 , units = "px", pointsize = 12,
      bg = " white ", res = NA , restoreConsole = TRUE )
par(mfrow=c(3,3))
plot (train.data$X1,train.data$Y, type ="p", pch=9, col ="red", main = "plot X1 dan Y")
plot(train.data$X2,train.data$Y,type ="p", pch=9, col ="red", main = "plot X2 dan Y")
plot(train.data$X3,train.data$Y,type ="p", pch=9, col ="red", main = "plot X3 dan Y")
plot(train.data$X4,train.data$Y,type ="p", pch=9, col ="red", main = "plot X4 dan Y")
plot(train.data$X5,train.data$Y,type ="p", pch=9, col ="red", main = "plot X5 dan Y")
plot(train.data$X6,train.data$Y,type ="p", pch=9, col ="red", main = "plot X6 dan Y")
plot(train.data$X7,train.data$Y,type ="p", pch=9, col ="red", main = "plot X7 dan Y")
plot(train.data$X8,train.data$Y,type ="p", pch=9, col ="red", main = "plot X8 dan Y")
dev.off()
detach(train.data)

#Cek Outlier
attach(train.data)
png ( filename = "Boxplot.png", width = 1000 ,
      height = 1000 , units = "px", pointsize = 12,
      bg = " white ", res = NA , restoreConsole = TRUE )
par(mfrow=c(3,3))
boxplot (Y, data =train.data, col= "lightblue")
boxplot (X1, data =train.data, col= "lightblue")
boxplot (X2, data =train.data, col= "lightblue")
boxplot (X3, data =train.data, col= "lightblue")
boxplot (X4, data =train.data, col= "lightblue")
boxplot (X5, data =train.data, col= "lightblue")
boxplot (X6, data =train.data, col= "lightblue")
boxplot (X7, data =train.data, col= "lightblue")
boxplot (X8, data =train.data, col= "lightblue")
dev.off()
detach(train.data)

#Menggunakan Regresi Linier
RegLin<-lm(Y~.,data = train.data)
summary(RegLin)
prediksi<-predict(RegLin,test.data)
RMSEReglin<-rmse(test.data$Y,prediksi)
RMSEReglin

#Testing linieritas model#
resetTest(Y~.,data = train.data)
#Testing Normalitas#
e<-RegLin$residuals
kstest.ols<-ks.test(e,"pnorm")
kstest.ols
#Testing Heteroskedastisitas --> Breush-Pagan Test#
bptest.ols<-lmTest(Y~.,data = train.data,"bp")
bptest.ols
#Testing Autokorelasi#
dwtest.ols<-lmTest(Y~.,data = train.data,"dw")
dwtest.ols
#Testing Multikolinieritas#
vif(RegLin)

#Regresi Kuantil
RegKu<-rq(Y~.,data = train.data)
summary(RegKu)
prediksi1<-predict(RegKu,test.data)
RMSERegKu<-RMSE(test.data$Y,prediksi1)
RMSERegKu

#SVR karnel Radial Basis
RegSVR<-svm(Y~.,data=train.data)
prediksi2<-predict(RegSVR,test.data)
RMSESVRRB<-rmse(test.data$Y,prediksi2)
RMSESVRRB

#Least-Square SVR
library(liquidSVM)
RegLS.SVR<-lsSVM(Y~.,train.data,display=1,max_gamma=25)
#estimasi parameter
RegLS.SVR$explanatory
RegLS.SVR$all_vars
RegLS.SVR$deleted
RegLS.SVR$dim
RegLS.SVR$formula
RegLS.SVR$gammas
RegLS.SVR$lambdas
ResultRegLS.SVR<-predict(RegLS.SVR,test.data)
prediksi3<-ResultRegLS.SVR[1:n.test]
RMSELSSVR<-rmse(test.data$Y,prediksi3)
RMSELSSVR


#RMSE
rmse<-function(y,yhat){
  n=length(y)
  rmse=sqrt(sum((y-yhat)^2)/n)
}
rmse1=rmse(test.data$Y,prediksi)
rmse2=rmse(test.data$Y,prediksi1)
rmse3=rmse(test.data$Y,prediksi2)
rmse4=rmse(test.data$Y,prediksi3)
RMSE=cbind.data.frame(RMSEReglin,RMSERegKu,RMSESVRRB,RMSELSSVR)
RMSE
colnames(RMSE)<-c("Linear","Quantile","SVR","LSSVR")
#R2
R2 <- function(y,yhat){
  Rsq=1-((sum((y-yhat)^2))/(sum((y-mean(y))^2)))
}
R2.ols=R2(test.data$Y,prediksi)
R2.Rq=R2(test.data$Y,prediksi1)
R2.SVR=R2(test.data$Y,prediksi2)
R2.LSSVR=R2(test.data$Y,prediksi3)
Rsquare=cbind.data.frame(R2.ols,R2.Rq,R2.SVR,R2.LSSVR)
Rsquare
colnames(Rsquare)<-c("Linear","Quantile","SVR","LSSVR")
#Comparing Model
Comparing.Model<-rbind.data.frame(RMSE,Rsquare)
rownames(Comparing.Model)<-c("RMSE","Rsquare")
Comparing.Model
#Buat Plot
y<-test.data$Y
x=seq(1:n.test)
obs=y
Linear=prediksi
Quantile=prediksi1
SVR=prediksi2
LSSVR=prediksi3
predict.comp<-cbind.data.frame(x,obs,Linear,Quantile,SVR,LSSVR)
#Plot jika bersama-sama, tapi hasilnya ga keliatan bedanya mas
predictMelted <- reshape2::melt(predict.comp, id.var='x')
head(predictMelted)
ggplot(predictMelted, aes(x=x, y=value, col=variable)) + geom_point()+geom_line()+
       scale_color_manual(values = c("black", "green","blue","purple","magenta"))
#plot jika dua-dua
#Linear
predict.comp1<-cbind.data.frame(x,obs,Linear)
predictMelted1 <- reshape2::melt(predict.comp1, id.var='x')
head(predictMelted1)
ggplot(predictMelted1, aes(x=x, y=value, col=variable)) + geom_point()+geom_line()+
       scale_color_manual(values = c("black", "green"))
#Quantile
predict.comp2<-cbind.data.frame(x,obs,Quantile)
predictMelted2 <- reshape2::melt(predict.comp2, id.var='x')
head(predictMelted2)
ggplot(predictMelted2, aes(x=x, y=value, col=variable)) + geom_point()+geom_line()+
  scale_color_manual(values = c("black", "blue"))
#SVR
predict.comp3<-cbind.data.frame(x,obs,SVR)
predictMelted3 <- reshape2::melt(predict.comp3, id.var='x')
head(predictMelted3)
ggplot(predictMelted3, aes(x=x, y=value, col=variable)) + geom_point()+geom_line()+
  scale_color_manual(values = c("black", "purple"))
#LSSVR
predict.comp4<-cbind.data.frame(x,obs,LSSVR)
predictMelted4 <- reshape2::melt(predict.comp4, id.var='x')
head(predictMelted4)
ggplot(predictMelted4, aes(x=x, y=value, col=variable)) + geom_point()+geom_line()+
  scale_color_manual(values = c("black", "magenta"))

write.xlsx(prediksi3,'E:\\Magister ITS\\tugas\\Analisis Data\\prediksilssvr.xlsx',row.names = FALSE)
write.xlsx(train.data,'E:\\Magister ITS\\tugas\\Analisis Data\\Data.Train74.xlsx',row.names = FALSE)
write.xlsx(test.data,'E:\\Magister ITS\\tugas\\Analisis Data\\Data.Test74.xlsx',row.names = FALSE)
