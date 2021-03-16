#预测博士人数 
###########################ARIMA
#基于10年的数据进行预测
Data2<-read.delim(file="C:/Users/lenovo/Desktop/毕业论文/10年高等中医药学院校学生总数.txt",header=TRUE)
doctor=ts(Data2$博士,start=c(2007))
plot(doctor,main="在校博士人数") 
library("tseries")
adf.test(doctor)
d1<-diff(doctor,1)
plot(d1,main="一阶差分")
adf.test(d1)
d2<-diff(doctor,2)
plot(d2,main="二阶差分")
adf.test(d2)
#d=2
acf(d2)  
#q=0
pacf(d2)
#p=0
#模型arima(0,2,0)
model<-arima(doctor,order=c(0,2,0))
Box.test(model$residuals)
predict(model,n.ahead=3)#基于10年的数据使用arima模型进行预测  

#基于15年的数据进行预测 
Data2<-read.delim(file="C:/Users/lenovo/Desktop/毕业论文/15年高等中医药学院校学生总数.txt",header=TRUE)
doctor=ts(Data2$博士,start=c(2002))
plot(doctor,main="在校博士人数") 
library("tseries")
adf.test(doctor)
#d=0
acf(doctor)  
#q=2
pacf(doctor)
#p=1
#模型arima(1,0,2)
model<-arima(doctor,order=c(1,0,2))
Box.test(model$residuals)
predict(model,n.ahead=3)#基于15年的数据使用arima模型进行预测  



####################################  霍尔特指数平滑法 
install.packages("forecast")
 #基于10年的数据进行预测
Data2<-read.delim(file="C:/Users/lenovo/Desktop/毕业论文/10年高等中医药学院校学生总数.txt",header=TRUE)
doctor=ts(Data2$博士,start=c(2007))
plot(doctor,main="在校博士人数")
forecasts<-HoltWinters(doctor,gamma=FALSE)
forecasts
forecasts$SSE
plot(forecasts)
HoltWinters(doctor,gamma=FALSE,l.start=3255,b.start=239)
library("forecast")
forecasts2=forecast(forecasts,h=3)
forecasts2
plot(forecasts2) #基于10年的数据使用Holt平滑法进行预测  

#基于15年的数据进行预测
Data2<-read.delim(file="C:/Users/lenovo/Desktop/毕业论文/15年高等中医药学院校学生总数.txt",header=TRUE)
doctor=ts(Data2$博士,start=c(2002),end=c(2014))
plot(doctor,main="在校博士人数")
forecasts<-HoltWinters(doctor,gamma=FALSE)
forecasts
forecasts$SSE
plot(forecasts)
HoltWinters(doctor,gamma=FALSE,l.start=1515,b.start=475)
library("forecast")
forecasts2=forecast(forecasts,h=3)
forecasts2
plot(forecasts2)  #基于15年的数据使用Holt平滑法进行预测 




#预测硕士人数 



###########################ARIMA
#基于10年的数据进行预测
Data2<-read.delim(file="C:/Users/lenovo/Desktop/毕业论文/10年高等中医药学院校学生总数.txt",header=TRUE)
master=ts(Data2$硕士,start=c(2007))
plot(master,main="在校硕士人数") 
library("tseries")
adf.test(master)
d1<-diff(master,1)
plot(d1,main="一阶差分")
adf.test(d1)
d2<-diff(master,2)
plot(d2,main="二阶差分")
adf.test(d2)
#不能预测 

#基于15年的数据进行预测 
Data2<-read.delim(file="C:/Users/lenovo/Desktop/毕业论文/15年高等中医药学院校学生总数.txt",header=TRUE)
master=ts(Data2$硕士,start=c(2002))
plot(master,main="在校博士人数") 
library("tseries")
adf.test(master)
d1<-diff(master,1)
plot(d1,main="一阶差分")
adf.test(d1)
d2<-diff(master,2)
plot(d2,main="二阶差分")
adf.test(d2)
#不能预测 


####################################  霍尔特指数平滑法 
install.packages("forecast")
 #基于10年的数据进行预测
Data2<-read.delim(file="C:/Users/lenovo/Desktop/毕业论文/10年高等中医药学院校学生总数.txt",header=TRUE)
master=ts(Data2$硕士,start=c(2007),end=c(2014))
plot(master,main="在校硕士人数")
forecasts<-HoltWinters(ps,gamma=FALSE)
forecasts
forecasts$SSE
plot(forecasts)
HoltWinters(ps,gamma=FALSE,l.start=20937,b.start=2306)
library("forecast")
forecasts2=forecast(forecasts,h=5)
forecasts2
plot(forecasts2) #基于10年的数据使用Holt平滑法进行预测  （不够准确） 

#基于15年的数据进行预测
Data2<-read.delim(file="C:/Users/lenovo/Desktop/毕业论文/15年高等中医药学院校学生总数.txt",header=TRUE)
master=ts(Data2$硕士,start=c(2002))
plot(master,main="在校硕士人数")
forecasts<-HoltWinters(master,gamma=FALSE)
forecasts
forecasts$SSE
plot(forecasts)
HoltWinters(master,gamma=FALSE,l.start=6391,b.start=2006)
library("forecast")
forecasts2=forecast(forecasts,h=3)
forecasts2
plot(forecasts2)  #基于15年的数据使用Holt平滑法进行预测  （较好） 


 #预测本科生


 
###########################ARIMA
#基于10年的数据进行预测
Data2<-read.delim(file="C:/Users/lenovo/Desktop/毕业论文/10年高等中医药学院校学生总数.txt",header=TRUE)
undergraduate=ts(Data2$普通本科,start=c(2007))
plot(undergraduate,main="在校本科生人数") 
library("tseries")
adf.test(undergraduate)
d1<-diff(undergraduate,1)
plot(d1,main="一阶差分")
adf.test(d1)
d2<-diff(undergraduate,2)
plot(d2,main="二阶差分")
adf.test(d2)
#不能预测 

#基于15年的数据进行预测 
Data2<-read.delim(file="C:/Users/lenovo/Desktop/毕业论文/15年高等中医药学院校学生总数.txt",header=TRUE)
undergraduate=ts(Data2$普通本科,start=c(2002))
plot(undergraduate,main="在校本科生人数") 
library("tseries")
adf.test(undergraduate)
d1<-diff(undergraduate,1)
plot(d1,main="一阶差分")
adf.test(d1)
#d=1
acf(undergraduate)  
#q=2
pacf(undergraduate)
#p=1
#模型arima(1,1,2)
model<-arima(undergraduate,order=c(1,1,2))
Box.test(model$residuals)
predict(model,n.ahead=3)#基于15年的数据使用arima模型进行预测  




####################################  霍尔特指数平滑法 
install.packages("forecast")
 #基于10年的数据进行预测
Data2<-read.delim(file="C:/Users/lenovo/Desktop/毕业论文/10年高等中医药学院校学生总数.txt",header=TRUE)
undergraduate=ts(Data2$普通本科,start=c(2007),end=c(2014))
plot(undergraduate,main="在校本科生人数")
forecasts<-HoltWinters(undergraduate,gamma=FALSE)
forecasts
forecasts$SSE
plot(forecasts)
HoltWinters(undergraduate,gamma=FALSE,l.start=261407,b.start=24892)
library("forecast")
forecasts2=forecast(forecasts,h=5)
forecasts2
plot(forecasts2) #基于10年的数据使用Holt平滑法进行预测  

#基于15年的数据进行预测
Data2<-read.delim(file="C:/Users/lenovo/Desktop/毕业论文/15年高等中医药学院校学生总数.txt",header=TRUE)
undergraduate=ts(Data2$普通本科,start=c(2002))
plot(undergraduate,main="在校本科生人数")
forecasts<-HoltWinters(undergraduate,gamma=FALSE)
forecasts
forecasts$SSE
plot(forecasts)
HoltWinters(doctor,gamma=FALSE,l.start=108591,b.start=33645)
library("forecast")
forecasts2=forecast(forecasts,h=3)
forecasts2
plot(forecasts2)  #基于15年的数据使用Holt平滑法进行预测  （更准确） 






#聚类
# 读入数据
Data<- read.csv("C:/Users/lenovo/Desktop/毕业论文/分专业人数(1).csv", header = TRUE)

#将第2到第4列进行kmeans聚类。
newData=Data[,2:4]
(CluR=kmeans(newData,3))
#创建一个连续表，在三个聚类中分别统计各种学位出现的次数。
table(Data$学位,CluR $cluster)
#根据最后的聚类结果画出散点图，数据为结果集中的列"本科"和"硕士"，颜色为用1，2，3表示的缺省颜色
plot(newData$本科, newData$硕士,col=CluR$cluster)
#根据最后的聚类结果画出散点图，数据为结果集中的列"本科"和"博士"，颜色为用1，2，3表示的缺省颜色
#plot(newData$本科,newData$博士,col=CluR$cluster)
#根据最后的聚类结果画出散点图，数据为结果集中的列"硕士"和"博士"，颜色为用1，2，3表示的缺省颜色
#plot(newData$硕士,newData$博士,col=CluR$cluster)
#在图上标出每个聚类的中心点。
points(CluR$centers,col=1:3,pch=8,cex=2)
#可视化
library(ggplot2)
library(ggfortify)
library(cluster)
autoplot(pam(Data[-1], 3), frame = TRUE, frame.type = 'norm') 


#将第2到第4列进行PAM聚类。
install.packages("cluster") 
library("cluster")
newData=Data[,2:4]
(PClu=pam(newData,k=3,do.swap=TRUE,stand=FALSE))