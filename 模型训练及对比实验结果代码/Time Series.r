#Ԥ�ⲩʿ���� 
###########################ARIMA
#����10������ݽ���Ԥ��
Data2<-read.delim(file="C:/Users/lenovo/Desktop/��ҵ����/10��ߵ���ҽҩѧԺУѧ������.txt",header=TRUE)
doctor=ts(Data2$��ʿ,start=c(2007))
plot(doctor,main="��У��ʿ����") 
library("tseries")
adf.test(doctor)
d1<-diff(doctor,1)
plot(d1,main="һ�ײ��")
adf.test(d1)
d2<-diff(doctor,2)
plot(d2,main="���ײ��")
adf.test(d2)
#d=2
acf(d2)  
#q=0
pacf(d2)
#p=0
#ģ��arima(0,2,0)
model<-arima(doctor,order=c(0,2,0))
Box.test(model$residuals)
predict(model,n.ahead=3)#����10�������ʹ��arimaģ�ͽ���Ԥ��  

#����15������ݽ���Ԥ�� 
Data2<-read.delim(file="C:/Users/lenovo/Desktop/��ҵ����/15��ߵ���ҽҩѧԺУѧ������.txt",header=TRUE)
doctor=ts(Data2$��ʿ,start=c(2002))
plot(doctor,main="��У��ʿ����") 
library("tseries")
adf.test(doctor)
#d=0
acf(doctor)  
#q=2
pacf(doctor)
#p=1
#ģ��arima(1,0,2)
model<-arima(doctor,order=c(1,0,2))
Box.test(model$residuals)
predict(model,n.ahead=3)#����15�������ʹ��arimaģ�ͽ���Ԥ��  



####################################  ������ָ��ƽ���� 
install.packages("forecast")
 #����10������ݽ���Ԥ��
Data2<-read.delim(file="C:/Users/lenovo/Desktop/��ҵ����/10��ߵ���ҽҩѧԺУѧ������.txt",header=TRUE)
doctor=ts(Data2$��ʿ,start=c(2007))
plot(doctor,main="��У��ʿ����")
forecasts<-HoltWinters(doctor,gamma=FALSE)
forecasts
forecasts$SSE
plot(forecasts)
HoltWinters(doctor,gamma=FALSE,l.start=3255,b.start=239)
library("forecast")
forecasts2=forecast(forecasts,h=3)
forecasts2
plot(forecasts2) #����10�������ʹ��Holtƽ��������Ԥ��  

#����15������ݽ���Ԥ��
Data2<-read.delim(file="C:/Users/lenovo/Desktop/��ҵ����/15��ߵ���ҽҩѧԺУѧ������.txt",header=TRUE)
doctor=ts(Data2$��ʿ,start=c(2002),end=c(2014))
plot(doctor,main="��У��ʿ����")
forecasts<-HoltWinters(doctor,gamma=FALSE)
forecasts
forecasts$SSE
plot(forecasts)
HoltWinters(doctor,gamma=FALSE,l.start=1515,b.start=475)
library("forecast")
forecasts2=forecast(forecasts,h=3)
forecasts2
plot(forecasts2)  #����15�������ʹ��Holtƽ��������Ԥ�� 




#Ԥ��˶ʿ���� 



###########################ARIMA
#����10������ݽ���Ԥ��
Data2<-read.delim(file="C:/Users/lenovo/Desktop/��ҵ����/10��ߵ���ҽҩѧԺУѧ������.txt",header=TRUE)
master=ts(Data2$˶ʿ,start=c(2007))
plot(master,main="��У˶ʿ����") 
library("tseries")
adf.test(master)
d1<-diff(master,1)
plot(d1,main="һ�ײ��")
adf.test(d1)
d2<-diff(master,2)
plot(d2,main="���ײ��")
adf.test(d2)
#����Ԥ�� 

#����15������ݽ���Ԥ�� 
Data2<-read.delim(file="C:/Users/lenovo/Desktop/��ҵ����/15��ߵ���ҽҩѧԺУѧ������.txt",header=TRUE)
master=ts(Data2$˶ʿ,start=c(2002))
plot(master,main="��У��ʿ����") 
library("tseries")
adf.test(master)
d1<-diff(master,1)
plot(d1,main="һ�ײ��")
adf.test(d1)
d2<-diff(master,2)
plot(d2,main="���ײ��")
adf.test(d2)
#����Ԥ�� 


####################################  ������ָ��ƽ���� 
install.packages("forecast")
 #����10������ݽ���Ԥ��
Data2<-read.delim(file="C:/Users/lenovo/Desktop/��ҵ����/10��ߵ���ҽҩѧԺУѧ������.txt",header=TRUE)
master=ts(Data2$˶ʿ,start=c(2007),end=c(2014))
plot(master,main="��У˶ʿ����")
forecasts<-HoltWinters(ps,gamma=FALSE)
forecasts
forecasts$SSE
plot(forecasts)
HoltWinters(ps,gamma=FALSE,l.start=20937,b.start=2306)
library("forecast")
forecasts2=forecast(forecasts,h=5)
forecasts2
plot(forecasts2) #����10�������ʹ��Holtƽ��������Ԥ��  ������׼ȷ�� 

#����15������ݽ���Ԥ��
Data2<-read.delim(file="C:/Users/lenovo/Desktop/��ҵ����/15��ߵ���ҽҩѧԺУѧ������.txt",header=TRUE)
master=ts(Data2$˶ʿ,start=c(2002))
plot(master,main="��У˶ʿ����")
forecasts<-HoltWinters(master,gamma=FALSE)
forecasts
forecasts$SSE
plot(forecasts)
HoltWinters(master,gamma=FALSE,l.start=6391,b.start=2006)
library("forecast")
forecasts2=forecast(forecasts,h=3)
forecasts2
plot(forecasts2)  #����15�������ʹ��Holtƽ��������Ԥ��  ���Ϻã� 


 #Ԥ�Ȿ����


 
###########################ARIMA
#����10������ݽ���Ԥ��
Data2<-read.delim(file="C:/Users/lenovo/Desktop/��ҵ����/10��ߵ���ҽҩѧԺУѧ������.txt",header=TRUE)
undergraduate=ts(Data2$��ͨ����,start=c(2007))
plot(undergraduate,main="��У����������") 
library("tseries")
adf.test(undergraduate)
d1<-diff(undergraduate,1)
plot(d1,main="һ�ײ��")
adf.test(d1)
d2<-diff(undergraduate,2)
plot(d2,main="���ײ��")
adf.test(d2)
#����Ԥ�� 

#����15������ݽ���Ԥ�� 
Data2<-read.delim(file="C:/Users/lenovo/Desktop/��ҵ����/15��ߵ���ҽҩѧԺУѧ������.txt",header=TRUE)
undergraduate=ts(Data2$��ͨ����,start=c(2002))
plot(undergraduate,main="��У����������") 
library("tseries")
adf.test(undergraduate)
d1<-diff(undergraduate,1)
plot(d1,main="һ�ײ��")
adf.test(d1)
#d=1
acf(undergraduate)  
#q=2
pacf(undergraduate)
#p=1
#ģ��arima(1,1,2)
model<-arima(undergraduate,order=c(1,1,2))
Box.test(model$residuals)
predict(model,n.ahead=3)#����15�������ʹ��arimaģ�ͽ���Ԥ��  




####################################  ������ָ��ƽ���� 
install.packages("forecast")
 #����10������ݽ���Ԥ��
Data2<-read.delim(file="C:/Users/lenovo/Desktop/��ҵ����/10��ߵ���ҽҩѧԺУѧ������.txt",header=TRUE)
undergraduate=ts(Data2$��ͨ����,start=c(2007),end=c(2014))
plot(undergraduate,main="��У����������")
forecasts<-HoltWinters(undergraduate,gamma=FALSE)
forecasts
forecasts$SSE
plot(forecasts)
HoltWinters(undergraduate,gamma=FALSE,l.start=261407,b.start=24892)
library("forecast")
forecasts2=forecast(forecasts,h=5)
forecasts2
plot(forecasts2) #����10�������ʹ��Holtƽ��������Ԥ��  

#����15������ݽ���Ԥ��
Data2<-read.delim(file="C:/Users/lenovo/Desktop/��ҵ����/15��ߵ���ҽҩѧԺУѧ������.txt",header=TRUE)
undergraduate=ts(Data2$��ͨ����,start=c(2002))
plot(undergraduate,main="��У����������")
forecasts<-HoltWinters(undergraduate,gamma=FALSE)
forecasts
forecasts$SSE
plot(forecasts)
HoltWinters(doctor,gamma=FALSE,l.start=108591,b.start=33645)
library("forecast")
forecasts2=forecast(forecasts,h=3)
forecasts2
plot(forecasts2)  #����15�������ʹ��Holtƽ��������Ԥ��  ����׼ȷ�� 






#����
# ��������
Data<- read.csv("C:/Users/lenovo/Desktop/��ҵ����/��רҵ����(1).csv", header = TRUE)

#����2����4�н���kmeans���ࡣ
newData=Data[,2:4]
(CluR=kmeans(newData,3))
#����һ�������������������зֱ�ͳ�Ƹ���ѧλ���ֵĴ�����
table(Data$ѧλ,CluR $cluster)
#�������ľ���������ɢ��ͼ������Ϊ������е���"����"��"˶ʿ"����ɫΪ��1��2��3��ʾ��ȱʡ��ɫ
plot(newData$����, newData$˶ʿ,col=CluR$cluster)
#�������ľ���������ɢ��ͼ������Ϊ������е���"����"��"��ʿ"����ɫΪ��1��2��3��ʾ��ȱʡ��ɫ
#plot(newData$����,newData$��ʿ,col=CluR$cluster)
#�������ľ���������ɢ��ͼ������Ϊ������е���"˶ʿ"��"��ʿ"����ɫΪ��1��2��3��ʾ��ȱʡ��ɫ
#plot(newData$˶ʿ,newData$��ʿ,col=CluR$cluster)
#��ͼ�ϱ��ÿ����������ĵ㡣
points(CluR$centers,col=1:3,pch=8,cex=2)
#���ӻ�
library(ggplot2)
library(ggfortify)
library(cluster)
autoplot(pam(Data[-1], 3), frame = TRUE, frame.type = 'norm') 


#����2����4�н���PAM���ࡣ
install.packages("cluster") 
library("cluster")
newData=Data[,2:4]
(PClu=pam(newData,k=3,do.swap=TRUE,stand=FALSE))