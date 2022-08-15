install.packages("tseries")
install.packages("lmtest")
install.packages("randomForest")
install.packages("tree")
install.packages("MASS")
install.packages("gbm")
install.packages("xgboost")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("Ecdat")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("lubridate")

install.packages("remotes")
remotes::install_github("davidADSP/xgboostExplainer")

setwd("C:/Users/User/Documents/College/ISU/MAT490_Project")
library("tseries")
library("lmtest")
library("tree")
library("MASS")
library("randomForest")
library("gbm")
library("xgboost")
library("ggplot2")
library("reshape2")
library("Ecdat")
library("jsonlite")
library("tidyverse")
library("lubridate")


Project_Data=read.csv("R_Project_Data.csv",header=T)

######Run first Creating dummy variables and natural log of data

DISC=rep(1,668)
for(i in 1:668){if(Project_Data$Type.of.breach[i]=="DISC"){DISC[i]=1}else{DISC[i]=0}}

HACK=rep(1,668)
for(i in 1:668){if(Project_Data$Type.of.breach[i]=="HACK"){HACK[i]=1}else{HACK[i]=0}}

INSD=rep(1,668)
for(i in 1:668){if(Project_Data$Type.of.breach[i]=="INSD"){INSD[i]=1}else{INSD[i]=0}}

PHYS=rep(1,668)
for(i in 1:668){if(Project_Data$Type.of.breach[i]=="PHYS"){PHYS[i]=1}else{PHYS[i]=0}}

UNKN=rep(1,668)
for(i in 1:668){if(Project_Data$Type.of.breach[i]=="UNKN"){UNKN[i]=1}else{UNKN[i]=0}}

ln_Revenue=log(Project_Data$Revenue)
ln_Records=log(Project_Data$Total.Records)

Project_Data=cbind(Project_Data,DISC,HACK,INSD,PHYS,UNKN,ln_Revenue,ln_Records)

samp=Project_Data

Project_Data=subset(Project_Data,Revenue>0&Employ>0)

####### Subseting data
Data2=subset(Project_Data,ln_Records>=0)
Data3=subset(Project_Data,ln_Records>=0&ln_Revenue>=0)

######### setting up vulnerability data ####################33
Vuln2018=fromJSON("nvdcve-1.1-2018.json")
Vuln2018=Vuln2018$CVE_Items
publ=Vuln2018$publishedDate
publ_df=as.data.frame(publ)

publdate=as.Date(publ)
publdate_df=as.data.frame(publdate)

publdate2_df=mutate(publdate_df,year=lubridate::year(publdate),month=lubridate::month(publdate),day=lubridate::day(publdate),week=lubridate::week(publdate))
data=dplyr::filter(publdate2_df,year==2018)
dataweek2=arrange(data,week)
dataweek3=group_by(dataweek2,week)
dataweek4=add_tally(dataweek3,week)
dataweek5=distinct(dataweek4,week,n)

######## extracting records from Project_Data
Project_Dataweek2=select(Project_Data,Date.Made.Public,Total.Records)
Project_Dataweek3=mutate(Project_Dataweek2,date=as.Date(Date.Made.Public))
Project_Dataweek4=mutate(Project_Dataweek3,year=lubridate::year(Date.Made.Public),month=lubridate::month(Date.Made.Public),day=lubridate::day(Date.Made.Public),week=lubridate::week(Date.Made.Public))
Project_Dataweek5=arrange(Project_Dataweek4,week)
Project_Dataweek6=group_by(Project_Dataweek5,week)
weekrecords=ts(summarise(Project_Dataweek6,weekrecords=sum(Total.Records))$weekrecords,start=c(2018,1),end=c(2018,53),freq=7)
DayRecordsL1=stats::lag(weekrecords,1)

data6=filter(dataweek5,week>=4)
data7=ts(data6$n,start=c(2018,1),end=c(2018,53),freq=7)

acf(ts.union(weekrecords,data7))
cor(weekrecords,data7)
cor(DayRecordsL1,data7)


plot(data6$n,DayRecords$dayrecords)
cor(data6$n,DayRecords$dayrecords)
cor(data6$n,DayRecordsL1[-1])


###### Vuln data collected by day
publdate2_df=mutate(publdate_df,year=lubridate::year(publdate),month=lubridate::month(publdate),day=lubridate::day(publdate),week=lubridate::week(publdate))
data=dplyr::filter(publdate2_df,year==2018)
dataday2=arrange(data,publdate)
dataday3=group_by(dataday2,publdate)
dataday4=summarize(dataday3,vuln_day=n())
dataday5=distinct(dataday4,publdate,vuln_day)

Project_Dataday2=select(Project_Data,Date.Made.Public,Total.Records)
Project_Dataday3=mutate(Project_Dataday2,date=as.Date(Date.Made.Public))
Project_Dataday4=mutate(Project_Dataday3,year=lubridate::year(Date.Made.Public),month=lubridate::month(Date.Made.Public),day=lubridate::day(Date.Made.Public),week=lubridate::week(Date.Made.Public))
Project_Dataday5=arrange(Project_Dataday4,date, month, day)
Project_Dataday6=group_by(Project_Dataday5,date, month,day)
Project_Dataday7=summarise(Project_Dataday6,dayrecords=sum(Total.Records))

record=merge(Project_Dataday7,dataday5,by.x="date",by.y="publdate")
### create a loop to replace 0 with 1
for(i in 1:185){if(record$dayrecords[i]==0){record$dayrecords[i]=1}}
##

dayrecords=ts(log(record$dayrecords),start=decimal_date(ymd("2018-01-22")),end=decimal_date(ymd("2018-12-31")),freq=365)
dataday6=ts(record$vuln_day,start=decimal_date(ymd("2018-01-22")),end=decimal_date(ymd("2018-12-31")),frequency=365)

plot(dayrecords,dataday6)
acf(ts.union(dayrecords,dataday6))
r=cor(dayrecords,dataday6)
d=length(dayrecords)-2
tstat=(r/sqrt(1-r^2))*sqrt(d)
pt(tstat,d,lower.tail=FALSE)

######### inputting day breach data into Project data set
Project_Data2=mutate(Project_Data,publdate=ymd(Project_Data$Date.Made.Public))
Project_Data2=right_join(dataday5,Project_Data2,by="publdate")
vuln_day=replace_na(Project_Data2$vuln_day,0)
vuln_day=data.frame(vuln_day)
Project_Data2=data.frame(select(Project_Data2,-vuln_day))
Project_Data2=cbind(Project_Data2,vuln_day)

Data22=subset(Project_Data2,ln_Records>=0)
Data23=subset(Project_Data2,ln_Records>=0&ln_Revenue>=0)

############################# Descriptive Statistics ###############################
########### comaring in and out of sample data
samp=mutate(samp,sample=ifelse(Revenue==0 | Employ==0,"out","in"))
group_by(samp,sample)%>%
summarise(record=mean(Total.Records),recordq2=median(Total.Records),revenueavg=mean(Revenue),revenueq2=median(Revenue),employavg=mean(Employ),employq2=median(Employ))

####### population data
summarize(samp,record=sum(Total.Records))

ggplot(dat=samp,aes(x=Type.of.organization,y=Total.Records))+geom_bar(stat="identity")
ggplot(dat=samp,aes(x="",y=Total.Records,fill=Type.of.organization))+geom_bar(stat="identity")+coord_polar("y",start=0)
ggplot(dat=samp,aes(x=Type.of.organization))+geom_bar(stat="count")
sampcount=group_by(samp,Type.of.organization)%>%
  summarise(count=n())
ggplot(dat=sampcount,aes(x="",y=count,fill=Type.of.organization))+geom_bar(stat="identity")+coord_polar("y",start=0)

######## population data by type of breach
sampcountbreach=group_by(samp,Type.of.breach)%>%
  summarise(count=n())
ggplot(dat=sampcountbreach,aes(x="",y=count,fill=Type.of.breach))+geom_bar(stat="identity",position="fill")+coord_polar("y",start=0)
ggplot(dat=samp,aes(x="",y=Total.Records,fill=Type.of.breach))+geom_bar(stat="identity",position="fill")+coord_polar("y",start=0)

ggplot(dat=samp,aes(x=Type.of.organization,y=Total.Records,fill=Type.of.breach))+geom_bar(stat="identity",position="fill")

ggplot(dat=samp,aes(x=Type.of.breach,y=Total.Records))+geom_bar(stat="identity")
ggplot(dat=samp,aes(x=Type.of.breach))+geom_bar(stat="count")
ggplot(dat=samp,aes(x=Type.of.breach,y=Total.Records,fill=Type.of.organization))+geom_bar(stat="identity",position="fill")

group_by(samp,Company)%>%
  summarise(count=n())%>%
  arrange(desc(count))%>%
  filter(count>1)

summarise(samp,records=sum(Total.Records))  

######## data excluded from sample
sampout=filter(samp,sample=="out")
summarize(sampout,record=sum(Total.Records))

(summarize(sampin,record=sum(Total.Records))[,"record"])/(summarize(sampin,record=sum(Total.Records))[,"record"])

ggplot(data=sampout,aes(x=Type.of.breach,y=Total.Records))+geom_bar(stat="identity")+
  coord_cartesian(ylim=c(0,10000000))

############## data in sample
sampin=filter(samp,sample=="in")
summarize(sampin,record=sum(Total.Records))

sampinbreach=group_by(sampin,Type.of.breach)%>%
  summarise(count=n())
ggplot(data=sampinbreach,aes(x="",y=count,fill=Type.of.breach))+geom_bar(stat="identity",position="fill")+coord_polar("y",start=0)

ggplot(dat=sampin,aes(x="",y=Total.Records,fill=Type.of.breach))+geom_bar(stat="identity",position="fill")+coord_polar("y",start=0)

group_by(sampin,Company)%>%
  summarise(count=n())%>%
  arrange(desc(count))

dim(Data2)
group_by(Data2,Company)%>%
  summarise(count=n())%>%
  arrange(desc(count))%>%
  filter(count>1)

dim(Data3)
group_by(Data3,Company)%>%
  summarise(count=n())%>%
  arrange(desc(count))%>%
  filter(count>1)
  


########## descriptive stats on sample
Project_D2=mutate(Project_Data2,month=month(publdate))
vuln_month=distinct(Project_D2,month,.keep_all=TRUE)
vuln_month=ungroup(vuln_month)
vuln_monthavg=summarise(vuln_month,vulnavg=round(mean(Vuln)))
vuln_monthsd=summarise(vuln_month,vulnsd=round(sd(Vuln),2))


AVG=summarise(Project_D2,Records=round(mean(Total.Records)),Employavg=round(mean(Employ),2),Revavg=round(mean(Revenue),2),Discavg=round(mean(DISC),4),hackavg=round(mean(HACK),4),insidavg=round(mean(INSD),4),physavg=round(mean(PHYS),4),unknavg=round(mean(UNKN),4))
AVG=cbind(AVG,vuln_monthavg)
names(AVG)=c("Total_Records","Employee","Revenue","DISC","HACK","INSD","PHYS","UNKN","vuln")

STDEV=summarise(Project_D2,Records=round(sd(Total.Records)),Employsd=round(sd(Employ),2),Revsd=round(sd(Revenue),2))
STDEV2=data.frame(STDEV,"DISC"="NA","HACK"="NA","INSD"="NA","PHYS"="NA","UNKN"="NA")
names(STDEV2)[1:3]=c("Total_Records","Employee","Revenue")
STDEV2=cbind(STDEV2,vuln_monthsd)
names(STDEV2)[9]="vuln"

descript=rbind(AVG,STDEV2)
rownames(descript)=c("Mean","st.dev")

summary(Project_Data[,c("Total.Records","Revenue","Employ","DISC","HACK","INSD","PHYS","UNKN")])
vuln_summ=summarise(vuln_month,min=min(Vuln),q1=quantile(Vuln,.25),median=median(Vuln),avg=mean(Vuln),q3=quantile(Vuln,.75),max=max(Vuln),sd=sd(Vuln),vuln_iqr=IQR(Vuln))
vulnday_summ=summarise(dataday5,min=min(vuln_day),q1=quantile(vuln_day,.25),median=median(vuln_day),avg=mean(vuln_day),q3=quantile(vuln_day,.75),max=max(vuln_day),sd=sd(vuln_day),vulnday_iqr=IQR(vuln_day))

########## detecting outliers
record_outliers=filter(Project_Data,Total.Records>24212.88)
dim(record_outliers)

record_outliers=filter(Project_Data,Revenue>1155052088)
dim(record_outliers)

record_outliers=filter(Project_Data,Employ>8074.75)
dim(record_outliers)

record_outliers=filter(Project_Data,Vuln>2559.25)
dim(record_outliers)

record_outliers=filter(Project_Data2,vuln_day>91)
dim(record_outliers)

##### Relationship between vulnerabilities and total records
plot(Project_Data$Vuln,Project_Data$Total.Records)

#############################Multiple Linear Regression #######################################################
############ Model 1 all data in levels #################################3
set.seed(1)
train=sample(1:nrow(Project_Data),.5*nrow(Project_Data))
train.data=Project_Data[train,]
test.data=Project_Data[-train,c("Revenue","Employ","DISC","HACK","PHYS","INSD","UNKN")]

model1=lm(Total.Records~Revenue+Employ+DISC+HACK+PHYS+INSD+UNKN+0,data=train.data)
summary(model1)

yhat=predict(model1,newdata=test.data)
sqrt(mean((Project_Data[-train,"Total.Records"]-yhat)^2))

qqnorm(resid(model1))
qqline(resid(model1))
jarque.bera.test(resid(model1))
bptest(model1)

############ Regression Model 1 with Vuln #################################3
set.seed(1)
train=sample(1:nrow(Project_Data),.5*nrow(Project_Data))
train.data=Project_Data[train,]
test.data=Project_Data[-train,c("Revenue","Employ","DISC","HACK","PHYS","INSD","UNKN","Vuln")]

model1V=lm(Total.Records~Revenue+Employ+DISC+HACK+PHYS+INSD+UNKN+Vuln+0,data=train.data)
summary(model1V)

yhat=predict(model1V,newdata=test.data)
sqrt(mean((Project_Data[-train,"Total.Records"]-yhat)^2))

qqnorm(resid(model1V))
qqline(resid(model1V))
jarque.bera.test(resid(model1V))

############ Regression Model 1 with day Vuln data#################################3
set.seed(1)
train=sample(1:nrow(Project_Data2),.5*nrow(Project_Data2))
train.data=Project_Data2[train,]
test.data=Project_Data2[-train,c("Revenue","Employ","DISC","HACK","PHYS","INSD","UNKN","vuln_day")]

model1V=lm(Total.Records~Revenue+Employ+DISC+HACK+PHYS+INSD+UNKN+vuln_day+0,data=train.data)
summary(model1V)

yhat=predict(model1V,newdata=test.data)
sqrt(mean((Project_Data2[-train,"Total.Records"]-yhat)^2))

qqnorm(resid(model1V))
qqline(resid(model1V))
jarque.bera.test(resid(model1V))

############ Model 2 ln records and revenue #################################3
set.seed(1)
train=sample(1:nrow(Data2),.5*nrow(Data2))
train.data=Data2[train,]
test.data=Data2[-train,c("Revenue","Employ","DISC","HACK","PHYS","INSD","UNKN")]

model2=lm(ln_Records~Revenue+Employ+DISC+HACK+PHYS+INSD+UNKN+0,data=train.data)
summary(model2)

yhat=exp(predict(model2,newdata=test.data))
sqrt(mean((Data2[-train,"Total.Records"]-yhat)^2))

qqnorm(resid(model2))
qqline(resid(model2))
jarque.bera.test(resid(model2))
bptest(model2)
############ Model 2 ln record and revenue with vuln ############
set.seed(1)
train=sample(1:nrow(Data2),.5*nrow(Data2))
train.data=Data2[train,]
test.data=Data2[-train,c("Revenue","Employ","DISC","HACK","PHYS","INSD","UNKN","Vuln")]

model2=lm(ln_Records~Revenue+Employ+DISC+HACK+PHYS+INSD+UNKN+Vuln+0,data=train.data)
summary(model2)

yhat=exp(predict(model2,newdata=test.data))
sqrt(mean((Data2[-train,"Total.Records"]-yhat)^2))

qqnorm(resid(model2))
qqline(resid(model2))
jarque.bera.test(resid(model2))

############ Model 2 ln records and revenue with daily Vuln #################################3
set.seed(1)
train=sample(1:nrow(Data22),.5*nrow(Data22))
train.data=Data22[train,]
test.data=Data22[-train,c("Revenue","Employ","DISC","HACK","PHYS","INSD","UNKN","vuln_day")]

model2=lm(ln_Records~Revenue+Employ+DISC+HACK+PHYS+INSD+UNKN+vuln_day+0,data=train.data)
summary(model2)

yhat=exp(predict(model2,newdata=test.data))
sqrt(mean((Data22[-train,"Total.Records"]-yhat)^2))

qqnorm(resid(model2))
qqline(resid(model2))
jarque.bera.test(resid(model2))

############ Model 3 with ln totals and ln revenue #################################3
set.seed(1)
train=sample(1:nrow(Data3),.5*nrow(Data3))
train.data=Data3[train,]
test.data=Data3[-train,c("Employ","ln_Revenue","DISC","HACK","PHYS","UNKN")]


model3=lm(ln_Records~ln_Revenue+Employ+DISC+HACK+PHYS+UNKN+0,data=train.data)
summary(model3)

yhat=exp(predict(model3,newdata=test.data))
sqrt(mean((Data3[-train,"Total.Records"]-yhat)^2))

qqnorm(resid(model3))
qqline(resid(model3))
jarque.bera.test(resid(model3))
bptest((model3))

########## OLS Model 3 with Vuln
set.seed(1)
train3=sample(1:nrow(Data3),.5*nrow(Data3))
train3.data=Data3[train3,]
test3.data=Data3[-train3,c("Employ","ln_Revenue","DISC","HACK","PHYS","UNKN","Vuln")]


model3=lm(ln_Records~ln_Revenue+Employ+DISC+HACK+PHYS+UNKN+Vuln+0,data=train.data)
summary(model3)

yhat=exp(predict(model3,newdata=test3.data))
sqrt(mean((Data3[-train3,"Total.Records"]-yhat)^2))

qqnorm(resid(model3))
qqline(resid(model3))
jarque.bera.test(resid(model3))

########## OLS Model 3 with daily Vuln
set.seed(1)
train23=sample(1:nrow(Data23),.5*nrow(Data23))
train23.data=Data23[train23,]
test23.data=Data23[-train23,c("Employ","ln_Revenue","DISC","HACK","PHYS","UNKN","vuln_day")]


model23=lm(ln_Records~ln_Revenue+Employ+DISC+HACK+PHYS+UNKN+vuln_day+0,data=train23.data)
summary(model23)

yhat23=exp(predict(model23,newdata=test23.data))
sqrt(mean((Data23[-train23,"Total.Records"]-yhat23)^2))

qqnorm(resid(model23))
qqline(resid(model23))
jarque.bera.test(resid(model23))


######################### Decision Tree ###############################
######### DT Model 1
library("MASS")
set.seed(1)
train=sample(1:nrow(Project_Data),nrow(Project_Data)/2)
DT=tree(Total.Records~Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS,Project_Data,subset=train)
summary(DT)

plot(DT)
text(DT,pretty=0)

yhatDT1=predict(DT,newdata=Project_Data[-train,])
DT.test=Project_Data[-train,"Total.Records"]
sqrt(mean((yhatDT1-DT.test)^2))


DTCV=cv.tree(DT)
plot(DTCV$size,DTCV$dev,type="b",main="CV Decision Tree Model 1")


DTprune=prune.tree(DT,best=2)
plot(DTprune,main="Pruned Decision Tree Model 1")
text(DTprune,pretty=0)

DT.test=Project_Data[-train,"Total.Records"]
yhatCV=predict(DTprune,newdata=Project_Data[-train,])
sqrt(mean((DT.test-yhatCV)^2))


############## DT Model 1 with Vuln
set.seed(1)
train=sample(1:nrow(Project_Data),nrow(Project_Data)/2)
DTV=tree(Total.Records~Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS+Vuln,Project_Data,subset=train)
summary(DTV)

plot(DTV)
text(DTV,pretty=0)

yhatDTV1=predict(DTV,newdata=Project_Data[-train,])
DTV.test=Project_Data[-train,"Total.Records"]
sqrt(mean((yhatDTV1-DTV.test)^2))


DTCVV=cv.tree(DTV)
plot(DTCVV$size,DTCVV$dev,type="b",main="CV Decision Tree Model 1")


DTpruneV=prune.tree(DTV,best=2)
plot(DTpruneV,main="Pruned Decision Tree Model 1")
text(DTpruneV,pretty=0)

DTV.test=Project_Data[-train,"Total.Records"]
yhatCVV=predict(DTpruneV,newdata=Project_Data[-train,])
sqrt(mean((DTV.test-yhatCVV)^2))

############## DT Model 1 with daily Vuln
set.seed(1)
train=sample(1:nrow(Project_Data2),nrow(Project_Data2)/2)
DTV=tree(Total.Records~Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS+vuln_day,Project_Data2,subset=train)
summary(DTV)

plot(DTV)
text(DTV,pretty=0)

yhatDTV1=predict(DTV,newdata=Project_Data2[-train,])
DTV.test=Project_Data2[-train,"Total.Records"]
sqrt(mean((yhatDTV1-DTV.test)^2))


DTCVV=cv.tree(DTV)
plot(DTCVV$size,DTCVV$dev,type="b",main="CV Decision Tree Model 1")


DTpruneV=prune.tree(DTV,best=5)
plot(DTpruneV,main="Pruned Decision Tree Model 1")
text(DTpruneV,pretty=0)

DTV.test=Project_Data2[-train,"Total.Records"]
yhatCVV=predict(DTpruneV,newdata=Project_Data2[-train,])
sqrt(mean((DTV.test-yhatCVV)^2))


######### DT Model 2
set.seed(1)
train2=sample(1:nrow(Data2),nrow(Data2)/2)
tree.revenue2=tree(ln_Records~Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS,Data2,subset=train2)
summary(tree.revenue2)

plot(tree.revenue2)
text(tree.revenue2,pretty=0)

yhatDT2=predict(tree.revenue2,newdata=Data2[-train2,])
yhatDT2=exp(yhatDT2)
revenue2.test=Data2[-train2,"Total.Records"]
sqrt(mean((yhatDT2-revenue2.test)^2))

cv.revenue2=cv.tree(tree.revenue2)
plot(cv.revenue2$size,cv.revenue2$dev,type="b",main="CV Decision Tree Model 2")

prune.revenue2=prune.tree(tree.revenue2,best=2)
plot(prune.revenue2)
text(prune.revenue2,pretty=0)

yhatDTCV2=exp(predict(prune.revenue2,newdata=Data2[-train2,]))
revenue2.test=Data2[-train2,"Total.Records"]
sqrt(mean((yhatDTCV2-revenue2.test)^2))

######### DT Model 2 with Vuln
set.seed(1)
train2=sample(1:nrow(Data2),nrow(Data2)/2)
tree.revenue2=tree(ln_Records~Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS+Vuln,Data2,subset=train2)
summary(tree.revenue2)

plot(tree.revenue2)
text(tree.revenue2,pretty=0)

yhatDT2=predict(tree.revenue2,newdata=Data2[-train2,])
yhatDT2=exp(yhatDT2)
revenue2.test=Data2[-train2,"Total.Records"]
sqrt(mean((yhatDT2-revenue2.test)^2))

cv.revenue2=cv.tree(tree.revenue2)
plot(cv.revenue2$size,cv.revenue2$dev,type="b",main="CV Decision Tree Model 2")

prune.revenue2=prune.tree(tree.revenue2,best=2)
plot(prune.revenue2)
text(prune.revenue2,pretty=0)

yhatDTCV2=exp(predict(prune.revenue2,newdata=Data2[-train2,]))
revenue2.test=Data2[-train2,"Total.Records"]
sqrt(mean((yhatDTCV2-revenue2.test)^2))

######### DT Model 2 with daily Vuln
set.seed(1)
train22=sample(1:nrow(Data22),nrow(Data22)/2)
tree.revenue22=tree(ln_Records~Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS+vuln_day,Data22,subset=train22)
summary(tree.revenue22)

plot(tree.revenue22)
text(tree.revenue22,pretty=0)

yhatDT22=predict(tree.revenue22,newdata=Data22[-train22,])
yhatDT22=exp(yhatDT22)
revenue22.test=Data22[-train22,"Total.Records"]
sqrt(mean((yhatDT22-revenue22.test)^2))

cv.revenue22=cv.tree(tree.revenue22)
plot(cv.revenue22$size,cv.revenue22$dev,type="b",main="CV Decision Tree Model 2")

prune.revenue22=prune.tree(tree.revenue22,best=2)
plot(prune.revenue22)
text(prune.revenue22,pretty=0)

yhatDTCV22=exp(predict(prune.revenue22,newdata=Data22[-train22,]))
revenue22.test=Data22[-train22,"Total.Records"]
sqrt(mean((yhatDTCV22-revenue22.test)^2))

######### DT Model 3
set.seed(1)
train3=sample(1:nrow(Data3),nrow(Data3)/2)
DT3=tree(ln_Records~ln_Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS,Data3,subset=train3)
summary(DT3)

plot(DT3)
text(DT3,pretty=0)

yhatDT3=exp(predict(DT3,newdata=Data3[-train3,]))
DT3.test=Data3[-train3,"Total.Records"]
sqrt(mean((yhatDT3-DT3.test)^2))

DTCV3=cv.tree(DT3)
plot(DTCV3$size,DTCV3$dev,type="b",main="CV Decision Tree Model 3")

DTprune3=prune.tree(DT3,best=2)
plot(DTprune3)
text(DTprune3,pretty=0)

yhatDTCV3=exp(predict(DTprune3,newdata=Data3[-train3,]))
DT3.test=Data3[-train3,"Total.Records"]
sqrt(mean((yhatDTCV3-DT3.test)^2))

######### DT Model 3 with Vuln
set.seed(1)
train3=sample(1:nrow(Data3),nrow(Data3)/2)
DT3=tree(ln_Records~ln_Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS+Vuln,Data3,subset=train3)
summary(DT3)

plot(DT3)
text(DT3,pretty=0)

yhatDT3=exp(predict(DT3,newdata=Data3[-train3,]))
DT3.test=Data3[-train3,"Total.Records"]
sqrt(mean((yhatDT3-DT3.test)^2))

DTCV3=cv.tree(DT3)
plot(DTCV3$size,DTCV3$dev,type="b",main="CV Decision Tree Model 3")

DTprune3=prune.tree(DT3,best=2)
plot(DTprune3)
text(DTprune3,pretty=0)

yhatDTCV3=exp(predict(DTprune3,newdata=Data3[-train3,]))
DT3.test=Data3[-train3,"Total.Records"]
sqrt(mean((yhatDTCV3-DT3.test)^2))

######### DT Model 3 with daily Vuln
set.seed(1)
train23=sample(1:nrow(Data23),nrow(Data23)/2)
DT23=tree(ln_Records~ln_Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS+vuln_day,Data23,subset=train23)
summary(DT23)

plot(DT23)
text(DT23,pretty=0)

yhatDT23=exp(predict(DT23,newdata=Data23[-train23,]))
DT23.test=Data23[-train23,"Total.Records"]
sqrt(mean((yhatDT23-DT23.test)^2))

DTCV23=cv.tree(DT23)
plot(DTCV23$size,DTCV23$dev,type="b",main="CV Decision Tree Model 3")

DTprune23=prune.tree(DT23,best=2)
plot(DTprune23)
text(DTprune23,pretty=0)

yhatDTCV23=exp(predict(DTprune23,newdata=Data23[-train23,]))
DT23.test=Data23[-train23,"Total.Records"]
sqrt(mean((yhatDTCV23-DT23.test)^2))

################ Random Forest ###################################
########### RF Model 1
set.seed(1)
trainRF1=sample(1:nrow(Project_Data),nrow(Project_Data)/2)

rf.revenue1=randomForest(Total.Records~Revenue+Employ+DISC+HACK+INSD+PHYS+UNKN,data=Project_Data,subset=trainRF1,mtry=3,importance=TRUE)
rf.revenue1$importance

yhatRF1=predict(rf.revenue1,newdata=Project_Data[-trainRF1,])
revenueRF1.test=Project_Data[-trainRF1,"Total.Records"]
sqrt(mean((yhatRF1-revenueRF1.test)^2))

########## RF Model 1 with Vuln
set.seed(1)
trainRF1=sample(1:nrow(Project_Data),nrow(Project_Data)/2)

RFV=randomForest(Total.Records~Revenue+Employ+DISC+HACK+INSD+PHYS+UNKN+Vuln,data=Project_Data,subset=trainRF1,mtry=5,importance=TRUE)
RFV$importance

yhatRFV=predict(RFV,newdata=Project_Data[-trainRF1,])
RFV.test=Project_Data[-trainRF1,"Total.Records"]
sqrt(mean((yhatRFV-RFV.test)^2))

########## RF Model 1 with daily Vuln
set.seed(1)
trainRF21=sample(1:nrow(Project_Data2),nrow(Project_Data2)/2)

RFV2=randomForest(Total.Records~Revenue+Employ+DISC+HACK+INSD+PHYS+UNKN+vuln_day,data=Project_Data2,subset=trainRF21,mtry=5,importance=TRUE)
RFV2$importance

yhatRFV2=predict(RFV2,newdata=Project_Data2[-trainRF21,])
RFV2.test=Project_Data2[-trainRF21,"Total.Records"]
sqrt(mean((yhatRFV2-RFV2.test)^2))

########### RF Model 2
set.seed(1)
trainRF2=sample(1:nrow(Data2),nrow(Data2)/2)

rf.revenue2=randomForest(ln_Records~Revenue+Employ+HACK+PHYS+INSD+UNKN+DISC,data=Data2,subset=trainRF2,mtry=5,importance=TRUE)
rf.revenue2$importance

yhatRF2=exp(predict(rf.revenue2,newdata=Data2[-trainRF2,]))
revenueRF2.test=Data2[-trainRF2,"Total.Records"]
sqrt(mean((yhatRF2-revenueRF2.test)^2))

########### RF Model 2 with Vuln
set.seed(1)
trainRF2=sample(1:nrow(Data2),nrow(Data2)/2)

rf.revenue2=randomForest(ln_Records~Revenue+Employ+HACK+PHYS+INSD+UNKN+DISC+Vuln,data=Data2,subset=trainRF2,mtry=8,importance=TRUE)
rf.revenue2$importance

yhatRF2=exp(predict(rf.revenue2,newdata=Data2[-trainRF2,]))
revenueRF2.test=Data2[-trainRF2,"Total.Records"]
sqrt(mean((yhatRF2-revenueRF2.test)^2))

########### RF Model 2 with daily Vuln
set.seed(1)
trainRF22=sample(1:nrow(Data22),nrow(Data22)/2)

rf.revenue22=randomForest(ln_Records~Revenue+Employ+HACK+PHYS+INSD+UNKN+DISC+vuln_day,data=Data22,subset=trainRF22,mtry=8,importance=TRUE)
rf.revenue22$importance

yhatRF22=exp(predict(rf.revenue22,newdata=Data22[-trainRF22,]))
revenueRF22.test=Data22[-trainRF22,"Total.Records"]
sqrt(mean((yhatRF22-revenueRF22.test)^2))

########### RF Model 3
set.seed(1)
trainRF3=sample(1:nrow(Data3),nrow(Data3)/2)

rf.revenue3=randomForest(ln_Records~ln_Revenue+Employ+DISC+HACK+INSD+PHYS+UNKN,data=Data3,subset=trainRF3,mtry=5,importance=TRUE)
rf.revenue3$importance

yhatRF3=exp(predict(rf.revenue3,newdata=Data3[-trainRF3,]))
revenueRF3.test=Data3[-trainRF3,"Total.Records"]
sqrt(mean((yhatRF3-revenueRF3.test)^2))

########### RF Model 3 with Vuln
set.seed(1)
trainRF3=sample(1:nrow(Data3),nrow(Data3)/2)

rf.revenue3=randomForest(ln_Records~ln_Revenue+Employ+DISC+HACK+INSD+PHYS+UNKN+Vuln,data=Data3,subset=trainRF3,mtry=8,importance=TRUE)
rf.revenue3$importance

yhatRF3=exp(predict(rf.revenue3,newdata=Data3[-trainRF3,]))
revenueRF3.test=Data3[-trainRF3,"Total.Records"]
sqrt(mean((yhatRF3-revenueRF3.test)^2))

########### RF Model 3 with daily Vuln
set.seed(1)
trainRF23=sample(1:nrow(Data23),nrow(Data23)/2)

rf.revenue23=randomForest(ln_Records~ln_Revenue+Employ+DISC+HACK+INSD+PHYS+UNKN+vuln_day,data=Data23,subset=trainRF23,mtry=8,importance=TRUE)
rf.revenue23$importance

yhatRF23=exp(predict(rf.revenue23,newdata=Data23[-trainRF23,]))
revenueRF23.test=Data23[-trainRF23,"Total.Records"]
sqrt(mean((yhatRF23-revenueRF23.test)^2))

########################### Boosting ###########################################
########## Boosting Model 1
set.seed(1)
train=sample(1:nrow(Project_Data),nrow(Project_Data)/2)

boost.revenue1=gbm(Total.Records~Revenue+Employ+DISC+HACK+INSD+PHYS+UNKN,data=Project_Data[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.revenue1)

yhat.boost1=predict(boost.revenue1,newdata=Project_Data[-train,],n.trees=5000)
boost.test1=Project_Data[-train,"Total.Records"]
sqrt(mean((yhat.boost1-boost.test1)^2))

########## Boosting Model 2
set.seed(1)
train=sample(1:nrow(Data2),nrow(Data2)/2)

boost.revenue2=gbm(ln_Records~Revenue+Employ+DISC+HACK+INSD+PHYS+UNKN,data=Data2[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.revenue2)

yhat.boost2=exp(predict(boost.revenue2,newdata=Data2[-train,],n.trees=5000))
boost.test2=Data2[-train,"Total.Records"]
sqrt(mean((yhat.boost2-boost.test2)^2))

########## Boosting Model 3
set.seed(1)
train=sample(1:nrow(Data3),nrow(Data3)/2)

boost.revenue3=gbm(ln_Records~ln_Revenue+Employ+DISC+HACK+INSD+PHYS+UNKN,data=Data3[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.revenue3)

yhat3=exp(predict(boost.revenue3,newdata=Data3[-train,],n.trees=5000))
boost.test3=Data3[-train,"Total.Records"]
sqrt(mean((yhat3-boost.test3)^2))

########################## xgboost ###########################################
###############################################################################
##############################################################################

########################## XGBoosting Modle 1 without vulnerability data ############################################## 
Model1_Data=select(Project_Data,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model1_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)
params=list(booster="gbtree",eta=.9,gamma=0,max_depth=15,min_child_weight=1,subsample=1,colsample_bylevel=.1)

model1=xgb.train(params=params,data=dtrain,nrounds=500)


yhat1=predict(model1,dtest)
boost.test=Model1_Data[-train,"Total.Records"]
sqrt(mean((boost.test-yhat1)^2))

model1.importance=xgb.importance(model=model1)
xgb.plot.importance(importance_matrix=model1.importance)

model1.cv=xgb.cv(params=params,data=dtrain,nrounds=500,nfold=5,metrics="rmse",verbose=FALSE,showsd=TRUE)

####### Tuning eta Model 1
Model1_Data=select(Project_Data,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model1_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

eta=seq(.1,1,.1)
test=nrow(Model1_Data)-train

conv_eta = matrix(data=NA,nrow=500,ncol=length(eta))
pred_eta = matrix(NA,length(test), length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta

for(i in 1:length(eta)){
  params=list(eta = eta[i], booster="gbtree",gamma=0,max_depth=14,min_child_weight=1,subsample=1,colsample_bylevel=.6)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, dtest)
}

conv_eta = data.frame(iter=1:500, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
ggplot(data = conv_eta) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1_Data[-train,"Total.Records"]

(RMSE_eta = sqrt(colMeans((ytest-pred_eta)^2)))

####### Tuning colsample_bylevel Model 1
Model1_Data=select(Project_Data,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model1_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

cs=seq(.1,1,.1)
test=nrow(Model1_Data)-train

conv_cs = matrix(data=NA,nrow=500,ncol=length(cs))
pred_cs = matrix(NA,length(test), length(cs))
colnames(conv_cs) = colnames(pred_cs) = cs

for(i in 1:length(cs)){
  params=list(eta = .9, booster="gbtree",gamma=0,max_depth=14,min_child_weight=1,subsample=1,colsample_bylevel=cs[i])
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_cs[,i] = xgb$evaluation_log$train_rmse
  pred_cs[,i] = predict(xgb, dtest)
}

conv_cs = data.frame(iter=1:500, conv_cs)
conv_cs = melt(conv_cs, id.vars = "iter")
ggplot(data = conv_cs) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1_Data[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_cs)^2)))

####### Tuning max_depth Model 1
Model1_Data=select(Project_Data,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model1_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

md=seq(1,10,1)
test=nrow(Model1_Data)-train

conv_md = matrix(data=NA,nrow=500,ncol=length(md))
pred_md = matrix(NA,length(test), length(md))
colnames(conv_md) = colnames(pred_md) = md

for(i in 1:length(md)){
  params=list(eta = .9, booster="gbtree",gamma=0,max_depth=md[i],min_child_weight=1,subsample=1,colsample_bylevel=.1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = predict(xgb, dtest)
}

conv_md = data.frame(iter=1:500, conv_md)
conv_md = melt(conv_md, id.vars = "iter")
ggplot(data = conv_md) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1_Data[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_md)^2)))

####### Tuning sub sample Model 1
Model1_Data=select(Project_Data,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model1_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

ss=seq(.1,1,.1)
test=nrow(Model1_Data)-train

conv_ss = matrix(data=NA,nrow=500,ncol=length(ss))
pred_ss = matrix(NA,length(test), length(ss))
colnames(conv_ss) = colnames(pred_ss) = ss

for(i in 1:length(ss)){
  params=list(eta = .9, booster="gbtree",gamma=0,max_depth=15,min_child_weight=1,subsample=ss[i],colsample_bylevel=.1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = predict(xgb, dtest)
}

conv_ss = data.frame(iter=1:500, conv_ss)
conv_ss = melt(conv_ss, id.vars = "iter")
ggplot(data = conv_ss) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1_Data[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_ss)^2)))

####### Tuning min child weight Model 1
Model1_Data=select(Project_Data,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model1_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

mcw=seq(1,10,1)
test=nrow(Model1_Data)-train

conv_mcw = matrix(data=NA,nrow=500,ncol=length(mcw))
pred_mcw = matrix(NA,length(test), length(mcw))
colnames(conv_mcw) = colnames(pred_mcw) = mcw

for(i in 1:length(mcw)){
  params=list(eta = .9, booster="gbtree",gamma=0,max_depth=15,min_child_weight=mcw[i],subsample=1,colsample_bylevel=.1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_mcw[,i] = xgb$evaluation_log$train_rmse
  pred_mcw[,i] = predict(xgb, dtest)
}

conv_mcw = data.frame(iter=1:500, conv_mcw)
conv_mcw = melt(conv_mcw, id.vars = "iter")
ggplot(data = conv_mcw) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1_Data[-train,"Total.Records"]

(RMSE_mcw = sqrt(colMeans((ytest-pred_mcw)^2)))

####### Tuning gamma Model 1
Model1_Data=select(Project_Data,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model1_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

gam=seq(0,10,1)
test=nrow(Model1_Data)-train

conv_gam = matrix(data=NA,nrow=500,ncol=length(gam))
pred_gam = matrix(NA,length(test), length(gam))
colnames(conv_gam) = colnames(pred_gam) = gam

for(i in 1:length(gam)){
  params=list(eta = .9, booster="gbtree",gamma=gam[i],max_depth=15,min_child_weight=1,subsample=1,colsample_bylevel=.1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_gam[,i] = xgb$evaluation_log$train_rmse
  pred_gam[,i] = predict(xgb, dtest)
}

conv_gam = data.frame(iter=1:500, conv_gam)
conv_gam = melt(conv_gam, id.vars = "iter")
ggplot(data = conv_gam) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1_Data[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_gam)^2)))

############ XGBoosting Model1: Data in levels with vuln ################################
Model1V_Data=select(Project_Data,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,Vuln)) 
Data=select(Model1V_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1V_Data),.5*nrow(Model1V_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1V_Data[train,"Total.Records"]
label2=Model1V_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)
params=list(booster="gbtree",eta=1,gamma=4,max_depth=10,min_child_weight=1,subsample=1,colsample_bylevel=.5)

model1=xgb.train(params=params,data=dtrain,nrounds=500)


yhat1=predict(model1,dtest)
boost.test=Model1V_Data[-train,"Total.Records"]
sqrt(mean((boost.test-yhat1)^2))

model1.importance=xgb.importance(model=model1)
xgb.plot.importance(importance_matrix=model1.importance)

model1.cv=xgb.cv(params=params,data=dtrain,nrounds=99,nfold=5,metrics="rmse",verbose=FALSE,showsd=TRUE)

####### Tuning eta Model 1 with Vuln
Model1V_Data=select(Project_Data,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,Vuln)) 
Data=select(Model1V_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1V_Data),.5*nrow(Model1V_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1V_Data[train,"Total.Records"]
label2=Model1V_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

eta=seq(.1,1,.1)
test=nrow(Model1V_Data)-train

conv_eta = matrix(data=NA,nrow=500,ncol=length(eta))
pred_eta = matrix(NA,length(test), length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta

for(i in 1:length(eta)){
  params=list(eta = eta[i], booster="gbtree",gamma=0,max_depth=14,min_child_weight=1,subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, dtest)
}

conv_eta = data.frame(iter=1:500, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
ggplot(data = conv_eta) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1V_Data[-train,"Total.Records"]

(RMSE_eta = sqrt(colMeans((ytest-pred_eta)^2)))

####### Tuning colsample_bylevel Model 1 with vuln
Model1V_Data=select(Project_Data,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,Vuln)) 
Data=select(Model1V_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1V_Data),.5*nrow(Model1V_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1V_Data[train,"Total.Records"]
label2=Model1V_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

cs=seq(.1,1,.1)
test=nrow(Model1V_Data)-train

conv_cs = matrix(data=NA,nrow=500,ncol=length(cs))
pred_cs = matrix(NA,length(test), length(cs))
colnames(conv_cs) = colnames(pred_cs) = cs

for(i in 1:length(cs)){
  params=list(eta = 1, booster="gbtree",gamma=0,max_depth=14,min_child_weight=1,subsample=1,colsample_bylevel=cs[i])
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_cs[,i] = xgb$evaluation_log$train_rmse
  pred_cs[,i] = predict(xgb, dtest)
}

conv_cs = data.frame(iter=1:500, conv_cs)
conv_cs = melt(conv_cs, id.vars = "iter")
ggplot(data = conv_cs) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1V_Data[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_cs)^2)))

####### Tuning max_depth Model 1 with vuln
Model1V_Data=select(Project_Data,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,Vuln)) 
Data=select(Model1V_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1V_Data),.5*nrow(Model1V_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1V_Data[train,"Total.Records"]
label2=Model1V_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

md=seq(10,20,1)
test=nrow(Model1V_Data)-train

conv_md = matrix(data=NA,nrow=500,ncol=length(md))
pred_md = matrix(NA,length(test), length(md))
colnames(conv_md) = colnames(pred_md) = md

for(i in 1:length(md)){
  params=list(eta = 1, booster="gbtree",gamma=0,max_depth=md[i],min_child_weight=1,subsample=1,colsample_bylevel=.5)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = predict(xgb, dtest)
}

conv_md = data.frame(iter=1:500, conv_md)
conv_md = melt(conv_md, id.vars = "iter")
ggplot(data = conv_md) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1V_Data[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_md)^2)))

####### Tuning sub sample Model 1 with vuln
Model1V_Data=select(Project_Data,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,Vuln)) 
Data=select(Model1V_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1V_Data),.5*nrow(Model1V_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1V_Data[train,"Total.Records"]
label2=Model1V_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

ss=seq(.1,1,.1)
test=nrow(Model1V_Data)-train

conv_ss = matrix(data=NA,nrow=500,ncol=length(ss))
pred_ss = matrix(NA,length(test), length(ss))
colnames(conv_ss) = colnames(pred_ss) = ss

for(i in 1:length(ss)){
  params=list(eta = 1, booster="gbtree",gamma=0,max_depth=10,min_child_weight=1,subsample=ss[i],colsample_bylevel=.5)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = predict(xgb, dtest)
}

conv_ss = data.frame(iter=1:500, conv_ss)
conv_ss = melt(conv_ss, id.vars = "iter")
ggplot(data = conv_ss) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1V_Data[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_ss)^2)))

####### Tuning min child weight Model 1 with vuln
Model1V_Data=select(Project_Data,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,Vuln)) 
Data=select(Model1V_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1V_Data),.5*nrow(Model1V_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1V_Data[train,"Total.Records"]
label2=Model1V_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

mcw=seq(1,10,1)
test=nrow(Model1V_Data)-train

conv_mcw = matrix(data=NA,nrow=500,ncol=length(mcw))
pred_mcw = matrix(NA,length(test), length(mcw))
colnames(conv_mcw) = colnames(pred_mcw) = mcw

for(i in 1:length(mcw)){
  params=list(eta = .9, booster="gbtree",gamma=0,max_depth=15,min_child_weight=mcw[i],subsample=1,colsample_bylevel=.1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_mcw[,i] = xgb$evaluation_log$train_rmse
  pred_mcw[,i] = predict(xgb, dtest)
}

conv_mcw = data.frame(iter=1:500, conv_mcw)
conv_mcw = melt(conv_mcw, id.vars = "iter")
ggplot(data = conv_mcw) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1V_Data[-train,"Total.Records"]

(RMSE_mcw = sqrt(colMeans((ytest-pred_mcw)^2)))

####### Tuning gamma Model 1 with vuln
Model1V_Data=select(Project_Data,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,Vuln)) 
Data=select(Model1V_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1V_Data),.5*nrow(Model1V_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1V_Data[train,"Total.Records"]
label2=Model1V_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

gam=seq(0,10,1)
test=nrow(Model1V_Data)-train

conv_gam = matrix(data=NA,nrow=500,ncol=length(gam))
pred_gam = matrix(NA,length(test), length(gam))
colnames(conv_gam) = colnames(pred_gam) = gam

for(i in 1:length(gam)){
  params=list(eta = 1, booster="gbtree",gamma=gam[i],max_depth=10,min_child_weight=1,subsample=1,colsample_bylevel=.5)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_gam[,i] = xgb$evaluation_log$train_rmse
  pred_gam[,i] = predict(xgb, dtest)
}

conv_gam = data.frame(iter=1:500, conv_gam)
conv_gam = melt(conv_gam, id.vars = "iter")
ggplot(data = conv_gam) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1V_Data[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_gam)^2)))

######## XGBoosting Model1: with daily Vuln #######################################
Model1_Data=select(Project_Data2,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model1_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)
params=list(booster="gbtree",eta=.1,gamma=0,max_depth=10,min_child_weight=1,subsample=1,colsample_bylevel=.1)

model1=xgb.train(params=params,data=dtrain,nrounds=500)


yhat1=predict(model1,dtest)
boost.test=Model1_Data[-train,"Total.Records"]
sqrt(mean((boost.test-yhat1)^2))

model1.importance=xgb.importance(model=model1)
xgb.plot.importance(importance_matrix=model1.importance)

model1.cv=xgb.cv(params=params,data=dtrain,nrounds=500,nfold=5,metrics="rmse",verbose=FALSE,showsd=TRUE)

############ XGBoosting Model2: Total Records in Logs without vulner ############################
Model2_Data=select(Data2,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),nrow(Model2_Data)/2)
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)
params=list(booster="gbtree",eta=.7,gamma=10,max_depth=13,min_child_weight=1,subsample=1,colsample_bylevel=.4)

model2=xgb.train(params=params,data=dtrain,nrounds=500)

yhat2=exp(predict(model2,dtest))
boost.test2=Data2[-train,"Total.Records"]
sqrt(mean((boost.test2-yhat2)^2))

model2.importance=xgb.importance(model=model2)
xgb.plot.importance(importance_matrix=model2.importance)

####### Tuning eta Model 2
Model2_Data=select(Data2,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

eta=seq(.1,1,.1)
test=nrow(Model2_Data[-train,])

conv_eta = matrix(data=NA,nrow=500,ncol=length(eta))
pred_eta = matrix(NA,nrow=test, length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta

for(i in 1:length(eta)){
  params=list(eta = eta[i], booster="gbtree",gamma=0,max_depth=6,min_child_weight=1,subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = exp(predict(xgb, dtest))
}

conv_eta = data.frame(iter=1:500, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
ggplot(data = conv_eta) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_eta = sqrt(colMeans((ytest-pred_eta)^2)))

####### Tuning colsample_bylevel Model 2
Model2_Data=select(Data2,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

cs=seq(.1,1,.1)
test=nrow(Model2_Data[-train,])

conv_cs = matrix(data=NA,nrow=500,ncol=length(cs))
pred_cs = matrix(NA,nrow=test, length(cs))
colnames(conv_cs) = colnames(pred_cs) = cs

for(i in 1:length(cs)){
  params=list(eta = .7, booster="gbtree",gamma=0,max_depth=6,min_child_weight=1,subsample=1,colsample_bylevel=cs[i])
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_cs[,i] = xgb$evaluation_log$train_rmse
  pred_cs[,i] = exp(predict(xgb, dtest))
}

conv_cs = data.frame(iter=1:500, conv_cs)
conv_cs = melt(conv_cs, id.vars = "iter")
ggplot(data = conv_cs) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_cs)^2)))

####### Tuning max_depth Model 2
Model2_Data=select(Data2,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

md=seq(10,20,1)
test=nrow(Model2_Data[-train,])

conv_md = matrix(data=NA,nrow=500,ncol=length(md))
pred_md = matrix(NA,nrow=test, length(md))
colnames(conv_md) = colnames(pred_md) = md

for(i in 1:length(md)){
  params=list(eta = .7, booster="gbtree",gamma=0,max_depth=md[i],min_child_weight=1,subsample=1,colsample_bylevel=.4)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = exp(predict(xgb, dtest))
}

conv_md = data.frame(iter=1:500, conv_md)
conv_md = melt(conv_md, id.vars = "iter")
ggplot(data = conv_md) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_md)^2)))

####### Tuning model 2 sub sample model 2
Model2_Data=select(Data2,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

ss=seq(.1,1,.1)
test=nrow(Model2_Data[-train,])

conv_ss = matrix(data=NA,nrow=500,ncol=length(ss))
pred_ss = matrix(NA,nrow=test, length(ss))
colnames(conv_ss) = colnames(pred_ss) = ss

for(i in 1:length(ss)){
  params=list(eta = .8, booster="gbtree",gamma=0,max_depth=4,min_child_weight=1,subsample=ss[i],colsample_bylevel=.6)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = exp(predict(xgb, dtest))
}

conv_ss = data.frame(iter=1:500, conv_ss)
conv_ss = melt(conv_ss, id.vars = "iter")
ggplot(data = conv_ss) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_ss)^2)))

####### Tuning model 2 min child weight model 2
mcw=seq(1,10,1)
test=nrow(Model2_Data[-train,])

conv_mcw = matrix(data=NA,nrow=500,ncol=length(mcw))
pred_mcw = matrix(NA,nrow=test, length(mcw))
colnames(conv_mcw) = colnames(pred_mcw) = mcw

for(i in 1:length(mcw)){
  params=list(eta = .3, booster="gbtree",gamma=0,max_depth=6,min_child_weight=mcw[i],subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_mcw[,i] = xgb$evaluation_log$train_rmse
  pred_mcw[,i] = exp(predict(xgb, dtest))
}

conv_mcw = data.frame(iter=1:500, conv_mcw)
conv_mcw = melt(conv_mcw, id.vars = "iter")
ggplot(data = conv_mcw) + geom_line(aes(x = iter, y = value, color = variable))

red=subset(Project_Data,ln_Records>=0)
ytest=red[-train,"Total.Records"]

(RMSE_mcw = sqrt(colMeans((ytest-pred_mcw)^2)))

####### Tuning gamma model2
gam=seq(0,10,1)
test=nrow(Model2_Data[-train,])

conv_gam = matrix(data=NA,nrow=500,ncol=length(gam))
pred_gam = matrix(NA,nrow=test, length(gam))
colnames(conv_gam) = colnames(pred_gam) = gam

for(i in 1:length(gam)){
  params=list(eta = .7, booster="gbtree",gamma=gam[i],max_depth=13,min_child_weight=1,subsample=1,colsample_bylevel=.4)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_gam[,i] = xgb$evaluation_log$train_rmse
  pred_gam[,i] = exp(predict(xgb, dtest))
}

conv_gam = data.frame(iter=1:500, conv_gam)
conv_gam = melt(conv_gam, id.vars = "iter")
ggplot(data = conv_gam) + geom_line(aes(x = iter, y = value, color = variable))

red=subset(Project_Data,ln_Records>=0)
ytest=red[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_gam)^2)))

############ XGBoosting Model2: Total Records in Logs with Vuln ########################
Model_DataXGB2V=select(Data2,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,Vuln))
DataXGB2V=select(Model_DataXGB2V,-ln_Records)

set.seed(1)
trainXGB2V=sample(1:nrow(Model_DataXGB2V),nrow(Model_DataXGB2V)/2)
Train.DataXGB2V=as.matrix(DataXGB2V[trainXGB2V,],rowname=TRUE)
Test.DataXGB2V=as.matrix(DataXGB2V[-trainXGB2V,],rowname=TRUE)
labelXGB2V=Model_DataXGB2V[trainXGB2V,"ln_Records"]
label2XGB2V=Model_DataXGB2V[-trainXGB2V,"ln_Records"]

dtrainXGB2V=xgb.DMatrix(data=Train.DataXGB2V,label=labelXGB2V)
dtestXGB2V=xgb.DMatrix(data=Test.DataXGB2V,label=label2XGB2V)
paramsXGB2V=list(booster="gbtree",eta=.6,gamma=0,max_depth=2,min_child_weight=1,subsample=1,colsample_bylevel=.4)

modelXGB2V=xgb.train(params=paramsXGB2V,data=dtrainXGB2V,nrounds=500)

yhatXGB2V=exp(predict(modelXGB2V,dtestXGB2V))
boost.testXGB2V=Data2[-trainXGB2V,"Total.Records"]
sqrt(mean((boost.testXGB2V-yhatXGB2V)^2))

modelXGB2V.importance=xgb.importance(model=modelXGB2V)
xgb.plot.importance(importance_matrix=modelXGB2V.importance)

####### Tuning eta Model 2 with vuln
Model2_Data=select(Data2,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,Vuln))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

eta=seq(.1,1,.1)
test=nrow(Model2_Data[-train,])

conv_eta = matrix(data=NA,nrow=500,ncol=length(eta))
pred_eta = matrix(NA,nrow=test, length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta

for(i in 1:length(eta)){
  params=list(eta = eta[i], booster="gbtree",gamma=0,max_depth=6,min_child_weight=1,subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = exp(predict(xgb, dtest))
}

conv_eta = data.frame(iter=1:500, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
ggplot(data = conv_eta) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_eta = sqrt(colMeans((ytest-pred_eta)^2)))

####### Tuning colsample_bylevel Model 2 with vuln
Model2_Data=select(Data2,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,Vuln))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

cs=seq(.1,1,.1)
test=nrow(Model2_Data[-train,])

conv_cs = matrix(data=NA,nrow=500,ncol=length(cs))
pred_cs = matrix(NA,nrow=test, length(cs))
colnames(conv_cs) = colnames(pred_cs) = cs

for(i in 1:length(cs)){
  params=list(eta = .6, booster="gbtree",gamma=0,max_depth=6,min_child_weight=1,subsample=1,colsample_bylevel=cs[i])
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_cs[,i] = xgb$evaluation_log$train_rmse
  pred_cs[,i] = exp(predict(xgb, dtest))
}

conv_cs = data.frame(iter=1:500, conv_cs)
conv_cs = melt(conv_cs, id.vars = "iter")
ggplot(data = conv_cs) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_cs)^2)))

####### Tuning max_depth Model 2 with vuln
Model2_Data=select(Data2,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,Vuln))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

md=seq(1,10,1)
test=nrow(Model2_Data[-train,])

conv_md = matrix(data=NA,nrow=500,ncol=length(md))
pred_md = matrix(NA,nrow=test, length(md))
colnames(conv_md) = colnames(pred_md) = md

for(i in 1:length(md)){
  params=list(eta = .6, booster="gbtree",gamma=0,max_depth=md[i],min_child_weight=1,subsample=1,colsample_bylevel=.4)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = exp(predict(xgb, dtest))
}

conv_md = data.frame(iter=1:500, conv_md)
conv_md = melt(conv_md, id.vars = "iter")
ggplot(data = conv_md) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_md)^2)))

####### Tuning gamma model2 with vuln
Model2_Data=select(Data2,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,Vuln))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

gam=seq(0,10,1)
test=nrow(Model2_Data[-train,])

conv_gam = matrix(data=NA,nrow=500,ncol=length(gam))
pred_gam = matrix(NA,nrow=test, length(gam))
colnames(conv_gam) = colnames(pred_gam) = gam

for(i in 1:length(gam)){
  params=list(eta = .6, booster="gbtree",gamma=gam[i],max_depth=2,min_child_weight=1,subsample=1,colsample_bylevel=.4)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_gam[,i] = xgb$evaluation_log$train_rmse
  pred_gam[,i] = exp(predict(xgb, dtest))
}

conv_gam = data.frame(iter=1:500, conv_gam)
conv_gam = melt(conv_gam, id.vars = "iter")
ggplot(data = conv_gam) + geom_line(aes(x = iter, y = value, color = variable))

red=subset(Project_Data,ln_Records>=0)
ytest=red[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_gam)^2)))

############ XGBoosting Model2: Total Records in Logs with daily Vuln ################
Model2_Data=select(Data22,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),nrow(Model2_Data)/2)
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)
params=list(booster="gbtree",eta=.1,gamma=0,max_depth=6,min_child_weight=7,subsample=1,colsample_bylevel=1)

model2=xgb.train(params=params,data=dtrain,nrounds=500)

yhat2=exp(predict(model2,dtest))
boost.test2=Data2[-train,"Total.Records"]
sqrt(mean((boost.test2-yhat2)^2))

model2.importance=xgb.importance(model=model2)
xgb.plot.importance(importance_matrix=model2.importance)

############ XGBoosting Model3: Total Records and Revenue in Logs #####################
Model3_Data=select(Data3,c(ln_Records,ln_Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model3_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model3_Data),nrow(Model3_Data)/2)
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model3_Data[train,"ln_Records"]
label2=Model3_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)
params=list(booster="gbtree",eta=.7,gamma=5,max_depth=15,min_child_weight=1,subsample=1,colsample_bytree=.4)

model3=xgboost(params=params,data=dtrain,nrounds=500,verbose=0)

yhat3=exp(predict(model3,dtest))
boost.test3=Data3[-train,"Total.Records"]
sqrt(mean((boost.test3-yhat3)^2))

model3.importance=xgb.importance(model=model3)
xgb.plot.importance(importance_matrix=model3.importance)

####### Tuning eta Model 3
Model3_Data=select(Data3,c(ln_Records,ln_Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model3_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model3_Data),nrow(Model3_Data)/2)
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model3_Data[train,"ln_Records"]
label2=Model3_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

eta=seq(.1,1,.1)
test=nrow(Model3_Data[-train,])

conv_eta = matrix(data=NA,nrow=500,ncol=length(eta))
pred_eta = matrix(NA,nrow=test, length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta

for(i in 1:length(eta)){
  params=list(eta = eta[i], booster="gbtree",gamma=0,max_depth=6,min_child_weight=1,subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = exp(predict(xgb, dtest))
}

conv_eta = data.frame(iter=1:500, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
ggplot(data = conv_eta) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data3[-train,"Total.Records"]

(RMSE_eta = sqrt(colMeans((ytest-pred_eta)^2)))

####### Tuning colsample_bylevel Model 3
Model3_Data=select(Data3,c(ln_Records,ln_Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model3_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model3_Data),nrow(Model3_Data)/2)
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model3_Data[train,"ln_Records"]
label2=Model3_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

cs=seq(.1,1,.1)
test=nrow(Model3_Data[-train,])

conv_cs = matrix(data=NA,nrow=500,ncol=length(cs))
pred_cs = matrix(NA,nrow=test, length(cs))
colnames(conv_cs) = colnames(pred_cs) = cs

for(i in 1:length(cs)){
  params=list(eta = .7, booster="gbtree",gamma=0,max_depth=6,min_child_weight=1,subsample=1,colsample_bylevel=cs[i])
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_cs[,i] = xgb$evaluation_log$train_rmse
  pred_cs[,i] = exp(predict(xgb, dtest))
}

conv_cs = data.frame(iter=1:500, conv_cs)
conv_cs = melt(conv_cs, id.vars = "iter")
ggplot(data = conv_cs) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data3[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_cs)^2)))

####### Tuning max_depth Model 3
Model3_Data=select(Data3,c(ln_Records,ln_Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model3_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model3_Data),nrow(Model3_Data)/2)
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model3_Data[train,"ln_Records"]
label2=Model3_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

md=seq(10,20,1)
test=nrow(Model3_Data[-train,])

conv_md = matrix(data=NA,nrow=500,ncol=length(md))
pred_md = matrix(NA,nrow=test, length(md))
colnames(conv_md) = colnames(pred_md) = md

for(i in 1:length(md)){
  params=list(eta = .7, booster="gbtree",gamma=0,max_depth=md[i],min_child_weight=1,subsample=1,colsample_bylevel=.4)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = exp(predict(xgb, dtest))
}

conv_md = data.frame(iter=1:500, conv_md)
conv_md = melt(conv_md, id.vars = "iter")
ggplot(data = conv_md) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data3[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_md)^2)))

####### Tuning gamma model3
Model3_Data=select(Data3,c(ln_Records,ln_Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model3_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model3_Data),nrow(Model3_Data)/2)
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model3_Data[train,"ln_Records"]
label2=Model3_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

gam=seq(0,10,1)
test=nrow(Model3_Data[-train,])

conv_gam = matrix(data=NA,nrow=500,ncol=length(gam))
pred_gam = matrix(NA,nrow=test, length(gam))
colnames(conv_gam) = colnames(pred_gam) = gam

for(i in 1:length(gam)){
  params=list(eta = .7, booster="gbtree",gamma=gam[i],max_depth=15,min_child_weight=1,subsample=1,colsample_bylevel=.4)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_gam[,i] = xgb$evaluation_log$train_rmse
  pred_gam[,i] = exp(predict(xgb, dtest))
}

conv_gam = data.frame(iter=1:500, conv_gam)
conv_gam = melt(conv_gam, id.vars = "iter")
ggplot(data = conv_gam) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data3[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_gam)^2)))

############ XGBoosting Model3: Total Records and Revenue in Logs with Vuln ####################
Model3_Data=select(Data23,c(ln_Records,ln_Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,Vuln))
Data=select(Model3_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model3_Data),nrow(Model3_Data)/2)
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model3_Data[train,"ln_Records"]
label2=Model3_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)
params=list(booster="gbtree",eta=.8,gamma=0,max_depth=1,min_child_weight=8,subsample=1,colsample_bytree=1)

model3=xgboost(params=params,data=dtrain,nrounds=500,verbose=0)

yhat3=exp(predict(model3,dtest))
boost.test3=Data23[-train,"Total.Records"]
sqrt(mean((boost.test3-yhat3)^2))

model3.importance=xgb.importance(model=model3)
xgb.plot.importance(importance_matrix=model3.importance)

############ XGBoosting Model3: Total Records and Revenue in Logs with daily Vuln ######################
Model_DataXGB3=select(Data23,c(ln_Records,ln_Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
DataXGB3=select(Model_DataXGB3,-ln_Records)

set.seed(1)
trainXGB3=sample(1:nrow(Model_DataXGB3),nrow(Model_DataXGB3)/2)
Train.DataXGB3=as.matrix(DataXGB3[trainXGB3,],rowname=TRUE)
Test.DataXGB3=as.matrix(DataXGB3[-trainXGB3,],rowname=TRUE)
labelXGB3=Model_DataXGB3[trainXGB3,"ln_Records"]
label2XGB3=Model_DataXGB3[-trainXGB3,"ln_Records"]

dtrainXGB3=xgb.DMatrix(data=Train.DataXGB3,label=labelXGB3)
dtestXGB3=xgb.DMatrix(data=Test.DataXGB3,label=label2XGB3)
paramsXGB3=list(booster="gbtree",eta=1,gamma=11,max_depth=8,min_child_weight=1,subsample=1,colsample_bytree=1)

modelXGB3=xgboost(params=paramsXGB3,data=dtrainXGB3,nrounds=500,verbose=0)

yhatXGB3=exp(predict(modelXGB3,dtestXGB3))
boost.testXGB3=Data23[-trainXGB3,"Total.Records"]
sqrt(mean((boost.testXGB3-yhatXGB3)^2))

modelXGB3.importance=xgb.importance(model=modelXGB3)
xgb.plot.importance(importance_matrix=modelXGB3.importance)

###################### sensitivity analysis #######################################
Data33=filter(Data23,Total.Records<=24212.88)

Model_DataXGB3=select(Data33,c(ln_Records,ln_Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
DataXGB3=select(Model_DataXGB3,-ln_Records)

set.seed(1)
trainXGB3=sample(1:nrow(Model_DataXGB3),nrow(Model_DataXGB3)/2)
Train.DataXGB3=as.matrix(DataXGB3[trainXGB3,],rowname=TRUE)
Test.DataXGB3=as.matrix(DataXGB3[-trainXGB3,],rowname=TRUE)
labelXGB3=Model_DataXGB3[trainXGB3,"ln_Records"]
label2XGB3=Model_DataXGB3[-trainXGB3,"ln_Records"]

dtrainXGB3=xgb.DMatrix(data=Train.DataXGB3,label=labelXGB3)
dtestXGB3=xgb.DMatrix(data=Test.DataXGB3,label=label2XGB3)
paramsXGB3=list(booster="gbtree",eta=1,gamma=11,max_depth=8,min_child_weight=1,subsample=1,colsample_bytree=1)

modelXGB3=xgboost(params=paramsXGB3,data=dtrainXGB3,nrounds=500,verbose=0)

yhatXGB3=exp(predict(modelXGB3,dtestXGB3))
boost.testXGB3=Data33[-trainXGB3,"Total.Records"]
sqrt(mean((boost.testXGB3-yhatXGB3)^2))

modelXGB3.importance=xgb.importance(model=modelXGB3)
xgb.plot.importance(importance_matrix=modelXGB3.importance)

##################### Tuning xgboosting parameters ################################


###################### Tuning eta Model 1 with Vuln ######################
Model1_Data=Project_Data[,-c(1:11,14,15,17,20:21,28:29)]
Data=Model1_Data[,-3]

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

eta=seq(.1,1,.1)
test=nrow(Model1_Data)-train

conv_eta = matrix(data=NA,nrow=500,ncol=length(eta))
pred_eta = matrix(NA,length(test), length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta

for(i in 1:length(eta)){
  params=list(eta = eta[i], booster="gbtree",gamma=0,max_depth=1,min_child_weight=1,subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, dtest)
}

conv_eta = data.frame(iter=1:500, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
ggplot(data = conv_eta) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Project_Data[-train,"Total.Records"]

(RMSE_eta = sqrt(colMeans((ytest-pred_eta)^2)))

####### Tuning colsample_bylevel Model 1 with Vuln
Model1_Data=Project_Data[,-c(1:11,14,15,17,20:21,28:29)]
Data=Model1_Data[,-3]

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

cs=seq(.1,1,.1)
test=nrow(Model1_Data)-train

conv_cs = matrix(data=NA,nrow=500,ncol=length(cs))
pred_cs = matrix(NA,length(test), length(cs))
colnames(conv_cs) = colnames(pred_cs) = cs

for(i in 1:length(cs)){
  params=list(eta = .1, booster="gbtree",gamma=0,max_depth=6,min_child_weight=1,subsample=1,colsample_bylevel=cs[i])
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_cs[,i] = xgb$evaluation_log$train_rmse
  pred_cs[,i] = predict(xgb, dtest)
}

conv_cs = data.frame(iter=1:500, conv_cs)
conv_cs = melt(conv_cs, id.vars = "iter")
ggplot(data = conv_cs) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1_Data[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_cs)^2)))

####### Tuning max_depth Model 1 with Vuln
Model1_Data=Project_Data[,-c(1:11,14,15,17,20:21,28:29)]
Data=Model1_Data[,-3]

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

md=seq(1,10,1)
test=nrow(Model1_Data)-train

conv_md = matrix(data=NA,nrow=500,ncol=length(md))
pred_md = matrix(NA,length(test), length(md))
colnames(conv_md) = colnames(pred_md) = md

for(i in 1:length(md)){
  params=list(eta = .1, booster="gbtree",gamma=0,max_depth=md[i],min_child_weight=1,subsample=1,colsample_bylevel=.1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = predict(xgb, dtest)
}

conv_md = data.frame(iter=1:500, conv_md)
conv_md = melt(conv_md, id.vars = "iter")
ggplot(data = conv_md) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1_Data[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_md)^2)))

####### Tuning sub-sample Model 1 with Vuln
Model1_Data=Project_Data[,-c(1:11,14,15,17,20:21,28:29)]
Data=Model1_Data[,-3]

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

ss=seq(.1,1,.1)
test=nrow(Model1_Data)-train

conv_ss = matrix(data=NA,nrow=500,ncol=length(ss))
pred_ss = matrix(NA,length(test), length(ss))
colnames(conv_ss) = colnames(pred_ss) = ss

for(i in 1:length(ss)){
  params=list(eta = .1, booster="gbtree",gamma=0,max_depth=5,min_child_weight=1,subsample=ss[i],colsample_bylevel=.1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = predict(xgb, dtest)
}

conv_ss = data.frame(iter=1:500, conv_ss)
conv_ss = melt(conv_ss, id.vars = "iter")
ggplot(data = conv_ss) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1_Data[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_ss)^2)))

####### Tuning min child weight Model 1
Model1_Data=Project_Data[,-c(1:11,14,15,17,20:21,28:29)]
Data=Model1_Data[,-3]

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

mcw=seq(1,10,1)
test=nrow(Model1_Data)-train

conv_mcw = matrix(data=NA,nrow=500,ncol=length(mcw))
pred_mcw = matrix(NA,length(test), length(mcw))
colnames(conv_mcw) = colnames(pred_mcw) = mcw

for(i in 1:length(mcw)){
  params=list(eta = .1, booster="gbtree",gamma=0,max_depth=5,min_child_weight=mcw[i],subsample=1,colsample_bylevel=.1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_mcw[,i] = xgb$evaluation_log$train_rmse
  pred_mcw[,i] = predict(xgb, dtest)
}

conv_mcw = data.frame(iter=1:500, conv_mcw)
conv_mcw = melt(conv_mcw, id.vars = "iter")
ggplot(data = conv_mcw) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1_Data[-train,"Total.Records"]

(RMSE_mcw = sqrt(colMeans((ytest-pred_mcw)^2)))

####### Tuning gamma Model 1
Model1_Data=Project_Data[,-c(1:11,14,15,17,20:21,28:29)]
Data=Model1_Data[,-3]

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

gam=seq(10,20,1)
test=nrow(Model1_Data)-train

conv_gam = matrix(data=NA,nrow=500,ncol=length(gam))
pred_gam = matrix(NA,length(test), length(gam))
colnames(conv_gam) = colnames(pred_gam) = gam

for(i in 1:length(gam)){
  params=list(eta = .1, booster="gbtree",gamma=gam[i],max_depth=5,min_child_weight=1,subsample=1,colsample_bylevel=.1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_gam[,i] = xgb$evaluation_log$train_rmse
  pred_gam[,i] = predict(xgb, dtest)
}

conv_gam = data.frame(iter=1:500, conv_gam)
conv_gam = melt(conv_gam, id.vars = "iter")
ggplot(data = conv_gam) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1_Data[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_gam)^2)))

####### Tuning eta Model 2
Model2_Data=select(Data22,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

eta=seq(.1,1,.1)
test=nrow(Model2_Data[-train,])

conv_eta = matrix(data=NA,nrow=500,ncol=length(eta))
pred_eta = matrix(NA,nrow=test, length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta

for(i in 1:length(eta)){
  params=list(eta = eta[i], booster="gbtree",gamma=0,max_depth=6,min_child_weight=1,subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = exp(predict(xgb, dtest))
}

conv_eta = data.frame(iter=1:500, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
ggplot(data = conv_eta) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_eta = sqrt(colMeans((ytest-pred_eta)^2)))

####### Tuning colsample_bylevel Model 2
Model2_Data=select(Data22,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

cs=seq(.1,1,.1)
test=nrow(Model2_Data[-train,])

conv_cs = matrix(data=NA,nrow=500,ncol=length(cs))
pred_cs = matrix(NA,nrow=test, length(cs))
colnames(conv_cs) = colnames(pred_cs) = cs

for(i in 1:length(cs)){
  params=list(eta = .8, booster="gbtree",gamma=0,max_depth=6,min_child_weight=1,subsample=1,colsample_bylevel=cs[i])
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_cs[,i] = xgb$evaluation_log$train_rmse
  pred_cs[,i] = exp(predict(xgb, dtest))
}

conv_cs = data.frame(iter=1:500, conv_cs)
conv_cs = melt(conv_cs, id.vars = "iter")
ggplot(data = conv_cs) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_cs)^2)))

####### Tuning max_depth Model 2
Model2_Data=select(Data22,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

Model2_Data=Data2[,-c(1:11,14,15,16,17,20:22,28)]
Data=Model2_Data[,-10]

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

md=seq(1,10,1)
test=nrow(Model2_Data[-train,])

conv_md = matrix(data=NA,nrow=500,ncol=length(md))
pred_md = matrix(NA,nrow=test, length(md))
colnames(conv_md) = colnames(pred_md) = md

for(i in 1:length(md)){
  params=list(eta = .8, booster="gbtree",gamma=0,max_depth=md[i],min_child_weight=1,subsample=1,colsample_bylevel=.5)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = exp(predict(xgb, dtest))
}

conv_md = data.frame(iter=1:500, conv_md)
conv_md = melt(conv_md, id.vars = "iter")
ggplot(data = conv_md) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_md)^2)))

####### Tuning model 2 sub sample
Model2_Data=select(Data22,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

ss=seq(.1,1,.1)
test=nrow(Model2_Data[-train,])

conv_ss = matrix(data=NA,nrow=500,ncol=length(ss))
pred_ss = matrix(NA,nrow=test, length(ss))
colnames(conv_ss) = colnames(pred_ss) = ss

for(i in 1:length(ss)){
  params=list(eta = .8, booster="gbtree",gamma=0,max_depth=4,min_child_weight=1,subsample=ss[i],colsample_bylevel=.6)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = exp(predict(xgb, dtest))
}

conv_ss = data.frame(iter=1:500, conv_ss)
conv_ss = melt(conv_ss, id.vars = "iter")
ggplot(data = conv_ss) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_ss)^2)))

####### Tuning model 2 min child weight
Model2_Data=select(Data22,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

mcw=seq(1,10,1)
test=nrow(Model2_Data[-train,])

conv_mcw = matrix(data=NA,nrow=500,ncol=length(mcw))
pred_mcw = matrix(NA,nrow=test, length(mcw))
colnames(conv_mcw) = colnames(pred_mcw) = mcw

for(i in 1:length(mcw)){
  params=list(eta = .3, booster="gbtree",gamma=0,max_depth=6,min_child_weight=mcw[i],subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_mcw[,i] = xgb$evaluation_log$train_rmse
  pred_mcw[,i] = exp(predict(xgb, dtest))
}

conv_mcw = data.frame(iter=1:500, conv_mcw)
conv_mcw = melt(conv_mcw, id.vars = "iter")
ggplot(data = conv_mcw) + geom_line(aes(x = iter, y = value, color = variable))

red=subset(Project_Data,ln_Records>=0)
ytest=red[-train,"Total.Records"]

(RMSE_mcw = sqrt(colMeans((ytest-pred_mcw)^2)))

####### Tuning gamma Model 2 with Vuln
Model2_Data=select(Data22,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

gam=seq(0,10,1)
test=nrow(Model2_Data[-train,])

conv_gam = matrix(data=NA,nrow=500,ncol=length(gam))
pred_gam = matrix(NA,nrow=test, length(gam))
colnames(conv_gam) = colnames(pred_gam) = gam

for(i in 1:length(gam)){
  params=list(eta = .3, booster="gbtree",gamma=gam[i],max_depth=6,min_child_weight=1,subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_gam[,i] = xgb$evaluation_log$train_rmse
  pred_gam[,i] = exp(predict(xgb, dtest))
}

conv_gam = data.frame(iter=1:500, conv_gam)
conv_gam = melt(conv_gam, id.vars = "iter")
ggplot(data = conv_gam) + geom_line(aes(x = iter, y = value, color = variable))

red=subset(Project_Data,ln_Records>=0)
ytest=red[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_gam)^2)))
output$DTRMSE2=renderPrint({yhatDT2=exp(predict(prune.revenue2(),newdata=Data2[-train,]))
revenue2.test=Data2[-train,"Total.Records"]
DTRMSE2=sqrt(mean((yhatDT2-revenue2.test)^2))}
paste("RMSE:",DTRMSE2))

plot(cv.revenue2$size,cv.revenue2$dev,type="b",main="Cross Validation Model 2")

###################### Tuning eta Model 1 with daily Vuln ######################
Model1_Data=select(Project_Data2,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model1_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

eta=seq(.1,1,.1)
test=nrow(Model1_Data)-train

conv_eta = matrix(data=NA,nrow=500,ncol=length(eta))
pred_eta = matrix(NA,length(test), length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta

for(i in 1:length(eta)){
  params=list(eta = eta[i], booster="gbtree",gamma=0,max_depth=1,min_child_weight=1,subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, dtest)
}

conv_eta = data.frame(iter=1:500, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
ggplot(data = conv_eta) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Project_Data[-train,"Total.Records"]

(RMSE_eta = sqrt(colMeans((ytest-pred_eta)^2)))

####### Tuning colsample_bylevel Model 1 with daily vuln
Model1_Data=select(Project_Data2,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model1_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

cs=seq(.1,1,.1)
test=nrow(Model1_Data)-train

conv_cs = matrix(data=NA,nrow=500,ncol=length(cs))
pred_cs = matrix(NA,length(test), length(cs))
colnames(conv_cs) = colnames(pred_cs) = cs

for(i in 1:length(cs)){
  params=list(eta = .1, booster="gbtree",gamma=0,max_depth=1,min_child_weight=1,subsample=1,colsample_bylevel=cs[i])
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_cs[,i] = xgb$evaluation_log$train_rmse
  pred_cs[,i] = predict(xgb, dtest)
}

conv_cs = data.frame(iter=1:500, conv_cs)
conv_cs = melt(conv_cs, id.vars = "iter")
ggplot(data = conv_cs) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1_Data[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_cs)^2)))

####### Tuning max_depth Model 1
Model1_Data=select(Project_Data2,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model1_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

md=seq(10,20,1)
test=nrow(Model1_Data)-train

conv_md = matrix(data=NA,nrow=500,ncol=length(md))
pred_md = matrix(NA,length(test), length(md))
colnames(conv_md) = colnames(pred_md) = md

for(i in 1:length(md)){
  params=list(eta = .1, booster="gbtree",gamma=0,max_depth=md[i],min_child_weight=1,subsample=1,colsample_bylevel=.1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = predict(xgb, dtest)
}

conv_md = data.frame(iter=1:500, conv_md)
conv_md = melt(conv_md, id.vars = "iter")
ggplot(data = conv_md) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1_Data[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_md)^2)))

####### Tuning sub sample Model 1 with daily vuln
Model1_Data=select(Project_Data2,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model1_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

ss=seq(.1,1,.1)
test=nrow(Model1_Data)-train

conv_ss = matrix(data=NA,nrow=500,ncol=length(ss))
pred_ss = matrix(NA,length(test), length(ss))
colnames(conv_ss) = colnames(pred_ss) = ss

for(i in 1:length(ss)){
  params=list(eta = .1, booster="gbtree",gamma=0,max_depth=10,min_child_weight=1,subsample=ss[i],colsample_bylevel=.1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = predict(xgb, dtest)
}

conv_ss = data.frame(iter=1:500, conv_ss)
conv_ss = melt(conv_ss, id.vars = "iter")
ggplot(data = conv_ss) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1_Data[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_ss)^2)))

####### Tuning min child weight Model 1 daily vuln
Model1_Data=select(Project_Data2,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model1_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

mcw=seq(1,10,1)
test=nrow(Model1_Data)-train

conv_mcw = matrix(data=NA,nrow=500,ncol=length(mcw))
pred_mcw = matrix(NA,length(test), length(mcw))
colnames(conv_mcw) = colnames(pred_mcw) = mcw

for(i in 1:length(mcw)){
  params=list(eta = .1, booster="gbtree",gamma=0,max_depth=10,min_child_weight=mcw[i],subsample=1,colsample_bylevel=.1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_mcw[,i] = xgb$evaluation_log$train_rmse
  pred_mcw[,i] = predict(xgb, dtest)
}

conv_mcw = data.frame(iter=1:500, conv_mcw)
conv_mcw = melt(conv_mcw, id.vars = "iter")
ggplot(data = conv_mcw) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1_Data[-train,"Total.Records"]

(RMSE_mcw = sqrt(colMeans((ytest-pred_mcw)^2)))

####### Tuning gamma Model 1 daily vuln
Model1_Data=select(Project_Data2,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model1_Data,-Total.Records)

set.seed(1)
train=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rownames=TRUE)
label=Model1_Data[train,"Total.Records"]
label2=Model1_Data[-train,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

gam=seq(10,20,1)
test=nrow(Model1_Data)-train

conv_gam = matrix(data=NA,nrow=500,ncol=length(gam))
pred_gam = matrix(NA,length(test), length(gam))
colnames(conv_gam) = colnames(pred_gam) = gam

for(i in 1:length(gam)){
  params=list(eta = .1, booster="gbtree",gamma=gam[i],max_depth=10,min_child_weight=1,subsample=1,colsample_bylevel=.1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_gam[,i] = xgb$evaluation_log$train_rmse
  pred_gam[,i] = predict(xgb, dtest)
}

conv_gam = data.frame(iter=1:500, conv_gam)
conv_gam = melt(conv_gam, id.vars = "iter")
ggplot(data = conv_gam) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Model1_Data[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_gam)^2)))

####### Tuning eta Model 2 daily vuln
Model2_Data=select(Data22,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

eta=seq(.1,1,.1)
test=nrow(Model2_Data[-train,])

conv_eta = matrix(data=NA,nrow=500,ncol=length(eta))
pred_eta = matrix(NA,nrow=test, length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta

for(i in 1:length(eta)){
  params=list(eta = eta[i], booster="gbtree",gamma=0,max_depth=6,min_child_weight=1,subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = exp(predict(xgb, dtest))
}

conv_eta = data.frame(iter=1:500, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
ggplot(data = conv_eta) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_eta = sqrt(colMeans((ytest-pred_eta)^2)))

####### Tuning colsample_bylevel daily vuln Model 2
Model2_Data=select(Data22,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

cs=seq(.1,1,.1)
test=nrow(Model2_Data[-train,])

conv_cs = matrix(data=NA,nrow=500,ncol=length(cs))
pred_cs = matrix(NA,nrow=test, length(cs))
colnames(conv_cs) = colnames(pred_cs) = cs

for(i in 1:length(cs)){
  params=list(eta = 1, booster="gbtree",gamma=0,max_depth=6,min_child_weight=1,subsample=1,colsample_bylevel=cs[i])
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_cs[,i] = xgb$evaluation_log$train_rmse
  pred_cs[,i] = exp(predict(xgb, dtest))
}

conv_cs = data.frame(iter=1:500, conv_cs)
conv_cs = melt(conv_cs, id.vars = "iter")
ggplot(data = conv_cs) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_cs)^2)))

####### Tuning max_depth Model 2 daily vuln
Model2_Data=select(Data22,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN, vuln_day))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

md=seq(1,10,1)
test=nrow(Model2_Data[-train,])

conv_md = matrix(data=NA,nrow=500,ncol=length(md))
pred_md = matrix(NA,nrow=test, length(md))
colnames(conv_md) = colnames(pred_md) = md

for(i in 1:length(md)){
  params=list(eta = .1, booster="gbtree",gamma=0,max_depth=md[i],min_child_weight=1,subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = exp(predict(xgb, dtest))
}

conv_md = data.frame(iter=1:500, conv_md)
conv_md = melt(conv_md, id.vars = "iter")
ggplot(data = conv_md) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_md)^2)))

####### Tuning model 2 sub sample daily vuln
Model2_Data=select(Data22,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

ss=seq(.1,1,.1)
test=nrow(Model2_Data[-train,])

conv_ss = matrix(data=NA,nrow=500,ncol=length(ss))
pred_ss = matrix(NA,nrow=test, length(ss))
colnames(conv_ss) = colnames(pred_ss) = ss

for(i in 1:length(ss)){
  params=list(eta = .1, booster="gbtree",gamma=0,max_depth=6,min_child_weight=1,subsample=ss[i],colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = exp(predict(xgb, dtest))
}

conv_ss = data.frame(iter=1:500, conv_ss)
conv_ss = melt(conv_ss, id.vars = "iter")
ggplot(data = conv_ss) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_ss)^2)))

####### Tuning model 2 min child weight
Model2_Data=select(Data22,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

mcw=seq(1,10,1)
test=nrow(Model2_Data[-train,])

conv_mcw = matrix(data=NA,nrow=500,ncol=length(mcw))
pred_mcw = matrix(NA,nrow=test, length(mcw))
colnames(conv_mcw) = colnames(pred_mcw) = mcw

for(i in 1:length(mcw)){
  params=list(eta = .1, booster="gbtree",gamma=0,max_depth=6,min_child_weight=mcw[i],subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_mcw[,i] = xgb$evaluation_log$train_rmse
  pred_mcw[,i] = exp(predict(xgb, dtest))
}

conv_mcw = data.frame(iter=1:500, conv_mcw)
conv_mcw = melt(conv_mcw, id.vars = "iter")
ggplot(data = conv_mcw) + geom_line(aes(x = iter, y = value, color = variable))

red=subset(Project_Data,ln_Records>=0)
ytest=red[-train,"Total.Records"]

(RMSE_mcw = sqrt(colMeans((ytest-pred_mcw)^2)))

####### Tuning gamma Model 2 with Vuln
Model2_Data=select(Data22,c(ln_Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model2_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model2_Data),.5*nrow(Model2_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model2_Data[train,"ln_Records"]
label2=Model2_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

gam=seq(0,10,1)
test=nrow(Model2_Data[-train,])

conv_gam = matrix(data=NA,nrow=500,ncol=length(gam))
pred_gam = matrix(NA,nrow=test, length(gam))
colnames(conv_gam) = colnames(pred_gam) = gam

for(i in 1:length(gam)){
  params=list(eta = .1, booster="gbtree",gamma=gam[i],max_depth=6,min_child_weight=7,subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_gam[,i] = xgb$evaluation_log$train_rmse
  pred_gam[,i] = exp(predict(xgb, dtest))
}

conv_gam = data.frame(iter=1:500, conv_gam)
conv_gam = melt(conv_gam, id.vars = "iter")
ggplot(data = conv_gam) + geom_line(aes(x = iter, y = value, color = variable))

red=subset(Project_Data,ln_Records>=0)
ytest=red[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_gam)^2)))

####### Tuning eta Model 3 daily vuln
Model3_Data=select(Data23,c(ln_Records,ln_Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model3_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model3_Data),.5*nrow(Model3_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model3_Data[train,"ln_Records"]
label2=Model3_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

eta=seq(.1,1,.1)
test=nrow(Model2_Data[-train,])

conv_eta = matrix(data=NA,nrow=500,ncol=length(eta))
pred_eta = matrix(NA,nrow=test, length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta

for(i in 1:length(eta)){
  params=list(eta = eta[i], booster="gbtree",gamma=0,max_depth=6,min_child_weight=1,subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = exp(predict(xgb, dtest))
}

conv_eta = data.frame(iter=1:500, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
ggplot(data = conv_eta) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_eta = sqrt(colMeans((ytest-pred_eta)^2)))

####### Tuning colsample_bylevel daily vuln Model 3
Model3_Data=select(Data23,c(ln_Records,ln_Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model3_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model3_Data),.5*nrow(Model3_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model3_Data[train,"ln_Records"]
label2=Model3_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

cs=seq(.1,1,.1)
test=nrow(Model2_Data[-train,])

conv_cs = matrix(data=NA,nrow=500,ncol=length(cs))
pred_cs = matrix(NA,nrow=test, length(cs))
colnames(conv_cs) = colnames(pred_cs) = cs

for(i in 1:length(cs)){
  params=list(eta = 1, booster="gbtree",gamma=0,max_depth=6,min_child_weight=1,subsample=1,colsample_bylevel=cs[i])
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_cs[,i] = xgb$evaluation_log$train_rmse
  pred_cs[,i] = exp(predict(xgb, dtest))
}

conv_cs = data.frame(iter=1:500, conv_cs)
conv_cs = melt(conv_cs, id.vars = "iter")
ggplot(data = conv_cs) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_cs)^2)))

####### Tuning max_depth Model 3 daily vuln
Model3_Data=select(Data23,c(ln_Records,ln_Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN, vuln_day))
Data=select(Model3_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model3_Data),.5*nrow(Model3_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model3_Data[train,"ln_Records"]
label2=Model3_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

md=seq(1,10,1)
test=nrow(Model2_Data[-train,])

conv_md = matrix(data=NA,nrow=500,ncol=length(md))
pred_md = matrix(NA,nrow=test, length(md))
colnames(conv_md) = colnames(pred_md) = md

for(i in 1:length(md)){
  params=list(eta = 1, booster="gbtree",gamma=0,max_depth=md[i],min_child_weight=1,subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = exp(predict(xgb, dtest))
}

conv_md = data.frame(iter=1:500, conv_md)
conv_md = melt(conv_md, id.vars = "iter")
ggplot(data = conv_md) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_cs = sqrt(colMeans((ytest-pred_md)^2)))

####### Tuning model 3 sub sample daily vuln
Model3_Data=select(Data23,c(ln_Records,ln_Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model3_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model3_Data),.5*nrow(Model3_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model3_Data[train,"ln_Records"]
label2=Model3_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

ss=seq(.1,1,.1)
test=nrow(Model2_Data[-train,])

conv_ss = matrix(data=NA,nrow=500,ncol=length(ss))
pred_ss = matrix(NA,nrow=test, length(ss))
colnames(conv_ss) = colnames(pred_ss) = ss

for(i in 1:length(ss)){
  params=list(eta = 1, booster="gbtree",gamma=0,max_depth=8,min_child_weight=1,subsample=ss[i],colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = exp(predict(xgb, dtest))
}

conv_ss = data.frame(iter=1:500, conv_ss)
conv_ss = melt(conv_ss, id.vars = "iter")
ggplot(data = conv_ss) + geom_line(aes(x = iter, y = value, color = variable))

ytest=Data2[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_ss)^2)))

####### Tuning model 3 min child weight daily vuln
Model3_Data=select(Data23,c(ln_Records,ln_Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model3_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model3_Data),.5*nrow(Model3_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model3_Data[train,"ln_Records"]
label2=Model3_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

mcw=seq(1,10,1)
test=nrow(Model2_Data[-train,])

conv_mcw = matrix(data=NA,nrow=500,ncol=length(mcw))
pred_mcw = matrix(NA,nrow=test, length(mcw))
colnames(conv_mcw) = colnames(pred_mcw) = mcw

for(i in 1:length(mcw)){
  params=list(eta = 1, booster="gbtree",gamma=0,max_depth=8,min_child_weight=mcw[i],subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_mcw[,i] = xgb$evaluation_log$train_rmse
  pred_mcw[,i] = exp(predict(xgb, dtest))
}

conv_mcw = data.frame(iter=1:500, conv_mcw)
conv_mcw = melt(conv_mcw, id.vars = "iter")
ggplot(data = conv_mcw) + geom_line(aes(x = iter, y = value, color = variable))

red=subset(Project_Data,ln_Records>=0)
ytest=red[-train,"Total.Records"]

(RMSE_mcw = sqrt(colMeans((ytest-pred_mcw)^2)))

####### Tuning gamma Model 3 with daily Vuln
Model3_Data=select(Data23,c(ln_Records,ln_Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
Data=select(Model3_Data,-ln_Records)

set.seed(1)
train=sample(1:nrow(Model3_Data),.5*nrow(Model3_Data))
Train.Data=as.matrix(Data[train,],rowname=TRUE)
Test.Data=as.matrix(Data[-train,],rowname=TRUE)
label=Model3_Data[train,"ln_Records"]
label2=Model3_Data[-train,"ln_Records"]

dtrain=xgb.DMatrix(data=Train.Data,label=label)
dtest=xgb.DMatrix(data=Test.Data,label=label2)

gam=seq(10,20,1)
test=nrow(Model2_Data[-train,])

conv_gam = matrix(data=NA,nrow=500,ncol=length(gam))
pred_gam = matrix(NA,nrow=test, length(gam))
colnames(conv_gam) = colnames(pred_gam) = gam

for(i in 1:length(gam)){
  params=list(eta = 1, booster="gbtree",gamma=gam[i],max_depth=8,min_child_weight=1,subsample=1,colsample_bylevel=1)
  xgb=xgboost(dtrain, nrounds = 500, params = params,verbose=0)
  conv_gam[,i] = xgb$evaluation_log$train_rmse
  pred_gam[,i] = exp(predict(xgb, dtest))
}

conv_gam = data.frame(iter=1:500, conv_gam)
conv_gam = melt(conv_gam, id.vars = "iter")
ggplot(data = conv_gam) + geom_line(aes(x = iter, y = value, color = variable))

red=subset(Project_Data,ln_Records>=0)
ytest=red[-train,"Total.Records"]

(RMSE_ss = sqrt(colMeans((ytest-pred_gam)^2)))

