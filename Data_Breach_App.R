library(shiny)
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

Project_Data<-read.csv("R_Project_Data.csv",header=T)

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

######### inputting day breach data into Project data set
Project_Data2=mutate(Project_Data,publdate=ymd(Project_Data$Date.Made.Public))
Project_Data2=right_join(dataday5,Project_Data2,by="publdate")
vuln_day=replace_na(Project_Data2$vuln_day,0)
vuln_day=data.frame(vuln_day)
Project_Data2=data.frame(select(Project_Data2,-vuln_day))
Project_Data2=cbind(Project_Data2,vuln_day)

Data22=subset(Project_Data2,ln_Records>=0)
Data23=subset(Project_Data2,ln_Records>=0&ln_Revenue>=0)

############################ descriptive statistics setup####################################
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

########################## OLS setup ############################
########## OLS Model without vulnerability data
set.seed(1)
train=sample(1:nrow(Project_Data),.5*nrow(Project_Data))
train.data=Project_Data[train,]
test.data=Project_Data[-train,c("Revenue","Employ","DISC","HACK","PHYS","INSD","UNKN","Vuln")]
model1=lm(Total.Records~Revenue+Employ+DISC+HACK+PHYS+INSD+UNKN+Vuln+0,data=train.data)

######### OLS Model 2 with daily vulnerability data
set.seed(1)
train22=sample(1:nrow(Data22),.5*nrow(Data22))
train22.data=Data22[train22,]
test22.data=Data22[-train22,c("Revenue","Employ","DISC","HACK","PHYS","INSD","UNKN","vuln_day")]
model22=lm(ln_Records~Revenue+Employ+DISC+HACK+PHYS+INSD+UNKN+vuln_day+0,data=train22.data)

######## OLS Model 3 with daily vulnerability data
set.seed(1)
train23=sample(1:nrow(Data23),.5*nrow(Data23))
train23.data=Data23[train23,]
test23.data=Data23[-train23,c("ln_Revenue","Employ","DISC","HACK","PHYS","INSD","UNKN","vuln_day")]
model23=lm(ln_Records~ln_Revenue+Employ+DISC+HACK+PHYS+UNKN+vuln_day+0,data=train23.data)

############# Decision Tree Setup ########################################################3
########## DT Model 2
set.seed(1)
train2=sample(1:nrow(Data2),.5*nrow(Data2))
########## DT Model 3
set.seed(1)
train3=sample(1:nrow(Data3),.5*nrow(Data3))

############ XGBoosting setup ##############################
######### XGBoosting Model 1 with daily vulnerability data
Model1_Data=select(Project_Data2,c(Total.Records,Revenue,Employ,DISC,HACK,INSD,PHYS,UNKN,vuln_day))
DataXGB=select(Model1_Data,-Total.Records)

set.seed(1)
trainXGB=sample(1:nrow(Model1_Data),.5*nrow(Model1_Data))
Train.DataXGB=as.matrix(DataXGB[trainXGB,],rowname=TRUE)
Test.DataXGB=as.matrix(DataXGB[-trainXGB,],rownames=TRUE)
label=Model1_Data[trainXGB,"Total.Records"]
label2=Model1_Data[-trainXGB,"Total.Records"]

dtrain=xgb.DMatrix(data=Train.DataXGB,label=label)
dtest=xgb.DMatrix(data=Test.DataXGB,label=label2)
params=list(booster="gbtree",eta=.1,gamma=0,max_depth=10,min_child_weight=1,subsample=1,colsample_bylevel=.1)

modelXGB1=xgb.train(params=params,data=dtrain,nrounds=500)

model1.importance=xgb.importance(model=modelXGB1)

########### XGBoosting Model 2 with Monthly Vulnerability Data
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

######### XGBoosting Model 3 with daily vulnerability data
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

ui <-navbarPage("Predicting Data Breach Size",
        tabPanel("Descriptive Statistics",sidebarLayout(
                sidebarPanel("Graph Inputs",
                        selectInput("xcat",h3("X-axis variable:"),choices=c("Breach Type"="Type.of.breach","Organization Type"="Type.of.organization","Total Records"="Total.Records","Revenue"="Revenue","Employment"="Employ")), 
                        selectInput("subcat",h3("X sub category"),choices=c("Breach Type"="Type.of.breach","Organization Type"="Type.of.organization")),
                        selectInput("ycat",h3("Y-axis variable:"),choices=c("Total.Records","Revenue","Employ"))),
                mainPanel(
                        tabsetPanel(
                                tabPanel("Bar Graph",radioButtons("bartype","Bar Graph Type",choices=c("Two Variables"="2var","Count"="count","Proportion"="prop")),plotOutput("Bar")),
                                tabPanel("Scatter Plot",
                                         checkboxInput("adjscale","Adjusted Scales",value=FALSE),
                                         column(4,numericInput("xmin","x-axis: Minimum",value=0),numericInput("xmax","x-axis: Maximum",value=100000000)),
                                         column(4,numericInput("ymin","y-axis: Minimum",value=0),numericInput("ymax","y-axis: Maximum",value=100000000)),
                                         plotOutput("Scat")),
                                tabPanel("Box Plot",
                                         checkboxInput("adjscale2","Adjusted Scales",value=FALSE),
                                         column(4,numericInput("ymin2","y-axis: Minimum",value=0),numericInput("ymax2","y-axis: Maximum",value=100000000)),
                                         plotOutput("Boxplot")),
                                tabPanel("Table",tableOutput("Desc"))
                                ))
                )),
        navbarMenu("OLS", 
             tabPanel("Model 1", titlePanel("OLS: Model 1 with Monthly Vulnerability Data"), tableOutput("OLS1"),
                      textOutput("OLSRMSE1")),
             tabPanel("Model 2",titlePanel("OLS: Model 2 with Daily Vulnerability Data"),tableOutput("OLS2"),
                      textOutput("OLSRMSE2")),
             tabPanel("Model 3",titlePanel("OLS: Model 3 with Daily Vulnerability Data"),tableOutput("OLS3"),
             textOutput("OLSRMSE3"))),
        navbarMenu("Decision Tree", 
             tabPanel("Model 1",titlePanel("Decision Tree Model 1"),column(6,plotOutput("DTCV")),
                      column(6,plotOutput("DT1"),textOutput("DTRMSE1"))),
             tabPanel("Model 2",titlePanel("Decision Tree Model 2"),column(4,plotOutput("DTCV2")), 
                      column(8,plotOutput("DT2"),textOutput("DTRMSE2")),),
             tabPanel("Model 3",titlePanel("Decision Tree Model 3"),column(4,plotOutput("DTCV3")),
                      column(8,plotOutput("DT3"),textOutput("DTRMSE3")))),
        navbarMenu("Random Forest", 
             tabPanel("Model 1",titlePanel("Random Forest Model 1"),tableOutput("RF1"),
                      textOutput("RFRMSE1"),textOutput("RF1summary")),
             tabPanel("Model 2",titlePanel("Random Forest Model 2 with Monthly Vulnerability Data"),tableOutput("RF2"),
                      textOutput("RFRMSE2"),column(8,textOutput("sumRF2"))),
             tabPanel("Model 3",titlePanel("Random Forest Model 3 with Monthly Vulnerability Data"),tableOutput("RF3"),
                      textOutput("RFRMSE3"))),
        navbarMenu("XGBoost", 
             tabPanel("Model 1",titlePanel("XGBoosting Model 1 with Daily Vulnerability Data "),column(8,plotOutput("XGB1")),
                      column(4,textOutput("XGBRMSE1"))),
             tabPanel("Model 2",titlePanel("XGBoosting Model 2 with Monthly Vulnerability Data"),column(8, plotOutput("XGB2")),
                      column(4,textOutput("XGBRMSE2"))),
             tabPanel("Model 3",titlePanel("XGBoosting Model 3 with Daily Vulnerability Data"),column(8,plotOutput("XGB3")),
                      column(4,textOutput("XGBRMSE3"))))
        )


server <- function(input, output) {
############### descriptive statistics #################################
output$Bar=renderPlot({Bar=switch(input$bartype,
                                  "2var"={z=ggplot(data=Project_Data2,aes(x=Project_Data2[,input$xcat], y=Project_Data2[,input$ycat],fill=Project_Data2[,input$subcat]))+geom_bar(stat="identity")+
                                          labs(x=input$xcat,y=input$ycat,fill=input$subcat)},
                                  "count"={z=ggplot(data=Project_Data2,aes(x=Project_Data2[,input$xcat],fill=Project_Data2[,input$subcat]))+geom_bar(stat="count")+
                                                  labs(x=input$xcat,fill=input$subcat)},
                                  "prop"={z=ggplot(data=Project_Data2,aes(x=Project_Data2[,input$xcat], y=Project_Data2[,input$ycat],fill=Project_Data2[,input$subcat]))+geom_bar(stat="identity",position="fill")+
                                          labs(x=input$xcat,y=input$ycat,fill=input$subcat)}
                                  )
z
        })
output$Scat=renderPlot({if(input$adjscale==FALSE){
        ggplot(data=Project_Data2,aes(x=Project_Data2[,input$xcat],y=Project_Data2[,input$ycat],color=Project_Data2[,input$subcat]))+
                geom_point()+
                labs(x=input$xcat,y=input$ycat,color=input$subcat)}
        else{ggplot(data=Project_Data2,aes(x=Project_Data2[,input$xcat],y=Project_Data2[,input$ycat],color=Project_Data2[,input$subcat]))+
                        geom_point()+
                        coord_cartesian(xlim=c(input$xmin,input$xmax),ylim=c(input$ymin,input$ymax))+
                        labs(x=input$xcat,y=input$ycat,color=input$subcat)}
        
})

output$Boxplot=renderPlot({if(input$adjscale2==FALSE){
        ggplot(data=Project_Data2,aes(x=Project_Data2[,input$xcat],y=Project_Data2[,input$ycat],color=Project_Data2[,input$subcat]))+
                geom_boxplot()+
                labs(x=input$xcat,y=input$ycat,color=input$subcat)}
        else{ggplot(data=Project_Data2,aes(x=Project_Data2[,input$xcat],y=Project_Data2[,input$ycat],color=Project_Data2[,input$subcat]))+
                        geom_boxplot()+
                        coord_cartesian(ylim=c(input$ymin2,input$ymax2))+
                        labs(x=input$xcat,y=input$ycat,color=input$subcat)}
        
})

output$Desc=renderTable({descript},rownames=TRUE)
########################## OLS ############################
########## OLS Model 
        
output$OLS1=renderTable({data.frame(summary(model1)$coefficients)},rownames=TRUE)
        
output$OLSRMSE1=renderPrint({yhat=predict(model1,newdata=test.data)
        OLSRMSE1=round(sqrt(mean((Project_Data[-train,"Total.Records"]-yhat)^2)))
        paste("RMSE:",OLSRMSE1)})

######## OLS Model 2
output$OLS2=renderTable({data.frame(summary(model22)$coefficients)},rownames=TRUE)

output$OLSRMSE2=renderPrint({yhat22=exp(predict(model22,newdata=test22.data))
        OLSRMSE2=round(sqrt(mean((Data22[-train22,"Total.Records"]-yhat22)^2)))
        paste("RMSE:",OLSRMSE2)       
})

####### OLS Model 3
output$OLS3=renderTable({data.frame(summary(model23)$coefficients)},rownames=TRUE)

output$OLSRMSE3=renderPrint({yhat23=exp(predict(model23,newdata=test23.data))
OLSRMSE23=round(sqrt(mean((Data23[-train23,"Total.Records"]-yhat23)^2)))
paste("RMSE:",OLSRMSE23)
})


########################## Decision Tree ###############################################
########## DT Model 1
output$DTCV=renderPlot({
DT1=tree(Total.Records~Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS,Project_Data,subset=train)
DTCV=tree::cv.tree(DT1)
plot(DTCV$size,DTCV$dev,type="b")
})

output$DT1=renderPlot({
DT1=tree(Total.Records~Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS,Project_Data,subset=train)
DTprune1=prune.tree(DT1,best=2)
plot(DTprune1)
text(DTprune1,pretty=0)})

output$DTRMSE1=renderPrint({
DT1=tree(Total.Records~Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS,Project_Data,subset=train)
DTprune1=prune.tree(DT1,best=2)
yhatDTCV=predict(DTprune1,newdata=Project_Data[-train,])
DTRMSE1=round(sqrt(mean((Project_Data[-train,"Total.Records"]-yhatDTCV)^2)))
paste("RMSE:",DTRMSE1)})

########### DT Model 2

output$DTCV2=renderPlot({
DT2=tree(ln_Records~Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS,Data2,subset=train2)
DTCV2=tree::cv.tree(DT2)
plot(DTCV2$size,DTCV2$dev,type="b")
})

output$DT2=renderPlot({
DT2=tree(ln_Records~Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS,Data2,subset=train2)
DT.prune2=prune.tree(DT2,best=2)
plot(DT.prune2)
text(DT.prune2,pretty=0)})

output$DTRMSE2=renderPrint({
DT2=tree(ln_Records~Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS,Data2,subset=train2)
DT.prune2=prune.tree(DT2,best=2)
yhatDT2=exp(predict(DT.prune2,newdata=Data2[-train2,]))
DTRMSE2=round(sqrt(mean((Data2[-train2,"Total.Records"]-yhatDT2)^2)))
paste("RMSE:",DTRMSE2)
})

############ DT Model 3
output$DTCV3=renderPlot({
DT3=tree(ln_Records~ln_Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS,Data3,subset=train3)
CVDT3=cv.tree(DT3)
plot(CVDT3$size, CVDT3$dev,type="b")
})

output$DT3=renderPlot({
DT3=tree(ln_Records~ln_Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS,Data3,subset=train3)
DT.prune3=prune.tree(DT3,best=2)
plot(DT.prune3)
text(DT.prune3,pretty=0)
})

output$DTRMSE3=renderPrint({
DT3=tree(ln_Records~ln_Revenue+Employ+DISC+HACK+UNKN+INSD+PHYS,Data3,subset=train3)
DT.prune3=prune.tree(DT3,best=2)
yhatDT3=exp(predict(DT.prune3,newdata=Data3[-train3,]))
revenueDT3.test=
DTRMSE3=round(sqrt(mean((Data3[-train3,"Total.Records"]-yhatDT3)^2)))
paste("RMSE:",DTRMSE3)})

##################### Random Forest ################################################
############ RF Model 1
output$RF1=renderTable({
        set.seed(1)
        train=sample(1:nrow(Project_Data),.5*nrow(Project_Data))
        
rf.revenue1=randomForest(Total.Records~Revenue+Employ+DISC+HACK+INSD+PHYS+UNKN,data=Project_Data,subset=train,mtry=3,importance=TRUE)
data.frame(rf.revenue1$importance)},rownames=TRUE)

output$RFRMSE1=renderPrint({set.seed(1)
rf.revenue1=randomForest(Total.Records~Revenue+Employ+DISC+HACK+INSD+PHYS+UNKN,data=Project_Data,subset=train,mtry=3,importance=TRUE)
yhatRF1=exp(predict(rf.revenue1,newdata=Project_Data[-train,]))
RFRMSE1=round(sqrt(mean((Project_Data[-train,"Total.Records"]-yhatRF1)^2)))
paste("RMSE:",RFRMSE1)})

output$RF1summary=renderPrint({
dim(Project_Data[-train,"Total.Records"])

})

############ RF Model 2 with monthly vulnerability data
output$RF2=renderTable({
set.seed(1)
train2=sample(1:nrow(Data2),.5*nrow(Data2))
rf.revenue2=randomForest(ln_Records~Revenue+Employ+HACK+PHYS+INSD+UNKN+DISC+Vuln,data=Data2,subset=train2,mtry=8,importance=TRUE)
data.frame(rf.revenue2$importance)},rownames=TRUE)

output$RFRMSE2=renderPrint({
set.seed(1)
train2=sample(1:nrow(Data2),.5*nrow(Data2))
rf.revenue2=randomForest(ln_Records~Revenue+Employ+HACK+PHYS+INSD+UNKN+DISC+Vuln,data=Data2,subset=train2,mtry=8,importance=TRUE)
yhatRF2=exp(predict(rf.revenue2,newdata=Data2[-train2,]))
RFRMSE2=round(sqrt(mean((Data2[-train2,"Total.Records"]-yhatRF2)^2)))
paste("RMSE:",RFRMSE2)
})

########## RF Model 3 with monthly vulnerability data
output$RF3=renderTable({
set.seed(1)
train3=sample(1:nrow(Data3),.5*nrow(Data3))
rf.revenue3=randomForest(ln_Records~ln_Revenue+Employ+DISC+HACK+INSD+PHYS+UNKN+Vuln,data=Data3,subset=train3,mtry=8,importance=TRUE)
data.frame(rf.revenue3$importance)},rownames=TRUE)

output$RFRMSE3=renderPrint({
set.seed(1)
train3=sample(1:nrow(Data3),.5*nrow(Data3))
rf.revenue3=randomForest(ln_Records~ln_Revenue+Employ+DISC+HACK+INSD+PHYS+UNKN+Vuln,data=Data3,subset=train3,mtry=8,importance=TRUE)
yhatRF3=exp(predict(rf.revenue3,newdata=Data3[-train3,]))
RFRMSE3=round(sqrt(mean((Data3[-train3,"Total.Records"]-yhatRF3)^2)))
paste("RMSE:",RFRMSE3)})

########################### XGBoosting ###########################################################
################## Model 1 with daily vulnerability data
output$XGB1=renderPlot({

xgb.plot.importance(importance_matrix=model1.importance)

})

output$XGBRMSE1=renderPrint({
yhat1=predict(modelXGB1,dtest)
boost.test=Model1_Data[-trainXGB,"Total.Records"]
XGBRMSE1=round(sqrt(mean((boost.test-yhat1)^2)))
paste("RMSE:",XGBRMSE1)})

########## XGBoosting Model 2 with monthly vulnerability data
output$XGB2=renderPlot({
modelXGB2V.importance=xgb.importance(model=modelXGB2V)
xgb.plot.importance(importance_matrix=modelXGB2V.importance)})

output$XGBRMSE2=renderPrint({
yhatXGB2V=exp(predict(modelXGB2V,dtestXGB2V))
boost.testXGB2V=Data2[-trainXGB2V,"Total.Records"]
XGBRMSE2=round(sqrt(mean((boost.testXGB2V-yhatXGB2V)^2)))
paste("RMSE:",XGBRMSE2)
})

############ XGBoosting model 3 with daily vulnerability data
output$XGB3=renderPlot({
modelXGB3.importance=xgb.importance(model=modelXGB3)
xgb.plot.importance(importance_matrix=modelXGB3.importance)})

output$XGBRMSE3=renderPrint({
yhatXGB3=exp(predict(modelXGB3,dtestXGB3))
boost.testXGB3=Data23[-trainXGB3,"Total.Records"]
RMSE3=round(sqrt(mean((boost.testXGB3-yhatXGB3)^2)))
paste("RMSE:", RMSE3)})
}

shinyApp(ui = ui, server = server)