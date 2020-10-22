rm(list=ls())
library("RCurl")
library("XML")
library("dplyr")
library("tidyr")
library("tseries")
library("forecast")
library("prophet")
library("ggplot2")


#---------------洗数小程序---------------
strrd<-function(txt,sep,num){
  if (missing(num)){
    htm1<-strsplit(txt,sep)
    htm2<-unlist(htm1)}
  else
  {htm1<-strsplit(txt,sep)
  htm2<-unlist(htm1)
  htm2[num]}
}


#---------------爬虫程序------------------
dayDayFund<-function(i){
#define url
url<-paste("http://fund.eastmoney.com/pingzhongdata/",i,".js",sep="")
#find the useful part
html<-getURL(url,.encoding ="utf-8")
html1<-strrd(html,"Data_netWorthTrend =",2)
html1<-strrd(html1,"累计净值走势",1)
html1<-strrd(html1,"\\},\\{")
html9<-as.data.frame(html1)
#split the useful part
html10<-separate(html9,col = html1,into=c("else1","time"),sep ="\"x\":") %>%
  separate(col=time,into = c("time","net_value"),sep=",\"y\":") %>%
  separate(col=net_value,into = c("net_value","equityReturn"),sep=",\"equityReturn\":") %>%
  separate(col=equityReturn,into= c("equityReturn","unitMoney"),sep = ",\"unitMoney\":\"") %>%
  separate(col=unitMoney,into=c("unitMoney","else2"),sep="\"") %>%
  subset(select=-c(else1,else2))
#set the form
html10$code = i
html10<-html10%>%select("code","time","net_value","equityReturn","unitMoney")
html10$time<-as.character(as.POSIXct(as.numeric(html10$time)/1000, origin="1970-01-01"))
html10$net_value<-as.numeric(html10$net_value)
html10$equityReturn<-as.numeric(html10$equityReturn)
html10
}

dayDayFund_Name<-function(i){
  
  url<-paste("http://fund.eastmoney.com/pingzhongdata/",i,".js",sep="")
  #find the useful part
  html<-getURL(url,.encoding ="utf-8")
  html1<-strrd(html,"fS_name = \"",2)
  html2<-strrd(html1,"\";var fS_code",1)
  html2
}

#基于30天/90天/180天建立模型，并预测未来k天

forecast_rd<-function(data,d,k){
df<-data[(nrow(data)-d+1) : nrow(data),2:3]
names(df)<-c("ds","y")
m<-prophet(df,growth = "linear",yearly.seasonality = FALSE,weekly.seasonality = FALSE,daily.seasonality=TRUE)
output<-make_future_dataframe(m,k,freq='day',include_history = TRUE)
fst<-predict(m,output)

fst
}

forecast_rd_plot<-function(data,d,k){
  df<-data[(nrow(data)-d+1) : nrow(data),2:3]
  names(df)<-c("ds","y")
  m<-prophet(df,growth = "linear",yearly.seasonality = FALSE,weekly.seasonality = FALSE,daily.seasonality=TRUE)
  output<-make_future_dataframe(m,k,freq='day',include_history = TRUE)
  fst<-predict(m,output)
  plot (m,fst,uncertainty =TRUE, plot_cap = TRUE, xlabel = 'ds', ylabel = 'y')
}



dayDayFund_realTime<-function(i){
  url<-paste("http://fundgz.1234567.com.cn/js/",i,".js",sep="")
  html<-getURL(url,.encoding ="utf-8")
  html1<-strrd(html,"gsz\":\"",2)
  html2<-as.numeric(strrd(html1,"\",\"gszzl",1))
  html2
}


forecast_increase<-function(outdata,k,i){
  est_value = round(outdata$yhat[nrow(outdata)-k+1],4)
  est_value_lst=round(outdata$yhat[nrow(outdata)-k],4)
  realTime_value = round(dayDayFund_realTime(i),4)
  error<-paste(round((realTime_value/est_value-1)*100,4),"%",sep="")
  error_numic<-realTime_value/est_value-1
  trend_numic<- est_value/est_value_lst-1
  
  
  if(error_numic<(-0.02)){
    advice<-"当前实际值远小于估值，建议买入"
  } else if(error_numic<(-0.01)){
    advice<-"当前实际值略小于估值，建议适当买入"
  } else if(error_numic<0.01){
    advice<-"当前实际值与估值接近，建议根据模型趋势进行选择"
  } else if(error_numic<0.02){
    advice<-"当前实际值略高于估值，建议适当卖出"
  } else {
    advice<-"当前实际值远高于估值，建议卖出"
  }
  
  
  
  
  output<-c(est_value,realTime_value,error,advice)
  output
}
