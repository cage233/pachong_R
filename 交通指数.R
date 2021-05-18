#--------------------加载包--------------------
rm(list = ls())
library(RCurl)
library(stringr)
library(httr)
library(jsonlite)
library(dplyr)
library(readxl)

json_to_csv<-function(json){
  email<-"495165378@qq.com"
  url="https://json-csv.com/api/getcsv"
  params<-list(
    'email'= email,
    'json'= json
  )
  html<-POST(url,body = params, encode = "form")
  mycondition<-content(html)
  mycondition
}


getmessage<-function(areaCode,dataTime){
  url<-paste("https://jiaotong.baidu.com/trafficindex/dashboard/historycurve?areaCode=",areaCode,"&type=0&dataTime=",dataTime,"&callback=jsonp_1614326602817_3562020",sep="")
  html<-getURL(url,encoding = 'utf-8')
  
  html_new<-strsplit(html,"\"data\":") %>% unlist(.)%>%.[2]%>%strsplit(.,",\"message\"")%>% unlist(.)%>%.[1]
  output<-json_to_csv(html_new)[,-1]
  names(output)<-c('date','jam_distance')
  
  output$date<-as.POSIXct(output$date,origin='1970-01-01')
  output
}

getmessageGD<-function(cityCode,year,quarter){
  url<-paste("https://report.amap.com/ajax/cityDailyQuarterly.do?cityCode=",cityCode,"&year=",year,"&quarter=",quarter,sep="")
  html<-getURL(url,encoding = 'utf-8')
  output<-json_to_csv(html)
  output
}




yeararray<-c("2020","2020","2020","2021")
quarterarray<-c("2","3","4","1")

cityCode<-"110000"
output<-NULL
for (i in 1:4) {
  year<-yeararray[i]
  quarter<-quarterarray[i]
  output0<-getmessageGD(cityCode,year,quarter)
  output<-rbind(output,output0)
}

write.table(output,file = "C:/Users/richard.jin/Desktop/dataoutput.csv",sep = ",",row.names = F,quote = F)
