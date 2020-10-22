rm(list = ls())
library(rvest)
library("RCurl")
library(dplyr)
library(readr)

getLink1<-function(){
url0<-"http://www.openbook.com.cn/CategoryDetails/2100.html"
html<-getURL(url0,.encoding ="utf-8")
web <- read_html(html)

Name<-web%>%html_nodes('h2.sybule')%>%html_text
Link<-web%>%html_nodes('div.phside05 div.tbcot div a')%>%html_attr('href')
Link<-paste('http://www.openbook.com.cn',Link,sep = "")
data.frame(Name,Link)
}


getLink<-function(result0){
Category<-NULL
Name<-NULL
Link<-NULL


for(a in 1:nrow(result0)){
for (i in 0:30) {
  Link0<-result0$Link[a]
  Link0<-substr(Link0,1,41)
  url0 <- paste(Link0,i, '.html',sep = "")
  html<-getURL(url0,.encoding ="utf-8")
  web <- read_html(html)
  
  Name0<-web%>%html_nodes('div.list ul li a.sy0b')%>%html_text
  Link0<-web%>%html_nodes('div.list ul li a.sy0b')%>%html_attr('href')
  Category0<-rep(as.character(result0$Name[a]),length(Link0))
  
  Category<-append(Category,Category0)
  Name<-append(Name,Name0)
  Link<-append(Link,Link0)
}
}
  Link<-paste('http://www.openbook.com.cn',Link,sep = "")
  data.frame(Category,Name,Link)
}


getData<-function(result){
df<-c("排名" = NULL,"ISBN" = NULL,"书名" = NULL, "出版社" = NULL,"作者" = NULL, "定价" = NULL, "category1" = NULL,"category2" = NULL)
for (i in 1:nrow(result)) {
  url0 <- result$Link[i]
  html<-getURL(url0,.encoding ="utf-8")
  web <- read_html(html)
  
  pd<-web%>%html_nodes('table')%>%html_table()
  if(length(pd)>1){
    df0<-as.data.frame(pd[length(pd)])
  } else {
  df0<-web%>%html_nodes('table')%>%html_table()%>%as.data.frame()
  }
  names(df0)<-as.array(t(df0[1,]))
  df0<-df0[-1,]

  df0$category1<- result$Category[i]
  df0$category2<- result$Name[i]
  df<- dplyr::bind_rows(df,df0)
}
}


result0<-getLink1()
result<-getLink(result0)
getData(result)

write_excel_csv(df,path = "C:/Users/richard.jin/Desktop/BJKJ.csv")
