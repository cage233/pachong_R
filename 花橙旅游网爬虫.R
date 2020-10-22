rm(list = ls())
library(rvest)
library(RCurl)
library(readr)
library(dplyr)

getComment<-function(){

print("start")
df<- NULL

  for (i in 1:317) {
    print(i)
    url0<-paste("https://www.smartoct.com/comment/listPage?page=",i,sep = "")
    html<-getURL(url0,.encoding ="utf-8")
    web <- read_html(html)
    
    ID<-web%>%html_nodes('a h3')%>%html_text%>%gsub("\r|\n|\t| ", "", .)
    comment<-web%>%html_nodes('div.comment p')%>%html_attr('title')
    date<-web%>%html_nodes('div.date-time-box span:nth-child(1)')%>%html_text
    address<-web%>%html_nodes('div.date-time-box span:nth-child(2)')%>%html_attr('title')
    
    df0<-data.frame(ID,comment,date,address)
    df<- dplyr::bind_rows(df,df0)
    
  }

df

}


getComment()

write_excel_csv(df,path = "C:/Users/richard.jin/Desktop/HC_Comment.csv")
