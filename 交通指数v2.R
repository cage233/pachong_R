#--------------------加载包--------------------
rm(list = ls())
library(RCurl)
library(stringr)
library(httr)
library(jsonlite)
library(dplyr)
library(readxl)
library(rvest)
library(downloader)

#--------------------公式区--------------------
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


url<-"http://www.mot.gov.cn/tongjishuju/gonglu/index.html"
html<-getURL(url,encoding = 'utf-8')
web <- read_html(html)
id<-web%>%html_nodes('a.list-group-item')%>%html_attr('title')
url_detail<-web%>%html_nodes('a.list-group-item')%>%html_attr('href')
i=1
for (i in 1:length(ID)) {
  
  if (ifelse(is.na(str_locate(id[i],"公路旅客运输量")[1]),0,str_locate(id[i],"公路旅客运输量")[1])>0) {
    url<-url_detail[i]
    html<-getURL(url,encoding = 'utf-8')
    web <- read_html(html)
    url_download<-web%>%html_nodes('div.fl.w100.gksqxz_fj ol li a')%>%html_attr('href')
    a1<-str_sub(url_download[1],start = 2)
    a2<-str_sub(url,start = 1,end = 46)
    url_download<-paste(a2,a1,sep = "")
    download(url_download,paste("C:/Users/richard.jin/Desktop/Case/客运/",id[i],".pdf",sep = ""), mode = "wb")
  } else if(ifelse(is.na(str_locate(id[i],"公路货物运输量")[1]),0,str_locate(id[i],"公路货物运输量")[1])>0){
    url<-url_detail[i]
    html<-getURL(url,encoding = 'utf-8')
    web <- read_html(html)
    url_download<-web%>%html_nodes('div.fl.w100.gksqxz_fj ol li a')%>%html_attr('href')
    a1<-str_sub(url_download[1],start = 2)
    a2<-str_sub(url,start = 1,end = 46)
    url_download<-paste(a2,a1,sep = "")
    download(url_download,paste("C:/Users/richard.jin/Desktop/Case/货运/",id[i],".pdf",sep = ""), mode = "wb")
  }   
}
dir.create("C:/Users/richard.jin/Desktop/Case") #新建文件夹
dir.create("C:/Users/richard.jin/Desktop/Case/客运") #新建文件夹
dir.create("C:/Users/richard.jin/Desktop/Case/货运") #新建文件夹
download("http://xxgk.mot.gov.cn/2020/jigou/zhghs/202102/P020210226552808417837.pdf",paste("C:/Users/richard.jin/Desktop/Case/picture",i,".pdf",sep = ""), mode = "wb")
