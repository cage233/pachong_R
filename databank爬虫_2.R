rm(list = ls())

library(websocket)
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

#获得json文件
getmessage<-function(url,mycookie){
  headers = list(
    'accept'= 'zh-CN,zh;q=0.9,en;q=0.8,en-US;q=0.7',
    'accept-encoding'= 'gzip, deflate, br',
    'accept-language'= 'zh-CN,zh;q=0.9',
    'cache-control'= 'max-age=0',
    'referer'= 'https://databank.tmall.com/',
    'user-agent'= 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.102 Safari/537.36',
    'cookie'= mycookie
  )
  html<-getURL(url,httpheader =headers,encoding = 'utf-8')
  html
}

get_outputlist1<-function(mycookie,mapping){
  
  last_day<-format(Sys.time()-86400, "%Y%m%d")[1]
  last_day7<-format(Sys.time()-7*86400, "%Y%m%d")[1]
#爬取只有一个参数的部分
  url1<-paste("https://databank.tmall.com/api/ecapi?path=/asset/cate/volumes&thedate=",last_day,sep = "")
  url2<-paste("https://databank.tmall.com/api/ecapi?path=/quick/v1/asset/cateCustomerList&date=",last_day,sep = "")
  url3<-paste("https://databank.tmall.com/api/ecapi?path=/quick/v1/asset/customerOverview&date=",last_day,sep = "")
  url4<-paste("https://databank.tmall.com/api/ecapi?path=/quick/v1/asset/customerTrend&date=",last_day,sep = "")
  url5<-paste("https://databank.tmall.com/api/ecapi?path=/quick/v1/asset/customerPropertyTrend&date=",last_day,"&kType=W",sep = "")
  url6<-paste("https://databank.tmall.com/api/ecapi?path=/quick/v1/asset/customerPropertyTrend&date=",last_day,"&kType=M",sep = "")
  url7<-paste("https://databank.tmall.com/api/ecapi?thedate=",last_day,"&path=/datatable/fulllink/",sep = "")
  url8<-paste("https://databank.tmall.com/api/ecapi?path=/databank/crowdFullLink/detail&beginTheDate=",last_day7,"&endTheDate=",last_day,sep = "")
  
  c_url<-c(url1,url2,url3,url4,url5,url6,url7,url8)
  outputlist1<-list()
  
  
  for (i in 1:length(c_url)) {
    url<-c_url[i]
    output<-getmessage(url,mycookie)%>%json_to_csv(.)
    names_old<-names(output)%>%data.frame(id=.)
    names_new<-join(names_old,mapping, by = "id",type = "left", match= "all")
    names(output)<-names_new$name
    outputlist1[[i]]<-output
  }
  
  cat_id<-outputlist1[[1]]$类别ID
  cat_name<-outputlist1[[1]]$类别名称
  
#爬取还有类别ID参数的部分
  output0<-data.frame(NULL)
  for (i in 1:length(cat_id)) {
    url<-paste("https://databank.tmall.com/api/ecapi?path=/asset/cate/overlap&thedate=",last_day,"&cateId=",cat_id[i],sep = "")
    output<-getmessage(url,mycookie)%>%json_to_csv(.)
    names_old<-names(output)%>%data.frame(id=.)
    names_new<-join(names_old,mapping, by = "id",type = "left", match= "all")
    names(output)<-names_new$name
    output$类别名称<-cat_name[i]
    output0<-rbind(output0,output)
  }
  
  output1<-data.frame(NULL)
  for (i in 1:length(cat_id)) {
    url<-paste("https://databank.tmall.com/api/ecapi?path=/quick/v1/asset/customerOverviewByCate&date=",last_day,"&cateId=",cat_id[i],sep = "")
    output<-getmessage(url,mycookie)%>%json_to_csv(.)
    names_old<-names(output)%>%data.frame(id=.)
    names_new<-join(names_old,mapping, by = "id",type = "left", match= "all")
    names(output)<-names_new$name
    output$类别名称<-cat_name[i]
    output1<-rbind(output1,output)
  }
  
  cat_id<-c("1010","1020","1030","1040")
  cat_name<-c("认知","兴趣","购买","忠诚")
  
  output2<-data.frame(NULL)
  for (i in 1:length(cat_id)) {
    url<-paste("https://databank.tmall.com/api/ecapi?path=/datatable/fulllink/trend&thedate=",last_day,"&statusId=",cat_id[i],sep = "")
    output<-getmessage(url,mycookie)%>%json_to_csv(.)
    names_old<-names(output)%>%data.frame(id=.)
    names_new<-join(names_old,mapping, by = "id",type = "left", match= "all")
    names(output)<-names_new$name
    output$人群名称<-cat_name[i]
    output2<-rbind(output2,output)
  }
  
  outputlist1[[9]]<-output0
  outputlist1[[10]]<-output1
  outputlist1[[11]]<-output2
  outputlist1
  
}

mycookie='enc=ACfTQkY3Vm27pcpfsSFx75Uom8oMlmBPs%2BgWc86TAgFG0AztxFibYDTqZDaDNTjUc5gnVL3TECOD%2BKDgqO3WFA%3D%3D; hng=CN%7Czh-CN%7CCNY%7C156; cna=JR+9FwCUzA0CAdINeAVKojS+; lid=%E6%88%B4%E5%B0%94%E5%AE%98%E6%96%B9%E6%97%97%E8%88%B0%E5%BA%97%3Amc; _tb_token_=c4e2082f-c279-4314-8ee7-74a29fcc3663; xlly_s=1; t=0ee0dc0e79adcfcfcb278ca7f2ad670e; unb=3435245864; sn=%E6%88%B4%E5%B0%94%E5%AE%98%E6%96%B9%E6%97%97%E8%88%B0%E5%BA%97%3Amc; _tb_token_=54e1333b8ee13; cookie2=1b8d953b6a0f3e24138fcc63e5184b51; db_base=bd76882d2b2c87ff30f9792b451ff4c4; welcomeShownTime=1600740151205; db_smart=fc49306d97602c8ed1be1dfbf0835ead; __YSF_SESSION__={"baseId":"2b2c87ff30f9792b","brandId":"b443f40be3100db0","departmentId":"4a76686f9b2418c2","smartId":"3a49fa751294b20f","databankProjectId":"835321183d1cd3fd"}; uc1=cookie14=Uoe0bU9CCrE9SQ%3D%3D&cookie21=UtASsssmfufd; csg=6c089bbd; tfstk=cM8ABbXUVYD0nG1JTnnl1yt5517AZkDOFS6gWXFXKid3fOzOiCYHJ7Lrc1wAybC..; l=eBQGR8RqqbWGjtefKOfanurza77OSIRYouPzaNbMiOCPOyCk5RRfWZr1S2LDC3GVhsOpR3uV-FgLBeYBqIjJim_U8oW-GjMmn; isg=BLKy4tqgOaTKBQfOTRtS9tudA_iUQ7bdp8p8GnyL3mVQD1IJZNMG7bhp_6uzeS51'
mapping <- read_excel("C:/Users/richard.jin/Desktop/mapping.xlsx")
outputlist1<-get_outputlist1(mycookie,mapping)


