#java -jar selenium-server-standalone-3.141.59.jar
#java -Dwebdriver.chrome.driver=:chromedriver.exe  -java -jar selenium-server-standalone-3.141.59.jar
#设计了一个函数，只需要输入城市和关键词，就可以通过百度地图API得到对应关键词的地理位置信息
rm(list = ls())
library(rvest)
library(dplyr)
library(RSelenium)
library(stringr)
library(readr)


baidumap<-function(cityname,keyword){
remDr <- remoteDriver(browserName = "chrome") 
remDr$open()
remDr$navigate("https://api.map.baidu.com/lbsapi/getpoint/index.html")

#点击更换城市
remDr$findElements(using = "css", value = "div.l span a#curCityText")[[1]]$clickElement()
Sys.sleep(0.5)
#选择城市
remDr$findElements(using = "css", value = paste("[name = ",cityname,"]"))[[1]]$clickElement()
Sys.sleep(0.5)
#输入关键词并查询
remDr$findElements(using = "css", value = "input#localvalue.text")[[1]]$sendKeysToElement(list(keyword, "\uE007"))
Sys.sleep(2)


name<-NULL
detail<-NULL
i=1
repeat{
  cat("page ",i, "\n",sep="" )
  i=i+1
  html<-remDr$getPageSource()[[1]]
  web <- read_html(html)
  #获取数据
  name0<-web%>%html_nodes('ul.local_s li div a')%>%html_text()
  detail0<-web%>%html_nodes('ul.local_s li div p')%>%html_text()
  name<-append(name,name0)
  detail<-append(detail,detail0)
  #获取下一页位置
  number_of_page<-web%>%html_nodes('p.page span a')%>%html_text
  n<-length(number_of_page)
  #如果只有一页，则停止
  if (n==0) {
    break
  }
  #如果已经到了最后一页则停止
  if (number_of_page[n] != "下一页") {
    break
  }
  #点击下一页
  remDr$findElements(using = "css", value = "p.page span a")[[n]]$clickElement()
  Sys.sleep(1)
}

remDr$close()
#数据拆分
detail1<-str_replace_all(detail," ","")
dz<-str_locate(detail1, "地址：")
dh<-str_locate(detail1, "电话：")
zb<-str_locate(detail1,"坐标：")
df<-NULL

#因为有的有电话，有的没有，所以要调整
for (i in 1:length(detail)) {
  if(is.na(dh[i,1])){
    dz_output0<-str_sub(detail1[i],start = dz[i,2]+1,end = zb[i,1]-1)%>%as.character(.)
    dh_output0<-"-"
  } else {
    dz_output0<-str_sub(detail1[i],start = dz[i,2]+1,end = dh[i,1]-1)%>%as.character(.)
    dh_output0<-str_sub(detail1[i],start = dh[i,2]+1,end = zb[i,1]-1)%>%as.character(.)
  }
  zb_output0<-str_sub(detail1[i],start = zb[i,2]+1)%>%str_split(.,",")
  zb_jd_output<-zb_output0[[1]][1]%>%as.numeric(.)
  zb_wd_output<-zb_output0[[1]][2]%>%as.numeric(.)
  
  df0<-data.frame(dz_output0,dh_output0,zb_jd_output,zb_wd_output)
  df<-rbind(df,df0)
}
cityname0<-rep(cityname,nrow(df))
keyword0<-rep(keyword,nrow(df))
output<-cbind(cityname0,keyword0,name,df)
names(output)<-c("城市","关键词","地点名称","地址","电话","坐标经度","坐标纬度")
output
}

#这里的cityname要注意要和html里button的名字一致，否则可能会出问题
xzl<-baidumap(cityname = "上海市", keyword = "写字楼")
starbucks<-baidumap(cityname = "上海市", keyword = "星巴克")
COSTA<-baidumap(cityname = "上海市", keyword = "COSTA")
sevenEleven<-baidumap(cityname = "上海市", keyword = "711")
familyMart<-baidumap(cityname = "上海市", keyword = "全家")
EGO<-baidumap(cityname = "上海市", keyword = "逸刻")
coffeeElse<-baidumap(cityname = "上海市", keyword = "咖啡")
bgl<-baidumap(cityname = "上海市", keyword = "办公楼")
ds<-baidumap(cityname = "上海市", keyword = "办公楼")

final<-rbind(xzl,starbucks,COSTA,sevenEleven,familyMart,EGO,coffeeElse)
write_excel_csv(bgl,path = "C:/Users/richard.jin/Desktop/baiduadd.csv")



