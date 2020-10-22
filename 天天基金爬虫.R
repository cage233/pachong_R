rm(list=ls())
library("RCurl")
library("XML")
library("readr")
library("dplyr")
library("tidyr")
detail_of_funds <- read_csv("C:/Users/Richard/Desktop/detail of funds.csv", 
                             locale = locale(encoding = "GB2312"))
strrd<-function(txt,sep,num){
  if (missing(num)){
    htm1<-strsplit(txt,sep)
    htm2<-unlist(htm1)}
  else
  {htm1<-strsplit(txt,sep)
  htm2<-unlist(htm1)
  htm2[num]}
}
user_agent<-c("Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/22.0.1207.1 Safari/537.1",
              "Mozilla/5.0 (X11; CrOS i686 2268.111.0) AppleWebKit/536.11 (KHTML, like Gecko) Chrome/20.0.1132.57 Safari/536.11",
              "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.6 (KHTML, like Gecko) Chrome/20.0.1092.0 Safari/536.6",
              "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.6 (KHTML, like Gecko) Chrome/20.0.1090.0 Safari/536.6",
              "Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/19.77.34.5 Safari/537.1",
              "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/536.5 (KHTML, like Gecko) Chrome/19.0.1084.9 Safari/536.5",
              "Mozilla/5.0 (Windows NT 6.0) AppleWebKit/536.5 (KHTML, like Gecko) Chrome/19.0.1084.36 Safari/536.5",
              "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1063.0 Safari/536.3",
              "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1063.0 Safari/536.3",
              "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_0) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1063.0 Safari/536.3",
              "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1062.0 Safari/536.3",
              "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1062.0 Safari/536.3",
              "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1061.1 Safari/536.3",
              "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1061.1 Safari/536.3",
              "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1061.1 Safari/536.3",
              "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1061.0 Safari/536.3",
              "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.24 (KHTML, like Gecko) Chrome/19.0.1055.1 Safari/535.24",
              "Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/535.24 (KHTML, like Gecko) Chrome/19.0.1055.1 Safari/535.24",
              "Mozilla/5.0 (Macintosh; U; Mac OS X Mach-O; en-US; rv:2.0a) Gecko/20040614 Firefox/3.0.0 ",
              "Mozilla/5.0 (Macintosh; U; PPC Mac OS X 10.5; en-US; rv:1.9.0.3) Gecko/2008092414 Firefox/3.0.3",
              "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.5; en-US; rv:1.9.1) Gecko/20090624 Firefox/3.5",
              "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; en-US; rv:1.9.2.14) Gecko/20110218 AlexaToolbar/alxf-2.0 Firefox/3.6.14",
              "Mozilla/5.0 (Macintosh; U; PPC Mac OS X 10.5; en-US; rv:1.9.2.15) Gecko/20110303 Firefox/3.6.15",
              "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.6; rv:2.0.1) Gecko/20100101 Firefox/4.0.1",
              "Opera/9.80 (Windows NT 6.1; U; en) Presto/2.8.131 Version/11.11",
              "Opera/9.80 (Android 2.3.4; Linux; Opera mobi/adr-1107051709; U; zh-cn) Presto/2.8.149 Version/11.10",
              "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US) AppleWebKit/531.21.8 (KHTML, like Gecko) Version/4.0.4 Safari/531.21.10",
              "Mozilla/5.0 (Windows; U; Windows NT 5.2; en-US) AppleWebKit/533.17.8 (KHTML, like Gecko) Version/5.0.1 Safari/533.17.8",
              "Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/533.19.4 (KHTML, like Gecko) Version/5.0.2 Safari/533.18.5",
              "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0",
              "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0; Trident/4.0)",
              "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0)",
              "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)")

myHttpheader<- c(
  "User-Agent"=sample(user_agent,1),
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
)

fund_out=data.frame(code=0,time=0,net_value=0,equityReturn=0,unitMoney=0)
fund_out=fund_out[-1,]
fund_s<-subset.data.frame(detail_of_funds,category == "股票指数")

for (i in fund_s$code){
#随机的header  

#define url
url<-paste("http://fund.eastmoney.com/pingzhongdata/001548.js",sep="")
#find the useful part
html<-getURL(url,httpheader = myHttpheader,.encoding ="utf-8",proxy="190.107.30.115:80")
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
fund_out<-rbind.data.frame(html10,fund_out)
}
rm(html10,html9)
fund_out_byday<-spread(fund_out[1:3],time,net_value,fill = NA)
fund_out_byday1<-fund_out_byday[,-2:-3882]
fund_out_byday1<-fund_out_byday1[,-101]
fund_out_byday1<-na.omit(fund_out_byday1)
write.table(fund_out_byday1,file = "C:/Users/Richard/Desktop/test.csv",sep=",",row.names = F,quote = F)

