#java -jar selenium-server-standalone-3.141.59.jar
rm(list = ls())
library(rvest)
library(dplyr)
library(RSelenium)


#利用profile登录知乎
cprof <- getChromeProfile("C:/Users/richard.jin/AppData/Local/Google/Chrome/User Data",profileDir = "Profile 1")
remDr <- remoteDriver(browserName = "chrome", extraCapabilities = cprof)
remDr$open()
remDr$navigate("https://www.zhihu.com")

#下拉滚动条
webElem <- remDr$findElement("css", "body")
for (i in 1:100) {
  webElem$sendKeysToElement(list(key = 'page_down'))
}

#抓取数据
html<-remDr$getPageSource()[[1]]
web <- read_html(html)
number_of_page<-web%>%html_nodes('h2.ContentItem-title a')%>%html_text
