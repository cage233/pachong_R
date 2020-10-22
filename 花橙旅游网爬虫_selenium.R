rm(list = ls())
library(rvest)
library(dplyr)
library(RSelenium)

hc<-function(){
remDr <- remoteDriver(browserName = "chrome") 
remDr$open()
remDr$navigate("https://www.smartoct.com/comment/list")
comment<-NULL
html<-remDr$getPageSource()[[1]]
web <- read_html(html)
number_of_page<-web%>%html_nodes('div.hc_page a:nth-child(7)')%>%html_text%>%as.numeric(.)



for (i in 1:number_of_page) {
  print(paste("page",i,sep = " "))
  html<-remDr$getPageSource()[[1]]
  web <- read_html(html)
  comment0<-web%>%html_nodes('div.comment p')%>%html_attr('title')
  comment<-append(comment,comment0)
  next_page_button<-remDr$findElements(using = "css", value = "div.hc_page a")
  next_page_button<-next_page_button[[length(next_page_button)]]
  next_page_button$clickElement()
  Sys.sleep(0.5)
}

remDr$quit()
comment

}

hc()






