rm(list = ls())
library(rvest)
library(magrittr)
library("RCurl")

library(downloader)
url0 <- 'https://www.autohome.com.cn/suv/1_1-0.0_0.0-0-0-0-0-0-0-0-0/'
html<-getURL(url0,.encoding ="gb2312")
web <- read_html(html)

Name_qczj<-web%>%html_nodes('ul.rank-list-ul h4 a')%>%html_text
Price_qczj<-web%>%html_nodes('ul.rank-list-ul div a.red')%>%html_text
Link_qczj<-web%>%html_nodes('ul.rank-list-ul h4 a')%>%html_attr('href')
ID_qczj<-web%>%html_nodes('ul.rank-list-ul li')%>%html_attr('id')
#学习这里的节点使用
Link0_qczj<-web%>%html_nodes('ul.rank-list-ul div:nth-child(3) a:nth-child(1)') %>%html_attr('href')
Link1_qczj<-web%>%html_nodes('ul.rank-list-ul div a:nth-child(2)') %>%html_attr('href')
Link2_qczj<-web%>%html_nodes('ul.rank-list-ul div a:nth-child(3)') %>%html_attr('href')
Link3_qczj<-web%>%html_nodes('ul.rank-list-ul div a:nth-child(4)') %>%html_attr('href')
Link4_qczj<-web%>%html_nodes('ul.rank-list-ul div a:nth-child(5)') %>%html_attr('href')

Link3_qczj[468]=Link3_qczj[467]


df_qczj<-data.frame(ID_qczj,Name_qczj,Price_qczj,Link_qczj,Link0_qczj,Link1_qczj,Link2_qczj,Link3_qczj,Link4_qczj)