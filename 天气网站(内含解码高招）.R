#--------------------加载包--------------------
rm(list = ls())
library(RCurl)
library(stringr)
library(httr)
library(jsonlite)
library(dplyr)
library(readxl)

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


url<-"http://hz.zc12369.com/api/CityStatComp/getAreaListByType?type=1"

headers = list(
  'Accept'= 'application/json, text/plain, */*',
  'Accept-Encoding'= 'gzip, deflate',
  'Accept-Language'= 'zh-CN,zh;q=0.9,en;q=0.8,en-US;q=0.7',
  'Auth'= '38e8349a-081d-47f1-8b8d-e9d08f385dc0',
  'Connection'= 'keep-alive',
  'Host'= 'hz.zc12369.com',
  'Referer'= 'http://hz.zc12369.com/home/meteorologicalData/cityContrast/',
  'User-Agent'='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36',
  'cookie'= 'Hm_lvt_af64eb0767b6236e3ce7683cc35df3e7=1615276818,1615276854; Hm_lpvt_af64eb0767b6236e3ce7683cc35df3e7=1615277062'
)
html<-getURL(url,httpheader =headers,.encoding = 'utf-8', encoding = 'utf-8')

