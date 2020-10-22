rm(list = ls())
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


getmessage<-function(url,mycookie){
  headers = list(
    'Accept'= 'application/json, text/plain, */*',
    'Accept-Encoding'= 'gzip, deflate',
    'Accept-Language'= 'zh-CN,zh;q=0.9,en;q=0.8,en-US;q=0.7',
    'Connection'= 'keep-alive',
    'Host'= 'zhishu.baidu.com',
    'Referer'= 'http://zhishu.baidu.com/v2/main/index.html',
    'User-Agent'='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36',
    'cookie'= mycookie
  )
  html<-getURL(url,httpheader =headers,encoding = 'utf-8')
  output<-json_to_csv(html)
  output
}

main<-function(keyword,date,mycookie){
  keyword_utf8<-URLencode(iconv(keyword,to = 'utf8',toRaw = F))
  url <- paste('http://zhishu.baidu.com/api/WordGraph/multi?wordlist%5B%5D=',keyword_utf8,'&datelist=',date,sep="")
  output<-getmessage(url,mycookie)[,4:7]
  names(output)<-c('相关词','热度','热度同比','相关程度')
  output
}


keyword<-'青岛'
#这是展现过去一周的指数，填写你所需要的周的最后一天，格式要求yyyymmdd
date <- '20201011'
mycookie <- 'BIDUPSID=189F301154164091CCD283D5721ECFA9; PSTM=1564126872; BDUSS=UtSWRPN0dHSEpRdHF-Q2VZSzlaMXdKTk01MmhSOU1EOExDRTYtQTJ2aktrTE5lSVFBQUFBJCQAAAAAAAAAAAEAAABa7EKQc2FraeKAhmwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMoDjF7KA4xeU0; BAIDUID=5BFC2F1242A611A80F25CED5C571D49C:FG=1; H_WISE_SIDS=145302_144157_142019_144883_140631_145496_141748_144117_144989_144135_145271_136863_131246_144681_137746_144741_138883_141941_127969_140065_144790_140593_144249_143491_144607_131423_100806_142206_145520_145353_139909_144872_139884_143477_144966_140312_145422_144535_143472_143856_145076_139914_110085; BDORZ=B490B5EBF6F3CD402E515D22BCDA1598; bdindexid=6i0o7tcma41ojmr7je58resfd4; __yjsv5_shitong=1.0_7_26f112fe67a11640f85036bad644c6da16a6_300_1602585841973_210.13.120.5_da44b016; BA_HECTOR=aha084800h0g2h62dh1focmoc0j; Hm_lvt_d101ea4d2a5c67dab98251f0b5de24dc=1602640758; RT="z=1&dm=baidu.com&si=wroky7qyy1&ss=kg7sp3b8&sl=1f&tt=1l2r&bcn=https%3A%2F%2Ffclog.baidu.com%2Flog%2Fweirwood%3Ftype%3Dperf"; Hm_lpvt_d101ea4d2a5c67dab98251f0b5de24dc=1602641074'
output<-main(keyword,date,mycookie)
