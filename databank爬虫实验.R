rm(list = ls())

library(websocket)
library(RCurl)
library(stringr)
library(httr)
library(jsonlite)

setws<-function(mycookie){
ws <- WebSocket$new("wss://ws-insight-engine.tmall.com/", 
                    headers = list("Accept-Encoding"= 'gzip, deflate, br',
                                   "Accept-Language"= 'zh-CN,zh;q=0.9,en;q=0.8,en-US;q=0.7',
                                   "Cache-Control"= 'no-cache',
                                   "Connection"= 'Upgrade',
                                   "Host"= 'ws-insight-engine.tmall.com',
                                   "Origin"= 'https://insight-engine.tmall.com',
                                   "Pragma"= 'no-cache',
                                   "Sec-WebSocket-Extensions"= 'permessage-deflate; client_max_window_bits',
                                   "Sec-WebSocket-Key"= 'URP1oCd/HslTFjRTxkI2TA==',
                                   "Sec-WebSocket-Version"= '13',
                                   "Upgrade"= 'websocket',
                                   "User-Agent"= 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.102 Safari/537.36',
                                   "Cookie" = mycookie
                                   ),
                    autoConnect = FALSE)
ws$onOpen(function(event) {
  cat("Connection opened\n")
})
ws$onMessage(function(event) {
  # cat("Client got msg: ", event$data, "\n")
  data0 <-as.data.frame(event$data)
  names(data0)="x"
  data <- read.csv("C:/Users/richard.jin/Desktop/data.csv", sep="")
  data<-rbind(data,data0)
  write.table(data,file = "C:/Users/richard.jin/Desktop/data.csv",row.names = F,quote = F)
})
ws$onClose(function(event) {
  # cat("Client disconnected with code ", event$code,
  #     " and reason ", event$reason, "\n", sep = "")
  cat("Connection closed")
})
ws$onError(function(event) {
  cat("Client failed to connect: ", event$message, "\n")
})
ws
}


setmessage<-function(mycondition,mycrowdID,mytags){
  send<-paste('{"method": "/iSheetCrowdService/report",
              "headers": {"rid": "159980557410603", "type": "PULL"},
              "body": {"args": {"id": "287", 
              "condition": ',mycondition,', 
              "tags": [',mytags,'],
              "bizParam": {"databankCrowdId": "',mycrowdID,'", 
              "bizType": "CUSTOM_ANALYSIS", 
              "tag_identifier": "all",
              "captcha": "%7B%22a%22%3A%22TSF2%22%2C%22c%22%3A%221582700342424%3A0.23973179491554664%22%2C%22d%22%3A%22nvc_register%22%2C%22h%22%3A%7B%22key1%22%3A%22code0%22%2C%22nvcCode%22%3A400%2C%22umidToken%22%3A%22T08C67EE3AD81E11A23D01F0EE4CA1134D6022447F84D66B6623678D5FE%22%7D%2C%22j%22%3A%7B%22test%22%3A1%7D%2C%22b%22%3A%22122%23i5kRCD9eEE%2BqAJpZy4pjEJponDJE7SNEEP7ZpJRBuDPpJFQLpCGwoHZDpJEL7SwBEyGZpJLlu4Ep%2BFQLpoGUEELWn4yE7SNEEP7ZpERBuDPE%2BBQPpC76EJponDJLKMQEspPA04nTtBOmKBvALOESIAOsJhoR8HJNZIbVrA4tlm9BE5R8XAJh3Ue7sqj97bm98oL6JXWpO%2F%2BDqMfEl8WxXWplul5EELXZ8CL6JNbERF3mqM3okBTlpM%2B1ul9rDLVZ8CRfJ4bEyF3mqW32E5pangLlul5EELXZ8oL6JNEEyB3DqMfbDEpxnSp1tP0EDLf%2F8om6J4EqRoBHqevfAIAKkS7gz4MzTChsGRkP%2BeqxYlg3ypGmtolPBVsN6ovcGQYuNVCPWaTtjBbU5X0QWvFMSFzE%2FaVEWBKYt4%2FavytijGp1%2FQCeg%2F2SfxjZw8fIlfio42QXEpWhdEspy4fBSEZgHrMRhwhnBIFLuxd8hTc7Y99x%2BfF2UuU5oOXLG0an0CHvFQqCYMqZXb8myhlZHuHOXI0GZ%2BPvFxGzVjTf4AmL63HeSjbSP8L6CSCuq47zX7tPDBWS%2FhdLqwO8dtOkEXes248plPRkVf5gVlcqsHSdYxDkjT%2FpfSszn8vXlIUg3GRKm9eMWi09PxekLM2tUu0nivXzonmbHdGHLsqhNe%2FbdjoKHYG6ygnnx3aZ9DU9ugUWmZgB9Ztbv1BYo%2FhPlYLykr4j14BCvVlwMUtgZbK%2BpgNu7vKERSRkRosaHoNotkt%2B%2BToClNeIRM%2Fk7vQm1x0YbZT3hzfU9k5kadIafvosIReZwiQhd4%2B0sYXOjxvChWtv2%2FSMKb9fIeSKsALCP%2FNshczJBF5y1TMo4YIPh7%2BaMuMfcUqmMC%2BWrr1Xm%2FUAjtyHnttlwQlkGDRPsSS4DMlM0OdWYLD9vL0ekEm7iz566ESLHP2aykbJ%2F3id3DwjcdgDENA6%2F8oojlvm6WP0JBDUviVTeDPK9V5RFekM3drtuDFit2UwRc%2B09xUvKcueMc%2FPrKKRhC%2FuRLXdx0WzgP4%2F2RJJeZhuKEQhYDTsFiDZnArDQMQyiMN0hRuwdfalhZFe27jXUG4Y%2BwPnHvpj1OXxEJ1VOgEtBzOO1AgwUr5SYa6UKhrbynm2X1J1HRyEn%2FVUqkvgc9Rx8ZYG2GqRr4L7eQ1N4sa7S6oqKEND6fX3eQhxFaqnimVCIg%2B6TwTp9Ant4P15WlGdVueq6HWATk8zB1CglVmsVH08lUwArNKjBpArD7v%2BIxx5VP1hJLChZOgfHqa6MVh7fP0lg6HXLeyMLSOXCz3oAK7iSOFkfu6RAx%2BGrBAj50ha%22%2C%22e%22%3A%22HELtCcggjijySBif5QK5Flm60fyrLzjVSvY2GZ7kF9k2ufvery5t6e1OxIGoUHpc7a0IbkE_0FA-F5WgiEpV7aeWlyQyxr1LL83v6PCoc3YbWdFNpRGiSow97HJFmhSolqL2iP8Yg3b6GvpNCl1IVN3_kiy7mdt7qA7PsE2Fu9J1ZID-lo1BWsvQpV6riLNbYizM9JlKkpiqJYbEB2zQGA%22%7D"}, 
              "insightType": 0, 
              "interaction": "false", 
              "rateParam": {}, 
              "appId": "208"}}}',sep = "")
  send
  }

#获得token
gettoken<-function(mycookie){
  url<-'https://databank.tmall.com/'
  headers = list(
    'accept'= 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3',
    'accept-encoding'= 'gzip, deflate, br',
    'accept-language'= 'zh-CN,zh;q=0.9',
    'cache-control'= 'max-age=0',
    'referer'= 'https://databank.tmall.com/',
    'user-agent'= 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.102 Safari/537.36',
    'cookie'= mycookie
  )
  html<-getURL(url,httpheader =headers,encoding = 'utf-8')
  #这里用到了技巧（?<=),零宽度断言前后预查
  token<-str_extract(html,pattern = '(?<=token=")(.*?)(?=")')
  token
}
#获得condition
getcondition<-function(mytoken,mycookie,mycrowdID){

  uri = 'https://databank.tmall.com/api/ecapi'  
  headers = c(
    'accept'= '*/*',
    'accept-encoding'= 'gzip, deflate, br',
    'accept-language'= 'zh-CN,zh;q=0.9,en;q=0.8,en-US;q=0.7',
    'content-length'= '210',
    'origin'= 'https://databank.tmall.com',
    'referer'= 'https://databank.tmall.com/',
    'x-csrf-token'= mytoken,
    'x-custom-router'= 'databank-CrowdPortrait',
    'x-requested-with'= 'XMLHttpRequest',
    'content-type'="application/json",
    'cookie'= mycookie
  )
  last_day<-format(Sys.time()-86400, "%Y-%m-%d")[1]
  mypers<-paste('{"bizType":"CUSTOM_ANALYSIS","paramsMap":{"DATE":"',last_day,' 00:00:00","CROWD_ID":"',mycrowdID,'"}}',sep = "")
  params<-list(
  'contentType'= 'application/json',
  'path'= '/v1/crowd/perspective/build/param',
  'perspectiveParamStr'= mypers
  )
  
  html<-POST(uri,add_headers(.headers = headers),body = params,encode = "json")
  mycondition<-content(html)$data
  mycondition
}

getdata<-function(mytaglist){
  df<-c("category_id" = NULL,"category_name" = NULL,"tagValue" = NULL,"tagValueName" = NULL, "count" = NULL,"Rate" = NULL, 
        "totalCount" = NULL, "sampleRate" = NULL)
  data_all <- read.csv("C:/Users/richard.jin/Desktop/data.csv", encoding ="UTF-8", sep="")[,1]%>%as.character(.)
  
  for (n in 2:length(data_all)) {
    data<-data_all[n]%>%fromJSON(.)%>%.$body
    for (i in 1:length(data)) {
      data_s<-data[[i]]
      df0<- data_s$perspectiveItems
      df0$category_id<-data_s$tagId
      df0$category_name<-data_s$tagName
      df0$totalCount<-data_s$totalCount
      df0$sampleRate<-data_s$sampleRate
      df<- rbind(df,df0)
    }
    df_output<-data.frame(df$category_name,df$tagValueName,df$rate,df$count)
  }
  
  
df_output
data0<-merge(df_output,mytaglist,by.x = "df.category_name",by.y = "df.tagId")
data0<-data.frame(data0$df.tagCateName,data0$df.tagTitle,data0$df.tagValueName,data0$df.rate,data0$df.count)
names(data0)<-c("Category","Subcategory","Tag","Pecentage","Count")
data0

}


gettaglist<-function(mycookie){
headers = list(
  'accept'= 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3',
  'accept-encoding'= 'gzip, deflate, br',
  'accept-language'= 'zh-CN,zh;q=0.9',
  'cache-control'= 'max-age=0',
  'referer'= 'https://databank.tmall.com/',
  'user-agent'= 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.102 Safari/537.36',
  'cookie'= mycookie
)
df<-data.frame("tagId"=NULL,"tagTitle"=NULL,"isMultiValue"=NULL,"tagCateId"=NULL,"sort"=NULL,"selected"=NULL,"tagCateId"=NULL,"tagCateName"=NULL)

for (n in 1:4) {
  url<-paste('https://databank.tmall.com/api/ecapi?tagTypeId=',n,'&path=/v1/tag/list',sep="")
  html<-getURL(url,httpheader =headers,encoding = 'utf-8')
  list<-fromJSON(html)$data
  for (i in 1:nrow(list)) {
    df0<-list$tagPropList[i]%>%as.data.frame(.)
    df0$tagCateId<-list$tagCateId[i]
    df0$tagCateName<-list$tagCateName[i]
    df<-rbind(df,df0)
  }
 
}
df<-data.frame(df$tagId,df$tagTitle,df$tagCateName)
c<-1:nrow(df)
a<-round((c-1) %/%2,0)+1
amax<-a[length(a)]
df$index<-a
df

}

cleandatabase<-function(){
  df<-data.frame("x"="Cleaned")
  write.table(df,file = "C:/Users/richard.jin/Desktop/data.csv",row.names = F,quote = F)
}

main<-function(mycookie,mycrowdID){
  mytoken<-gettoken(mycookie)
  cat("token:",mytoken, "\n",sep="" )
  mycondition<-getcondition(mytoken,mycookie,mycrowdID)
  cat("condition:",mycondition, "\n",sep="" )
  mytaglist<-gettaglist(mycookie)
  
  ws<-setws(mycookie)
  ws$connect()
  Sys.sleep(3)
  cat("Connection open\n")
  cleandatabase()
  cat("Data cleaned\n")
  
  for (i in 1:mytaglist[nrow(mytaglist),4]) {
    text<-round(i/mytaglist[nrow(mytaglist),4]*100,0)
    cat(text,"%\n",sep="")
    mytags<-mytaglist$df.tagId[mytaglist$index==i]%>% paste('"',.,'"',sep = "")%>%paste0(.,collapse=',')
    message<-setmessage(mycondition,mycrowdID,mytags)
    ws$send(message)
    Sys.sleep(3)
  }
  ws$close()
}

mycookie<-'enc=ACfTQkY3Vm27pcpfsSFx75Uom8oMlmBPs%2BgWc86TAgFG0AztxFibYDTqZDaDNTjUc5gnVL3TECOD%2BKDgqO3WFA%3D%3D; hng=CN%7Czh-CN%7CCNY%7C156; cna=JR+9FwCUzA0CAdINeAVKojS+; lid=%E6%88%B4%E5%B0%94%E5%AE%98%E6%96%B9%E6%97%97%E8%88%B0%E5%BA%97%3Amc; db_base=bd76882d2b2c87ff30f9792b451ff4c4; db_smart=fc49306d97602c8ed1be1dfbf0835ead; _tb_token_=98bf919d-6190-4c5f-b3a2-fb194f9d93c1; t=0ee0dc0e79adcfcfcb278ca7f2ad670e; unb=3435245864; sn=%E6%88%B4%E5%B0%94%E5%AE%98%E6%96%B9%E6%97%97%E8%88%B0%E5%BA%97%3Amc; _tb_token_=3e9638eee7676; cookie2=1530293e25fe06af080ec49cde648edf; __YSF_SESSION__={"baseId":"2b2c87ff30f9792b","brandId":"b443f40be3100db0","departmentId":"4a76686f9b2418c2","smartId":"3a49fa751294b20f","databankProjectId":"835321183d1cd3fd"}; xlly_s=1; uc1=cookie21=WqG3DMC9Eman&cookie14=Uoe0bUt%2F%2B1l98w%3D%3D; csg=0d2d05d5; welcomeShownTime=1600322467655; tfstk=cB9OB0bzO20MIGsp4Chhcj0W5NsOZIhOo2bzH0FrtnBasZeAiMpkekpUKgUOpDC..; l=eBQGR8RqqbWGjYRAKOfwourza77OSIRAguPzaNbMiOCP9bXB5ueNWZrVCWx6C3GVhsOpR3uV-FgLBeYBqnfJim_U8oW-GjMmn; isg=BPLyOJYM-W-jNccODdsSNptdQzjUg_YdZwo8WrzLHqWQT5JJpBNGLfipP-tzP261'
mycrowdID<-'51694484'
main(mycookie,mycrowdID)
mytaglist<-gettaglist(mycookie)
output<-getdata(mytaglist)
write.table(output,file = "C:/Users/richard.jin/Desktop/dataoutput.csv",sep = ",",row.names = F,quote = F)




