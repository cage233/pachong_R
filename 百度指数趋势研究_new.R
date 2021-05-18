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


set_url<-function(keyword,start_date,end_date,area="0",index_type=1){
  keyword_utf8<-URLencode(iconv(keyword,to = 'utf8',toRaw = F))
  if (index_type == 1) {
    url = paste('http://zhishu.baidu.com/api/SearchApi/index?area=',area,'&word=[[%7B%22name%22:%22',keyword_utf8,'%22,%22wordType%22:1%7D]]&startDate=',start_date,'&endDate=',end_date,sep="")
  } else if (index_type ==2 ) {
    url = paste('http://zhishu.baidu.com/api/FeedSearchApi/getFeedIndex?area=',area,'&word=[[%7B%22name%22:%22',keyword_utf8,'%22,%22wordType%22:1%7D]]&startDate=',start_date,'&endDate=',end_date,sep="")
  } else {
    url = paste('http://zhishu.baidu.com/api/NewsApi/getNewsIndex?area=',area,'&word=[[%7B%22name%22:%22',keyword_utf8,'%22,%22wordType%22:1%7D]]&startDate=',start_date,'&endDate=',end_date,sep="")
  }
  url
}


decrypt<-function(keys,data){
  half<-str_length(keys) %/% 2
  pre<-str_sub(keys,end = half)
  pro<-str_sub(keys,start = half+1)
  o<-NULL
  for (i in 1:half) {
    o_append<-c(str_sub(pro,i,i))
    names(o_append)<-str_sub(pre,i,i)
    o<-append(o,o_append)
  }
  
  str_output<-NULL
  for(i in 1:str_length(data)){
    data0<-str_sub(data,i,i)
    o_append_to_str<-o[data0]
    str_output<-str_c(str_output,o_append_to_str,sep="")
  }
  output<-str_split(str_output,",")[[1]]%>%as.numeric(.)
  output
}

main<-function(keyword,start_date,end_date,index_type,device_type,mycookie,area="0"){
  url = set_url(keyword,start_date,end_date,area=area,index_type)
  message<-getmessage(url,mycookie)
  
  u_id<-message$data__uniqid[1]
  if(index_type ==1){
    data<-message$`data__userIndexes__|__data`[device_type+1]
  } else{
    data<-message$data__index__data[1]
  }
  
  url2<-paste('http://zhishu.baidu.com/Interface/ptbk?uniqid=',u_id,sep="")
  message2<-getmessage(url2,mycookie)
  keys<-message2$data
  index<-decrypt(keys,data)
  
  #
  start_ymd<-as.Date(start_date)
  end_ymd<-as.Date(end_date)
  mapping<-c("星期一"=0,"星期二"=1,"星期三"=2,"星期四"=3,"星期五"=4,"星期六"=5,"星期日"=6)
  if(end_ymd-start_ymd>365){
    wkday<-weekdays(start_ymd)
    wkday_number_start<-mapping[wkday]
    wkday<-weekdays(end_ymd)
    wkday_number_end<-mapping[wkday]
    start_ymd<-start_ymd-wkday_number_start
    end_ymd<-end_ymd-wkday_number_end
    time1<-seq(from=start_ymd,to=end_ymd,by=7)%>%as.Date(.,origin="1970-01-01")
    time2<-seq(from=start_ymd+6,to=end_ymd+6,by=7)%>%as.Date(.,origin="1970-01-01")
    time<-paste(time1,"~",time2,sep=" ")
  } else{
    time<-c(start_ymd:end_ymd)%>%as.Date(.,origin="1970-01-01")
  }

  output<-data.frame(time,index)
  output
}




#-------------------- 省份编号索引表: CODE2PROVINCE, PROVINCE2CODE--------------------
##   - CODE2PROVINCE来源: main-vendor.xxxxxxxxxxxxxxxxxxx.js源码中直接复制可得
##   - js源码URL: e.g. http://index.baidu.com/v2/static/js/main-vendor.dbf1ed721cfef2e2755d.js
CODE2PROVINCE<-c("山东","贵州","江西","重庆","内蒙古","湖北","辽宁","湖南","福建",
                 "上海","北京","广西","广东","四川","云南","江苏","浙江","青海",
                 "宁夏","河北","黑龙江","吉林","天津","陕西","甘肃","新疆","河南",
                 "安徽","山西","海南","台湾","西藏","香港","澳门")
PROVINCE2CODE<-c("901","902","903","904","905","906","907","908","909","910",
                 "911","912","913","914","915","916","917","918","919","920",
                 "921","922","923","924","925","926","927","928","929","930",
                 "931","932","933","934")
names(CODE2PROVINCE)<-PROVINCE2CODE
names(PROVINCE2CODE)<-CODE2PROVINCE

#--------------------城市编号索引表: CODE2CITY, CITY2CODE--------------------
##   - CODE2PROVINCE来源: main-vendor.xxxxxxxxxxxxxxxxxxx.js源码中直接复制可得
##   - js源码URL: e.g. http://index.baidu.com/v2/static/js/main-vendor.dbf1ed721cfef2e2755d.js
CODE2CITY<-c("济南","贵阳","黔南","六盘水","南昌","九江","鹰潭","抚州","上饶",
             "赣州","重庆","包头","鄂尔多斯","巴彦淖尔","乌海","阿拉善盟",
             "锡林郭勒盟","呼和浩特","赤峰","通辽","呼伦贝尔","武汉","大连",
             "黄石","荆州","襄阳","黄冈","荆门","宜昌","十堰","随州","恩施","鄂州",
             "咸宁","孝感","仙桃","长沙","岳阳","衡阳","株洲","湘潭","益阳","郴州",
             "福州","莆田","三明","龙岩","厦门","泉州","漳州","上海","遵义","黔东南",
             "湘西","娄底","怀化","常德","天门","潜江","滨州","青岛","烟台","临沂",
             "潍坊","淄博","东营","聊城","菏泽","枣庄","德州","宁德","威海","柳州",
             "南宁","桂林","贺州","贵港","深圳","广州","宜宾","成都","绵阳","广元",
             "遂宁","巴中","内江","泸州","南充","德阳","乐山","广安","资阳","自贡",
             "攀枝花","达州","雅安","吉安","昆明","玉林","河池","玉溪","楚雄","南京",
             "苏州","无锡","北海","钦州","防城港","百色","梧州","东莞","丽水","金华",
             "萍乡","景德镇","杭州","西宁","银川","石家庄","衡水","张家口","承德",
             "秦皇岛","廊坊","沧州","温州","沈阳","盘锦","哈尔滨","大庆","长春",
             "四平","连云港","淮安","扬州","泰州","盐城","徐州","常州","南通","天津",
             "西安","兰州","郑州","镇江","宿迁","铜陵","黄山","池州","宣城","巢湖",
             "淮南","宿州","六安","滁州","淮北","阜阳","马鞍山","安庆","蚌埠","芜湖",
             "合肥","辽源","松原","云浮","佛山","湛江","江门","惠州","珠海","韶关",
             "阳江","茂名","潮州","揭阳","中山","清远","肇庆","河源","梅州","汕头",
             "汕尾","鞍山","朝阳","锦州","铁岭","丹东","本溪","营口","抚顺","阜新",
             "辽阳","葫芦岛","张家界","大同","长治","忻州","晋中","太原","临汾",
             "运城","晋城","朔州","阳泉","吕梁","海口","万宁","琼海","三亚","儋州",
             "新余","南平","宜春","保定","唐山","南阳","新乡","开封","焦作","平顶山",
             "许昌","永州","吉林","铜川","安康","宝鸡","商洛","渭南","汉中","咸阳","榆林",
             "石河子","庆阳","定西","武威","酒泉","张掖","嘉峪关","台州","衢州","宁波",
             "眉山","邯郸","邢台","伊春","大兴安岭","黑河","鹤岗","七台河","绍兴","嘉兴",
             "湖州","舟山","平凉","天水","白银","吐鲁番","昌吉","哈密","阿克苏","克拉玛依",
             "博尔塔拉","齐齐哈尔","佳木斯","牡丹江","鸡西","绥化","乌兰察布","兴安盟",
             "大理","昭通","红河","曲靖","丽江","金昌","陇南","临夏","临沧","济宁","泰安",
             "莱芜","双鸭山","日照","安阳","驻马店","信阳","鹤壁","周口","商丘","洛阳","漯河",
             "濮阳","三门峡","阿勒泰","喀什","和田","亳州","吴忠","固原","延安","邵阳",
             "通化","白山","白城","甘孜","铜仁","安顺","毕节","文山","保山","东方","阿坝",
             "拉萨","乌鲁木齐","石嘴山","凉山","中卫","巴音郭楞","来宾","北京","日喀则",
             "伊犁","延边","塔城","五指山","黔西南","海西","海东","克孜勒苏柯尔克孜",
             "天门仙桃","那曲","林芝","None","防城","玉树","伊犁哈萨克","五家渠","思茅",
             "香港","澳门","崇左","普洱","济源","西双版纳","德宏","文昌","怒江","迪庆",
             "甘南","陵水黎族自治县","澄迈县","海南","山南","昌都","乐东黎族自治县",
             "临高县","定安县","海北","昌江黎族自治县","屯昌县","黄南","保亭黎族苗族自治县",
             "神农架","果洛","白沙黎族自治县","琼中黎族苗族自治县","阿里","阿拉尔","图木舒克")
CITY2CODE<-c("1","2","3","4","5","6","7","8","9","10","11","13","14","15","16","17","19",
             "20","21","22","25","28","29","30","31","32","33","34","35","36","37","38",
             "39","40","41","42","43","44","45","46","47","48","49","50","51","52","53",
             "54","55","56","57","59","61","65","66","67","68","73","74","76","77","78",
             "79","80","81","82","83","84","85","86","87","88","89","90","91","92","93",
             "94","95","96","97","98","99","100","101","102","103","104","106","107","108",
             "109","111","112","113","114","115","117","118","119","123","124","125","126",
             "127","128","129","130","131","132","133","134","135","136","137","138","139",
             "140","141","143","144","145","146","147","148","149","150","151","152","153",
             "154","155","156","157","158","159","160","161","162","163","164","165","166",
             "168","169","172","173","174","175","176","177","178","179","181","182","183",
             "184","185","186","187","188","189","191","194","195","196","197","198","199",
             "200","201","202","203","204","205","207","208","209","210","211","212","213",
             "215","216","217","218","219","220","221","222","223","224","225","226","227",
             "228","229","230","231","232","233","234","235","236","237","239","241","242",
             "243","244","246","253","256","259","261","262","263","264","265","266","268",
             "269","270","271","272","273","274","275","276","277","278","280","281","282",
             "283","284","285","286","287","288","289","291","292","293","295","297","300",
             "301","302","303","304","305","306","307","308","309","310","311","312","315",
             "317","318","319","320","322","323","324","331","333","334","335","337","339",
             "342","343","344","346","350","352","353","356","359","366","370","371","373",
             "374","375","376","378","379","380","381","383","384","386","391","395","396",
             "401","405","407","408","410","417","422","424","426","437","438","456","457",
             "466","467","472","479","480","499","506","514","516","520","525","563","582",
             "588","608","652","653","654","655","656","657","658","659","660","661","662",
             "663","664","665","666","667","668","669","670","671","672","673","674","675",
             "676","677","678","679","680","681","682","683","684","685","686","687","688",
             "689","690","691","692","693")
names(CODE2CITY)<-CITY2CODE
names(CITY2CODE)<-CODE2CITY


#--------------------输入参数--------------------

# 开始时间超过最早的数据日期会导致数据不准确
# 整体搜索指数（2011年1月1日开始）
#pc趋势数据（2006年6月1日开始）
#移动趋势数据（2011年1月1日开始）
#分全国、省、市数据（地级市以上区域可选）
#数据周期可选（日数据、周数据、月数据、年数据、自定时间段数据）
#资讯指数及媒体指数（全时段）(不可分城市)


#index_type = 1-搜索指数，2-资讯指数,3-媒体指数
index_type <- 1
#device_type = 1-PC+MO,2-PC,3-MO
device_type <- 1
mycookie <- 'BIDUPSID=189F301154164091CCD283D5721ECFA9; PSTM=1564126872; BDUSS=UtSWRPN0dHSEpRdHF-Q2VZSzlaMXdKTk01MmhSOU1EOExDRTYtQTJ2aktrTE5lSVFBQUFBJCQAAAAAAAAAAAEAAABa7EKQc2FraeKAhmwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMoDjF7KA4xeU0; BAIDUID=5BFC2F1242A611A80F25CED5C571D49C:FG=1; H_WISE_SIDS=145302_144157_142019_144883_140631_145496_141748_144117_144989_144135_145271_136863_131246_144681_137746_144741_138883_141941_127969_140065_144790_140593_144249_143491_144607_131423_100806_142206_145520_145353_139909_144872_139884_143477_144966_140312_145422_144535_143472_143856_145076_139914_110085; MCITY=-%3A; BDORZ=B490B5EBF6F3CD402E515D22BCDA1598; Hm_lvt_d101ea4d2a5c67dab98251f0b5de24dc=1612434237; bdindexid=i670n4j4d1h5tp2qdq5kcjjk87; __yjsv5_shitong=1.0_7_9ecb14e8f88a5e04c49acc3daf24a885faf2_300_1612434263522_210.13.120.5_2b2e0284; RT="z=1&dm=baidu.com&si=353mx00j9ts&ss=kkqppneq&sl=6&tt=7iy&bcn=https%3A%2F%2Ffclog.baidu.com%2Flog%2Fweirwood%3Ftype%3Dperf"; Hm_lpvt_d101ea4d2a5c67dab98251f0b5de24dc=1612434289'
#日期输入格式要求 yyyy-mm-dd,时间间隔不宜过长，否则不会按天返回数据，间隔超过一年(365天)
start_date<-'2018-11-3'
end_date<-'2021-2-28'


#--------------------主要运行--------------------

#单一关键词
output<-main(keyword,start_date,end_date,index_type,device_type,mycookie)

#多关键词
output=NULL
for (keyword in c("戴尔","电脑","笔记本电脑","台式电脑","新冠","疫情")) {
  output0<-main(keyword,start_date,end_date,index_type,device_type,mycookie)
  output0$kw=keyword
  output<-rbind(output,output0)
}

#多省份
output=NULL
for(area in PROVINCE2CODE){
  output0<-main(keyword,start_date,end_date,index_type,device_type,mycookie,area = area)
  output0$area=CODE2PROVINCE[area]
  output<-rbind(output,output0)
}

#多城市
output=NULL
for(area in CITY2CODE){
  output0<-main(keyword,start_date,end_date,index_type,device_type,mycookie,area = area)
  output0$area=CODE2CITY[area]
  output<-rbind(output,output0)
}

#多关键词多省份
output=NULL
for (keyword in c("戴尔","电脑")) {
  for(area in PROVINCE2CODE){
    output0<-main(keyword,start_date,end_date,index_type,device_type,mycookie,area = area)
    output0$kw=keyword
    output0$area=CODE2PROVINCE[area]
    output<-rbind(output,output0)
  }
}

#多关键词多城市
output=NULL
for (keyword in c("戴尔","电脑")) {
  for(area in CITY2CODE){
    output0<-main(keyword,start_date,end_date,index_type,device_type,mycookie,area = area)
    output0$kw=keyword
    output0$area=CODE2CITY[area]
    output<-rbind(output,output0)
  }
}

write.table(output,file = "C:/Users/richard.jin/Desktop/dataoutput.csv",sep = ",",row.names = F,quote = F)
