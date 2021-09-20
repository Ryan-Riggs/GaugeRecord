library(RSelenium)
library(rvest)

original = "https://explorador.cr2.cl/request.php?options={%22variable%22:{%22id%22:%22qflxDaily%22,%22var%22:%22caudal%22,%22intv%22:%22daily%22,%22season%22:%22year%22,%22stat%22:%22mean%22,%22minFrac%22:80},%22time%22:{%22start%22:-946771200,%22end%22:1631664000,%22months%22:%22A%C3%B1o%20completo%22},%22anomaly%22:{%22enabled%22:false,%22type%22:%22dif%22,%22rank%22:%22no%22,%22start_year%22:1980,%22end_year%22:2010,%22minFrac%22:70},%22map%22:{%22stat%22:%22mean%22,%22minFrac%22:10,%22borderColor%22:%227F7F7F%22,%22colorRamp%22:%22Jet%22,%22showNaN%22:false,%22limits%22:{%22range%22:[5,95],%22size%22:[4,12],%22type%22:%22prc%22}},%22series%22:{%22sites%22:[%22"
  01201003
ending = "%22],%22start%22:null,%22end%22:null},%22export%22:{%22map%22:%22Shapefile%22,%22series%22:%22CSV%22,%22view%22:{%22frame%22:%22Vista%20Actual%22,%22map%22:%22roadmap%22,%22clat%22:-18.0036,%22clon%22:-69.6331,%22zoom%22:5,%22width%22:461,%22height%22:2207}},%22action%22:[%22export_series%22]}"
  
##Chile gauges. 
gages = foreign::read.dbf("E:\\research\\GlobalGaugeData\\Chile\\EC_mapa.dbf")
library(BBmisc)

qFun = function(f){
  Sys.sleep(.25)
  outpath = paste0("E:\\research\\GlobalGaugeData\\Chile\\Discharge\\", f, ".csv")
  website = paste0(original, f, ending)
  file = try(html_session(website)%>%html_element('body')%>%html_text('url'))
  if(is.error(file)){next}
  page = gsub(".*https", "", file)
  page = gsub("}}}", "", page)
  page = paste0("https", page)
  page = noquote(page)
  page = gsub('"', '', page)
  download.file(page, outpath)
}

for(i in 1:nrow(gages)){
  print(i)
  tab = try(qFun(gages$codigo_esta[i]))
}


