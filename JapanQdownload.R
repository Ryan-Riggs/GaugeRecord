library(RSelenium)
library(rvest)

###Works. 
file = html_session("http://www1.river.go.jp/cgi-bin/DspWaterData.exe?KIND=6&ID=301011281104010&BGNDATE=20100101&ENDDATE=20201231")%>%
  read_html()%>%html_element('body')
file1 = file%>%html_table()
##
df = file1[5:nrow(file1),]
dts = as.Date(df$X1, format="%Y/%m/%d")
df = df[,2:ncol(df)]
df = apply(df, 2, as.numeric)
df = as.data.frame(df)
df1 = rowMeans(df, na.rm = TRUE)
df1 = as.data.frame(df1)
df1$Date = dts





#########################################################
##Run on multiple. 
#########################################################
library(data.table)
library(BBmisc)
japan = fread("E:\\research\\GSIM\\GSIM_metadata\\GSIM_catalog\\GSIM_metadata.csv")
japan = japan[japan$reference.db=="mlit",]
path = "http://www1.river.go.jp/cgi-bin/DspWaterData.exe?KIND=6&ID="

output = function(f){
  day = paste0(f, '01')
  website = paste0(path, site, "&BGNDATE=", start, "&ENDDATE=", ending)
  file = try(html_session(website)%>%
    read_html()%>%html_element('body'))
  if(is.error(file)){next}
  file1 = file%>%html_table()
  ##
  df = file1[5:nrow(file1),]
  dts = as.Date(df$X1, format="%Y/%m/%d")
  df = df[,2:ncol(df)]
  df = apply(df, 2, as.numeric)
  df = as.data.frame(df)
  df1 = rowMeans(df, na.rm = TRUE)
  df1 = as.data.frame(df1)
  df1$Date = dts
  return(df1)
}


for(i in 1:nrow(japan)){
site=japan$reference.no[i]
beginning = japan$year.start[i]
start = paste0(beginning, "0101")
end = japan$year.end[i]
ending = paste0(end, "1231")
rng = beginning:end

dayrng = c('01','02','03','04','05','06','07','08','09','10','11','12')

dtList = list()
for(r in 1:length(rng)){
  print(r)
  year = rng[r]
  v = as.vector(12)
  for(j in 1:length(dayrng)){
    v[j] = paste0(year,dayrng[j],'01')
  }
dtList[[r]] = v  
}
dates = unlist(dtList)

tab = list()
for(k in 1:length(dates)){
day = dates[k]
website = paste0(path, site, "&BGNDATE=", day, "&ENDDATE=", ending)
file = try(html_session(website)%>%
  read_html()%>%html_element('body'))
if(is.error(file)){next}
file1 = try(file%>%html_table())
if(is.error(file1)){next}
##
df = file1[5:nrow(file1),]
dts = as.Date(df$X1, format="%Y/%m/%d")
df = df[,2:ncol(df)]
df = apply(df, 2, as.numeric)
df = as.data.frame(df)
df1 = rowMeans(df, na.rm = TRUE)
df1 = as.data.frame(df1)
df1$Date = dts
tab[[k]] = df1
print(k)
}

tabOut=rbindlist(tab)
tabOut = tabOut[!is.na(tabOut$df1)]
colnames(tabOut) = c("Q", "Date")
fwrite(tabOut, paste0("E:\\research\\GlobalGaugeData\\Japan\\", japan$reference.no[i], ".csv"))
}

