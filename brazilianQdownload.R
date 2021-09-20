brazil = read.csv("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\brazil.csv")

gsim = read.csv("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\GSIM_GRWL_30m.csv")
gsim_all = read.csv("E:\\research\\GSIM\\GSIM_metadata\\GSIM_catalog\\GSIM_metadata.csv")
gsim_br = gsim[grep("BR_", gsim$Station_Num), ]
gsim_br$gage = gsim_all$reference.no[match(gsim_br$Station_Num, gsim_all$gsim.no)]
stations = gsim_br$gage

link = "https://www.snirh.gov.br/hidroweb/rest/api/documento/convencionais?tipo=3&documentos="#13580000,10800000,10700000,10600000,10200000,10100000,10300000,10500000
stations = brazil$CODIGO

  
go = c(link, unlist(stations))


is.error <- function(
  expr,
  tell=FALSE,
  force=FALSE
)
{
  expr_name <- deparse(substitute(expr))
  test <- try(expr, silent=TRUE)
  iserror <- inherits(test, "try-error")
  if(tell) if(iserror) message("Note in is.error: ", test)
  if(force) if(!iserror) stop(expr_name, " is not returning an error.", call.=FALSE)
  # output:
  iserror
}
outpath2 = "E:\\research\\GlobalGaugeData\\Brazil\\zips2\\"
outpath = "E:\\research\\GlobalGaugeData\\Brazil\\zips\\"
outpath_csv = "E:\\research\\GlobalGaugeData\\Brazil\\"
for(i in 1544:length(stations)){
  print(i)
  files = paste0(link, stations[i])
  out = paste0(outpath, stations[i], ".zip")
  try(download.file(files, out, method = "curl"))
  a = unzip(out)
  data = try(read.table(unzip(a[grep("vazoes", a)]), sep = ";", header = TRUE))
  if(!is.error(data)){
  #data1 = data[9:nrow(data),]
  #cols = strsplit(as.character(data1[1,]), ";")}
  data1 = data[9:nrow(data),]
  cols = data1[1:78]
  data1 = data1[79:length(data1)]
  starts = data1 == as.character(stations[i])
  starts = which(starts)
  }else{next}
  df = as.data.frame(matrix(numeric(), nrow =length(data1)/length(cols), ncol = length(unlist(cols))))
  colnames(df) = cols
  for(j in 1:length(starts)){
    start = starts[j]
    end = starts[j]+77
    dt = data1[start:end]
    dt = gsub(",", ".", dt)
    df[j,1:length(cols)] = dt
  }
  fwrite(df,paste0(outpath_csv,  sub(",","",stations[i]), ".csv"))
}
########################################
##outputs as month down and day of month across (Vazoes = day of month.)


t = "E:\\research\\RatingCurveAnalysis\\GaugeLocations\\DischargeDatasets\\Brazil\\zips\\11400000.zip"
a = unzip(t)
list.files(a)

c = unzip(a[grep("vazoes", a)])
b = read_csv(c)

file.info(c)


l = unz(t, "vazoes_C_11400000.zip")
p = unz(l, "vazoes_C_11400000.csv")
file.info(p)
r = read.csv(p)

b = read_csv(unzip(a[grep("vazoes", a)]))
b = b[9:nrow(b),]


cols = strsplit(as.character(b[1,]), ";")
df = as.data.frame(matrix(numeric(), nrow =nrow(b), ncol = length(unlist(cols))))
for(j in 1:nrow(b)){
  a = strsplit(as.character(b[j,]), ";") 
  df[j,1:length(unlist(a))] = unlist(a)
  print(j)
}

station = list.files(outpath_csv, full.names = TRUE)
tab = read.csv(station[1])
tab2 = melt(tab)
tab2 = tab2[grep("Vazao", tab2$variable),]
tab2 = tab2[-grep("Status", tab2$variable),]
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
tab2$variable = as.character(tab2$variable)
tab2$Day = substrRight(tab2$variable, 2)
tab2$Day = as.numeric(tab2$Day)
tab2$Date = as.Date(as.character(tab2$Data), format = "%d/%m/%Y")
tab2$updatedDate = tab2$Date+(tab2$Day-1)

