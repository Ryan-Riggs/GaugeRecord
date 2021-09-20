dbf = read.csv("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\Thailand_GRWL_30m.csv")
dbf = foreign::read.dbf("E:/research/RatingCurveAnalysis/GaugeLocations/30_m_1984/combined_andIndia.dbf")


#grdc = dbf[grep("grdc", dbf$Sttn_Nm),]
thailand = dbf[grep("th", dbf$Sttn_Nm),]
russia = dbf[grep("ru", dbf$Sttn_Nm),]
india = dbf[grep("In", dbf$Sttn_Nm),]




usgs = dbf[grep("usgs", dbf$Sttn_Nm),]
usgs_sites = gsub("_usgs", "", usgs$Sttn_Nm)
usgs_sites = as.character(usgs_sites)
usgs_sites = paste0("0", usgs_sites)
for(i in 1:length(usgs_sites)){
  if(nchar(usgs_sites[i]) > 9){
    usgs_sites[i] = sub("0", "", usgs_sites[i])
  }}



usgs_q_processing = function(usgs_q){
  q_v = as.vector(usgs_q[,4])
  q_c = as.character(usgs_q[4])
  q_n = as.numeric(q_v)
  q= q_n *0.02832
  usgs_q = cbind(usgs_q, q)
  as.character(usgs_q$datetime)
  return(usgs_q)
}
usgs = try(usgs_q_processing(rawDailyData <- readNWISdv(usgs_sites[2],"00060","1984-01-01")))

processing = function(f){
  usgs = try(usgs_q_processing(rawDailyData <- readNWISdv(f,"00060","1984-01-01")))
  if(!is.error(usgs)){
  fwrite(usgs, paste0("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\DischargeDatasets\\USGS\\", f, ".csv"))
  }
}

lapply(usgs_sites, processing)


usaFiles = list.files("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\DischargeDatasets\\USGS\\")
usa= gsub(".csv", "_usgs", usaFiles)
for(i in 1:length(usa)){
  if(substr(usa[i], 1, 1)=="0"){
    usa[i] = substring(usa[i], 2)
  }}


write.csv(as.data.frame(usa), "C:\\Users\\rriggs\\Downloads\\usa1.csv")




canada = dbf[grep("can", dbf$Sttn_Nm),]
canada_sites = gsub("_can", "", canada$Sttn_Nm)



library(tidyhydat)
can = hy_daily_flows(canada_sites[1])#, Date =as.Date("1984-01-01"))

processingCan = function(f){
  can = try(hy_daily_flows(f))
  if(!is.error(can)){
  can = can[can$Date>="1984-01-01",]
  if(!is.error(can)&nrow(can)>=2){
    fwrite(can, paste0("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\DischargeDatasets\\Canada\\", f, ".csv"))
  }
  }
  
}
lapply(canada_sites, processingCan)



canadaFiles = list.files("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\DischargeDatasets\\Canada\\")
can = gsub(".csv", "_can", canadaFiles)
write.csv(as.data.frame(can), "C:\\Users\\rriggs\\Downloads\\can.csv")

###################################################################################
##Dams vs no dams. 
###################################################################################
data = fread("E:\\research\\RatingCurveAnalysis\\obs\\lookupFull.csv")
gage_stats = data
gage_stats = as.data.table(gage_stats)
group = gage_stats[gage_stats[,.I[which.max(NSE)], by=Site_number]$V1]
gage_file = st_read("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\combined_andIndia.shp")
gage_file = gage_file[gage_file$Sttn_Nm%in%paste0(group$Site_number, "_grdc"),]
group$Sttn_Nm = paste0(group$Site_number, "_grdc")
gage_stats_vals = merge(gage_file, group,by = 'Sttn_Nm')

library(ggplot2)
library(tmap)

tmap_mode("view")

tm_shape(gage_stats_vals[1,])+
  tm_bubbles(col = "NSE",size = 0.2, breaks = c(-1,0,seq(.1,1,.1)))

boxplot(gage_stats_vals$NSE)
boxplot(gage_stats_vals$NSE[!is.na(gage_stats_vals$dams)])
boxplot(gage_stats_vals$NSE[is.na(gage_stats_vals$dams)])



data$dams = gage_regulation_GRDC$distance[match(data$Site_number, gage_regulation_GRDC$stationid)]













































