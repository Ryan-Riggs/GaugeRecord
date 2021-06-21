files = "E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984"
library(tidyr)
library(dplyr)
library(plyr)

grdc = fread(paste0(files, "\\GRDC1_GRWL_30m.csv"))%>%select(-V4, -V5)
gsim = fread(paste0(files, "\\GSIM_GRWL_30m.csv"))%>%select(-V4, -V5)
russ= fread(paste0(files, "\\Russia_GRWL_30m.csv"))%>%select(-V4, -V5)
usgs= fread(paste0(files, "\\USGS_GRWL_30m.csv"))%>%select(-V4, -V5)
thailand= fread(paste0(files, "\\Thailand_GRWL_30m.csv"))%>%select(-V4, -V5)
can= fread(paste0(files, "\\Canada_GRWL_30m.csv"))%>%select(-V4, -V5)
afr= fread(paste0(files, "\\Africa_GRWL_30m.csv"))%>%select(-V4, -V5)

fun = function(f){
  f$Station_Num = as.character(f$Station_Num)
  return(f)
}

grdc$Station_Num = paste0(grdc$Station_Num, "_grdc")
gsim$Station_Num = paste0(gsim$Station_Num, "_gsim")
russ$Station_Num = paste0(russ$Station_Num, "_russ")
usgs$Station_Num = paste0(usgs$Station_Num, "_usgs")
thailand$Station_Num = paste0(thailand$Station_Num, "_thailand")
can$Station_Num = paste0(can$Station_Num, "_can")
afr$Station_Num = paste0(afr$Station_Num, "_africa")










a = lapply(list(grdc, gsim, russ, usgs, thailand, can, afr), FUN = fun)

all = rbindlist(a)
length(unique(all$Station_Num))

##Filter out from gsim: usgs, grdc, canada, thailand, russia. 
#gsim1 = gsim[!grepl("^TH", gsim$Station_Num),]
#gsim1 = gsim[!grepl("^US", gsim$Station_Num),]
#gsim1 = gsim[!grepl("^RU", gsim$Station_Num),]
#gsim1 = gsim[!grepl("^CA", gsim$Station_Num),]
# gsim1 = gsim[!grepl("^GB", gsim$Station_Num),]
# 
# 
# 
# plot(gsim$LONG, gsim$LAT)
# plot(gsim1$LONG, gsim1$LAT)

comb = st_read("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\combined.shp")

india_sp = st_as_sf(gTab, coords = c("LONG", "LAT"))
india_sp$Station_Num = as.character(india_sp$Station_Num)


colnames(india_sp) = colnames(comb)
t = rbind(comb, india_sp)

st_write(t, "E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\combined_andIndia.shp")





