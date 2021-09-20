library(data.table)
library(dplyr)
library(tidyr)
library(plyr)
library(dataRetrieval)
library(tidyhydat)
library(sf)
library(sp)
names = c('station_nm', 'longitude', 'latitude')
##Australian gauges. 
library(bomWater)
au = get_station_list()
dischargeData = list.files("E:\\research\\GlobalGaugeData\\Australia\\")
dischargeData = gsub(".csv", "", dischargeData)
au = au[au$station_no%in%dischargeData,]
au = au[,c('station_no', 'station_longitude', 'station_latitude')]
colnames(au) = names
au$station_nm = as.character(au$station_nm)
au$longitude = as.numeric(au$longitude)
au$latitude = as.numeric(au$latitude) 
au$agency = "BOM"

##Brazilian gauges. 
dischargeData = list.files("E:\\research\\GlobalGaugeData\\Brazil\\")
dischargeData = gsub(".csv", "", dischargeData)
br = fread("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\brazil.csv")
br = br[br$CODIGO%in%dischargeData,]
br = br[,c('CODIGO', 'LONGITUDE', 'LATITUDE')]
colnames(br) = names
br$station_nm = as.character(br$station_nm)
br$longitude = as.numeric(br$longitude)
br$latitude = as.numeric(br$latitude) 
br$agency = "ANA"

##Chilean gauges. 
ch = read.table("E:\\research\\GlobalGaugeData\\Chile\\cr2_qflxDaily_2018\\cr2_qflxDaily_2018_stations.txt",fill=TRUE, header = TRUE, sep=",")
ch = foreign::read.dbf("E:\\research\\GlobalGaugeData\\Chile\\EC_mapa.dbf")
chD = list.files("E:\\research\\GlobalGaugeData\\Chile\\Discharge\\")
chD = gsub(".csv", "", chD)
ch = ch[ch$codigo_esta%in%chD,]
ch = ch[,c('codigo_esta', 'longitud', 'latitud')]
colnames(ch) = names
ch$station_nm = as.character(ch$station_nm)
ch$longitude = as.numeric(as.character(ch$longitude))
ch$latitude = as.numeric(as.character(ch$latitude))
ch$agency = "CCRR"

##Russian gauges. 
#ru = fread("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\Russian.csv")
#ru = ru[,c('Code', 'Long', 'Lat')]
ru = fread("E:\\research\\GlobalGaugeData\\ArcticNet\\SiteAttributes1.csv")
ru=  as.data.frame(ru)
ru = ru[, c("Code", "Long", "Lat")]
colnames(ru) = names
ru$station_nm = as.character(ru$station_nm)
ru$longitude = as.numeric(ru$longitude)
ru$latitude = as.numeric(ru$latitude) 
ru$agency = "arcticnet"

##thailand gauges. 
th = fread('E:\\research\\RatingCurveAnalysis\\GaugeLocations\\Thailand.csv')
th = th[,c('site', 'long', 'lat')]
colnames(th) = names
th$station_nm = as.character(th$station_nm)
th$longitude = as.numeric(th$longitude)
th$latitude = as.numeric(th$latitude) 
th$agency = "RID"

##Chinese gauges. 
ci = fread('E:\\research\\GlobalGaugeData\\China\\stations.csv')
ci = ci[,c('station_number', 'longitude', 'lat')]
colnames(ci) = names
ci$station_nm = as.character(ci$station_nm)
ci$longitude = as.numeric(ci$longitude)
ci$latitude = as.numeric(ci$latitude)
ci$agency = "CHP"

##GRDC gauges. 
grdc = readxl::read_xlsx("C:\\Users\\rriggs\\Downloads\\grdc_stations (2)\\GRDC_Stations.xlsx")
grdc = grdc[,c('grdc_no', 'long', 'lat')]
colnames(grdc) = names
grdc$station_nm = as.character(grdc$station_nm)
grdc$longitude = as.numeric(grdc$longitude)
grdc$latitude = as.numeric(grdc$latitude)
grdc$agency = "GRDC"

##USGS gauges. 
code <- "00060"
library(datasets)
states = state.abb
sites_list = list()
for(i in 1:length(states)){
  sites <- whatNWISsites(stateCd = states[i],parameterCd="00060",hasDataTypeCd="dv")#, period = paste0("P", date_range, "D"))
  sites_list[[i]] = sites
  print(i)
}
usgs = rbindlist(sites_list)
usgs = usgs[,c("site_no", "dec_long_va", "dec_lat_va")]
colnames(usgs) = names
usgs$station_nm = as.character(usgs$station_nm)
usgs$longitude = as.numeric(usgs$longitude)
usgs$latitude = as.numeric(usgs$latitude)
usgs$agency = "USGS"

##Canadian gauges. 
can = hy_stations()
can = can[,c("STATION_NUMBER", "LONGITUDE", "LATITUDE")]
colnames(can) = names
can$station_nm = as.character(can$station_nm)
can$longitude = as.numeric(can$longitude)
can$latitude = as.numeric(can$latitude)
can$agency = "HYDAT"

##Indian gauges. 
india = fread("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\India_sameOrderasQfiles.csv")
india = india[,c("Sttn_Nm", "Long", "Lat")]
colnames(india) = names
india$station_nm = as.character(india$station_nm)
india$longitude = as.numeric(india$longitude)
india$latitude = as.numeric(india$latitude)
india$agency = "IWRIS"

##Spain gauges. 
spain = fread("E:\\research\\GlobalGaugeData\\Spain\\sites.csv")
spain = spain[,c("indroea", "long", "lat")]
colnames(spain) = names
spain$station_nm = as.character(spain$station_nm)
spain$longitude = as.numeric(spain$longitude)
spain$latitude = as.numeric(spain$latitude)
spain$agency = "AFD"





##GSIM gauges, remove GRDC locations in other datasets. 
gsim = fread("E:\\research\\GSIM\\GSIM_metadata\\GSIM_catalog\\GSIM_metadata.csv")
gsim = gsim[gsim$reference.db!=gsim$paired.db,]

##Remove GRDC gauges that are in other databases. 
'%!in%' <- function(x,y)!('%in%'(x,y))
#grdc = grdc[grdc$station_nm%!in%gsim$grdb.no,]


japan = fread("E:\\research\\GSIM\\GSIM_metadata\\GSIM_catalog\\GSIM_metadata.csv")
japan = japan[japan$reference.db=="mlit",]
japanQ = list.files("E:\\research\\GlobalGaugeData\\Japan\\")
japanQ = gsub(".csv", "", japanQ)
japan = japan[japan$reference.no%in%japanQ,]
japan = japan[,c("reference.no", "longitude", "latitude")]
colnames(japan) = names
japan$station_nm = as.character(japan$station_nm)
japan$longitude = as.numeric(japan$longitude)
japan$latitude = as.numeric(japan$latitude)
japan$agency = "MLIT"




combined = bind_rows(au, br, ru, th, ch,ci, usgs, can, india, spain, japan, grdc)
combined = combined[!is.na(combined$latitude)&!is.na(combined$longitude),]
combined = combined[combined$latitude>=-90,]
grdcShp = st_as_sf(grdc, coords = c("longitude", 'latitude'))



library(ggplot2)
library(sf)
library(sp)

shp = st_as_sf(combined, coords = c('longitude', 'latitude'))
shp$agency = factor(shp$agency, levels = c("GRDC","ANA", "arcticnet", "BOM", "CCRR",
                                           "CHP", "HYDAT", "IWRIS","MLIT", "RID","AFD", "USGS"))
ggplot(data = shp, aes(col = agency))+
  geom_sf()

gages = shp
library(tmap)
tmap_mode("view")



japan = fread("E:\\research\\GSIM\\GSIM_metadata\\GSIM_catalog\\GSIM_metadata.csv")
japan = japan[japan$reference.db=="mlit",]



tm_shape(shp)+
  tm_dots(col="agency", size = 0.02)




ggplot(data=shp[shp$agency=="AFD",])+geom_sf()


distance <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}


Gage_df = combined
Gage_df = Gage_df%>%dplyr::select(station_nm,latitude, longitude)
Gage_df$latitude = as.numeric(Gage_df$latitude)
Gage_df$longitude = as.numeric(Gage_df$longitude)
Gage_df = Gage_df[!is.na(Gage_df$latitude),]
Gage_df = Gage_df%>%dplyr::select(station_nm, latitude, longitude)
######################################################## Determine nearest grwl widths, lake flags, etc. 

GRWLdirIn = paste0('E:/research/GRWL/GRWL_vector_V01.01')
GRWLPaths = list.files(GRWLdirIn, 'dbf', full.names=T)

if (!"foreign" %in% rownames(installed.packages())){
  install.packages("foreign")}; require(foreign)
if (!"rgdal" %in% rownames(installed.packages())){
  install.packages("rgdal")}; require(rgdal)
if (!"shapefiles" %in% rownames(installed.packages())){
  install.packages("shapefiles")}; require(shapefiles)
if (!"RColorBrewer" %in% rownames(installed.packages())){
  install.packages("RColorBrewer")}; require(RColorBrewer)
if (!"zyp" %in% rownames(installed.packages())){
  install.packages("zyp")}; require(zyp)


##############################################################################
# read in functions
##############################################################################
isna <- function(x){return(which(is.na(x)))}

##############################################################################
# 2 FIND GRWL WIDTH FOR EACH GAUGE STATION
##############################################################################
gTab = Gage_df[1:nrow(Gage_df),]
names(gTab) = c("Station_Num", "LAT", "LONG")

# identify which IMW tile the gauge is located in:
# IMW tiles are 4x6 latxlong:
p = paste0("0", rep(1:60, 44))
p = rep(substr(p, nchar(p)-1, nchar(p)))
r = c(rep(paste0("N", rev(LETTERS[1:22])), each=60),
      rep(paste0("S", LETTERS[1:22]), each=60))
IMWarray = matrix(paste0(r, p), byrow=T, nrow=44)
test = cbind(ceiling(((gTab$LAT-88)/-176)*44), 
             ceiling(((gTab$LONG+180)/360)*60))
gTab = gTab[test[,1]!=0,]
gTab$IMW = IMWarray[cbind(ceiling(((gTab$LAT-88)/-176)*44), 
                          ceiling(((gTab$LONG+180)/360)*60))]


GRWLIMW = substr(GRWLPaths, nchar(GRWLPaths)-7, 
                 nchar(GRWLPaths)-4)

# number of samples to average over:
nGRWLsamples = 5

uIMW = unique(gTab$IMW)
for(i in 1:length(uIMW)){
  # read in GRWL IMW tile:
  GRWLPath = GRWLPaths[uIMW[i] == GRWLIMW]
  if(length(GRWLPath)>0){GRWL = read.dbf(GRWLPath)}
  if ('dbf' %in% names(GRWL)){GRWL = GRWL$dbf}
  
  # remove GRWL data at tributary junctions:
  GRWL = GRWL[GRWL$width_m >= 30, ]
  # remove GRWL data at lakes, reservoirs, deltas, & canals: 
  # keep for now to avoid comparing widths in the wrong location along a river:
  #GRWL = GRWL[GRWL$lakeFlag == 0, ] 
  
  gaugeIMW = which(uIMW[i] == gTab$IMW)
  for (j in 1:length(gaugeIMW)){
    
    # dist = ((gTab$e[gaugeIMW[j]] - GRWL$utm_east)^2 + (gTab$n[gaugeIMW[j]] - GRWL$utm_north)^2)
    # nearest = order(dist)[1:nGRWLsamples]
    
    dist = distance(gTab$LONG[gaugeIMW[j]], gTab$LAT[gaugeIMW[j]],GRWL$lon, GRWL$lat)
    nearest = order(dist)[1:nGRWLsamples]
    
    
    
    
    
    gTab$GRWL_width[gaugeIMW[j]] = mean(GRWL$width_m[nearest])
    gTab$dist2GRWL[gaugeIMW[j]] = dist[nearest[1]]#^0.5
    gTab$lakeFlag[gaugeIMW[j]] = GRWL$lakeFlag[nearest[1]]
    
  }
  
  print(paste(uIMW[i], i, "of", length(uIMW)))
  
}

all = gTab
gTab = gTab[!is.na(gTab$dist2GRWL), ]
gTab = gTab[gTab$dist2GRWL<1, ]
ggplot(data = gTab, aes(x = LONG, y = LAT))+geom_point(aes(colour = log10(gTab$GRWL_width)))

shp = st_as_sf(gTab, coords = c("LONG", "LAT"))
ggplot(shp)+
  geom_sf(aes(colour = GRWL_width))+
  scale_color_gradient(limits = c(30,1000))

library(tmap)
tmap_mode("view")
tm_shape(shp)+
  tm_dots(col="GRWL_width", size = 0.02, breaks = c(0,30,90,100,300,500,1000,2000,5000))




tm_shape(shp)+
  tm_dots(col="agency", size = 0.02)




