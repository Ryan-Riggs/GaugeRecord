
basin = "GALICIA Costa"
discharge = read.csv(paste0("E:\\research\\GlobalGaugeData\\Spain\\afliq_", basin, ".csv"), sep=";")

stations = list.files("E:\\research\\GlobalGaugeData\\Spain\\stations\\", full.names = TRUE)


tab = list()
for(i in 1:length(stations)){
  sites = read.csv(stations[i], sep=";")
  sites = sites[,c("indroea", "xutm30", "yutm30")]
  tab[[i]] = sites
}
gages = rbindlist(tab)
library(terra)
points = cbind(gages$xutm, gages$yutm)
v = vect(points, crs = "+proj=utm +zone=30 +datum=WGS84  +units=m")
v = project(v, "+proj=longlat +datum=WGS84")
lonlat <- geom(v)[, c("x", "y")]
lonlat = as.data.frame(lonlat)
gages$lat = lonlat$y
gages$long = lonlat$x

fwrite(gages,"E:\\research\\GlobalGaugeData\\Spain\\sites.csv")
