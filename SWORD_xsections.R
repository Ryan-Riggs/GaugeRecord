# SWORD_xSec_generator.R

# load packages (you must install these before you can run this script):
require(foreign)
require(geosphere)
require(rgdal)
require(shapefiles)
library(spatial)
library(sf)
library(raster)
library(tidyr)
library(dplyr)
library(maptools)
library(spbabel)
##
folds = c("AF", "AS", "EU", "NA", "OC", "SA")
wd = "E:\\research\\SWORD\\shp\\"
#for(i in 1:length(folds)){
i = value
shp = list.files(paste0(wd, folds[i], "\\"), pattern = "nodes")
shp = shp[grep(".shp", shp)]
shp = shp[grep(tolower(folds[i]), shp)]
for(j in 1:length(shp)){
nodes = st_read(paste0(wd, folds[i], "\\", shp[j]))
noi = nodes
noi_pts = as_Spatial(noi)
b = bearing(noi_pts)
b[which(is.na(b))] = 90
b[b < 0] = b[b < 0] + 360

# calculate orthogonal to azimuth:
xDir = b
xDir[xDir > 360] = xDir[xDir > 360] - 360

noi_pts$azimuth = xDir
noi_pts$w = noi_pts$width

###############################################################
q = 90-b
q[q < 0] = q[q < 0] + 360

x = noi_pts$x
y = noi_pts$y

p1 = cbind(x + sin(q*pi/180), y - cos(q*pi/180))
p2 = cbind(x - sin(q*pi/180), y + cos(q*pi/180))

# cross section length:
# 3x GRWL width (from Yang et al. RivWidthCloud):
xD = noi_pts$width
xD = 3*((noi_pts$width)/distGeo(p1, p2))


o1x = x + sin(q*pi/180)*xD
o1y = y - cos(q*pi/180)*xD
o2x = x - sin(q*pi/180)*xD
o2y = y + cos(q*pi/180)*xD

# create polygon shapefile:Trying to solve this part so I can double check I am doing the above correctly. 
X = c(o1x, o2x)
Y = c(o1y, o2y)
ID = rep(1:length(o1x), 2)
Name = unique(ID)

dd = data.frame(ID=ID, X=X, Y=Y)


# lines.list <- list()                 
# for( k in unique(dd$ID) ) {
#   print(k)
#   l <- list(as.matrix(dd[dd$ID == k,][2:3]))
#   lines.list[[k]] <- SpatialLines(list(Lines(list(Line(l)), ID=as.character(k))))
# }                  
# my.lines <- do.call("rbind", lines.list)
# my.lines <- SpatialLinesDataFrame(my.lines, 
#                                   data.frame(row.names = as.character(unique(dd$ID)), 
#                                              ID = 1:length(my.lines) ) )
my.lines <- 
  dd %>% mutate(object_ = ID, branch_ = ID, order_ = row_number()) %>% 
  rename(x_ = X, y_ = Y) %>% 
  sp()

ptsProj = projection(noi_pts)
projection(my.lines) = ptsProj
my.lines$reach_id = noi_pts$reach_id
my.lines$node_id = noi_pts$node_id
my.lines$width = noi_pts$width
my.lines$lakeflag = noi_pts$lakeflag
outFile = paste0("E:\\research\\SWORD\\3xSections\\",gsub(".shp","",shp[j]))
writeSpatialShape(my.lines,outFile)
rm(dd)
rm(my.lines)
}
#}

# library(tmap)
# tmap_mode("view")
# 
# 
# tm_shape(my.lines[1:200,])+
#   tm_lines()+
#   tm_shape(noi_pts[1:200,])+
#   tm_bubbles(scale= 0.00025)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ddShapefile = convert.to.shapefile(dd, ddTable, "ID", 3)
# plot(ddShapefile$dbf)
# 
# xsections = ddShapefile$dbf
