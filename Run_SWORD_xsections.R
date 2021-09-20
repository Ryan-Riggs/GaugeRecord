value = 1
rstudioapi::jobRunScript("E:/research/RatingCurveAnalysis/src/SWORD_xsections.R",importEnv = TRUE)
value = 2
rstudioapi::jobRunScript("E:/research/RatingCurveAnalysis/src/SWORD_xsections.R",importEnv = TRUE)
value = 3
rstudioapi::jobRunScript("E:/research/RatingCurveAnalysis/src/SWORD_xsections.R",importEnv = TRUE)
value = 4
rstudioapi::jobRunScript("E:/research/RatingCurveAnalysis/src/SWORD_xsections.R",importEnv = TRUE)
value = 5
rstudioapi::jobRunScript("E:/research/RatingCurveAnalysis/src/SWORD_xsections.R",importEnv = TRUE)
value = 6
rstudioapi::jobRunScript("E:/research/RatingCurveAnalysis/src/SWORD_xsections.R",importEnv = TRUE)
value = 7
rstudioapi::jobRunScript("E:/research/RatingCurveAnalysis/src/SWORD_xsections.R",importEnv = TRUE)
value = 8
rstudioapi::jobRunScript("E:/research/RatingCurveAnalysis/src/SWORD_xsections.R",importEnv = TRUE)
value = 9
rstudioapi::jobRunScript("E:/research/RatingCurveAnalysis/src/SWORD_xsections.R",importEnv = TRUE)




all = list.files("E:\\research\\SWORD\\3xSections\\", pattern = ".shp", full.names = TRUE)

l = list()
for(i in 1:length(all)){
  print(i)
  shp = st_read(all[i])
  l[[i]] = shp
}

library(data.table)
merge = st_as_sf(rbindlist(l))

library(tmap)
tmap_mode("view")
# 
# 
tm_shape(sample_n(merge, 1000))+
   tm_lines()

st_write(merge, "E:\\research\\SWORD\\shp\\AllxSections3.shp")

gsim = st_read("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\combined_andIndia.shp")
gsim_buffer = st_buffer(gsim, 5000)

tm_shape(sample_n(gsim_buffer, 1000))+
  tm_bubbles(scale = 0.00025)





nearby = st_read("E:\\research\\SWORD\\shp\\Allnodes selection.shp")
sub = st_is_within_distance(merge, gsim, dist = 5000)


l = list()
for(i in 1:length(all)){
  print(i)
  shp = st_read(all[i])
  
  
  
  
  shp = st_intersects(gsim_buffer,shp)
  l[[i]] = shp
}











merge_sub = merge%>%filter(merge$node_id%in%nearby$node_id)
st_write(merge_sub, "E:\\research\\SWORD\\shp\\SWORD_3x_20km.shp")












