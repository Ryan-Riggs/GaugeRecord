grdc = readxl::read_xlsx("E:\\research\\GRDC\\GRDC_Stations.xlsx")
library(ggplot2)

ggplot(data = grdc,aes(x = long, y=lat))+
  geom_point()

library(rnaturalearth)
library(rnaturalearthdata)

world = ne_countries(scale = "small", returnclass="sf")
world = world[world$sovereignt!="Antarctica",]

##Basemap. 
###############################################################################
ggplot(world)+
  geom_sf(color="black", fill = "black")+
  coord_sf(ylim=c(-50,90))+
  theme_classic()+
  theme(axis.title=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.line.x = element_blank())+
  geom_point(data = grdc, aes(x=long, y=lat), size =.2,shape=1,color="red",fill="red", alpha=.5)

#################################################################################
clean = grdc[grdc$d_miss==0,]
clean = st_as_sf(clean[!is.na(clean$lat)&!is.na(clean$long),], coords=c("long", "lat"))
st_crs(clean) = crs(world)

#####################################################################################
##Proper projection into Wagner 5
map = ggplot(world)+
  geom_sf(color="grey", fill = "grey")+
  coord_sf(crs = "+proj=wag5")+
  theme_classic()+
  theme(axis.title=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.line.x = element_blank())+
  geom_sf(data = clean, color="blue", fill = "blue", size=.2)+
  coord_sf(crs ="+proj=wag5")
map
#####################################################################################
##Map of continuous vs discontinuous GRDC records. 
#####################################################################################
grdc = readxl::read_xlsx("E:\\research\\GRDC\\GRDC_Stations.xlsx")
grdc = st_as_sf(grdc[!is.na(grdc$lat)&!is.na(grdc$long),], coords=c("long", "lat"))
st_crs(grdc) = crs(world)
grdc$continuous = ifelse(grdc$d_miss==0, "Continuous", "Discontinuous")
grdc$continuous = ifelse(is.na(grdc$continuous),"Discontinuous", "Continuous")
grdc$continuous = as.factor(grdc$continuous)
grdc$continuous = ifelse(grdc$continuous=="Continuous", 1, 0)


clean = grdc[grdc$d_miss==0,]
discont = grdc[grdc$d_miss>0,]

map = ggplot(world)+
  geom_sf(color="grey85", fill = "grey85")+
  coord_sf(crs = "+proj=wag5")+
  theme_classic()+
  theme(axis.title=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.line.x = element_blank())+
  geom_sf(data = discont, color="firebrick3", fill = "red", size=.2, alpha=.5)+
  coord_sf(crs ="+proj=wag5")+
  geom_sf(data = clean, color="blue4", fill = "blue", size=.2, alpha=.5, show.legend="point", inherit.aes = FALSE)+
  coord_sf(crs ="+proj=wag5")+
  scale_fill_manual(values=c("Discontinuous gauges"="red", "Continuous gauges"="blue"),
                    labels = c("Discontinous", "Continuous"))
map  

# png("E:\\research\\RatingCurveAnalysis\\Figures\\recordMap.png",
#     units = "in",
#     width = 12,
#     height = 6,
#     pointsize = 10,
#     res=500
# )


##################################################################################################
##NSE results map. 
##################################################################################################
library(data.table)
library(raster)
library(sf)
library(sp)
lookup = fread("E:\\research\\RatingCurveAnalysis\\obs\\lookupFull_pcw.csv")
look = lookup[!is.na(lookup$NSE),]
gage_stats = lookup
gage_stats = as.data.table(gage_stats)
group = gage_stats[gage_stats[,.I[which.max(NSE)], by=Site_number]$V1]
gage_file = st_read("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\combined_andIndia.shp")
gage_file = gage_file[gage_file$Sttn_Nm%in%paste0(group$Site_number, "_grdc"),]
group$Sttn_Nm = paste0(group$Site_number, "_grdc")
gage_stats_vals = merge(gage_file, group,by = 'Sttn_Nm')
st_crs(gage_stats_vals) = crs(world)

nse = ggplot(world)+
  geom_sf(color="grey85", fill = "grey85")+
  coord_sf(crs = "+proj=wag5")+
  theme_classic()+
  theme(axis.title=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.line.x = element_blank())+
  geom_sf(data = gage_stats_vals, aes(color=NSE),size=1, pch=19)+
  scale_colour_gradient2(low="firebrick",mid="darkolivegreen1", high="darkolivegreen", midpoint=0, limits=c(-1,1))+
  #scale_color_viridis_c(option="plasma", direction =-1)+
  coord_sf(crs ="+proj=wag5")
nse  

# png("E:\\research\\RatingCurveAnalysis\\Figures\\nseMap.png",
#     units = "in",
#     width = 12,
#     height = 6,
#     pointsize = 10,
#     res=500
# )


gage_stats_vals$mode = factor(gage_stats_vals$mode, levels=c("Linear", "Pwr", "Spl","Pcw", "RF", "KNN"))
mode = ggplot(world)+
  geom_sf(color="grey85", fill = "grey85")+
  coord_sf(crs = "+proj=wag5")+
  theme_classic()+
  theme(axis.title=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.line.x = element_blank())+
  geom_sf(data = gage_stats_vals, aes(color=mode),size=1, pch=19)+
  scale_colour_brewer(palette="Set1")+
  #scale_color_viridis_c(option="plasma", direction =-1)+
  coord_sf(crs ="+proj=wag5")
mode  













#######################################################################################################
##RRMSE map
#######################################################################################################
lookup = fread("E:\\research\\RatingCurveAnalysis\\obs\\lookupFull.csv")
gage_stats = lookup
gage_stats = as.data.table(gage_stats)
group = gage_stats[gage_stats[,.I[which.min(RRMSE)], by=Site_number]$V1]
gage_file = st_read("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\combined_andIndia.shp")
gage_file = gage_file[gage_file$Sttn_Nm%in%paste0(group$Site_number, "_grdc"),]
group$Sttn_Nm = paste0(group$Site_number, "_grdc")
gage_stats_vals = merge(gage_file, group,by = 'Sttn_Nm')
st_crs(gage_stats_vals) = crs(world)

rrmse = ggplot(world)+
  geom_sf(color="grey85", fill = "grey85")+
  coord_sf(crs = "+proj=wag5")+
  theme_classic()+
  theme(axis.title=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.line.x = element_blank())+
  geom_sf(data = gage_stats_vals, aes(color=RRMSE), scale=0.2)+
  scale_color_distiller(palette="greens")+
  #scale_color_viridis_c(option="plasma", direction =-1)+
  coord_sf(crs ="+proj=wag5")
rrmse  



####GRDC
gage_regulation = read.csv("C:\\Users\\rriggs\\Downloads\\gage_regulation_new.csv")
gage_GRDC = grep("^GRDC", as.character(gage_regulation$stationid))
gage_regulation_GRDC = gage_regulation[gage_GRDC,]

##modify the station id so that it can match with gageinfo file. 
gage_reg_grdc_updated = gsub("^GRDC", "", gage_regulation_GRDC$stationid)
gage_reg_grdc_updated_1 = gsub("^_", "", gage_reg_grdc_updated)

#gage_reg_usgs_updated_0 = gsub("^0", "", gage_reg_usgs_updated_1)
gage_regulation_GRDC$stationid = gage_reg_grdc_updated_1

########################################################################################################
##Boxplots. 
########################################################################################################
library(vioplot)
##All results.
lookup = fread("E:\\research\\RatingCurveAnalysis\\obs\\lookupFull.csv")
gage_stats = lookup
gage_stats = as.data.table(gage_stats)
gage_stats$recurrence=NA
gage_stats = gage_stats[gage_stats[,.I[which.max(NSE)], by=Site_number]$V1]
gage_stats$Sttn_Nm = paste0(gage_stats$Site_number, "_grdc")
gage_stats$GRWL_width_m=gage_stats_vals$GRWL_wd[match(gage_stats$Sttn_Nm, gage_stats_vals$Sttn_Nm)]
gage_stats$R = gage_stats$NSE
gage_stats$dams = gage_regulation_GRDC$distance[match(gage_stats$Site_number, gage_regulation_GRDC$stationid)]
gage_stats$dams = ifelse(!is.na(gage_stats$dams), gage_stats$dams, -1)
#gage_stats$GRWL_width_m=gage_stats$dams
width = 400
n = boxplot(gage_stats$NRMSE)
rB = boxplot(gage_stats$rBias)
rR = boxplot(gage_stats$RRMSE)
R = boxplot(gage_stats$R)
K = boxplot(gage_stats$KGE)
par(mai = c(1,1,1,1))
par(mfrow = c(1,1), pty = "s")
myCol = c("white", "lightskyblue2")
myCol1 = myCol
limits = c(-10, 200)
vColor = "grey90"
vOutline = "grey90"
vWdth = 1
vTy = 1
thickness = 2.65
b_Thickness = 0.6

boxplot(outline = FALSE, gage_stats$rBias,
        gage_stats$rBias[gage_stats$GRWL_width_m>=width], gage_stats$RRMSE,gage_stats$RRMSE[gage_stats$GRWL_width_m>=width],NA,NA, ylim = limits,xlim = c(0,9),
        ylab = "", las=2,boxwex = b_Thickness,
        at = c(1,2,4,5,7,8), col = rep(myCol, 4), medcol = "black", staplelty = 0, lwd =2, whisklty = 1,medlwd =2,bty = "n",xaxt = "n")
box(lwd = 2)
mtext("Percent (%)", side = 2,line = 3, col="black", cex = 1.25)
medList = list(gage_stats$NRMSE, gage_stats$NRMSE[gage_stats$GRWL_width_m>=width], gage_stats$rBias, gage_stats$rBias[gage_stats$GRWL_width_m>=width],
               gage_stats$RRMSE, gage_stats$RRMSE[gage_stats$GRWL_width_m>=width])
meds = lapply(medList, median, na.rm = TRUE)
#text(c(1,2,4,5,7,8), unlist(meds)+10, round(unlist(meds)))
numb = paste0("n=", nrow(gage_stats))
numb1 = paste0("n=", nrow(gage_stats[gage_stats$GRWL_width_m>=width,]))
numbCombined = rep(c(numb, numb1),4)
#text(c(1,2,4,5,7,8), unlist(meds)-5, numbCombined[1:6])
par(new = TRUE)
boxplot(outline = FALSE,NA, NA, NA,NA,gage_stats$R,gage_stats$R[gage_stats$GRWL_width_m>=width], ylim = c(-1,1), axes = FALSE, col = rep(myCol, 4),
        boxwex = b_Thickness,
        medcol = "red", staplelty = 0, border = "red", lwd = 2,whisklty = 1,medlwd = 2, at = c(1,2,4,5,7,8), xlim = c(0,9))
medList = list(gage_stats$KGE,gage_stats$KGE[gage_stats$GRWL_width_m>=width],gage_stats$R, gage_stats$R[gage_stats$GRWL_width_m>=width])
meds = lapply(medList, median, na.rm = TRUE)
#text(c(10,11,13,14), unlist(meds)+.05, signif(unlist(meds),1))
#text(10:11, unlist(meds)-.05, numbCombined[7:8])

axis(4, col = "red",col.ticks = "red",col.axis = "red", las = TRUE, lwd = 2)
axis(4, col = "red",col.ticks = "red",col.axis = "red", las = TRUE, lwd = 2, at =seq(-1.5, 1.5))
axis(1, at = c(1.5,4.5), labels= c("rBias","RRMSE"))
axis(1, at = c(7.5), labels = c("NSE"), col = "red", col.ticks = "red", col.axis = "red")
mtext("NSE", side = 4,line = 3, col="red", cex = 1.25)
n = nrow(gage_stats)
#text = paste("N = ", n)
top = expression(Width >= paste(30, " m"), 
                 atop(paste("N = 949 Gauges")))
bottom =expression(Width >= paste(400, " m"), 
                   atop(paste("N = 91 Gauges")))
legend("topleft", legend = c(top, bottom), xpd = TRUE, fill = c(myCol1[1], NA, myCol1[2], NA),
       horiz = FALSE, bty = "n", inset = c(0,0),
       border =c("black", NA, "black", NA), cex = 0.8, y.intersp = .5)



png("E:\\research\\RatingCurveAnalysis\\Figures\\boxplots.png",
    units = "in",
    width = 5.5,
    height = 5.5,
    res = 500,
    pointsize = 10)

dev.off()


lks = st_read("E:\\research\\MISC\\hydroLAKES\\HydroLAKES_polys_v10_shp\\HydroLAKES_polys_v10.shp")
######################################################################################################################################
##Width analysis. 
######################################################################################################################################
library(tidyr)
library(data.table)
lookup = fread("E:\\research\\RatingCurveAnalysis\\obs\\lookupFull.csv")
look = lookup[!is.na(lookup$NSE),]
gage_stats = lookup
gage_stats = as.data.table(gage_stats)
group = gage_stats[gage_stats[,.I[which.max(NSE)], by=Site_number]$V1]
gage_file = st_read("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\combined_andIndia.shp")
gage_file = gage_file[gage_file$Sttn_Nm%in%paste0(group$Site_number, "_grdc"),]
group$Sttn_Nm = paste0(group$Site_number, "_grdc")
gage_stats_vals = merge(gage_file, group,by = 'Sttn_Nm')
widthOut = as.data.frame(gage_stats_vals)
breaks = c(60,90,120,200,400, max(widthOut$GRWL_wd, na.rm = TRUE))
values = c(paste0(as.character(breaks), "\u2264"))
widthOut$width = NA
widthOut$width[widthOut$GRWL_wd<=breaks[1]] = values[1] 
widthOut$width[widthOut$GRWL_wd<=breaks[2]&widthOut$GRWL_wd>breaks[1]] = values[2] 
widthOut$width[widthOut$GRWL_wd<=breaks[3]&widthOut$GRWL_wd>breaks[2]] = values[3] 
widthOut$width[widthOut$GRWL_wd<=breaks[4]&widthOut$GRWL_wd>breaks[3]] = values[4] 
widthOut$width[widthOut$GRWL_wd<=breaks[5]&widthOut$GRWL_wd>breaks[4]] = values[5]
widthOut$width[widthOut$GRWL_wd>breaks[5]] = values[6] 

widthOut = widthOut[!is.na(widthOut$width),]
widthOut = widthOut%>%dplyr::select(Sttn_Nm,width,NSE,RRMSE,NRMSE,KGE,rBias)



widthLong = melt(setDT(widthOut),id.vars = "width")
aridLong = widthLong
aridLong = aridLong[aridLong$variable=="NSE"|aridLong$variable=="RRMSE"|aridLong$variable=="NRMSE"|aridLong$variable=="KGE"|aridLong$variable=="rBias",]
aridLong$value=as.numeric(aridLong$value)
aridLong$width=factor(aridLong$width, levels = c(values))


rrmse=ggplot(aridLong[aridLong$variable=="RRMSE",], aes(x=variable,y=value, fill=width))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(0,200))+
  theme_classic()+
  scale_fill_brewer(palette="RdYlBu")
rrmse

nrmse=ggplot(aridLong[aridLong$variable=="NRMSE",], aes(x=variable,y=value, fill=width))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(0,200))+
  theme_classic()+
  scale_fill_brewer(palette="RdYlBu")
nrmse

rbias=ggplot(aridLong[aridLong$variable=="rBias",], aes(x=variable,y=value, fill=width))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(-25,25))+
  theme_classic()+
  scale_fill_brewer(palette="RdYlBu")
rbias

kge=ggplot(aridLong[aridLong$variable=="KGE",], aes(x=variable,y=value, fill=width))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(-1,1))+
  theme_classic()+
  scale_fill_brewer(palette="RdYlBu")
kge

nse=ggplot(aridLong[aridLong$variable=="NSE",], aes(x=variable,y=value, fill=width))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(-1,1))+
  theme_classic()+
  scale_fill_brewer(palette="RdYlBu")
nse


nse=ggplot(aridLong[aridLong$variable=="NSE",], aes(x=variable,y=value, fill=width))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(0,1))+
  theme_classic()+ylab("NSE")+
  theme(axis.title.x=element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(), legend.position = "bottom", legend.direction = "horizontal")+
  scale_fill_brewer(palette="RdYlBu")+scale_color_discrete(NULL) +guides(color = guide_legend(nrow = 1))
nse

ggarrange(nse+theme(axis.title=element_blank()), legend="bottom")

pdf("E:\\research\\RatingCurveAnalysis\\Figures\\Widthboxplots.pdf",
    width = 8,
    height = 3)

dev.off()



library(ggpubr)
ggarrange(nrmse+theme(axis.title = element_blank()), rbias+theme(axis.title = element_blank()), 
          rrmse+theme(axis.title = element_blank()), 
          kge+theme(axis.title = element_blank()),
          nse+theme(axis.title = element_blank()),
          common.legend=TRUE, legend="bottom", ncol=5, nrow=1)




############################################################################################################
##Aridity index. 
############################################################################################################
library(data.table)
lookup = fread("E:\\research\\RatingCurveAnalysis\\obs\\lookupFull.csv")
look = lookup[!is.na(lookup$NSE),]
gage_stats = lookup
gage_stats = as.data.table(gage_stats)
group = gage_stats[gage_stats[,.I[which.max(NSE)], by=Site_number]$V1]
gage_file = st_read("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\combined_andIndia.shp")
gage_file = gage_file[gage_file$Sttn_Nm%in%paste0(group$Site_number, "_grdc"),]
group$Sttn_Nm = paste0(group$Site_number, "_grdc")
gage_stats_vals = merge(gage_file, group,by = 'Sttn_Nm')
arid=st_read("E:\\research\\HydroAtlas\\BasinATLAS_v10_shp\\BasinATLAS_v10_lev10.shp")
gage = gage_stats_vals#[1:50,]

st_crs(gage) = crs(arid)

arid_pts = st_join(gage, arid, join=st_within)
library(tidyr)
library(reshape)
library(reshape2)
aridOut = as.data.frame(arid_pts)
aridOut = aridOut%>%dplyr::select(NSE, ari_ix_sav,Sttn_Nm, KGE, rBias, RRMSE, NRMSE)
aridOut$aridity = NA
aridOut$aridity[aridOut$ari_ix_sav<20] = "Arid"
aridOut$aridity[aridOut$ari_ix_sav>=20&aridOut$ari_ix_sav<50] = "Semiarid"
aridOut$aridity[aridOut$ari_ix_sav>=50&aridOut$ari_ix_sav<65] = "Subhumid"
aridOut$aridity[aridOut$ari_ix_sav>=65] = "Humid"
aridOut = aridOut[!is.na(aridOut$aridity),]

aridLong = melt(setDT(aridOut),id.vars = "aridity")
aridLong = aridLong[aridLong$variable=="NSE"|aridLong$variable=="RRMSE"|aridLong$variable=="NRMSE"|aridLong$variable=="KGE"|aridLong$variable=="rBias"]
aridLong$value=as.numeric(aridLong$value)
aridLong$aridity=factor(aridLong$arid, levels = c("Arid", "Semiarid", "Subhumid", "Humid"))


rrmse=ggplot(aridLong[aridLong$variable=="RRMSE",], aes(x=variable,y=value, fill=aridity))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(0,200))+
  theme_classic()+
  scale_fill_brewer(palette="RdYlBu")
rrmse

nrmse=ggplot(aridLong[aridLong$variable=="NRMSE",], aes(x=variable,y=value, fill=aridity))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(0,200))+
  theme_classic()+
  scale_fill_brewer(palette="RdYlBu")
nrmse

rbias=ggplot(aridLong[aridLong$variable=="rBias",], aes(x=variable,y=value, fill=aridity))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(-25,25))+
  theme_classic()+
  scale_fill_brewer(palette="RdYlBu")
rbias

kge=ggplot(aridLong[aridLong$variable=="KGE",], aes(x=variable,y=value, fill=aridity))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(-1,1))+
  theme_classic()+
  scale_fill_brewer(palette="RdYlBu")
kge

nse=ggplot(aridLong[aridLong$variable=="NSE",], aes(x=variable,y=value, fill=aridity))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(0,1))+
  theme_classic()+ylab("NSE")+
  theme(axis.title.x=element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(), legend.position = "bottom")+
  scale_fill_brewer(palette="RdYlBu")
nse

png("E:\\research\\RatingCurveAnalysis\\Figures\\Aridboxplots.png",
    units = "in",
    width = 4,
    height = 6,
    res = 500,
    pointsize = 10)

dev.off()









ggarrange(nse+theme(axis.title=element_blank()), legend="bottom")


library(ggpubr)

ggarrange(nrmse+theme(axis.title = element_blank()), rbias+theme(axis.title = element_blank()), 
          rrmse+theme(axis.title = element_blank()), 
          kge+theme(axis.title = element_blank()),
          nse+theme(axis.title = element_blank()),
          common.legend=TRUE, legend="bottom", ncol=5, nrow=1)

###############################################################################################################
##Dams
###############################################################################################################
dams=st_read("E:\\research\\MISC\\GRAND\\GRanD_Version_1_3\\GRanD_dams_v1_3.shp")
gage = gage_stats_vals#[1:50,]
dams=st_buffer(dams, 1)
dam_pts=st_intersection(gage,dams)
#####################################
##Still working on this. 
#####################################
##Recurrence vs long-term. 
#####################################
library(data.table)  
recur1 = fread("E:\\research\\RatingCurveAnalysis\\obs\\recurrence_Full.csv")
recur2=fread("E:\\research\\RatingCurveAnalysis\\obs\\recurrence_Full_pt2.csv")
library(dplyr)
recur = bind_rows(recur1, recur2)
recur = recur[recur[,.I[which.max(NSE)], by=Site_number]$V1]
# multiple$multiple = multiple$NSE

lookup = fread("E:\\research\\RatingCurveAnalysis\\obs\\lookupFull.csv")
gage_stats = lookup
gage_stats = as.data.table(gage_stats)
gage_stats$recurrence=NA
gage_stats = gage_stats[gage_stats[,.I[which.max(NSE)], by=Site_number]$V1]
# group$single = group$NSE
# group$recurrence=NA
#group= merge(gage_stats, recur, by = c("Site_number"))
group = bind_rows(gage_stats, recur)



statsLong = melt(setDT(group),id.vars = "recurrence")
statsLong = statsLong[statsLong$variable=="NSE"|statsLong$variable=="RRMSE"|statsLong$variable=="NRMSE"|statsLong$variable=="KGE"|statsLong$variable=="rBias",]
statsLong$value=as.numeric(statsLong$value)
statsLong$recurrence=factor(statsLong$recurrence,levels=c(NA,2:30))
#aridLong$aridity=factor(aridLong$arid, levels = c("Arid", "Semiarid", "Subhumid", "Humid"))
g=ggplot(statsLong, aes(x=variable, y=value, fill=recurrence)) + 
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~variable, scale="free")
g

pal = colorRampPalette(brewer.pal(11, "RdYlBu"))

nse=ggplot(statsLong[statsLong$variable=="NSE",], aes(x=variable,y=value, fill=recurrence))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(-1,1))+
  theme_classic()+
  scale_fill_manual(values=pal(length(unique(statsLong$recurrence))))
  #scale_fill_brewer(palette="RdYlBu",n=unique(statsLong$recurrence))
nse


rec = unique(statsLong$recurrence)
rec = c(2,5,10,15,20)
stats = list()
for(i in 1:length(rec)){
  print(i)
  stat = statsLong$value[statsLong$variable=="NSE"&statsLong$recurrence==rec[i]]
  #if(length(na.omit(stat))>1){
  stats[[i]] = summary(na.omit(stat))
  #}
}

for(j in 1:length(stats)){
  s = stats[j]
  df = as.data.frame(unlist(s))
  stats[[j]] = as.data.frame(t(df))
}
st = rbindlist(stats)
st$recurrence = rec
st$median = 1
st$interquartile = "grey"

line=ggplot(st,aes(x=as.numeric(st$rec),y=st$Median, colour = "median"))+
  coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  geom_ribbon(aes(ymin=st$`1st Qu.`, ymax = st$`3rd Qu.`,colour="interquartile"), alpha=0.2,linetype=0)+geom_line()+geom_point()+
  ylab("NSE")+xlab("Recurrence")+scale_colour_manual(name='', values=c("interquartile" = "transparent", "median" = "black")) +
  scale_fill_manual(name = '',  values=c("interquartile" = "transparent", "median" = "black"))+theme(legend.position=c(.8,.9))
line



png("E:\\research\\RatingCurveAnalysis\\Figures\\lineNSE.png",
    units = "in",
    width = 4.4,
    height = 3.56,
    res = 500,
    pointsize = 10)

dev.off()




rrmse=ggplot(statsLong[statsLong$variable=="RRMSE",], aes(x=variable,y=value, fill=recurrence))+
  geom_boxplot(outlier.shape = NA)+
  #scale_y_continuous(limits=c(0,100))+
  coord_cartesian(ylim=c(0,200))+
  theme_classic()+
  scale_fill_manual(values=pal(length(unique(statsLong$recurrence))))
rrmse
#scale_fill_brewer(palette="RdYlBu",n=unique(statsLong$recurrence))

nrmse=ggplot(statsLong[statsLong$variable=="NRMSE",], aes(x=variable,y=value, fill=recurrence))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(0,200))+
  theme_classic()+
  scale_fill_manual(values=pal(length(unique(statsLong$recurrence))))
#scale_fill_brewer(palette="RdYlBu",n=unique(statsLong$recurrence))


rbias=ggplot(statsLong[statsLong$variable=="rBias",], aes(x=variable,y=value, fill=recurrence))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(-25,25))+
  theme_classic()+
  scale_fill_manual(values=pal(length(unique(statsLong$recurrence))))
#scale_fill_brewer(palette="RdYlBu",n=unique(statsLong$recurrence))

kge=ggplot(statsLong[statsLong$variable=="KGE",], aes(x=variable,y=value, fill=recurrence))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim=c(-1,1))+
  theme_classic()+
  scale_fill_manual(values=pal(length(unique(statsLong$recurrence))))
#scale_fill_brewer(palette="RdYlBu",n=unique(statsLong$recurrence))


library(ggpubr)

ggarrange(nrmse+theme(axis.title = element_blank()), rbias+theme(axis.title = element_blank()), 
          rrmse+theme(axis.title = element_blank()), 
          kge+theme(axis.title = element_blank()),
          nse+theme(axis.title = element_blank()),
          common.legend=TRUE, legend="bottom", ncol=5, nrow=1)


statsLong$value=as.numeric(statsLong$value)
statsLong$recurrence = as.numeric(statsLong$recurrence)
ggplot(statsLong[statsLong$variable=="RRMSE"&statsLong$recurrence==35,], aes(x=variable,y=value))+
  geom_boxplot()+
  scale_y_continuous(limits=c(0,500))

















library(data.table)  
recur1 = fread("E:\\research\\RatingCurveAnalysis\\obs\\recurrence_Full.csv")
recur2=fread("E:\\research\\RatingCurveAnalysis\\obs\\recurrence_Full_pt2.csv")
library(dplyr)
recur = bind_rows(recur1, recur2)
multiple = recur[recur[,.I[which.max(NSE)], by=Site_number]$V1]
multiple$multiple = multiple$NSE

lookup = fread("E:\\research\\RatingCurveAnalysis\\obs\\lookupFull.csv")
gage_stats = lookup
gage_stats = as.data.table(gage_stats)
group = gage_stats[gage_stats[,.I[which.max(NSE)], by=Site_number]$V1]
group$single = group$NSE
group$recurrence=NA

group= merge(group, multiple, by = "Site_number")
group$Sttn_Nm=group$Site_number
group$diff = group$multiple-group$single
mean(group$diff[group$diff>0])


# gage_file = st_read("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\combined_andIndia.shp")
# gage_file = gage_file[gage_file$Sttn_Nm%in%paste0(group$Site_number, "_grdc"),]
# group$Sttn_Nm = paste0(group$Site_number, "_grdc")
# gage_stats_vals = merge(gage_file, group,by = 'Sttn_Nm')
# st_crs(gage_stats_vals) = crs(world)
#   
# tm_shape(gage_stats_vals)+
#   tm_bubbles(col="NSE", breaks=c(-1,0,.25,.5,.75,1))
#   
# tm_shape(gage_stats_vals)+
#   tm_bubbles(col="diff", breaks=c(-1,0,1))
#   
  


library(tmap)
tmap::tmap_mode("view")  
tm_shape(gage_stats_vals)+
  tm_bubbles(col = "mode",size = 0.2, breaks = c("KNN", "RF", "Linear", "Pwr", "Spl"))













#################################################################################################################
##Gauges
#################################################################################################################
df = st_read("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\combined_andIndia.shp")
pth = "E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\"
usgs = fread(paste0(pth, "USGS_GRWL_30m.csv"))
usgs$Station_Num=as.character(usgs$Station_Num)
usgs$agency = "USGS"
india = fread(paste0(pth, "India_GRWL_30m.csv"))
india$agency= "IWRIS"
russia = fread(paste0(pth, "Russia_GRWL_30m.csv"))
russia$Station_Num = as.character(russia$Station_Num)
russia$agency = "Arcticnet"
thailand = fread(paste0(pth, "Thailand_GRWL_30m.csv"))
thailand$agency = "RID"
canada = fread(paste0(pth, "Canada_GRWL_30m.csv"))
canada$agency="HYDAT"
brazil = fread(paste0(pth, "brazil_notInGRDC.csv"))
brazil$Station_Num = as.character(brazil$Station_Num)
brazil$agency = "ANA"
grdc = fread(paste0(pth, "GRDC1_GRWL_30m.csv"))
grdc$Station_Num = as.character(grdc$Station_Num)
grdc$agency = "GRDC"
chile = fread("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\cr2_qflxDaily_2018\\cr2_qflxDaily_2018_stations.txt", sep=",", data.table = FALSE)
chile$agency = "CCRR"
chile$LONG = chile$longitud
chile$LAT = chile$latitud


comb = bind_rows(usgs, india, russia, thailand, canada,brazil, grdc)
comb = comb%>%dplyr::select(agency, LONG, LAT)
comb = bind_rows(comb, chile%>%dplyr::select(agency, LONG, LAT))
comb$agency = factor(comb$agency, levels = c("GRDC","Arcticnet","ANA",  "HYDAT", "IWRIS", "RID", "USGS", "CCRR"))
df = st_as_sf(comb, coords=c("LONG", "LAT"))
st_crs(df) = crs(world)
df1 = ggplot(world)+
  geom_sf(color="grey85", fill = "grey85")+
  coord_sf(crs = "+proj=wag5")+
  theme_classic()+
  theme(axis.title=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.line.x = element_blank())+
  geom_sf(data = df, aes(color=agency),size=.25, pch=19, alpha = 0.5)+
  scale_colour_brewer(palette="Dark2")+
  coord_sf(crs ="+proj=wag5")
df1  

png("E:\\research\\RatingCurveAnalysis\\Figures\\gauges.png",
    units = "in",
    width = 12,
    height = 6,
    pointsize = 10,
    res=500
)


dev.off()


grwl = st_read("E:\\research\\GRWL\\GRWL_summaryStats\\GRWL_summaryStats.shp")

library(tmap)
tmap::tmap_mode("view")  
tm_shape(df)+
  tm_bubbles(size=0.0025)+
  tm_shape(grwl)+tm_lines()




















df = st_read("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\combined_andIndia.shp")
pth = "E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\"
usgs = fread(paste0(pth, "USGS_GRWL_30m.csv"))
usgs$Station_Num=as.character(usgs$Station_Num)
usgs$agency = "USGS"
india = fread(paste0(pth, "India_GRWL_30m.csv"))
india$agency= "IWRIS"
canada = fread(paste0(pth, "Canada_GRWL_30m.csv"))
canada$agency="HYDAT"
chile = fread("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\cr2_qflxDaily_2018\\cr2_qflxDaily_2018_stations.txt", sep=",", data.table = FALSE)
chile$agency = "CCRR"
chile$LONG = chile$longitud
chile$LAT = chile$latitud
gsim = fread(paste0(pth, "GSIM_GRWL_30m.csv"))






comb = bind_rows(usgs, india, russia, thailand, canada,brazil, grdc)
comb = comb%>%dplyr::select(agency, LONG, LAT)
comb = bind_rows(comb, chile%>%dplyr::select(agency, LONG, LAT))
comb$agency = factor(comb$agency, levels = c("GRDC","ANA", "Arcticnet", "HYDAT", "IWRIS", "RID", "USGS", "CCRR"))
df = st_as_sf(comb, coords=c("LONG", "LAT"))
st_crs(df) = crs(world)
df1 = ggplot(world)+
  geom_sf(color="grey85", fill = "grey85")+
  coord_sf(crs = "+proj=wag5")+
  theme_classic()+
  theme(axis.title=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.line.x = element_blank())+
  geom_sf(data = df, aes(color=agency),size=.25, pch=19, alpha = 0.5)+
  scale_colour_brewer(palette="Dark2")+
  coord_sf(crs ="+proj=wag5")
df1  



