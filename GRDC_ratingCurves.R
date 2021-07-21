library(sf)
library(geosphere)
library(tidyverse)
library(sf)
library(sp)
library(ggplot2)
library(rgeos)
library(dplyr)
library(gmt)
library(vroom)
library(BAMMtools)
library(hydroGOF)
library(data.table)
library(parallel)
library(foreach)
library(reshape)
library(ggplot2)
require(gridExtra)
library(cowplot)
library(randomForest)

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

##Error functions. 
source("E:/research/2019_08_30_rivObs/git/src/Error_stats_functions.R")


###Read in effective widths csv files. 
Eff_widths = map_df(list.files("E:\\research\\RatingCurveAnalysis\\Obs\\Gauges_30m_widths", full.names = TRUE, pattern = "grdc"), ~vroom(.x))
Eff_widths$Date = as.Date(as.POSIXct(Eff_widths$`system:time_start`/1000, origin = "1970-01-01"))

######################################################################################################################
###Set up validation data. 
Site_number_xsections = unique(Eff_widths$ID)

##################################################################################################
###Functions. 
auto.legend.pos <- function(x,y,xlim=NULL,ylim=NULL) {
  if (dev.cur() > 1) {
    p <- par('usr')
    if (is.null(xlim)) xlim <- p[1:2]
    if (is.null(ylim)) ylim <- p[3:4]
  } else {
    if (is.null(xlim)) xlim <- range(x, na.rm = TRUE)
    if (is.null(ylim)) ylim <- range(y, na.rm = TRUE)
  }
  countIt <- function(a) {
    tl <- sum(x <= xlim[1]*(1-a)+xlim[2]*a & y >= ylim[1]*a+ylim[2]*(1-a))
    tr <- sum(x >= xlim[1]*a+xlim[2]*(1-a) & y >= ylim[1]*a+ylim[2]*(1-a))
    bl <- sum(x <= xlim[1]*(1-a)+xlim[2]*a & y <= ylim[1]*(1-a)+ylim[2]*a)
    br <- sum(x >= xlim[1]*a+xlim[2]*(1-a) & y <= ylim[1]*(1-a)+ylim[2]*a)
    c(topleft=tl,topright=tr,bottomleft=bl,bottomright=br)
  }
  for (k in seq(0.5,0.1,by=-0.05)) {
    a <- countIt(k)
    if (sum(a==0)>0) break
    #if (!is.na(sum(a))) break
    
  }
  names(a)[which(a==0)][1]   # may delete "[1]"
}

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


validation = function(sim, obs){
  rrmse = RRMSE(sim, obs)
  nse = NSE(sim, obs)
  kge = KGE(sim, obs)
  nrmse = NRMSE(sim, obs)
  rbias = rBias(sim, obs)
  return(c(rrmse, nse, kge, nrmse, rbias))
}
usgs_q_processing = function(usgs_q){
  q_v = as.vector(usgs_q[,4])
  q_c = as.character(usgs_q[4])
  q_n = as.numeric(q_v)
  q= q_n *0.02832
  usgs_q = cbind(usgs_q, q)
  as.character(usgs_q$datetime)
  return(usgs_q)
}
#################################################################################################
gage_file = st_read("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\combined_andIndia.shp")
gage_stats$GRWL_width_m = gage_file$GRWL_wd[match(gage_stats$Site_number, gage_file$Sttn_Nm)]

gage_stats = as.data.frame(gage_stats)
apply(gage_stats, 2, median, na.rm= TRUE)
gage_stats = as.data.frame(gage_stats)
boxplot(gage_stats$RRMSE[gage_stats$GRWL_width_m>90], ylim = c(0,200))

gage_file = gage_file[gage_file$Sttn_Nm%in%gage_stats$Site_number,]
gage_stats$Sttn_Nm = gage_stats$Site_number
gage_stats_vals = merge(gage_file, gage_stats,by = 'Sttn_Nm')

library(ggplot2)
library(tmap)

tmap_mode("view")

tm_shape(gage_stats_vals)+
  tm_bubbles(col = "NSE",size = 0.2, breaks = c(-1,-.5, 0,.5, 1))
tm_shape(gage_stats_vals)+
  tm_bubbles(col = "KGE",size = 0.02, breaks = c(-1, -.4, 1))
tm_shape(gage_stats_vals)+
  tm_bubbles(col = "mode",size = 0.02)


tm_shape(gage_stats_vals)+
  tm_bubbles(col = "rBias",size = 0.02, breaks = c(-100, 0, 100))
tm_shape(gage_stats_vals[gage_stats_vals$GRWL_wd>90,])+
  tm_bubbles(col = "RRMSE",size = 0.2, breaks = c(10, 35, 50, 75, 100, 150))


tm_shape(gage_stats_vals)+
  tm_bubbles(col = "n_Landsat_obs",size = 0.02, breaks = c(0, 100, 500, 1000, 2000))



gage_stats_vals1 = gage_stats_vals%>%select(mode, geometry, rBias, NRMSE, RRMSE, KGE, NSE, n_Landsat_obs, Site_number, lakeFlg, ds2GRWL, GRWL_wd)
st_write(gage_stats_vals1, "E:\\research\\RatingCurveAnalysis\\stats\\grdc_prelim1.shp", append = FALSE)




canada = st_read("E:\\research\\RatingCurveAnalysis\\stats\\canada_prelim.shp")
brazil = st_read("E:\\research\\RatingCurveAnalysis\\stats\\brazil_prelim1.shp")
thailand = st_read("E:\\research\\RatingCurveAnalysis\\stats\\thailand_prelim1.shp")
russia = st_read("E:\\research\\RatingCurveAnalysis\\stats\\russia_prelim1.shp")
grdc = st_read("E:\\research\\RatingCurveAnalysis\\stats\\grdc_prelim1.shp")



all=bind_rows(canada, brazil, thailand, russia, grdc)
tm_shape(all)+
  tm_bubbles(col = "NSE",size = 0.2, breaks = c(-1,-.5, 0,.5, 1))

tm_shape(grdc)+
  tm_bubbles(col = "NSE",size = 0.2, breaks = c(-1,-.5, 0,.5, 1))


tm_shape(grdc)+
  tm_bubbles(col = "mode",size = 0.2,breaks = c(1,2,3) )





##Knn and/or RF works the best it seems. 
########################################################################################################################
##GRDC cross sections. 
########################################################################################################################
grdc_path = "E:\\research\\RatingCurveAnalysis\\GaugeLocations\\DischargeDatasets\\GRDC\\"
gages = list.files("E:/research/RatingCurveAnalysis/obs/SWORD/Xsections_2km/GRDC_xsection_widths/")
gages = gsub("Gauge__", "", gages)
Site_number_xsections = gsub("_grdc.csv", "", gages)
grdc_files = paste0(Site_number_xsections, "_Q_Day.Cmd.txt")
grdc_files = paste0(grdc_path, grdc_files)
xsection_files = paste0("E:/research/RatingCurveAnalysis/obs/SWORD/Xsections_2km/GRDC_xsection_widths/", "Gauge__", Site_number_xsections, "_grdc.csv")



random = function(x){
  x = na.omit(x)
  v = runif(1000, x-90, x+90)
  return(v)
}


start = as.Date("1979-01-01")
data_val = Eff_widths
RC_End = as.Date("2014-12-31") ###WAs 2014
RC_year = format(RC_End, "%Y")
RC_year_1 = format(RC_End+1, "%Y")
data_val$ID = data_val$ID
data_val$ID_2 = data_val$ID
data_val$calc_mean = data_val$Effective_width

#data_val$Date = as.character(data_val$Date)
tab$ID = tab$id
tab$ID_2 = tab$id
tab$width_m = data_val$width_m[match(tab$ID, data_val$ID)]
#tab$change= tab$median
#tab$change[mapply(is.na, tab$change)] <- 0
tab$width_m = data$width_m[match(tab$ID_2, data$ID_2)]
xSecq=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecw=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecIDcol=grep("V", names(Site_number_xsections))
mInd = array(5, dimnames = NULL)
rangedf_1 = as.data.frame(matrix(numeric(), nrow = 1, ncol = 4))
gage_stats = as.data.frame(matrix(numeric(), nrow =length(Site_number_xsections), ncol = 23))
gage_stats_GRADES = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 14))
colnames(gage_stats_GRADES)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "p_val","Bias", "RRMSE", "avg_std", "change", 'RRMSE_median', "std_Q", "STDE", "mode")
as.data.frame(gage_stats_GRADES)
l_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
Mean_grades = as.vector(nrow(Site_number_xsections))
u_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
sd_vals = as.data.frame(matrix(numeric(), nrow =length(Site_number_xsections), ncol = 20))
sd_vals_1 = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
width_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
gage_quants_q = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 100))
gage_quants_w = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 100))
colnames(gage_stats)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "mode","Bias", "RRMSE","avg_std", "change", "RRMSE_median", "std_Q","STDE", "KGE", "NSE", "rBias",
                        "SDRR", "MRR", "NRMSE", "Q_50", "W_50")
as.data.frame(gage_stats)
gage_stats_col1 = as.vector(1)
gage_stats_col2 = as.vector(1)
gage_stats_GRADES_col1 = as.vector(1)
gage_stats_GRADES_col2 = as.vector(1)
paired_df_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
rmse = 1
width_grouping = 30
percentiles = c(0.05, 0.95)
training = 0.7
data_val = as.data.table(data_val)
setkey(data_val, ID)

library(profvis)
library(caret)
library(dataRetrieval)

pdfOut = "E:/research/temp_plots/GRDC_RC_xsections_hydrographs.pdf"
pdf(pdfOut)



gage_stats = as.data.frame(matrix(numeric(), nrow =50000, ncol = 24))
colnames(gage_stats)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "mode","Bias", "RRMSE","avg_std", "change", "std_Q","STDE", "KGE", "NSE", "rBias",
                        "SDRR", "MRR", "NRMSE", "Q_50", "W_50", "node_id",".geo")
cols = c("green","yellow", "blue", "red")
n = 0

#profvis({
for(i in 1:length(Site_number_xsections)){
  paired_df = fread(xsection_files[i])
  paired_df = paired_df[paired_df$width>0&paired_df$cloud<.1&paired_df$count==2&paired_df$max==0,]
  paired_df$Date = as.Date(as.POSIXct(paired_df$`system:time_start`/1000, origin = "1970-01-01"))
  paired_df$width = paired_df$width*paired_df$length
  nodes = unique(paired_df$node_id)
  paired_df = as.data.table(paired_df)
  setkey(paired_df, node_id)
  pd1 = paired_df
  usgs_q = try(read.table(grdc_files[i], stringsAsFactors = FALSE))
  if(!is.error(usgs_q)){
    usgs_q$V1 = substr(usgs_q$V1, 0, 10)
    usgs_q$datetime = usgs_q$V1
    usgs_q$q = as.numeric(usgs_q$V2)
    usgs_q$Date = as.Date(usgs_q$datetime, format = "%Y-%m-%d")
    all = usgs_q
    usgs_q = usgs_q[usgs_q$q>0&usgs_q$Date>as.Date("1983-12-31", format ="%Y-%m-%d"),]
    paired_df = inner_join(paired_df, usgs_q)
    paired_df$Q = paired_df$q
    paired_df = paired_df[!is.na(paired_df$Q),]
    #########################################################################################################    
    nodes = unique(paired_df$node_id)
    
    if(length(nodes)==0){next}
    for(p in 1:length(nodes)){
    n = n+1
    gage_stats$Site_number[n] = Site_number_xsections[i]
    gage_stats$node_id[n] = as.numeric(nodes[p])
    
    paired_df_1 = paired_df[.(nodes[p])]
    pd = pd1[.(nodes[p])]
    gage_stats$.geo[n] = paired_df_1$.geo[1]
    if(nrow(paired_df_1)<3){next}
    set.seed(1)
    trainIndex = createDataPartition(paired_df_1$Q, p = training,
                                     list = FALSE)
    
    Train = paired_df_1[ trainIndex,]
    Valid = paired_df_1[-trainIndex,]
    
    
    if(nrow(Valid)<3|nrow(Train)<3){next}
    
    linear = lm(Train$Q~Train$width)
    ln = (Valid$width*linear$coefficients[[2]])+linear$coefficients[[1]]
    
    par(mfrow = c(2,1))
    mn = as.Date("1984-01-01")
    mx = as.Date("2021-12-31")
    all = seq.Date(mn, mx, 1)
    all = as.data.frame(all)
    colnames(all) = "Date"
    all = full_join(usgs_q, all)
    plot(all$Date[order(all$Date)], all$q[order(all$Date)], type = "l", ylab = "Discharge (cms)", xlab = "Year")
    title(paste("GRDC:", Site_number_xsections[i], ",", nodes[p]), line =0.25)
    #points(Train$Date, Train$Q, col = "purple", pch = 19)
    points(Valid$Date, (Valid$width*linear$coefficients[[2]])+linear$coefficients[[1]], col = "green")
    
    
    
    log_linear = lm(log(Train$Q)~log(Train$width))
    power = (Valid$width^log_linear$coefficients[[2]])*(exp(log_linear$coefficients[[1]]))
    #power = (Valid$width*exp(linear$coefficients[[1]]))^linear$coefficients[[2]]
    points(Valid$Date, power, col = "yellow")
    
    
    
    names = c("rrmse", "nse", "kge", "nrmse", "rbias")
    
    ##Quantile validation 
    Valid$model = ln
    qVal = validation(Valid$model, Valid$Q)
    qVal = as.data.frame(t(qVal))
    colnames(qVal) = names
    
    Valid$pwr = power
    pwrVal = validation(Valid$pwr, Valid$Q)
    pwrVal = as.data.frame(t(pwrVal))
    colnames(pwrVal) = names
    
    
    rf = randomForest(Q ~width,data = Train, ntree= nrow(Train), mtry = 1)
    out = predict(rf, Valid)
    Valid$rf = out
    rfVal = validation(Valid$rf, Valid$Q)
    
    rfVal = as.data.frame(t(rfVal))
    colnames(rfVal) = names
    
    points(Valid$Date, Valid$rf, col = "blue")
    
    plsFit = try(train(Q ~width, 
                       data = Train,
                       method = "knn",
                       preProc=c("center", "scale"),
                       trControl = trainControl(method = "cv", number = c(5, 10, 20, 50, 100),search = "random")))
    
    preds <- predict(plsFit, newdata = Valid)
    dlVal = validation(preds, Valid$Q)
    dlVal = as.data.frame(t(dlVal))
    colnames(dlVal) = names
    
    points(Valid$Date, preds, col = "red")
    best = c(qVal$nse,pwrVal$nse, rfVal$nse, dlVal$nse)
    max(best, na.rm = TRUE)
    
    input = which(best==max(best, na.rm = TRUE))
    if(length(input) ==0){next}
    comb = list(qVal,pwrVal, rfVal, dlVal)
    out = as.data.frame(comb[input])
    #gage_stats$mode[i] = input
    
    rrmse = out$rrmse 
    r = out$r
    nse = out$nse
    kge = out$kge
    nrmse = out$nrmse
    rbias = out$rbias
    
    bestFit = c("Linear","Pwr", "RF", "KNN")
    legend1 = c(bestFit[input], paste0("NSE=",signif(nse, 3)),
                paste0("KGE=",signif(kge, 3)),paste0("rBias=",signif(rbias, 3)),paste0("NRMSE=",signif(nrmse, 3)),paste0("RRMSE=",signif(rrmse, 3)))
    legend("top", legend1, xpd = TRUE, bty = "n",  inset = c(0, -.3), horiz = TRUE, cex =0.75)
    
    Valid$dl = preds
    comb = cbind(Valid$model,Valid$pwr, Valid$rf, preds)
    comb = as.data.frame(comb)

    comb$true = Valid$Q

    error = comb[input] - comb$true
    #gage_stats$Site_number[i+(p-1)] = Site_number_xsections[i]
    
    gage_stats$RMSE[n] = sqrt(mean((error$V1^2), na.rm = TRUE))
    gage_stats$Bias[n] = mean(error$V1, na.rm = TRUE)
    gage_stats$RRMSE[n] = rrmse
    gage_stats$NSE[n] = nse
    gage_stats$KGE[n] = kge
    gage_stats$NRMSE[n] = nrmse
    gage_stats$rBias[n] = rbias
    #gage_stats$Site_number[i+(p-1)] = paired_df$ID[1]
    gage_stats$n_Landsat_obs[n] = nrow(paired_df_1)
    gage_stats$mode[n] = bestFit[input]
    
    print(n)
    
    
    plot(all$Date[order(all$Date)], all$q[order(all$Date)], type = "l", ylab = "Discharge (cms", xlab = "Year")
    pd = left_join(pd, all[is.na(all$q),])
    comb = cbind((pd$width*linear$coefficients[[2]])+linear$coefficients[[1]],(pd$width^log_linear$coefficients[[2]])*(exp(log_linear$coefficients[[1]])),predict(rf, pd), predict(plsFit, pd))
    comb = as.data.frame(comb)
    try(points(pd$Date, comb[,input], col = cols[input]))
    
    legend2 = c("Linear","Power", "Random Forest", "KNN")
    legend("top", legend2,pch = c(01, 01,01,01), col = c(cols), xpd = TRUE, bty = "n",  inset = c(0, -.3), horiz = TRUE, cex =.75)
    }  
  } else{next}
}
dev.off()
cmd = paste('open', pdfOut)
system(cmd)

original = gage_stats
fwrite(gage_stats, "E:/research/RatingCurveAnalysis/obs/lookup.csv")
########################################################################################################
##Producing gaps in the data, does it improve to make rc based on widths near gap or across time series.
########################################################################################################
lookup = gage_stats%>%select(Site_number, node_id, mode, NSE)
lookup = as.data.table(lookup)
keys = c("Site_number", "node_id")
setkeyv(lookup, keys)


gage_stats = as.data.frame(matrix(numeric(), nrow =50000, ncol = 24))
colnames(gage_stats)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "mode","Bias", "RRMSE","avg_std", "change", "std_Q","STDE", "KGE", "NSE", "rBias",
                        "SDRR", "MRR", "NRMSE", "Q_50", "W_50", "node_id",".geo")
cols = c("green","yellow", "blue", "red")
n = 0
'%!in%' <- function(x,y)!('%in%'(x,y))
profvis({
for(i in 1:length(Site_number_xsections)){
#for(i in 1:17){
  paired_df = fread(xsection_files[i])
  paired_df = paired_df[paired_df$width>0&paired_df$cloud<.1&paired_df$count==2&paired_df$max==0,]
  paired_df$Date = as.Date(as.POSIXct(paired_df$`system:time_start`/1000, origin = "1970-01-01"))
  paired_df$width = paired_df$width*paired_df$length
  nodes = unique(paired_df$node_id)
  paired_df = as.data.table(paired_df)
  setkey(paired_df, node_id)
  pd1 = paired_df
  usgs_q = try(read.table(grdc_files[i], stringsAsFactors = FALSE))
  if(!is.error(usgs_q)&nrow(usgs_q)>1){
    usgs_q$V1 = substr(usgs_q$V1, 0, 10)
    usgs_q$datetime = usgs_q$V1
    usgs_q$q = as.numeric(usgs_q$V2)
    usgs_q$Date = as.Date(usgs_q$datetime, format = "%Y-%m-%d")
    all = usgs_q
    usgs_q = usgs_q[usgs_q$q>0&usgs_q$Date>as.Date("1983-12-31", format ="%Y-%m-%d"),]
    usgs_q$Year = year(usgs_q$Date)
    paired_df = inner_join(paired_df, usgs_q)
    paired_df$Q = paired_df$q
    paired_df = paired_df[!is.na(paired_df$Q),]
    #########################################################################################################    
    nodes = unique(paired_df$node_id)
    if(length(nodes)==0){next}
    for(p in 1:length(nodes)){
        annual = aggregate(q~Year, usgs_q, max)
        # Sort data smallest to largest
        annualSorted = annual[order(annual$q),]
        # Count total obervations
        n1 = nrow(annualSorted)
        # Add a numbered column 1 -> n to use in return calculation for rank
        annualSorted$r = 1:n1
        annualSorted$probability = (n1-annualSorted$r+1)/(n1+1)
        annualSorted$return = 1/annualSorted$probability
        spl = try(approxfun(annualSorted$return, annualSorted$q))
        fiveYear = try(spl(5))
        #fiveYear = annualSorted$q[round(annualSorted$return)==5]
        reclass = try(na.omit(usgs_q[usgs_q$q>=fiveYear,]))
        if(nrow(reclass)>1&!is.error(spl)){
        reclass = aggregate(Date~Year, reclass, min)
        reclass = c(as.Date("1983-12-31"), reclass$Date, as.Date("2021-12-31"))
        for(r in 1:length(reclass)-1){
      outlist = list()
      n = n+1
      gage_stats$Site_number[n] = Site_number_xsections[i]
      gage_stats$node_id[n] = as.numeric(nodes[p])
      paired_df_1 = paired_df[.(nodes[p])]
      pd = pd1[.(nodes[p])]
      gage_stats$.geo[n] = paired_df_1$.geo[1]
      paired_df_1 = paired_df_1[paired_df_1$Date>reclass[r]&paired_df_1$Date<reclass[r+1],]
      #algorithm = lookup[lookup$Site_number==Site_number_xsections[i]&lookup$node_id==nodes[p],]
      #algorithm = na.omit(algorithm)
      #algorithm = algorithm$mode
      algorithm = lookup[.(Site_number_xsections[i], as.numeric(nodes[p]))]
      algorithm = na.omit(algorithm$mode)
      
      if(nrow(paired_df_1)<3|length(algorithm)==0){next}
      set.seed(1)
      trainIndex = createDataPartition(paired_df_1$Q, p = training,
                                        list = FALSE)
      
      Train = paired_df_1[ trainIndex,]
      Valid = paired_df_1[-trainIndex,]
      
      if(nrow(Valid)<3|nrow(Train)<3){next}
      
      if(algorithm=="Linear"){
        linear = lm(Train$Q~Train$width)
        out = (Valid$width*linear$coefficients[[2]])+linear$coefficients[[1]]
        
      }
      if(algorithm=="Pwr"){
      log_linear = lm(log(Train$Q)~log(Train$width))
       out = (Valid$width^log_linear$coefficients[[2]])*(exp(log_linear$coefficients[[1]]))
        
      }
      if(algorithm=="RF"){
        rf = randomForest(Q ~width,data = Train, ntree= nrow(Train), mtry = 1)
        out = predict(rf, Valid)
      }
      if(algorithm=="KNN"){
        plsFit = try(train(Q ~width, 
                           data = Train,
                           method = "knn",
                           preProc=c("center", "scale"),
                           trControl = trainControl(method = "cv", number = c(5, 10, 20, 50, 100),search = "random")))
        out <- predict(plsFit, newdata = Valid)
      }
      Valid$model = out
      outlist[[r]] = Valid
      break
        }
      if(nrow(rbindlist(outlist))<1){next}
      model = rbindlist(outlist)
      performance = validation(model$model, Valid$Q)
      performance = as.data.frame(t(performance))
      names = c("rrmse", "nse", "kge", "nrmse", "rbias")
      colnames(performance) = names
      error = model$model-model$q
      #gage_stats$Site_number[i+(p-1)] = Site_number_xsections[i]
      gage_stats$RMSE[n] = sqrt(mean((error^2), na.rm = TRUE))
      gage_stats$Bias[n] = mean(error, na.rm = TRUE)
      gage_stats$RRMSE[n] = performance$rrmse
      gage_stats$NSE[n] = performance$nse
      gage_stats$KGE[n] = performance$kge
      gage_stats$NRMSE[n] = performance$nrmse
      gage_stats$rBias[n] = performance$rbias
      #gage_stats$Site_number[i+(p-1)] = paired_df$ID[1]
      gage_stats$n_Landsat_obs[n] = nrow(paired_df_1)
      gage_stats$mode[n] = algorithm
      print(n)
     }
    }
  } else{next}
}
})
ryan = merge(lookup[!is.na(lookup$Site_number),],gage_stats[!is.na(gage_stats$Site_number),],by =c("Site_number", "node_id"))
ryan$diff = ryan$NSE.y-ryan$NSE.x
plot(ryan$NSE.x, ryan$NSE.y, xlim =c(-1,1), ylim = c(-1,1), xlab = "1 curve", ylab = "multi-curve")
abline(0,1)

ryan = as.data.table(ryan)
ryanX = ryan[ryan[,.I[which.max(NSE.x)], by=Site_number]$V1]
ryanY = ryan[ryan[,.I[which.max(NSE.y)], by=Site_number]$V1]




gages = merge(ryanX, ryanY, by = c("Site_number"))
plot(gages$NSE.x.x, gages$NSE.y.y, xlim = c(-1,1), ylim = c(-1,1), col = gages$mode.x.x)
abline(0,1)

diff = gages$NSE.y.y - gages$NSE.x.x
length(diff[diff>0])






ggplot(data = gages, aes(x = NSE.x.x, y = NSE.y.y))+geom_point(aes(colour = mode.x.x))
################################################################################################
##At what point does recurrence interval effect performance?
################################################################################################
gage_stats = as.data.frame(matrix(numeric(), nrow =500000, ncol = 24))
colnames(gage_stats)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "mode","Bias", "RRMSE","avg_std", "change", "std_Q","STDE", "KGE", "NSE", "rBias",
                        "SDRR", "recurrence", "NRMSE", "Q_50", "W_50", "node_id",".geo")
cols = c("green","yellow", "blue", "red")
n = 0
'%!in%' <- function(x,y)!('%in%'(x,y))
recurrence = c(2:20)
for(i in 1:length(Site_number_xsections)){
    #for(i in 1:17){
    paired_df = fread(xsection_files[i])
    paired_df = paired_df[paired_df$width>0&paired_df$cloud<.1&paired_df$count==2&paired_df$max==0,]
    paired_df$Date = as.Date(as.POSIXct(paired_df$`system:time_start`/1000, origin = "1970-01-01"))
    paired_df$width = paired_df$width*paired_df$length
    nodes = unique(paired_df$node_id)
    paired_df = as.data.table(paired_df)
    setkey(paired_df, node_id)
    pd1 = paired_df
    usgs_q = try(read.table(grdc_files[i], stringsAsFactors = FALSE))
    if(!is.error(usgs_q)&nrow(usgs_q)>1){
      usgs_q$V1 = substr(usgs_q$V1, 0, 10)
      usgs_q$datetime = usgs_q$V1
      usgs_q$q = as.numeric(usgs_q$V2)
      usgs_q$Date = as.Date(usgs_q$datetime, format = "%Y-%m-%d")
      all = usgs_q
      usgs_q = usgs_q[usgs_q$q>0&usgs_q$Date>as.Date("1983-12-31", format ="%Y-%m-%d"),]
      usgs_q$Year = year(usgs_q$Date)
      paired_df = inner_join(paired_df, usgs_q)
      paired_df$Q = paired_df$q
      paired_df = paired_df[!is.na(paired_df$Q),]
      #########################################################################################################    
      nodes = unique(paired_df$node_id)
      if(length(nodes)==0){next}
      for(p in 1:length(nodes)){
        annual = aggregate(q~Year, usgs_q, max)
        # Sort data smallest to largest
        annualSorted = annual[order(annual$q),]
        # Count total obervations
        n1 = nrow(annualSorted)
        # Add a numbered column 1 -> n to use in return calculation for rank
        annualSorted$r = 1:n1
        annualSorted$probability = (n1-annualSorted$r+1)/(n1+1)
        annualSorted$return = 1/annualSorted$probability
        spl = try(approxfun(annualSorted$return, annualSorted$q))
        for(y in 1:length(recurrence)){
        fiveYear = try(spl(recurrence[y]))
        #fiveYear = annualSorted$q[round(annualSorted$return)==5]
        reclass = try(na.omit(usgs_q[usgs_q$q>=fiveYear,]))
        if(nrow(reclass)>1&!is.error(spl)){
          reclass = aggregate(Date~Year, reclass, min)
          reclass = c(as.Date("1983-12-31"), reclass$Date, as.Date("2021-12-31"))
          for(r in 1:length(reclass)-1){
            n = n+1
            gage_stats$.geo[n] = paired_df_1$.geo[1]
            gage_stats$Site_number[n] = Site_number_xsections[i]
            gage_stats$node_id[n] = as.numeric(nodes[p])
            outlist = list()
            paired_df_1 = paired_df[.(nodes[p])]
            pd = pd1[.(nodes[p])]
            paired_df_1 = paired_df_1[paired_df_1$Date>reclass[r]&paired_df_1$Date<reclass[r+1],]
            #algorithm = lookup[lookup$Site_number==Site_number_xsections[i]&lookup$node_id==nodes[p],]
            #algorithm = na.omit(algorithm)
            #algorithm = algorithm$mode
            algorithm = lookup[.(Site_number_xsections[i], as.numeric(nodes[p]))]
            algorithm = na.omit(algorithm$mode)
            
            if(nrow(paired_df_1)<3|length(algorithm)==0){next}
            set.seed(1)
            trainIndex = createDataPartition(paired_df_1$Q, p = training,
                                             list = FALSE)
            
            Train = paired_df_1[ trainIndex,]
            Valid = paired_df_1[-trainIndex,]
            
            if(nrow(Valid)<3|nrow(Train)<3){next}
            
            if(algorithm=="Linear"){
              linear = lm(Train$Q~Train$width)
              out = (Valid$width*linear$coefficients[[2]])+linear$coefficients[[1]]
              
            }
            if(algorithm=="Pwr"){
              log_linear = lm(log(Train$Q)~log(Train$width))
              out = (Valid$width^log_linear$coefficients[[2]])*(exp(log_linear$coefficients[[1]]))
              
            }
            if(algorithm=="RF"){
              rf = randomForest(Q ~width,data = Train, ntree= nrow(Train), mtry = 1)
              out = predict(rf, Valid)
            }
            if(algorithm=="KNN"){
              plsFit = try(train(Q ~width, 
                                 data = Train,
                                 method = "knn",
                                 preProc=c("center", "scale"),
                                 trControl = trainControl(method = "cv", number = c(5, 10, 20, 50, 100),search = "random")))
              out <- predict(plsFit, newdata = Valid)
            }
            Valid$model = out
            outlist[[r]] = Valid
          }
          if(nrow(rbindlist(outlist))<1){next}
          
          model = rbindlist(outlist)
          performance = validation(model$model, Valid$Q)
          performance = as.data.frame(t(performance))
          names = c("rrmse", "nse", "kge", "nrmse", "rbias")
          colnames(performance) = names
          error = model$model-model$q
          #gage_stats$Site_number[i+(p-1)] = Site_number_xsections[i]
          gage_stats$RMSE[n] = sqrt(mean((error^2), na.rm = TRUE))
          gage_stats$Bias[n] = mean(error, na.rm = TRUE)
          gage_stats$RRMSE[n] = performance$rrmse
          gage_stats$NSE[n] = performance$nse
          gage_stats$KGE[n] = performance$kge
          gage_stats$NRMSE[n] = performance$nrmse
          gage_stats$rBias[n] = performance$rbias
          #gage_stats$Site_number[i+(p-1)] = paired_df$ID[1]
          gage_stats$n_Landsat_obs[n] = nrow(paired_df_1)
          gage_stats$mode[n] = algorithm
          gage_stats$recurrence[n] = recurrence[y]
          print(n)
        }
        }
      }
    } else{next}
  }

ryan = merge(lookup[!is.na(lookup$NSE),],gage_stats[!is.na(gage_stats$NSE),],by =c("Site_number", "node_id"))
ryan$diff = ryan$NSE.y-ryan$NSE.x
plot(ryan$diff, ylim = c(-1,1))
plot(ryan$NSE.x, ryan$NSE.y, xlim =c(-1,1), ylim = c(-1,1), xlab = "1 curve", ylab = "multi-curve")
abline(0,1)

##Filter to highest performing recurrence interval for each site number and node. 
ryan = as.data.table(ryan)
ryanY = ryan[ryan[,.I[which.max(NSE.y)], by=list(Site_number, node_id)]$V1]
median(ryanY$diff)

##Filter to highest performing recurrence interval for each gauge only. 
gage_highestPerformanceSingle = ryan[ryan[,.I[which.max(NSE.x)], by=Site_number]$V1]
gage_highestPerformanceSingle$single = gage_highestPerformanceSingle$NSE.x

gage_highestPerformanceMulti =  ryan[ryan[,.I[which.max(NSE.y)], by=Site_number]$V1]
gage_highestPerformanceMulti$multi = gage_highestPerformanceMulti$NSE.y

gage_combined = merge(gage_highestPerformanceSingle, gage_highestPerformanceMulti, by = "Site_number")
gage_combined$diff = gage_combined$multi-gage_combined$single
plot(gage_combined$single, gage_combined$multi, xlim = c(-1,1), ylim = c(-1,1))
abline(0,1)
nrow(gage_combined[gage_combined$diff>0,])/nrow(gage_combined)
nrow(gage_combined[gage_combined$mode.x.x=="Pwr"|gage_combined$mode.x.x=="Linear",])/nrow(gage_combined)

traditional = gage_combined[gage_combined$mode.x.x=="Pwr"|gage_combined$mode.x.x=="Linear",]
ml = gage_combined[gage_combined$mode.x.x=="KNN"|gage_combined$mode.x.x=="RF",]

trad1 = gage_combined[gage_combined$mode.y.y=="Pwr"|gage_combined$mode.y.y=="Linear",]
ml1 = gage_combined[gage_combined$mode.y.y=="KNN"|gage_combined$mode.y.y=="RF",]

##Assign true highest values and if it needs to be evolved or not. 
gage_combined$NSE = ifelse(gage_combined$diff>0, gage_combined$multi, gage_combined$single)
gage_combined$recurrence = ifelse(gage_combined$diff>0, gage_combined$recurrence.y, 0)
gage_combined$mode = ifelse(gage_combined$diff>0, gage_combined$mode.y.y, gage_combined$mode.x.x)


plot(gage_combined$recurrence, gage_combined$NSE)
median(gage_combined$NSE)




df = gage_combined[!is.na(gage_combined$NSE),]
gage_file = st_read("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\combined_andIndia.shp")
gage_file = gage_file[gage_file$Sttn_Nm%in%paste0(df$Site_number, "_grdc"),]
df$Sttn_Nm = paste0(df$Site_number, "_grdc")
gage_stats_vals = merge(gage_file, df,by = 'Sttn_Nm')

library(ggplot2)
library(tmap)

tmap_mode("view")

tm_shape(gage_stats_vals)+
  tm_bubbles(col = "NSE",size = 0.2, breaks = c(-1,0,.25,.5,.75, 1))

tm_shape(gage_stats_vals)+
  tm_bubbles(col = "recurrence",size = 0.2, breaks = c(0,1,2,5,10,20))

tm_shape(gage_stats_vals)+
  tm_bubbles(col = "mode",size = 0.2)











#########################################################################################
##Add in gauge results. 
############################################################################################

df = gage_stats[!is.na(gage_stats$NSE),]
gage_stats = df
gage_stats = as.data.table(gage_stats)
group = gage_stats[gage_stats[,.I[which.max(NSE)], by=Site_number]$V1]
gage_file = st_read("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\combined_andIndia.shp")
gage_file = gage_file[gage_file$Sttn_Nm%in%paste0(group$Site_number, "_grdc"),]
group$Sttn_Nm = paste0(group$Site_number, "_grdc")
gage_stats_vals = merge(gage_file, group,by = 'Sttn_Nm')

library(ggplot2)
library(tmap)

tmap_mode("view")

tm_shape(gage_stats_vals)+
  tm_bubbles(col = "NSE",size = 0.2, breaks = c(-1,0,.25,.5,.75, 1))

tm_shape(gage_stats_vals)+
  tm_bubbles(col = "mode",size = 0.2)

tm_shape(gage_stats_vals)+
  tm_bubbles(col = "n_Landsat_obs",size = 0.2, breaks = c(10,20,30,50,100,500,1000))








###############################################################################
##add in cross section results. 
###############################################################################
df = gage_stats
library(strex)
d = unlist(df$.geo)
sp = d
sp1 = str_sub(sp, 43, nchar(sp))
sp2 = str_before_nth(sp1, ",", 2)
sp3 = gsub("[", "", sp2, fixed = TRUE)
sp4 = gsub("]", "", sp3, fixed = TRUE)
x = str_before_first(sp4,",")
y = str_after_last(sp4,",")

long = as.numeric(x)
lat = as.numeric(y)

df$long = long
df$lat = lat


df = as.data.frame(df)


sp = st_as_sf(df, coords = c("long", "lat"))

tmap_mode("view")

tm_shape(sp)+
  tm_bubbles(col = "NSE",size = 0.0000002, breaks = c(-1,-.5, 0,.5, 1))


tm_shape(sp)+
  tm_bubbles(col = "mode",size = 0.0000002)












sp = as.data.frame(gage_stats)

t = SpatialLinesDataFrame(gage_stats)


library(dplyr)

f <- function(.x) 
  if (is.na(.x) || .x == "") data.frame()[1, ] else 
    as.data.frame(jsonlite::fromJSON(.x))

spatial  = gage_stats %>% 
  tidyr::unnest(UnloadVars = lapply(UnloadVars, f)) %>% 
  mutate_at(vars(ends_with("type")), as.character)




















gage_file = foreign::read.dbf("E:/research/SWORD/shp/AllxSections3.dbf")
gage_file = gage_file[gage_file$node_id%in%gage_stats$node_id,]
gage_stats_vals = merge(gage_file, gage_stats,by = 'node_id')

library(ggplot2)
library(tmap)

tmap_mode("view")

tm_shape(gage_stats_vals)+
  tm_bubbles(col = "NSE",size = 0.2, breaks = c(-1,-.5, 0,.5, 1))
































########################################################################################################################
##Use best output for each gauge location. GRDc.  
########################################################################################################################
grdc_path = "E:\\research\\RatingCurveAnalysis\\GaugeLocations\\DischargeDatasets\\GRDC\\"
grdc_files = gsub("_grdc", "_Q_Day.Cmd.txt", Site_number_xsections)
grdc_files = paste0(grdc_path, grdc_files)



random = function(x){
  x = na.omit(x)
  v = runif(1000, x-90, x+90)
  return(v)
}


start = as.Date("1979-01-01")
data_val = Eff_widths
RC_End = as.Date("2014-12-31") ###WAs 2014
RC_year = format(RC_End, "%Y")
RC_year_1 = format(RC_End+1, "%Y")
data_val$ID = data_val$ID
data_val$ID_2 = data_val$ID
data_val$calc_mean = data_val$Effective_width

#data_val$Date = as.character(data_val$Date)
tab$ID = tab$id
tab$ID_2 = tab$id
tab$width_m = data_val$width_m[match(tab$ID, data_val$ID)]
#tab$change= tab$median
#tab$change[mapply(is.na, tab$change)] <- 0
tab$width_m = data$width_m[match(tab$ID_2, data$ID_2)]
xSecq=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecw=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecIDcol=grep("V", names(Site_number_xsections))
mInd = array(5, dimnames = NULL)
rangedf_1 = as.data.frame(matrix(numeric(), nrow = 1, ncol = 4))
gage_stats = as.data.frame(matrix(numeric(), nrow =length(Site_number_xsections), ncol = 23))
gage_stats_GRADES = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 14))
colnames(gage_stats_GRADES)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "p_val","Bias", "RRMSE", "avg_std", "change", 'RRMSE_median', "std_Q", "STDE", "mode")
as.data.frame(gage_stats_GRADES)
l_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
Mean_grades = as.vector(nrow(Site_number_xsections))
u_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
sd_vals = as.data.frame(matrix(numeric(), nrow =length(Site_number_xsections), ncol = 20))
sd_vals_1 = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
width_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
gage_quants_q = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 100))
gage_quants_w = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 100))
colnames(gage_stats)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "mode","Bias", "RRMSE","avg_std", "change", "RRMSE_median", "std_Q","STDE", "KGE", "NSE", "rBias",
                        "SDRR", "MRR", "NRMSE", "Q_50", "W_50")
as.data.frame(gage_stats)
gage_stats_col1 = as.vector(1)
gage_stats_col2 = as.vector(1)
gage_stats_GRADES_col1 = as.vector(1)
gage_stats_GRADES_col2 = as.vector(1)
paired_df_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
rmse = 1
width_grouping = 30
percentiles = c(0.05, 0.95)
training = 0.7
data_val = as.data.table(data_val)
setkey(data_val, ID)
cols = c("green", "blue", "red")

library(profvis)
library(caret)
library(dataRetrieval)

pdfOut = "E:/research/temp_plots/GRDC_RC_hydrographs.pdf"
pdf(pdfOut)
#profvis({
for(i in 1:length(Site_number_xsections)){
  id = Site_number_xsections[i]
  paired_df = data_val[.(Site_number_xsections[i])]
  pd = paired_df
  usgs_q = try(read.table(grdc_files[i], stringsAsFactors = FALSE))
  if(!is.error(usgs_q)){
    usgs_q$V1 = substr(usgs_q$V1, 0, 10)
    usgs_q$datetime = usgs_q$V1
    usgs_q$q = as.numeric(usgs_q$V2)
    usgs_q$Date = as.Date(usgs_q$datetime, format = "%Y-%m-%d")
    usgs_q = usgs_q[usgs_q$q>=0&usgs_q$Date>as.Date("1983-12-31", format ="%Y-%m-%d"),]
   
    
    paired_df = inner_join(paired_df, usgs_q)
    paired_df$Q = paired_df$q
    paired_df = inner_join(paired_df, usgs_q)
    #paired_df = distinct(paired_df, Date, .keep_all = TRUE)
    #########################################################################################################    
    paired_df$Q = paired_df$q  
    paired_df = paired_df[!is.na(paired_df$Q),]
    
    if(nrow(paired_df)<3){next}
    set.seed(1)
    trainIndex = createDataPartition(paired_df$Q, p = training,
                                     list = FALSE)
    
    Train = paired_df[ trainIndex,]
    Valid = paired_df[-trainIndex,]
    # years = unique(year(paired_df$Date))
    # train = round(length(years)*.6)
    # paired_df$Year = year(paired_df$Date)
    # set.seed(5)
    # t = sample(years, train)
    # TrainingSet = paired_df$Year%in%t
    # Train = paired_df[TrainingSet,]
    # Valid = paired_df[!TrainingSet,]
    # Train = paired_df[paired_df$Date>=as.Date(as.character('2015-01-01', format = "%Y-%m-%d")),]
    # Valid = paired_df[paired_df$Date<as.Date(as.character('2015-01-01', format = "%Y-%m-%d")),]
    
    if(nrow(Valid)<3|nrow(Train)<3){next}
    
    y = quantile(Train$Q, probs = seq(percentiles[1],percentiles[2],.01), na.rm = TRUE)
    x = quantile(Train$calc_mean, probs = seq(percentiles[1],percentiles[2],.01), na.rm = TRUE)
    if(length(unique(x))>1){
      ###Create function to estimate Q from a Landsat width based on quantile pair up. 
      spl = approxfun(x, y) #, method = "hyman")) #####either usse 'approxfun' or use splinefun with method = "hyman"
    } else{next}
    
    par(mfrow = c(2,1))
    mn = as.Date("1984-01-01")
    mx = as.Date("2021-12-31")
    all = seq.Date(mn, mx, 1)
    all = as.data.frame(all)
    colnames(all) = "Date"
    all = full_join(usgs_q, all)
    plot(all$Date[order(all$Date)], all$q[order(all$Date)], type = "l", ylab = "Discharge (cms)", xlab = "Year")
    points(Train$Date, Train$Q, col = "purple", pch = 19)
    #plot(paired_df$Date, paired_df$q, type = "l")
    points(Valid$Date, spl(Valid$calc_mean), col = "green")
    
    names = c("rrmse", "nse", "kge", "nrmse", "rbias")
    
    ##Quantile validation 
    Valid$model = spl(Valid$calc_mean)
    qVal = validation(Valid$model, Valid$Q)
    qVal = as.data.frame(t(qVal))
    colnames(qVal) = names
    
    
    rf = randomForest(Q ~calc_mean+Date,data = Train, ntree= nrow(Train), mtry = 1)
    out = predict(rf, Valid)
    Valid$rf = out
    rfVal = validation(Valid$rf, Valid$Q)
    
    rfVal = as.data.frame(t(rfVal))
    colnames(rfVal) = names
    
    points(Valid$Date, Valid$rf, col = "blue")
    
    plsFit = try(train(Q ~calc_mean+Date, 
                       data = Train,
                       method = "knn",
                       preProc=c("center", "scale"),
                       trControl = trainControl(method = "cv", number = c(5, 10, 20, 50, 100),search = "random")))
    
    preds <- predict(plsFit, newdata = Valid)
    dlVal = validation(preds, Valid$Q)
    dlVal = as.data.frame(t(dlVal))
    colnames(dlVal) = names
    
    points(Valid$Date, preds, col = "red")
    best = c(qVal$nse, rfVal$nse, dlVal$nse)
    max(best, na.rm = TRUE)
    
    input = which(best==max(best, na.rm = TRUE))
    if(length(input) ==0){next}
    comb = list(qVal, rfVal, dlVal)
    out = as.data.frame(comb[input])
    gage_stats$mode[i] = input
    
    rrmse = out$rrmse 
    r = out$r
    nse = out$nse
    kge = out$kge
    nrmse = out$nrmse
    rbias = out$rbias
    
    bestFit = c("QNT", "RF", "KNN")
    legend1 = c(bestFit[input], paste0("NSE=",signif(nse, 3)),
                paste0("KGE=",signif(kge, 3)),paste0("rBias=",signif(rbias, 3)),paste0("NRMSE=",signif(nrmse, 3)),paste0("RRMSE=",signif(rrmse, 3)))
    legend("top", legend1, xpd = TRUE, bty = "n",  inset = c(0, -.3), horiz = TRUE, cex =0.75)
    
    Valid$dl = preds
    comb = cbind(Valid$model, Valid$rf, preds)
    comb = as.data.frame(comb)
    sdVals = apply(comb, 1, sd, na.rm = TRUE)
    
    comb$true = Valid$Q
    comb$sd = sdVals

    error = comb[input] - comb$true
    gage_stats$RMSE[i] = sqrt(mean((error$V1^2), na.rm = TRUE))
    gage_stats$Bias[i] = mean(error$V1, na.rm = TRUE)
    gage_stats$RRMSE[i] = rrmse
    gage_stats$NSE[i] = nse
    gage_stats$KGE[i] = kge
    gage_stats$NRMSE[i] = nrmse
    gage_stats$rBias[i] = rbias
    gage_stats$Site_number[i] = paired_df$ID[1]
    gage_stats$n_Landsat_obs[i] = nrow(paired_df)
    print(i)
    
    
    plot(all$Date[order(all$Date)], all$q[order(all$Date)], type = "l", ylab = "Discharge (cms", xlab = "Year")
    pd = left_join(pd, all[is.na(all$q),])
    comb = cbind(spl(pd$calc_mean),predict(rf, pd), predict(plsFit, pd))
    comb = as.data.frame(comb)
    random_df = sapply(pd$calc_mean,random)

    ##This is taking forever. 
    sdVals = as.vector(nrow(pd))
    for(j in 1:ncol(random_df)){
      dts = pd$Date[j]
      widths = random_df[,j]
      df = cbind(dts, unlist(widths))
      df = as.data.frame(df)
      colnames(df) = c("Date", "calc_mean")
      df$Date = as.Date(df$Date)
      df1 = cbind(spl(df$calc_mean),predict(rf, df), predict(plsFit, df))
      df1 = df1[,input]
      min = min(df1, na.rm = TRUE)
      max = max(df1, na.rm = TRUE)
      sdVals[j] = max-min
    }
    
    sd_vals[i, 1:length(sdVals)] = sdVals
    arrows(pd$Date, comb[,input] - sdVals, pd$Date, comb[,input]+sdVals,col = "indianred",
           code=3, angle = 180, length = 0, lwd = 0.5)
    points(pd$Date, comb[,input], col = cols[input])
    legend2 = c("Training", "Quantile", "Random Forest", "KNN")
    legend("top", legend2,pch = c(19, 01,01,01), col = c("purple", cols), xpd = TRUE, bty = "n",  inset = c(0, -.3), horiz = TRUE, cex =.75)
    
  } else{next}
}
dev.off()
cmd = paste('open', pdfOut)
system(cmd)
biasE = apply(sd_vals, 1, mean, na.rm = TRUE)
rm = function(x){
  rmse = sqrt(mean(na.omit(x)^2))
  return(rmse)
}
rmseE = apply(sd_vals, 1, rm)
plot(abs(gage_stats$Bias), abs(biasE), xlim = c(1, 1000), ylim = c(1,1000), log = "xy")

plot(gage_stats$RMSE, rmseE, xlim = c(1, 10000), ylim = c(1,10000), log = "xy")





##interpolation ranges. 
gaps = as.data.frame(matrix(numeric(), nrow =length(Site_number_xsections), ncol = 20))
gap_dates = as.data.frame(matrix(numeric(), nrow =length(Site_number_xsections), ncol = 20))
for(i in 1:length(Site_number_xsections)){
  usgs_q = try(read.table(grdc_files[i], stringsAsFactors = FALSE))
  if(!is.error(usgs_q)){
    usgs_q$V1 = substr(usgs_q$V1, 0, 10)
    usgs_q$datetime = usgs_q$V1
    usgs_q$q = as.numeric(usgs_q$V2)
    usgs_q$Date = as.Date(usgs_q$datetime, format = "%Y-%m-%d")
    usgs_q = usgs_q[usgs_q$q>=0&usgs_q$Date>as.Date("1983-12-31", format ="%Y-%m-%d"),]
    usgs_q$gap <- c(NA, with(usgs_q, usgs_q$Date[-1] -usgs_q$Date[-nrow(usgs_q)]))
    gp = usgs_q[usgs_q$gap>1,]
    gaps[i,1:nrow(gp)] = gp$gap
    gap_dates[i,1:nrow(gp)] = gp$Date
    
    print(i)
    }
}



mean(unlist(gaps), na.rm = TRUE)
median(unlist(gaps), na.rm = TRUE)

gaps_sites = apply(gaps, 1, mean, na.rm = TRUE)
plot(unlist(gap_dates), unlist(gaps), type = "l")

library(RColorBrewer)
n = nrow(gaps)
pal = rainbow(n)

plot(as.Date(unlist(gap_dates[12,])),gaps[12,])

st = as.Date("1984-01-01")
en = as.Date("2021-01-01")
plot(0,0, xlim = c(st,en), ylim = c(0,2000))
for(i in 1:nrow(gaps)){
lines(as.Date(unlist(gap_dates[i,])),unlist(gaps[i,]), col=pal[i])
}




a = seq(1,10, 1)

a[5] = NA

plot(a, type = "l")

Date= seq.Date(mn, mx, 1)
discharge = as.data.frame(matrix(numeric(), nrow =length(Site_number_xsections), ncol = length(Date)))
all = as.data.frame(Date)
for(i in 1:length(Site_number_xsections)){
usgs_q = try(read.table(grdc_files[i], stringsAsFactors = FALSE))
if(!is.error(usgs_q)){
  usgs_q$V1 = substr(usgs_q$V1, 0, 10)
  usgs_q$datetime = usgs_q$V1
  usgs_q$q = as.numeric(usgs_q$V2)
  usgs_q$Date = as.Date(usgs_q$datetime, format = "%Y-%m-%d")
  usgs_q = usgs_q[usgs_q$q>=0&usgs_q$Date>as.Date("1983-12-31", format ="%Y-%m-%d"),]
  usgs_q = full_join(all, usgs_q)
  discharge[i,] = usgs_q$q
}
}
colnames(discharge) = Date
discharge$Sttn_Nm = Site_number_xsections

t = melt(discharge, id.vars = "Sttn_Nm")



fwrite(t, "E:\\research\\RatingCurveAnalysis\\GaugeLocations\\DischargeDatasets\\grdc_gee1.csv")


################################################################################################################################
##India gauges. 
################################################################################################################################
########################################################################################################################
India_discharge = list.files("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\DischargeDatasets\\India\\", full.names = TRUE)
t = readxl::read_xlsx(India_discharge[1])
years = c(1984:2020)
sites_file = fread("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\India_sameOrderasQfiles.csv")
sites = as.data.frame(Site_number_xsections)
# t[1,]
# year = seq.Date(as.Date('1984-01-01'), as.Date('1984-12-31'), 1)
# levelCols= grep("Level", t[2,])
# t = t%>%select(-levelCols)
# flowCols = grep("Flow", t[2,])
# df = t[,flowCols]
# df = df[3:nrow(df),]
# colnames(df) = year
# df = as.data.frame(df)
# df = sapply(df, as.numeric)
# df$ID = sites_file$Sttn_Nm
# df = melt(df, id.vars = "ID", measure.vars = year)
# df$Date = rownames(df)
# df$q = as.numeric(df$V1)

# for(i in 1:length(Site_number_xsections)){
# outputs_list = list()
# for(r in 1:length(India_discharge)){
#   t = readxl::read_xlsx(India_discharge[r])
#   year = seq.Date(as.Date(paste0(years[r],'-01-01')), as.Date(paste0(years[r],'-12-31')), 1)
#   levelCols= grep("Level", t[2,])
#   t = t%>%select(-levelCols)
#   flowCols = grep("Flow", t[2,])
#   rw = 2+i
#   df = t[rw,flowCols]
#   colnames(df) = year
#   df = as.data.frame(df)
#   df = sapply(df, as.numeric)
#   df = melt(df,measure.vars = year)
#   df$Date = rownames(df)
#   df$q = as.numeric(df$value)
#   df = df[!is.na(df$value),]
#   if(nrow(df)>0){
#   outputs_list[[r]] = df
#   }
#   else{next}
#   print(r)
# }
# out = rbindlist(outputs_list)
# out = out[!is.na(out$q),]
# if(nrow(out)>0){
# fwrite(out, paste0("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\DischargeDatasets\\India\\Processed\\",Site_number_xsections[i], ".csv"))
# print(i)
# }
# else{next}
# }
# 
# 
# 

India = "E:\\research\\RatingCurveAnalysis\\GaugeLocations\\DischargeDatasets\\India\\Processed\\"





# t$station_id = gsim_all$gsim.no[match(t$`River Point Level & Flow Report (ALL AGENCIES) From 19840101 to 19841231`, gsim_all$station)]
# 

# colnames(India) = c("River", "Lat", "Long")
# India = India[3:nrow(India),]
# India$Sttn_Nm = paste0(1:nrow(India), "_In")
# 





start = as.Date("1979-01-01")
data_val = Eff_widths
RC_End = as.Date("2014-12-31") ###WAs 2014
RC_year = format(RC_End, "%Y")
RC_year_1 = format(RC_End+1, "%Y")
data_val$ID = data_val$ID
data_val$ID_2 = data_val$ID
data_val$calc_mean = data_val$Effective_width

#data_val$Date = as.character(data_val$Date)
tab$ID = tab$id
tab$ID_2 = tab$id
tab$width_m = data_val$width_m[match(tab$ID, data_val$ID)]
#tab$change= tab$median
#tab$change[mapply(is.na, tab$change)] <- 0
tab$width_m = data$width_m[match(tab$ID_2, data$ID_2)]
xSecq=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecw=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecIDcol=grep("V", names(Site_number_xsections))
mInd = array(5, dimnames = NULL)
rangedf_1 = as.data.frame(matrix(numeric(), nrow = 1, ncol = 4))
gage_stats = as.data.frame(matrix(numeric(), nrow =length(Site_number_xsections), ncol = 23))
gage_stats_GRADES = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 14))
colnames(gage_stats_GRADES)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "p_val","Bias", "RRMSE", "avg_std", "change", 'RRMSE_median', "std_Q", "STDE", "mode")
as.data.frame(gage_stats_GRADES)
l_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
Mean_grades = as.vector(nrow(Site_number_xsections))
u_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
sd_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
sd_vals_1 = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
width_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
gage_quants_q = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 100))
gage_quants_w = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 100))
colnames(gage_stats)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "mode","Bias", "RRMSE","avg_std", "change", "RRMSE_median", "std_Q","STDE", "KGE", "NSE", "rBias",
                        "SDRR", "MRR", "NRMSE", "Q_50", "W_50")
as.data.frame(gage_stats)
gage_stats_col1 = as.vector(1)
gage_stats_col2 = as.vector(1)
gage_stats_GRADES_col1 = as.vector(1)
gage_stats_GRADES_col2 = as.vector(1)
paired_df_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
rmse = 1
width_grouping = 30
percentiles = c(0.05, 0.95)
training = 0.7
data_val = as.data.table(data_val)
setkey(data_val, ID)

library(profvis)
library(caret)
library(dataRetrieval)
#profvis({
for(i in 1:length(Site_number_xsections)){
  id = Site_number_xsections[i]
  paired_df = data_val[.(Site_number_xsections[i])]
  
  usgs_q = try(fread(paste0(India, sites$Site_number_xsections[i], ".csv")))
    if(!is.error(usgs_q)){
    # usgs_q$V1 = substr(usgs_q$V1, 0, 10)
    # usgs_q$datetime = usgs_q$V1
    # usgs_q$q = as.numeric(usgs_q$V2)
    # usgs_q$Date = as.Date(usgs_q$datetime, format = "%Y-%m-%d")
    usgs_q = usgs_q[usgs_q$q>=0,]
    usgs_q$Date = as.Date(usgs_q$Date)
    paired_df = inner_join(paired_df, usgs_q)
    paired_df$Q = paired_df$q
    
    paired_df = inner_join(paired_df, usgs_q)
    #paired_df = distinct(paired_df, Date, .keep_all = TRUE)
    #########################################################################################################    
    paired_df$Q = paired_df$q  
    paired_df = paired_df[!is.na(paired_df$Q),]
    
    if(nrow(paired_df)<3){next}
    set.seed(1)
    trainIndex = createDataPartition(paired_df$Q, p = training,
                                     list = FALSE)
    Train = paired_df[ trainIndex,]
    Valid = paired_df[-trainIndex,]
    
    if(nrow(Valid)<3){next}
    
    y = quantile(Train$Q, probs = seq(percentiles[1],percentiles[2],.01), na.rm = TRUE)
    x = quantile(Train$calc_mean, probs = seq(percentiles[1],percentiles[2],.01), na.rm = TRUE)
    if(length(unique(x))>1){
      ###Create function to estimate Q from a Landsat width based on quantile pair up. 
      spl = approxfun(x, y) #, method = "hyman")) #####either usse 'approxfun' or use splinefun with method = "hyman"
    } else{next}
    
    
    par(pty = "s")
    plot(c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)),c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)), type = "n", xlab = "In situ Discharge (cms)", ylab = "Landsat Discharge (cms)", main = "In situ vs Landsat")
    points(Valid$Q, spl(Valid$calc_mean), col = "blue")
    abline(0,1)
    
    names = c("rrmse", "nse", "kge", "nrmse", "rbias")
    
    ##Quantile validation 
    Valid$model = spl(Valid$calc_mean)
    qVal = validation(Valid$model, Valid$Q)
    qVal = as.data.frame(t(qVal))
    colnames(qVal) = names
    
    
    rf = randomForest(Q ~calc_mean,data = Train, ntree= 5000, mtry = 1)
    out = predict(rf, Valid)
    Valid$rf = out
    rfVal = validation(Valid$rf, Valid$Q)
    
    rfVal = as.data.frame(t(rfVal))
    colnames(rfVal) = names
    
    points(Valid$Q, Valid$rf, pch = 19)
    
    plsFit = try(train(Q ~calc_mean, 
                       data = Train,
                       method = "knn",
                       preProc=c("center", "scale"),
                       trControl = trainControl(method = "cv", number = 10, search = "random")))
    
    preds <- predict(plsFit, newdata = Valid)
    dlVal = validation(preds, Valid$Q)
    dlVal = as.data.frame(t(dlVal))
    colnames(dlVal) = names
    
    points(Valid$Q, preds, col = "red")
    best = c(qVal$nse, rfVal$nse, dlVal$nse)
    max(best)
    
    input = which(best==max(best, na.rm = TRUE))
    if(length(input) ==0){next}
    comb = list(qVal, rfVal, dlVal)
    out = as.data.frame(comb[input])
    gage_stats$mode[i] = input
    
    rrmse = out$rrmse 
    r = out$r
    nse = out$nse
    kge = out$kge
    nrmse = out$nrmse
    rbias = out$rbias
    
    bestFit = c("QNT", "RF", "KNN")
    
    legend1 = c(bestFit[input], paste0("RRMSE=",signif(rrmse, 3)),paste0("NSE=",signif(nse, 3)),
                paste0("rBias=",signif(rbias, 3)), paste0("KGE=",signif(kge, 3)), paste0("NRMSE=",signif(nrmse, 3)))
    legend("topleft", legend1, xpd = TRUE, bty = "n", adj = 1, inset = c(-.05, -.1))
    
    
    gage_stats$RRMSE[i] = rrmse
    gage_stats$NSE[i] = nse
    gage_stats$KGE[i] = kge
    gage_stats$NRMSE[i] = nrmse
    gage_stats$rBias[i] = rbias
    gage_stats$Site_number[i] = paired_df$ID[1]
    gage_stats$n_Landsat_obs[i] = nrow(paired_df)
    print(i)
    
  } else{next}
}
################################################################################################################################
##Canada gauges. 
################################################################################################################################
########################################################################################################################
library(hydat)
library(tidyhydat)
gsim_all = read.csv("E:\\research\\GSIM\\GSIM_metadata\\GSIM_catalog\\GSIM_metadata.csv")
sites = as.data.frame(Site_number_xsections)
sites$gsim = gsub("_gsim", "", sites$Site_number_xsections)
sites$files= gsim_all$reference.no[match(sites$gsim, gsim_all$gsim.no)]




start = as.Date("1979-01-01")
data_val = Eff_widths
RC_End = as.Date("2014-12-31") ###WAs 2014
RC_year = format(RC_End, "%Y")
RC_year_1 = format(RC_End+1, "%Y")
data_val$ID = data_val$ID
data_val$ID_2 = data_val$ID
data_val$calc_mean = data_val$Effective_width

#data_val$Date = as.character(data_val$Date)
tab$ID = tab$id
tab$ID_2 = tab$id
tab$width_m = data_val$width_m[match(tab$ID, data_val$ID)]
#tab$change= tab$median
#tab$change[mapply(is.na, tab$change)] <- 0
tab$width_m = data$width_m[match(tab$ID_2, data$ID_2)]
xSecq=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecw=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecIDcol=grep("V", names(Site_number_xsections))
mInd = array(5, dimnames = NULL)
rangedf_1 = as.data.frame(matrix(numeric(), nrow = 1, ncol = 4))
gage_stats = as.data.frame(matrix(numeric(), nrow =length(Site_number_xsections), ncol = 23))
gage_stats_GRADES = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 14))
colnames(gage_stats_GRADES)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "p_val","Bias", "RRMSE", "avg_std", "change", 'RRMSE_median', "std_Q", "STDE", "mode")
as.data.frame(gage_stats_GRADES)
l_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
Mean_grades = as.vector(nrow(Site_number_xsections))
u_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
sd_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
sd_vals_1 = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
width_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
gage_quants_q = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 100))
gage_quants_w = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 100))
colnames(gage_stats)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "mode","Bias", "RRMSE","avg_std", "change", "RRMSE_median", "std_Q","STDE", "KGE", "NSE", "rBias",
                        "training", "validation", "NRMSE", "Q_50", "W_50")
as.data.frame(gage_stats)
gage_stats_col1 = as.vector(1)
gage_stats_col2 = as.vector(1)
gage_stats_GRADES_col1 = as.vector(1)
gage_stats_GRADES_col2 = as.vector(1)
paired_df_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
rmse = 1
width_grouping = 30
percentiles = c(0.05, 0.95)
training = 0.7
data_val = as.data.table(data_val)
setkey(data_val, ID)

library(profvis)
library(caret)
library(dataRetrieval)
library(lubridate)
#profvis({
for(i in 1:length(Site_number_xsections)){
  id = Site_number_xsections[i]
  paired_df = data_val[.(Site_number_xsections[i])]
  usgs_q = try(hy_daily(sites$files[i]))
  usgs_q = as.data.frame(usgs_q)
  if(nrow(usgs_q)>0){
    usgs_q$q = as.numeric(usgs_q$Value)
    usgs_q = usgs_q[usgs_q$q>=0,]
    #usgs_q_level  = usgs_q[usgs_q$Parameter =="Level",]
    usgs_q= usgs_q[usgs_q$Parameter=="Flow",]
    # colnames(usgs_q_level) = c(colnames(usgs_q[1:5]), "level")
    # usgs_q = left_join(usgs_q, usgs_q_level, by = "Date")
    # usgs_q = usgs_q%>%select(STATION_NUMBER.x, Date, q, level)
    paired_df = inner_join(paired_df, usgs_q, by="Date")
    paired_df$Q = paired_df$q

    paired_df = paired_df[paired_df$q>0,]
    #########################################################################################################    
    paired_df$Q = paired_df$q  
    paired_df = paired_df[!is.na(paired_df$Q),]
    
    if(nrow(paired_df)<3){next}
    set.seed(1)
    trainIndex = createDataPartition(paired_df$Q, p = training,
                                     list = FALSE)

    Train = paired_df[ trainIndex,]
    Valid = paired_df[-trainIndex,]
    # years = unique(year(paired_df$Date))
    # train = round(length(years)*.6)
    # paired_df$Year = year(paired_df$Date)
    # set.seed(5)
    # t = sample(years, train)
    # TrainingSet = paired_df$Year%in%t
    # Train = paired_df[TrainingSet,]
    # Valid = paired_df[!TrainingSet,]

    
    
    
    
    # Train = paired_df[paired_df$Date>=as.Date(as.character('2015-01-01', format = "%Y-%m-%d")),]
    # Valid = paired_df[paired_df$Date<as.Date(as.character('2015-01-01', format = "%Y-%m-%d")),]
    
    if(nrow(Valid)<3|nrow(Train)<3){next}
    
    y = quantile(Train$Q, probs = seq(percentiles[1],percentiles[2],.01), na.rm = TRUE)
    x = quantile(Train$calc_mean, probs = seq(percentiles[1],percentiles[2],.01), na.rm = TRUE)
    if(length(unique(x))>1){
      ###Create function to estimate Q from a Landsat width based on quantile pair up. 
      spl = approxfun(x, y) #, method = "hyman")) #####either usse 'approxfun' or use splinefun with method = "hyman"
    } else{next}
    
    
    # par(pty = "s")
    # plot(c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)),c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)), type = "n", xlab = "In situ Discharge (cms)", ylab = "Landsat Discharge (cms)", main = "In situ vs Landsat")
    # points(Valid$Q, spl(Valid$calc_mean), col = "blue")
    # abline(0,1)
    
    plot(paired_df$Date, paired_df$q, type = "l")
    points(Valid$Date, spl(Valid$calc_mean), col = "green")
    
    names = c("rrmse", "nse", "kge", "nrmse", "rbias")
    
    ##Quantile validation 
    Valid$model = spl(Valid$calc_mean)
    qVal = validation(Valid$model, Valid$Q)
    qVal = as.data.frame(t(qVal))
    colnames(qVal) = names
    
    
    rf = randomForest(Q ~calc_mean+Date,data = Train, ntree= nrow(Train), mtry = 1)
    out = predict(rf, Valid)
    Valid$rf = out
    rfVal = validation(Valid$rf, Valid$Q)
    
    rfVal = as.data.frame(t(rfVal))
    colnames(rfVal) = names
    
    #points(Valid$Q, Valid$rf, pch = 19)
    points(Valid$Date, Valid$rf, col = "blue")
    
    plsFit = try(train(Q ~calc_mean+Date, 
                       data = Train,
                       method = "knn",
                       preProc=c("center", "scale"),
                       trControl = trainControl(method = "cv", number = c(5, 10, 20, 50, 100),search = "random")))
    
    preds <- predict(plsFit, newdata = Valid)
    dlVal = validation(preds, Valid$Q)
    dlVal = as.data.frame(t(dlVal))
    colnames(dlVal) = names
    
    #points(Valid$Q, preds, col = "red")
    points(Valid$Date, preds, col = "red")
    best = c(qVal$nse, rfVal$nse, dlVal$nse)
    max(best, na.rm = TRUE)
    
    input = which(best==max(best, na.rm = TRUE))
    if(length(input) ==0){next}
    comb = list(qVal, rfVal, dlVal)
    out = as.data.frame(comb[input])
    gage_stats$mode[i] = input
    
    rrmse = out$rrmse 
    r = out$r
    nse = out$nse
    kge = out$kge
    nrmse = out$nrmse
    rbias = out$rbias
    
    bestFit = c("QNT", "RF", "KNN")
    
    legend1 = c(bestFit[input], paste0("RRMSE=",signif(rrmse, 3)),paste0("NSE=",signif(nse, 3)),
                paste0("rBias=",signif(rbias, 3)), paste0("KGE=",signif(kge, 3)), paste0("NRMSE=",signif(nrmse, 3)))
    legend("topleft", legend1, xpd = TRUE, bty = "n", adj = 1, inset = c(-.05, -.1))
    
    
    gage_stats$RRMSE[i] = rrmse
    gage_stats$NSE[i] = nse
    gage_stats$KGE[i] = kge
    gage_stats$NRMSE[i] = nrmse
    gage_stats$rBias[i] = rbias
    gage_stats$Site_number[i] = paired_df$ID[1]
    gage_stats$n_Landsat_obs[i] = nrow(paired_df)
    gage_stats$validation[i] = nrow(Valid)
    gage_stats$training[i] = nrow(Train)
    print(i)
    
  } else{next}
}
################################################################################################################################
##Russian gauges. 
################################################################################################################################
########################################################################################################################
RussiaQ = fread("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\DischargeDatasets\\Russia\\Russian_daily_Q.txt", fill = TRUE)
RussiaQ = RussiaQ[RussiaQ$Year>=1984,]
tab = melt(RussiaQ, id.vars = c("Code", "Day", "Year"), measure.vars = c(month.abb))
tab$Month = match(tab$variable, month.abb)
tab$Date = paste(tab$Month, tab$Day, tab$Year, sep = "/")
tab$Date = as.Date(tab$Date, format = "%m/%d/%Y")

gsim_all = read.csv("E:\\research\\GSIM\\GSIM_metadata\\GSIM_catalog\\GSIM_metadata.csv")
sites = as.data.frame(Site_number_xsections)
sites$gsim = gsub("_gsim", "", sites$Site_number_xsections)
sites$files= gsim_all$reference.no[match(sites$gsim, gsim_all$gsim.no)]




start = as.Date("1979-01-01")
data_val = Eff_widths
RC_End = as.Date("2014-12-31") ###WAs 2014
RC_year = format(RC_End, "%Y")
RC_year_1 = format(RC_End+1, "%Y")
data_val$ID = data_val$ID
data_val$ID_2 = data_val$ID
data_val$calc_mean = data_val$Effective_width

#data_val$Date = as.character(data_val$Date)
tab$ID = tab$id
tab$ID_2 = tab$id
tab$width_m = data_val$width_m[match(tab$ID, data_val$ID)]
#tab$change= tab$median
#tab$change[mapply(is.na, tab$change)] <- 0
tab$width_m = data$width_m[match(tab$ID_2, data$ID_2)]
xSecq=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecw=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecIDcol=grep("V", names(Site_number_xsections))
mInd = array(5, dimnames = NULL)
rangedf_1 = as.data.frame(matrix(numeric(), nrow = 1, ncol = 4))
gage_stats = as.data.frame(matrix(numeric(), nrow =length(Site_number_xsections), ncol = 23))
gage_stats_GRADES = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 14))
colnames(gage_stats_GRADES)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "p_val","Bias", "RRMSE", "avg_std", "change", 'RRMSE_median', "std_Q", "STDE", "mode")
as.data.frame(gage_stats_GRADES)
l_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
Mean_grades = as.vector(nrow(Site_number_xsections))
u_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
sd_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
sd_vals_1 = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
width_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
gage_quants_q = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 100))
gage_quants_w = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 100))
colnames(gage_stats)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "mode","Bias", "RRMSE","avg_std", "change", "RRMSE_median", "std_Q","STDE", "KGE", "NSE", "rBias",
                        "SDRR", "MRR", "NRMSE", "Q_50", "W_50")
as.data.frame(gage_stats)
gage_stats_col1 = as.vector(1)
gage_stats_col2 = as.vector(1)
gage_stats_GRADES_col1 = as.vector(1)
gage_stats_GRADES_col2 = as.vector(1)
paired_df_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
rmse = 1
width_grouping = 30
percentiles = c(0.05, 0.95)
training = 0.7
data_val = as.data.table(data_val)
setkey(data_val, ID)

library(profvis)
library(caret)
library(dataRetrieval)
#profvis({
for(i in 1:length(Site_number_xsections)){
  id = Site_number_xsections[i]
  paired_df = data_val[.(Site_number_xsections[i])]
  usgs_q = try(tab[tab$Code==sites$files[i],])
  if(nrow(usgs_q)>0){
    usgs_q$q = as.numeric(usgs_q$value)
    usgs_q = usgs_q[usgs_q$q>=0,]
    paired_df = inner_join(paired_df, usgs_q)
    paired_df$Q = paired_df$q
    
    paired_df = inner_join(paired_df, usgs_q)
    #paired_df = distinct(paired_df, Date, .keep_all = TRUE)
    #########################################################################################################    
    paired_df$Q = paired_df$q  
    paired_df = paired_df[!is.na(paired_df$Q),]
    
    if(nrow(paired_df)<3){next}
    set.seed(1)
    trainIndex = createDataPartition(paired_df$Q, p = training,
                                     list = FALSE)
    
    Train = paired_df[ trainIndex,]
    Valid = paired_df[-trainIndex,]
    # years = unique(year(paired_df$Date))
    # train = round(length(years)*.6)
    # paired_df$Year = year(paired_df$Date)
    # set.seed(5)
    # t = sample(years, train)
    # TrainingSet = paired_df$Year%in%t
    # Train = paired_df[TrainingSet,]
    # Valid = paired_df[!TrainingSet,]
    
    
    
    
    
    # Train = paired_df[paired_df$Date>=as.Date(as.character('2015-01-01', format = "%Y-%m-%d")),]
    # Valid = paired_df[paired_df$Date<as.Date(as.character('2015-01-01', format = "%Y-%m-%d")),]
    
    if(nrow(Valid)<3|nrow(Train)<3){next}
    
    y = quantile(Train$Q, probs = seq(percentiles[1],percentiles[2],.01), na.rm = TRUE)
    x = quantile(Train$calc_mean, probs = seq(percentiles[1],percentiles[2],.01), na.rm = TRUE)
    if(length(unique(x))>1){
      ###Create function to estimate Q from a Landsat width based on quantile pair up. 
      spl = approxfun(x, y) #, method = "hyman")) #####either usse 'approxfun' or use splinefun with method = "hyman"
    } else{next}
    
    
    # par(pty = "s")
    # plot(c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)),c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)), type = "n", xlab = "In situ Discharge (cms)", ylab = "Landsat Discharge (cms)", main = "In situ vs Landsat")
    # points(Valid$Q, spl(Valid$calc_mean), col = "blue")
    # abline(0,1)
    
    plot(paired_df$Date, paired_df$q, type = "l")
    points(Valid$Date, spl(Valid$calc_mean), col = "green")
    
    names = c("rrmse", "nse", "kge", "nrmse", "rbias")
    
    ##Quantile validation 
    Valid$model = spl(Valid$calc_mean)
    qVal = validation(Valid$model, Valid$Q)
    qVal = as.data.frame(t(qVal))
    colnames(qVal) = names
    
    
    rf = randomForest(Q ~calc_mean+Date,data = Train, ntree= nrow(Train), mtry = 1)
    out = predict(rf, Valid)
    Valid$rf = out
    rfVal = validation(Valid$rf, Valid$Q)
    
    rfVal = as.data.frame(t(rfVal))
    colnames(rfVal) = names
    
    #points(Valid$Q, Valid$rf, pch = 19)
    points(Valid$Date, Valid$rf, col = "blue")
    
    plsFit = try(train(Q ~calc_mean+Date, 
                       data = Train,
                       method = "knn",
                       preProc=c("center", "scale"),
                       trControl = trainControl(method = "cv", number = c(5, 10, 20, 50, 100),search = "random")))
    
    preds <- predict(plsFit, newdata = Valid)
    dlVal = validation(preds, Valid$Q)
    dlVal = as.data.frame(t(dlVal))
    colnames(dlVal) = names
    
    #points(Valid$Q, preds, col = "red")
    points(Valid$Date, preds, col = "red")
    best = c(qVal$nse, rfVal$nse, dlVal$nse)
    max(best, na.rm = TRUE)
    
    input = which(best==max(best, na.rm = TRUE))
    if(length(input) ==0){next}
    comb = list(qVal, rfVal, dlVal)
    out = as.data.frame(comb[input])
    gage_stats$mode[i] = input
    
    rrmse = out$rrmse 
    r = out$r
    nse = out$nse
    kge = out$kge
    nrmse = out$nrmse
    rbias = out$rbias
    
    bestFit = c("QNT", "RF", "KNN")
    
    legend1 = c(bestFit[input], paste0("RRMSE=",signif(rrmse, 3)),paste0("NSE=",signif(nse, 3)),
                paste0("rBias=",signif(rbias, 3)), paste0("KGE=",signif(kge, 3)), paste0("NRMSE=",signif(nrmse, 3)))
    legend("topleft", legend1, xpd = TRUE, bty = "n", adj = 1, inset = c(-.05, -.1))
    
    
    gage_stats$RRMSE[i] = rrmse
    gage_stats$NSE[i] = nse
    gage_stats$KGE[i] = kge
    gage_stats$NRMSE[i] = nrmse
    gage_stats$rBias[i] = rbias
    gage_stats$Site_number[i] = paired_df$ID[1]
    gage_stats$n_Landsat_obs[i] = nrow(paired_df)
    print(i)
    
  } else{next}
}
################################################################################################################################
##Thailand gauges. 
################################################################################################################################
########################################################################################################################
thailandQ = "E:\\research\\RatingCurveAnalysis\\GaugeLocations\\DischargeDatasets\\Thailand\\"
gsim_all = read.csv("E:\\research\\GSIM\\GSIM_metadata\\GSIM_catalog\\GSIM_metadata.csv")
sites = as.data.frame(Site_number_xsections)
sites$gsim = gsub("_gsim", "", sites$Site_number_xsections)
sites$files= gsim_all$reference.no[match(sites$gsim, gsim_all$gsim.no)]



start = as.Date("1979-01-01")
data_val = Eff_widths
RC_End = as.Date("2014-12-31") ###WAs 2014
RC_year = format(RC_End, "%Y")
RC_year_1 = format(RC_End+1, "%Y")
data_val$ID = data_val$ID
data_val$ID_2 = data_val$ID
data_val$calc_mean = data_val$Effective_width

#data_val$Date = as.character(data_val$Date)
tab$ID = tab$id
tab$ID_2 = tab$id
tab$width_m = data_val$width_m[match(tab$ID, data_val$ID)]
#tab$change= tab$median
#tab$change[mapply(is.na, tab$change)] <- 0
tab$width_m = data$width_m[match(tab$ID_2, data$ID_2)]
xSecq=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecw=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecIDcol=grep("V", names(Site_number_xsections))
mInd = array(5, dimnames = NULL)
rangedf_1 = as.data.frame(matrix(numeric(), nrow = 1, ncol = 4))
gage_stats = as.data.frame(matrix(numeric(), nrow =length(Site_number_xsections), ncol = 23))
gage_stats_GRADES = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 14))
colnames(gage_stats_GRADES)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "p_val","Bias", "RRMSE", "avg_std", "change", 'RRMSE_median', "std_Q", "STDE", "mode")
as.data.frame(gage_stats_GRADES)
l_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
Mean_grades = as.vector(nrow(Site_number_xsections))
u_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
sd_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
sd_vals_1 = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
width_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
gage_quants_q = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 100))
gage_quants_w = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 100))
colnames(gage_stats)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "mode","Bias", "RRMSE","avg_std", "change", "RRMSE_median", "std_Q","STDE", "KGE", "NSE", "rBias",
                        "SDRR", "MRR", "NRMSE", "Q_50", "W_50")
as.data.frame(gage_stats)
gage_stats_col1 = as.vector(1)
gage_stats_col2 = as.vector(1)
gage_stats_GRADES_col1 = as.vector(1)
gage_stats_GRADES_col2 = as.vector(1)
paired_df_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
rmse = 1
width_grouping = 30
percentiles = c(0.05, 0.95)
training = 0.7
data_val = as.data.table(data_val)
setkey(data_val, ID)

library(profvis)
library(caret)
library(dataRetrieval)
#profvis({
for(i in 1:length(Site_number_xsections)){
  id = Site_number_xsections[i]
  paired_df = data_val[.(Site_number_xsections[i])]
  t = list.files(thailandQ, sites$files[1], full.names = TRUE)
  usgs_q = try(rbindlist(lapply(t, fread)))
  if(!is.error(usgs_q)){
    usgs_q$q = as.numeric(usgs_q$V2)
    usgs_q$Date = as.Date(usgs_q$V1, format = "%Y/%m/%d")
    usgs_q = usgs_q[usgs_q$q>=0,]
    paired_df = inner_join(paired_df, usgs_q)
    paired_df$Q = paired_df$q
    
    paired_df = inner_join(paired_df, usgs_q)
    #paired_df = distinct(paired_df, Date, .keep_all = TRUE)
    #########################################################################################################    
    paired_df$Q = paired_df$q  
    paired_df = paired_df[!is.na(paired_df$Q),]
    
    if(nrow(paired_df)<3){next}
    set.seed(1)
    trainIndex = createDataPartition(paired_df$Q, p = training,
                                     list = FALSE)
    
    Train = paired_df[ trainIndex,]
    Valid = paired_df[-trainIndex,]
    # years = unique(year(paired_df$Date))
    # train = round(length(years)*.6)
    # paired_df$Year = year(paired_df$Date)
    # set.seed(5)
    # t = sample(years, train)
    # TrainingSet = paired_df$Year%in%t
    # Train = paired_df[TrainingSet,]
    # Valid = paired_df[!TrainingSet,]
    
    
    
    
    
    # Train = paired_df[paired_df$Date>=as.Date(as.character('2015-01-01', format = "%Y-%m-%d")),]
    # Valid = paired_df[paired_df$Date<as.Date(as.character('2015-01-01', format = "%Y-%m-%d")),]
    
    if(nrow(Valid)<3|nrow(Train)<3){next}
    
    y = quantile(Train$Q, probs = seq(percentiles[1],percentiles[2],.01), na.rm = TRUE)
    x = quantile(Train$calc_mean, probs = seq(percentiles[1],percentiles[2],.01), na.rm = TRUE)
    if(length(unique(x))>1){
      ###Create function to estimate Q from a Landsat width based on quantile pair up. 
      spl = approxfun(x, y) #, method = "hyman")) #####either usse 'approxfun' or use splinefun with method = "hyman"
    } else{next}
    
    
    # par(pty = "s")
    # plot(c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)),c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)), type = "n", xlab = "In situ Discharge (cms)", ylab = "Landsat Discharge (cms)", main = "In situ vs Landsat")
    # points(Valid$Q, spl(Valid$calc_mean), col = "blue")
    # abline(0,1)
    
    plot(paired_df$Date, paired_df$q, type = "l")
    points(Valid$Date, spl(Valid$calc_mean), col = "green")
    
    names = c("rrmse", "nse", "kge", "nrmse", "rbias")
    
    ##Quantile validation 
    Valid$model = spl(Valid$calc_mean)
    qVal = validation(Valid$model, Valid$Q)
    qVal = as.data.frame(t(qVal))
    colnames(qVal) = names
    
    
    rf = randomForest(Q ~calc_mean+Date,data = Train, ntree= nrow(Train), mtry = 1)
    out = predict(rf, Valid)
    Valid$rf = out
    rfVal = validation(Valid$rf, Valid$Q)
    
    rfVal = as.data.frame(t(rfVal))
    colnames(rfVal) = names
    
    #points(Valid$Q, Valid$rf, pch = 19)
    points(Valid$Date, Valid$rf, col = "blue")
    
    plsFit = try(train(Q ~calc_mean+Date, 
                       data = Train,
                       method = "knn",
                       preProc=c("center", "scale"),
                       trControl = trainControl(method = "cv", number = c(5, 10, 20, 50, 100),search = "random")))
    
    preds <- predict(plsFit, newdata = Valid)
    dlVal = validation(preds, Valid$Q)
    dlVal = as.data.frame(t(dlVal))
    colnames(dlVal) = names
    
    #points(Valid$Q, preds, col = "red")
    points(Valid$Date, preds, col = "red")
    best = c(qVal$nse, rfVal$nse, dlVal$nse)
    max(best, na.rm = TRUE)
    
    input = which(best==max(best, na.rm = TRUE))
    if(length(input) ==0){next}
    comb = list(qVal, rfVal, dlVal)
    out = as.data.frame(comb[input])
    gage_stats$mode[i] = input
    
    rrmse = out$rrmse 
    r = out$r
    nse = out$nse
    kge = out$kge
    nrmse = out$nrmse
    rbias = out$rbias
    
    bestFit = c("QNT", "RF", "KNN")
    
    legend1 = c(bestFit[input], paste0("RRMSE=",signif(rrmse, 3)),paste0("NSE=",signif(nse, 3)),
                paste0("rBias=",signif(rbias, 3)), paste0("KGE=",signif(kge, 3)), paste0("NRMSE=",signif(nrmse, 3)))
    legend("topleft", legend1, xpd = TRUE, bty = "n", adj = 1, inset = c(-.05, -.1))
    
    
    gage_stats$RRMSE[i] = rrmse
    gage_stats$NSE[i] = nse
    gage_stats$KGE[i] = kge
    gage_stats$NRMSE[i] = nrmse
    gage_stats$rBias[i] = rbias
    gage_stats$Site_number[i] = paired_df$ID[1]
    gage_stats$n_Landsat_obs[i] = nrow(paired_df)
    gage_stats$validation[i] = nrow(Valid)
    gage_stats$training[i] = nrow(Train)
    print(i)
    
  } else{next}
}
################################################################################################################################
##Brazil gauges. 
################################################################################################################################
########################################################################################################################
br_files = "E:\\research\\RatingCurveAnalysis\\GaugeLocations\\DischargeDatasets\\Brazil\\"
gsim = read.csv("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\GSIM_GRWL_30m.csv")
gsim_all = read.csv("E:\\research\\GSIM\\GSIM_metadata\\GSIM_catalog\\GSIM_metadata.csv")
gsim_br = gsim[grep("BR_", gsim$Station_Num), ]
gsim_br$gage = gsim_all$reference.no[match(gsim_br$Station_Num, gsim_all$gsim.no)]
sites = as.data.frame(Site_number_xsections)
sites$files= gsim_br$gage[match(sites[,1], paste0(gsim_br$Station_Num, "_gsim"))]
  
stations = paste0(sites$files, ".csv")
br_files = paste0(br_files, stations)







start = as.Date("1979-01-01")
data_val = Eff_widths
RC_End = as.Date("2014-12-31") ###WAs 2014
RC_year = format(RC_End, "%Y")
RC_year_1 = format(RC_End+1, "%Y")
data_val$ID = data_val$ID
data_val$ID_2 = data_val$ID
data_val$calc_mean = data_val$Effective_width

#data_val$Date = as.character(data_val$Date)
tab$ID = tab$id
tab$ID_2 = tab$id
tab$width_m = data_val$width_m[match(tab$ID, data_val$ID)]
#tab$change= tab$median
#tab$change[mapply(is.na, tab$change)] <- 0
tab$width_m = data$width_m[match(tab$ID_2, data$ID_2)]
xSecq=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecw=as.data.frame(matrix(numeric(), nrow =5, ncol = 11))
xSecIDcol=grep("V", names(Site_number_xsections))
mInd = array(5, dimnames = NULL)
rangedf_1 = as.data.frame(matrix(numeric(), nrow = 1, ncol = 4))
gage_stats = as.data.frame(matrix(numeric(), nrow =length(Site_number_xsections), ncol = 23))
gage_stats_GRADES = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 14))
colnames(gage_stats_GRADES)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "p_val","Bias", "RRMSE", "avg_std", "change", 'RRMSE_median', "std_Q", "STDE", "mode")
as.data.frame(gage_stats_GRADES)
l_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
Mean_grades = as.vector(nrow(Site_number_xsections))
u_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
sd_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
sd_vals_1 = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
width_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
gage_quants_q = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 100))
gage_quants_w = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 100))
colnames(gage_stats)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "mode","Bias", "RRMSE","avg_std", "change", "RRMSE_median", "std_Q","STDE", "KGE", "NSE", "rBias",
                        "SDRR", "MRR", "NRMSE", "Q_50", "W_50")
as.data.frame(gage_stats)
gage_stats_col1 = as.vector(1)
gage_stats_col2 = as.vector(1)
gage_stats_GRADES_col1 = as.vector(1)
gage_stats_GRADES_col2 = as.vector(1)
paired_df_vals = as.data.frame(matrix(numeric(), nrow =nrow(Site_number_xsections), ncol = 20))
rmse = 1
width_grouping = 30
percentiles = c(0.05, 0.95)
training = 0.7
data_val = as.data.table(data_val)
setkey(data_val, ID)

library(profvis)
library(caret)
library(dataRetrieval)
#profvis({
for(i in 1:length(Site_number_xsections)){
  id = Site_number_xsections[i]
  paired_df = data_val[.(Site_number_xsections[i])]
  usgs_q = try(read.csv(br_files[i]))
  if(!is.error(usgs_q)){
    tab2 = melt(usgs_q)
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
    tab2$Date = tab2$updatedDate
    usgs_q = tab2
    usgs_q$q = as.numeric(usgs_q$value)
    usgs_q$Date = usgs_q$Date
    usgs_q = usgs_q[usgs_q$q>=0,]
    paired_df = inner_join(paired_df, usgs_q)
    paired_df$Q = paired_df$q
    
    paired_df = inner_join(paired_df, usgs_q)
    #paired_df = distinct(paired_df, Date, .keep_all = TRUE)
    #########################################################################################################    
    paired_df$Q = paired_df$q  
    paired_df = paired_df[!is.na(paired_df$Q),]
    
    if(nrow(paired_df)<3){next}
    set.seed(1)
    trainIndex = createDataPartition(paired_df$Q, p = training,
                                     list = FALSE)
    
    Train = paired_df[ trainIndex,]
    Valid = paired_df[-trainIndex,]
    # years = unique(year(paired_df$Date))
    # train = round(length(years)*.6)
    # paired_df$Year = year(paired_df$Date)
    # set.seed(5)
    # t = sample(years, train)
    # TrainingSet = paired_df$Year%in%t
    # Train = paired_df[TrainingSet,]
    # Valid = paired_df[!TrainingSet,]
    
    
    
    
    
    # Train = paired_df[paired_df$Date>=as.Date(as.character('2015-01-01', format = "%Y-%m-%d")),]
    # Valid = paired_df[paired_df$Date<as.Date(as.character('2015-01-01', format = "%Y-%m-%d")),]
    
    if(nrow(Valid)<3|nrow(Train)<3){next}
    
    y = quantile(Train$Q, probs = seq(percentiles[1],percentiles[2],.01), na.rm = TRUE)
    x = quantile(Train$calc_mean, probs = seq(percentiles[1],percentiles[2],.01), na.rm = TRUE)
    if(length(unique(x))>1){
      ###Create function to estimate Q from a Landsat width based on quantile pair up. 
      spl = approxfun(x, y) #, method = "hyman")) #####either usse 'approxfun' or use splinefun with method = "hyman"
    } else{next}
    
    
    # par(pty = "s")
    # plot(c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)),c(min(spl(x), na.rm = TRUE), max(spl(x), na.rm = TRUE)), type = "n", xlab = "In situ Discharge (cms)", ylab = "Landsat Discharge (cms)", main = "In situ vs Landsat")
    # points(Valid$Q, spl(Valid$calc_mean), col = "blue")
    # abline(0,1)
    
    plot(paired_df$Date, paired_df$q, type = "l")
    points(Valid$Date, spl(Valid$calc_mean), col = "green")
    
    names = c("rrmse", "nse", "kge", "nrmse", "rbias")
    
    ##Quantile validation 
    Valid$model = spl(Valid$calc_mean)
    qVal = validation(Valid$model, Valid$Q)
    qVal = as.data.frame(t(qVal))
    colnames(qVal) = names
    
    
    rf = randomForest(Q ~calc_mean+Date,data = Train, ntree= nrow(Train), mtry = 1)
    out = predict(rf, Valid)
    Valid$rf = out
    rfVal = validation(Valid$rf, Valid$Q)
    
    rfVal = as.data.frame(t(rfVal))
    colnames(rfVal) = names
    
    #points(Valid$Q, Valid$rf, pch = 19)
    points(Valid$Date, Valid$rf, col = "blue")
    
    plsFit = try(train(Q ~calc_mean+Date, 
                       data = Train,
                       method = "knn",
                       preProc=c("center", "scale"),
                       trControl = trainControl(method = "cv", number = c(5, 10, 20, 50, 100),search = "random")))
    
    preds <- predict(plsFit, newdata = Valid)
    dlVal = validation(preds, Valid$Q)
    dlVal = as.data.frame(t(dlVal))
    colnames(dlVal) = names
    
    #points(Valid$Q, preds, col = "red")
    points(Valid$Date, preds, col = "red")
    best = c(qVal$nse, rfVal$nse, dlVal$nse)
    max(best, na.rm = TRUE)
    
    input = which(best==max(best, na.rm = TRUE))
    if(length(input) ==0){next}
    comb = list(qVal, rfVal, dlVal)
    out = as.data.frame(comb[input])
    gage_stats$mode[i] = input
    
    rrmse = out$rrmse 
    r = out$r
    nse = out$nse
    kge = out$kge
    nrmse = out$nrmse
    rbias = out$rbias
    
    bestFit = c("QNT", "RF", "KNN")
    
    legend1 = c(bestFit[input], paste0("RRMSE=",signif(rrmse, 3)),paste0("NSE=",signif(nse, 3)),
                paste0("rBias=",signif(rbias, 3)), paste0("KGE=",signif(kge, 3)), paste0("NRMSE=",signif(nrmse, 3)))
    legend("topleft", legend1, xpd = TRUE, bty = "n", adj = 1, inset = c(-.05, -.1))
    
    
    gage_stats$RRMSE[i] = rrmse
    gage_stats$NSE[i] = nse
    gage_stats$KGE[i] = kge
    gage_stats$NRMSE[i] = nrmse
    gage_stats$rBias[i] = rbias
    gage_stats$Site_number[i] = paired_df$ID[1]
    gage_stats$n_Landsat_obs[i] = nrow(paired_df)
    gage_stats$validation[i] = nrow(Valid)
    gage_stats$training[i] = nrow(Train)
    print(i)
    
  } else{next}
}



gage_file = st_read("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\combined.shp")
gage_stats$GRWL_width_m = gage_file$GRWL_wd[match(gage_stats$Site_number, gage_file$Sttn_Nm)]

gage_stats = as.data.frame(gage_stats)
apply(gage_stats, 2, median, na.rm= TRUE)
gage_stats = as.data.frame(gage_stats)
boxplot(gage_stats$RRMSE[gage_stats$GRWL_width_m>90], ylim = c(0,200))

gage_file = gage_file[gage_file$Sttn_Nm%in%gage_stats$Site_number,]
gage_stats$Sttn_Nm = gage_stats$Site_number
gage_stats_vals = merge(gage_file, gage_stats,by = 'Sttn_Nm')

library(ggplot2)
library(tmap)

tmap_mode("view")

tm_shape(gage_stats_vals)+
  tm_bubbles(col = "NSE",size = 0.1, breaks = c(-1,-.5, 0,.5, 1))
tm_shape(gage_stats_vals)+
  tm_bubbles(col = "KGE",size = 0.02, breaks = c(-1, -.4, 1))
tm_shape(gage_stats_vals)+
  tm_bubbles(col = "mode",size = 0.02)


tm_shape(gage_stats_vals)+
  tm_bubbles(col = "rBias",size = 0.02, breaks = c(-100, 0, 100))



tm_shape(gage_stats_vals)+
  tm_bubbles(col = "n_Landsat_obs",size = 0.02, breaks = c(0, 100, 500, 1000, 2000))



gage_stats_vals1 = gage_stats_vals%>%select(mode, geometry, rBias, NRMSE, RRMSE, KGE, NSE, n_Landsat_obs, Site_number, lakeFlg, ds2GRWL, GRWL_wd)
st_write(gage_stats_vals1, "E:\\research\\RatingCurveAnalysis\\stats\\grdc_prelim1.shp")










######See if GRDC record is same or different for same gauges. 
gage_stats$gsim = gsub("_gsim", "", gage_stats$Site_number)
gage_stats$GRDC = gsim_all$paired.db.no[match(gage_stats$gsim, gsim_all$gsim.no)]
gage_stats$GRDC = gsim_all$grdb.no[match(gage_stats$GRDC, gsim_all$reference.no)]

gage_stats$ANA = sites$files


##ANA
usgs_q = try(read.csv(br_files[6]))
  tab2 = melt(usgs_q)
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
  tab2$Date = tab2$updatedDate
  usgs_q = tab2
  usgs_q$q = as.numeric(usgs_q$value)
  usgs_q$Date = usgs_q$Date
  usgs_q = usgs_q[usgs_q$q>=0,]
  ana = usgs_q
ana = ana[order(ana$Date),]
  
##GRDC
  grdc_path = "E:\\research\\RatingCurveAnalysis\\GaugeLocations\\DischargeDatasets\\GRDC\\"
  grdc_files = paste0("3618951", "_Q_Day.Cmd.txt")
  grdc_files = paste0(grdc_path, grdc_files)
  usgs_q = try(read.table(grdc_files, stringsAsFactors = FALSE))
    usgs_q$V1 = substr(usgs_q$V1, 0, 10)
    usgs_q$datetime = usgs_q$V1
    usgs_q$q = as.numeric(usgs_q$V2)
    usgs_q$Date = as.Date(usgs_q$datetime, format = "%Y-%m-%d")
    usgs_q = usgs_q[usgs_q$q>=0,]



plot(usgs_q$Date, usgs_q$q, type = "l")
lines(ana$Date, ana$q, col = "red")




join = inner_join(ana, usgs_q, by = "Date")
plot(join$q.x, join$q.y)


