ryan = as.data.frame(matrix(numeric(), ncol = 1, nrow = 10))
a = c(1, NA, 5, 3, NA, 7, 6, NA, 8, 11, 15)
y = 0
for(i in 1:length(a)){
 y = y+1
 ryan$V1[y] = a[i]
}


################################################################################################
##At what point does recurrence interval effect performance?
################################################################################################
lookup = fread("E:/research/RatingCurveAnalysis/obs/lookupFull.csv")
lookup = lookup%>%select(Site_number, node_id, mode, NSE)
lookup = lookup[!is.na(lookup$NSE),]
lookup = as.data.table(lookup)
keys = c("Site_number", "node_id")
setkeyv(lookup, keys)


gage_stats = as.data.frame(matrix(numeric(), nrow =2000000, ncol = 24))
colnames(gage_stats)= c("Site_number", "GRWL_width_m","n_Landsat_obs","R_2", "R", "RMSE", "mode","Bias", "RRMSE","avg_std", "change", "std_Q","STDE", "KGE", "NSE", "rBias",
                        "SDRR", "recurrence", "NRMSE", "Q_50", "W_50", "node_id",".geo")
cols = c("green","yellow", "blue", "red", "black")
n = 0
'%!in%' <- function(x,y)!('%in%'(x,y))
recurrence = c(2:20)
#recurrence = rev(c(2:20))

readFile = function(xsectionFile, gaugeFile){
  paired_df = fread(xsectionFile)
  paired_df = paired_df[paired_df$width>0&paired_df$cloud<.1&paired_df$count==2&paired_df$max==0,]
  paired_df$Date = as.Date(as.POSIXct(paired_df$`system:time_start`/1000, origin = "1970-01-01"))
  paired_df$width = paired_df$width*paired_df$length
  nodes = unique(paired_df$node_id)
  paired_df = as.data.table(paired_df)
  setkey(paired_df, node_id)
  pd1 = paired_df
  usgs_q = try(read.table(gaugeFile, stringsAsFactors = FALSE))
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
  }
  tab = list(usgs_q, paired_df)
  return(tab)
  }



#recurrence = 2:20


LinearFun = function(Train, Valid){
  linear = lm(Train$Q~Train$width)
  out = (Valid$width*linear$coefficients[[2]])+linear$coefficients[[1]]
}
PwrFun = function(Train,Valid){
  log_linear = lm(log(Train$Q)~log(Train$width))
  out = (Valid$width^log_linear$coefficients[[2]])*(exp(log_linear$coefficients[[1]]))
}


RFFun = function(Train, Valid){
  rf = randomForest(Q ~width,data = Train, ntree= nrow(Train), mtry = 1)
  out = predict(rf, Valid)
}
KNNFun = function(Train, Valid){
  plsFit = try(train(Q ~width, 
                     data = Train,
                     method = "knn",
                     preProc=c("center", "scale"),
                     trControl = trainControl(method = "cv", number = c(5, 10, 20, 50, 100),search = "random")))
  out <- predict(plsFit, newdata = Valid)
}
SplFun = function(Train, Valid){
  spline_test = try(smooth.spline(Train$width, Train$Q, spar = 0.9))
  if(is.error(spline_test)){
    out = rep(0, nrow(Valid))
  }else{
    spl_preds = predict(spline_test, Valid$width)
    out = spl_preds$y
  }}




outlist = list()
recFun = function(nd, rec, site, paired_df, usgs_q, fn){
  nodes = nd
  fiveYear = try(spl(rec))
  reclass = try(na.omit(usgs_q[usgs_q$q>=fiveYear,]))
  if(nrow(reclass)>1&!is.error(spl)){
    reclass = try(na.omit(usgs_q[usgs_q$q>=fiveYear,]))
    reclass = aggregate(Date~Year, reclass, min)
    reclass = c(as.Date("1983-12-31"), reclass$Date, as.Date("2021-12-31"))
    tab = list()
    for(r in 1:length(reclass)-1){
      # n = n+1
      # gage_stats$.geo[n] = paired_df_1$.geo[1]
      # gage_stats$Site_number[n] = Site_number_xsections[i]
      # gage_stats$node_id[n] = as.numeric(nodes)
      paired_df_1 = paired_df[paired_df$node_id==nodes,]
      paired_df_1 = paired_df_1[paired_df_1$Date>reclass[r]&paired_df_1$Date<reclass[r+1],]
      #algorithm = lookup[lookup$Site_number==Site_number_xsections[i]&lookup$node_id==nodes[p],]
      #algorithm = na.omit(algorithm)
      #algorithm = algorithm$mode
      
      algorithm = lookup[.(as.integer(site))]
      algorithm = algorithm[algorithm$node_id==nodes,]
      algorithm = na.omit(algorithm$mode)
      
      nm = paste0(algorithm, "Fun")
      
      
      if(nrow(paired_df_1)<3|length(algorithm)==0){next}
      set.seed(1)
      trainIndex = createDataPartition(paired_df_1$Q, p = .7,
                                       list = FALSE)
      
      Train = paired_df_1[ trainIndex,]
      Valid = paired_df_1[-trainIndex,]
      
      out = do.call(nm, list(Train, Valid))
      
      #if(nrow(Valid)<3|length(unique(Train$width))<4){next}
      
      # if(algorithm=="Linear"){
      #   linear = lm(Train$Q~Train$width)
      #   out = (Valid$width*linear$coefficients[[2]])+linear$coefficients[[1]]
      #   
      # }
      # if(algorithm=="Pwr"){
      #   log_linear = lm(log(Train$Q)~log(Train$width))
      #   out = (Valid$width^log_linear$coefficients[[2]])*(exp(log_linear$coefficients[[1]]))
      #   
      # }
      # if(algorithm=="RF"){
      #   rf = randomForest(Q ~width,data = Train, ntree= nrow(Train), mtry = 1)
      #   out = predict(rf, Valid)
      # }
      # if(algorithm=="KNN"){
      #   plsFit = try(train(Q ~width, 
      #                      data = Train,
      #                      method = "knn",
      #                      preProc=c("center", "scale"),
      #                      trControl = trainControl(method = "cv", number = c(5, 10, 20, 50, 100),search = "random")))
      #   out <- predict(plsFit, newdata = Valid)
      # }
      # if(algorithm=="Spl"){
      #   spline_test = try(smooth.spline(Train$width, Train$Q, spar = 0.9))
      #   if(is.error(spline_test)){
      #     out = rep(0, nrow(Valid))
      #   }else{
      #     spl_preds = predict(spline_test, Valid$width)
      #     out = spl_preds$y
      #   }
      # }
      Valid$model = out
      Valid$recurrence = rec
      Valid$mode = algorithm
      Valid$breakDate = reclass[r]
      outlist[[r]] = Valid
    }
    #return(outlist)
    # if(nrow(rbindlist(outlist))>1){
    # model = rbindlist(outlist)
    # performance = validation(model$model, Valid$Q)
    # performance = as.data.frame(t(performance))
    # names = c("rrmse", "nse", "kge", "nrmse", "rbias")
    # colnames(performance) = names
    # tab[[r]] = list(performance$nse)
    # }
  }
  return(outlist)
}

historicFun = function(nodes, recur, site, paired_df, usgs_q, spl){
  ryan = list()
  for(j in 1:length(nodes)){
    r = recFun(nodes[j], recur, site, paired_df, usgs_q,spl)
    if(length(r)>=1){
    ryan[[j]] = rbindlist(r)
  }}
  df = rbindlist(ryan)
  return(df)
}

r = recFun(nodes[5], 5, Site_number_xsections[10], paired_df, usgs_q, spl)
rt = rbindlist(r)

a = historicFun(nodes, 10, Site_number_xsections[10], paired_df, usgs_q, spl)







r = readFile(xsection_files[10], grdc_files[10])#lapply(xsection_files[1], readFile, gaugeFile = grdc_files[1])
usgs_q = r[[1]]
paired_df = rbindlist(r[2])
paired_df = as.data.table(paired_df)
nodes = unique(paired_df$node_id)
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



validationFun = function(nd, df){
  roi = df[df$node_id==nd,]
  rec = unique(roi$recurrence)
  out = list()
  for(k in 1:length(rec)){
    aoi = roi[roi$recurrence==rec[k],]
    val = validation(aoi$model, aoi$Q)
    v = as.data.frame(t(val))
    v = cbind(v, rec[k], aoi$mode[1], nd, aoi$Sttn_Nm[1])
    out[[k]] = v
  }
  performance = rbindlist(out)
  colnames(performance) = c("rrmse", "nse", "kge", "nrmse", "rbias", "recurrence", "mode", "node_id", "Site_number")
  return(performance)
}



performance = validation(model$model, Valid$Q)
performance = as.data.frame(t(performance))
names = c("rrmse", "nse", "kge", "nrmse", "rbias")
colnames(performance) = names





tab = list()
inside = list()
for(i in 1:length(Site_number_xsections)){
  print(i)
  r = readFile(xsection_files[i], grdc_files[i])#lapply(xsection_files[1], readFile, gaugeFile = grdc_files[1])
  usgs_q = r[[1]]
  if(nrow(usgs_q)>=2){
  paired_df = rbindlist(r[2])
  paired_df = as.data.table(paired_df)
  nodes = unique(paired_df$node_id)
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
  
  for(k in 1:length(recurrence)){
    a = historicFun(nodes, recurrence[k], Site_number_xsections[i], paired_df, usgs_q, spl)
    inside[[k]] = a
  }
  
  df = rbindlist(inside)
  
  if(nrow(df)>0){
  nodes = unique(df$node_id)
  val = validationFun(nodes, df)
  #vals = df[df[,.I[which.max(NSE)], by = list(Site_number)]$V1]

  tab[[i]] = val
  }
  }
}
df = rbindlist(tab)
fwrite(df, "E:/research/RatingCurveAnalysis/obs/recurrence_Full.csv")

multiple = df[df[,.I[which.max(nse)], by = list(Site_number)]$V1]
multiple$NSE = multiple$nse
single = fread("E:/research/RatingCurveAnalysis/obs/lookupFull.csv")
single = single[single[,.I[which.max(NSE)], by = list(Site_number)]$V1]


single$single = single$NSE
single$Site_number = paste0(single$Site_number, "_grdc")
multiple$multiple = multiple$NSE

comb = merge(single, multiple, by = "Site_number")
comb$diff = comb$multiple-comb$single

nrow(comb[comb$mode.x=="Pwr"|comb$mode.x=="Linear"|comb$mode.x=="Spl",])
comb$NSE = ifelse(comb$diff>0, comb$multiple, comb$single)
comb$recurrence = ifelse(comb$diff>0, comb$recurrence, 0)
comb$mode = ifelse(comb$diff>0, comb$mode.y, comb$mode.x)





















gage_file = st_read("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\30_m_1984\\combined_andIndia.shp")
#gage_file = gage_file[gage_file$Sttn_Nm%in%paste0(df$Site_number, "_grdc"),]
comb$Sttn_Nm = comb$Site_number#aste0(multiple$Site_number, "_grdc")
gage_stats_vals = merge(gage_file, comb,by = 'Sttn_Nm')

library(ggplot2)
library(tmap)

tmap_mode("view")

tm_shape(gage_stats_vals)+
  tm_bubbles(col = "NSE",size = 0.2, breaks = c(-1,0,.25,.5,.75, 1))




