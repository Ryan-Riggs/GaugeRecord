site = "2260100"#"2260110"
i = which(Site_number_xsections==site)
node = gage_stats_vals$node_id[gage_stats_vals$Sttn_Nm==paste0(site, "_grdc")]
mode = gage_stats_vals$mode[gage_stats_vals$Sttn_Nm==paste0(site, "_grdc")]

cols=c("yellow", "blue", "green","red", "purple","brown")

cols = RColorBrewer::brewer.pal(6, "Set1")
w=2
paired_df = fread(xsection_files[i])
paired_df = paired_df[paired_df$width>0&paired_df$cloud<.1&paired_df$count==2&paired_df$max==0,]
paired_df$Date = as.Date(as.POSIXct(paired_df$`system:time_start`/1000, origin = "1970-01-01"))
paired_df$width = paired_df$width*paired_df$length
nodes = unique(paired_df$node_id)
paired_df = as.data.table(paired_df)
setkey(paired_df, node_id)
pd1 = paired_df
usgs_q = try(read.table(grdc_files[i], stringsAsFactors = FALSE))
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
p = which(nodes==node)
paired_df_1 = paired_df[.(nodes[p])]
pd = pd1[.(nodes[p])]
set.seed(1)
trainIndex = createDataPartition(paired_df_1$Q, p = training,
                                 list = FALSE)

Train = paired_df_1[ trainIndex,]
Valid = paired_df_1[-trainIndex,]
linear = lm(Train$Q~Train$width)
log_linear = lm(log(Train$Q)~log(Train$width))
rf = randomForest(Q ~width,data = Train, ntree= nrow(Train), mtry = 1)
plsFit = try(train(Q ~width, 
                   data = Train,
                   method = "knn",
                   preProc=c("center", "scale"),
                   trControl = trainControl(method = "cv", number = c(5, 10, 20, 50, 100),search = "random")))
spline_test = try(smooth.spline(Train$width, Train$Q, spar = 0.9))
library(scales)
plot(Valid$Q, Valid$width, type="n", ylab="Width (m)", xlab="Discharge (cms)")
title("Rating Curve", line =0.25)
points(Train$Q, Train$width,col=alpha("black",0.1), pch=19)
rng = seq(min(Train$width, na.rm = TRUE), max(Train$width, na.rm= TRUE))
lines(linear$coefficients[[2]]*rng+linear$coefficients[[1]], rng, col=cols[1], lwd=w)
lines((rng^log_linear$coefficients[[2]])*(exp(log_linear$coefficients[[1]])), rng, col=cols[2],lwd=w)
rf_df = as.data.frame(rng)
colnames(rf_df) = "width"
lines(predict(rf, rf_df), rng, col=cols[4],lwd=w)
lines(predict(plsFit, rf_df),rng, col=cols[5], lwd=w)
splOut = predict(spline_test, rf_df$width)
lines(splOut$y, splOut$x, col = cols[3],lwd=w)

##Add in piecewise linear regression. 
segmented.mod = segmented(lm(Q~width, Train), seg.Z = ~width)
lines(predict(segmented.mod, rf_df), rng,col=cols[6], lwd=w)

legend2 = c("Linear","Power","Spline","Piecewise","KNN", "Random Forest", "Calibration")
legend("bottomright", legend2,lty = c(1, 1,1,1,1,1, NA),lwd=c(rep(w,6), NA),pch= c(rep(NA,6),19),col = c(cols[1:3],cols[6], cols[5],cols[4], alpha("black",0.1)), xpd = TRUE, bty = "n",  horiz = FALSE, cex =.75)
#points(Valid$Q, Valid$width, col="black", pch =1)


# png("E:\\research\\temp_plots\\8_27RC.png",
#     units = "in",
#     width = 8,
#     height = 6,
#     pointsize = 10,
#     res=500)
# 
# dev.off()




t=10
t2=5
bl = alpha("black", .1)
mn = as.Date("1984-01-01")
mx = as.Date("2021-12-31")
all = seq.Date(mn, mx, 1)
all = as.data.frame(all)
colnames(all) = "Date"
all = full_join(usgs_q, all)
plot(all$Date[order(all$Date)], all$q[order(all$Date)], type = "n", ylab = "Discharge (cms)", xlab = "Year")
title(paste("GRDC:", Site_number_xsections[i]), line =0.25)
lines(all$Date[order(all$Date)], all$q[order(all$Date)], col ="Darkgrey")
allNa = all[is.na(all$q), ]
pd = inner_join(pd, allNa)
points(Train$Date, Train$q, col=alpha("blue",.75), pch = 19)
points(Valid$Date, Valid$q, col=alpha("orange",.75), pch = 19)
points(pd$Date, predict(spline_test, pd$width)$y, col=alpha(cols[3],.75), pch = 19)
# legend2 = c("In Situ","Calibration","Validation", "Extended record")
# legend("topright", legend2,lty = c(1, NA,NA,NA),pch= c(NA,19,19,19),col = c("Darkgrey",alpha("blue",.75),alpha("orange",.75),alpha(cols[3],.75)), xpd = TRUE, bty = "n",  horiz = FALSE, cex =1)
legend2 = c("In Situ","5 year floods","Calibration","Validation", "Extended record")
legend("topright", legend2,lty = c(1,1, NA,NA,NA),pch= c(NA,NA,19,19,19),col = c("Darkgrey","Black",alpha("blue",.75),alpha("orange",.75),alpha(cols[3],.75)), xpd = TRUE, bty = "n",  horiz = FALSE, cex =1)


# png("E:\\research\\RatingCurveAnalysis\\Figures\\8_27HydrographBase.png",
#         units = "in",
#         width = 10,
#         height = 5,
#         pointsize = 10,
#         res=500
# )
recurrence = recur[recur$Site_number==site&recur$node_id==node,]
usgs_q$Year = year(usgs_q$Date)
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
fiveYear = try(spl(t))
#fiveYear = annualSorted$q[round(annualSorted$return)==5]
reclass = try(na.omit(usgs_q[usgs_q$q>=fiveYear,]))
reclass = aggregate(Date~Year, reclass, min)
reclass = c(as.Date("1983-12-31"), reclass$Date, as.Date("2021-12-31"))
first=reclass
rc = reclass[2:length(reclass)]
abline(v=rc[1:length(rc)-1], col="black")
for(r in 1:length(reclass)){
paired_df_1 = paired_df[.(nodes[p])]
pd = pd1[.(nodes[p])]
paired_df_1 = paired_df_1[paired_df_1$Date>reclass[r]&paired_df_1$Date<reclass[r+1],]
set.seed(1)
trainIndex = try(createDataPartition(paired_df_1$Q, p = training,
                                     list = FALSE))
if(is.error(trainIndex)){next}
print(r)
Train = paired_df_1[ trainIndex,]
Valid = paired_df_1[-trainIndex,]
points(Train$Date, Train$q, col=alpha("blue",.75), pch=19)
points(Valid$Date, Valid$q, col=alpha("orange",.75), pch=19)

}
# png("E:\\research\\RatingCurveAnalysis\\Figures\\8_27HydrographRecurrence.png",
#     units = "in",
#     width = 10,
#     height = 5,
#     pointsize = 10,
#     res=500
# )




#abline(h=spl(t))
fiveYear = try(spl(t2))
#fiveYear = annualSorted$q[round(annualSorted$return)==5]
reclass = try(na.omit(usgs_q[usgs_q$q>=fiveYear,]))
reclass = aggregate(Date~Year, reclass, min)
reclass = c(as.Date("1983-12-31"), reclass$Date, as.Date("2021-12-31"))
abline(v=reclass, col="red")





























library(tmap)
tmap::tmap_mode("view")  
tm_shape(gage_stats_vals)+
  tm_bubbles(col = "mode",size = 0.2, breaks = c("KNN", "RF", "Linear", "Pwr", "Spl"))


tm_shape(gage_stats_vals)+
  tm_bubbles(col = "n_Landsat_obs",size = 0.002, breaks=c(0,10,20,50,100,400,600))

tm_shape(gage_stats_vals)+
  tm_bubbles(col = "NSE",size = 0.02, breaks=c(-1,0,.25,.5,.75,1))

























