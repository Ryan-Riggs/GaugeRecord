devtools::install_github('buzacott/bomWater')
library(bomWater)
gages = get_station_list(parameter_type = "Water Course Discharge")

##Example. 
tab = get_daily(parameter_type = 'Water Course Discharge',
                station_number=gages$station_no[1], 
                start_date = '1900-01-01', 
                end_date = '2021-09-10')
plot(tab$Timestamp, tab$Value, type = "l")

##Apply on all gages and export data. 
outpath = "E:\\research\\GlobalGaugeData\\Australia\\"
library(BBmisc)
for(i in 1:length(gages$station_no)){
  print(i)
  discharge = try(get_daily(parameter_type = 'Water Course Discharge',
                        station_number=gages$station_no[i], 
                        start_date = '1900-01-01', 
                        end_date = '2021-09-10'))
  if(is.error(discharge)){next}else{
  file = paste0(outpath,gages$station_no[i], ".csv")
  fwrite(discharge, file)
  }
}
