
# ESTDA- Boulder colarado daily precipitation data

# set working directory
setwd("~/Documents/UCL/T2_spatio_temporal/rainfall")


# list files in directory
list.files(path = ".", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

getwd()

# readRDS restores a single R object called co_prec
co_prec = readRDS("~/Documents/UCL/T2_spatio_temporal/rainfall/boulder_prcp.rds")
head(co_prec, 10)


# make new column of day of year
co_prec$doy = as.integer(strftime(as.POSIXct(paste(co_prec$DATE), format="%Y-%m-%d"), format = "%j"))

# make new column for year 
co_prec$year = as.integer(strftime(as.POSIXct(paste(co_prec$DATE), format="%Y-%m-%d"), format = "%Y"))
co_prec

# assign a variable for each year
year_2014 <- filter(co_prec, co_prec$year =="2014")
head(year_2014, 5)

# 2015
year_2015 <- data.frame(filter(co_prec, year =="2015"))
head(year_2015, 5)

# 2016
year_2016 <- data.frame(filter(co_prec, year =="2016"))

# 2017
year_2017 <- data.frame(filter(co_prec, year =="2017"))



# get mean daily precip for Boulder Colarado. is 0.067 inches
mean(co_prec$PRCP, na.rm = TRUE) 
mean_daily_prec <- mean(co_prec$PRCP, na.rm = TRUE) 

# max precip in Boulder Colarado between 2014-2017 is 3.29 inches
max(co_prec$PRCP, na.rm = TRUE) 



# histogram of precip values to check distribution. very negatively skewed towards low precipitation values. 
hist(co_prec$PRCP)
abline(v=mean_daily_precip, col="red")


# the quantile-quantile QQ plot is used to examine how the distribution diverges from normality/ the theoretical normal distribution (red line)
# QQ plot shows the distribution diverges at the tail in the upper quartile
qqnorm(co_prec$PRCP)
qqline(co_prec$PRCP, col="red")




# Look at how the data vary in SPACE and TIME

# Pairwise scatterplot to look at relationship between precip, latitude, longitude and elevation 
# relationships dont show much, no vast diff in lat, long at Boulder, Colorado 
pairs(~LONGITUDE+LATITUDE+ELEVATION+mean(PRCP),data=co_prec,
      main="Simple Scatterplot Matrix")


# plot precip on map
library(ggplot2)
library(OpenStreetMap)
library(raster)
require(maps)

# find min and max long and lat for bounding box download
max(co_prec$LATITUDE, na.rm = TRUE)
max(co_prec$LONGITUDE, na.rm = TRUE)

min(co_prec$LONGITUDE, na.rm = TRUE)
min(co_prec$LATITUDE, na.rm = TRUE)


# BACKGROUND MAP NEEDS FIXING/ADJUSTING
year_2014 <- filter(co_prec, co_prec$year =="2014")
# Change the column name:
colnames(year_2014)[7]<-"precvalue"
# Make a proportional symbol of the latest data. Convert latitude and longitude to Mercator projection:
year_2014[,3:4] <-projectMercator(year_2014$LATITUDE, year_2014$LONGITUDE)
# Download a map tile:
#max lat upper left, min long lower 
map <- openmap(c(39.2202,-105.6), c(40.2494,-105.0571),type= 'esri-topo')

autoplot.OpenStreetMap(map)+ geom_point (data= year_2014, aes(x=LONGITUDE,y=LATITUDE, color=precvalue, size=precvalue))+ ggtitle("Annual Average Temperature in Boulder, Colorado, 2014")





# temporal characteristics
library(ggplot2)

# make new column for month
co_prec$month = as.integer(strftime(as.POSIXct(paste(co_prec$DATE), format="%Y-%m-%d"), format = "%m"))
head(co_prec,5)


# plot average rainfall at each station. DOESNT WORK
co_prec %>%
  group_by(STATION) %>%
  summarise_at(vars(-doy, -DATE, -year, -WT01, -WT03, -WT04, -WT05, -WT06, -WT11, -TAVG, -SNOW, -SNWD, -LATITUDE, -LONGITUDE, -ELEVATION, -NAME), funs(mean(., na.rm=TRUE))) -> station_av_rain
# head(station_av_rain)
ggplot(aes(x=month,y=PRCP),data=station_av_rain) + geom_line(stat='identity') + labs(y='Mean Daily Rainfall per Station')


# DOESNT WORK
co_prec %>%
  group_by(month) %>%
  summarise(mrain =mean(PRCP)) -> rain_month
# head(rain_years)
  ggplot(aes(x=month,y=mrain),data=rain_month) + geom_line(stat='identity') + labs(y='Mean monthly Rainfall')  
  
  
 # plot per station. TOO many stations, plot station 1-10?
 co_prec %>% group_by(year, STATION) %>% 
    summarise(total_rain=sum(PRCP)) -> rain_years_st
  ggplot(aes(x=year,y=total_rain),data=rain_years_st) + 
    geom_smooth() + labs(y='Mean Annual Rainfall') +
    facet_wrap(~STATION,nrow=4) + theme_light() 
  
  
  
  
  
  
  


# GRAPHS
# make graph of average temps at stations

# average precip at each station- use to show average. plot this on map showing av precp across region at each station?
library(dplyr)

co_prec %>%
  group_by(STATION) %>%
  summarise_at(vars(-doy, -WT01, -WT03, -WT04, -WT05, -WT06, -WT11, -TAVG, -SNOW, -SNWD, -LATITUDE, -LONGITUDE, -ELEVATION, -NAME), funs(mean(., na.rm=TRUE))) -> station_av_rain
ggplot(aes(x=STATION,y=total_rain),data=station_av_rain) + geom_line(stat='identity') + labs(y='Mean Annual Rainfall')



# get average precip at each station
av_precip_each_station <- summarise(group_by(co_prec, STATION), Mean=mean(PRCP))
typeof(av_precip_each_station)



# SPACE. shows average precip on each date across all weather stations
co_prec %>%
  group_by(DATE) %>%
  summarise_at(vars(-doy, -WT01, -WT03, -WT04, -WT05, -WT06, -WT11, -TAVG, -SNOW, -SNWD, -LATITUDE, -LONGITUDE, -ELEVATION, -NAME), funs(mean(., na.rm=TRUE)))


# THEN JUST OVERLAY LINES ON LINE GRAPH- ONE LINE PER YEAR
# WANT TO SEE WHAT THE WEATHER IS LIKE AT EACH STATION ON EACH DATE 




















