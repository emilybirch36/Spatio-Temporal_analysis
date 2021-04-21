
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


# normalise precip because the distribution is skewed 
norm_prec <- log(co_prec$PRCP)
hist(norm_prec)


# the quantile-quantile QQ plot is used to examine how the distribution diverges from normality/ the theoretical normal distribution (red line)
# QQ plot shows the distribution diverges at the tail in the upper quartile
qqnorm(co_prec$PRCP)
qqline(co_prec$PRCP, col="red")

# plot normalised precip as QQ plot. doesn't work!
qqnorm(log(co_prec$PRCP))
qqline(log(co_prec$PRCP), col="red")





# Look at how the data vary in SPACE and TIME

# Pairwise scatterplot to look at relationship between precip, latitude, longitude and elevation 
# relationships dont show much, no vast diff in lat, long at Boulder, Colorado 
pairs(~LONGITUDE+LATITUDE+ELEVATION+PRCP,data=co_prec,
      main="Simple Scatterplot Matrix")


# plot precip on map. SPACE
library(ggplot2)
library(OpenStreetMap)
library(raster)
require(maps)

# find min and max long and lat for bounding box download
# min and max are longc(W 105°18'05"--W 105°10'41"/lat, N 40°05'39"--N 39°57'51")
max(co_prec$LATITUDE, na.rm = TRUE)
max(co_prec$LONGITUDE, na.rm = TRUE)

min(co_prec$LONGITUDE, na.rm = TRUE)
min(co_prec$LATITUDE, na.rm = TRUE)


# LON1 =  -105.1805 ; LON2 = -105.1041
# LAT1 = 40.0539 ; LAT2 = 39.5751

# BACKGROUND MAP NEEDS FIXING/ADJUSTING- coordinates of backgorund map are slightly off centre (maximum x needs to be larger)
year_2017 <- filter(co_prec, co_prec$year =="2017")
# Change the column name:
# colnames(year_2017)[7]<- "precvalue"
# Make a proportional symbol of the latest data. Convert latitude and longitude to Mercator projection:
year_2017[,3:4] <-projectMercator(year_2017$LATITUDE, year_2017$LONGITUDE)
# Download a map tile:
#max lat upper left, min long lower. longitude (E-W first), lat
map <- openmap(c(40.0539, -105.1805), c(38.5751, -105.1041), minNumTiles=50, mergeTiles = TRUE, type= 'esri-topo')
# check map plots the osm bounds background
plot(map)


# library(ggmap)
# map <- get_map(location = c(lon = -105.1041, lat = 40.0539), zoom = "auto", maptype = c("terrain"), source = c("osm"))

                                                                            
# upper left max y max 40.0539, -105.1805 , lower right  38.5751, -105.1041
autoplot.OpenStreetMap(map) + geom_point (data= year_2017, aes(x=LONGITUDE,y=LATITUDE, color=PRCP, size=PRCP))+ ggtitle("Annual Average Precip in Boulder, Colorado, 2017")








# Temporal characteristics
library(ggplot2)

# make new column for month
co_prec$month = as.integer(strftime(as.POSIXct(paste(co_prec$DATE), format="%Y-%m-%d"), format = "%m"))
head(co_prec,5)


# Line graph showing the mean precip at each station from 2014-2017. Shows interannual vaiation and trend from 2014-2017.
  co_prec %>%
    group_by(STATION) %>% 
    group_by(year)
    summarise_at(vars(-doy, -DATE, -month, -WT01, -WT03, -WT04, -WT05, -WT06, -WT11, -TAVG, -SNOW, -SNWD, -LATITUDE, -LONGITUDE, -ELEVATION, -NAME), funs(mean(., na.rm=TRUE))) -> station_av_rain
  head(station_av_rain)
  ggplot(aes(x=year,y=PRCP),data=station_av_rain) + geom_line(stat='identity') + labs(y='Mean Daily Rainfall per Station')


  
  
  # Multiple line plot of day of year (corresponds to the month) against precipitation for 10 chosen stations. Shows annual/seasonal variation
  # Get station names for plot
  co_prec$STATION
  tail(co_prec$STATION, n = 10000)
  # note that there are large peaks in rainfall between the 100-150th day of the year. 
  library(lattice)
  STATION.chosen=c("US1COJF0326", "US1COGL0015","US1COJF0327","US1COGL0010", "US1COJF0290", "US1COBO0018","US1COBO0014","US1COBO0019","US1COBO0022","US1COBO0304","US1COBO0306")
  #Create a variable containing just the selected stations
  a <- co_prec[co_prec$STATION %in% STATION.chosen,]
  
  xyplot(PRCP ~ doy | STATION, xlab = "doy", type = "l",
         layout = c(5, 2),
         data=a,
         main = "Daily Precipitation in Boulder, Colorado")
  
  

  
# FIX THIS !!  
# pattern over space AND time
# examine the changes in precip over the time period 2014-2017. One map per year. 
# looking at the difference in precip because there is greater variation between the precip at differnt sites than the precip recorded in different years. 
# blue on the map shows the years where precip has changed a lot
  
  # Calculate difference in precip from year to year (first year is zero as there is no preceding year)
  precdiff <- cbind(0, co_prec[,6:ncol(co_prec)]-temp[,5:(ncol(co_prec)-1)])
  newprec <- cbind(newtemp, unlist(precdiff))
  colnames(newprec)[ncol(newprec)] <- "precdiff"
  
  autoplot.OpenStreetMap(map) + geom_point(data=newprec[year,], aes(x=LONGITUDE,y=LATITUDE, color=precdiff, size=log(ALTITUDE))) + scale_colour_gradient2(low="red", mid='white', high="blue") + facet_wrap(facets=~year)
  
  
  

  
  
  
 ########### 
 # AUTOCORRELATION
  
# Temporal autocorrelation and partial autocorrelation function (PACF). 
# Correlation with the time series itself, seperated by a temporal lag.
# 1 is perfect positive autocorrelation
  
  
  # PLOT AUTOCORRELATION COEFFICIENT   NOT PLOTTING CORRECTLY! 
  # r = 0.26, a PMCC value showing that precipitation on previous days is not strongly correlated.
  library(gridExtra)
  bLagged <- data.frame(t=co_prec$PRCP[2:co_prec$doy], t_minus_1=co_prec$PRCP[1:(co_prec$doy)-1]) 
  p2 <- ggplot(bLagged, aes(x=t, y=t_minus_1)) + 
    geom_point() + 
    labs(y="t-1") +
    geom_smooth(method="lm")+ # Add a regression line to the plot
    annotate("text", 8.5, 10, label=paste("r =", round(cor(bLagged$t, bLagged$t_minus_1), 3))) # Calculate PMCC
  # plot
  p2
  
  
# more useful for precip:
# PLOT ACF
  acf(co_prec$PRCP, na.action = na.pass) 
  
  

  # Line graph showing the mean precip at each station from 2014-2017. Shows interannual vaiation and trend from 2014-2017.
  co_prec %>%
  group_by(STATION) %>% 
  group_by(year)
  summarise_at(vars(-doy, -DATE, -month, -WT01, -WT03, -WT04, -WT05, -WT06, -WT11, -TAVG, -SNOW, -SNWD, -LATITUDE, -LONGITUDE, -ELEVATION, -NAME), funs(mean(., na.rm=TRUE))) -> station_av_rain
  acf(station_av_rain, na.action = na.pass)
      

    
    
    
    

     

    
    
    
    
    
    
  
  
  
  
  
 
  
  
















