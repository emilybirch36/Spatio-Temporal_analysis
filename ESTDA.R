
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

# reassign NA values
# na.strings = 999.99)

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


# look at mean and SD, want mean 0 and SD 1 for normal distribution
# get mean daily precip for Boulder Colarado. is 0.067 inches
mean_daily_prec <- mean(co_prec$PRCP, na.rm = TRUE) 
mean_daily_prec

# get SD. 0.186
sd(co_prec$PRCP, na.rm = TRUE)

# max precip in Boulder Colarado between 2014-2017 is 3.29 inches
max(co_prec$PRCP, na.rm = TRUE) 


# summary stats
summary(co_prec$PRCP)



# histogram of precip values to check distribution. very negatively skewed towards low precipitation values. 
hist(co_prec$PRCP)
abline(v=mean_daily_precip, col="red")

# OR
ggplot(data=co_prec, aes(PRCP)) + geom_histogram(breaks=seq(0,300,5))


# normalise precip because the distribution is skewed 

# standardise column to have mean of 0 and SD of 1 
# prcp_norm <- (co_prec$PRCP - mean(co_prec$PRCP)) / sd(co_prec$PRCP)
#  head(prcp_norm, 10)
# hist(prcp_norm$PRCP) ???


# normalise, but check the observation is constant first before transformation. 
# cant normalise PRCP because some PRCP values are 0 so this is not divisible ?

normalise <- function(x) {
  if(min(x, na.rm=TRUE)!=max(x, na.rm=TRUE)) {
    res <- ((x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)))
  } else {
    res <- 0.5
  }
  res
}


# use normalise func to normalise precip and make a new column
co_prec$PRCP_norm <-normalise(co_prec$PRCP)
head(co_prec, 10)

par(mfrow=c(1,2))
prcp_norm_hist <- hist(co_prec$PRCP_norm)
prcp_hist <- hist(co_prec$PRCP), MF
arrange(prcp_norm_hist, prcp_hist)


library(gridExtra)
p1 <- ggplot(data=co_prec, aes(PRCP)) + geom_histogram(breaks=seq(0,300,5), na.rm = TRUE)
p2 <- ggplot(data=co_prec, aes(PRCP_norm)) + geom_histogram(breaks=seq(0,300,5), na.rm = TRUE)
grid.arrange(p1, p2)

mean(co_prec$PRCP_norm, na.rm = TRUE) 
norm_mean_daily_prec <- mean(co_prec$PRCP_norm, na.rm = TRUE) 
abline(v=norm_mean_daily_precip, col="red")
hist(co_prec$PRCP_norm)



# ? co_prec$norm_PRCP <- data.frame(log(co_prec$PRCP))




# look at elevation range
hist(co_prec$ELEVATION)


# the quantile-quantile QQ plot is used to examine how the distribution diverges from normality/ the theoretical normal distribution (red line)
# QQ plot shows the distribution diverges at the tail in the upper quartile
qqnorm(co_prec$PRCP)
qqline(co_prec$PRCP, col="red")

# plot normalised precip as QQ plot. doesn't work!
qqnorm(co_prec$PRCP_norm)
qqline(co_prec$PRCP_norm, col="red")





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

# make new column for month
co_prec$month = as.integer(strftime(as.POSIXct(paste(co_prec$DATE), format="%Y-%m-%d"), format = "%m"))
head(co_prec,5)


# Line graph showing the mean precip at each station from 2014-2017. Shows interannual vaiation and trend from 2014-2017.
co_prec %>%
  group_by(STATION) %>% 
  group_by(year) 
  summarise_at(vars(-doy, -DATE, -month, -WT01, -WT03, -WT04, -WT05, -WT06, -WT11, -TAVG, -SNOW, -SNWD, -LATITUDE, -LONGITUDE, -ELEVATION, -NAME), funs(mean(., na.rm=TRUE))) -> station_av_rain
  head(station_av_rain, 10) 
  plot(station_av_rain$PRCP, main = "Mean daily precip in Boulder, Colorado
       from 2013-2017", xlab = "Year", ylab = "Precipitation (inches)", type="l", xaxt="n",
       axis(side = 1, at =c(2013: 2017), labels = (2013: 2017)))
             
       # cant have axis with x as station_av_rain$year   

  
  
  # Multiple line plot of day of year (corresponds to the month) against precipitation for 10 chosen stations. Shows annual/seasonal variation
  # Get station names for plot
  library(lattice)
  library(ggplot2)
  co_prec$STATION
  tail(co_prec$STATION, n = 10000)
  
  # reset graphics so plot loads 
  dev.off()
  # note that there are large peaks in rainfall between the 100-150th day of the year. 
  co_prec$STATION.chosen=c("US1COJF0326", "US1COGL0015","US1COJF0327","US1COGL0010", "US1COJF0290", "US1COBO0018","US1COBO0014","US1COBO0019","US1COBO0022","US1COBO0304","US1COBO0306")
  #Create a variable containing just the selected stations
  a <- co_prec[co_prec$STATION %in% STATION.chosen,]
  xyplot(PRCP ~ doy | STATION, xlab = "doy", type = "l",
         layout = c(5, 2),
         data=a,
         main = "Daily Precipitation in Boulder Colorado")
  


  

  # total monthly precip in each year 
  daily_precip_per_month <- co_prec %>%
    group_by(month, year) %>%
    order_by(year) %>%
    summarise(total_precip = sum(PRCP, na.rm = TRUE), .groups = 'keep')
  head(daily_precip_per_month, 10)
  
  
  daily_precip_per_month %>%
    ggplot(aes(x = month, y = total_precip)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    facet_wrap(~ year, ncol = 3) +
    labs(title = "Total Monthly Precipitation, 
         Boulder, Colorado",
         subtitle = "(per year: 2013-2017)",
         y = "Daily precip (inches)",
         x = "Month") + theme_bw(base_size = 15) 
  
  
  
  
 # plot similar but for mean monthly precip 
 mean_monthly_prcp <-  co_prec %>%
 group_by(year, month) %>%
   arrange_by(year)
   summarise(mean_precip = mean(co_prec$PRCP, na.rm = TRUE), .groups = 'keep')
  head(mean_monthly_prcp, 10)
 
 
 mean_monthly_prcp %>%
   ggplot(aes(x = month, y = mean_precip)) +
   geom_bar(stat = "identity", fill = "darkblue") +
   facet_wrap(~ year, ncol = 3) +
   labs(title = "Mean monthly Precipitation, 
         Boulder, Colorado",
        subtitle = "(per year: 2013-2017)",
        y = "Mean daily precip (inches)",
        x = "Month") + theme_bw(base_size = 15) 
  
 
 # plot of mean daily precip
 plot(mean_monthly_prcp$mean_precip, main = "Mean daily precip in Boulder, Colorado
      from 2013-2017", xlab = "Year 2013-2017", ylab = "Precipitation (inches)", type="l", xaxt="n")
      
      

 
  

  
  
  
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
  
# TEMPORAL AUTOCORRELATION  
  
# Temporal autocorrelation and partial autocorrelation function (PACF). 
# Correlation with the time series itself, seperated by a temporal lag.
# 1 is perfect positive autocorrelation
  
  
  # PLOT AUTOCORRELATION COEFFICIENT   NOT PLOTTING CORRECTLY! - do for month instead?
  # r = 0.26, a PMCC value showing that precipitation on previous days is not strongly correlated.
  library(gridExtra)
  bLagged <- data.frame(t=mean_monthly_prcp$mean_precip[2:mean_monthly_prcp$month], t_minus_1=mean_monthly_prcp$mean_precip[1:(mean_monthly_prcp$month)-1]) 
  p2 <- ggplot(bLagged, aes(x=t, y=t_minus_1)) + 
    geom_point() + 
    labs(y="t-1") +
    geom_smooth(method="lm")+ # Add a regression line to the plot
    annotate("text", 8.5, 10, label=paste("r =", round(cor(bLagged$t, bLagged$t_minus_1), 3))) # Calculate PMCC
  # plot
  p2
  

  

# ACF
 acf(mean_monthly_prcp$mean_precip, na.action = na.pass, main = "ACF plot of average monthly precipitation") 
  


  # ACF for individual stations up to lag 50
  STATION.chosen=c("US1COJF0326") 
  # for one station
  chosen_station <- co_prec[co_prec$STATION %in% STATION.chosen,]
  acf(chosen_station$PRCP, lag.max=50, main="ACF, station US1COJF0326", na.action = na.pass)
  
  STATION.chosen=c("US1COGL0015") 
  # for one station
  chosen_station <- co_prec[co_prec$STATION %in% STATION.chosen,]
  acf(chosen_station$PRCP, lag.max=50, main="ACF, station US1COGL0015", na.action = na.pass)
  
  
  STATION.chosen=c("US1COJF0327") 
  # for one station
  chosen_station <- co_prec[co_prec$STATION %in% STATION.chosen,]
  acf(chosen_station$PRCP, lag.max=50, main="ACF, station US1COJF0327", na.action = na.pass)
  
  
  STATION.chosen=c("US1COGL0010") 
  # for one station
  chosen_station <- co_prec[co_prec$STATION %in% STATION.chosen,]
  acf(chosen_station$PRCP, lag.max=50, main="ACF, station US1COGL0010", na.action = na.pass)
  
  
  
  STATION.chosen=c("US1COJF0290") 
  # for one station
  chosen_station <- co_prec[co_prec$STATION %in% STATION.chosen,]
  acf(chosen_station$PRCP, lag.max=50, main="ACF, station US1COJF0290", na.action = na.pass)
  
  
  STATION.chosen=c("US1COBO0014") 
  # for one station
  chosen_station <- co_prec[co_prec$STATION %in% STATION.chosen,]
  acf(chosen_station$PRCP, lag.max=50, main="ACF, station US1COBO0014", na.action = na.pass)
  
  
  
  
  # PACF
  pacf(mean_monthly_prcp$mean_precip, na.action = na.pass, main = "ACF plot of average monthly precipitation") 
  
  
  
# PACF. partial autocorrelation function for one of the weather stations up to lag 50.
  STATION.chosen=c("US1COJF0326") 
  # for one station
  chosen_station <- co_prec[co_prec$STATION %in% STATION.chosen,]
  pacf(chosen_station$PRCP, lag.max=50, main="PACF, station US1COJF0326", na.action = na.pass)
    
  
 STATION.chosen=c("US1COGL0015") 
 # for one station
 chosen_station <- co_prec[co_prec$STATION %in% STATION.chosen,]
  pacf(chosen_station$PRCP, lag.max=50, main="PACF, station US1COGL0015", na.action = na.pass)
 
 
 STATION.chosen=c("US1COJF0327") 
 # for one station
 chosen_station <- co_prec[co_prec$STATION %in% STATION.chosen,]
 pacf(chosen_station$PRCP, lag.max=50, main="PACF, station US1COJF0327", na.action = na.pass)
 
 
 STATION.chosen=c("US1COGL0010") 
 # for one station
 chosen_station <- co_prec[co_prec$STATION %in% STATION.chosen,]
 pacf(chosen_station$PRCP, lag.max=50, main="PACF, station US1COGL0010", na.action = na.pass)
 

 
  STATION.chosen=c("US1COJF0290") 
 # for one station
 chosen_station <- co_prec[co_prec$STATION %in% STATION.chosen,]
 pacf(chosen_station$PRCP, lag.max=50, main="PACF, station US1COJF0290", na.action = na.pass)
 
 
 STATION.chosen=c("US1COBO0014") 
 # for one station
 chosen_station <- co_prec[co_prec$STATION %in% STATION.chosen,]
pacf(chosen_station$PRCP, lag.max=50, main="PACF, station US1COBO0014", na.action = na.pass)
 





# SPATIAL AUTOCORRELATION  
  
# Spatial autocorrelation- quantifies the extent to which near observations of a process are more similar than distant observations in space

  # measuring autocorrelation in point data 
  # use a semivariogram- measures how the variance in the difference between observations of a process increases as the distance between measurement locations increases

# the four panes show the semivariance centred on 0, 45, 90 and 135°
# only need to look at angles up to 135 bc the remianing angles ar eopposite and identical 
# the rate of increase in semivariance is different in different directions. This indicates anisotropy, and has implications for modelling


  # variogram plot for precip
  library(gstat)
  coords = list(projectMercator(co_prec[,3], co_prec[,4]))
  
  plot(variogram(list(co_prec$PRCP), locations=coords))
  
  
  library(geoR)
  # so have to make a variogram with all of the precip values - doesnt load points 
  coords = list(projectMercator(co_prec[,3], co_prec[,4]))
  precip = list(mean_monthly_prcp$mean_precip)
  plot(variogram(precip, locations=coords))
  
  
  # look at summary of distances between stations
  library(geoR)
  dists <- dist(co_prec[,3:4])
  
 breaks = seq(0, 1.5, l =11)
 variog
  






# Spatio-Temporal Autocorrelation
# use space-time semi-variogram data to examine the point data 

 # store the data in a spacio-temporal datafreame (STDF) using the space-time package

 
 library(spacetime)
 pts <- SpatialPoints(co_prec[,3:4], 
                      proj4string=CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
 # time <- co_prec$DATE
 time <- seq(as.Date("2013-01-01"), length = 52, by = "year")
 precip_matrix<-data.matrix(co_prec[7])
 # store the data in a spacio-temporal dataframe 
 stfdf <- STFDF(pts, time, data.frame(as.vector(t(precip_matrix))))
 names(stfdf@data) <- "Precipitation"

# calculate the semivariogram
 STVar <- variogram(Precipitation~1, stfdf, width=100, cutoff=1000,tlags=0:10) 

# plot
 plot(STVar, qireframe=T)


 

