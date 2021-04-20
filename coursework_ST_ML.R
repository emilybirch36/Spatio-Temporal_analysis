# precipitation at either salt lake city, utah OR boulder colerado. daily precip for 5 years.
# time series of daily precipitation measurements from NOAA https://www.ncdc.noaa.gov  

# RF spatio-temporal prediction framework adapted from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6119462/ (Hengl et al., 2018)


# load packages
library(sp)
library(ranger)

# set working directory
setwd("~/Documents/UCL/T2_spatio_temporal/rainfall")


# list files in directory
list.files(path = ".", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

getwd()

# readRDS restores a single R object called co_prec
co_prec = readRDS("~/Documents/UCL/T2_spatio_temporal/rainfall/boulder_prcp.rds")

str(co_prec)
head(co_prec, 10)


# ESTDA
# Basic exploratory analysis, plots and trends
# make new column of day of year
co_prec$doy = as.integer(strftime(as.POSIXct(paste(co_prec$DATE), format="%Y-%m-%d"), format = "%j"))


co_prec$PRCP = 
  
group_by(co_prec$doy)




# Add station to precip readings with column bind
precip <-cbind(prec_df$STATION, prec_df$PRCP)
# head(prec_df, 10)


# arrange data by day of year to look at long term seasonal trend
library(dplyr)
arrange(co_prec, by_group = co_prec$doy)


# make df with only some columns 
prec_df <- data.frame(co_prec$PRCP)
head(prec_df, 10)


# summary stats on df
colnames(prec_df)
summary(prec_df)


# average a value to replace NA for PRCP col DOESN'T WORK
co_prec@data$PRCP[is.na(co_prec@data$PRCP)] <- mean(co_prec@data$PRCP)






# ESTDA

# 1. Organise the data and Explore Basic Stats
# make a dataframe with the prec_station and the dates
prec_df <- data.frame(co_prec$PRCP)
head(prec_df, 10)  

# order by doy


# convert to a MATRIX for ESTDA
prec_matrix <- data.matrix(prec_df, rownames.force = NA)
head(prec_matrix, 5)


hist(prec_matrix)
abline(v=mu, col="red")



group_by(co_prec$doy)




# 2. Explore Temporal Patterns in the data:

# plot time along x axis, daily average precip on y axis
library(gridExtra)
plot1 <- autoplot(co_prec)



# 3. Explore Spatial Patterns in the data:
# Plot the precip data on pixels 
library(maptools)
library(sp)
brks <- quantile(co_grids$PRSC, probs=seq(0,1,0.1))
lbls <- findInterval(co_grids$PRSC, brks)
cols <- colorRampPalette(c("red", "green"))(11)
plot(lasRosas, col=cols[lbls])
legend("bottomleft", legend=leglabs(brks), fill=cols, cex=0.5, title="Yield")
title(main="Corn yield (quintals per hectare)")




# 4. Address Spatial Autocorrelation in the data

# Interpolation for raster grid, with grid data treated as point data with centroids



# Semi-variance with semi-variogram




# Moran's I
Wl <- nb2listw(nb) # a listw object is a weights list for use in autocorrelation measures.
moran(file$norm_death, Wl, n=length(Wl$neighbours), S0=Szero(Wl)) 

# extract map residuals
targetfile2$modelres <- residuals(model)
tm_shape(targetfile2)+tm_polygons("modelres", palette = "-RdBu", style = "quantile")


# Plot residuals
ggplot(data = targetfile2@data, aes(modelres)) + geom_histogram()

ggplot(data = targetfile2@data, aes(sample = modelres)) + geom_qq() + geom_qq_line()


# Spatial weight matrix 
model.W = nb2listw(poly2nb(targetfile2), zero.policy = TRUE, na.action(na.exclude))


# Local moran's I
# (test for residual autocorrelation). Limited because it doesn't reveal the type of lag/error
lm.morantest(model, model.W, zero.policy = TRUE, na.action(na.exclude))



# VIF 
vif_values <- vif(model)
vif_values
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")








# 5. Address Temporal Autocorrelation in the data. 

# Plot the temporal lags. Temporal autocorrelation is the correlation of a time series with itself, separated by a temporal lag


# convert data types to numeric so they can be used for stats 
co_prec$PRCP <- as.numeric(co_prec$PRCP)

df <- as.data.frame(co_prec$PRCP, co_prec$doy)
df
df$Mean <- rowMeans(Df[,1:2])


boulderMeanPrecip <- rowMeans(co_prec$PRCP, na.rm = FALSE, dims = 1)


library(data.table)
d_table <- data.table(co_prec)
hi <- d_table[mean(PRCP), by = .(DATE)]
head(hi, 10)


    
                          
boulderMeanPrecip <- rowMeans(co_pre[,4:(ncol(co_prec))])
boulderLagged <- data.frame(day = 1:366, t=ChMeanTemp[2:(length(ChMeanTemp))], t_minus_1=ChMeanTemp[1:(length(ChMeanTemp)-1)])
p1 <- ggplot(ChLagged, aes(x=year, y=t)) + geom_line()
p2 <- ggplot(ChLagged, aes(x=t, y=t_minus_1)) + 
  geom_point() + 
  labs(y="t-1") +
  geom_smooth(method="lm")+
  annotate("text", 8.5, 10, label=paste("r =", round(cor(ChLagged$t, ChLagged$t_minus_1), 3)))

grid.arrange(p1,p2, nrow=1)



# ACF plot of mean annual temp
acf(ChMeanTemp)



# PACF- Partial Autocorrelation Function









# ML- Random Forest to Predict Rainfall Spatially and Temporally. 

#  data preprocessing- remove NA?

# FUNCTION TO REMOVE NA's IN sp DataFrame OBJECT
#   x           sp spatial DataFrame object
#   margin      Remove rows (1) or columns (2) 
sp.na.omit <- function(x, margin=1) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
  if(margin == 1) {  
    cat("DELETING ROWS: ", na.index, "\n") 
    return( x[-na.index,]  ) 
  }
  if(margin == 2) {  
    cat("DELETING COLUMNS: ", na.index, "\n") 
    return( x[,-na.index]  ) 
  }
}


# DELETE NA's IN targetfile AND SHOW CHANGE IN dim
prec_file <- sp.na.omit(co_prec)     
dim(co_prec)
dim(prec_file) 

# remove NA ?
data[ data == "?"] <- NA
colSums(is.na(data))



# split data 80%/20% training/ test. 20% is most recent years in time series. 
# split data into test vs train
sample = sample.split(data$num, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)




# Prepare data for ML
# get fixed spatial locations
co_locs.sp = co_prec[!duplicated(co_prec$STATION),c("STATION","LATITUDE","LONGITUDE")]
coordinates(co_locs.sp) = ~ LONGITUDE + LATITUDE
proj4string(co_locs.sp) = CRS("+proj=longlat +datum=WGS84")


# create new covariates: day of year (doy) and cumulative day (cdate) 
co_prec$doy = as.integer(strftime(as.POSIXct(paste(co_prec$DATE), format="%Y-%m-%d"), format = "%j"))
co_prec$cdate = floor(unclass(as.POSIXct(as.POSIXct(paste(co_prec$DATE), format="%Y-%m-%d")))/86400)


# overlay these with long term precip (PRISM_prec) and elevation (elev_1km)
co_grids = readRDS("~/Documents/UCL/T2_spatio_temporal/rainfall/boulder_grids.rds")

# coercion method to convert stacked raster layers into a spatial pixels dataframe
# co_grids is an object of class SpatialPixelsDataFrame
co_grids = as(co_grids, "SpatialPixelsDataFrame")

# look at layout of pixel dataframe, data summary
co_grids
# plot histogram of long term precip
hist(co_grids$PRISM_prec)
abline(v=mu, col="red")


# could overfit due to misinterpretation of variables 
# like the inclusion of a unique spatial variable per location, like the elevation

# plot temp against elevation
# elevation IS important
# the importance originates from the ability of the algorithm to access the time series and not from spatial meaning





# co_locs.sp is object of the class SpatialPoints
co_locs.sp = spTransform(co_locs.sp, co_grids@proj4string)
sel.co <- over(co_locs.sp, co_grids[1])
co_locs.sp <- co_locs.sp[!is.na(sel.co$elev_1km),]


# get buffer distances from the weather stations to account for 'space' in RF
grid.distP <- GSIF::buffer.dist(co_locs.sp["STATION"], co_grids[1], as.factor(1:nrow(co_locs.sp)))
dnP <- paste(names(grid.distP), collapse="+")


# create a space-time model to predict daily rainfall as a func of:
# covariates, buffer distances (space) and time
fmP <- as.formula(paste("PRCP ~ cdate + doy + elev_1km + PRISM_prec +", dnP))
fmP


# make the space-time regression matrix
# bind together covariates, buffer distances, time
ov.prec <- do.call(cbind, list(co_locs.sp@data, over(co_locs.sp, grid.distP), over(co_locs.sp, co_grids[c("elev_1km","PRISM_prec")])))
rm.prec <- plyr::join(co_prec, ov.prec)
rm.prec


rm.prec <- rm.prec[complete.cases(rm.prec[,c("PRCP","elev_1km","cdate")]),]
# use a sample sie of 2000 because its a large dataset
rm.prec.s <- rm.prec[sample.int(size=2000, nrow(rm.prec)),]



# Cross Validation- spatial blocks, leave one out method
cv_numeric <- function(varn, points, covs, nfold=5, idcol, method="ranger", cpus=1, Nsub=1e4, OK=FALSE, spcT=TRUE, Log=FALSE, LLO=TRUE, pars.ranger, predDist=NULL){
  points = points[!is.na(points@data[,varn]),]
  if(missing(idcol)) { 
    points$SOURCEID = row.names(points@data)
    idcol = "SOURCEID"
  }



# fit model
# initialise thr rf. fit to train data
rf <- randomForest(
  num ~ .,
  data=train
)


# predict new training eg
pred = predict(rf, newdata=test[-14])

# confusuion matrix
cm = table(test[,14], pred)



library(ranger)
# adjust ntree and mtry hyperparameters
# quantreg = true used for regression problem
# Option 1: treat binomial variable as numeric variable with 0 / 1 values (thus a regression problem),
m.prec <- ranger(fmP, rm.prec.s, num.trees=150, mtry=25, case.weights=1/(rm.prec.s$PRCP.sd^2), quantreg = TRUE)
m.prec




library(CAST)
indices <- CreateSpacetimeFolds(trainingData,
                                spacevar="Station")
model <- train(predictors,
               response,
               method="rf"
               trControl)




# Feature Importance?




# Plot the Predicted/ Forecasted RF Model vs the Actual/ R






# Last Step- Evaluate model
# Plot the residuals- aim for low autocorrelation i.e. not clustered. Which means its a good model














