# ML model (rf)
library(sp)



# set working directory
setwd("~/Documents/UCL/T2_spatio_temporal/rainfall")


# list files in directory
list.files(path = ".", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

getwd()

# CHANGE THE FILE PATHS TO RUN THE FILE
# readRDS restores a single R object called precip_data
precip_data = readRDS("~/Documents/UCL/T2_spatio_temporal/rainfall/boulder_prcp.rds")
# read in raster long term precip and elevation
raster_prcp_elevation = readRDS("~/Documents/UCL/T2_spatio_temporal/rainfall/boulder_grids.rds")



# plot histogram of long term precip
mean_elev <- mean(raster_prcp_elevation$PRISM_prec, na.rm = TRUE) 

hist(raster_prcp_elevation$PRISM_prec, main= "Histogram of long term precip")
abline(v=mean_elev, col="red")





# Preprocessing - not much preprocessing as rf is very robust 
# - remove NA values- impute these with the mean
# - no need to scale the data for RF. 
# - create/ remove columns 
# DONT need to difference the data because RF benefits from the ability to model non-linear data
# DONT need to standardise the data. scale divides each value by its standard deviation
# find correlation between variables ?



# remove or impute NAs with the column mean- ML cant handle NA values 
library(imputeTS)
cleaned_precip_data <- na_mean(precip_data)

#  precip_data$PRCP[is.na(precip_data$PRCP)] <- round(mean(precip_data$PRCP, na.rm = TRUE))
#  head(precip_data$PRCP, 50)

# loop over for all columns
#  for(i in 1:ncol(data)){
#  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
# }

# remove NAs in the other file
library(imputeTS)
raster_prcp_elev <- na_mean(raster_prcp_elevation)


# look at layout of pixel dataframe, data summary
raster_prcp_elev



# remove unecessary columns 
cleaned_precip_data$WT01 <- NULL
cleaned_precip_data$WT03 <- NULL
cleaned_precip_data$WT04 <- NULL
cleaned_precip_data$WT05 <- NULL
cleaned_precip_data$WT06 <- NULL
cleaned_precip_data$WT11 <- NULL
cleaned_precip_data$month <- NULL
cleaned_precip_data$NAME <- NULL


# check columns have been removed 
head(cleaned_precip_data, 10)



# S-T FRAMEWORK ADAPTED FROM Hengl, T., Nussbaum, M., Wright, M. and Heuvelink, G.B.M., 2018. "Random Forest as a Generic Framework for Predictive Modeling of Spatial and Spatio-temporal Variables". PeerJ.

# get buffer distances and create new covariates for time
# overlay the points and grids to create a S-T regression matrix
# create space-time folds
# fit the rf modelwith cv s-t folds
# evaluate model accuracy with LLOCV
# predict some dates
# estimate the prediction error 



# deal with SPATIAL
# coercion method to convert stacked raster layers into a spatial pixels dataframe
# co_grids is an object of class SpatialPixelsDataFrame
raster_prcp_elev = as(raster_prcp_elevation, "SpatialPixelsDataFrame")


# get fixed spatial locations
locations.sp = cleaned_precip_data[!duplicated(cleaned_precip_data$STATION),c("STATION","LATITUDE","LONGITUDE")]
coordinates(locations.sp) = ~ LONGITUDE + LATITUDE
# project into correct coord ref system
proj4string(locations.sp) = CRS("+proj=longlat +datum=WGS84")


# locations.sp is object of the class SpatialPoints
locations.sp = spTransform(locations.sp, raster_prcp_elev@proj4string)
sel.co <- over(locations.sp, raster_prcp_elev[1])
locations.sp <- locations.sp[!is.na(sel.co$elev_1km),]


# get buffer distances from the weather stations to account for 'space' in RF
grid.distP <- GSIF::buffer.dist(locations.sp["STATION"], raster_prcp_elev[1], as.factor(1:nrow(locations.sp)))
# concatenate the vectors 
dist_elev_grid <- paste(names(grid.distP), collapse="+")
dist_elev_grid 




# deal with TEMPORAL
# create new covariates: day of year and cumulative day (cumulative_date) 
cleaned_precip_data$dayofyear = as.integer(strftime(as.POSIXct(paste(co_prec$DATE), format="%Y-%m-%d"), format = "%j"))

cleaned_precip_data$cumulative_date   = floor(unclass(as.POSIXct(as.POSIXct(paste(cleaned_precip_data$DATE), format="%Y-%m-%d")))/86400)
# check new columns have been made 
head(cleaned_precip_data, 10) 






# MAKE AND FIT THE RF MODEL


library(caret)
set.seed(1)

# Split into train and test blocks
# year <- cleaned_precip_data$year
# test <- cleaned_precip_data %>% 
#  filter(year == 2017)
# train <- cleaned_precip_data %>% 
#  filter(year < 2017)
# head(test, 10)


# split train/test 80/20%. 
# the order of dates have been removed so can split 80/20 randomly
train_test_split <- sample(1:nrow(cleaned_precip_data), size = 0.8*nrow(cleaned_precip_data))

# train- include only elements in index
train <- cleaned_precip_data [train_test_split, ]

# test- include all but the elements in the index
test <- cleaned_precip_data [-train_test_split, ]

# check the data has been split
head(test, 10)


# create the s-t model (which will predict rainfall as a function of:
# covariates, space-buffer distances- and time-cumulative_date and doy-)
st_model <- as.formula(paste("PRCP ~ cumulative_date + elev_1km + +PRISM_prec + dayofyear +", dist_elev_grid))
st_model



# CHANGE THE NAMES OF THESE!!!!!!!
ov.prec <- do.call(cbind, list(locations.sp@data, over(locations.sp, grid.distP), over(locations.sp, raster_prcp_elev[c("PRISM_prec", "elev_1km")])))
rm.prec <- plyr::join(train, ov.prec)
rm.prec


rm.prec <- rm.prec[complete.cases(rm.prec[,c("PRCP","elev_1km","cumulative_date")]),]
# use a sample sie of 2000 because its a large dataset


rm.prec.s <- rm.prec[sample.int(size=2000, nrow(rm.prec)),]




# CREATE SPACE-TIME FOLDS
# https://rdrr.io/cran/CAST/man/CreateSpacetimeFolds.html 

### Prepare for leave-One-Location-Out cross validation
library(CAST)
indices <- CreateSpacetimeFolds(cleaned_precip_data,spacevar="STATION",
                                k=length(unique(cleaned_precip_data$STATION)))
str(indices)



rf <- train(predictors,
               response,
               method='rf',
               mtry=212,
               seed=1,
               num.trees=150
               trControl=trainControl(method="cv", index= indices$index))



# tuned the hyperparameters using tuneRanger
# ntree = 150 and mtry 212 are optimal


# OR THIS ONE- TRY BOTH MODELS
# FIT the rf model on the data 
library(ranger)
rf <- ranger(formula = st_model,
                 data = rm.prec,
                 mtry=200,
                 seed = 1, 
                 num.trees=150,
                 importance = "impurity", 
                 quantreg=TRUE)
rf





# Variable importance
# Using the importance()  function to calculate the importance of each variable
importance <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(importance) <- "% Inc MSE"
importance

barplot(-sort(-importance(rf))) # Variable importance



# then evaluate the rf model. 
sd(cleaned_precip_data$PRCP, na.rm = TRUE)



# predict 
rain.rfd1 <- predict(m1.rain, cbind(swiss.dist0@data, swiss1km@data), type="quantiles", quantiles = quantiles)$predictions



# cross validation (LLOCV) for accuracy score of the model on train data 




# fit the model on TEST data






# plot a map of the predictions vs prediction accuracy










