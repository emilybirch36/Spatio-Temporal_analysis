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


library(plyr)
# bind/join together 
grid_and_locations.prec <- do.call(cbind, list(locations.sp@data, over(locations.sp, grid.distP), over(locations.sp, raster_prcp_elev[c("PRISM_prec", "elev_1km")])))
joined.prec <- plyr::join(train, grid_and_locations.prec)
# check it has joined
joined.prec


joined.prec <- joined.prec[complete.cases(joined.prec[,c("PRCP","elev_1km","cumulative_date")]),]

# for speed can fit on smaller sample size 
joined.prec.s <- joined.prec[sample.int(size=1000, nrow(joined.prec)),]


# Split into train and test blocks
# year <- cleaned_precip_data$year
# test <- cleaned_precip_data %>% 
#  filter(year == 2017)
# train <- cleaned_precip_data %>% 
#  filter(year < 2017)
# head(test, 10)



# split train/test 80/20%. 
# the order of dates have been removed so can split 80/20 randomly
train_test_split <- sample(1:nrow(joined.prec), size = 0.8*nrow(joined.prec))

# train- include only elements in index
train <- joined.prec [train_test_split, ]

# test- include all but the elements in the index
test <- joined.prec [-train_test_split, ]

# check the data has been split
head(test, 10)
head(train, 10)


# MAKE AND FIT THE RF MODEL


library(caret)
set.seed(1)

# create the s-t model (which will predict rainfall as a function of:
# covariates, space-buffer distances- and time-cumulative_date and doy-)
st_model <- as.formula(paste("PRCP ~ cumulative_date + elev_1km + +PRISM_prec + dayofyear +", dist_elev_grid))
st_model


# CREATE SPACE-TIME FOLDS
# https://rdrr.io/cran/CAST/man/CreateSpacetimeFolds.html 

### Prepare for leave-One-Location-Out cross validation
library(CAST)
indices <- CreateSpacetimeFolds(cleaned_precip_data,spacevar="STATION",
                                k=length(unique(cleaned_precip_data$STATION)))
str(indices)
indices


  

# takes half an hour to fit all trees 
# build the rf model on the data

# trControl=trainControl(method="cv", index= indices$index)
  
  

# split the training set again to tune hyperparameters:
# create another testing and training set
# in_training <- createDataPartition(train, p = .75, list = FALSE)
# training_2 <- train[ in_training,]
# testing_2  <- train[-in_training,]


# use the s-t LLO CV
fit_control <- trainControl(method="cv",index=indices$index,
                            indexOut=indices$indexOut)


  

# Tuning hyperparameters using tuneGrid =
# rf_grid <- expand.grid(mtry = c(20, 50, 100, 150, 200),
#                       splitrule = c("gini", "extratrees"),
#                       min.node.size = c(1, 3, 5))
# rf_grid


# then fit model with the parameter grid to find best accuracy
# library(ranger)
# rf_tune_fit <- ranger(formula = st_model,
#             data = train,
#             importance = "impurity",
#             trControl=fit_control,
#             tuneGrid = rf_grid)
# rf_tune_fit

# ntree = 150 and mtry 200 are optimal


# Random forests are highly sensitive to correlated predictors: it splits their importance
# plot from https://brunaw.com/slides/rladies-dublin/RF/intro-to-rf.html#31 
library(corrplot)
library(dplyr)
corrplot::corrplot(cor(cleaned_precip_data %>% select_if(is.numeric), 
                       method = "spearman"))


# remove the correlated variables and build the rf model
# fit the model on training data WITH LLO cv
library(ranger)
rf <- ranger(formula = st_model,
                 data = train,
                 mtry=200,
                 seed = 1, 
                 num.trees=150,
                 importance = "impurity",
                 quantreg=TRUE,
                 trControl=fit_control)
rf


# Variable importance
importance <- rf$variable.importance/max(rf$variable.importance)
importance


# TEST
# predict on the test (new, unseen data) (with leave one location out-cross-validation) and calculate RMSE
# adapted from https://stackoverflow.com/questions/60685866/how-do-i-find-out-the-rmse-of-a-random-forest-in-r 
library(Metrics)

# set seed
set.seed(825)

res = lapply(c(111,222),function(i){
  set.seed(i)
  fit = rf
  
pred_values = predict(fit,test)$predictions

# pred_rf <- predict(rf,test)$predictions
actual_values = test$PRCP


data.frame(seed=i,
           metrics_rmse = rmse(pred_values,actual_values),
           cal_rmse = mean((pred_values-actual_values)^2,  na.rm = T)^0.5
)
})


res = do.call(rbind,res)
head(res)

# RMSE is 0.0373


# compare predicted outcome and true outcome
confusionMatrix(pred_values, as.factor(actual_values))




# Plot the predicted vs observed 
# adapted from https://brunaw.com/slides/rladies-dublin/RF/intro-to-rf.html#26 
test %>% 
  mutate(predicted = predict(rf, test)$predictions) %>% 
  ggplot(aes(predicted, test$PRCP)) +
  geom_point(colour = "#ff6767", alpha = 0.3) +
  labs(title = "Predicted and observed") +  theme_bw(18)









