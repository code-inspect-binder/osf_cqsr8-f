#------------------------------------
# set seed to replicate the same results
set.seed(500)

# fetch data
data_stop <- read.csv("mock_data/stop_publishing.csv", header = TRUE, sep = ";")

# convert classifying variable
data_stop <- data_stop %>% dplyr::mutate(class_var=ifelse(categoryTarget=="NON_TRAFFIC_RELATED_STOP",1,ifelse(categoryTarget=="TRAFFIC_RELATED_STOP",2,NA)))

# variables according to feature selection
data_stop <- data_stop %>% dplyr::select(avgSamplingInterval, avgSpeed, distanceSlow, distanceToNextHighLevelRoad, distanceToNextLowLevelRoad, duration, minimumSpeed, frcOfNextHighLevelRoad, frcOfNextLowLevelRoad, summedAngles, stopDistance,class_var)

# choose only complete cases without NA'S
data_stop <- data_stop[complete.cases(data_stop), ]

# normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_stop <- as.data.frame(lapply(data_stop, normalize))

# convert class_var into a factor (necessary for the randomForest package)
data_stop$class_var <- as.factor(data_stop$class_var)

# draw train and test data
data_stop$id <- 1:nrow(data_stop)

# define train and test set
train_id <- data_stop %>% dplyr::sample_frac(.75)
test  <- dplyr::anti_join(data_stop, train_id, by = 'id')

# upsize the sample ####
x_upSample <- as.data.frame(dplyr::select(train_id,-class_var,id))
y_upSample <- train_id$class_var
train <- upSample(x_upSample,y_upSample,yname = "class_var")

levels(train$class_var) <- make.names(levels(factor(train$class_var)))
#------------------------------------