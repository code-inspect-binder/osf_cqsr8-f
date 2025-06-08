# -----------------------------------
# generate training and test data ####
# (feature selection is applied only on training data)

# set seed
set.seed(500)

# fetch data
data_stop <- read.csv("mock_data/stop_publishing.csv", header = TRUE, sep = ";")

# convert classifying variable
data_stop <- data_stop %>% dplyr::mutate(class_var=ifelse(categoryTarget=="NON_TRAFFIC_RELATED_STOP",1,ifelse(categoryTarget=="TRAFFIC_RELATED_STOP",2,NA)))

# select usefull variables
data_stop <- data_stop %>% dplyr::select(duration,stopDistance,avgSpeed,minimumSpeed,avgSamplingInterval,summedAngles,significantDirectionChanges,distanceSlow,distanceToNextHighLevelRoad,frcOfNextHighLevelRoad,fowOfNextHighLevelRoad,distanceToNextLowLevelRoad,frcOfNextLowLevelRoad,fowOfNextLowLevelRoad,class_var)

# choose only complete cases without NA'S
data_stop <- data_stop[complete.cases(data_stop), ]

# normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_stop <- as.data.frame(lapply(data_stop, normalize))

# convert class_var into a factor
data_stop$class_var <- as.factor(data_stop$class_var)

# draw train and test data
data_stop$id <- 1:nrow(data_stop)
train <- data_stop %>% dplyr::sample_frac(.75)
test  <- dplyr::anti_join(data_stop, train, by = 'id')
# -----------------------------------

# -------------------------------
# generate plot for obb error ####

# delete target variable (class_var) to run carSelRF package
temp_train <- dplyr::select(train,-class_var,-id)

# run varSelRF package for feature selection
rf.vs1 <- varSelRF(temp_train, train$class_var, ntree = 800,
                   vars.drop.frac = 0.2,
                   ntreeIterat = 800)

# generate plot
plot(rf.vs1, nvar = 11, which = c(2))
# -------------------------------

# --------------------------------------
# generate plot MeanDecreaseAccuracy ####

plot_rf.vs1 <- as.data.frame(rf.vs1$initialImportances)
setDT(plot_rf.vs1, keep.rownames = TRUE)[]

plot_rf.vs1 <- plot_rf.vs1[order(-MeanDecreaseAccuracy),]
plot_rf.vs1 <- plot_rf.vs1[1:11,]
plot_rf.vs1$rnames <- c("distance to next high-level road","average speed","FRC of next low-level road","track length","slow distance","minimum speed","distance to next low-level road","summed angles","duration","FRC of next high-level road","average sampling interval")

ggplot2::ggplot(plot_rf.vs1, aes(x=MeanDecreaseAccuracy*100,y=reorder(rnames,MeanDecreaseAccuracy))) +
  geom_point() + 
  ylab("") + 
  xlab("Mean decrease in accuracy in %") + 
  ggtitle("Variable importance")  +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) + 
  theme(axis.title.x = element_text(size = 13)) + 
  theme(title = element_text(size = 13))
# --------------------------------------