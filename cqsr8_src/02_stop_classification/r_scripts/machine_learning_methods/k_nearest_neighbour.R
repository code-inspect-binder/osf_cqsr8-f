#------------------------------------
# generate training and test data ####

# set seed
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
train <- data_stop %>% dplyr::sample_frac(.75)
test  <- dplyr::anti_join(data_stop, train, by = 'id')
levels(train$class_var) <- make.names(levels(factor(train$class_var)))
#------------------------------------

#--------
# KNN ####

# define settings for cross validation
repeats = 1
numbers = 10
tunel = 10 

x = trainControl(method = "repeatedcv",
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

# run KNN
set.seed(500)
cv_knn <- train(class_var ~ avgSamplingInterval + avgSpeed + distanceSlow + distanceToNextHighLevelRoad + distanceToNextLowLevelRoad + duration + minimumSpeed + frcOfNextHighLevelRoad + frcOfNextLowLevelRoad + summedAngles + stopDistance, 
                data = train, 
                method = "kknn",
                trControl = x,
                metric = "ROC",
                tuneLength = tunel)


# prediction
pred <- predict(cv_knn,test, type = "prob")
pred <- apply(pred, 1, which.max)
pred <- ifelse(pred==2,1,0)

# join id-number to predicted data 
prediction<-cbind(test$id,pred)
colnames(prediction)<-c("id", "class_var_pred")

# convert data to data.frame
prediction <- as.data.frame(prediction)

# join predicted to test data 
test_pred <- left_join(test,prediction,by="id")
test_pred <- as.data.frame(test_pred)
test_pred$class_var <- as.factor(test_pred$class_var)
test_pred$class_var_pred <- as.factor(test_pred$class_var_pred)

# confusion matrix
conf_mat_knn <- caret::confusionMatrix(data=test_pred$class_var_pred,reference=test_pred$class_var)
#--------

#--------------------------------
# calculate results for plots ####

# store results for plots
knn_accuarcy <- as.data.frame(conf_mat_knn$overall)
knn_detail <- as.data.frame(conf_mat_knn$byClass)
knn_table <- conf_mat_knn$table

# calc the mathew correlation coefficient
mccr_knn <- mccr(test_pred$class_var, test_pred$class_var_pred)
mccr_knn <- as.data.frame(cbind("MCC",as.numeric(mccr_knn)))
colnames(mccr_knn) <- c("indices","KNN")

# calc AUC
KNN_AUC <- as.numeric(pROC::auc(response=as.numeric(test_pred$class_var), predictor=as.numeric(test_pred$class_var_pred)))
KNN_AUC <- cbind("AUROC",KNN_AUC)
colnames(KNN_AUC) <- c("indices","KNN")

# calc accuracy and kappa
knn_plot_accuarcy <- setDT(knn_accuarcy, keep.rownames = TRUE)[]
knn_plot_accuarcy <- dplyr::slice(knn_plot_accuarcy, 1:2)
colnames(knn_plot_accuarcy) <- c("indices","KNN")

# calc sensiticity and specificity
knn_plot_details <- setDT(knn_detail, keep.rownames = TRUE)[]
knn_plot_details <- dplyr::slice(knn_plot_details, c(1:2,4:5,7))
colnames(knn_plot_details) <- c("indices","KNN")

# calc Informedness and Markedness
knn_Info_Markedness <- Informedness_Markedness(knn_table)
setDT(knn_Info_Markedness, keep.rownames = TRUE)[]
colnames(knn_Info_Markedness) <- c("indices","KNN")

# combinde calculated indices (and add AUC which is calculated in the method file)
knn_plot <- rbind(knn_plot_accuarcy,knn_plot_details,mccr_knn,knn_Info_Markedness,KNN_AUC)
#--------------------------------