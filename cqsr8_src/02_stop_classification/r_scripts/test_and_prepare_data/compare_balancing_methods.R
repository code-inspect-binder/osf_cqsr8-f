#----------------
# OVERSAMPLING ####
#----------------

#------------------------------------
# generate training and test data ####

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

# convert class_var into a factor
data_stop$class_var <- as.factor(data_stop$class_var)

# draw train and test data
data_stop$id <- 1:nrow(data_stop)

# define train and test set
train_id <- data_stop %>% dplyr::sample_frac(.75)
test  <- dplyr::anti_join(data_stop, train_id, by = 'id')
#------------------------------------

#----------------------
# upsize the sample ####
x_upSample <- as.data.frame(dplyr::select(train_id,-class_var,id))
y_upSample <- train_id$class_var
train <- upSample(x_upSample,y_upSample,yname = "class_var")
#----------------------

#------------------
# random forest ####

levels(train$class_var) <- make.names(levels(factor(train$class_var)))

# define settings for cross validation
repeats = 1
numbers = 10
tunel = 10

x = trainControl(method = "repeatedcv",
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary,
                 savePredictions = T)

# run random forest 
set.seed(500)
cv_randomForest <- train(class_var ~ avgSamplingInterval + avgSpeed + distanceSlow + distanceToNextHighLevelRoad + distanceToNextLowLevelRoad + duration + minimumSpeed + frcOfNextHighLevelRoad + frcOfNextLowLevelRoad + summedAngles + stopDistance,
                         data = train,
                         method = "rf",
                         trControl = x,
                         metric = "ROC",
                         tuneLength = tunel,
)

# prediction
pred <- predict(cv_randomForest,test, type = "prob")
pred <- apply(pred, 1, which.max)
pred <- ifelse(pred==2,1,0)

# join id-number to predicted data 
prediction <- cbind(test$id,pred)
colnames(prediction) <- c("id", "class_var_pred")

# convert data to data.frame
prediction <- as.data.frame(prediction)

# join predicted to test data 
test_pred <- left_join(test,prediction,by="id")
test_pred <- as.data.frame(test_pred)
test_pred$class_var <- as.factor(test_pred$class_var)
test_pred$class_var_pred <- as.factor(test_pred$class_var_pred)

# confusion matrix
conf_mat_randomForest <- caret::confusionMatrix(data=test_pred$class_var_pred,reference=test_pred$class_var)
#------------------

#--------------------------------
# calculate results for plots ####

# store results for plots
randomForest_accuarcy <- as.data.frame(conf_mat_randomForest$overall)
randomForest_detail <- as.data.frame(conf_mat_randomForest$byClass)
randomForest_conf_table <- conf_mat_randomForest$table

# calc the mathew correlation coefficient
mccr_randomForest <- mccr(test_pred$class_var, test_pred$class_var_pred)
mccr_randomForest <- as.data.frame(cbind("MCC",as.numeric(mccr_randomForest)))
colnames(mccr_randomForest) <- c("indices","Random Forests")

# calc AUC
randomForest_AUC <- as.numeric(pROC::auc(response=as.numeric(test_pred$class_var), predictor=as.numeric(test_pred$class_var_pred)))
randomForest_AUC <- cbind("AUROC",randomForest_AUC)
colnames(randomForest_AUC) <- c("indices","Random Forests")

# calc accuracy and kappa
rf_plot_accuracy <-setDT(randomForest_accuarcy, keep.rownames = TRUE)[]
rf_plot_accuracy <- as.data.frame(dplyr::slice(rf_plot_accuracy, 1:2))
colnames(rf_plot_accuracy) <- c("indices","Random Forests")

# calc sensiticity and specificity
rf_plot_details <- setDT(randomForest_detail, keep.rownames = TRUE)[]
rf_plot_details <- as.data.frame(dplyr::slice(rf_plot_details, c(1:2,4:5,7)))
colnames(rf_plot_details) <- c("indices","Random Forests")

# calc Informedness and Markedness
rf_Info_Markedness <- Informedness_Markedness(randomForest_conf_table)
rf_Info_Markedness <- setDT(rf_Info_Markedness, keep.rownames = TRUE)[]
rf_Info_Markedness <- as.data.frame(rf_Info_Markedness)
colnames(rf_Info_Markedness) <- c("indices","Random Forests")

rf_table_oversampling <- rbind(rf_plot_accuracy,rf_plot_details,mccr_randomForest,rf_Info_Markedness,randomForest_AUC)

colnames(rf_table_oversampling) <- c("indices","Oversampling")
#--------------------------------



#----------------
# UNDERSAMPLING ####
#----------------

#------------------------------------
# generate training and test data ####

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

# convert class_var into a factor
data_stop$class_var <- as.factor(data_stop$class_var)

# draw train and test data
data_stop$id <- 1:nrow(data_stop)

# define train and test set
train_id <- data_stop %>% dplyr::sample_frac(.75)
test  <- dplyr::anti_join(data_stop, train_id, by = 'id')
#------------------------------------

#------------------------
# downsize the sample ####
x_downSample <- as.data.frame(dplyr::select(train_id,-class_var))
y_downSample <- train_id$class_var
train <- downSample(x_downSample,y_downSample,yname = "class_var")
#------------------------

#------------------
# random forest ####

levels(train$class_var) <- make.names(levels(factor(train$class_var)))

# define settings for cross validation
repeats = 1
numbers = 10
tunel = 10

x = trainControl(method = "repeatedcv",
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary,
                 savePredictions = T)

# rund random forest
set.seed(500)
cv_randomForest <- train(class_var ~ avgSamplingInterval + avgSpeed + distanceSlow + distanceToNextHighLevelRoad + distanceToNextLowLevelRoad + duration + minimumSpeed + frcOfNextHighLevelRoad + frcOfNextLowLevelRoad + summedAngles + stopDistance, 
                         data = train, 
                         method = "rf",
                         trControl = x,
                         metric = "ROC",
                         tuneLength = tunel,
)

# prediction
pred <- predict(cv_randomForest,test, type = "prob")
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
conf_mat_randomForest <- caret::confusionMatrix(data=test_pred$class_var_pred,reference=test_pred$class_var)
#------------------

#--------------------------------
# calculate results for plots ####

# store results for plots
randomForest_accuarcy <- as.data.frame(conf_mat_randomForest$overall)
randomForest_detail <- as.data.frame(conf_mat_randomForest$byClass)
randomForest_conf_table <- conf_mat_randomForest$table

# calc the mathew correlation coefficient
mccr_randomForest <- mccr(test_pred$class_var, test_pred$class_var_pred)
mccr_randomForest <- as.data.frame(cbind("MCC",as.numeric(mccr_randomForest)))
colnames(mccr_randomForest) <- c("indices","Random Forests")

# calc AUC
randomForest_AUC <- as.numeric(pROC::auc(response=as.numeric(test_pred$class_var), predictor=as.numeric(test_pred$class_var_pred)))
randomForest_AUC <- cbind("AUROC",randomForest_AUC)
colnames(randomForest_AUC) <- c("indices","Random Forests")

# calc accuracy and kappa
rf_plot_accuracy <-setDT(randomForest_accuarcy, keep.rownames = TRUE)[]
rf_plot_accuracy <- as.data.frame(dplyr::slice(rf_plot_accuracy, 1:2))
colnames(rf_plot_accuracy) <- c("indices","Random Forests")

# calc sensiticity and specificity
rf_plot_details <- setDT(randomForest_detail, keep.rownames = TRUE)[]
rf_plot_details <- as.data.frame(dplyr::slice(rf_plot_details, c(1:2,4:5,7)))
colnames(rf_plot_details) <- c("indices","Random Forests")

# calc Informedness and Markedness
rf_Info_Markedness <- Informedness_Markedness(randomForest_conf_table)
rf_Info_Markedness <- setDT(rf_Info_Markedness, keep.rownames = TRUE)[]
rf_Info_Markedness <- as.data.frame(rf_Info_Markedness)
colnames(rf_Info_Markedness) <- c("indices","Random Forests")

rf_table_undersampling <- rbind(rf_plot_accuracy,rf_plot_details,mccr_randomForest,rf_Info_Markedness,randomForest_AUC)

colnames(rf_table_undersampling) <- c("indices","Undersampling")
#--------------------------------



#----------
# SMOTE ####
#----------

#------------------------------------
# generate training and test data ####

# set seed to replicate the same results
set.seed(500)

# fetch data
data_stop <- read.csv("mock_data/stop_publishing.csv", header = TRUE, sep = ";")

# convert classifying variable
data_stop <- data_stop %>% dplyr::mutate(class_var=ifelse(categoryTarget=="NON_TRAFFIC_RELATED_STOP",1,ifelse(categoryTarget=="TRAFFIC_RELATED_STOP",2,NA)))

#variables according to feature selection
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
#------------------------------------

#---------------------
# sampling - SMOTE ####
train <- SMOTE(class_var ~ avgSamplingInterval + avgSpeed + distanceSlow + distanceToNextHighLevelRoad + distanceToNextLowLevelRoad + duration + minimumSpeed + frcOfNextHighLevelRoad + frcOfNextLowLevelRoad + summedAngles + stopDistance,
               data=train_id,
               k=5,
               perc.over = 100,
               perc.under=200)
#---------------------

#------------------
# random forest ####
levels(train$class_var) <- make.names(levels(factor(train$class_var)))

# define settings for cross validation
repeats = 1
numbers = 10
tunel = 10

x = trainControl(method = "repeatedcv",
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary,
                 savePredictions = T)

# run random forest
set.seed(500)
cv_randomForest <- train(class_var ~ avgSamplingInterval + avgSpeed + distanceSlow + distanceToNextHighLevelRoad + distanceToNextLowLevelRoad + duration + minimumSpeed + frcOfNextHighLevelRoad + frcOfNextLowLevelRoad + summedAngles + stopDistance,
                         data = train, method = "rf",
                         trControl = x,
                         metric = "ROC",
                         tuneLength = tunel,
)

# prediction
pred <- predict(cv_randomForest,test, type = "prob")
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
conf_mat_randomForest <- caret::confusionMatrix(data=test_pred$class_var_pred,reference=test_pred$class_var)
#------------------

#--------------------------------
# calculate results for plots ####

# store results for plots
randomForest_accuarcy <- as.data.frame(conf_mat_randomForest$overall)
randomForest_detail <- as.data.frame(conf_mat_randomForest$byClass)
randomForest_conf_table <- conf_mat_randomForest$table

# calc the mathew correlation coefficient
mccr_randomForest <- mccr(test_pred$class_var, test_pred$class_var_pred)
mccr_randomForest <- as.data.frame(cbind("MCC",as.numeric(mccr_randomForest)))
colnames(mccr_randomForest) <- c("indices","Random Forests")

# calc AUC
randomForest_AUC <- as.numeric(pROC::auc(response=as.numeric(test_pred$class_var), predictor=as.numeric(test_pred$class_var_pred)))
randomForest_AUC <- cbind("AUROC",randomForest_AUC)
colnames(randomForest_AUC) <- c("indices","Random Forests")

# calc accuracy and kappa
rf_plot_accuracy <-setDT(randomForest_accuarcy, keep.rownames = TRUE)[]
rf_plot_accuracy <- as.data.frame(dplyr::slice(rf_plot_accuracy, 1:2))
colnames(rf_plot_accuracy) <- c("indices","Random Forests")

# calc sensiticity and specificity
rf_plot_details <- setDT(randomForest_detail, keep.rownames = TRUE)[]
rf_plot_details <- as.data.frame(dplyr::slice(rf_plot_details, c(1:2,4:5,7)))
colnames(rf_plot_details) <- c("indices","Random Forests")

# calc Informedness and Markedness
rf_Info_Markedness <- Informedness_Markedness(randomForest_conf_table)
rf_Info_Markedness <- setDT(rf_Info_Markedness, keep.rownames = TRUE)[]
rf_Info_Markedness <- as.data.frame(rf_Info_Markedness)
colnames(rf_Info_Markedness) <- c("indices","Random Forests")

rf_table_SMOTE <- rbind(rf_plot_accuracy,rf_plot_details,mccr_randomForest,rf_Info_Markedness,randomForest_AUC)

colnames(rf_table_SMOTE) <- c("indices","SMOTE")
#--------------------------------



#--------------------
# imbalanced data ####
#--------------------

#------------------------------------
# generate training and test data ####

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
train <- data_stop %>% dplyr::sample_frac(.75)
test  <- dplyr::anti_join(data_stop, train, by = 'id')
#------------------------------------

#------------------
# random forest ####

levels(train$class_var) <- make.names(levels(factor(train$class_var)))

# define settings for cross validation
repeats = 1
numbers = 10
tunel = 10

x = trainControl(method = "repeatedcv",
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary,
                 savePredictions = T)

# rund random forest
set.seed(500)
cv_randomForest <- train(class_var ~ avgSamplingInterval + avgSpeed + distanceSlow + distanceToNextHighLevelRoad + distanceToNextLowLevelRoad + duration + minimumSpeed + frcOfNextHighLevelRoad + frcOfNextLowLevelRoad + summedAngles + stopDistance,
                         data = train, 
                         method = "rf",
                         trControl = x,
                         metric = "ROC",
                         tuneLength = tunel,
)

# prediction
pred <- predict(cv_randomForest,test, type = "prob")
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
conf_mat_randomForest <- caret::confusionMatrix(data=test_pred$class_var_pred,reference=test_pred$class_var)
#---------------------

#--------------------------------
# calculate results for plots ####

# store results for plots
randomForest_accuarcy <- as.data.frame(conf_mat_randomForest$overall)
randomForest_detail <- as.data.frame(conf_mat_randomForest$byClass)
randomForest_conf_table <- conf_mat_randomForest$table

# calc the mathew correlation coefficient
mccr_randomForest <- mccr(test_pred$class_var, test_pred$class_var_pred)
mccr_randomForest <- as.data.frame(cbind("MCC",as.numeric(mccr_randomForest)))
colnames(mccr_randomForest) <- c("indices","Random Forests")

# calc AUC
randomForest_AUC <- as.numeric(pROC::auc(response=as.numeric(test_pred$class_var), predictor=as.numeric(test_pred$class_var_pred)))
randomForest_AUC <- cbind("AUROC",randomForest_AUC)
colnames(randomForest_AUC) <- c("indices","Random Forests")

# calc accuracy and kappa
rf_plot_accuracy <-setDT(randomForest_accuarcy, keep.rownames = TRUE)[]
rf_plot_accuracy <- as.data.frame(dplyr::slice(rf_plot_accuracy, 1:2))
colnames(rf_plot_accuracy) <- c("indices","Random Forests")

# calc sensiticity and specificity
rf_plot_details <- setDT(randomForest_detail, keep.rownames = TRUE)[]
rf_plot_details <- as.data.frame(dplyr::slice(rf_plot_details, c(1:2,4:5,7)))
colnames(rf_plot_details) <- c("indices","Random Forests")

# calc Informedness and Markedness
rf_Info_Markedness <- Informedness_Markedness(randomForest_conf_table)
rf_Info_Markedness <- setDT(rf_Info_Markedness, keep.rownames = TRUE)[]
rf_Info_Markedness <- as.data.frame(rf_Info_Markedness)
colnames(rf_Info_Markedness) <- c("indices","Random Forests")

rf_table_imbalanced <- rbind(rf_plot_accuracy,rf_plot_details,mccr_randomForest,rf_Info_Markedness,randomForest_AUC)

colnames(rf_table_imbalanced) <- c("indices","Imbalanced")
#--------------------------------



#----------------------------------------------------------
# generate table to compare results of sampling methods ####
table_balancing_methods <- left_join(rf_table_imbalanced,rf_table_oversampling,by=c("indices")) %>% 
  left_join(., rf_table_undersampling,by=c("indices"))  %>% 
  left_join(., rf_table_SMOTE,by=c("indices"))  
#----------------------------------------------------------