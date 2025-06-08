#------------
# xgbDART ####

# define settings for cross validation
repeats = 1
numbers = 10
tunel = 10

x = trainControl(method = "repeatedcv",
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

# run xgbDART
set.seed(500)
cv_boosting_xgbDART <- caret::train(class_var ~ avgSamplingInterval + avgSpeed + distanceSlow + distanceToNextHighLevelRoad + distanceToNextLowLevelRoad + duration + minimumSpeed + frcOfNextHighLevelRoad + frcOfNextLowLevelRoad + summedAngles + stopDistance,
                             data = train, 
                             method = "xgbDART",
                             trControl = x,
                             metric = "ROC",
                             tuneLength = tunel)

# prediction
pred <- predict(cv_boosting_xgbDART,test, type = "prob")
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
conf_mat_boosting_xgbDART <- caret::confusionMatrix(data=test_pred$class_var_pred,reference=test_pred$class_var)
#------------


#--------------------------------
# calculate results for plots ####

# store results for plots
boosting_accuarcy_xgbDART <- as.data.frame(conf_mat_boosting_xgbDART$overall)
boosting_detail_xgbDART <- as.data.frame(conf_mat_boosting_xgbDART$byClass)
boosting_table_xgbDART <- conf_mat_boosting_xgbDART$table

# calc the mathew correlation coefficient
mccr_boosting_xgbDART <- mccr(test_pred$class_var, test_pred$class_var_pred)
mccr_boosting_xgbDART <- as.data.frame(cbind("MCC",as.numeric(mccr_boosting_xgbDART)))
colnames(mccr_boosting_xgbDART) <- c("indices","XGBoost")

# calc AUC
Boosting_xgbDART_AUC <- as.numeric(pROC::auc(response=as.numeric(test_pred$class_var), predictor=as.numeric(test_pred$class_var_pred)))
Boosting_xgbDART_AUC <- cbind("AUROC",Boosting_xgbDART_AUC)
colnames(Boosting_xgbDART_AUC) <- c("indices","XGBoost")

# calc accuracy and kappa
boosting_xgbDART_plot_accuracy <- setDT(boosting_accuarcy_xgbDART, keep.rownames = TRUE)[]
boosting_xgbDART_plot_accuracy <- dplyr::slice(boosting_xgbDART_plot_accuracy, 1:2)
colnames(boosting_xgbDART_plot_accuracy) <- c("indices","XGBoost")

# calc sensiticity and specificity
boosting_xgbDART_plot_details <- setDT(boosting_detail_xgbDART, keep.rownames = TRUE)[]
boosting_xgbDART_plot_details <- dplyr::slice(boosting_xgbDART_plot_details, c(1:2,4:5,7))
colnames(boosting_xgbDART_plot_details) <- c("indices","XGBoost")

# calc Informedness and Markedness
boosting_xgbDART_Info_Markedness <- Informedness_Markedness(boosting_table_xgbDART)
setDT(boosting_xgbDART_Info_Markedness, keep.rownames = TRUE)[]
colnames(boosting_xgbDART_Info_Markedness) <- c("indices","XGBoost")

# combinde calculated indices (and add AUC which is calculated in the method file)
boosting_xgbDART_plot <- rbind(boosting_xgbDART_plot_accuracy,boosting_xgbDART_plot_details,mccr_boosting_xgbDART,boosting_xgbDART_Info_Markedness,Boosting_xgbDART_AUC)
#--------------------------------