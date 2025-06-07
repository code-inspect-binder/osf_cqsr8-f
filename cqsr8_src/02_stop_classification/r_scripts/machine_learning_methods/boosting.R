#-------------
# boosting ####

# define settings for cross validation
repeats = 1
numbers = 10
tunel = 10

x = trainControl(method = "repeatedcv",
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

# run boosting
set.seed(500)
cv_boosting <- caret::train(class_var ~ avgSamplingInterval + avgSpeed + distanceSlow + distanceToNextHighLevelRoad + distanceToNextLowLevelRoad + duration + minimumSpeed + frcOfNextHighLevelRoad + frcOfNextLowLevelRoad + summedAngles + stopDistance, 
                     data = train, 
                     method = "AdaBoost.M1",
                     trControl = x,
                     metric = "ROC",
                     tuneLength = tunel)


# prediction
pred <- predict(cv_boosting,test, type = "prob")
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
conf_mat_boosting <- caret::confusionMatrix(data=test_pred$class_var_pred,reference=test_pred$class_var)
#-------------

#--------------------------------
# calculate results for plots ####

# store results for plots
boosting_accuarcy <- as.data.frame(conf_mat_boosting$overall)
boosting_detail <- as.data.frame(conf_mat_boosting$byClass)
boosting_table <- conf_mat_boosting$table

# calc the mathew correlation coefficient
mccr_boosting <- mccr(test_pred$class_var, test_pred$class_var_pred)
mccr_boosting <- as.data.frame(cbind("MCC",as.numeric(mccr_boosting)))
colnames(mccr_boosting) <- c("indices","AdaBoost.M1")

# calc AUC
Boosting_AUC <- as.numeric(pROC::auc(response=as.numeric(test_pred$class_var), predictor=as.numeric(test_pred$class_var_pred)))
Boosting_AUC <- cbind("AUROC",Boosting_AUC)
colnames(Boosting_AUC) <- c("indices","AdaBoost.M1")

# calc accuracy and kappa
boosting_plot_accuracy <- setDT(boosting_accuarcy, keep.rownames = TRUE)[]
boosting_plot_accuracy <- dplyr::slice(boosting_plot_accuracy, 1:2)
colnames(boosting_plot_accuracy) <- c("indices","AdaBoost.M1")

# calc sensiticity and specificity
boosting_plot_details <- setDT(boosting_detail, keep.rownames = TRUE)[]
boosting_plot_details <- dplyr::slice(boosting_plot_details, c(1:2,4:5,7))
colnames(boosting_plot_details) <- c("indices","AdaBoost.M1")

# calc Informedness and Markedness
boosting_Info_Markedness <- Informedness_Markedness(boosting_table)
setDT(boosting_Info_Markedness, keep.rownames = TRUE)[]
colnames(boosting_Info_Markedness) <- c("indices","AdaBoost.M1")

# combinde calculated indices (and add AUC which is calculated in the method file)
boosting_plot <- rbind(boosting_plot_accuracy,boosting_plot_details,mccr_boosting,boosting_Info_Markedness,Boosting_AUC)
#--------------------------------