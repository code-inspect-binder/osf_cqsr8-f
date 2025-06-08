#------------
# bagging ####

# define settings for cross validation
repeats = 1
numbers = 10
tunel = 10

x = trainControl(method = "repeatedcv",
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

# run bagging
set.seed(500)
cv_bagging <- caret::train(class_var ~ avgSamplingInterval + avgSpeed + distanceSlow + distanceToNextHighLevelRoad + distanceToNextLowLevelRoad + duration + minimumSpeed + frcOfNextHighLevelRoad + frcOfNextLowLevelRoad + summedAngles + stopDistance,
                    data = train,
                    method = "treebag",
                    trControl = x,
                    metric = "ROC",
                    tuneLength = tunel)


# prediction
pred <- predict(cv_bagging,test, type = "prob")
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
conf_mat_bagging <- caret::confusionMatrix(data=test_pred$class_var_pred,reference=test_pred$class_var)
#------------

#--------------------------------
# calculate results for plots ####

# store results for plots
bagging_accuarcy <- as.data.frame(conf_mat_bagging$overall)
bagging_detail <- as.data.frame(conf_mat_bagging$byClass)
bagging_conf_table <- conf_mat_bagging$table

# calc the mathew correlation coefficient
mccr_bagging <- mccr(test_pred$class_var, test_pred$class_var_pred)
mccr_bagging <- as.data.frame(cbind("MCC",as.numeric(mccr_bagging)))
colnames(mccr_bagging) <- c("indices","Bagging")

# calc AUC
Bagging_AUC <- as.numeric(pROC::auc(response=as.numeric(test_pred$class_var), predictor=as.numeric(test_pred$class_var_pred)))
Bagging_AUC <- cbind("AUROC",Bagging_AUC)
colnames(Bagging_AUC) <- c("indices","Bagging")

# calc accuracy and kappa
bagging_plot_accuracy <- setDT(bagging_accuarcy, keep.rownames = TRUE)[]
bagging_plot_accuracy <- as.data.frame(dplyr::slice(bagging_plot_accuracy, 1:2))
colnames(bagging_plot_accuracy) <- c("indices","Bagging")

# calc sensiticity and specificity
bagging_plot_details <- setDT(bagging_detail, keep.rownames = TRUE)[]
bagging_plot_details <- dplyr::slice(bagging_plot_details, c(1:2,4:5,7))
colnames(bagging_plot_details) <- c("indices","Bagging")

# calc Informedness and Markedness
bagging_Info_Markedness <- Informedness_Markedness(bagging_conf_table)
setDT(bagging_Info_Markedness, keep.rownames = TRUE)[]
colnames(bagging_Info_Markedness) <- c("indices","Bagging")

# combinde calculated indices (and add AUC which is calculated in the method file)
bagging_plot <- rbind(bagging_plot_accuracy,bagging_plot_details,mccr_bagging,bagging_Info_Markedness,Bagging_AUC)
#--------------------------------