#-------------------
# random forests ####

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
cv_randomForest <- caret::train(class_var ~ avgSamplingInterval + avgSpeed + distanceSlow + distanceToNextHighLevelRoad + distanceToNextLowLevelRoad + duration + minimumSpeed + frcOfNextHighLevelRoad + frcOfNextLowLevelRoad + summedAngles + stopDistance,
                         trControl = x,
                         data = train,
                         method = "rf",
                         metric = "ROC",
                         tuneLength = tunel
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
#-------------------

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

rf_plot <- rbind(rf_plot_accuracy,rf_plot_details,mccr_randomForest,rf_Info_Markedness,randomForest_AUC)
#--------------------------------