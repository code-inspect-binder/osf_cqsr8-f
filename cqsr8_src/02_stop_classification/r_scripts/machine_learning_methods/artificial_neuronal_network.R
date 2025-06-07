#--------
# ANN ####

# define settings for cross validation
repeats = 1
numbers = 10
tunel = 10

x = trainControl(method = "repeatedcv",
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

# run ANN
set.seed(500)
cv_neuralnet <- caret::train(class_var ~ avgSamplingInterval + avgSpeed + distanceSlow + distanceToNextHighLevelRoad + distanceToNextLowLevelRoad + duration + minimumSpeed + frcOfNextHighLevelRoad + frcOfNextLowLevelRoad + summedAngles + stopDistance,
                      data = train,
                      method = "nnet",
                      trControl = x,
                      tuneLength = tunel)

# prediction
pred <- predict(cv_neuralnet,test, type = "prob")
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
nnet_conf_mat_neuralnet <- caret::confusionMatrix(data=test_pred$class_var_pred,reference=test_pred$class_var)
#------------

#--------------------------------
# calculate results for plots ####

# store results for plots
neuralnet_accuarcy <- as.data.frame(nnet_conf_mat_neuralnet$overall)
neuralnet_detail <- as.data.frame(nnet_conf_mat_neuralnet$byClass)
neuralnet_table <- nnet_conf_mat_neuralnet$table

# calc the mathew correlation coefficient
mccr_neuralnet <- mccr(test_pred$class_var, test_pred$class_var_pred)
mccr_neuralnet <- as.data.frame(cbind("MCC",as.numeric(mccr_neuralnet)))
colnames(mccr_neuralnet) <- c("indices","Neuronal networks")

# calc AUC
Neuralnet_AUC <- as.numeric(pROC::auc(response=as.numeric(test_pred$class_var), predictor=as.numeric(test_pred$class_var_pred)))
Neuralnet_AUC <- cbind("AUROC",Neuralnet_AUC)
colnames(Neuralnet_AUC) <- c("indices","Neuronal networks")

# calc accuracy and kappa
neuronal_plot_accuracy <- setDT(neuralnet_accuarcy, keep.rownames = TRUE)[]
neuronal_plot_accuracy <- dplyr::slice(neuronal_plot_accuracy, 1:2)
colnames(neuronal_plot_accuracy) <- c("indices","Neuronal networks")

# calc sensiticity and specificity
neuronal_plot_details <- setDT(neuralnet_detail, keep.rownames = TRUE)[]
neuronal_plot_details <- dplyr::slice(neuronal_plot_details, c(1:2,4:5,7))
colnames(neuronal_plot_details) <- c("indices","Neuronal networks")

# calc Informedness and Markedness
knn_Info_Markedness <- Informedness_Markedness(neuralnet_table)
setDT(knn_Info_Markedness, keep.rownames = TRUE)[]
colnames(knn_Info_Markedness) <- c("indices","Neuronal networks")

# combinde calculated indices (and add AUC which is calculated in the method file)
neuronal_plot <- rbind(neuronal_plot_accuracy,neuronal_plot_details,mccr_neuralnet,knn_Info_Markedness,Neuralnet_AUC)
#--------------------------------