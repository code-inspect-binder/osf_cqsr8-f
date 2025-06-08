Informedness_Markedness <- function(conf_matrix) {
  
  Sensitivity <- conf_matrix[1,1]/(conf_matrix[1,1]+conf_matrix[2,1])
  Specificity <- conf_matrix[2,2]/(conf_matrix[2,2]+conf_matrix[1,2])
  Precision <- conf_matrix[1,1]/(conf_matrix[1,1]+conf_matrix[1,2])
  negative_predicted_values <- conf_matrix[2,2]/(conf_matrix[2,2]+conf_matrix[2,1])
  
  Informedness <- Sensitivity + Specificity - 1 
  Markedness <- Precision + negative_predicted_values - 1 
  
  Info_Markedness <<- as.data.frame(rbind(Informedness,Markedness))

}


