class_output = read.csv("classification-output-data.csv")
#Raw Confusion Matrix
conf_matrix=table(class_output$class,class_output$scored.class)
cat("Confusion Matrix\n")
print(conf_matrix)
#Rows represent acctual 
sum(conf_matrix[1,])==sum(class_output$class==0)
sum(conf_matrix[2,])==sum(class_output$class==1)

#Columns represent Predicted value
sum(conf_matrix[,1])==sum(class_output$scored.class==0)
sum(conf_matrix[,2])==sum(class_output$scored.class==1)


#Function to compute accuracy
accuracy=function(df){
  #True positive - Actual class is positive and predicted class is positive
  TP = sum((df$class==1) & (df$scored.class==1))
  #True Negative - Actual class is negative and predicted class is negative
  TN = sum((df$class==0) & (df$scored.class==0))
  #False positive - Actual class is negative and predicted class is positive
  FP = sum((df$class==0) & (df$scored.class==1))
  #False Negative - Actual class is positive and predicted class is negative
  FN = sum((df$class==1) & (df$scored.class==0))
  #Compute Accuracy
  accuracy = (TP+TN)/(TP+TN+FP+FN)
  return(accuracy)
}

cat("\nAccuracy:",accuracy(class_output))

#Function to compute precision
precision=function(df){
  #True positive - Actual class is positive and predicted class is positive
  TP = sum((df$class==1) & (df$scored.class==1))
  #False positive - Actual class is negative and predicted class is positive
  FP = sum((df$class==0) & (df$scored.class==1))
  #Compute Precision 
  precision = TP/(TP+FP)
  return(precision)
}

cat("\nPrecision:",precision(class_output))

#Function to compute Specificity 
specificity=function(df){
  #True Negative - Actual class is negative and predicted class is negative
  TN = sum((df$class==0) & (df$scored.class==0))
  #False positive - Actual class is negative and predicted class is positive
  FP = sum((df$class==0) & (df$scored.class==1))
  #Compute Specificity
  specificity = TN/(TN+FP)
  return(specificity)
}

cat("Specificity:",specificity(class_output))
