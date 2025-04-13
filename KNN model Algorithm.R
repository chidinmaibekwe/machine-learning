# Importing libraries
library(caret) # Package for machine learning algorithms
library(ggplot2)
library(class)
# Importing the Iris dataset
irs <- datasets::iris
irs <- data.frame(irs)
View(iris)

# missing values
sum(is.na(irs))

#apply column transformation
#build a  function
col_names_transformation <- function(x){
  new_names = names(x)
  new_names = gsub("\\.", "", new_names)
  
  names(x) = new_names
  return(x)
}

irs <- col_names_transformation(irs)

# To achieve reproducible model; set the random seed number
set.seed(189)

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(irs$Species, p=0.8, list = FALSE)
TrainingSet <- irs[TrainingIndex,] # Training Set
TestingSet <- irs[-TrainingIndex,] # Test Set

#KNN model
knn_model<- train(Species ~.,data  = TrainingSet, 
                  method = "knn",
                  trControl = trainControl(method = "none"),
                  tuneGrid = data.frame(k = 5))

#KNN CV MODEL
knn_cv_model <- train(Species ~ ., data = TrainingSet,
                   method = "knn",
                   trControl = trainControl(method = "cv", number = 10),
                   tuneGrid = data.frame(k=5))

#Apply Knn model for prediction
knn_model_training <- predict(knn_model,TrainingSet)
knn_model_testing <- predict(knn_model, TestingSet)
knn_cv_model <- predict(knn_cv_model,TrainingSet)

#Knn model performance
knn_training_confusion <- confusionMatrix(knn_model_training,TrainingSet$Species)
knn_testing_confusion <- confusionMatrix(knn_model_testing, TestingSet$Species)
knn_cv_confusion <- confusionMatrix(knn_cv_model, TrainingSet$Species)

print(knn_training_confusion)
print(knn_testing_confusion)
print(knn_cv_confusion)

#calculate the additional metric: Accuracy, Precision, Recall, F1Score
calculate_metrics <- function (conf_matrix) {
  accuracy <- conf_matrix$overall["Accuracy"]
  precision <- mean(conf_matrix$byClass)["Precision"]
  recall <- mean(conf_matrix$byClass)["Recall"]
  f1_score <- 2 * (Precision * recall) /(precision + recall)
  
  metrics <- data.frame(
    metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
    value = c(accuracy, precision, recall, f1_score))
  
  return (metrics)
}

