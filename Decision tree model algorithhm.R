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

#decision tree model
dt_model <- train(Species ~ ., data = TrainingSet, 
                  method = "rpart",
                  trControl = trainControl(method = "none"))

#decision tree cv model
dt_cv_model <- train(Species~ ., data = TrainingSet,
                     method = "rpart",
                     trControl = trainControl(method = "cv", number = 10))

#Applying decision tree model for prediction
dt_model_training <- predict(dt_model, TrainingSet)
dt_model_testing <- predict(dt_model, TestingSet)
dt_cv_model <- predict(dt_cv_model, TrainingSet)

#Decision tree model performance
dt_training_confusion <- confusionMatrix(dt_model_training,TrainingSet$Species)
dt_testing_confusion <- confusionMatrix(dt_model_testing,TestingSet$Species)
dt_cv_confusion <- confusionMatrix(dt_cv_model, TrainingSet$Species)

print(dt_training_confusion)
print(dt_testing_confusion)
print(dt_cv_confusion)

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




