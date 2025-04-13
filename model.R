# Importing libraries
library(caret) # Package for machine learning algorithms
library(ggplot2)

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

 # Compare scatter plot of the 80 and 20 data subsets
#scatter plot for training set
#Training plot<-ggplot(TrainingSet,aes(x = Sepal.Length, y = Sepal.Width, colour = species))+
# geom_point()+
#labs(title = "Scatter Plot: Training Set(80%)")

#Display the plots
#print(Training plot)
#print(Testing plot)




###############################
# SVM model (polynomial kernel)

# Build Training model
Model <- train(Species ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Build CV model
Model.cv <- train(Species ~ ., data = TrainingSet,
                   method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv", number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1))


# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set
Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Species)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Species)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

#calculate the additional metric: Accuracy, Precision, Recall, F1Score
calculate_metrics <- function (conf_matrix) {
  accuracy <- conf_matrix$overall["Accuracy"]
   precision <- mean(Model.training.confusion$byClass)["Precision"]
  recall <- min(Model.training.confusion$byClass)["Recall"]
  f1_score <- 2 * (Precision * recall) /(precision + recall)
  
  metrics <- data.frame(
  metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  value = c(accuracy, precision, recall, f1_score))
    
  return (metrics)
}
  #training metrics
  training_metrics <- calculate_metrics
  (Model.training.confusion)
    print("training Metrics:") 
    print(training_metrics)
    
   #Visualize the confusion matrix
    visualize_confusion_matrix<- function(conf_matrix, title) 
      {
      cm<-as.data.frame(conf_matrix$table)
      colnames(cm)<- c("Actual", "Prediction", "Freq")
      
      ggplot(cm, aes(x = Prediction, y = Actual))+
        geom_tile(aes(fill = Freq), color = "white")+
        scale_fill_gradient(low = "white", high = "blue")+
        geom_text(aes(label = Freq), vjust = 1)+
        ggtitle(title)+
        theme_minimal()
      }
      #plot confusion matrix
      plot_training_cm <- visualize_confusion_matrix(Model.training.confusion,
      "training Set Confusion Matrix")
      plot_testing_cm <-visualize_confusion_matrix(Model.testing.confusion,
     "testing confusion matrix")
    print(plot_training_cm)
    print(plot_testing_cm)
    
     # Feature importance
    Importance <- varImp(Model)
    plot(Importance)
    plot(Importance, col = "red")
    
    
    
    
    
    
  
 