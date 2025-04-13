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


 
