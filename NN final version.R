#######################################################################################################
# Data Preparation
#######################################################################################################

# importing all necessary libraries
library(keras)
library(tidyverse)
library(caret)
library(caTools)
library(keras)
library(readxl)
library(Metrics)

# importing datasheet containing:
# 1) Mask type
# 2) Air resistance for each mask 
# 3) Water vapor permeability for each mask 
# 4) Face temperature change for each mask 
# 5) Survey comfort rating for each mask
my_data <- read_excel('ML final datasheet.xlsx', sheet="ML data final")

# Split the dataset into training and testing sets using an 85-15 split
sample_split <- sample.split(my_data$Score, SplitRatio = 0.85)
train_data <- t(subset(my_data, sample_split == TRUE))
test_data <- t(subset(my_data, sample_split == FALSE))

# Preparing training dataset

# Make sure each of the parameter columns are stored as numeric variables
a = as.numeric(train_data[2, 1:length(train_data[1,])])
b = as.numeric(train_data[3, 1:length(train_data[1,])])
c = as.numeric(train_data[4, 1:length(train_data[1,])])

# Input variable matrix is composed of each of the three comfort parameter columns
x = as.matrix(data.frame(a,b,c))

# Output variable is the comfort score from the survey (converted to numeric variable)
y = as.numeric(train_data[5, 1:length(train_data[1,])])
y = as.matrix(y)

# Repeat same thing with the testing dataset

a2 = as.numeric(test_data[2, 1:length(test_data[1,])])
b2 = as.numeric(test_data[3, 1:length(test_data[1,])])
c2 = as.numeric(test_data[4, 1:length(test_data[1,])])

x2 = as.matrix(data.frame(a2,b2,c2))

y2 = as.numeric(test_data[5, 1:length(test_data[1,])])
y2 = as.matrix(y2)

#######################################################################################################
# Neural Network Model Creation
#######################################################################################################

# Using Keras to create model

# Input shape for first layer is 3 since there are 3 input comfort parameters

# 1st hidden layer has 256 nodes and uses the 'relu' (Rectified Linear Unit) activation function. This has
# become the standard activation function used for many neural networks because models that use this 
# activation function are easier to train and often perform better. For more detail, 'relu' is a linear 
# piecewise function that outputs the exact input if the result is positive or outputs 0 if the result is 
# negative. 

# 2nd hidden layer has 128 nodes and again uses the 'relu' activation function.

# Output layer has 1 unit (because there should be 1 output, the comfort score). It uses a linear activation 
# function, which is a common output layer function for regression models. 

model = keras_model_sequential() %>% 
  layer_dense(units=256, activation="relu", input_shape=3) %>% 
  layer_dense(units=128, activation = "relu") %>% 
  layer_dense(units=1, activation="linear")

# The 'adam' optimizer was used. Optimizers are used to minimize loss. The Adam optimizer is usually the best 
# adaptive optimizer. Adaptive optimizers are particularly useful with sparse datasets. For this reason, the 
# Adam optimizer was chosen. Although a learning rate does not need to be tuned with adaptive optimizers, a value
# of 0.001 was specified as the learning rate to ensure proper speed of learning by the model. A learning rate that
# is too low will cause the model to be trained extremely slowly. A learning rate that is too high will cause 
# poor performances and abornal behavior. 

opt = optimizer_adam(lr = 0.001)

# Compiling the model to prepare it to run. This model uses the 'mse'  (Mean Squared Error) as the loss function.
model %>% compile(
  loss = "mse",
  optimizer =  opt, 
)

# Training the model using the training set. Running 50 epochs, or iterations of training the model on the entire 
# training set. 
model %>% fit(x, y, epochs = 50,verbose = 0)

# Use the created neural network model to make predictions from the x variables in the testing set
y_pred = model %>% predict(x2)

# Assessing various performance metrics

# Min-max accuracy
prediction_frame <- data.frame(cbind(actual_values = y2, predicted_values = y_pred ))
min_max_accuracy <- mean(apply(prediction_frame, 1, min) / apply(prediction_frame, 1, max))
min_max_accuracy

# RMSE (Root Mean Squared Error)
rmse(y2, y_pred)

# MAE (Mean Absolute Error)
mae(y2, y_pred)

# R^2
rsq <- function (x, y) cor(x, y) ^ 2
rsq(y2, y_pred)

