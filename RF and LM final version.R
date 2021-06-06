#######################################################################################################
# Data Preparation
#######################################################################################################

# importing all necessary libraries
library(caret)
library(caTools)
library(randomForest)
library(Metrics)
library(readxl)
set.seed(3456)

# importing datasheet containing:
# 1) Mask type
# 2) Air resistance for each mask 
# 3) Water vapor permeability for each mask 
# 4) Face temperature change for each mask 
# 5) Survey comfort rating for each mask
my_data <- read_excel('ML final datasheet.xlsx', sheet="ML data final")

# First column of datasheet contains mask types (not needed for ML) so selecting only the remaining columns
my_data <- my_data[2:5]

# Make sure the survey comfort rating (called "Score" in datasheet) is stored as a numeric variable
my_data$Score = as.numeric(my_data$Score)
sapply(my_data, class)
my_data <- transform(
  my_data,
  Score=as.numeric(Score)
)

# Split the dataset into training and testing sets using an 85-15 split
sample = sample.split(my_data$Score, SplitRatio = .85)
train = subset(my_data, sample == TRUE)
test  = subset(my_data, sample == FALSE)
dim(train)
dim(test)

#######################################################################################################
# Random Forest (RF) Regression
#######################################################################################################

# The "trainControl" function generates parameters that control how the model is generated. 
# The method "repeatedcv" is used with number = 10 and repeats = 3. This means that a resampling method 
# of 10-fold cross validation repeated 3 times is used. That is, the entire code is iterated 3 times. 
# Each time, the training dataset is divided randomly into 10 portions, and each of these 10 portions is 
# then used as a testing set for a model trained on the other 9 portions. 
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(3333)

# The RF regressor is trained using the training set
# Input parameters are: 
# 1) The predictor (in this case, the Score is the predictor)
# 2) The training dataset
# 3) The model type - 'rf' (Random Forest Regressor)
# 4) The loss metric- 'RMSE' (Root Mean Squared Error)
# 5) The parameters that result from the "trainControl" function above
rf <- train(Score ~., data=train, method="rf", metric='RMSE', trControl=trctrl)
rf

# Use the created RF model to make predictions from the x variables in the testing set
test_pred <- predict(rf, newdata = test)
test_pred

# Assessing various performance metrics

# Min-max accuracy
prediction_frame <- data.frame(cbind(actual_values = test$Score, predicted_values = test_pred))
min_max_accuracy <- mean(apply(prediction_frame, 1, min) / apply(prediction_frame, 1, max))
min_max_accuracy

# RMSE (Root Mean Squared Error)
rmse(test$Score, test_pred)

# MAE (Mean Absolute Error)
mae(test$Score, test_pred)

# R^2
rsq <- function (x, y) cor(x, y) ^ 2
rsq(test$Score, test_pred)

#######################################################################################################
# Multiple Linear Regression
#######################################################################################################

# Same as trainControl method above
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)

# Trained the Linear Regression model ('lm') in the same way as above
lm <- train(Score ~., data=train, method="lm",trControl=trctrl)
lm

# Use the created linear regression model to make predictions from the x variables in the testing set
test_pred <- predict(lm, newdata = test)
test_pred

# Same performance metrics as above

# Min-Max Accuracy
prediction_frame <- data.frame(cbind(actual_values = test$Score, predicted_values = test_pred))
min_max_accuracy <- mean(apply(prediction_frame, 1, min) / apply(prediction_frame, 1, max))
min_max_accuracy

# RMSE (Root Mean Squared Error)
rmse(test$Score, test_pred)

# MAE (Mean Absolute Error)
mae(test$Score, test_pred)

# R^2
rsq <- function (x, y) cor(x, y) ^ 2
rsq(test$Score, test_pred)

# Calculating comfort scores of different masks 

# kn95
maskScore <- lm$finalModel$coefficients[2]*3.65 + lm$finalModel$coefficients[3]*-0.217+lm$finalModel$coefficients[4]*1.46 + lm$finalModel$coefficients[1]
maskScore

# procedural
maskScore <- lm$finalModel$coefficients[2]*0.63 + lm$finalModel$coefficients[3]*-0.3027+lm$finalModel$coefficients[4]*0.5 + lm$finalModel$coefficients[1]
maskScore

# cotton
maskScore <- lm$finalModel$coefficients[2]*0.28 + lm$finalModel$coefficients[3]*-0.421+lm$finalModel$coefficients[4]*0.8 + lm$finalModel$coefficients[1]
maskScore

# polyester
maskScore <- lm$finalModel$coefficients[2]*1.56 + lm$finalModel$coefficients[3]*-0.2833+lm$finalModel$coefficients[4]*0.7 + lm$finalModel$coefficients[1]
maskScore

# knitted
maskScore <- lm$finalModel$coefficients[2]*1.77 + lm$finalModel$coefficients[3]*-0.4182+lm$finalModel$coefficients[4]*0.5 + lm$finalModel$coefficients[1]
maskScore



