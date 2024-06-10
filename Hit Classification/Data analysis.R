## Example script for running data analysis
## 7 June 2024

library(dplyr)
library(tidymodels)
library(tensorflow)
library(keras)
library(ggplot2)
library(caret)

# Read in data
# I'm going to use Bo Bichette's 2023 data
bichette_2023 <- read.csv("Batter Data/2023_Bichette.csv")

# Look at the structure of the data
str(bichette_2023)

# Binary hit or not variable
bichette_2023$hit_outcome<-ifelse(is.na(bichette_2023$hc_x)==TRUE, 0, 1)

ggplot(data = bichette_2023) + 
  geom_point(aes(x = plate_x, y = plate_z, col = factor(hit_outcome)), alpha = 0.2) +
  annotate("rect", xmin = -0.71, xmax = 0.71, ymin = mean(bichette_2023$sz_bot), ymax = mean(bichette_2023$sz_top),fill="NA", col="black")

ggplot(data = bichette_2023) + 
  geom_point(aes(x = plate_x, y = plate_z), alpha = 0.2) +
  annotate("rect", xmin = -0.71, xmax = 0.71, ymin = mean(bichette_2023$sz_bot), ymax = mean(bichette_2023$sz_top),fill="NA", col="black") + 
  facet_wrap(~hit_outcome)

# When running a ML analysis, it is best practice to split data in training, validating, and testing data. Eventually, we will also introduce cross-validation. We fit the model with the training data, and validate it after each epoch with the validate data. At the end of the model fitting process we will test how well the model performs on the unseen test data. 

set.seed(73)
split<-initial_split(bichette_2023,strata = hit_outcome, prop=0.80)
train<-training(split)
val_test<-testing(split)
split2<-initial_split(val_test,strata = hit_outcome, prop=0.50)
validate<-training(split2)
test<-testing(split2)

# Check that the distributions of the split are even.
ggplot()+
  geom_density(data = train, aes(x = hit_outcome), fill = "red", alpha = 0.25) + 
  geom_density(data = validate, aes(x = hit_outcome), fill = "green", alpha = 0.25) + 
  geom_density(data = test, aes(x = hit_outcome), fill = "blue", alpha = 0.25)

ggplot()+
  geom_density(data = train, aes(x = plate_x), fill = "red", alpha = 0.25) + 
  geom_density(data = validate, aes(x = plate_x), fill = "green", alpha = 0.25) + 
  geom_density(data = test, aes(x = plate_x), fill = "blue", alpha = 0.25)

ggplot()+
  geom_density(data = train, aes(x = plate_z), fill = "red", alpha = 0.25) + 
  geom_density(data = validate, aes(x = plate_z), fill = "green", alpha = 0.25) + 
  geom_density(data = test, aes(x = plate_z), fill = "blue", alpha = 0.25)

x_train<-as.matrix(train[,c(30,31)])
scaler <- preProcess(x_train, method = 'scale')
x_train <- predict(scaler, x_train)
y_train<-train[,93]

x_validate<-as.matrix(validate[,c(30,31)])
x_validate <- predict(scaler, x_validate)
y_validate<-validate[,93]

x_test<-as.matrix(test[,c(30,31)])
x_test <- predict(scaler, x_test)
y_test<-test[,93]

model1 <- keras_model_sequential()
model1 %>%
  layer_dense(units = 2, input_shape = c(2)) %>%
  layer_activation_relu()%>%
  layer_dense(units = 1, activation = "sigmoid")

model1 %>% compile(
  loss = 'binary_crossentropy',
  optimizer =  optimizer_adam(),
  metrics = c('accuracy'))

model_history <- model1 %>% 
  fit(
  x_train, y_train,
  batch_size = 500, 
  epochs = 150,
  validation_data = list(x_validate,y_validate))

evaluate(model1, x_test, y_test) 

preds<-predict(model1, x = x_test) # This is the probability of a hit

test$hit_prob<-preds

ggplot(data = test) + 
  geom_density(aes(x = hit_prob, group = hit_outcome, fill = factor(hit_outcome)), alpha = 0.25)

hit.predictions <- ifelse(preds > 0.5 , 1, 0)
hit.predictions<-as.factor(hit.predictions)

confusionMatrix(hit.predictions,as.factor(test$hit_outcome))

# All hit predictions are the of the most common factor. There are three ways to deal with this: either we can subsample the no hits to create balanced classes, only use the last pitch of the at bat (likely will still be unbalanced but better), or assign a higher weight in the model to hits. 

# Assign class weights
summary(factor(train$hit_outcome))
1237/319 # roughly 3.9 times more misses than hits
summary(factor(validate$hit_outcome))
265/69 # 3.8 times more
summary(factor(test$hit_outcome))
266/69 # 3.9

model1 <- keras_model_sequential()
model1 %>%
  layer_dense(units = 2, input_shape = c(2)) %>%
  layer_activation_relu()%>%
  layer_dense(units = 1, activation = "sigmoid")

model1 %>% compile(
  loss = 'binary_crossentropy',
  optimizer =  optimizer_adam(),
  metrics = c('accuracy'))

model_history <- model1 %>% 
  fit(
    x_train, y_train,
    batch_size = 100, 
    epochs = 50,
    validation_data = list(x_validate,y_validate),
    class_weight = list("0"=1,"1"=3.9))

evaluate(model1, x_test, y_test) 

preds<-predict(model1, x = x_test) # This is the probability of a hit

test$hit_prob<-preds

ggplot(data = test) + 
  geom_density(aes(x = hit_prob, group = hit_outcome, fill = factor(hit_outcome)), alpha = 0.25)

hit.predictions <- ifelse(preds > 0.5 , 1, 0)
hit.predictions<-as.factor(hit.predictions)

confusionMatrix(hit.predictions,as.factor(test$hit_outcome))

## Now try adding in other predictors
ggplot()+
  geom_density(data = train, aes(x = release_speed), fill = "red", alpha = 0.25) + 
  geom_density(data = validate, aes(x = release_speed), fill = "green", alpha = 0.25) + 
  geom_density(data = test, aes(x = release_speed), fill = "blue", alpha = 0.25)

ggplot()+
  geom_density(data = train, aes(x = release_pos_x), fill = "red", alpha = 0.25) + 
  geom_density(data = validate, aes(x = release_pos_x), fill = "green", alpha = 0.25) + 
  geom_density(data = test, aes(x = release_pos_x), fill = "blue", alpha = 0.25)

ggplot()+
  geom_density(data = train, aes(x = release_pos_z), fill = "red", alpha = 0.25) + 
  geom_density(data = validate, aes(x = release_pos_z), fill = "green", alpha = 0.25) + 
  geom_density(data = test, aes(x = release_pos_z), fill = "blue", alpha = 0.25)

x_train<-as.matrix(train[,c(3:5,30,31)])
scaler <- preProcess(x_train, method = 'scale')
x_train <- predict(scaler, x_train)
y_train<-train[,93]

x_validate<-as.matrix(validate[,c(3:5,30,31)])
x_validate <- predict(scaler, x_validate)
y_validate<-validate[,93]

x_test<-as.matrix(test[,c(3:5,30,31)])
x_test <- predict(scaler, x_test)
y_test<-test[,93]

model1 <- keras_model_sequential()
model1 %>%
  layer_dense(units = 10, input_shape = c(5)) %>%
  layer_activation_relu()%>%
  layer_dense(units = 5) %>%
  layer_activation_relu()%>%
  layer_dense(units = 1, activation = "sigmoid")

model1 %>% compile(
  loss = 'binary_crossentropy',
  optimizer =  optimizer_adam(),
  metrics = c('accuracy'))

model_history <- model1 %>% 
  fit(
    x_train, y_train,
    batch_size = 100, 
    epochs = 50,
    validation_data = list(x_validate,y_validate),
    class_weight = list("0"=1,"1"=3.9))

evaluate(model1, x_test, y_test) 

preds<-predict(model1, x = x_test) # This is the probability of a hit

test$hit_prob<-preds

ggplot(data = test) + 
  geom_density(aes(x = hit_prob, group = hit_outcome, fill = factor(hit_outcome)), alpha = 0.25)

hit.predictions <- ifelse(preds > 0.5 , 1, 0)
hit.predictions<-as.factor(hit.predictions)

confusionMatrix(hit.predictions,as.factor(test$hit_outcome))
