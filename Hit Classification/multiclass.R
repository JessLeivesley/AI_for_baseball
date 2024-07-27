# goal : build a multi-class classifier to predict baseball events.

# tools
library(tidyverse)
library(tensorflow)
library(keras)

# events of interest :
  
# when the ball is hit into play :
# - single
# - double
# - triple
# - home_run
# - out

# when the ball isn't hit into play :
# - ball
# - strike
# - foul
# - blocked_ball
# - hit_by_pitch

# predictors :
# - release_speed
# - plate_x
# - plate_z

# clean data.
# using springer data. 
data <- read_csv('Data/Batter Data/2023_Springer.csv')

# different types of strikes are just classified under strike.
# all types of fouls are classified under foul.
# when description is 'hit_into_play' outcome is under events. 
data <- data |>
  select(release_speed, plate_x, plate_z, events, description) |>
  mutate(description = if_else(str_detect(description, 'strike'), 'strike', description)) |>
  mutate(description = if_else(str_detect(description, 'foul'), 'foul', description)) |>
  mutate(events = if_else(
    description == 'hit_into_play' & !(events %in% c('single', 'double', 'triple', 'home_run')), 
    'out', events
  )) |>
  mutate(description = case_when(
    description == 'hit_into_play' ~ events,
    TRUE ~ description
  )) |>
  select(-events)

# transform description into indicators. 
data <- data |>
  mutate(value = 1) |>
  pivot_wider(names_from = description, values_from = value, values_fill = list(value = 0))

### split data into training, testing, and validation sets. ###
# randomly shuffle data
set.seed(25)
shuffled_indices <- sample(nrow(data))
shuffled_data <- data[shuffled_indices, ]


training_prop <- 0.7
validation_prop <- 0.15

train_num <- floor(training_prop * nrow(data))
val_num <- floor(validation_prop * nrow(data)) + train_num

training_set <- shuffled_data[1:train_num , ]
validation_set <- shuffled_data[(train_num + 1):val_num , ]
testing_set <- shuffled_data[(val_num + 1):nrow(data) , ]

# split the features and targets
x_train <- scale(as.matrix(training_set[, 1:3]))
y_train <- as.matrix(training_set[, 4:ncol(training_set)])

x_valid <- scale(as.matrix(validation_set[, 1:3]))
y_valid <- as.matrix(validation_set[, 4:ncol(validation_set)])

x_test <- scale(as.matrix(testing_set[, 1:3]))
y_test <- as.matrix(testing_set[, 4:ncol(testing_set)])


### build model ###
build_model <- function(neurons_hidden) {
  
  model <- keras_model_sequential() |>
    layer_dense(units = neurons_hidden, activation = 'relu', input_shape = c(3)) |>
    layer_dense(units = 10, activation = 'softmax')
  
  # Compile the model
  model |> compile(
    optimizer = optimizer_adam(),
    loss = 'categorical_crossentropy',
    metrics = c('accuracy')
  )
  
  return(model)
}


### train model ###
results <- data.frame(
  neurons_hidden = integer(),
  accuracy = numeric()
)


for (neurons_hidden in seq(from = 3, to = 10)) {
  model <- build_model(neurons_hidden)
  
  model_history <- model |> fit(
    x_train, y_train, 
    epochs = 10, 
    batch_size = 35, 
    validation_data = list(x_valid, y_valid),
  )
  
  # evaulate model
  model_eval <- model |> evaluate(x_valid, y_valid)
  
  # results
  results <- rbind(results, data.frame(
    neurons_hidden = neurons_hidden,
    accuracy = model_eval['accuracy']
  ))
  
  print(paste("Hidden Neurons:", neurons_hidden, "Accuracy:", model_eval['accuracy']))
}

# best config
best_model <- results[which.max(results$accuracy), ]
print("Best Configuration: ")
print(best_model)

