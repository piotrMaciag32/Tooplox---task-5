install.packages("tensorflow") #if necessary
install.packages("keras") #if necessary

library(keras)

install_keras(tensorflow = "1.11-gpu")
use_implementation("tensorflow")

library(tensorflow)
tfe_enable_eager_execution(device_policy = "silent")

##############################################################
###For near pair (egyptian cat vs persian cat)
##############################################################

  ## For multi task

  #set Path to data
  path = "D:/Tooplox/Data/tiny-imagenet-200/near/egyptian cat vs persian cat/"
  
  #set classified categories
  categories = c("background","egyptian cat", "persian cat")
  
  #Load training and validate data set (validate not presented in experiments)
  #data is loaded using flow_images_from_directory Keras library function (target size refers to the image width and height)
  #as a result a genrator used to build model is returned
  near_training_array_gen = flow_images_from_directory(paste(path, "multi task/training",sep = ""), classes = categories, target_size = c(64, 64))
  near_validating_array_gen = flow_images_from_directory(paste(path, "multi task/validating", sep = ""), classes = categories, target_size = c(64, 64))
  
  #Take dataset size (number of pictures in each generator)
  datasetSize = near_training_array_gen$n
  datasetSizeValidating = near_validating_array_gen$n
  
  #set parameters of images and numb of learning iterations
  img_width = 64
  img_height = 64
  channels = 3 #for RGB
  
  epochNum = 30 #numbers of training iterations
  
  #define model (with sequential layers)
  model <- keras_model_sequential()
  
  model %>%

    layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same", input_shape = c(img_width, img_height, channels)) %>%
    layer_activation("relu") %>%
    
    layer_conv_2d(filter = 16, kernel_size = c(3,3), padding = "same") %>%
    layer_activation_leaky_relu(0.5) %>%
    layer_batch_normalization() %>%
    
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_dropout(0.25) %>%
    
    layer_flatten() %>%
    layer_dense(100) %>%
    layer_activation("relu") %>%
    layer_dropout(0.5) %>%
    
    layer_dense(3) %>% #set number of categories to 3
    layer_activation("softmax")
  
  #compile model
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = tf$train$AdamOptimizer(),
    metrics = "accuracy"
  )
  
  #learn model using fit generator function from Keras
  modelMultiTask <- model %>% fit_generator(near_training_array_gen,
    steps_per_epoch = as.integer(datasetSize / 500), #500 refers to batch size
    epochs = epochNum, 
    validation_data = near_validating_array_gen,
    validation_steps = as.integer(datasetSizeValidating / 500), #500 refers to batch size
    verbose = 2 #print results while tarining model
  )
  
  #save results obtained from model
  resultsMultiTask = data.frame(modelMultiTask$metrics$acc) #remeber results on training data (not validation)
  
  ## For single task
  
  #perform similar steps for the single task (Egyptian cat vs background) as for multi task
  
  #load dataset using generators for Keras
  #load data only for egyptian cat and background (not persian cat)
  
  near_training_array_gen = flow_images_from_directory(paste(path, "single task/training",sep = ""), classes = categories[c(1, 2)], target_size = c(64, 64))
  near_validating_array_gen = flow_images_from_directory(paste(path, "single task/validating", sep = ""), classes = categories[c(1, 2)], target_size = c(64, 64))
  
  datasetSize = near_training_array_gen$n
  datasetSizeValidating = near_validating_array_gen$n
  
  img_width = 64
  img_height = 64
  channels = 3
  
  model <- keras_model_sequential()
  
  model %>%
    
    layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same", input_shape = c(img_width, img_height, channels)) %>%
    layer_activation("relu") %>%
    layer_batch_normalization() %>%
    

    layer_conv_2d(filter = 16, kernel_size = c(3,3), padding = "same") %>%
    layer_activation_leaky_relu(0.5) %>%
    
    

    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_dropout(0.25) %>%
    
    layer_flatten() %>%
    layer_dense(100) %>%
    layer_activation("relu") %>%
    layer_dropout(0.5) %>%
    
    
    layer_dense(2) %>% #set number of classe to 2 (Egyptian cat and background)
    layer_activation("softmax")
  

  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = tf$train$AdamOptimizer(),
    metrics = "accuracy"
  )
  
  modelSingleTask <- model %>% fit_generator(
    near_training_array_gen,
    steps_per_epoch = as.integer(datasetSize / 525), 
    epochs = epochNum, 
    validation_data = near_validating_array_gen,
    validation_steps = as.integer(datasetSizeValidating / 525),
    verbose = 2
  )
  
  resultsSingleTask = data.frame(modelSingleTask$metrics$acc)
  
  results = cbind(resultsMultiTask, resultsSingleTask)
  
  colnames(results) = c("Multi Task", "Single Task")

  resultsElephantVsTeapot = results
  
  
##############################################################
###For far pair (elephant vs teapot)
##############################################################

  
  ## For multi task (elephant vs teapot vs background)
  
  path = "D:/Tooplox/Data/tiny-imagenet-200/far/elephant vs teapot/"
  categories = c("background","elephant", "teapot")
  
  far_training_array_gen = flow_images_from_directory(paste(path, "multi task/training",sep = ""), classes = categories, target_size = c(64, 64))
  far_validating_array_gen = flow_images_from_directory(paste(path, "multi task/validating", sep = ""), classes = categories, target_size = c(64, 64))
  
  datasetSize = far_training_array_gen$n
  datasetSizeValidating = far_validating_array_gen$n
  
  img_width = 64
  img_height = 64
  channels = 3
  
  epochNum = 30
  
  model <- keras_model_sequential()
  
  model %>%
    
    layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same", input_shape = c(img_width, img_height, channels)) %>%
    layer_activation("relu") %>%
    
    layer_conv_2d(filter = 16, kernel_size = c(3,3), padding = "same") %>%
    layer_activation_leaky_relu(0.5) %>%
    layer_batch_normalization() %>%
    
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_dropout(0.25) %>%
    
    layer_flatten() %>%
    layer_dense(100) %>%
    layer_activation("relu") %>%
    layer_dropout(0.5) %>%
    
    layer_dense(3) %>% 
    layer_activation("softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = tf$train$AdamOptimizer(),
    metrics = "accuracy"
  )
  
  #The following
  modelMultiTask <- model %>% fit_generator(
    far_training_array_gen,
    steps_per_epoch = as.integer(datasetSize / 500), 
    epochs = epochNum, 
    validation_data = far_validating_array_gen,
    validation_steps = as.integer(datasetSizeValidating / 500),
    verbose = 2
  )
  
  resultsMultiTask = data.frame(modelMultiTask$metrics$acc)
  
  ## For single task (elephant vs background)
  
  far_training_array_gen = flow_images_from_directory(paste(path, "single task/training",sep = ""), classes = categories[c(1, 2)], target_size = c(64, 64))
  far_validating_array_gen = flow_images_from_directory(paste(path, "single task/validating", sep = ""), classes = categories[c(1, 2)], target_size = c(64, 64))
  
  datasetSize = far_training_array_gen$n
  datasetSizeValidating = far_validating_array_gen$n
  
  img_width = 64
  img_height = 64
  channels = 3
  
  model <- keras_model_sequential()
  
  model %>%
    
    layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same", input_shape = c(img_width, img_height, channels)) %>%
    layer_activation("relu") %>%
    layer_batch_normalization() %>%
    
    layer_conv_2d(filter = 16, kernel_size = c(3,3), padding = "same") %>%
    layer_activation_leaky_relu(0.5) %>%
    
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_dropout(0.25) %>%
    
    layer_flatten() %>%
    layer_dense(100) %>%
    layer_activation("relu") %>%
    layer_dropout(0.5) %>%
    
    layer_dense(2) %>% 
    layer_activation("softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = tf$train$AdamOptimizer(),
    metrics = "accuracy"
  )
  
  modelSingleTask <- model %>% fit_generator(
    far_training_array_gen,
    steps_per_epoch = as.integer(datasetSize / 525), 
    epochs = epochNum, 
    validation_data = far_validating_array_gen,
    validation_steps = as.integer(datasetSizeValidating / 525),
    verbose = 2
  )
  
  resultsSingleTask = data.frame(modelSingleTask$metrics$acc)
  
  results = cbind(resultsMultiTask, resultsSingleTask)
  
  colnames(results) = c("Multi Task", "Single Task")
  
  resultsEgyptianCatVsPersianCat = results
  
  
  
  
