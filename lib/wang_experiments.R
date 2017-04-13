###Ten Experiments Function#########

experiment <- function(train_files, test_files){
  
  #input: 10 random sampled train sets and test sets of one nameset
  #output: the mean and standard deviation of accuracy
  
  model <- NULL
  prediction <- NULL
  accuracy <- NA
  for(i in 1:10){
    train_x <- as.matrix(train_files[[i]][,-1])
    train_y <- train_files[[i]][,1]
    model[[i]] <- nb_train(train_x, train_y)
    prediction[[i]] <- nb_test(model[[i]], test_files[[i]][,-1])
    accuracy[i] <- mean(prediction[[i]] == test_files[[i]][,1])
  }
  return(list(mean = mean(accuracy), StdDev = sd(accuracy)))
}
