###split data function: split one class data in a nameset into test and train

split_data <- function(m){
  
  #input: m = one class of one nameset
  #output: train and test dataset
  
  
  obs <- nrow(m)
  index <- sample(1:obs,round(obs/2))
  train_data <- m[index,]
  test_data <- m[-index,]
  
  return(list(train_data = train_data, test_data = test_data))
}