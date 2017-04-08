###Naive Bayes test function###

nb_test <- function(model, test_data){
  n <- dim(test_data)[1]
  d <- dim(test_data)[2]
  test_data_omt <- test_data[,-d]
  K <- rowSums(test_data_omt)
  n_id <- length(model$Prior)
  posterior <- matrix(NA, nrow = n, ncol = n_id)
  class <- NA
  
  for(i in 1:n){
    N <- K[i]
    name_ind <- test_data_omt[i,] == 1
    for(j in 1:n_id){
      if(N == 0){
        posterior[i,j] <- model$P_No[j] * model$Prior[j]
      }
      else if(N >= 1){
        posterior[i,j] <- prod( model$P_A1k_Seen_Co[j, name_ind] * model$P_Seen_Co[j] * model$P_Co[j] +   rep(model$P_A1k_Unseen_Co[j], sum(name_ind)) * model$P_Unseen_Co[j] * model$P_Co[j]) * model$Prior[j]
      }
    }
    class[i] <- which.max(posterior[i,])
  }
  return(prediction = class)
} 