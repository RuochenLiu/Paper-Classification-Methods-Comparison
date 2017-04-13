##naive bayes train function##

nb_train <- function(train_x, train_y){
  
  
  d <- ncol(train_x)
  id <- unique(train_y)
  k <- length(id)
  P_No <- NA
  P_Co <- NA
  co_times <- matrix(NA, nrow = k, ncol = d)
  
  for(i in 1:k){
    ##prob of writing without coauthors and with ones
    row_sum <- rowSums(train_x)
    index <- train_y == id[i]
    row_sum_id <- row_sum[index]
    No_ind <- row_sum_id == 1
    P_No[i] <- mean(No_ind)
    P_Co[i] <- 1 - P_No[i]
    
    ##prob of wrting witho seen coauthors and without ones
    train_x_id <- train_x[index,]
    dimension <- ifelse(is.matrix(train_x_id), 2, 1)
    if(dimension == 1){
      co_times[i,] <- train_x_id
    }
    else if(dimension == 2){
      co_times[i,] <- colSums(train_x_id)
    }
  }
  co_times_omt<- co_times[, -d]
  seen_times <- apply(co_times_omt, 1, function(x){sum(x[x >= 2])})
  total_times <- rowSums(co_times_omt)
  P_Seen_Co <- seen_times / total_times
  P_Unseen_Co <- 1 - P_Seen_Co
  
  ##
  P_A1k_Seen_Co <- prop.table(co_times_omt, 1)
  P_A1k_Unseen_Co <- 1 / (d - rowSums(co_times_omt))
  
  ##
  prop <- prop.table(co_times, 2)
  Prior <- prop[,d]
  
  return(list(Prior = Prior,
              P_No = P_No,
              P_Co = P_Co, 
              P_Seen_Co = P_Seen_Co, 
              P_Unseen_Co = P_Unseen_Co,
              P_A1k_Seen_Co = P_A1k_Seen_Co,
              P_A1k_Unseen_Co = P_A1k_Unseen_Co
              ))
}
