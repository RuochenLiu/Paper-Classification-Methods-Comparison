#### Train function
#### Input: features, M matrix, the number of clusters, t extension
Train <- function(d, M, k, t){
  #### Initialization
  n <- length(d) #### Number of publications
  m <- M
  for(i in 1:t){
    m <- m%*%M
  }
  m <- m[1:n, 1:n]
  l <- vector("numeric", n)
  
}