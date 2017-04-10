#### Train function
#### Input: features, M matrix, the number of clusters, t extension
Train <- function(data.train, M, k, t){
  
  #### Initialization
  
  n <- nrow(data.train) #### Number of publications
  m.matrix <- M
  for(i in 1:t){
    m.matrix <- m.matrix%*%M
  }
  m.matrix <- m.matrix[1:n, 1:n]
  w <- 0.7^t
  label2 <- vector("numeric", n) #### Store train_labels
  label1 <- vector("numeric", n) #### For comparison
  y <- matrix(0, nrow = k, ncol = ncol(data.train))
  A <- matrix(0, nrow = ncol(data.train), ncol = ncol(data.train))
  diag(A) <- rep(1,nrow(A))
  
  #### D function
  
  D <- function(xi,xj,A){
    s <- 1 - as.numeric( (t(xi) %*% A %*% xj)/ sqrt(t(xi) %*% A %*% xi)/ sqrt(t(xj) %*% A %*% xj) )
    return(s)
  }
  
  #### Initial assignments (l1)
  
  label2
  
  
  
  #### Initialize y values
  
  for(i in 1:k){
    y[i,] <- apply(data.train[(label2 == i),], 2, sum)/sum(label2 == i)
  }
  
  #### Initialize A matirx
  
  diag(A) <- rep(1,nrow(A))
  
  #### Iteration functions
  
  while(label2 != label1){
    label1 <- label2
    #### E step
    for(i in 1:n){
      iter.value <- vector("numeric", k)
      for(j in 1:k){
        label2[i] <- j
        fobj <- 0
        for(l in 1:n){
          fobj <- fobj + D(data.train[i,],data.train[l,],A)*(l[i]!=l[l])*w*m.matrix[i,l]
        }
        iter.value[j] <- fobj + D(x[i,],y[j,],A)
      }
      label2[i] <- which.min(iter.value)
    }
    #### M step
    #### Update y
    for(i in 1:k){
      y[i,] <- apply(data.train[(label2 == i),], 2, sum)/sum(label2 == i)
    }
    #### Update A matrix
    for(i in 1:nrow(A)){
      
    }
    
  }
  return(label2)
}