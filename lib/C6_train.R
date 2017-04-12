#### Train function
#### Input: features, M matrix, the number of clusters, t extension
Train <- function(data.train, M, k, t){
  
  #### Initialization
  
  n <- nrow(data.train) #### Number of publications
  f <- ncol(data.train) #### Number of features
  m.matrix <- M
  for(i in 1:t){
    m.matrix <- m.matrix%*%M
  }
  m.matrix <- m.matrix[1:n, 1:n]
  w <- 0.7^t
  label2 <- vector("numeric", n) #### Store train_labels
  label1 <- vector("numeric", n) #### For comparison
  y <- matrix(0, nrow = k, ncol = f)
  A <- matrix(0, nrow = f, ncol = f)
  diag(A) <- rep(1,nrow(A))
  
  #### D function
  
  D <- function(xi,xj,A){
    s <- 1 - as.numeric( (t(xi) %*% A %*% xj)/ sqrt(t(xi) %*% A %*% xi)/ sqrt(t(xj) %*% A %*% xj) )
    return(s)
  }
  
  #### A Norm
  
  Anorm <- function(xi,A){
    return(sqrt(t(xi) %*% A %*% xi))
  }
  
  #### Gradient function
  
  gradient <- function(xi,xj,A){
    g <- vector("numeric", nrow(A))
    for(i in 1:nrow(A)){
      part1 <- xi[i]*xj[i]*Anorm(xi,A)*Anorm(xj,A)
      part2 <- t(xi)%*%A%*%xj*((xi[i]^2)*(Anorm(xi,A)^2) + (xj[i]^2)*(Anorm(xj,A)^2))/2/Anorm(xi,A)/Anorm(xj,A)
      part3 <- (Anorm(xi,A)^2)*(Anorm(xj,A)^2)
      g[i] <- (part1 - part2)/part3
    }
    return(g)
  }
  
  
  #### Initial assignments (l1)
  
  while(length(table(label2)) != k){
    label2 <- sample(1:k, n, replace =  TRUE)
  }
  
  #### Initialize y values
  
  for(i in 1:k){
    if(sum(label2 == i) > 1){
      y[i,] <- apply(data.train[(label2 == i),], 2, sum)/sum(label2 == i)
    }
    else if(sum(label2 == i) == 1){
      y[i,] <- data.train[(label2 == i),]
    }
    else{
      y[i,] <- rep(0,p)
    }
  }
  
  #### Initialize A matirx
  
  diag(A) <- rep(1,nrow(A))
  
  #### Iteration functions
  
  while(sum(label1 != label2) > n/100){
    label1 <- label2
    #### E step
    for(i in 1:n){
      iter.value <- rep(0,k)
      for(j in 1:k){
        label2[i] <- j
        fobj <- 0
        for(l in 1:n){
          fobj <- fobj + D(data.train[i,],data.train[l,],A)*(label2[i]!=label2[l])*w*(m.matrix[i,l] >0)
        }
        iter.value[j] <- fobj + D(data.train[i,],y[j,],A)
      }
      label2[i] <- which.min(iter.value)
    }
    #### M step
    #### Update y
    for(i in 1:k){
      if(sum(label2 == i) > 1){
        y[i,] <- apply(data.train[(label2 == i),], 2, sum)/sum(label2 == i)
      }
      if(sum(label2 == i) == 1){
        y[i,] <- data.train[(label2 == i),]
      }
      if(sum(label2 == i) == 0){
        y[i,] <- rep(0,f)
      }
    }
    #### Update A matrix
    delta <- rep(0,f)
    part1 <- matrix(nrow = n, ncol = f)
    for(i in 1:n){
      if(sum(label2 == label2[i]) > 1){
        xa <- apply(data.train[label2 == label2[i], ], 2, sum)
      }
      if(sum(label2 == label2[i]) == 1){
        xa <- data.train[label2 == label2[i], ]
      }
      if(sum(label2 == label2[i]) == 0){
        xa <- rep(0,f)
      }
      part1[i,] <- gradient(data.train[i,], y[label2[i],]*sqrt(t(xa) %*% A %*% xa), A)
    }
    part1 <- colSums(part1)
    part2 <- rep(0,f)
    for(i in 1:n){
      for(j in 1:n){
        if(m.matrix[i,j]>0 & label2[i] != label2[j]){
          part2 <- part2 + gradient(data.train[i,], data.train[j,], A)
        }
      }
    }
    delta <- part1 + part2
    for(i in 1:f){
      A[i,i] <- A[i,i] + 0.005*delta[i]
    }
    
  }
  return(label2)
}