M6 <- function(d){
  
  n <- length(d)
  
  ## P Matrix
  
  P <- matrix( rep(0, n*n), nrow = n)
  diag(P) <- rep(1,n)
  
  ## Coauthor List
  
  co <- NULL
  for(i in 1:n){
    co <- c(co, d[[i]][[3]])
  }
  co <- unique(co)
  
  ## AP Matrix
  
  m <- length(co)
  AP <- matrix(rep(0,n*m), nrow = n)
  for(i in 1:m){
    for( j in 1:n){
      if(co[i] %in% d[[j]][[3]]){
        AP[j,i] <- 1
      }
    }
  }
  
  ## AA Matrix
  
  AA <- matrix(rep(0, m*m), nrow = m)
  for(i in 1:m){
    for(j in 1:m){
      for(k in 1:n){
        if(co[i] %in% d[[k]][[3]] & co[j] %in% d[[k]][[3]]){
          AA[i,j] <- 1
          break
        }
      }
    }
  }
  
  ## M Matrix
  
  M1 <- cbind(P, AP)
  M2 <- cbind(t(AP), AA)
  M <- rbind(M1,M2)
  
  return(M)
}
