acc.test <- function(df, j_p) {
  # split data
  index <- split_data(df)
  train <- df[index, ]
  test <- df[-index, ]
  # train result
  train <- p.journal(train, df, j_p = j_p)
  # calculate posterior probability for each author
  post.p <- matrix(NA, nrow = nrow(test), ncol = K)
  for (k in 1:nrow(test)) {
    subdic <- word.infor(test[k, ])
    # number of different words in sub-dictionary
    n.word <- length(subdic)
    p <- matrix(train[names(subdic), ] * train["p.title.seen", ] + 
                  train["p.word.unseen", ] * train["p.title.unseen", ], ncol = K)
    post.p[k,] <- apply(p, 2, prod) * train["prior.author", ]
  }
  result.class <- author[apply(post.p, 1, which.max)]
  output <- mean(as.numeric(result.class) == test$clusterid)
  return(output)
}


test.result <- function(index, df, j_p) {
  # train result
  train <- df[index, ]
  test <- df[-index, ]
  train <- p.journal(train, df, j_p = j_p)
  # calculate posterior probability for each author
  post.p <- matrix(NA, nrow = nrow(test), ncol = K)
  for (k in 1:nrow(test)) {
    subdic <- word.infor(test[k, ])
    # number of different words in sub-dictionary
    n.word <- length(subdic)
    p <- matrix(train[names(subdic), ] * train["p.title.seen", ] + 
                  train["p.word.unseen", ] * train["p.title.unseen", ], ncol = K)
    post.p[k,] <- apply(p, 2, prod) * train["prior.author", ]
  }
  output <- author[apply(post.p, 1, which.max)]
  return(output)
}
