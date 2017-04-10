test <- function(df) {
  # split data
  index <- split_data(df)
  train <- df[index, ]
  test <- df[-index, ]
  # train result
  ak.train <- p.journal(train, df)
  # calculate posterior probability for each author
  post.p <- matrix(NA, nrow = nrow(test), ncol = K)
  for (k in 1:nrow(test)) {
    subdic <- word.infor(test[k, ])
    # number of different words in sub-dictionary
    n.word <- length(subdic)
    p <- matrix(ak.train[names(subdic), ] * ak.train["p.title.seen", ] + 
                  ak.train["p.word.unseen", ] * ak.train["p.title.unseen", ], ncol = K)
    post.p[k,] <- apply(p, 2, prod) * ak.train["prior.author", ]
  }
  result.class <- author[apply(post.p, 1, which.max)]
  output <- mean(as.numeric(result.class) == test$clusterid)
  return(output)
}
