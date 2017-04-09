library("NLP")
library("tm")

# decompose a string into words
dec <- function(z) {
  return(strsplit(z, split = " "))
}

# count word frequency and sort
word.infor <- function(df) {
  dic <- Corpus(VectorSource(df$Journal))
  dic <- tm_map(dic, content_transformer(tolower))
  dic <- tm_map(dic, removePunctuation)
  dic <- tm_map(dic, removeWords, stopwords("english"))
  dic <- lapply(dic, s)
  dic <- unlist(dic)
  dic <- sort(table(dic), decreasing = T)
  dic <- dic[-1]
  return(dic)
}

# find word location in the total dictionary and compute the probability of each word
find_loc <- function(input) {
  output <- rep(0, I)
  for (i in 1:length(input)) {
    index <- which(dic == names(input)[i])
    output[index] <- input[i] / sum(input)
  }
  return(output)
}

# prior probability to each author in dataset
prior_p <- function(j, df) {
  author <- sort(unique(df$clusterid))
  subdata <- subset(df, clusterid == author[j])
  subdic <- word.infor(subdata)
  # probability of the author publish a paper on a journal with a seen word in the journal title
  p.title.seen <- sum(subdic[subdic >= 2]) / sum(subdic)
  # probability of the author publish a paper on a journal with a unseen word in the journal title
  p.title.unseen <- 1 - p.seen
  # probability of the author publish a paper on a journal with kth word condition on the jornal name has a seen word
  p.word.seen <- find_loc(subdic)
  # probability of the author publish a paper on a journal with kth word condition on the jornal name has a unseen word
  p.word.unseen <- 1 / (all)
  output <- c(p.title.seen, p.title.unseen, p.word.seen, p.word.unseen)
  return(output)
}

# train function
p.journal <- function(df) {
  # the total word dictionary
  dic <- word.infor(df)
  all <- sum(dic)
  dic <- names(dic)
  author <- sort(unique(df$clusterid))
  K <- length(author)
  I <- length(dic)
  q <- matrix(1:K, ncol = 1)
  prior.dic <- apply(q, 1, prior_p, df)
  prior.author <- table(df$clusterid) / length(df$clusterid)
  output <- rbind(prior.dic, prior.author)
  colnames(output) <- author
  row.names(output) <- c("p.title.seen", "p.title.unseen", dic, "p.word.unseen", "prior.author")
  return(output)
}
