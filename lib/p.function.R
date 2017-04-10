library("NLP")
library("tm")

# read in a new datasset
read.data <- function(input) {
  df <- paste(data.lib, input, sep="/")
  df <- data.frame(scan(df, what = list(Coauthor = "", Paper = "", Journal = ""),
                        sep=">", quiet=TRUE), stringsAsFactors=F)
  df$clusterid <- sub("_.*","",df$Coauthor) # author_id
  df$citationid <- sub(".*_(\\w*)\\s.*", "\\1", df$Coauthor) # paper_id
  df$coauthor <- gsub("<","",sub("^.*?\\s","", df$Coauthor))
  df$Paper <- gsub("<","",df$Paper)
  return(df)
}

#transpose list into data frame


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
  dic <- lapply(dic, dec)
  dic <- unlist(dic)
  dic <- sort(table(dic), decreasing = T)
  if ("" %in% names(dic)) { dic <- dic[-which(names(dic) == "")]}
  return(dic)
}

# find word location in the total dictionary and compute the probability of each word
find_loc <- function(input) {
  output <- rep(0, L)
  for (i in 1:length(input)) {
    index <- which(dic == names(input)[i])
    output[index] <- input[i] / sum(input)
  }
  return(output)
}

# prior probability to each author in dataset
prior.author.p <- function(j, df) {
  subdata <- subset(df, clusterid == author[j])
  subdic <- word.infor(subdata)
  # probability of the author publish a paper on a journal with a seen word in the journal title
  p.title.seen <- sum(subdic[subdic >= 2]) / sum(subdic)
  # probability of the author publish a paper on a journal with a unseen word in the journal title
  p.title.unseen <- 1 - p.title.seen
  # probability of the author publish a paper on a journal with kth word condition on the jornal name has a seen word
  p.word.seen <- find_loc(subdic)
  # probability of the author publish a paper on a journal with kth word condition on the jornal name has a unseen word
  p.word.unseen <- 1 / (n.all)
  output <- c(p.title.seen, p.title.unseen, p.word.seen, p.word.unseen)
  return(output)
}

# split data
split_data <- function(df){
  author <- unique(df$clusterid)
  output <- numeric(0)
  for(i in 1:length(author)) {
    obs <- which(df$clusterid == author[i])
    random <- sample(obs, round(length(obs)/2))
    output <- c(output, random)
  }
  return(output)
}

# train function
p.journal <- function(df, total_df) {
  # the total word dictionary
  dic <- word.infor(total_df)
  n.all <<- sum(dic)
  dic <<- names(dic)
  author <<- sort(unique(total_df$clusterid))
  K <<- length(author)
  L <<- length(dic)
  q <- matrix(1:K, ncol = 1)
  prior.dic <- apply(q, 1, prior.author.p, df)
  prior.author <- table(df$clusterid) / length(df$clusterid)
  output <- rbind(prior.dic, prior.author)
  colnames(output) <- author
  row.names(output) <- c("p.title.seen", "p.title.unseen", names(dic), "p.word.unseen", "prior.author")
  return(output)
}
