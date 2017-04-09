setwd("~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/data/nameset")

# Needed <- c("tm", "NLP", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
# install.packages(Needed, dependencies=TRUE)
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

library("NLP")
library("tm")


AKumar <- data.frame(scan("AKumar.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE), stringsAsFactors=F)
AKumar$clusterid <- sub("_.*","",AKumar$Coauthor) # author_id
AKumar$citationid <- sub(".*_(\\w*)\\s.*", "\\1", AKumar$Coauthor) # paper_id
AKumar$coauthor <- gsub("<","",sub("^.*?\\s","", AKumar$Coauthor))
AKumar$Paper <- gsub("<","",AKumar$Paper)


attach("CleanData.RData")
data.files <- list.files(path = data.lib, "*.txt")
query.list <- substring(data.files, 1, nchar(data.files)-4)
names(data_list) <- query.list

s <- function(z) { return(strsplit(z, split = " ")) }

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
dic <- word.infor(AKumar)
all <- sum(dic)
dic <- names(dic)

author <- sort(unique(AKumar$clusterid))
K <- length(author)

I <- length(dic)
J <- length(author)


find_loc <- function(input) {
  output <- rep(0, I)
  for (i in 1:length(input)) {
    index <- which(dic == names(input)[i])
    output[index] <- input[i] / sum(input)
  }
  return(output)
}

# prior probability to each author in dataset
####### should put variable all in function def?
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

q <- matrix(1:J, ncol = 1)
prior.dic <- apply(q, 1, prior_p, AKumar)
colnames(prior.dic) <- author
row.names(prior.dic) <- c("p.title.seen", "p.title.unseen", dic, "p.word.unseen")
prior.author <- table(AKumar$clusterid) / length(AKumar$clusterid)