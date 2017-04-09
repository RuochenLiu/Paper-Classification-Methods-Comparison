setwd("~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/data/nameset")

# Needed <- c("tm", "NLP", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
# install.packages(Needed, dependencies=TRUE)
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

library("NLP")
library("tm")


AKumar <- data.frame(scan("AKumar.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE), stringsAsFactors=F)
AKumar$clusterid <- sub("_.*","",AKumar$Coauthor) # author_id
AKumar$citationid <- sub(".*_(\\w*)\\s.*", "\\1", AKumar$Coauthor) # paper_id
AKumar$coauthor <- gsub("<","",sub("^.*?\\s","", AKumar$Coauthor))
AKumar$Paper <- gsub("<","",AKumar$Paper)


# unique(AKumar$Journal)
# length(AKumar$Journal)


# read in Journal name
a <- paste(AKumar$Journal, collapse = " ")
# a = tolower(a) #make it lower case
# a = gsub('[[:punct:]]', '', a)
# a <- strsplit(a, split = " ")
# n <- length(a[[1]])
# n



s <- function(z) { return(strsplit(z, split = " ")) }

dic <- Corpus(VectorSource(AKumar$Journal))
dic <- tm_map(dic, content_transformer(tolower))
dic <- tm_map(dic, removePunctuation)
dic <- tm_map(dic, removeWords, stopwords("english"))
dic <- lapply(dic, s)
dic <- unlist(dic)
dic <- sort(table(dic), decreasing = T)
dic <- dic[-1]
dic <- names(dic)

author <- sort(unique(AKumar$clusterid)) ###?
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


prior_p <- function(j, df) {
  author <- sort(unique(df$clusterid))
  subdata <- subset(df, clusterid == author[j])
  subdic <- Corpus(VectorSource(subdata$Journal))
  subdic <- tm_map(subdic, content_transformer(tolower))
  subdic <- tm_map(subdic, removePunctuation)
  subdic <- tm_map(subdic, removeWords, stopwords("english"))
  subdic <- lapply(subdic, s)
  subdic <- unlist(subdic)
  subdic <- sort(table(subdic), decreasing = T)
  subdic <- subdic[-1]
  sub.p <- find_loc(subdic)
  return(sub.p)
}

q <- matrix(1:J, ncol = 1)
prior.dic <- apply(q, 1, prior_p, AKumar)
prior.author <- table(AKumar$clusterid) / length(AKumar$clusterid)
