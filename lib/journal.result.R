# Needed <- c("tm", "NLP", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
# install.packages(Needed, dependencies=TRUE)
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

library("NLP")
library("tm")

data.lib <- "~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/data/nameset"
data.files <- list.files(path = data.lib, "*.txt")
AKumar <- read.data(data.files[2])
# split data
index <- split_data(AKumar)
train <- AKumar[index, ]
test <- AKumar[-index, ]
# train result
ak.train <- p.journal(train, AKumar)
ak <- p.journal(AKumar, AKumar)

test.dic <- word.infor(test)

# calculate posterior probability for each word
p.word.post <- function(i, j, subdic) {
  output <- ak.train[names(subdic)[i], j] * ak.train["p.title.seen", j] +
    ak.train["p.word.unseen", j] * ak.train["p.title.unseen", j]
  return(log(output))
}

# calculate posterior probability for each author
q <- function(j, df) {
  subdata <- subset(df, clusterid == author[j])
  subdic <- word.infor(subdata)
  # number of different words in sub-dictionary
  n.word <- length(subdic)
  index <- matrix(1:n.word, ncol = 1)
  p <- apply(index, 1, p.word.post, j, subdic)
  output <- prod(p)
  return(output)
}



c("p.title.seen", "p.title.unseen", names(dic), "p.word.unseen", "prior.author")
return(output)