library("NLP")
library("tm")

setwd("~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/output")
attach("CleanData.RData")
source("~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/lib/p.function.R", local = T)
source("~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/lib/title.test.R", local = T)

df <- trans.data(1)
index <- read.csv("random_agupta.csv")
index <- index$x
train <- df[index, ]
test <- df[-index, ]

time.j <- function()
{# train result for journal title
train.j <- p.journal(train, df, j_p = "j")
# calculate posterior probability for each author
post.p <- matrix(NA, nrow = nrow(test), ncol = K)
for (k in 1:nrow(test)) {
  subdic <- word.infor(test[k, ])
  # number of different words in sub-dictionary
  n.word <- length(subdic)
  p <- matrix(train.j[names(subdic), ] * train.j["p.title.seen", ] + 
                train.j["p.word.unseen", ] * train.j["p.title.unseen", ], ncol = K)
  post.p[k,] <- apply(p, 2, prod) * train.j["prior.author", ]
}
result.class.j <- author[apply(post.p, 1, which.max)]
acc.j <- mean(as.numeric(result.class) == test$clusterid)}
write.csv(result.class.j, file = "~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/output/agupta.j.pred.csv")

# train result for paper title
time.p <- function()
{train.p <- p.journal(train, df, j_p = "p")
# calculate posterior probability for each author
post.p <- matrix(NA, nrow = nrow(test), ncol = K)
for (k in 1:nrow(test)) {
  subdic <- word.infor(test[k, ])
  # number of different words in sub-dictionary
  n.word <- length(subdic)
  p <- matrix(train.p[names(subdic), ] * train.p["p.title.seen", ] + 
                train.p["p.word.unseen", ] * train.p["p.title.unseen", ], ncol = K)
  post.p[k,] <- apply(p, 2, prod) * train.p["prior.author", ]
}
result.class.p <- author[apply(post.p, 1, which.max)]
acc.p <- mean(as.numeric(result.class) == test$clusterid)}
write.csv(result.class.p, file = "~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/output/agupta.p.pred.csv")
