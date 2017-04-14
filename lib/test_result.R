library("NLP")
library("tm")

setwd("~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/output")
attach("CleanData.RData")
source("~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/lib/p.function.R", local = T)
source("~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/lib/title.test.R", local = T)

df <- trans.data(1)
index <- read.csv("random_agupta.csv")
index <- index$x

# train result for journal title
result.class.j <- test.result(index, df, "j")
acc.j <- mean(as.numeric(result.class) == test$clusterid)
write.csv(result.class.j, file = "~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/output/agupta.j.pred.csv")
time.journal <- system.time(test.result(index, df, "j"))

# train result for paper title
result.class.p <- test.result(index, df, "p")
acc.p <- mean(as.numeric(result.class) == test$clusterid)
write.csv(result.class.p, file = "~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/output/agupta.p.pred.csv")
time.paper <- system.time(test.result(index, df, "p"))
