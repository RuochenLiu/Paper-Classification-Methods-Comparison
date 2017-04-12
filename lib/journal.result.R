# Needed <- c("tm", "NLP", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
# install.packages(Needed, dependencies=TRUE)
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

library("NLP")
library("tm")

setwd("~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/output")
attach("CleanData.RData")
source("~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/lib/p.function.R", local = T)
source("~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/lib/title.test.R", local = T)

# journal title
final.j <- matrix(NA, nrow = 10, ncol = 14)
for (j in 1:14) {
  df <- trans.data(j)
  for(i in 1:10) {
    final.j[i, j] <- acc.test(df, "j")
  }
}
acc.mean.j <- apply(final.j, 2, mean)
acc.mean.j
acc.sd.j <- apply(final.j, 2, sd)
acc.sd.j
mean(acc.mean.j)
sd(acc.mean.j)
mean(acc.sd.j)
sd(acc.sd.j)
output.j <- rbind(final.j, acc.mean.j, acc.sd.j)
write.csv(output.j, file = "Journal.accuracy.result.csv")

# paper title
final.p <- matrix(NA, nrow = 10, ncol = 14)
for (j in 1:14) {
  df <- trans.data(j)
  for(i in 1:10) {
    final.p[i, j] <- acc.test(df, "p")
  }
}
acc.mean.p <- apply(final.p, 2, mean)
acc.mean.p
acc.sd.p <- apply(final.p, 2, sd)
acc.sd.p
mean(acc.mean.p)
sd(acc.mean.p)
mean(acc.sd.p)
sd(acc.sd.p  )
output.p <- rbind(final.p, acc.mean.p, acc.sd.p)
write.csv(output.p, file = "Paper.accuracy.result.csv")
