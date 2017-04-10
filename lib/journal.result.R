# Needed <- c("tm", "NLP", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
# install.packages(Needed, dependencies=TRUE)
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

library("NLP")
library("tm")


setwd("~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/output")
attach("CleanData.RData")
data.lib <- "~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/data/nameset"
data.files <- list.files(path = data.lib, "*.txt")

final <- matrix(NA, nrow = 10, ncol = length(data.files))
for (j in 1:length(data.files)) {
  df <- trans.data(j)
  for(i in 1:10) {
    final[i, j] <- acc.test(df)
  }
}
acc.mean <- apply(final, 2, mean)
acc.mean
mean(apply(final, 2, mean))

output <- rbind(final, acc.mean)
write.csv(output, file = "Journal.accuracy.result.csv")

