# Needed <- c("tm", "NLP", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
# install.packages(Needed, dependencies=TRUE)
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

library("NLP")
library("tm")


setwd("~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/output")
attach("CleanData.RData")













data.lib <- "~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/data/nameset"
data.files <- list.files(path = data.lib, "*.txt")
AKumar <- read.data(data.files[2])





final <- numeric(length(data.files))
for (zzz in 1:length(data.files)) {
  df <- read.data(data.files[zzz])
  acc <- numeric(10)
  for(zz in 1:10) {
    acc[zz] <- test(df)
  }
  final[zzz] <- mean(acc)
}
final





