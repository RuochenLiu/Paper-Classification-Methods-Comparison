# Needed <- c("tm", "NLP", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
# install.packages(Needed, dependencies=TRUE)
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

library("NLP")
library("tm")

data.lib <- "~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/data/nameset"
data.files <- list.files(path = data.lib, "*.txt")
AKumar <- read.data(data.files[2])
# train result
ak <- p.journal(AKumar)
