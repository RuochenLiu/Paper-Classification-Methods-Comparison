install.packages("knitr")
library(knitr)
source('~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/lib/evaluation_measures.R', local = T)
source('~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/lib/test_result.R', local = T)

result.class.c <- read.csv("coauthor_pred.csv")
result.class.c <- result.class.c$x

matching_matrix_coauthor <- matching_matrix(as.numeric(df$clusterid[-index]), result.class.c)
performance_coauthor <- performance_statistics(matching_matrix_coauthor)
matching_matrix_journal <- matching_matrix(as.numeric(df$clusterid[-index]),result.class.j)
performance_journal <- performance_statistics(matching_matrix_journal)
matching_matrix_paper <- matching_matrix(as.numeric(df$clusterid[-index]), result.class.p)
performance_paper <- performance_statistics(matching_matrix_paper)
compare_df <- data.frame(method = c("coauthor", "paper", "journal"),
                         precision = c(performance_coauthor$precision, performance_paper$precision, performance_journal$precision),
                         recall = c(performance_coauthor$recall, performance_paper$recall, performance_journal$recall),
                         f1 = c(performance_coauthor$f1, performance_paper$f1, performance_journal$f1),
                         accuracy = c(performance_coauthor$accuracy, performance_paper$accuracy, performance_journal$accuracy),
                         time = c(time.coauthor, time.paper[3], time.journal[3]))
compare_df
kable(compare_df,caption = "Comparision of performance for two clustering methods", digits = 2)



