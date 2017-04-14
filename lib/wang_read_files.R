###read 10 experiments data sets###

read_files <- function(num_nameset){
  
  #input: which nameset to read (indicated by number)
  #output: 10 experiments train and test data set for that nameset
  
  trainset <- paste("/train_", num_nameset, ".csv", sep = "")
  testser <- paste("/test_", num_nameset, ".csv", sep = "")
  
  train_files <- NULL
  for(i in 1:10){
    file_dir <- paste("~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/output/Exp",i,trainset,sep = "")
    train_files[[i]] <- read.csv(file_dir, header = T)
  }
  
  test_files <- NULL
  for(i in 1:10){
    file_dir <- paste("~/Desktop/sem 2/Applied data science/Spr2017-proj4-team-14/output/Exp",i,trainset,sep = "")
    test_files[[i]] <- read.csv(file_dir, header = T)
  }
  return(list(train_files = train_files,test_files = test_files))
}