library(tidyverse)


getwd()
setwd()

readAllCsvFiles <- function(directory, id = 1:332){
  myDataBase <- dir(directory, full.names = T) %>% map_df(read.csv)
  return(myDataBase)
}

pmean <- function(directory, pollutant, id = 1:332){
  myDataBase <- readAllCsvFiles(directory, id)
  amount = NULL
  for(i in id){
  amount = c(amount, myDataBase[[pollutant]])
  }
  result <- mean(amount, na.rm = T)
  return(result)
}

pmean("specdata", "sulfate", 1:10)
pmean("specdata", "sulfate", 55)
pmean("specdata", "nitrate")


complete <- function(directory, id){
   myDataBase <- readAllCsvFiles(directory, id)
   nobs = NULL
   for (i in id) {
     nobs = c(nobs, nrow(na.omit(myDataBase)))
   }
   result <- data.frame(id, nobs)
   return(result)
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 50:60)


corr <- function(directory, threshold = 0){
  myDataBase <- readAllCsvFiles(directory, id)
  corelations = numeric()
  
  for (i in 1:332) {
    monitor = na.omit(myDataBase)
    if (nrow(monitor) > threshold) {
      corelations = c(corelations, cor(monitor$sulfate, monitor$nitrate))
    }
  }
  return(corelations)
}

cr <- corr("specdata", 150)
print(head(cr)) 
print(summary(cr))


cr <- corr("specdata", 400)
print(head(cr)) 
print(summary(cr))


cr <- corr("specdata", 5000)
print(head(cr)) 
print(summary(cr))
print(length(cr))

