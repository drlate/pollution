pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  wd <- "G:/R_projects/pollution/"
  sub <- file.path(getwd(),directory)
  setwd(sub)
  files <- list.files(getwd())
  
  for (i in id){
    raw <- read.csv(files[i], as.is=TRUE)
    data <- c(data, list(cleanNA(raw, pollutant)))
  }
  
  data <- data[-1]
  setwd(wd)
  return(round(mean(unlist(data)),digits=3))
  
}

cleanNA <- function(raw, pollutant) {
  valid<-!is.na(raw[,pollutant])
  return(raw[valid, pollutant])
  rm(valid)
}
