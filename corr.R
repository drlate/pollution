corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  files <- list.files(path=getwd(), pattern="*.csv")
  df <- data.frame(1:length(files))
  
  for(i in 1:nrow(df)){
    raw <- read.csv(file.path(getwd(), files[df[i,1]]), as.is = TRUE)
    
    is_complete <- complete.cases(raw)
    cases <- length(which(is_complete))
  
    if(cases>threshold){
      data <- c(data, raw[is_complete,2:3]) }

    df[i,2] = cases
    
  }
  
  data <- data[-1]
  colnames(df)[1] <- "id"
  colnames(df)[2] <- "cases"
  
  return()
}