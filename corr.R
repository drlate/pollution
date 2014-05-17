corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  wd <- "G:/R_projects/pollution/"
  setwd(wd)
  sub <- file.path(getwd(),directory)
  setwd(sub)
  
  files <- list.files(getwd(), pattern="*.csv")
  
  sulfate <- vector("numeric")
  nitrate <- vector("numeric")
  correls <- vector("numeric")
  
  for(i in 1:length(files)){
    raw <- read.csv(file.path(getwd(), files[i]), as.is = TRUE)
    
    is_complete <- complete.cases(raw)
    cases <- length(which(is_complete))
  
    if(cases>threshold){
      sulfate <- raw[is_complete, "sulfate"]
      nitrate <- raw[is_complete, "nitrate"]
      correls <- c(correls, cor(sulfate,nitrate))
    }
  }
  
  setwd(wd)
  
  return(correls)
}