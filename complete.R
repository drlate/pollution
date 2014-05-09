complete <- function(directory, id) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  files <- list.files(path=getwd(), pattern="*.csv")
  df <- data.frame(id)
  
  for(i in 1:nrow(df)){
    raw <- read.csv(file.path(getwd(), files[df[i,1]]), as.is = TRUE)
    is_complete <- complete.cases(raw)
    cases <- length(which(is_complete))
    df[i,2] = cases
  }
  colnames(df)[2] <- "nobs"
  return(df)
}

