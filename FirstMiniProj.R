#NUMBER 1 POLLUTANT MEAN
pollutantmean<-function(directory, pollutant, id=1:332){
  mylist <- list.files(path = directory, pattern = ".csv")
  x <- numeric()
  for( i in id){
    mydata <- read.csv(mylist[i])
    x <- c(x,mydata[[pollutant]])
  }
  mean(x, na.rm = TRUE)
}
pollutantmean("C:/Users/Acer/Desktop/specdata","sulfate",1:10)

#NUMBER 2 COMPLETE
complete <- function(directory, id = 1:332) {
  mylist <- list.files(path = directory, pattern = ".csv")
  nobs <- numeric()
  for( i in id){
    mydata <- read.csv(mylist[i])
    mysum <- sum(complete.cases(mydata))
    nobs <- c(nobs, mysum)
  }
  data.frame(id, nobs)
}
complete ("C:/Users/Acer/Desktop/specdata", 1) 


#NUMBER 3 CORR
corr <- function(directory, threshold=0){
  mylist <- list.files(path = directory, pattern = ".csv")
  mydir <- complete(directory)
  ids <- mydir[mydir["nobs"] > threshold, ]$id
  cors <- numeric()
  for( i in ids){
    mydata <- read.csv(mylist[i])
    mydir2 <- mydata[complete.cases(mydata), ]
    cors<- c(cors, cor(mydir2$sulfate, mydir2$nitrate))
  }
  return(cors)
}
cr <- corr("C:/Users/Acer/Desktop/specdata", 150)
head(cr);summary(cr) 


#NUMBER 4 30 DAY MORTALITY
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], xlab = "Deaths", main = "Hospital 30-Day Death (MOrtality) Rates from Heart Attack")
