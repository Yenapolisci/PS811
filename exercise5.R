library(readr)
national <- read_csv("Week6/national.csv")

## Function 
## problem 1 

meanf <- function(a) {
  a1<- mean(a)
  return(a1)
}

medianf <- function(a) {
  a2 <- median(a)
  return(a2)
}

stdf <- function(a) {
  a3 <- sd(a)
  return(a3)
}


## problem 2 

meanwo <- function(a) { 
  a4 <- mean(a[-c(which.max(a), which.min(a))]) 
  return(a4)}



## problem 3

r1 <- meanf(national$christianity_all)

mean(national$christianity_all)

r2 <- medianf(national$christianity_all)

median(national$christianity_all)

r3 <- stdf(national$christianity_all)

sd(national$christianity_all)

r4<- meanwo(national$christianity_all)


## problem 4 : Write a function that lists all the unique years with more than 300,000 Christians in total.

yearf <- function(a) {if(national$christianity_all > 3000000) {
  a5 <- unique(a) 
    return(a5)}
}

r5 <- yearf(national$year)

##Loop/Apply

##Problem 1 
for (i in 1:nrow(national)) {
  count <- ncol(national)
  print(count)
}

##Problem 2 

for (i in 1:length(national)) {
  avg_result <- aggregate(national$christianity_protestant, list(national$state), FUN=mean)
}


sapply(split(national$christianity_protestant, national$state), mean)
tapply(national$christianity_protestant, national$state, mean)

##Problem 3

sapply(national, class)
