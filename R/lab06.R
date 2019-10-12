set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

knapsack_brute_force <- function(x, W){
  stopifnot(is.data.frame(x), names(x) == c("w", "v"),is.numeric(W))
  n <- nrow(x)
  bestValue <- 0
  bestWeight <- 0
  A <- replicate(n, 0)
  for (i in 1:(2^n)){
    j <- n
    tempWeight <- 0
    tempValue <- 0
    while (A[j] != 0 && j > 0) {
      A[j] <- 0
      if (j == 1) {break()}
      else {j <- j - 1}
      }
    A[j] <- 1
    for (k in 1:n){
      if(A[k] == 1){
        tempWeight <- tempWeight + x$w[k]
        tempValue <- tempValue + x$v[k]
      }
    }
    if ((tempValue > bestValue) && (tempWeight <= W)){
      bestValue <- tempValue
      bestWeight <- tempWeight
      bestChoice <- A
    }
  }
  bestChoiceList <- list(
    value = sum(x$v[which(bestChoice ==1)]),
    elements = which(bestChoice ==1)
    )
  return(bestChoiceList)
}


#' 
#' @title Dynamic programming in in knapsack problem
#' @param x A dataframe with two columns: the values (v) and the weights (w) of each item to put in the knapsack.
#' @param W A positive number representing the knapsack size.
#' @return A list of two elements: a positive number with the maximum knapsack \code{value} and a vector of all the \code{elements} in the knapsack size.
#' @example
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),v=runif(n = n, 0, 10000))
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#' @export knapsack_dynamic

knapsack_dynamic <- function(x, W){
  n<-dim(x)[1]
  m<-matrix(ncol=W+1,nrow=n+1) #matrix of alg.
  m[1,]<-rep(0,W+1)
  val<-x$v
  wei<-x$w
  
  #building m[i,j] and looking for the greatest sum lower than W
  for(i in 1:n){    
    for(j in 0:W){
      if(wei[i] > j){
        m[i+1,j+1]<-m[i,j+1]
      }else{
        m[i+1,j+1]<-max(m[i,j+1],m[i,j+1-wei[i]]+val[i])
      }
    }
  }
  
  #looking for the elements from the sum
  
  j=j+1  
  i<-which.max(m[,j]) #row selected is the one of the first element selected
  elements<-length(n)
  k<-1
  elements[k]<-i-1
  
  while(m[i,j]!=0 && j!=1 && i!=0){
    k<-k+1
    j<-(j-wei[i-1])
    i<-which(m[,j] == m[i-1,j])[1]
    elements[k]<-i-1
  }
  
  value<-round(m[n+1,W+1])
  elements<-sort(elements[which(elements>0)])
  
  values<-list(value=value,elements=elements)  
  
  return(values)
}

greedy_knapsack <- function(x, W){
  n<-nrow(x)
  x$weight < -x$v / x$w
  value <- 0
  elements <- replicate(n, 0)
  k<-1
  
  #we find the max value of the weights, we take the position so calculate the sum and to save the elements we are adding
  while((sum(x$w[elements]) + x$w[which.max(x$weight)]) <= W && any(x$weight>0)){
    i<-which.max(x$weight)
    value<-value+x$v[i] 
    elements[k]<-i
    x$weight[i]<-0 
    k<-k+1
  }
  
  value<-round(value)
  elements<-elements[which(elements>0)]
  
  values<-list(value=value,elements=elements)
  
  return(values)
}
