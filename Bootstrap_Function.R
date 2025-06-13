#Bootstrap function
boot <- function(x,B,func,...){
  #x is a data vector or matrix with each row a case
  #B is number of bootstrap replications
  #func is R function that inputs a data vector or matrix 
  
  x <- as.matrix(x)
  n <- nrow(x)
  f0 <- func(x,...)
  fmat <- matrix(0,length(f0),B)
  for (b in 1:B){
    i <- sample(1:n,n,replace=TRUE)
    fmat[,b] <- func(x[i,],...)
  }
  drop(fmat)
}

#Test it
x <-c(1,2,3,3,2,2,3,4)
boot(x=x,B=100,mean)