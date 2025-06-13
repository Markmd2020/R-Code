########Financial Mathematics Example#######

#Load libraries
library(tseries)

#Hypothesis Testing
Prob <- function(n,r){
  tmp <- 0
  for (i in r:n){
    tmp <- tmp + choose(n,i)
  }
  return(tmp/2^n)
}

Prob(10,6)

#Plot t-tests distribution with 10 and 6 degrees of freedom respectively
x <- seq(-4,4,by=0.1)
plot(x,dt(x,10),ylab="f(x)","l")
lines(x,dt(x,10),lty=2) 

#Time Series Analysis

#Retrieve daily returns
google <- get.hist.quote(instrument = "GOOGL",start="2015-1-8",end="2015-3-9",
                         quote="Close",provider=c("yahoo"),compression = "d")
#Plot returns
plot(google,type="l")

#Calculate returns
google.diff  <- diff(google)

#Test for stationarity

#ADF test
adf.test(google.diff)

#Philips - Perron test
PP.test(google.diff)

#Differencing log returns
diff(log(google.diff))

#Fit ARIMA model
google.diff <- ts(google.diff)
google.ar <- ar(google.diff,method="ols")
google.ar$x.mean #Extract parameters

#Residual analysis
plot(google.ar$resid,xlab="time index",ylab="residuals")

#Using Jarque-Bera Test to assess normality of residuals
#The first 16 values are excluded as they are used to fit the model
jarque.bera.test(google.ar$resid[-(1:16)])

#Using Box-Ljung test to check the hypothesis of no autocorrelation
Box.test(google.ar$resid[-(1:16)],type="Ljung")

#Forecast 10 steps ahead
google.pred <- predict(google.ar,n.ahead=10)

#Black Schools Call Formula
black_scholes1 <- function(S,K,r,sigma,T){
  d1 <- (log(S/K) + (r + sigma^2/2)*T/(sigma*sqrt(T)))
  d2 <- d1 - sigma*sqrt(T)
  
  CO <- S*pnorm(d1) - exp(-r*T)*K*pnorm(d2)
  return (c("Call option price"=CO))
}

black_scholes1(100,100,0.01,0.2,1)

#Black Schools Put Formula
black_scholes2 <- function(S,K,r,sigma,T){
  d1 <- (log(S/K) + (r + sigma^2/2)*T/(sigma*sqrt(T)))
  d2 <- d1 - sigma*sqrt(T)
  
  CO <- S*pnorm(d1) - exp(-r*T)*K*pnorm(d2)
  PO <- CO - S + exp(-r*T)*K
  return (c("Call option price"=CO,"put option price"=PO))
}

black_scholes2(100,100,0.01,0.2,1)

#Implied volatility

#Calculate ane errir formula between the market price and the one given
#by the Black Scholes formulat
err <- function(S,K,r,sigma,T,MktPrice) {
  tmp <- abs(MktPrice - black_scholes1(S,K,r,sigma,T))
  return(tmp)
}

#The miminum value returned by the optimise function corresponds to the implied volatility
optimize(err,interval=c(0,5),maximum=FALSE,
         MktPrice=8.43,S=100,K=100,r=0.01,T=1)

#MonteCarlo Simulation to price options
call.monte1.vec <- function(S,K,r,sigma,T,N){
  x <- rnorm(N,0,1)
  y <- S*exp((r-0.5*sigma^2)*T + sigma*sqrt(T)*x) - K
  #Calculation of the price of call option taking sum of positives parts of y
  CO <- sum(y[y>0])*exp(-r*T)/N
  return(c("Price calculated by Monte Carlo simulation"= CO))
}

call.monte1.vec(100,100,0.01,0.2,1,10000) 

#Use moment matching method in MonteCarlo Simulation
call.monte3.vec <- function(S,K,r,sigma,T,N){
  x <- rnorm(N,0,1)
  y <- (x-mean(x))/sd(x)
  y <- S*exp((r-0.5*sigma^2)*T + sigma*sqrt(T)*y) - K
  #Calculation of the price of call option taking sum of positives parts of y
  CO <- sum(y[y>0])*exp(-r*T)/N
  return(c("Price calculated by Monte Carlo simulation"= CO))
}


call.monte3.vec(100,100,0.01,0.2,1,10000)

#Constrained Optimisation Example

#Create Black Scholes formula
black_scholes <- function(S,K,r,sigma,T){
  d1 <- (log(S/K)  + (r+sigma^2/2)*T)/(sigma*sqrt(T))
  d2 <- d1 -sigma*sqrt(T)
  
  CO <- S*pnorm(d1) - exp(-r*T)*K*pnorm(d2)
  return(CO)
}

#Create error function
err2 <- function(S,K,var,T,MktPrice){
  
  tmp <- (MktPrice - black_scholes(S,K,var[1],var[2],T))^2
  
  return(sum(tmp))
}
 
#create parameters
K_sample <- c(80,90,100,110,120)
Mkt_sample <- c(22.75,15.1,8.43,4.72,3.28)
#The resulst shows the optimal risk free rate and the optimal volatility
optim(c(0.01,0.1),err2,MktPrice=Mkt_sample,S=100,K=K_sample,T=1)

