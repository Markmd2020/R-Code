#######Bayesian Methods Examples######

#Load libraries
library(MASS)
library(coda)

#Set seed to ensure reproducibility
set.seed(135)

#Chapter 1:  Beta Distributions Example#
a<-2 ; b<-20
a/(a+b)
(a-1)/(a-1+b-1)
pbeta(.20,a,b) - pbeta(.05,a,b)
pbeta(.10,a,b)


par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))

dbinom(0,20,.05)
n<-20
x<-0:n
del<-.25
plot( range(x-del), c(0,.4),xlab="number infected in the sample",
      ylab="probability",type="n")

points( x-del,dbinom(x,n,.05),type="h",col=gray(.75),lwd=3)
points( x,dbinom(x,n,.10),type="h",col=gray(.5),lwd=3)
points( x+del,dbinom(x,n,.20),type="h",col=gray(0),lwd=3)
legend(10,.35,legend=c(
  expression(paste(theta,"=0.05",sep="")), 
  expression(paste(theta,"=0.10",sep="")),
  expression(paste(theta,"=0.20",sep="")) ),
  lwd=c(3,3,3), 
  col=gray(c(.75,.5,0)) ,bty="n") 

#Chapter 2: Random Distribution Examples#
mu<-10.75
sig<- .8

par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))

x<- seq(7.9,13.9,length=500)
plot(x,pnorm(x,mu,sig),type="l",ylab=expression(paste(italic("F"),"(",italic("y"),")")),xlab=
       expression(italic(y)),lwd=1)
abline(h=c(0,.5,1),col="gray")
plot(x,dnorm(x,mu,sig),type="l",ylab=expression(paste(italic("p"),"(",italic("y"),")")),xlab=expression(italic(y)),lwd=1)
abline(v=mu,col="gray")


#Chapter 3:Modelling Birth Rates with a Poisson Model#

#Setup prior parameters
a <- 2
b <- 1

#Setup population parameters for data
n1 <- 111
sy1 <- 217
n2 <- 44
sy2 <- 66

#Posterior mean
(a + sy1)/(b + n1)
#Posterior mode
(a + sy1 -1)/(b + n1)

#Posterior Confidence Interval for a gamma distribution
qgamma(c(0.025,0.975),a + sy1 , b+ n1)

(a + sy2)/(b+ n2)
(a + sy2 -1 )/(b+ n2)

qgamma(c(0.025,0.975),a + sy2 , b+ n2)

#Using a negative binomial distribution
y <- 0:10
dnbinom(y, size=(a+sy1),mu=(a +sy1)/(b +n1))
dnbinom(y, size=(a+sy2),mu=(a +sy1)/(b +n2))

#Chapter 4:MonteCarlo Approximations#
a<-2 
b<-1
sy<-66
n<-44

theta.sim10<-rgamma(10,a+sy,b+n)
theta.sim100<-rgamma(100,a+sy,b+n)
theta.sim1000<-rgamma(1000,a+sy,b+n)

mean(theta.sim10)
mean(theta.sim100)
mean(theta.sim1000)

pgamma(1.75,a+sy,b+n)

mean( theta.sim10<1.75)
mean( theta.sim100<1.75)
mean( theta.sim1000<1.75)

qgamma(c(.025,.975),a+sy,b+n)
quantile( theta.sim10, c(.025,.975))
quantile( theta.sim100, c(.025,.975))
quantile( theta.sim1000, c(.025,.975))

#Using log-odds for posterior inference
a<-1 
b<-1 
theta.prior.sim<-rbeta(10000,a,b)
gamma.prior.sim<- log( theta.prior.sim/(1-theta.prior.sim))

n0<-860-441 
n1<-441
theta.post.sim<-rbeta(10000,a+n1,b+n0)
gamma.post.sim<- log( theta.post.sim/(1-theta.post.sim))

par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(2,3))
par(cex=.8)

#Posterior Predictive Examples
a<-2 
b<-1
sy1<-217 
n1<-111
sy2<-66 
n2<-44

theta1.mc<-rgamma(10000,a+sy1, b+n1)
theta2.mc<-rgamma(10000,a+sy2, b+n2)

y1.mc<-rpois(10000,theta1.mc)
y2.mc<-rpois(10000,theta2.mc)


mean(theta1.mc>theta2.mc)

mean(y1.mc>y2.mc)

par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,1))
plot(density(theta1.mc/theta2.mc,adj=2),main="",xlim=c(.75,2.25),
     xlab=expression(gamma==theta[1]/theta[2]),
     ylab=expression(paste(italic("p("),gamma,"|",bold(y[1]),",",
                           bold(y[2]),")",sep="")) )

par(mfrow=c(1,2),mar=c(3,3,1,1), mgp=c(1.75,.75,.0))
plot(density(gamma.prior.sim,adj=2),xlim=c(-5,5),main="", 
     xlab=expression(gamma),
     ylab=expression(italic(p(gamma))),col="gray")
plot(density(gamma.post.sim,adj=2),xlim=c(-5,5),main="",xlab=expression(gamma),
     ylab=expression(paste(italic("p("),gamma,"|",y[1],"...",y[n],")",sep="")))
lines(density(gamma.prior.sim,adj=2),col="gray")

#Chapter 5: Normal Model#
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))

par(mfrow=c(1,1))
theta<-2 
sigma<- .5
x<-seq(0,10,length=100)
plot(x,dnorm(x,theta,sigma),type="l",xlab=expression(italic(y)),
     ylab=expression(paste(italic("p(y"),"|",theta,",",sigma^2,")",
                           sep="")),col=gray(.15),lwd=2)

theta<-5 ; sigma<-2
lines(x, dnorm(x,theta,sigma),col=gray(.5),lwd=2)

theta<-7 ; sigma<-1
lines(x, dnorm(x,theta,sigma),col=gray(.85),lwd=2)
legend( 5,.7, c(expression(paste(theta==2,",",sigma^2==.25,sep="")),
                expression(paste(theta==5,",",sigma^2==4,sep="")),
                expression(paste(theta==7,",",sigma^2==1,sep="")) ),
        col=gray(c(.15,.5,.85)), lwd=c(2,2,2),lty=c(1,1,1),bty="n")

#Chapter 6: Posterior Distribution With Gibbs Sampling#

# priors
mu0<-1.9
t20<-0.95^2
s20<-.01 
nu0<-1

# data
y<-c(1.64,1.70,1.72,1.74,1.82,1.82,1.82,1.90,2.08)
n<-length(y) 
mean.y <-mean(y) 
var.y<-var(y)


# grid 
G<-100 
H<-100

mean.grid<-seq(1.505,2.00,length=G) 
prec.grid<-seq(1.75,175,length=H) 

post.grid<-matrix(nrow=G,ncol=H)

for(g in 1:G) {
  for(h in 1:H) { 
    
    post.grid[g,h]<- dnorm(mean.grid[g], mu0, sqrt(t20)) *
      dgamma(prec.grid[h], nu0/2, s20*nu0/2 ) *
      prod( dnorm(y,mean.grid[g],1/sqrt(prec.grid[h])) )
  }
}

post.grid <-post.grid/sum(post.grid)

par(mfrow=c(1,3),mar=c(2.75,2.75,.5,.5),mgp=c(1.70,.70,0))
image( mean.grid,prec.grid,post.grid,col=gray( (10:0)/10 ),
       xlab=expression(theta), ylab=expression(tilde(sigma)^2) )

mean.post<- apply(post.grid,1,sum)
plot(mean.grid,mean.post,type="l",xlab=expression(theta),
     ylab=expression( paste(italic("p("),
                            theta,"|",italic(y[1]),"...",italic(y[n]),")",sep="")))

prec.post<-apply(post.grid,2,sum)
plot(prec.grid,prec.post,type="l",xlab=expression(tilde(sigma)^2),
     ylab=expression( paste(italic("p("),
                            tilde(sigma)^2,"|",italic(y[1]),"...",italic(y[n]),")",sep=""))) 

#Gibbs Sampling Example#
mean.y <- mean(y)
var.y <- var(y)
n <- length(y)

#Starting values
S <- 1000
PHI <- matrix(nrow=S,ncol=2)
PHI[1,] <- phi <- c(mean.y,1/var.y)

#Create loop
for (s in 2:S){

#Generate a new theta value from its full conditional distribution
mun <- (mu0/t20 + n*mean.y*phi[2])/(1/t20 + n*phi[2])
t2n <- 1/(1/t20 + n*phi[2])
phi[1] <- rnorm(1,mun,sqrt(t2n))

#Generate a new 1/sigma^2 value from its full conditional distribution
nun <- nu0 + n
s2n <- (nu0*s20 + (n-1)*var.y + n*(mean.y -phi[1])^2)/nun
phi[2] <- rgamma(1,nun/2,nun*s2n/2)
PHI[s,] <- phi
}
 
#CI for population mean
quantile(PHI[,1],c(0.025,0.5,0.975))

#CI for population precision
quantile(PHI[,2],c(0.025,0.5,0.975))

#CI for population standard deviation
quantile(1/sqrt(PHI[,2]),c(0.025,0.5,0.975))

#### Intro to MCMC diagnostics
mu<-c(-3,0,3)
s2<-c(.33,.33,.33)
w<-c(.45,.1,.45)

ths<- seq(-5,5,length=100)
plot(ths, w[1]*dnorm(ths,mu[1],sqrt(s2[1])) +
       w[2]*dnorm(ths,mu[2],sqrt(s2[2])) +
       w[3]*dnorm(ths,mu[3],sqrt(s2[3])) ,type="l")

#### MC Sampling
S<-2000
d<-sample(1:3,S, prob=w,replace=TRUE)
th<-rnorm(S,mu[d],sqrt(s2[d]))
THD.MC<-cbind(th,d)

#### MCMC sampling
th<-0
THD.MCMC<-NULL
S<-10000

for(s in 1:S) {
  d<-sample(1:3 ,1,prob= w*dnorm(th,mu,sqrt(s2)))
  th<-rnorm(1,mu[d],sqrt(s2[d]) )
  THD.MCMC<-rbind(THD.MCMC,c(th,d) )
}

plot(THD.MCMC[,1])
lines( mu[THD.MCMC[,2]])

#Chapter 7:Multivariate Normal Model#

#### Simulate multivariate normal vector
rmvnorm <- function(n,mu,Sigma) {
    p<-length(mu)
    res<-matrix(0,nrow=n,ncol=p)
    if( n>0 & p>0 ) {
      E<-matrix(rnorm(n*p),n,p)
      res<-t(  t(E%*%chol(Sigma)) +c(mu))
    }
    res
}

#### Simulate inverse-Wishart matrix
rinvwish<-function(n,nu0,iS0) 
{
  sL0 <- chol(iS0) 
  S<-array( dim=c( dim(L0),n ) )
  for(i in 1:n) 
  {
    Z <- matrix(rnorm(nu0 * dim(L0)[1]), nu0, dim(iS0)[1]) %*% sL0  
    S[,,i]<- solve(t(Z)%*%Z)
  }     
  S[,,1:n]
}

#### Log density of the multivariate normal distribution
ldmvnorm<-function(y,mu,Sig){  # log mvn density
  c(  -(length(mu)/2)*log(2*pi) -.5*log(det(Sig)) -.5*
        t(y-mu)%*%solve(Sig)%*%(y-mu)   )  
}

#### Simulate from the Wishart distribution
rwish<-function(n,nu0,S0)
{
  sS0 <- chol(S0)
  S<-array( dim=c( dim(S0),n ) )
  for(i in 1:n)
  {
    Z <- matrix(rnorm(nu0 * dim(S0)[1]), nu0, dim(S0)[1]) %*% sS0
    S[,,i]<- t(Z)%*%Z
  }
  S[,,1:n]
}

#Gibbs Sampling Mean & Covariance#
load("C:/Data/reading.RData")
Y <- reading

mu0 <- c(50,50)
L0 <- matrix(c(625,312.5,312.5,625),nrow=2,ncol=2)

nu0<-4
S0<-matrix( c(625,312.5,312.5,625),nrow=2,ncol=2)

n<-dim(Y)[1] 
ybar<-apply(Y,2,mean)
Sigma<-cov(Y) 
THETA<-SIGMA<-NULL
YS<-NULL

for(s in 1:5000) 
{
  
  ###update theta
  Ln<- solve( solve(L0) + n*solve(Sigma) )
  mun<- Ln%*%( solve(L0)%*%mu0 + n*solve(Sigma)%*%ybar )
  theta<-rmvnorm(1,mun,Ln)  
  ### 
  
  ###update Sigma
  Sn<- S0 + ( t(Y)-c(theta) )%*%t( t(Y)-c(theta) ) 
  Sigma<-solve( rwish(1, nu0+n, solve(Sn)) )
  ###
  
  
  ### save results 
  THETA<-rbind(THETA,theta) ; SIGMA<-rbind(SIGMA,c(Sigma))
}

quantile(  SIGMA[,2]/sqrt(SIGMA[,1]*SIGMA[,4]), prob=c(.025,.5,.975) )
quantile(   THETA[,2]-THETA[,1], prob=c(.025,.5,.975) )
mean( THETA[,2]-THETA[,1])
mean( THETA[,2]>THETA[,1]) 
mean(YS[,2]>YS[,1])

#Chapter 8:Group Scores and Hierarchical Modelling#
load("C:/Data/nels.RData")

#Setup data parameters
y1<-y.school1
y2<-y.school2

boxplot(list(y1,y2),range=0,ylab="score",names=c("school 1","school 2"))

n1<-length(y1)
n2<-length(y2)
mean(y1)
mean(y2)
sd(c(y1,y2))
s2p<- ( var(y1)*(n1-1) + var(y2)*(n2-1) )/(n1+n2-2 )#Standard pooled variance
tstat<- ( mean(y1)-mean(y2) ) /sqrt( s2p*(1/length(y1)+1/length(y2)))#T-stat
t.test(y1,y2)

ts<-seq(-4,4,length=100)
plot(ts,dt(ts,n1+n2-1),type="l",xlab=expression(italic(t)),ylab="density")
abline(v=tstat,lwd=2,col="gray")

## data 
n1<-length(y1) ; n2<-length(y2)

## prior parameters
mu0<-50 
g02<-625
del0<-0 
t02<-625
s20<-100 
nu0<-1

## starting values
mu<- ( mean(y1) + mean(y2) )/2
del<- ( mean(y1) - mean(y2) )/2

## Gibbs sampler
MU<-DEL<-S2<-NULL
Y12<-NULL

for(s in 1:5000) 
{
  
  ##update s2
  s2<-1/rgamma(1,(nu0+n1+n2)/2, 
               (nu0*s20+sum((y1-mu-del)^2)+sum((y2-mu+del)^2))/2)
  ##
  
  ##update mu
  var.mu<-  1/(1/g02+ (n1+n2)/s2 )
  mean.mu<- var.mu*( mu0/g02 + sum(y1-del)/s2 + sum(y2+del)/s2 )
  mu<-rnorm(1,mean.mu,sqrt(var.mu))
  ##
  
  ##update del
  var.del<-  1/(1/t02+ (n1+n2)/s2 )
  mean.del<- var.del*( del0/t02 + sum(y1-mu)/s2 - sum(y2-mu)/s2 )
  del<-rnorm(1,mean.del,sqrt(var.del))
  ##
  
  ##save parameter values
  MU<-c(MU,mu) ; DEL<-c(DEL,del) ; S2<-c(S2,s2) 
  Y12<-rbind(Y12,c(rnorm(2,mu+c(1,-1)*del,sqrt(s2))))
}                 

#### MCMC approximation to posterior for the hierarchical normal model

Y <- Y.school.mathscore

## weakly informative priors
nu0<-1  
s20<-100
eta0<-1 
t20<-100
mu0<-50 
g20<-25

## starting values
m<-length(Y) 
n<-sv<-ybar<-rep(NA,m) 
for(j in 1:m) 
{ 
  ybar[j]<-mean(Y[[j]])
  sv[j]<-var(Y[[j]])
  n[j]<-length(Y[[j]]) 
}
theta<-ybar
sigma2<-mean(sv)
mu<-mean(theta)
tau2<-var(theta)

## setup MCMC
S<-5000
THETA<-matrix( nrow=S,ncol=m)
MST<-matrix( nrow=S,ncol=3)

## MCMC algorithm
for(s in 1:S) 
{
  
  # sample new values of the thetas
  for(j in 1:m) 
  {
    vtheta<-1/(n[j]/sigma2+1/tau2)
    etheta<-vtheta*(ybar[j]*n[j]/sigma2+mu/tau2)
    theta[j]<-rnorm(1,etheta,sqrt(vtheta))
  }
  
  #sample new value of sigma2
  nun<-nu0+sum(n)
  ss<-nu0*s20;for(j in 1:m){ss<-ss+sum((Y[[j]]-theta[j])^2)}
  sigma2<-1/rgamma(1,nun/2,ss/2)
  
  #sample a new value of mu
  vmu<- 1/(m/tau2+1/g20)
  emu<- vmu*(m*mean(theta)/tau2 + mu0/g20)
  mu<-rnorm(1,emu,sqrt(vmu)) 
  
  # sample a new value of tau2
  etam<-eta0+m
  ss<- eta0*t20 + sum( (theta-mu)^2 )
  tau2<-1/rgamma(1,etam/2,ss/2)
  
  #store results
  THETA[s,]<-theta
  MST[s,]<-c(mu,sigma2,tau2)
  
} 

mcmc1<-list(THETA=THETA,MST=MST)

#Chapter 9:Bayesian Linear Regression#
x1<-c(0,0,0,0,0,0,1,1,1,1,1,1)
x2<-c(23,22,22,25,27,20,31,23,27,28,22,24)
y<-c(-0.87,-10.74,-3.27,-1.97,7.50,-7.25,17.05,4.96,10.40,11.05,0.26,2.51)

par(mfrow=c(1,1))
plot(y~x2,pch=16,xlab="age",ylab="change in maximal oxygen uptake", 
     col=c("black","gray")[x1+1])
legend(27,0,legend=c("aerobic","running"),pch=c(16,16),col=c("gray","black"))


#### OLS estimation 
n<-length(y)
X<-cbind(rep(1,n),x1,x2,x1*x2)
p<-dim(X)[2]
beta.ols<- solve(t(X)%*%X)%*%t(X)%*%y

#### Bayesian estimation via MCMC
n<-length(y)
X<-cbind(rep(1,n),x1,x2,x1*x2)
p<-dim(X)[2]

fit.ls<-lm(y~-1+ X)
beta.0<-rep(0,p) 
Sigma.0<-diag(c(150,30,6,5)^2,p)
nu.0<-1 
sigma2.0<- 15^2

beta.0<-fit.ls$coef
nu.0<-1  
sigma2.0<-sum(fit.ls$res^2)/(n-p)
Sigma.0<- solve(t(X)%*%X)*sigma2.0*n


S<-5000

rmvnorm<-function(n,mu,Sigma) 
{ # samples from the multivariate normal distribution
  E<-matrix(rnorm(n*length(mu)),n,length(mu))
  t(  t(E%*%chol(Sigma)) +c(mu))
}

## some convenient quantites
n<-length(y)
p<-length(beta.0)
iSigma.0<-solve(Sigma.0)
XtX<-t(X)%*%X

## store mcmc samples in these objects
beta.post<-matrix(nrow=S,ncol=p)
sigma2.post<-rep(NA,S)

## starting value
sigma2<- var( residuals(lm(y~0+X)) )

## MCMC algorithm
for( scan in 1:S) {
  
  #update beta
  V.beta<- solve( iSigma.0 + XtX/sigma2 )
  E.beta<- V.beta%*%( iSigma.0%*%beta.0 + t(X)%*%y/sigma2 )
  beta<-t(rmvnorm(1, E.beta,V.beta) )
  
  #update sigma2
  nu.n<- nu.0+n
  ss.n<-nu.0*sigma2.0 + sum(  (y-X%*%beta)^2 )
  sigma2<-1/rgamma(1,nu.n/2, ss.n/2)
  
  #save results of this scan
  beta.post[scan,]<-beta
  sigma2.post[scan]<-sigma2
}

round( apply(beta.post,2,mean), 3)

#Chapter 10: Metropolis Hastings Algorithm applied to a normal distribution#


#Load sparrows data
load("C:/Data/sparrows.RData")  
fledged<-sparrows[,1] 
age<-sparrows[,2] 
age2<-age^2
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(fledged~as.factor(age),range=0,xlab="age",ylab="offspring",
     col="gray")
summary(glm(fledged~age+age2,family="poisson"))

s2<-1 
t2<-10
mu<-5

#Generate sythetic data
n <-5
y <-round(rnorm(n,10,1),2)

mu.n <-( mean(y)*n/s2 + mu/t2 )/( n/s2+1/t2) 
t2.n <- 1/(n/s2+1/t2)

## MCMC
s2<-1 
t2<-10 
mu<-5 
y<-c(9.37, 10.18, 9.16, 11.60, 10.33)
theta<-0 
delta<-2 
S<-10000 
THETA<-NULL

for(s in 1:S)
{
  
  theta.star<-rnorm(1,theta,sqrt(delta))
  
  log.r<-( sum(dnorm(y,theta.star,sqrt(s2),log=TRUE)) +
             dnorm(theta.star,mu,sqrt(t2),log=TRUE) )  -
    ( sum(dnorm(y,theta,sqrt(s2),log=TRUE)) +
        dnorm(theta,mu,sqrt(t2),log=TRUE) ) 
  
  if(log(runif(1))<log.r) { theta<-theta.star }
  
  THETA<-c(THETA,theta)
  
}


#Chapter 11:Generalized Mixed Effects Models#

#Load data
load("C:/Data/nelsSES.RData")

#Setup data parameters
ids<-sort(unique(nels$sch_id)) 
m<-length(ids)
Y<-list() 
X<-list()
N<-NULL

head(nels)

for(j in 1:m) 
{
  Y[[j]]<-nels[nels$sch_id==ids[j], 4] 
  N[j]<- sum(nels$sch_id==ids[j])
  xj<-nels[nels$sch_id==ids[j], 3] 
  xj<-(xj-mean(xj))
  X[[j]]<-cbind( rep(1,N[j]), xj  )
}


#### OLS fits
S2.LS<-BETA.LS<-NULL
for(j in 1:m) {
  fit<-lm(Y[[j]]~-1+X[[j]] )
  BETA.LS<-rbind(BETA.LS,c(fit$coef)) 
  S2.LS<-c(S2.LS, summary(fit)$sigma^2) 
} 

par(mar=c(2.75,2.75,.5,.5),mgp=c(1.7,.7,0))
par(mfrow=c(1,3))

#Plot OLS data and plots of estimates of group versus sample sizes
plot(range(nels[,3]),range(nels[,4]),type="n",xlab="SES", 
      ylab="math score")
for(j in 1:m) {    abline(BETA.LS[j,1],BETA.LS[j,2],col="gray")  }

BETA.MLS<-apply(BETA.LS,2,mean)
abline(BETA.MLS[1],BETA.MLS[2],lwd=2)

plot(N,BETA.LS[,1],xlab="sample size",ylab="intercept")
abline(h= BETA.MLS[1],col="black",lwd=2)
plot(N,BETA.LS[,2],xlab="sample size",ylab="slope")
abline(h= BETA.MLS[2],col="black",lwd=2)

#### Hierarchical regression model

## mvnormal simulation
rmvnorm<-function(n,mu,Sigma)
{ 
  E<-matrix(rnorm(n*length(mu)),n,length(mu))
  t(  t(E%*%chol(Sigma)) +c(mu))
}

## Wishart simulation
rwish<-function(n,nu0,S0)
{
  sS0 <- chol(S0)
  S<-array( dim=c( dim(S0),n ) )
  for(i in 1:n)
  {
    Z <- matrix(rnorm(nu0 * dim(S0)[1]), nu0, dim(S0)[1]) %*% sS0
    S[,,i]<- t(Z)%*%Z
  }
  S[,,1:n]
}

## Setup
p<-dim(X[[1]])[2]
theta<-mu0<-apply(BETA.LS,2,mean)
nu0<-1 
s2<-s20<-mean(S2.LS)
eta0<-p+2 
Sigma<-S0<-L0<-cov(BETA.LS) 
BETA<-BETA.LS
THETA.b<-S2.b<-NULL
iL0<-solve(L0) 
iSigma<-solve(Sigma)
Sigma.ps<-matrix(0,p,p)
SIGMA.PS<-NULL
BETA.ps<-BETA*0
BETA.pp<-NULL
set.seed(1)
mu0[2]+c(-1.96,1.96)*sqrt(L0[2,2])

## MCMC
for(s in 1:10000) {
  ##update beta_j 
  for(j in 1:m) 
  {  
    Vj<-solve( iSigma + t(X[[j]])%*%X[[j]]/s2 )
    Ej<-Vj%*%( iSigma%*%theta + t(X[[j]])%*%Y[[j]]/s2 )
    BETA[j,]<-rmvnorm(1,Ej,Vj) 
  } 
  ##
  
  ##update theta
  Lm<-  solve( iL0 +  m*iSigma )
  mum<- Lm%*%( iL0%*%mu0 + iSigma%*%apply(BETA,2,sum))
  theta<-t(rmvnorm(1,mum,Lm))
  ##
  
  ##update Sigma
  mtheta<-matrix(theta,m,p,byrow=TRUE)
  iSigma<-rwish(1, eta0+m, solve( S0+t(BETA-mtheta)%*%(BETA-mtheta) ) )
  ##
  
  ##update s2
  RSS<-0
  for(j in 1:m) { RSS<-RSS+sum( (Y[[j]]-X[[j]]%*%BETA[j,] )^2 ) }
  s2<-1/rgamma(1,(nu0+sum(N))/2, (nu0*s20+RSS)/2 )
  ##
  ##store results
  if(s%%10==0) 
  { 
    cat(s,s2,"\n")
    S2.b<-c(S2.b,s2);THETA.b<-rbind(THETA.b,t(theta))
    Sigma.ps<-Sigma.ps+solve(iSigma) ; BETA.ps<-BETA.ps+BETA
    SIGMA.PS<-rbind(SIGMA.PS,c(solve(iSigma)))
    BETA.pp<-rbind(BETA.pp,rmvnorm(1,theta,solve(iSigma)) )
  }
  ##
}

## MCMC diagnostics#
effectiveSize(S2.b)
effectiveSize(THETA.b[,1])
effectiveSize(THETA.b[,2])

apply(SIGMA.PS,2,effectiveSize)

tmp<-NULL 

for(j in 1:dim(SIGMA.PS)[2]) { tmp<-c(tmp,acf(SIGMA.PS[,j])$acf[2]) }

acf(S2.b)
acf(THETA.b[,1])
acf(THETA.b[,2])

par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))

plot(density(THETA.b[,2],adj=2),xlim=range(BETA.pp[,2]), 
     main="",xlab="slope parameter",ylab="posterior density",lwd=2)
lines(density(BETA.pp[,2],adj=2),col="gray",lwd=2)
legend( -3 ,1.0 ,legend=c( expression(theta[2]),expression(tilde(beta)[2])), 
        lwd=c(2,2),col=c("black","gray"),bty="n") 

quantile(THETA.b[,2],prob=c(.025,.5,.975))
mean(BETA.pp[,2]<0) 

BETA.PM<-BETA.ps/1000
plot( range(nels[,3]),range(nels[,4]),type="n",xlab="SES",
      ylab="math score")
for(j in 1:m) {    abline(BETA.PM[j,1],BETA.PM[j,2],col="gray")  }
abline( mean(THETA.b[,1]),mean(THETA.b[,2]),lwd=2 )

#Chapter 12:Ordered Probit Regression and Rank Likelihood#

#Load educational attainment data
load("C:/Data/socmob.RData") 

#Setup data parameters
yincc<- match(socmob$INC,sort(unique(socmob$INC)))
ydegr<-socmob$DEGREE+1
yage<-socmob$AGE
ychild<-socmob$CHILD
ypdeg<-1*(socmob$PDEG>2)
tmp<-lm(ydegr~ychild+ypdeg+ychild:ypdeg)

#Plot educational attainment data
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))
plot(table(socmob$DEG+1)/sum(table(socmob$DEG+1)),
     lwd=2,type="h",xlab="DEG",ylab="probability")
plot(table(socmob$CHILD)/sum(table(socmob$CHILD)),lwd=2,type="h",xlab="CHILD",ylab="probability" )

#Ordinal Probit Regression
#### Ordinal probit regression

#Data parameters
X<-cbind(ychild,ypdeg,ychild*ypdeg)
y<-ydegr
keep<- (1:length(y))[ !is.na( apply( cbind(X,y),1,mean) ) ]
X<-X[keep,] 
y<-y[keep]
ranks<-match(y,sort(unique(y))) 
uranks<-sort(unique(ranks))
n<-dim(X)[1] 
p<-dim(X)[2]
iXX<-solve(t(X)%*%X)  
V<-iXX*(n/(n+1)) 
cholV<-chol(V)

## setup
beta<-rep(0,p) 
z<-qnorm(rank(y,ties.method="random")/(n+1))
g<-rep(NA,length(uranks)-1)
K<-length(uranks)
BETA<-matrix(NA,1000,p) 
Z<-matrix(NA,1000,n) 
ac<-0
mu<-rep(0,K-1) 
sigma<-rep(100,K-1) 

## MCMC
S<-25000
for(s in 1:S) 
{
  
  #update g 
  for(k in 1:(K-1)) 
  {
    a<-max(z[y==k])
    b<-min(z[y==k+1])
    u<-runif(1, pnorm( (a-mu[k])/sigma[k] ),
             pnorm( (b-mu[k])/sigma[k] ) )
    g[k]<- mu[k] + sigma[k]*qnorm(u)
  }
  
  #update beta
  E<- V%*%( t(X)%*%z )
  beta<- cholV%*%rnorm(p) + E
  
  #update z
  ez<-X%*%beta
  a<-c(-Inf,g)[ match( y-1, 0:K) ]
  b<-c(g,Inf)[y]  
  u<-runif(n, pnorm(a-ez),pnorm(b-ez) )
  z<- ez + qnorm(u)
  
  
  #help mixing
  c<-rnorm(1,0,n^(-1/3))  
  zp<-z+c ; gp<-g+c
  lhr<-  sum(dnorm(zp,ez,1,log=T) - dnorm(z,ez,1,log=T) ) + 
    sum(dnorm(gp,mu,sigma,log=T) - dnorm(g,mu,sigma,log=T) )
  if(log(runif(1))<lhr) { z<-zp ; g<-gp ; ac<-ac+1 }
  
  if(s%%(S/1000)==0) 
  { 
    cat(s/S,ac/s,"\n")
    BETA[s/(S/1000),]<-  beta
    Z[s/(S/1000),]<- z
  }
} 

par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))
plot(X[,1]+.25*(X[,2]),Z[1000,],
     pch=15+X[,2],col=c("gray","black")[X[,2]+1],
     xlab="number of children",ylab="z", ylim=range(c(-2.5,4,Z[1000,])),
     xlim=c(0,9))

beta.pm<-apply(BETA,2,mean)
ZPM<-apply(Z,2,mean)
abline(0,beta.pm[1],lwd=2 ,col="gray")
abline(beta.pm[2],beta.pm[1]+beta.pm[3],col="black",lwd=2 )
legend(5,4,legend=c("PDEG=0","PDEG=1"),pch=c(15,16),col=c("gray","black"))


plot(density(BETA[,3],adj=2),lwd=2,xlim=c(-.5,.5),main="",xlab=expression(beta[3]),ylab="density")
sd<-sqrt(  solve(t(X)%*%X/n)[3,3] )
x<-seq(-.7,.7,length=100)
lines(x,dnorm(x,0,sd),lwd=2,col="gray")
legend(-.5,6.5,legend=c("prior","posterior"),lwd=c(2,2),col=c("gray","black"),bty="n")

#### Rank likelihood regression 
beta.pm<-apply(BETA,2,mean)
beta.pm[1]+beta.pm[3]
quantile(BETA[,3],prob=c(.025,.0975))
quantile(BETA[,3],prob=c(0.025,0.975))

source("c:/R Code/rlreg.R")
rfit<-treg(y,X) 




