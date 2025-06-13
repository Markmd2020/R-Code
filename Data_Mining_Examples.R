#####Data Mining Exampes####

#Load libraries
library(MASS)
library(lattice)
library(locfit)
library(lars)
library(ROCR)
library(textir)
library(VGAM)
library(tree)
library(cluster)

#EDA Example#
#Read data
oj <- read.csv("C:/Data/oj.csv") 
oj$store <- factor(oj$store) 
oj[1:2,] 
t1=tapply(oj$logmove,oj$brand,FUN=mean,na.rm=TRUE) 
t1 
t2=tapply(oj$logmove,INDEX=list(oj$brand,oj$week),FUN=mean,na.rm=TRUE) 
t2 
plot(t2[1,],type= "l",xlab="week",ylab="dominicks",ylim=c(7,12)) 
plot(t2[2,],type= "l",xlab="week",ylab="minute.maid",ylim=c(7,12)) 
plot(t2[3,],type= "l",xlab="week",ylab="tropicana",ylim=c(7,12)) 
logmove=c(t2[1,],t2[2,],t2[3,]) 
week1=c(40:160) 
week=c(week1,week1,week1) 
brand1=rep(1,121) 
brand2=rep(2,121) 
brand3=rep(3,121) 
brand=c(brand1,brand2,brand3) 
xyplot(logmove~week|factor(brand),type= "l",layout=c(1,3),col="black") 
boxplot(logmove~brand,data=oj) 
histogram(~logmove|brand,data=oj,layout=c(1,3)) 
densityplot(~logmove|brand,data=oj,layout=c(1,3),plot.points=FALSE) 
densityplot(~logmove,groups=brand,data=oj,plot.points=FALSE)  
xyplot(logmove~week,data=oj,col="black") 

#Local Polynomial Regression#
## first we read in the data 
OldFaithful <- read.csv("C:/Data/OldFaithful.csv") 
OldFaithful[1:3,] 
## density histograms and smoothed density histograms 
## time of eruption 
hist(OldFaithful$TimeEruption,freq=FALSE) 
fit1 <- locfit(~lp(TimeEruption),data=OldFaithful) 
plot(fit1) 
## waiting time to next eruption 
hist(OldFaithful$TimeWaiting,freq=FALSE) 
fit2 <- locfit(~lp(TimeWaiting),data=OldFaithful) 
plot(fit2) 
## experiment with different smoothing constants 
fit2 <- locfit(~lp(TimeWaiting,nn=0.9,deg=2),data=OldFaithful) 
plot(fit2) 
fit3 <- locfit(~lp(TimeWaiting,nn=0.3,deg=2),data=OldFaithful) 
plot(fit3)

## cross-validation of smoothing constant  
## for waiting time to next eruption 
alpha<-seq(0.20,1,by=0.01) 
n1=length(alpha) 
g=matrix(nrow=n1,ncol=4) 
for (k in 1:length(alpha)) { 
  g[k,]<- gcv(~lp(TimeWaiting,nn=alpha[k]),data=OldFaithful) 
} 
g 
plot(g[,4]~g[,3],ylab="GCV",xlab="degrees of freedom") 
## minimum at nn = 0.66 
fit2 <- locfit(~lp(TimeWaiting,nn=0.66,deg=2),data=OldFaithful) 
plot(fit2)

## local polynomial regression of TimeEruption on TimeWaiting 
plot(TimeWaiting~TimeEruption,data=OldFaithful) 
# standard regression fit 
fitreg=lm(TimeWaiting~TimeEruption,data=OldFaithful) 
plot(TimeWaiting~TimeEruption,data=OldFaithful) 
abline(fitreg) 
# fit with nearest neighbor bandwidth 
fit3 <- locfit(TimeWaiting~lp(TimeEruption),data=OldFaithful) 
plot(fit3) 
fit4 <- locfit(TimeWaiting~lp(TimeEruption,deg=1),data=OldFaithful) 
plot(fit4) 
fit5 <- locfit(TimeWaiting~lp(TimeEruption,deg=0),data=OldFaithful) 
plot(fit5)

####Lasso Regression###
prostate <- read.csv("C:/Data/prostate.csv") 
prostate[1:3,] 
m1=lm(lcavol~.,data=prostate) 
summary(m1) 
## the model.matrix statement defines the model to be fitted  
x <- model.matrix(lcavol~age+lbph+lcp+gleason+lpsa,data=prostate) 
x=x[,-1]   
## stripping off the column of 1s as LASSO includes the intercept 
## automatically 
## lasso on all data 
lasso <- lars(x=x,y=prostate$lcavol,trace=TRUE) 
## trace of lasso (standardized) coefficients for varying penalty 
plot(lasso) 
lasso 
coef(lasso,s=c(.25,.50,0.75,1.0),mode="fraction") 
## cross-validation using 10 folds 
cv.lars(x=x,y=prostate$lcavol,K=10) 
## another way to evaluate lasso’s out-of-sample prediction performance 
MSElasso25=dim(10)  
MSElasso50=dim(10)  
MSElasso75=dim(10)  
MSElasso100=dim(10)  
set.seed(135) 
for(i in 1:10){ 
  train <- sample(1:nrow(prostate),80) 
  lasso <- lars(x=x[train,],y=prostate$lcavol[train]) 
  MSElasso25[i]=  
    mean((predict(lasso,x[-train,],s=.25,mode="fraction")$fit -
          prostate$lcavol[-train])^2) 
  MSElasso50[i]=   
    mean((predict(lasso,x[-train,],s=.50,mode="fraction")$fit -
          prostate$lcavol[-train])^2) 
  MSElasso75[i]= 
    mean((predict(lasso,x[-train,],s=.75,mode="fraction")$fit -
          prostate$lcavol[-train])^2) 
  MSElasso100[i]= 
    mean((predict(lasso,x[-train,],s=1.00,mode="fraction")$fit -
          prostate$lcavol[-train])^2) 
} 
mean(MSElasso25) 
mean(MSElasso50) 
mean(MSElasso75) 
mean(MSElasso100)

###Logistic Regression###
credit <- read.csv("C:/Data/germancredit.csv") 
credit 
credit$Default <- factor(credit$Default) 
## re-level the credit history and a few other variables 
credit$history = factor(credit$history, 
                        levels=c("A30","A31","A32","A33","A34")) 
levels(credit$history) = c("good","good","poor","poor","terrible") 
credit$foreign <- factor(credit$foreign, levels=c("A201","A202"), 
                         labels=c("foreign","german")) 
credit$rent <- factor(credit$housing=="A151") 
credit$purpose <- factor(credit$purpose, 
                         levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410")) 
levels(credit$purpose) <- 
  c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz") 
## for demonstration, cut the dataset to these variables 
credit <- credit[,c("Default","duration","amount","installment","age",                    
                    "history", "purpose","foreign","rent")] 
credit[1:3,] 
summary(credit) # check out the data 
## create a design matrix  
## factor variables are turned into indicator variables  
## the first column of ones is omitted 
Xcred <- model.matrix(Default~.,data=credit)[,-1]  
Xcred[1:3,] 
## creating training and prediction datasets 
## select 900 rows for estimation and 100 for testing 
set.seed(135) 
train <- sample(1:1000,900) 
xtrain <- Xcred[train,] 
xnew <- Xcred[-train,] 
ytrain <- credit$Default[train] 
ynew <- credit$Default[-train] 
credglm=glm(Default~.,family=binomial,data=data.frame(Default=ytrain,xtrain)) 
summary(credglm) 
## prediction: predicted default probabilities for cases in test set 
ptest <- predict(credglm,newdata=data.frame(xnew),type="response") 
data.frame(ynew,ptest)

## What are our misclassification rates on that training set?  
## We use probability cutoff 1/6 
## coding as 1 (predicting default) if probability 1/6 or larger 
gg1=floor(ptest+(5/6)) 
ttt=table(ynew,gg1) 
ttt 
error=(ttt[1,2]+ttt[2,1])/100 
error 

## R macro for plotting the ROC curve 
## plot the ROC curve for classification of y with p 
roc <- function(p,y){ 
  y <- factor(y) 
  n <- length(p) 
  p <- as.vector(p) 
  Q <- p > matrix(rep(seq(0,1,length=500),n),ncol=500,byrow=TRUE) 
  fp <- colSums((y==levels(y)[1])*Q)/sum(y==levels(y)[1]) 
  tp <- colSums((y==levels(y)[2])*Q)/sum(y==levels(y)[2]) 
  plot(fp, tp, xlab="1-Specificity", ylab="Sensitivity") 
  abline(a=0,b=1,lty=2,col=8) 
} 
## ROC for hold-out period 
roc(p=ptest,y=ynew) 
## ROC for all cases (in-sample) 
credglmall <- glm(credit$Default ~ Xcred,family=binomial) 
roc(p=credglmall$fitted, y=credglmall$y)

#Multinomial Logistic Regression
data(fgl)  
fgl 
## to standardize the features 
## a library of example datasets 
## loads the data into R; see help(fgl) 
## standardization, using the normalize function in the library textir 
covars <- scale(fgl[,1:9],s=sdev(fgl[,1:9])) 
sd(covars) ## convince yourself that features are standardized 
dd=data.frame(cbind(type=fgl$type,covars)) 
gg <- vglm(type~ Na+Mg+Al,multinomial,data=dd) 
summary(gg) 
predict(gg) ## obtain log-odds relative to last group 
round(fitted(gg),2)  ## probabilities 
cbind(round(fitted(gg),2),fgl$type)

#########LDA  Example#####
glass=data.frame(fgl) 
glass 
## linear discriminant analysis 
m1=lda(type~.,glass) 
m1 
predict(m1,newdata=data.frame(RI=3.0,Na=13,Mg=4,Al=1,Si=70,K=0.06,Ca=9,Ba=0,Fe=0)) 
predict(m1,newdata=data.frame(RI=3.0,Na=13,Mg=4,Al=1,Si=70,K=0.06,Ca=9,Ba=0,Fe=0))$class

#####Decision Trees Example####
data(mcycle) 
mcycle 
plot(accel~times,data=mcycle) 
mct <- tree(accel ~ times, data=mcycle) 
mct 
plot(mct, col=8) 
text(mct, cex=.75) ## we use different font size to avoid print overlap 

#####K-Means Example####

## read in the data 
food <- read.csv("C:/Data/protein.csv") 
food[1:3,] 
## first, clustering on just Red and White meat (p=2) and k=3 clusters 
set.seed(135) ## to fix the random starting clusters 
grpMeat <- kmeans(food[,c("WhiteMeat","RedMeat")], centers=3, nstart=10) 
grpMeat 
## list of cluster assignments 
o=order(grpMeat$cluster) 
data.frame(food$Country[o],grpMeat$cluster[o]) 
## plotting cluster assignments on Red and White meat scatter plot 
plot(food$Red, food$White, type="n", xlim=c(3,19), xlab="Red Meat", 
     ylab="White Meat") 
text(x=food$Red, y=food$White, labels=food$Country, col=grpMeat$cluster+1)

#####Hierarchical Cluster####
food <- read.csv("C:/Data/protein.csv") 
food[1:3,] 
food[1:3,] 
## we use the program agnes in the package cluster  
## argument diss=FALSE indicates that we use the dissimilarity  
## matrix that is being calculated from raw data.  
## argument metric="euclidian" indicates that we use Euclidian distance 
## no standardization is used as the default 
## the default is "average" linkage  

## first we consider just Red and White meat clusters 
food2=food[,c("WhiteMeat","RedMeat")] 
food2agg=agnes(food2,diss=FALSE,metric="euclidian") 
food2agg 
plot(food2agg) 
food2agg$merge 
## dendrogram 
## describes the sequential merge steps 
## identical result obtained by first computing the distance matrix 
food2aggv=agnes(daisy(food2),metric="euclidian") 
plot(food2aggv)
## Using data on all nine variables (features) 
## Euclidean distance and average linkage  
foodagg=agnes(food[,-1],diss=FALSE,metric="euclidian") 
plot(foodagg) 
## dendrogram 
foodagg$merge 
## describes the sequential merge steps 
## Using data on all nine variables (features) 
## Euclidean distance and single linkage 
foodaggsin=agnes(food[,-1],diss=FALSE,metric="euclidian",method="single") 
plot(foodaggsin) ## dendrogram 
foodaggsin$merge ## describes the sequential merge steps 
## Euclidean distance and complete linkage 
foodaggcomp=agnes(food[,-1],diss=FALSE,metric="euclidian",method="complete") 
plot(foodaggcomp) ## dendrogram 
foodaggcomp$merge ## describes the sequential merge steps

#PCA Example#
food <- read.csv("C:/Data/protein.csv") 
food 
## correlation matrix 
cor(food[,-1]) 
pcafood <- prcomp(food[,-1], scale=TRUE)  
## we strip the first column (country labels) from the data set 
## scale = TRUE: variables are first standardized. Default is FALSE 
pcafood 
foodpc <- predict(pcafood) 
foodpc  
## how many principal components do we need? 
plot(pcafood, main="")  
mtext(side=1, "European Protein Principal Components",  line=1, font=2) 
## how do the PCs look? 
par(mfrow=c(1,2)) 
plot(foodpc[,1:2], type="n", xlim=c(-4,5)) 
text(x=foodpc[,1], y=foodpc[,2], labels=food$Country) 
plot(foodpc[,3:4], type="n", xlim=c(-3,3)) 
text(x=foodpc[,3], y=foodpc[,4], labels=food$Country) 
pcafood$rotation[,2]  