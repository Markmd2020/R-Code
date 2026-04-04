#Distribution Checking


#Load libraries
library(vcd)
library(DescTools)
library(fitdistrplus)

#Read data
maternity <- read.csv("C:/Data/Maternity.csv")

View(maternity)

# let's start with discrete
# plot a histogram
barplot(prop.table(table(maternity$No.births.day)), space=0)

# to decide whether or not a discrete dist is good to describe empirical
# need to think about the assumptions
# for poisson 
# births are independent
# each day would have the same rate of births
# in reality maybe not hold but for this example, let's assume they do

# lets find mean of data - which would be lambda
mean(maternity$No.births.day)

gf <- goodfit(maternity$No.births.day,type= "poisson",method= "ML")
plot(gf,main="Count data vs Poisson distribution", lines_gp = gpar(col = "blue", lwd = 2),
     points_gp = gpar(col = "blue"), pch = 19) 
# need to watch red line
# plots the expected - observed frequency 
# if observed matches expected the bar would go down to 0 exactly
# if observed greater than expected bar goes below zero
# if observed less than expected bar stops above zero

# H0 - data follows a Poisson distribution
# H1 - data does not follow a Poisson distribution
summary(gf) # p-value=0.76 so do not reject null and assumed follows poisson

# the underlying assumptions may not in reality but theoretically good fit

## now for continuous 

# lets assume follows normal
hist(maternity$Birth.weight.kg, freq=FALSE)
lines(density(maternity$Birth.weight.kg, adjust=2))
# taken the empirical data
# found the best fitting normal curve which most closely relates to empirical observations
# minimises the difference between the observered (bars) and theoretical (line)
mean(maternity$Birth.weight.kg)
sd(maternity$Birth.weight.kg)
# best fit has mean=3.11 and sd=0.49
# used maximum likelihood to minimise the differences

# could also fit other probability distributions to data if we though it wasn't normal
descdist(maternity$Birth.weight.kg, discrete = FALSE)

# check for normal
fitdistr(maternity$Birth.weight.kg, "normal")
require(DescTools)
AndersonDarlingTest(maternity$Birth.weight.kg, "pnorm", mean=3.11, sd=0.49)
# could follow normal as p>0.05 so we fail to reject null hypothesis
# we could change these mean and sd values to any specific value that we may be
# interested in checking the distribution for
AndersonDarlingTest(maternity$Birth.weight.kg, "pnorm", mean=2, sd=2)
# p<0.05 so does not follow this normal distribution

# check for gamma
fitdistr(maternity$Birth.weight.kg, "gamma")
require(DescTools)
AndersonDarlingTest(maternity$Birth.weight.kg,
                    null="pgamma", shape=39.8568459,rate=12.8012794)
# could follow gamma as p>0.05 so we fail to reject null hypothesis
# 14.92 births