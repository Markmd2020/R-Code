#Extreme Values Modelling#

#Load libraries
library(cowplot)
library(ggplot2)
library(mgcv)
library(ismev)

#Load and explore data
data(fremantle, package = "ismev")
head(fremantle)

#Predicting Sea Level Maximum Values and relationship with Southern Oscillation Index (SOI)
p1 <- ggplot(fremantle, aes(x = Year, y = SeaLevel)) +
  geom_line() + geom_smooth(se = FALSE)
p2 <- ggplot(fremantle, aes(x = SOI, y = SeaLevel)) +
  geom_point() + geom_smooth(se = FALSE)
plot_grid(p1, p2, ncol = 1)

#Centre Year variable
fremantle <- transform(fremantle, cYear = Year - median(Year))

#Applying Generalized Extreme Value Distributions
m1 <- gam(list(SeaLevel ~ s(cYear) + s(SOI),
               ~ s(cYear) + s(SOI),
               ~ 1),
          data = fremantle, method = "REML",
          family = gevlss(link = list("identity", "identity", "identity")))
summary(m1)

plot(m1, pages = 1, scheme = 1, scale = 0, seWithMean = TRUE)

