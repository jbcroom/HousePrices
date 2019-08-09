################
# File for assessment of quality and condition indicators
# Authors: Kevin Thompson, Brandon Croom, Sterling Beason
# Last Updated: August 4, 2019
################
library(ggplot2)
library(ggpubr)

load(file = "/data/.RData")

#Build out plots for Analysis I assumptions
housePrices_A1 = dplyr::filter(housePrices, housePrices$Neighborhood %in% c('BrkSide','Edwards','NAmes'))

#plot data
housePrices_A1 %>% 
  ggplot(aes(SalePrice, GrLivArea, color=Neighborhood, shape=Neighborhood)) + geom_point() + ggtitle("Housing Data - Analysis 1")

#Plot shows a few outliers

#build the model
#set edwards as the reference model
housePrices_A1 = within(housePrices_A1, Neighborhood <- relevel(Neighborhood, ref="Edwards"))

#build the model
Analysis1.lm = lm(SalePrice ~ GrLivArea*Neighborhood, data=housePrices_A1)
summary(Analysis1.lm)

#grab the qqplots, residual plots
plot(Analysis1.lm, which=c(1:3))

#build out histogram
##Histogram with normal curve
##Store studentized residuals
studresanalysis1 <- rstudent(Analysis1.lm)

##Histogram
hist(studresanalysis1, freq=FALSE, main="Distribution of Studentized Residuals",
     xlab="Studentized Residuals", ylab="Density", ylim=c(0,0.5))

##Create range of x-values for normal curve
xfit2 <- seq(min(studresanalysis1)-1, max(studresanalysis1)+1, length=40)

##Generate values from the normal distribution at the specified values
yfit2 <- (dnorm(xfit2))

##Add the normal curve
lines(xfit2, yfit2, ylim=c(0,0.5))
