################
# File for assessment of quality and condition indicators
# Authors: Kevin Thompson, Brandon Croom, Sterling Beason
# Last Updated: August 4, 2019
################
library(ggplot2)
library(ggpubr)

source("preprocessing.R")

#Perform some basic counts of categorical and numerical variables

#Numeric Variables
housePrices %>% keep(is.numeric) %>% tidyr::drop_na()

#Categorical Observations
housePrices %>% keep(is.factor) %>% tidyr::drop_na() 


#Graph out the quality indicators as density plots against the overall quality indicator to see if they line up
#Exterior Quality
p1 = housePrices %>% ggplot(aes(x=ExterQual, y=OverallQual, color=ExterQual)) + geom_point() + theme_pubr()

#Basement Quality
p2 = housePrices %>% ggplot(aes(x=BsmtQual, y=OverallQual, color=BsmtQual)) + geom_point() + theme_pubr()

#Kitchen Quality
p3 = housePrices %>% ggplot(aes(x=KitchenQual, y=OverallQual, color=KitchenQual)) + geom_point() + theme_pubr()
  
#Fireplace Quality
p4 = housePrices %>% ggplot(aes(x=FireplaceQu, y=OverallQual, color=FireplaceQu)) + geom_point() + theme_pubr()

#Garage Quality
p5 = housePrices %>% ggplot(aes(x=GarageQual, y=OverallQual, color=GarageQual)) + geom_point() + theme_pubr()

ggarrange(p1,p2,p3,p4,p5,ncol=2, nrow=3)

#Graph out the condition indicators as density plots against the overall condition indicator to see if they line up
#Exterior Condition
p1 = housePrices %>% ggplot(aes(x=ExterQual, y=OverallCond, color=ExterQual)) + geom_point() + theme_pubr()

#Basement Condtion
p2 = housePrices %>% ggplot(aes(x=BsmtCond, y=OverallCond, color=BsmtCond)) + geom_point() + theme_pubr()

#Garage Quality
p3 = housePrices %>% ggplot(aes(x=GarageCond, y=OverallCond, color=GarageCond)) + geom_point() + theme_pubr()

ggarrange(p1,p2,p3)

