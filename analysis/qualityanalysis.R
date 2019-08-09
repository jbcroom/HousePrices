################
# File for assessment of quality and condition indicators
# Authors: Kevin Thompson, Brandon Croom, Sterling Beason
# Last Updated: August 4, 2019
################
library(ggplot2)
library(ggpubr)

load(file = "/data/.RData")

#Perform some basic counts of categorical and numerical variables
#Numeric Variables
housePrices %>% keep(is.numeric) %>% tidyr::drop_na()

#Categorical Observations
housePrices %>% keep(is.factor) %>% tidyr::drop_na() 


#Graph out the quality indicators as bar plots against the overall quality indicator to see if they line up. Calculate using mean
#Exterior Quality
p1 =  housePrices %>%
         group_by(ExterQual) %>%
         dplyr::summarise(OverallQual_Avg = mean(as.numeric(OverallQual)[])) %>%
         ggplot(aes(x = ExterQual, y = OverallQual_Avg))+ geom_bar(stat = 'identity',fill = "blue") + theme_pubr() + 
         geom_text(aes(label=OverallQual_Avg), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)

#Basement Quality
p2 =  housePrices %>%
        group_by(BsmtQual) %>%
        dplyr::summarise(OverallQual_Avg = mean(as.numeric(OverallQual)[])) %>%
        ggplot(aes(x = BsmtQual, y = OverallQual_Avg))+ geom_bar(stat = 'identity',fill = "blue") + theme_pubr() + 
        geom_text(aes(label=OverallQual_Avg), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)

#Kitchen Quality
p3 =  housePrices %>%
        group_by(KitchenQual) %>%
        dplyr::summarise(OverallQual_Avg = mean(as.numeric(OverallQual)[])) %>%
        ggplot(aes(x = KitchenQual, y = OverallQual_Avg))+ geom_bar(stat = 'identity',fill = "blue") + theme_pubr() + 
        geom_text(aes(label=OverallQual_Avg), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)

  
#Fireplace Quality
p4 =  housePrices %>%
        group_by(FireplaceQu) %>%
        dplyr::summarise(OverallQual_Avg = mean(as.numeric(OverallQual)[])) %>%
        ggplot(aes(x = FireplaceQu, y = OverallQual_Avg))+ geom_bar(stat = 'identity',fill = "blue") + theme_pubr() + 
        geom_text(aes(label=OverallQual_Avg), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)

#Garage Quality
p5 =  housePrices %>%
      group_by(GarageQual) %>%
      dplyr::summarise(OverallQual_Avg = mean(as.numeric(OverallQual)[])) %>%
      ggplot(aes(x = GarageQual, y = OverallQual_Avg))+ geom_bar(stat = 'identity',fill = "blue") + theme_pubr() + 
      geom_text(aes(label=OverallQual_Avg), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) 

ggarrange(p1,p2,p3,p4,p5,ncol=2, nrow=3)

#Graph out the quality indicators as bar plots against the overall quality indicator to see if they line up. Calculate using median
#Exterior Quality
p1 =  housePrices %>%
      group_by(ExterQual) %>%
      dplyr::summarise(OverallQual_Avg = median(as.numeric(OverallQual)[])) %>%
      ggplot(aes(x = ExterQual, y = OverallQual_Avg))+ geom_bar(stat = 'identity',fill = "blue") + theme_pubr() + 
      geom_text(aes(label=OverallQual_Avg), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)

#Basement Quality
p2 =  housePrices %>%
      group_by(BsmtQual) %>%
      dplyr::summarise(OverallQual_Avg = median(as.numeric(OverallQual)[])) %>%
      ggplot(aes(x = BsmtQual, y = OverallQual_Avg))+ geom_bar(stat = 'identity',fill = "blue") + theme_pubr() + 
      geom_text(aes(label=OverallQual_Avg), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) 

#Kitchen Quality
p3 =  housePrices %>%
      group_by(KitchenQual) %>%
      dplyr::summarise(OverallQual_Avg = median(as.numeric(OverallQual)[])) %>%
      ggplot(aes(x = KitchenQual, y = OverallQual_Avg))+ geom_bar(stat = 'identity',fill = "blue") + theme_pubr() + 
      geom_text(aes(label=OverallQual_Avg), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)

#Fireplace Quality
p4 =  housePrices %>%
      group_by(FireplaceQu) %>%
      dplyr::summarise(OverallQual_Avg = median(as.numeric(OverallQual)[])) %>%
      ggplot(aes(x = FireplaceQu, y = OverallQual_Avg))+ geom_bar(stat = 'identity',fill = "blue") + theme_pubr() + 
      geom_text(aes(label=OverallQual_Avg), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)

#Garage Quality
p5 =  housePrices %>%
      group_by(GarageQual) %>%
      dplyr::summarise(OverallQual_Avg = median(as.numeric(OverallQual)[])) %>%
      ggplot(aes(x = GarageQual, y = OverallQual_Avg))+ geom_bar(stat = 'identity',fill = "blue") + theme_pubr() + 
      geom_text(aes(label=OverallQual_Avg), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)

ggarrange(p1,p2,p3,p4,p5,ncol=2, nrow=3)


#Graph out the condition indicators as bar plots against the overall condition indicator to see if they line up. Calculate using mean
#Exterior Condition
p1 =  housePrices %>%
      group_by(ExterCond) %>%
      dplyr::summarise(OverallCond_Avg = mean(as.numeric(OverallCond)[])) %>%
      ggplot(aes(x = ExterCond, y = OverallCond_Avg))+ geom_bar(stat = 'identity',fill = "blue") + theme_pubr() + 
      geom_text(aes(label=OverallCond_Avg), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)

#Basement Condtion
p2 =  housePrices %>%
      group_by(BsmtCond) %>%
      dplyr::summarise(OverallCond_Avg = mean(as.numeric(OverallCond)[])) %>%
      ggplot(aes(x = BsmtCond, y = OverallCond_Avg))+ geom_bar(stat = 'identity',fill = "blue") + theme_pubr() + 
      geom_text(aes(label=OverallCond_Avg), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)

#Garage Quality
p3 =  housePrices %>%
      group_by(GarageCond) %>%
      dplyr::summarise(OverallCond_Avg = mean(as.numeric(OverallCond)[])) %>%
      ggplot(aes(x = GarageCond, y = OverallCond_Avg))+ geom_bar(stat = 'identity',fill = "blue") + theme_pubr() + 
      geom_text(aes(label=OverallCond_Avg), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)

ggarrange(p1,p2,p3)

#Graph out the condition indicators as bar plots against the overall condition indicator to see if they line up. Calculate using median
#Exterior Condition
p1 =  housePrices %>%
      group_by(ExterCond) %>%
      dplyr::summarise(OverallCond_Avg = median(as.numeric(OverallCond)[])) %>%
      ggplot(aes(x = ExterCond, y = OverallCond_Avg))+ geom_bar(stat = 'identity',fill = "blue") + theme_pubr() + 
      geom_text(aes(label=OverallCond_Avg), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)

#Basement Condtion
p2 =  housePrices %>%
      group_by(BsmtCond) %>%
      dplyr::summarise(OverallCond_Avg = median(as.numeric(OverallCond)[])) %>%
      ggplot(aes(x = BsmtCond, y = OverallCond_Avg))+ geom_bar(stat = 'identity',fill = "blue") + theme_pubr() + 
      geom_text(aes(label=OverallCond_Avg), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)

#Garage Quality
p3 =  housePrices %>%
      group_by(GarageCond) %>%
      dplyr::summarise(OverallCond_Avg = median(as.numeric(OverallCond)[])) %>%
      ggplot(aes(x = GarageCond, y = OverallCond_Avg))+ geom_bar(stat = 'identity',fill = "blue") + theme_pubr() + 
      geom_text(aes(label=OverallCond_Avg), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)

ggarrange(p1,p2,p3)