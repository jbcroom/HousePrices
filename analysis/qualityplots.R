################
# File for assessment of quality and condition indicators
# Authors: Kevin Thompson, Brandon Croom, Sterling Beason
# Last Updated: August 8, 2019
################

library(tidyverse)

load("analysis/data/.RData")

relevantData <- housePrices %>% filter(Neighborhood %in% c("NAmes", "Edwards", "BrkSide"))

# Quality vs. Condition
relevantData %>%
  group_by(OverallQual) %>%
  summarize(meanoverall = mean(as.numeric(OverallCond))) %>%
  ggplot(aes(x = OverallQual, y = meanoverall)) +
  geom_col() +
  labs(x = "Overall Quality", y = "Average Overall Condition")


# Kitchen Quality
relevantData %>%
  group_by(KitchenQual) %>%
  summarize(meanoverall = mean(as.numeric(OverallQual))) %>%
  ggplot(aes(x = KitchenQual, y = meanoverall)) +
  geom_col() +
  labs(x = "Kitchen Quality", y = "Average Overall Quality")

# Exterior Quality
relevantData %>%
  group_by(ExterQual) %>%
  summarize(meanoverall = mean(as.numeric(OverallQual))) %>%
  ggplot(aes(x = ExterQual, y = meanoverall)) +
  geom_col() +
  labs(x = "Exterior Quality", y = "Average Overall Quality")

# Basement Quality
relevantData %>%
  group_by(BsmtQual) %>%
  summarize(meanoverall = mean(as.numeric(OverallQual))) %>%
  ggplot(aes(x = BsmtQual, y = meanoverall)) +
  geom_col() + 
  labs(x = "Basement Quality", y = "Average Overall Quality")

# Fireplace Quality
relevantData %>%
  group_by(FireplaceQu) %>%
  summarize(meanoverall = mean(as.numeric(OverallQual))) %>%
  ggplot(aes(x = FireplaceQu, y = meanoverall)) +
  geom_col() +
  labs(x = "Fireplace Quality", y = "Average Overall Quality")

# Garage Quality
relevantData %>%
  group_by(GarageQual) %>%
  summarize(meanoverall = mean(as.numeric(GarageQual))) %>%
  ggplot(aes(x = GarageQual, y = meanoverall)) +
  geom_col() +
  labs(x = "Garage Quality", y = "Average Overall Quality")