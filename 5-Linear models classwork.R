####### Lesson 5- linear models ##########


##Load packages: 
library(tidyverse)
library(palmerpenguins)
library(GGally)
library(ggiraph)
library(ggiraphExtra)
library(car)


## fast way to see how all variables are correlated to eachother. 
penguins %>%
  select(-island, -sex, -year) %>%
  GGally::ggpairs(aes(color=species)) 
summary(temp)

##Look at relationship betwee bill length and bill depth: 
penguins %>%
  select(bill_length_mm, bill_depth_mm) %>%
  ggpairs()
  #output shows plot with little correlation on scatterplot


##Build a linear model: 
lm_1 = lm(bill_depth_mm ~ bill_length_mm, data=penguins)
summary(lm_1)

  ##plot it!
ggplot(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm)) +
  geom_point() +
  geom_smooth(method ="lm") # added a smooth line trend 

##plot lm function to different plot types:
  ## this outputs a residuals vs fitted and it shows a non homogonous variance
class(lm_1)
plot(lm_1) ## conitnue to hit enter will show up a different type of plot each time. 

head(gentoo)


##Now separate out the species to build a better model
gentoo = penguins %>%
  filter(species=="Gentoo")
  #run a quick correlation matrix on it: 
gentoo %>%
  select(bill_depth_mm, bill_length_mm) %>%
  ggpairs()

lm2 = lm(bill_depth_mm ~ bill_length_mm, data = gentoo)
class(lm2)
summary(lm2)
  #create correlation plot: 
ggplot(aes(y=bill_depth_mm, x=bill_length_mm), data=gentoo) +
  geom_point() +
  geom_smooth(method="lm") ##smoothed trend line added 
  #edit plot to show all species separated by color: 
ggplot(aes(y=bill_depth_mm, x=bill_length_mm, color=species), data=penguins) +
  geom_point() +
  geom_smooth(method="lm")
##different way to make plot above while adding total overall trendline through all species
ggplot(data=penguins) +
  geom_point(aes(y=bill_depth_mm, x=bill_length_mm, color=species)) +
  geom_smooth(aes(y=bill_depth_mm, x=bill_length_mm, color=species), method="lm") +
  geom_smooth(aes(y=bill_depth_mm, x=bill_length_mm), method="lm", color="purple4")

colors()


##Exercise 5.1: build a linear model predicting Gentoo bill length as a function of flipper length.
  #Plot the predictions. Which explanatory variable does a better job at predicting bill depth? whats your evidence? 

lm_exercise = lm(bill_depth_mm ~ flipper_length_mm, data= gentoo)
class(lm_exercise)
summary(lm_exercise)

ggplot(aes(x=flipper_length_mm, y=bill_length_mm), data=gentoo)+
  geom_point()+
  geom_smooth(method= "lm") #rsquared value= 0.495










