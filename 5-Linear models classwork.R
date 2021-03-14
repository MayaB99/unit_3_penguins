####### Lesson 5- linear models ##########


##Load packages: 
library(tidyverse)
library(palmerpenguins)
library(GGally)
library(ggiraph)
library(ggiraphExtra)
library(car)
library(broom)


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



###########################################
#             BUILDING MODELS
##############################3###########


library(palmerpenguins)
summary(penguins)
penguins_lm_3 = penguins%>%
  filter(!is.na(sex),
         !is.na(bill_depth_mm),
         !is.na(bill_length_mm))
summary(penguins_lm_3)



#building models: 
lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data=penguins)
coef(lm_3)[2]
anova(lm_3)

##analysis of variance is equivalent to a linear model 

##output data to a csv: 
  broom::tidy(lm_3)
  
broom::tidy(lm_3, conf.int=TRUE) %>%
  mutate_if(is.numeric, round, 2)

ggPredict(lm_3, se=TRUE)
##Interactive plot: 
ggPredict(lm_3, se=TRUE, interactive = TRUE)

##Predict with predict() function
  #first build function
lm_3_predictions = predict(lm_3)
lm_3_predictions = predict(lm_3, interval = "confidence")
head(lm_3_predictions)
head(penguins_lm_3)
  #create new dataframe and column combine with data we used to build model and predictions
penguins_lm_3_predict = cbind(penguins_lm_3, lm_3_predictions) ####CHECK ON ERRORS OF THIS CODE LINE
add(penguins_lm_3_predict) #### CHECK ERRORS IN FUNCTION NAME

#Plot predictions: generate models predictions on known data set. ### ERROR IN NO DATA EXISTS 
ggplot(data=penguins_lm_3_predict,
       aes(y=bill_depth_mm, x=bill_length_mm, color=species)) +
  geom_point()+
  geom_line(aes(y=fit)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=species), color=NA, alpha=0.5) +  ##adding confidence interval levels 
  theme_bw()
  

##Generate predictions with new full range of data: 
  #first create new data frame of all possible variable we want 
newdata_bill_length_mm = seq( min(penguins$bill_length_mm), max(penguins_lm_3$bill_length_mm),by=0.1 ) #### CHECK ERRORS 
  #get this associated with all 3 species: 
newdata = expand.grid(bill_length_mm = newdata_bill_length_mm, species = unique(penguins_lm_3$species))
tail(newdata)

newdata_predictions = predict(lm_3, newdata = newdata, interval="confidence")
head(newdata_predictions)
newdata_predict_lm_3 = cbind(newdata_predictions)
head(newdata_predict_lm_3)

ggplot() + 
  geom_point(data=penguins_lm_3, aes(y=bill_depth_mm, x= bill_length_mm, color=species)) +
  geom_line(data=newdata_predict_lm_3, aes(yfit, x=bill_length_mm, color=species)) +
  geom_ribbon(data=newdata_predict_lm_3, aes(ymin-lwr, ymax=upr, x=bill_length_mm, fill=species), color=nA, alpha=0.5)


##generate predictions with new data in tidy verse: 
tidy_predict = lm_3 %>%
  broom::augment(penguins_lm_3, se_fit=TRUE) %>%
  mutate(lwr = .fitted - 1.96* .se.fit,
         upr = .fitted + 1.96* .se.fit)
  #plot it
ggplot() + 
  geom_point(data=penguins_lm_3, aes(y=bill_depth_mm, x= bill_length_mm, color=species)) +
  geom_line(data=tidy_predict, aes(y=.fitted, x=bill_length_mm, color=species)) +
  geom_ribbon(data=tidy_predict, aes(ymin-lwr, ymax=upr, x=bill_length_mm, fill=species), color=nA, alpha=0.5)











