########### Lecture notes ##############3
 library(palmerpenguins)
 
 ##Install packages: 
 library(tidyverse)
 library(GGally)
 library(ggiraph)
 library(ggiraphExtra) #for predictions
 library(car)
 library(broom)

 ######## Multiple regressions ########
 
 penguins_lm_3 = penguins %>%
   filter(!is.na(bill_depth_mm),
          !is.na(bill_length_mm),
          !is.na(sex))
 
# (##Build a model to take quick look: 
 lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data=penguins_lm_3)
 summary(lm_3) ## Output: model did a good job bc the coefficient is significant (adjusted r-squared=0.765).
 #It explained abt 3/4 (or 0.76) of the variation seen it y.) 
 
 ##Interaction term- relationshp btwn bill depth and bill length was parallel for all 3 species and adding the interactiong term allows the model to changes the slope
    #of the relationship between the 2 vaiables. 
 
 ##Adding an interaction term to models: 
 lm_4 = lm(bill_depth_mm ~ bill_length_mm + species, data=penguins_lm_3)
 #lm_4 is a function bill depth and bill length for each species. In gemeral if you interact with variables you want to run them on their own. 
 summary(lm_4) #output- both interaction coefficients were not significant so they did not do a good job. Adjusted Rsquared also declined. 
  ##Thus, lm_4 was a bust and we would we revert back to the most recent function before it. 
 
 
 
 ##Step functions- takes complex models and dismantles the variables to find best possible model as a subset of your most complex model. 
  ##makes model serially less complex to benefit the best fit. 
 
##Building a step function: 
best_model = step(lm_4)
summary(best_model) #outputs are same as model 3 so we use lm_3 
AIC(lm_3, lm_4) #$calculates akaike information criteria for each model. tells negatived likelihood that has been penalized.
  #AIC output, small AIC= better fit as its a function of negative likelihood that model is correct. AIC diff of 2 units or more is statistically significant. 


(lm_gentoo_3)
summary(lm_gentoo_3)
AIC(lm_gentoo_3, lm_gentoo_2, lm_gentoo_1)


##Predictions in tidyverse: 
newdata = penguins_lm_3 %>%
  expand(bill_length_mm, species)
head(newdata)
  ##if we have a bill legth of 33, what would the bill depth be of our model with it? 
lm_4_prerdict = lm_4 %>%
  augment(newdata = newdata, se_fit=TRUE) %>%
  mutate(lwr = .fitted - .se.fit*1.96,
         upr = .fitted + .se.fit*1.96) ##Add 95% confidence intervals 
head(lm_4_prerdict)
  ##Now plot the lm_4 data: 
ggplot() +
  geom_point(data=penguins_lm_3, aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_line(data=lm_4_prerdict, aes(x=bill_length_mm, y=.fitted, color=species)) +
  geom_ribbon(data=lm_4_prerdict, aes(x=bill_length_mm, ymin=lwr, ymax=upr, fill=species), alpha=0.5)


### Multiple regressions with 2-3 continuous variables:  It changes how you generate predictions.   

gentoo = penguins %>%
  filter(species == "Gentoo")
summary(gentoo)

##Build function: 
lm_gentoo_1 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
lm_gentoo_2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data=gentoo)
lm_gentoo_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, data=gentoo)
  #Check for multicolliniarity: 
vif(lm_gentoo_3) # model 1- not multicollinear; 4-5= kinda multicollinear; vif>=10probnlem!!
  #vif output- all numbers very low. compares how well they predict the y. 

##Step fumction: 
best_model = step(lm_gentoo_3)
summary(lm_gentoo_3)
AIC(lm_gentoo_3, lm_gentoo_2, lm_gentoo_1) #in order from most complex model. 
  #output: lm_gentoo_3 did a signfiiantly  better job at predictions bc its much lower than the other 2 models. 


##Prediction figure 
newdata = gentoo %>%
  expand(bill_length_mm) %>%
  mutate(flipper_length_mm = median(gentoo$flipper_length_mm, na.rm=TRUE),
         body_mass_g = median(gentoo$body_mass_g, na.rm=TRUE))
summary(newdata)
head(newdata)

##Create prediction dataframe for lm3 gentoo: 
lm_gentoo_3_predict = lm_gentoo_3 %>%
  augment(newdata = newdata, se_fit=TRUE) %>%
  mutate(lmr = .fitted - .se.fit*1.96,
         upr = .fitted + .se.fit*1.96)
head(lm_gentoo_3_predict)
  ##Now plot it: 
ggplot() + 
  geom_point(data=gentoo, aes(x=bill_length_mm, y=bill_depth_mm)) +
  geom_line(data=lm_gentoo_3_predict, aes(y= .fitted, x=bill_length_mm)) +
  geom_ribbon(data=lm_gentoo_3_predict, aes(x=bill_length_mm, ymin=lwr, ymax=upr), alpha=0.5) +
  annotate("text", x=53, y=14, label=paste0("flipper_length = ", median(gentoo$flipper_length_mm, na.rm=TRUE))) +
  annotate("text", x=53, y=13, label=paste0("body_mass = ", median(gentoo$body_mass_g, na.rm=TRUE)))
           

##Exercise 5.3: 
#   Plot the model predictions from our model lm_gentoo_3 so that we can see the variation in bill depth vs. flipper length while holding bill length and body mass constant at their medians. 


##ANOVA

penguin_lm = lm(body_mass_g ~ species + sex, data=penguins)
summary(penguin_lm)
anova(penguin_lm)#output- gentoo was significantly different from adelie, sex male is sig diff from female in anova table. 
#tukey test tells which species are diff from eachothere. 
penguins_anova = aov(body_mass_g ~ species + sex, data=penguins)
summary(penguins_anova)
  #run tukey test on anova model: usually run for categorical variables. 
TukeyHSD(penguins_anova)
  #output- diff in mean of 2 speacies so 26.9 is bigger and pvalue is not statistically sig between chinstrap and adelie. 


