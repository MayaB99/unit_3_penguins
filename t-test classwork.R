########## Lesson 3: t-tests ############

library(tidyverse)
library(lubridate)
library(rmarkdown)


### 1sample t-tes:  1 distribution compared to theoretical mean 
#### unpaired 2-sample t-test: compare (ex flipper length of penguins
  #with disribution of flipper length of daily penguins)

## 1sample t test: 
library(palmerpenguins)
summary(penguins)
#gentoo
gentoo = penguins %>%
  filter(species == "Gentoo")

#make histogram of gentoo:
ggplot(data=gentoo) +
  geom_histogram(aes(x=body_mass_g))

##Calculate summary stats:
summary(gentoo)

##t-test for gentoo body mass
mean(gentoo$body_mass_g, na.rm=TRUE) ##output mean is 5,076.016 grams 
sd(gentoo$body_mass_g, na.rm=TRUE) ##standard deviation= 504.1162
###assumptions that must be made about the data set: 
#1. there must not be any outliers, id them
#2. make sure data is normal so: test for outliers: 
##test for outliers: 
gentoo %>%
  identify_outliers(body_mass_g)
###output is 0 rows meaning no outliers. 

##Q-Qplot- nice way to check if data looks pretty  normal
ggplot(data=gentoo) +
  stat_qq(aes(sample=body_mass_g))

## 1 sample t-test:  
  #Gntoo body mass
t.test(gentoo$body_mass_g, mu=5500) #compare with mean from EOL website

gentoo_results = gentoo %>%
  t_test(body_mass_g ~ 1, mu=5500)
##t_test format(body mass (as a function of ~) 1)
gentoo_results

##Unpaired 2 sample t-test(or independent sample t-test): 2 distributions and seeing if similar
  #ex: compare body mass of gento and Adelie penguins 
    #first create sepereate data frame to look at just what were interested in: 
data_for_t_test = penguins %>%
  filter(species %in% c("Gentoo", "Adelie")) %>%
  filter(!is.na(body_mass_g)) #filter out the NAs
summary(data_for_t_test)
#to simplify the data set we are going to remove the columns of everything but species and body
data_for_t_test = penguins %>%
  filter(species %in% c("Gentoo", "Adelie")) %>%
  filter(!is.na(body_mass_g)) %>%
  select(species, body_mass_g) %>%
  droplevels()

  summary(data_for_t_test)

  
## Calculate summary statistics
data_for_t_test %>%
  group_by(species) %>% #this will calc mean seperately for gentoo and adelie 
  summarize(mean = mean(body_mass_g),
            sd = sd(body_mass_g))

#create a historgram for it: splitview of 2 histograms per specie
ggplot(data=data_for_t_test) +
  geom_histogram(aes(x=body_mass_g)) +
  facet_wrap(~species, scales="free")

##Identify outliers- 0 rows means no outliers. 
data_for_t_test %>%
  group_by(species) %>%
  identify_outliers(body_mass_g)

##QQplot for data for t test
ggplot(data=data_for_t_test) +
  stat_qq(aes(sample=body_mass_g)) +
  facet_wrap(~species, scales="free")

##To run unpaired 2 sample t test must first check variances are similar
  #check for equality of variance: 
data_for_t_test %>%
  levene_test(body_mass_g~species) #comparing body mass distribution for each factor in the species column
  ##output- tested alternative hypothesis, pvalues=0.159 and not less than 0.05 so the data passed the levene test and the variances are equal enough to run the t-test now. 
  
  #unpaired 2-sample t test
t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species, var.equal=TRUE)
##output- the adelie body mass is significantly samller than gentoo body mass. 
data_for_t_test %>%
  t_test(body_mass_g ~ species)

## Correlations of penguin bill depth and length: 
ggplot(data=gentoo) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm))
  ##output is scatterplot to see how correlated
  #now check correlation
cor.test(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm, use="complete.obs") #complete.obs takes values of only anctual numbers and no NA or missing data
summary(gentoo)
##cor.text output, if zero that means no correlation. 

#note using just cor will just ouput the pure value but if u want detailed results use cor.text. 

##to run correlation on specific columns that only have numberical data: 
cor(gentoo[,3:6], use="complete.obs") #note placing a coma first in brackets tells R that we want all rows and only columns 3 through 6. 

##load in GGally 
library(GGally)
##to quicly look at our data relations to one another: 
Correlation_all_variables = penguins %>%
  select(species, bill_length_mm, bill_depth_mm, body_mass_g, flipper_length_mm) %>%
  ggpairs(aes(color=species))

##save plots: 
write.csv(Correlation_all_variables, 'data/processed/Total penguin variable correlation summary.csv')








