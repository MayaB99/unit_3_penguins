######## Unit 3- Lesson 1: dplyr ###############

##Install tidtverse: 
installed.packages("tidyverse")
##load package to use every time in R: 
library("tidyverse")
###can check to see what packages tidyverse loaded by: 
tidyverse_packages()

##download penguin data from packages: 
install.packages("palmerpenguins")

##load in penguin data: 
library(palmerpenguins)

head(penguins) ##looks at first 6 rows of data set
summary(penguins)
dim(penguins) ## tidyverse version of head
##using a head command om tables in tidyverse are called tibbles 

class(penguins)
##output shows "tbl_df" tells us that tidyverse worked with the dataframe to output it 

glimpse(penguins)


##########################################
#           Intro to dplyr
##########################################
##filter function: use to separate specific sections of data
ladies = filter(penguins, sex=="female")

summary(ladies)
##output shows all females and zero males so we know it workes

###string filters together: 
gentoo_ladies = filter(penguins, sex=="female", species== "Gentoo")
summary (gentoo_ladies)
 
###Notes- the first parameter of all the functions in the dplyr package is the dataset

#### the pipe function "%>%"- strings different functions together: so first grab data then filter it 
#Ex: create gentoo ladies table: 
gentoo_ladies = penguins %>% filter(sex=="female", species=="Gentoo")
summary(gentoo_ladies)


##spacing: typical style for coding in dplyr is carriage return after every pipe. To combat, place cursor anywhere over pip commands and run command and it will run it all
#spacing (cont)- if you filter by multiple rules like this female and gentoo, put each rule on a new line: 
gentoo_ladies = penguins %>% 
  filter(sex=="female", 
         species=="Gentoo") %>%
  ##pipe after gentoo will take your column and collects it to a single value 
  summarize(mean_body_mass_g = mean(body_mass_g))  #Ex: summarize mean bodymass of gentooladies
gentoo_ladies


## Compare to base R: 
female_penguins = penguins[which(penguins$sex=="female"), ]
gentoo_ladies = female_penguins[which(female_penguins$species=="Gentoo"), ] ##comma and spae at the end indicates we want all columns of data. 
mean(gentoo_ladies$body_mass_g)


#Exercise 1.1 : Build a data set that contains only chinstrap penguins. Then build another data set that contains only chinstrap penguins with a flipper length greater than 200 mm. 
#1.1cont.- What is the sex ratio of chinstrap penguins? How does that compared to the sex ratio of chinstrap penguins with a flipper length greater than 200 mm? use summary function 

chinstrap <- penguins %>%
  filter(species=="Chinstrap")
glimpse(chinstrap)

chinstrap_200 <- penguins %>%
  filter(species=="Chinstrap",
         flipper_length_mm > 200)
glimpse(chinstrap_200)

summary(chinstrap)
summary(chinstrap_200)
###output results show first summary an even split of slipper length between male and female but the 200 filter resulted in only 1 
  #output cont.- female and the rest males suggesting that males typically have longer flippers. 



####Let's find the ass of each species by using the groupby term: 
penguin_masses = penguins %>%
  group_by(species) %>%  ##separate each species into a distinct group
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm = TRUE)) ##na.rm takes the NA out of the data so we can calculate the means
penguin_masses


##want mean body mass for each group and each species: 
penguin_masses = penguins %>%
  group_by(species, sex) %>%  ##separate each species into a distinct group
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm = TRUE))
penguin_masses
##now filter out the NAs that are in resultig new output: 
penguin_masses = penguins %>%
  filter(is.na(sex)) %>%
  group_by(species, sex) %>%  ##separate each species into a distinct group
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm = TRUE))
penguin_masses ##output result is all the NA's of the data set together. 
##to flip filter to show only data that does not contain NA put "!" before is.na: 
penguin_masses = penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%  ##separate each species into a distinct group
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm = TRUE))
penguin_masses ## output is all species without the NA as a sex name  


##Saving data in tidyverse: 
#Note: base r and tidyverse different: 
  #Base R= write.csv()
  #tidyverse= write_csv()
write_csv(penguin_masses, 'data/processed/penguin_masses_classwork.csv')



####### More dplyr functions ###########

penguins %>%
  group_by(species) %>%
  summarize(num = n()) ##output shows how many individuals we have of each species

##count number of species found in each island: 
num_species_island = penguins %>%
  group_by(species, island) %>%
  summarize(num = n())
num_species_island

#mutate function: convert g to lbs for body mass
penguins_for_america = penguins %>%
  mutate(body_mass_lbs = body_mass_g * 0.0022)
head(penguins_for_america)  ##output didn't show body mass bc sometimes it likes to keep things on same line so it will hide it. 
glimpse(penguins_for_america)

#Distinct function: what are the islands we have data for?
penguins %>%
  distinct(island) ##will printout whatever is unique in the data set. also removes any duplicates. 

#select function: chooses specific row or column
penguins %>%
  select(species)
##or remove everthing I dont want with a minus before: 
penguins %>%
  select(-bill_length_mm, -bill_depth_mm)

##arrange function: arrange data by specific type
penguins %>%
  arrange(rev(body_mass_g)) ##rev will reverse the order 


