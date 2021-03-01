#####Ggplots: 
library(palmerpenguins)

##find a function using: find("filter")- this will use the version from dplyr
##namesapce- all the special words that come w the package. 

find("filter")
head(penguins)
data=penguins

###################################################
#               scatterplot
###################################################

ggplot() + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species), data=penguins)
##we are mapping a feature of an aestetic feature of the plot in the x axis (above) 
##inside parenthese with x is the aestetics and putting color inside vs out will differentiate what is colored. 
  ##inside aestics color= by grouping, outside= total plot color. 
    ##Ex: 
      ggplot() + 
         geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=sex, size=bill_length_mm), data=penguins)
            ##above output is outside aestetic, this feature of size is menat to be mapped on some aspect of our data. 
  ##can add a smother to plots above: ex
      data=penguins
      ggplot() + 
        geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species), data=penguins)+
        geom_smooth(aes(x=flipper_length_mm, y=body_mass_g))
      ##output has smoother function to guide you
      ggplot(aes(x=flipper_length_mm, y=body_mass_g), data=penguins)+ 
        geom_point(aes(color=species))+
        geom_smooth(aes(color=species))
        
##hack- want a daily look? filter by using filter func. 
      ggplot(aes(x=flipper_length_mm, y=body_mass_g), data=data %>% filter(species=="Adelie"))
        geom_point() +
          geom_smooth(method="lm")+
          ylab("Body mass (g)")+
          xlab("Flipper length (mm)")+
          ggtitle("Penguins are cute")+
          theme_classic()
        
rlang::last_error()
      
####Exercise 2.1- Build scatter plot A build up versus build length for Adelie penguins. Map the point colors to the island that the Adelie Penguins was observed on. Add access labels and Main title. 
      ##first filter out specific penguins we want to look at: 
      Adelie_Penguins <- penguins %>% filter(species == "Adelie")
      ##output pulls new data table of just penguins with adelie in specie name 
      ggplot(aes(x= bill_depth_mm, y = bill_length_mm, color = island), 
             data = Adelie_Penguins)+
        geom_point(aes(colors = island))+
        labs(y = "Bill Length (mm)", x= "Bill Depth (mm)")+
        ggtitle("Bill Depth vs. Bill Length for Adelie Penguins")
      
###how has penguin catch differed between years? 
      penguins_ts = penguins %>%
        group_by(year) %>%
        summarize(num =n())
      penguins_ts
ggplot(aes(x=year, y=num, color=species), data=penguins_ts)+ 
  geom_plot()+
  geom_line()




#############################################
#             histograms
#############################################
ggplot(data=penguins) +
  geom_histogram(aes(x=flipper_length_mm))
##to make plot above look more normal: 
ggplot(data=penguins %>% filter) +
  geom_histogram(aes(x=flipper_length_mm))

ggplot(data=penguins) +
  geom_histogram(aes(x=flipper_length_mm, fill=species), color="black")


ggplot(data=penguins) +
  geom_histogram(aes(x=flipper_length_mm, fill=species), color="black", position="identity", alpha=0.5)
ggplot(data=penguins) +
  geom_histogram(aes(x=flipper_length_mm, fill=species), color="black", position="identity", alpha=0.5, binwidth=5)

##custom colored histogram: 
ggplot(data=penguins) +
  geom_histogram(aes(x=flipper_length_mm, fill=species), position="identity", alpha=0.5, binwidth=5)+
  scale_fill_manual(values=c("springgreen2", "salmon3", "turquoise3"))


##creat a boxplot: 

ggplot(data=penguins)+
  geom_boxplot(aes(y=flipper_length_mm, x=species))+
  geom_jitter((aes(y=flipper_length_mm, x=species, color=species)))
  
##create a barplot: 
  ggplot(data=penguins) +
    geom_bar(aes(x=sex, fill=species))
  
  ##seperate plot of daily chin strap and sperate for gentoo species: 
  ggplot(data=penguins) +
    geom_bar(aes(x=sex, fill=species))+
    facet_wrap(~species)
    ###output is 3 seperate color coded plots of each specie. 
  
  ##create barplot horizontally and remove NA sex: and save it
  ggplot(data=penguins %>% filter(!is.na(sex))) +
    geom_bar(aes(x=sex, fill=species))+
    coord_flip()+
    facet_wrap(~species, ncol=1)
  ggsave(filename="figures/penguin_sex_bars.png", device="png", width=4, height=4, dpi=300, units=5)


colors()
##output gives list of R color names. 





      
      
      
      
      