#Plotting in R using ggplot2
#Day_2
#Nasreen Arend
#30 January 2019

library(tidyverse) #Load libraries

chicks <- datasets::ChickWeight #Assigns a name to the CW dataset
??ChickWeight #Help is enabled

ggplot (data = chicks, aes(x = Time, y = weight, colour = Diet)) + #Parent line, aes used to specificy which data to look at
  geom_point() + #When plotting + is used instead of %>% 
  geom_line(aes(group = Chick)) #Groups the Chick variable in dataset, similiar to sites  
  
ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") #Using the linear model in the geom_smooth method to show the most effective diet

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(colour = "blue") +
  geom_line(aes(group = Chick))

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(aes (size = weight)) + #aes function allows the point size to reflect the weight, size of point corresponds to weight
  geom_smooth(method = "lm") +
  labs(x = "Days", y = "Weight (kg)") +#labs function allows labels to be changed
  ggtitle("A") +
  theme_bw() #allows the graph's theme to change

#Practice Exercise

Bea <- datasets::beaver1

ggplot(Bea, aes(x = time, y = temp, colour = day)) +
  geom_point() +
  geom_line(aes(group = day))


#Facetting in ggplot

library(ggpubr)

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Diet, ncol = 2) #Depicting different figures, wrapping them in the same plot. ~ specifies that Diets should be split into diff panes. ncol = number of columns


chicks_2 <- chicks %>% 
  filter(Time == 21) #Filters the data for the 21st day only

plot_1 <- ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) + #Saves name to graph
  geom_point() +
  geom_line(aes(group = Chick)) +
  labs(x = "Days", y = "weight") +
  ggtitle("A") #plot did not appear because it was saved in the env pane for later use. double click on plot name + run, it can appear in 4th pane

plot_2 <- ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("B")
plot_2

plot_3 <- ggplot(data = chicks_2, aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100) + #position dodge places histogram along side eachother, binwidth det. the width of graphs
  labs(x = "Final Mass (g)", y = "Count")
plot_3

plot_4 <- ggplot(data = chicks_2, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) +
  labs(x = "Diet", y = "Final Mass (g)")
plot_4

plot_combined <- ggarrange(plot_1, plot_2, plot_3, plot_4) #ggarange found in ggpubr package

#3rd library

library(boot)

urine <- boot::urine
??urine

urine %>% 
  select(-cond) #Using select function and minus, removes cond from dataset, appears in console

ggplot(data = urine, aes(x = osmo, y = ph)) + #Specifies dataset, aes specifies which variables should be included
  geom_point(aes(colour = cond)) #Specifies which graph should be used (scatter plot), aes specifies that colour intensity should change with conductivity

ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = as.factor(r))) 


# [A.A]
# Script runs complete, Neat script
# Should add a few more comments

