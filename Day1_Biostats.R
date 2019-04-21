#Nasreen Arend
#9 April 2019
#Introduction to Biostsats
#Day 1

library(tidyverse) #Load libraries
library(lubridate)
library(ggpubr)
library(readr)

chicks <- as_tibble(ChickWeight) #Assigns a name to the loaded dataset
dim(chicks)     #Gives the dimensions of the data
summary(chicks) #Gives the min, mean, max and quartile of data. 
                #Ordinal data. 
                #Distribution of data
ncol(chicks) #Shows the number of columns of the dataset
nrow(chicks) #Shows the number of rows of the dataset

chicks %>% 
  summarise(avrg_weight = mean(weight)) #The average weight of the whole dataset

chicks %>%  #Using the chicks dataset
  filter(Time == 0) %>% #Focuses only on the 0 day in the dataset
  group_by(Diet) %>% #Only grouping by diet
  summarise(mean_wt = mean(weight), #The mean weighht, sd weight and max weight was calculated of day 0 from each diet
            sd_wt = sd(weight),
            max_wt = max(weight))

#the mean
nums <- c(113, 666, 13, 77, 38, 13) #Created vector using the concatinate funct

sum(nums)/length(nums) #The long way of calculating the mean. Sum of all numbers divided by the length of all numbers
mean(nums) #The mean of the created vector

num1 <- c(21, 25,NA,36, 55, 68)
mean(num1) #Could not be calculated due to NA value
?mean #Gives information about the ean function

mean(nums,na.rm = TRUE) #na.rm = TRUE is an argument within the function mean, that removes na values

chicks %>% 
  summarise(min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25), 
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight))

summary(chicks$weight)

group_chick_1 <- chicks %>%
  filter(Time < 12) %>% 
  group_by(Diet, Time) %>% 
  summarise(mean_wt = round(mean(weight, na.rm = TRUE), 2),
            med_wt = median(weight, na.rm = TRUE),
            sd_wt = round(sd(weight, na.rm = TRUE), 2),
            sum_wt = sum(weight),
            min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight),
            n_wt = n())
range(chicks$weight)
chicks %>% 
  summarise(lower_wt = range(weight)[1],
            upper_wt = range(weight)[2])

summary(chicks$weight)

grp_stat <- chicks %>%
  filter(Time <12) %>% 
  group_by(Diet, Time) %>% 
  summarise(mean_wt = round(mean(weight, na.rm = TRUE), 2),
            med_wt = median(weight, na.rm = TRUE),
            sd_wt = round(sd(weight, na.rm = TRUE), 2),
            sum_wt = sum(weight),
            min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight),
            n_wt = n())
grp_stat

library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(ggthemes)

iris.cnt <- iris

iris.cnt <- iris %>%
  count(Species) %>% # automagically creates a column, n, with the counts
  mutate(prop = n / sum(n)) # creates the relative proportion of each species
iris.cnt



plt1 <- ggplot(data = iris.cnt, aes(x = "", y = n, fill = Species)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()

plt2 <- ggplot(data = iris.cnt, aes(x = "", y = prop, fill = Species)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()

plt2

plt3 <- plt1 + coord_polar("y", start = 0) +
  labs(title = "Friends don't let...", subtitle = "...friends make pie charts",
       x = NULL, y = NULL) +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal()

# Very neat script
# Runs nicely
# Good comments


               
  
