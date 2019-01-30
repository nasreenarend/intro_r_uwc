#Day_1.R
#Laminaria dataset exploring and learning
#Nasreen Arend
#29 January 2019
#Trial

#Loading libraries

library(tidyverse)
lam <- read_csv("data/laminaria.csv") #explore data

head(lam) #shows first 6 rows
tail(lam) #shows last 6 rows
head(lam, n = 3) #shows first 3 rows

lam_select <- lam %>%
  select(site, total_length) %>% #selecting only the site and total length columns
  slice(54:80) #selecting only rows 54 to 80

lam_kom <- lam %>% #In the Laminaria dataset
  filter(site == "Kommetjie") #filter from the site column, the site Kommetjie 

#In the Laminaria dataset select only site and blade_length column
#Filter only for Sea Point

lam_sample <- lam %>%
  select(site, blade_length) %>%
  filter(site == "Sea Point")

lam %>% #selecting laminaria dataset
  filter(total_length == max(total_length)) #Filtering the total lemgth that is equal to maximum total length 
summary(lam) #explore dataset
dim(lam) #dimensions of a large dataset

lam %>% 
  summarise(avrg_bl = mean(blade_length),
            med_bl = median(blade_length),
            sd_bl = sd(blade_length))


lam %>%
  group_by(site) %>%
  summarize(var_bl = var(blade_length),
            n = n()) %>%
  mutate(se = sqrt(var_bl/n))

lam2 <-  lam %>%
  select(-blade_thickness, -blade_length) #Removed these two variables from laminaria dataset

lam %>%
  select(blade_length) %>% #The blade length data was selected
  na.omit %>% #NA values excluded 
  summarise(n = n())

lam %>%
  select(blade_length) %>% 
  summarise(n = n())

ggplot(data = lam, aes(x = stipe_mass, y = stipe_length)) +
  geom_point(shape = 21, colour = "salmon", fill = "white") +
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)")
