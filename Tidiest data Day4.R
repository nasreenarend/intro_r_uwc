#Tidiest data
#Day 4
#Nasreen Arend
#1 February 2019

library(tidyverse)
library(lubridate)

load("data/SACTNmonthly_v4.0.RData") #Load data because is the R.Data format

SACTN <- SACTNmonthly_v4.0 #Rename dataset to one with a shorter name

rm(SACTNmonthly_v4.0) #Removes original dataset from env pane

SACTN_depth <- SACTN %>% #Select and rename dataset
  group_by(depth) %>%  #Group data by depth
  summarise(mean_temp = mean(temp, na.rm = TRUE), #assign names to calculated values; mean_temp and count.Mean_temp calculated with summarise function, excluding na values.
            count = n()) #Count is the number of temp entries for each depth
SACTN_depth #View dataset or double click on name

ggplot(data = SACTN_depth, mapping = aes(x = depth, y = mean_temp)) + #Plotting depth and temp
  geom_point(aes(size = count), alpha = 1/3) + #Point size increases as count increases
  geom_smooth(se = FALSE) + #A line fitted to plot
  ggtitle("Relationship between temperature and depth") + #Adding title
  labs (x = "Depth", y = "Temperature") + #Addind labels
  theme_bw() #Adding theme

SACTN_30_years <- SACTN %>% #Rename dataset
  group_by(site, src) %>% #Grouping only site and src values
  filter(n() > 360) #Extract data for sites with more than 360 months of data

selected_sites <- c("Paternoster", "Oudekraal", "Muizenberg", "Humewood") #Concatinate creates a vector or set with only desired sites and created set in named with <- 

SACTN %>% #Select dataset 
  filter(site %in% selected_sites) %>% #Filter site column in selected sites
  group_by(site, src) %>%  #groups only site and src data
  summarise(mean_temp = mean(temp, na.rm = TRUE), #stat calculations named. Mean temp calculated and na values are excluded
            sd_temp = sd(temp, na.rm = TRUE)) #Standard deviation calculated and na values are excluded

# [A.A]
# Greating comments
# Sjoh! NIcely done
