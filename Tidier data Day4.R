#Tidier data
#Day 4
#Nasreen Arend
#1 February 2019

#load libraries
library(tidyverse)
library(lubridate)

load("data/SACTNmonthly_v4.0.RData") #load R.dataset

SACTN <- SACTNmonthly_v4.0 #rename dataset, shorter name

rm(SACTNmonthly_v4.0) #The original dataset removed from env. pane

SACTN %>% #Select SACTN dataset
  filter(site == "Amanzimtoti") #Extract data from Amanzimtoti site cloumn only

SACTN %>%  #Select SACTN dataset
  filter(site == "Pollock Beach", month(date) == 12 | month(date) == 1)#Extract data for Pollock Beach site only, select the months within the site column for December or January

SACTN %>%  #Select SACTN dataset
  filter(site == "Amanzimtoti", month(date) == 11 | month(date) == 2) #Extract data for Amanzimtoti only, select the months within the site for November or February

SACTN %>% #Select SACTN dataset
  arrange(desc(temp)) #Arranges temp. from lowest to highest

SACTN %>% #Select SACTN dataset
  filter(site == "Humewood", year(date) == 1990) #Filter data to show the temp from the year 1990

SACTN %>% #Select SACTN dataset
  filter(site == "Humewood", year(date) == 1996) #Filter data to show the temp from the year 1996

#Select columns individually
Try_1 <- SACTN %>% #Renaming of dataset, appears in env pane
  select(site, src, date, temp) #Only the data in the columns: site, src, date and temp was selected

Try_2 <- SACTN %>% 
  select(site:temp) #Select data from site to temp within the renamed dataset

Try_3 <- SACTN %>% 
  select(-date, -depth) #Excluded date and depth from renamed dataset

# Select all columns except those within a given sequence
# Note that the '-' goes outside of a new set of brackets
# that are wrapped around the sequence of columns to remove
Try_4 <- SACTN %>% 
  select(-(date:depth)) #Excludes everything from date to depth

Try_5 <- SACTN %>% #Rename SACTN dataset and then;
  mutate(kelvin = temp + 273.15) #Create new column, named Kelvin, data is temp value + 273.15

SACTN %>%
  mutate(half_temp = temp/2) #Creates a new column with half of the temp. values

SACTN %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE))

SACTN %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE), #Opens the functions: mean, standard dev., min and max values for temp values
            sd_temp = sd(temp, na.rm = TRUE), #A name has to be assigned to every calculation
            min_temp = min(temp, na.rm = TRUE), #na.rm = TRUE meanes remove NA values
            max_temp = max(temp, na.rm = TRUE)) 

SACTN %>% 
  summarise(mean(temp, na.rm = T), #No name assigned, values calculated without specification.
            sd(temp, na.rm = T), 
            min(temp, na.rm = T),
            max(temp, na.rm = T))


# [A.A]
# Sufficient comments made throughut the script
# Script runs
# Neat script
# Nicely done
