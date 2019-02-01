#Tidy data
#Day 4
#Nasreen Arend
#31 January  2019

library(tidyverse)
library(lubridate)

load("data/SACTN_mangled.RData")

ggplot(data = SACTN1, aes(x = date, y = temp)) +
  geom_line(aes(colour = site, group = paste0(site, src))) + #paste0 used to group more than 1 variable
  labs(x = "Date", y = "Temperature (°C)", colour = "Site") +
  theme_bw()

#Tidy data should be longer(rows) than broader(columns)
  
SACTN2_tidy <- SACTN2 %>%
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp") #this function allows all the sources to be grouped into one column

SACTN3_tidy <- SACTN3 %>% 
  spread(key = var, value = val) #allows columns to be joined. same data

SACTN4a_tidy <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ") #seperates columns. c function used to seperate more than 1

SACTN4b_tidy <- SACTN4b %>% 
  unite(year, month, day, col = "date", sep = "-") #3 columns: day, month and date joined into one column, date. sep by -.

SACTN4_tidy <- left_join(SACTN4a_tidy, SACTN4b_tidy) #datasets combined 
