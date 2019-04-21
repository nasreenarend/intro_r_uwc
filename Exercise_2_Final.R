#Exercise 2
#Three sections of R problems to be solved
#Nasreen Arend
#5 February 2019

#Load libraries
library(tidyverse)
library(ggpubr)
library(readr)
library(lubridate)

#LOad dataset
load("data/SACTNmonthly_v4.0.RData") #Assign new name to loaded dataset
SACTN_plot <-  SACTNmonthly_v4.0 #Dataset assigned a name, using SACTNmonthly_v4.0 dataset

kznsb.temp <- SACTN_plot #SACTN_plot dataset assigned a new nae kznsb.temp

first <- SAC %>% #SAC dataset assigned the name first
  mutate(year = format(date, "%Y")) %>% #A new column was created, with the date format specified
  group_by(site, year) %>% #plot was grouped by site and year
  summarise(average_temp = mean(temp, na.rm = TRUE)) #Average temp for every year was calculated

kznsb.temp <- temp %>% 
  filter(src == "KZNSB") #Filtered by source. KZNSB

SAC <-  kznsb.temp %>% 
  filter(src ==  "KZNSB") #Filtered by source. KZNSB

Temp_final <- ggplot(first, aes(x = year, y = average_temp)) + #New name assigned, Temp_plot. Using first dataset , the x and y values were specified.
  geom_line(aes(group = site, colour = "red")) + #Plot line grouped by site and the colour red was assigned
  facet_wrap(~site, ncol = 5) + #Seperate graphs joined into into plot with 5 columns
  labs(x = "Year", y = "Temperature (C)") + #Labels assigned to x and y axis
  scale_y_continuous(breaks = seq(20, 24, 2)) + #y axis scale specified, continuous scale
  scale_x_discrete(breaks = seq(1980, 2014, 20)) #x axis scale specified, discrete scale

#Laminaria plot
laminaria <- read_csv("data/laminaria.csv") #Dataset assigned a name. The laminaria csv file is loaded by the read_csv function

falsebay <- laminaria %>% #Assign a name to the laminaria dataset
  filter(region == "FB") #The dataset is filtered by the FB region

Blade_plot <- ggplot(falsebay, aes(x = blade_length, y = blade_weight)) + #Assigning a nae to the plot. using the falsebay dataset, the x and y axis was specified
  geom_line(aes(colour = site), size = 1) + #Specifies the line colour according to site
  geom_point(aes(colour = site), size = 4) + #Point size and colour specified
  facet_wrap(~site, ncol = 3) + #Allows graph to be plot on one plot with 3 columns 
  labs(x = "Blade length (cm)", y = "Blade mass (kg)") + #Add labels to x and y axis
  scale_colour_brewer(palette = "Accent") #Allowed maximum for Accent pallete is 8, so last graph is removed



#Tooth growth data
toothgrowth <- datasets::ToothGrowth %>% #Data loaded with dataset:: function and assigned a name 
  group_by(supp, dose) %>%  #Grouped by supplement and dose
  summarise(mn.ln = mean(len), #Summarise the mean length and standard deviation length 
            sd.ln = sd(len))

tooth_col <- ggplot(toothgrowth, aes(x = dose, y = mn.ln, fill = supp)) + #New name assigned using toothgrowth dataset to create graph with x value being dose and y being mn.ln with the fill specified by supp
  geom_col(aes(fill = supp), position = "dodge", colour = "navyblue") + #A column plot used, using the supp column as a fill, "dodge" function used to align the columns
  geom_errorbar(aes(ymin = mn.ln - sd.ln, #Error bars inserted in columns using the mean and standard deviation values on the y axis
                    ymax = mn.ln + sd.ln),
                position = "dodge") + #Aligns the position of error bars
  labs(x = "Dose (mg/d)", y = "Tooth length (mm)") + #Labels assigned
  ggtitle("The relationship between vitamin C dosage (mg/d) tooth length(mm)") #Title added
  
# Good comments throughout the script
# The SAC dataset used in line 18 is not previously identifed 
# Script is neat: making it easier for the marker which is important

