#Section 4
#Nasreen Arend
#2 February 2019

# Make use of any two built in datasets:
# Make use of the summarise, select, group_by functions
# Create at least two visualisations that were not done in the Intro R workshop. Density plot 

library(tidyverse)
library(ggplot2)

toothgrowth <- datasets::ToothGrowth %>% #Data loaded with dataset:: function and assigned a name 
  group_by(supp, dose) %>%  #Grouped by supplement and dose
  summarise(mn.ln = mean(len), #Summarise the mean length and standard deviation length 
            sd.ln = sd(len))

tooth_col <- ggplot(toothgrowth, aes(x = dose, y = mn.ln, fill = supp)) + #
  geom_col(aes(fill = supp), position = "dodge", colour = "navyblue") + #A column plot used, using the supp column as a fill, "dodge" function used to align the columns
  geom_errorbar(aes(ymin = mn.ln - sd.ln, #Error bars inserted in columns using the mean and standard deviation values on the y axis
                    ymax = mn.ln + sd.ln),
                position = "dodge") +
  labs(x = "Dose (mg/d)", y = "Tooth length (mm)") +
  ggtitle("The relationship between vitamin C dosage (mg/d) tooth length(mm)")

chick_1 <- datasets::ChickWeight #The chickWeight dataset was loaded by using the "dataset::" function and assigning it a name with the " <- " function

chick_dens <- ggplot(chick_1, aes(x = weight)) + #A name was assigned to the graph usins the chick_1 dataset. aes function was used to specify the x values
  geom_density(color = "darkblue", fill = "lightblue") #A density plot was chosen, it plots density vs weight with a dark blue outline and light blue fill.

chick_dens2 <- ggplot(chick_1, aes(x = weight, color = Diet)) + #A name was assigned to the graph usins the chick_1 dataset. aes function was used to specify the x values while each diet was assigned a colour.
  geom_density() #A density plot was chosen



#Tidy data should be longer(rows) than broader(columns)
#Load libraries
library(tidyverse) 
library(lubridate)

load("data/SACTN_mangled.RData") #Load SACTN_mangled R data

SACTN2_tidy <- SACTN2 %>%
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp") #this function allows all the sources to be grouped into one column

SACTN3_tidy <- SACTN3 %>% 
  spread(key = var, value = val)
#allows columns to be joined. same data

SACTN4a_tidy <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ") #seperates columns. c function used to seperate more than 1

SACTN4b_tidy <- SACTN4b %>% 
  unite(year, month, day, col = "date", sep = "-") #3 columns: day, month and date joined into one column, date. sep by -.

SACTN4_tidy <- left_join(SACTN4a_tidy, SACTN4b_tidy) #datasets combined 





               
               

             