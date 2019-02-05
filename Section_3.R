#Section 3
#Nasreen Arend
#2 February 2019

library(tidyverse) #Load libraries. Tidyverse loaded, allows multiple 'tidyverse' packages loaded in a single step
library(lubridate) #Allows a variety of date formats to be read

SACTN_day_1 <- read_csv("data/SACTN_day_1.csv") #read_csv function used in order for R to read a csv file, this loaded file was assigned a the name SACTN_day_1 in order for it to be saved in the environment pane.

SACTN_day_1 %>% #using the SACTN_day_1 dataset the maximum and minimum date was calculated using the summarise function.
  summarise(max_date = max(date), #A name was assigned to the max and min calculation in order to distinguish its value in the console pane
            min_date = min(date))

#Plotting of the graph
SACTN_p <- ggplot(data = SACTN_day_1, aes(x = date, y = temp)) + #The name SACTN_p was assigned to the plot using the assign function " <- ". ggplot function was used to plot a graph using the SACTN_day_1 dataset. Using the aes function it was specified that the x and y axis should be data from the date and temp columns respectively. The "+" sign was used while plotting to continue the code in order to edit the parent line.
  geom_line(aes(colour = site)) + #paste0 used to group more than 1 variable
  labs(x = "Date", y = "Temperature (°C)", colour = "Site") + #Using labs function labels were assigned to the x and y axis as well as theh site key.
  ggtitle("Temperature variation between sites from 1972 - 2016") + #Using the ggtitle function a title was assigned to the plot
  theme_bw() #A black and white theme was assigned 

#Temperature selection of one site and specified months
SACTN_day_1%>%  #Select SACTN_day_1 dataset. Use pipe function "%>% " to continue the code
  filter(site == "Port Nolloth", month(date) == 8 | month(date) == 9) #Filter function allows data to be extracted for Port Nolloth site only, select the months within the site column for August or September. "\" means or.

#Temperature selection of one site for a specified year
SACTN_day_1 %>% #Select SACTN_day_1 dataset. Use pipe function "%>% " to continue the code
  filter(site == "Port Nolloth", year(date) == "1994") #The site Port Nolloth was only selected, the months within 1994 was onlu selected.

SACTN_day_1 %>% #SACTN_day_1 dataset selected.
  na.omit() %>% #omit NA values in dataset to avoid inaccurate or NA values
  summarise(avg_temp = mean(temp)) #Summarise function used to calculate the mean temperatures. A name was assigned to the calculated mean

#Tidy data
#Is longer than wider, the columns should be reduced or optimized
SACTN_1 <- SACTN_day_1 %>% #Using the SACTN_day_1 dataset a new dataset was assign using the " <- " function and named SACTN_1
  select(-index) #Excludes index from renamed dataset

SACTN_2 <- SACTN_day_1 %>% #A new dataset, SACTN_2, was assigned using the SACT_day_1 dataset 
  select(site, src, date, temp) #Includes the site, src, date and temp in the new dataset.

load("data/SACTN_mangled.RData") #Load SACTN_mangled.RData file using the "load" function

ggplot(data = SACTN1, aes(x = date, y = temp)) + #Plot SACTN1 data and using the aes function to specify the x and y values. x being date and y being temperature
  geom_line(aes(colour = site, group = paste0(site, src))) + #geom_line specifies that a line graph should be plotted, aes function specifies that the colour of the line should be according to site and the lines should be group according to site and source. Paste0 used to group more than 1 variable, in this caes, site and source.
  labs(x = "Date", y = "Temperature (°C)", colour = "Site") + #Labels added to the x and y axis
  theme_bw() #The black and white theme was assigned to the graph making the background white and a grid present within the graph.

SACTN2_tidy <- SACTN2 %>% #A new name was assigned to the SACTN2 dataset
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp") #this function allows all the selected data sources to be grouped into one column

SACTN3_tidy <- SACTN3 %>% #A new name was assigned to the SACTN3 dataset
  spread(key = var, value = val) #allows columns to be joined. used for repeatition data

SACTN4a_tidy <- SACTN4a %>% #A new name was assigned to the SACTN4a dataset
  separate(col = index, into = c("site", "src"), sep = "/ ") #seperates columns. c function or concatinate used to specify the names of the newly created columns. "/" specifies where to seperate the data into new columns.

SACTN4b_tidy <- SACTN4b %>% #A new name was assigned to the SACTN4b dataset
  unite(year, month, day, col = "date", sep = "-") #3 columns: day, month and date joined into one column, date. seperated by "-".

SACTN4_tidy <- left_join(SACTN4a_tidy, SACTN4b_tidy) #Datasets SACTN4a_tidy and SACTN4b_tidy joined from the left inordre 

#Tidier data
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
  arrange(desc(temp)) #Arranges temp. from lowest to highest, descending order

SACTN %>% #Select SACTN dataset
  filter(site == "Humewood", year(date) == 1990) #Filter data to show the temp from the year 1990

SACTN %>% #Select SACTN dataset
  filter(site == "Humewood", year(date) == 1996) #Filter data to show the temp from the year 1996

#Select columns individually
Try_1 <- SACTN %>% #Renaming of dataset usind SACTN dataset, appears in env pane
  select(site, src, date, temp) #Only the data in the columns: site, src, date and temp was selected

Try_2 <- SACTN %>% #Renaming SACTN dataset to Try_2
  select(site:temp) #Select data from site to temp within the renamed dataset

Try_3 <- SACTN %>% #Renaming SACTN dataset to Try_3
  select(-date, -depth) #Excluded date and depth from renamed dataset

# Select all columns except those within a given sequence
# Note that the '-' goes outside of a new set of brackets
# that are wrapped around the sequence of columns to remove
Try_4 <- SACTN %>% #Renaming SACTN dataset to Try_4
  select(-(date:depth)) #Excludes everything from date to depth

Try_5 <- SACTN %>% #Rename SACTN dataset and then;
  mutate(kelvin = temp + 273.15) #Create new column, named Kelvin, data is temp value + 273.15

SACTN %>% #Using SACTN dataset
  mutate(half_temp = temp/2) #Creates a new column with half of the temp. values

SACTN %>% #Using SACTN dataset
  summarise(mean_temp = mean(temp, na.rm = TRUE)) #Summarise function allows stat calculations to be made. Mean temp was calculated and the NA values were removed

SACTN %>% #Using SACTN dataset
  summarise(mean_temp = mean(temp, na.rm = TRUE), #Opens the functions: mean, standard dev., min and max values for temp values
            sd_temp = sd(temp, na.rm = TRUE), #A name has to be assigned to every calculation
            min_temp = min(temp, na.rm = TRUE), #na.rm = TRUE meanes remove NA values
            max_temp = max(temp, na.rm = TRUE)) 

SACTN %>% 
  summarise(mean(temp, na.rm = T), #No name assigned, values calculated without specification.
            sd(temp, na.rm = T), #T used instead of TRUE, however, it still allows the NA values to be removed
            min(temp, na.rm = T),
            max(temp, na.rm = T))

#Tidiest data
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





