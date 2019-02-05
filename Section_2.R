#Section 2
#Nasreen Arend
#1 February 2019

# Make use of the ecklonia.csv dataset:
# Explore the data (Hint* head, tail, glimpse functions)
# Demonstrate the dimensions of the dataset
# Create three graphs; bargraph, line graph and boxplot: Write hypothesis for each of the graphs and answer these hypotheses
# Make use of the ggarrange function and arrange these three graphs created above into 1 plot
# All graphs must have labels as well as titles
# Calculate the mean,max,min,median and variance for the stipe_length, stipe_diameter for each of the sites (Hint* group_by site)
# Calculate standard error. use stdv and variance to get se
# Determine the min and maximum frond length and stipe length
# Determine the overall summary of the dataset. use summary(dataset name)

library(tidyverse) #Load libraries. Tidyverse package was run.
library(ggpubr)

ecklonia <- read_csv("data/ecklonia.csv") #The ecklonia.csv file was read into R using the read_csv function. The new dataset was assigned the name ecklonia.

head(ecklonia) #shows first 6 rows of ecklonia dataset
tail(ecklonia) #shows last 6 rows of ecklonia dataset

head(ecklonia, n = 13) #shows first 13 rows of ecklonia dataset
tail(ecklonia, n = 13) #shows last 13 rows of ecklonia dataset

glimpse(ecklonia) #this makes it possible to see every column in a data frame 
??glimpse

dim(ecklonia) #Shows the dimensions of a large dataset as well as smaller ones such as the selected ecklonia datasets

#Plotting of bargraph, line graph and boxplot

#Epiphyte length would be relatively the same length at the Batsata Rock and Boulders Beach site. The graph did not support the hypothesis, the epiphyte_length was generally higher at the Batsata Rock site than Boulders Beach.
ecklonia_1 <- ggplot(data = ecklonia, aes(x = epiphyte_length)) + #Using the ecklonia dataset a name, ecklonia_1, was assigned to the graph. A bargraph was plotted and aes function allowed for the x values to be specified to epiphyte length
  geom_bar(aes(fill = site), position = "dodge") + #Position dodge places bargraph along side eachother, binwidth determines the width of graphs
  labs(x = "Epiphyte length", y = "Count") + #Labels were assigned to the x and y axis.
  ggtitle("The ephiphte lengths across two sites") + #A title was assigned to the plot
  theme_bw() #A black and white theme was assigned to the plot

#Stipe length would positively correlate with stipe mass in Ecklonia maxima across two sites 
ecklonia_2 <-  ggplot(ecklonia, aes(x = stipe_length, y = stipe_mass, colour = site)) + #Using ecklonia dataset a name was assigned to the graph, ecklonia_2. The aes function specified the x, y and colour values respetively.
  geom_point() + #Points were plotted on the graph based on the ggplot specifications.
  geom_line(aes(group = site)) + #Lines were fitted to connect points according to their sites
  labs(x = "Stipe length", y = "Stipe mass") + #x and y labels were added
  ggtitle("The relationship between stipe length and stipe mass in Ecklonia maxima") + #A title was added to the graph
  theme_bw() #A black and white theme was assigned

#Stipe length would be higher in the site Batsata Rock than Boulders 
ecklonia_3 <- ggplot(data = ecklonia, aes(x = site, y = stipe_length)) + #The name, ecklonia_3, was assigned to the graph using ecklonia dataset. The aes function specified that site value should be plotted on the x axis and stipe length should be plotted on the y axis.
  geom_boxplot(aes(fill = site)) + #Specifies the graph type, a box plot. The fill of the boxes should be according to site values.
  labs(x = "Site", y = "Stipe length") + #Labels were assigned to the x and y axis
  ggtitle('Stipe length vs site') + #A title was assigned to the graph
  theme_bw() #A black and white theme was assigned to the graph

ecklonia_combined <- ggarrange(ecklonia_1, ecklonia_2, ecklonia_3) #The ggarange function found in the ggpubr package allowed the three ecklonia graphs to be combined into one plot.

ecklonia %>% #Using the ecklonia dataset
  group_by(site) %>% #Groups the dataset by site column only
  summarise(mean_sl = mean(stipe_length), #Calculates, mean, max, min, median, vaiance and number of observations of stipe length by using the summarise function.
            max_sl = max(stipe_length), #Each calculation was assigned a name
            min_sl = min(stipe_length), #A "," was placed after every calculation instead of a %>% because each calculation falls under the summarise function
            med_sl = median(stipe_length),
            var_sl = var(stipe_length),
            n = n()) %>% 
  mutate(se = sqrt(var_sl/n)) #Mutate function used to add another column containing the stipe length standard error. Standard error calculated by dividing variance by number of observations

            

ecklonia %>% #Using the ecklonia dataset
group_by(site) %>% #Groups the dataset by site column only
  summarise(mean_sd = mean(stipe_diameter), #Calculates, mean, max, min, median, vaiance and number of observations of stipe diameter by using the summarise function.
            max_sd = max(stipe_diameter), #Each calculation was assigned a name
            min_sd = min(stipe_diameter),
            med_sd = median(stipe_diameter),
            var_sd = var(stipe_diameter),
            n = n()) %>%
  mutate(se = sqrt(var_sd/n)) #Mutate function used to add another column containing the stipe diameter standard error. Standard error calculated by dividing variance by number of observations  

ecklonia %>% #Using the ecklonia dataset
  summarise(min_fl = min(frond_length), #Summarise function used to calculate the in and max values of frond length and stiper length.
            max_fl = max(frond_length), #Each calculation was assigned a name
            min_sl = min(stipe_length),
            max_sl = max(stipe_length))


summary(ecklonia) #Allows a general oveview of the ecklonia dataset 

ecklonia %>% #Specifies the dataset, ecklonia
  summary() #Allows a general overview of the dataset



