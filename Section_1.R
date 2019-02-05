#Section_1 Mapping
#Nasreen Arend
#1 February 2019

# Make use of the rast_feb and rast_aug dataset:
# Explore the dataset (Hint* head, tail, glimpse etc) - Make use of google for more functions on exploring a dataset
# Create a map by making use of the lat and long variables
# Create a colour pallete using the link in the document and make use this colour pallete on the map
# Add complete labels and titles to the map
# Add the name of the oceans (Atlanic and indian ocean) on the map, increase the size of the labels
# The map should include the north arrow and scale bar
# Bonus marks for insetting (having a smaller map inside another map)
# Get creative, try new things.

#load libraries
library(tidyverse) #Tidyverse pakage contains a range of functions to explore datasets
library(ggpubr) #Package allows for plotting or mapping of data
library(scales) #Graphical scales map data to aesthetics, and provide methods for automatically determining breaks and labels for axes and legends.
library(ggsn) ##dds north symbols (18 options) and scale bars in kilometers to maps in geographic or metric coordinates created with 'ggplot' or 'ggmap'.

#Explaination of packages
??tidyverse
??ggpubr
??scales
??ggsn

load("data/rast_aug.RData") #load datasets
load("data/rast_feb.RData")
load("data/sa_provinces.RData")

head(rast_aug) #Exploring the rast_aug dataset. Displays first 6 rows
head(rast_aug, n = 150) #Displays the first 150 rows
head(rast_feb) #Exploring the rast_feb dataset. Displays first 6 rows 
head(rast_feb, n = 150) #Displays the first 150 rows of rast_feb

tail(rast_aug) #Exploring the rast_feb dataset. Displays last 6 rows
tail(rast_aug, n = 150) #Displays the last 150 rows
tail(rast_feb) #Exploring the rast_feb dataset. Displays last 6 rows
tail(rast_feb, n = 150) #Displays the last 150 rows

glimpse(rast_aug) #This makes it possible to see every column in the rast_aug data frame in the comsole pane
glimpse(rast_feb) #This makes it possible to see every column in the rast_feb data frame in the console pane

col_temp <- c("#58C7D2", "#53B6C2", "#4EA4B2", "#4893A2", "#438292", "#3D7282", "#366372", "#305362", "#294552") #assigning a name to the colour pallete. Using the concatinate 'c' function to create a vector file containing a range of colour codes.

#Mapping raster_aug dataset
ggplot(data = rast_aug, aes(x = lon, y = lat)) + #using rast_aug dataset a  map outline was created with latitude and longitude values
  geom_point() #values were mapped as coordinate points

map_aug <-  ggplot(rast_aug, aes(x = lon, y = lat)) +
  geom_point() +
  geom_polygon(colour = "black", fill = "grey70", aes(group = temp)) +
  geom_raster(data = rast_aug, aes(fill = bins)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_aug, aes(x = lon, y = lat, fill = bins), 
            colour = "white", size = 0.5) +
  scale_fill_manual("Temp. (°C)", values = col_temp) +
  coord_equal(xlim = c(15, 35), ylim = c(-37, -26), expand = 0) +
  labs(x = "Longitude", y = "Latitude") + #function allows labels to be assigned to the axis
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  annotate("text", label = "Atlantic\nOcean", 
           x = 16.5, y = -32.5, 
           size = 7.0, 
           angle = 10, 
           colour = "navyblue") +
  annotate("text", label = "Indian\nOcean", 
           x = 31.5, y = -33, 
           size = 7.0, 
           angle = 10, 
           colour = "red") +
  scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
           dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
           dd2km = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 16.5, x.max = 19, y.min = -34, y.max = -36, # Set location of symbol
        scale = 1.2, symbol = 16) +
  annotation_custom(grob = ggplotGrob(africa_map),
                    xmin = 22, xmax = 24,
                    ymin = -27.5, ymax = -29.5) +
  ggtitle("Sea surface temperature around the coast of South Africa during August")
map_aug  

aug_map <- ggplot(data = rast_aug, aes(x = lon, y = lat))+ #Assign a name aug_map to plot using rast_aug dataset.Using aes function it was specified that the data lat and long should be plotted on the x and y axis respectively.
  geom_point() +
  geom_polygon(colour = "black", fill = "grey70", aes(group = temp)) +
  geom_path(data = rast_aug, aes(group = temp)) + 
  geom_raster(data = rast_aug, aes(fill = bins)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_aug, aes(x = lon, y = lat, fill = bins), 
            colour = "white", size = 0.5) + # The coastal temperature values
  scale_fill_manual("Temp. (°C)", values = col_temp) +
  coord_equal(xlim = c(15, 35), ylim = c(-37, -27), expand = 0) +
  labs(x = "Longitude", y = "Latitude") + #function allows labels to be assigned to the axis
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  annotate("text", label = "Atlantic\nOcean", 
           x = 16.5, y = -32.5, 
           size = 6.0, 
           angle = 10, 
           colour = "navyblue") +
  annotate("text", label = "Indian\nOcean", 
           x = 31.5, y = -33, 
           size = 6.0, 
           angle = 10, 
           colour = "red") +
  scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
          dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
          dd2km = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 16.5, x.max = 17.5, y.min = -34, y.max = -35, # Set location of symbol
        scale = 1.2, symbol = 16) +
  annotation_custom(grob = ggplotGrob(africa_map),
                    xmin = 22, xmax = 24,
                    ymin = -27.5, ymax = -29.5) +
  ggtitle("Sea surface temperature around the coast of South Africa during August")
aug_map

#Mapping rast_feb dataset

ggplot(data = rast_feb, aes(x = lon, y = lat)) + #using rast_aug dataset a map outline was created with latitude and longitude values
  geom_point()

#Practice feb map
feb_map <- ggplot(data = rast_feb, aes(x = lon, y = lat)) + #Assign a name aug_map to plot using rast_aug dataset.Using aes function it was specified that the data lat and long should be plotted on the x and y axis respectively.
  geom_raster(data = rast_feb, aes(fill = bins)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_feb, aes(x = lon, y = lat, fill = bins), 
            colour = "white", size = 0.5) + # The coastal temperature values
  scale_fill_manual("Temp. (°C)", values = col_temp) +
  coord_equal(xlim = c(13, 38), ylim = c(-38, -26), expand = 0) +
  labs(x = "Longitude", y = "Latitude") + #function allows labels to be assigned to the axis
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  theme(axis.title = element_blank(), # Remove the axis labels
        legend.text = element_text(size = 6), # Change text size in legend
        legend.title = element_text(size = 6), # Change legend title text size
        legend.key.height = unit(0.3, "cm"), # Change size of legend
        legend.background = element_rect(colour = "white"), # Add legend background
        legend.justification = c(1, 0), # Change position of legend
        legend.position = c(1, 0.578)) +
  annotate("text", label = "Atlantic\nOcean", 
           x = 15.2, y = -31, 
           size = 5.0, 
           angle = 10, 
           colour = "navyblue") +
  annotate("text", label = "Indian\nOcean", 
           x = 31.5, y = -33, 
           size = 5.0, 
           angle = 10, 
           colour = "red") +
  scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
           dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
           dd2km = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 13, x.max = 17.5, y.min = -26, y.max = -28, # Set location of symbol
        scale = 1.2, symbol = 16) +
  annotation_custom(grob = ggplotGrob(africa_map),
                    xmin = 33.5, xmax = 38,
                    ymin = -38.5, ymax = -32) 
feb_map

map_feb <-  ggplot(rast_feb, aes(x = lon, y = lat)) + #Parent line. Using rast_feb dataset coordinates were mapped based on x and y values being latitude and longitude vales respectively.
  geom_point() + #An outline was created
  geom_polygon(colour = "black", fill = "grey70", aes(group = temp)) + #The outline was black while majority of the map was grey, the aes also allowed us to group the data based on temperature.
  geom_raster(data = rast_feb, aes(fill = bins)) + 
  geom_path(data = sa_provinces, aes(group = group)) + #The dataset sa_provinces was used within the map to allow the provinces to be mapped on the map_feb map and grouped according to the group column
  geom_tile(data = rast_aug, aes(x = lon, y = lat, fill = bins), #
            colour = "white", size = 0.5) + #The colour and size of the outline tiles along the coast were adjusted
  scale_fill_manual("Temp. (°C)", values = col_temp) +
  coord_equal(xlim = c(15, 35), ylim = c(-37, -26), expand = 0) +
  labs(x = "Longitude", y = "Latitude") + #function allows labels to be assigned to the axis
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  annotate("text", label = "Atlantic\nOcean", 
           x = 16.5, y = -32.5, 
           size = 7.0, 
           angle = 10, 
           colour = "navyblue") +
  annotate("text", label = "Indian\nOcean", 
           x = 31.5, y = -33, 
           size = 7.0, 
           angle = 10, 
           colour = "red") +
  scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
           dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
           dd2km = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 16.5, x.max = 19, y.min = -34, y.max = -36, # Set location of symbol
        scale = 1.2, symbol = 16) +
  annotation_custom(grob = ggplotGrob(africa_map),
                    xmin = 22, xmax = 24,
                    ymin = -27.5, ymax = -29.5) +
  ggtitle("Sea surface temperature around the coast of South Africa during February")
map_feb  







