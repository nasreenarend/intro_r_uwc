#Plotting in R using ggplot2
#Day_2
#Nasreen Arend
#30 January 2019


#Dataset_1
#Hypothesis: The concentration of DNase would increase with the increase of density. The results supported the hypothesis.

dna <-  datasets::DNase
??DNase  

dna_p1 <- ggplot (data = dna, aes(x = conc, y = density, color = Run))+ #Plot named and appears in the environment pane. The ggplot function allows a plot to be created from the dna dataset specifically focusing concentration, density and run.
  geom_point() + #Creates a scatter plot
  geom_line(aes(group = Run))+ #Adds a continuous line through each Run's data
  labs(x = "Concentration", y = "Density") + #Assigns labels to each axis
  ggtitle("Elisa assay of DNase") + #Allows a title to be added
  theme_bw() #Assigns black and white theme to plot

dna_p2 <- ggplot(dna, aes(x = conc, y = density, colour = Run)) + #The <- symbol allows the plot to be named and appear in the environment pane. The ggplot function allows a plot to be created from the dna dataset and aes specifies that the variables concentration, density and run should only be included.
  geom_point() + #Creates a scatter plot
  geom_smooth(method = "lm") + #Makes use of a linear model to fit a best fit line to each run
  facet_wrap(~Run, ncol = 4, nrow = 3) + #This function allows different figures and wraps them in the same plot. ~ specifies that Runs should be split into different figures. ncol = number of columns, whereas, nrow = number of rows
  ggtitle("Elisa assay of DNase") + #Allows a title to be added
  theme_bw() #Assigns a theme to plot

dna %>% 
  summarise (avrg_conc = mean(conc)) #Concentration average

dna %>% 
  summarise (avrg_dens = mean(density)) #Density average


#Dataset_2
#Hypothesis: The subjects' response to indometacin would decrease over time as its concentration increases.
#The plots show that the subjects' response decreases with an increase in indometacin concentration.

indometh <-  datasets::Indometh
??Indometh #Provides more info on dataset

indometh_1 <- ggplot (data = Indometh, aes(x = time, y = conc, color = Subject)) + #Assigns a name to the plot. Plots Indometh dataset
  geom_point() +
  geom_line(aes(group = Subject)) +
  labs(x = "Time (hr)", y = "Concentration (mcg/ml)") +
  ggtitle("Response to indometacin of various concentrations over time") +
  theme_bw()

indometh_2 <- ggplot(Indometh, aes(x = time, y = conc, colour = Subject)) + 
  geom_point() + #Creates scatter plot
  geom_smooth(method = "lm") + #Uses a linear model, fits best-fit line to plot data
  labs(x = "Time (hr)", y = "Concentration (mcg/ml)") +
  ggtitle("Response to indometacin of various concentrations over time") +
  theme_bw()

indometh %>% 
  summarise (avrg_time = mean(time)) #Average time

indometh %>% 
  summarise (avrg_conc = mean(conc)) #Average concentration



#Dataset3
#Hypothesis: An increase in vitamin C would increase tooth length in guinea pigs with orange juice being a more effective vitamin C source than ascorbic acid.
#The results supported the hypothesis.

tooth <- datasets::ToothGrowth #Assign name to dataset, to appear in environment pane
??ToothGrowth

tooth_1 <- ggplot(ToothGrowth, aes(x = dose, y = len, colour = supp)) + #Assigns a name to plot. Plots variables dose, length and supplement only.
  geom_point() +
  geom_smooth(method = "lm") + #Linear model applied to plot
  ggtitle("The effects of various doses of vitamin  C on tooth growth in guinea pigs") + #Title 
  theme_bw()

tooth_2 <- ggplot(tooth, aes(x = dose, y = len, colour = supp)) + #Saves name to graph
  geom_point() +
  geom_line(aes(group = supp)) +
  labs(x = "Dose (mg/dy)", y = "Length") +
  ggtitle("The effects of various doses of vitamin  C on tooth growth in guinea pigs") +
  theme_bw()

tooth %>% 
  summarise (avrg_dose = mean(dose)) #Average dose

tooth %>% 
  summarise (avrg_len = mean(len)) #Density average
 

#Dataset4
#Hypothesis: The blade length of Laminaria positively collerates with its blade weight and blade thickness
#The results shows that the is a positive relationship between blade length and weight, however, blade thickness does not increase with an increase in blade length. 

Lam <- read.csv("laminaria.csv") #Allows csv file to be added to environment pane

Lam_pl <- ggplot (data = Lam, aes(x = blade_weight, y = blade_length, color = blade_thickness))+ #Allows plot to be named, dataset Lam was used, only focused on blade length, weight and thickness.
  geom_point() + #Scatter plot created
  geom_smooth(method = "lm") + #Linear model applied
  labs(x = "Blade weight", y = "Blade length") +
  ggtitle("Blade weight and blade length of Laminaria of varying thickness") +
  theme_bw()

Lam_p2 <- ggplot (data = Lam, aes(x = blade_weight, y = blade_length, color = site))+ #Allows plot to be named, dataset Lam was used, only focused on blade length, weight and site.
  geom_point(shape = 21, colour = "black", fill = "white") + #Scatter plot point customized
  geom_smooth(method = "lm") + #Linear model applied
  labs(x = "Blade weight", y = "Blade length") +
  ggtitle("Blade weight and blade length of Laminaria found across various sites") +
  theme_bw()

Lam2 <- Lam %>% #A new dataset created from Lam dataset
  select(site, blade_length, blade_weight, stipe_length, stipe_mass) %>% #Select only blade length, blade weight, stipe length, stipe mass and site columns
  na.omit %>% #NA values excluded
  group_by(site) %>% #site grouped
  summarise(avrg_bl = mean(blade_length), #a function that averages specified columns
            avrg_bw = mean(blade_weight),
            avrg_sl = mean(stipe_length),
            avrg_sm = mean(stipe_mass))

# Nice plots
# Neat script
# Different plot could allow for better hypotheses but this will be taught in biostats
# After creating a visualisation its always good to asnwer the hypotheses using the visualisation
# Nicely done




