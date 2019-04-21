
library(fitdistrplus)
library(logspline)
library(tidyverse)
library(ggplot2)
library(readr)
library(ggpubr)
library(lubridate)
library(dplyr)


y <- c(37.50,46.79,48.30,46.04,43.40,39.25,38.49,49.51,40.38,36.98,40.00,
       38.49,37.74,47.92,44.53,44.91,44.91,40.00,41.51,47.92,36.98,43.40,
       42.26,41.89,38.87,43.02,39.25,40.38,42.64,36.98,44.15,44.91,43.40,
       49.81,38.87,40.00,52.45,53.13,47.92,52.45,44.91,29.54,27.13,35.60,
       45.34,43.37,54.15,42.77,42.88,44.26,27.14,39.31,24.80,16.62,30.30,
       36.39,28.60,28.53,35.84,31.10,34.55,52.65,48.81,43.42,52.49,38.00,
       38.65,34.54,37.70,38.11,43.05,29.95,32.48,24.63,35.33,41.34) #Created vector using 
par(mfrow = c(2, 2))
plot(x = c(1:length(y)), y = y)
hist(y)
descdist(y, discrete = FALSE, boot = 100)

x <- c(18,9,31,7,47,28,20,300,19,6,19,21,99,85,52,68,69,3,48,116,15,27,51,100,105,99,73,58,1,89,222,56,27,36,300,121,5,42,184,88,24,127,67,93,85,60,92,23,39,140,60,71,333,42,16,51,151,625,624,200,350,4,105,199,88,742) #Class generated data, named x
par(mfrow = c(2, 2))
plot(x = c(1:length(x)), y = x) #Geneated scatter graph for the x vector dataset
hist(x) #Histogram generated for x dataset
descdist(x, discrete = FALSE, boot = 100) #Summary statistics of x dataset. min, max, median, mean, est. sd, est. skewness, est. kurtosis was calculated
length(x) #calculates no. of obs. in the vector dataset
mean(x) #Calculated mean

#normal distribution
xy <- rnorm(100, 13, 2) #Created dataset and named xy
par(mfrow = c(2, 2))
plot(x = c(1:100), y = xy)
hist(xy)
descdist(xy, discrete = FALSE)

# uniformly distributed data
xyz <- rexp(100, 0.7)
par(mfrow = c(2, 2))
plot(x = c(1:100), y = xyz)
hist(xyz)
descdist(xyz, discrete = FALSE)

y <-rnorm(n = 200, m = 13, sd = 2)
par(mfrow = c(2, 2))
# using some basic base graphics as ggplot2 is overkill;
# we can get a histogram using hist() statement
hist(y, main = "Histogram of observed data")
plot(density(y), main = "Density estimate of data")
plot(ecdf(y), main = "Empirical cumulative distribution function")
# standardise the data
z.norm <- (y - mean(y)) / sd(y) 
# make a qqplot
qqnorm(z.norm)
# add a 45-degree reference line
abline(0, 1)

# Random normal data
set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Create histogram
h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value") #The label "value" was assigned to the x - axis
h

shapiro.test(r_dat$dat) #Did not work due to the selected data not bein inpendent of each other

# we use the square bracket notation to select only the p-value;
# had we used `[1]` we'd have gotten W
r_dat %>% 
  group_by(sample) %>% #Samples were grouped by samples
  summarise(norm_dat = as.numeric(shapiro.test(dat)[2])) #A seperate shapiro test was done on each data set

out <- shapiro.test(r_dat$dat) #Tests for the normality..
str(r_dat)
out$p.value #singles out only the p value of the shapiro test. we want the p value to be greater than 0.05 so we can do the t test

r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat)) #Refer to chapter 8

#One sample t-test
# create a single sample of random normal data
set.seed(666) #generate a dataset
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A") #dataset is generated, renamed and a normal distribution was selected, the no. of obs, mean and standard deviation were assigned.

# check normality
shapiro.test(r_one$dat) #p value was calculated and greater than 0.05, data was normally distributed

t.test(r_one$dat, mu = 20) # compare random data against a population mean of 20

ggplot(data = r_one, aes(y = dat, x = sample)) +
  geom_boxplot(fill = "lightsalmon") +
  # population  mean (mu) = 20
  geom_hline(yintercept = 20, colour = "blue", 
             size = 1, linetype = "dashed") +
  # population  mean (mu) = 30
  geom_hline(yintercept = 30, colour = "red", 
             size = 1, linetype = "dashed") +
  labs(y = "Value", x = NULL) +
  coord_flip()

shapiro.test(r_one$dat)

# check against the trailing tail
t.test(r_one$dat, mu = 30, alternative = "less")
# check against the leading tail
t.test(r_one$dat, mu = 30, alternative = "greater")

# random normal data
set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# perform t-test
# note how we set the `var.equal` argument to TRUE because we know 
# our data has the same SD (they are simulated as such!)
t.test(dat ~ sample, data = r_two, var.equal = TRUE) #Are the data different in the column different, depending on whether the data belongs to A or B

chicks <- as_tibble(ChickWeight) #Load ChickWeight dataset and assign a name to it 
chicks_sub <- chicks %>% #Assign a new name to the created dataset.
  filter(Diet %in% c(1, 2), Time == 21) #

compare_means(weight ~ Diet, data = chicks_sub, method = "t.test") #
t.test(weight ~ Diet, data = chicks_sub)

chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks.aov1) #Allows the ANOVA results to be viewed in the console

chick_box <- ggplot(data = filter(chicks, Time == 21), aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet), notch = TRUE) +
  labs(x = "Diet", y = "Weight") +
  ggtitle('Diet vs Weight') +
  theme_bw()
chick_box

#COPY PAST TUKEY. CHAPTER 7
TukeyHSD(chicks.aov1)

summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(0))))


# Neat script
# Everything runs and perfect comments
# Its always good to start the script by explaining what the script is about

