#Nasreen Arend
#6 May 2019
#Biostats Assignments
#Metabolic scaling in Meduca sexta

#Load libraries
library(tidyverse)
library(ggpubr)
library(corrplot)
library(lubridate)
library(readr)
library(RColorBrewer)
library(Stat2Data)

#Load dataset
data("MetabolicRate") 

#A Shapiro-Wilks test was done as well as a test for variance in order to determine whether the data is normally distributed and homoscedastic. 
#From here I saw that most of the data was normally distributed and homoscedastic.
MetabolicRate %>% #Using the MetabolicRate dataset
  group_by(Instar) %>% #Group by function allows the data to by grouped by the Instar column
  summarise(r_norm_dist = as.numeric(shapiro.test(LogMrate)[2]),
            r_norm_var = var(LogMrate)) 

MetabolicRate %>% 
  summarise(body_norm_dist = as.numeric(shapiro.test(LogBodySize)[2]),
            body_norm_var = var(LogBodySize))

ms_rate <- read_delim("Basic_Stats/Nasreen_Arend/ms_rate(1).csv", ";", 
                      escape_double = FALSE, trim_ws = TRUE)
#Data found in the fifth instar was not normally distributed, thus was removed from the dataset and the new dataset was named ms_rate which only included 4 instars. 
#Homoscedastic because variance of one is not more than two to four times greater than the other
#We could predict values for this instar stage and its correlating body weight at a later stage after statistical analyses are performed. 
#All other assumptions for majority of statistical analyses were met
#An analysis of variance (ANOVA), correlation, and linear regression were done on the ms_rate dataset.

#A one-way ANOVA was performed on the Instar and MRate (metabolic rate) columns
#This was performed to determine whether variance among the selected variables existed.
mr.aov <- aov(LogMrate ~ as.factor(Instar), data = ms_rate)
summary(mr.aov)

#A histogram of the residuals were made to check the assuptions
#It was normal
ms_residuals <- residuals(mr.aov)
hist(ms_residuals, col = "blue")

#Perform Tukey HSD test 
#This was a pairwise comparisons of all of the groups being compared. It showed where the variance occurred
tuk_ms_rate <- TukeyHSD(mr.aov)

#A boxplot was created (using the ggplot function) to graphically diaplay the results
#Seperate labels a-d were added (using annotate function) to the plot based on the Tukey HSD test results
in_mr_box <- ggplot(data = ms_rate, aes(x = as.factor(Instar), y = LogMrate, 
                                     fill = as.factor(Instar))) +
  geom_boxplot(notch = TRUE) +
  annotate("text", x = 1, y = 0.1, label = "a") +
  annotate("text", x = 2, y = 0.85, label = "b") +
  annotate("text", x = 3, y = 1.5, label = "c") +
  annotate("text", x = 4, y = 1.83, label = "d") +
  labs(x = "Instar", y = "Metabolic rate  (uL CO 2 h-1)", fill = "Instar") + #Labels were assigned to the x & y axis and the key 
  theme_classic() #A theme was assigned and the gridlines were removed

#A correlation was done after a subset of data was made where variables were removed that would not apply in the correlation test. 
#A correlation determined a relationship between body size and metabolic rate. 
#Variables left to perform the correlation were LogBodySize (log body size), Instar and LogMrate (log metabolic rate). 
#A Shapiro-Wilks normality test was done on log body size to see if the data was normally distributed.
sub_mr <- ms_rate %>% 
  select(-Computer, -BodySize, -CO2ppm, -Mrate)

sub_norm <- sub_mr %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))

#The data was not normally distributed
#This violates the normality assumption. 
#A Kendall rank correlation test was used. 
#The data was visualised using a scatterplot fitted with a best fit line based on the linear model. 

cor.test(ms_rate$LogBodySize, ms_rate$LogMrate, method = "kendall")

tau_print <- paste0("tau = ", round(cor(x = sub_mr$LogBodySize, sub_mr$LogMrate, method = "kendall"),2))

plot_corr <- ggplot(data = ms_rate, aes(x = LogBodySize, y = LogMrate)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  annotate("text", x = -2.2, y = 1.7, label = tau_print) +
  labs(x = "Body mass (g)", y = "Metabolic rate (uL CO 2 h-1)") +
  theme_classic()

#A subset of the data was used to create a correlation so that the strength of the relationship between LogBodySize (log body size), Instar stage and LogMrate (log metabolic rate) was determined. 
#A linear regression was performed to determine if body size influences metabolic rate. 
#This was done using a linear model and visualised using the ggplot function. 
linmod_mr <- lm(LogBodySize ~ LogMrate, data = ms_rate)
summary(linmod_mr)

slope <- round(linmod_mr$coef[2], 3) #round function rounds the values off to 3 decimal places
p.val <- 0.001 # p.val <- round(coefficients(summary(met_lm)
r2 <- round(summary(linmod_mr)$r.squared, 3)

plot_linmod <- ggplot(data = ms_rate, aes(x = LogBodySize, y = LogMrate)) +
  geom_point() +
  annotate("text", x = -2.5, y = 1.9, label = paste0("slope == ", slope, "~(min/min)"), #An
           parse = TRUE, hjust = 0) +
  annotate("text", x = -2.5, y = 1.7, label = paste0("italic(p) < ", p.val),
           parse = TRUE, hjust = 0) +
  annotate("text", x = -2.5, y = 1.5, label = paste0("italic(r)^2 == ", r2), 
           parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm") +
  labs(x = "Body mass (g)", y = "Metabolic rate (uL CO 2 h-1)") +
  theme_classic()
