#Nasreen Arend
#Day 3
#16 April 2019
#Regression lines

library(fitdistrplus)
library(logspline)
library(tidyverse)
library(ggplot2)
library(readr)
library(ggpubr)
library(lubridate)
library(dplyr)
library(corrplot)

head(faithful)
eruption.lm <- lm(eruptions ~ waiting, data = faithful) 
summary(eruption.lm)
slope <- round(eruption.lm$coef[2], 3)
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

ggplot(data = eruption.lm, aes(x = waiting, y = eruptions, colour = waiting)) +
  geom_point() + #Point size increases as waiting increases) + 
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +#Point size increases as count increases
  geom_smooth(method = lm) + #A line fitted to plot
  labs(title = "Old Faithful eruption data",
   subtitle = "Linear regression",
   x = "Waiting time (minutes)",
   y = "Eruption duration (minutes)") +
  labs (x = "Waiting", y = "Eruption") + #Adding labels
  theme_bw() #Adding theme

# Load data
ecklonia <- read_csv("ecklonia.csv")
View(ecklonia)

ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID)

cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "pearson")
ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

# Always check if its normally distributed
# Create ordinal data
#Closer to 1, strong colleration
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

# Run test on any variable
cor.test(ecklonia$length, ecklonia$digits)

#Always do shapiro test. it tests for normality. If the value is under 0.05, not noramlly distributed, then use Kendall rank correlation
ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall") #Use kednall method, because data is not normally distributed
# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2)) # Created to make annotation on graph

ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

# Bonus marks, heat maps! do a colour gradient
corrplot(ecklonia_pearson, method = "circle") 
