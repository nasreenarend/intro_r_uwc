#Nasreen Arend
#3557056
#Biostats Exercises 6, 7 & 9
#18 April 2019

#Load libraries
library(fitdistrplus)
library(logspline)
library(tidyverse)
library(ggplot2)
library(readr)
library(ggpubr)
library(lubridate)
library(dplyr)
library(corrplot)
library(plotly)
library(reshape2)

#6.7 Exercises
#6.7.1 Exercise 1
#Hypothesis : Males are taller, in length (cm), than females.

MF_Length <- data.frame(length = c(rnorm(n = 100, mean = 175, sd = 8), #Random dataset created and assigned a name. Each catergory has the same standard deviation
                                rnorm(n = 100, mean = 165, sd = 8)),
                     sample = c(rep("Male", 100), rep("Female", 100)))

#Create plot to visualize the data
Plot_MF_Length <- ggplot(data = MF_Length, aes(x = sample, y = length, fill = sample)) +
  geom_boxplot() + #Create a box and whisker plot
  coord_flip() + #Co-ordinates were inverted
  labs(y = "Length (cm)", x = "") + #Labels were assigned, however no label was assigned to the x axis
  theme(axis.text.y = element_blank(),
           axis.ticks.y = element_blank())

#T-test
# Question: are males taller, in length(cm), than females?
# H0 (Null hypothesis)        : Length of males is not greater than length of females
# H1 (Alternative hypothesis) : Length of males is greater than length of females
#OR
#H0 = A ≤ B
#H = A > B

# For a t-test, we must meet certain assumptions:
#dependent variable must be continuous
#observations in the groups being compared are independent of each other
#data are normally distributed
#data are homoscedastic, and in particular, that there are no outliers

#Test normality and homoscedasticity, calculate using shaprio.test and var, respectively
MF_Length %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(length)[2]),
            var_dat = var(length)[1])
# for both male and female, p > 0.05 thus data is normally distributed
# homoscedastic because variance of one is not more than two to four times greater than the other

# Perform t-test 
t.test(length ~ sample, data = random, var.equal = TRUE, alternative = "greater") #p-value = 1
#The t-test gives a t, df and p-value, along with the means of each group.

# note that compare_means() takes the same arguments as t.test()
compare_means(length ~ sample, data = random, method = "t.test", var.equal = TRUE, alternative = "greater")
# p > 0.05 in both tests, we can reject the null hypothesis and accept the alternative hypothesis
# Thus the Length of males is greater than length of females 

#7.4. Exercises
# 7.4.1 Exercise 1
# Does feed type have an effect on the mass of pigs at the end of the experiment?
#enter the mass at the end of the experiment
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7) #Assign a name to eah vector file. The vector file was created by using the c or concatinate function
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# create a dataframe with the created vector files
bacon <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))),
  mass = c(feed_1, feed_2, feed_3, feed_4)))

# Perform an ANOVA test
# QUESTION: Does feed type have an effect on the mass of pigs at the end of the experiment?
# H0: There is no difference in pig mass at the end of the experiments after being fed one of four diets OR H0:μ1=μ2=μ3=μ4
# H1: There IS a difference in pig mass at the end of the experiments after being fed one of four diets

pigs.aov <- aov(mass ~ feed, data = bacon) #An ANOVA dataset was created
summary(pigs.aov) #Summarize the ANOVA model
# Pr(>f) is more than 0.05, we reject null hypothesis
# Thus, there is a difference in pig mass in one or more of the feed types at the end of the experiment after being fed one of four diets

# There is a difference, but the ANOVA does not specify which feeds results in these differences

#These differences can be Visualised with boxplots 
ggplot(data = bacon, aes(x = feed, y = mass, fill = feed )) +
  geom_boxplot(notch = TRUE)
# none of the feeds notched overlap... but this graph is just a suggestion

#Do a Tukey HSD Test for a more ‘precise’ approach. 
#Run a Tukey HSD test on the results of the ANOVA by wrapping tukeyHSD() around aov():
TukeyHSD(pigs.aov)
# p-adj < 0.05 for all feeds: show all diets are different and significant
# lwr and upr does not go across zero in all cases

#Plot the Tukey HSD test results to visualize it
plot(TukeyHSD(pigs.aov))

# 7.4.2 Exercise 2 
#Load built in teeth growth dataset
teeth <- datasets::ToothGrowth #Assign a name to the loaded teeth growth dataset 

# QUESTION: Does the difference in Vitamin C doses have an effect on the length of tooth growth in guinea pigs?
# H0: There is no difference in the tooth length of guinea pigs receiving different vitamin C doses. OR H0:μ1=μ2=μ3
# H1: There a difference in the tooth length of guinea pigs receiving different vitamin C doses

# We only need the Vitamin C doses 
teeth_vitc <- ToothGrowth %>% #Assign a name to the new dataset
  filter(supp == "VC") #Use the filter function to filter the vitamin c supplement values

#Perfrom an ANOVA test
teeth.aov <- aov(len ~ as.factor(dose), data = teeth_vc) #Run an anova test on the filtered teeth_vc dataset and assign a name to it
summary(teeth.aov) #Use the summary function to sumarize the teeth_vc dataset anova test
# pr < 0.05, so we reject the null hypothesis
# There is a difference in tooth lengths of guinea pigs receiving one of three doses of vitamin C

# To determine which doses causes the difference in tooth length we Visualise the data using a boxplot 
ggplot(data = teeth_vc, aes(x = as.factor(dose), y = len, fill = as.factor(dose))) +
  geom_boxplot(notch = TRUE)
# none of the dose notches overlap, thus there are differences in one, two or all of the vitamin C doses.

#Do a Tukey HSD Test for a more ‘precise’ approach. 
#Run a Tukey HSD test on the results of the ANOVA by wrapping tukeyHSD() around aov():
TukeyHSD(teeth.aov)
# p-adj < 0.05, very low for all. so all doses are significant and different 
# lwr and upr does not cross zero

#Visualise the Tukey HSD Test by plotting it
plot(TukeyHSD(teeth.aov))

# 7.4.3 Exercise 3 
#Two-Way ANOVA Test
#There is no difference in the means of factor A
#There is no difference in means of factor B
#There is no interaction between factors A and B

#load teeth dataset
teeth <- datasets::ToothGrowth #Assign a name to the dataset

# H0: interactions between supplement and dose have NO effect on length of teeth
# H1: interactions between supplement and dose DO have an effect on length of teeth

# looking at only length by supplement
summary(aov(len ~ supp, data = teeth))

#Visualize the data by plotting a box plot
ggboxplot(teeth, x = "dose", y = "len", color = "supp",
          palette = c("#00AFBB", "#E7B800")) #Assign different colours to each supplement type

# Line plots with multiple groups
# Plot tooth length ("len") by groups ("dose")
# Color box plot by a second group: "supp"
# Add error bars: mean_se
ggline(teeth, x = "dose", y = "len", color = "supp",
      add = c("mean_se", "dotplot"),
      palette = c("#00AFBB", "#
                  E7B800"))

#Run ANOVA test on teeth dataset
res.aov2 <- aov(len ~ supp + dose, data = teeth)
#Visualize ANOVA test by using the summary function
summary(res.aov2)
#From the ANOVA table we can conclude that both supp and dose are statistically significant. dose is the most significant factor variable. These results would lead us to believe that changing delivery methods (supp) or the dose of vitamin C, will impact significantly the mean tooth length.

# Two-way ANOVA with interaction effect
# These two calls are equivalent
res.aov3 <- aov(len ~ supp * dose, data = teeth)
res.aov3 <- aov(len ~ supp + dose + supp:dose, data = teeth)
summary(res.aov3) #It can be seen that the two main effects (supp and dose) are statistically significant, as well as their interaction.
#From the ANOVA results, you can conclude the following, based on the p-values and a significance level of 0.05:
#the p-value of supp is 0.000429 (significant), which indicates that the levels of supp are associated with significant different tooth length.
#the p-value of dose is < 2e-16 (significant), which indicates that the levels of dose are associated with significant different tooth length.
#the p-value for the interaction between supp*dose is 0.02 (significant), which indicates that the relationships between dose and tooth length depends on the supp method.

#Turkey HSD performs multiple pairwise-comparison between the means of groups. 
TukeyHSD((aov(len ~ supp, data = teeth)))

plot(TukeyHSD((aov(len ~ supp, data = teeth))))


summary(aov(len ~ supp * as.factor(dose), data = teeth)) 
# pr < 0.05, so we reject the null hypothesis
# so, interactions between supplement and dose DOES havae an effcet on length of teeth

# So which have the effect?
TukeyHSD((aov(len ~ supp * as.factor(dose), data = teeth)))

plot(TukeyHSD((aov(len ~ supp * as.factor(dose), data = teeth))))
# On plot, all combinations not crossing zero show which combinations of supplement and diet have the most effect on length of teeth  

# Exercise 9.6 
#Ecklonia heat map
melted_ecklonia <- melt(ecklonia_pearson) #Assign a name to the ecklonia dataset 

ggplot(melted_ecklonia, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "indianred", name = "Pearson correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))

#Genes vs patient heat map
# Labels of rows and columns generate both datasets
name_genes <- paste(rep("GEN", 20), LETTERS[1:20], sep="_") # rows and assigned name of dataset
name_patients <- paste(rep("PATIENT", 20), seq(1,20,1), sep="_") # columns assigned name of dataset

# Generation of dataframe
value_expression <- data.frame(genes = name_genes, 
                               matrix(rnorm(400, 2, 1.8),nrow = 20, ncol = 20))
names(value_expression)[2:21] <- name_patients

# Melt dataframe
df_heatmap <- melt(value_expression, id.vars = "genes")
names(df_heatmap)[2:3] <- c("patient", "expression_level")

head(df_heatmap)

# Elaboration of heatmap (white to steelblue)
ggplot(df_heatmap, aes(patient, genes )) +
  geom_tile(aes(fill = expression_level), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ylab("List of genes ") +
  xlab("List of patients") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Expression level",
       title = "The various genes and its corresponding expression level in patients")

# Elaboration of heatmap with custom gradient (red to steelblue)
ggplot(df_heatmap, aes(patient, genes )) +
  geom_tile(aes(fill = expression_level), , color = "white") +
  scale_fill_gradient(low = "red", high = "steelblue") +
  ylab("List of genes ") +
  xlab("List of patients") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Expression level",
       title = "The various genes and its corresponding expression level in patients")

# Elaboration of heatmap with custom gradient (red to green)
ggplot(df_heatmap, aes(patient, genes )) +
  geom_tile(aes(fill = expression_level), , color = "white") +
  scale_fill_gradient(low = "red", high = "green") +
  ylab("List of genes ") +
  xlab("List of patients") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Expression level",
       title = "The various genes and its corresponding expression level in patients")


# Nicely done!!!
# Good comments
 